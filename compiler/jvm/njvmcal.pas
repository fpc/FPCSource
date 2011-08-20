{
    Copyright (c) 2011 by Jonas Maebe

    JVM-specific code for call nodes

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit njvmcal;

{$i fpcdefs.inc}

interface

    uses
      cgbase,
      symtype,symdef,
      node,ncgcal;

    type
       tjvmcallparanode = class(tcgcallparanode)
        protected
         outcopybasereg: tregister;
         procedure push_formal_para; override;
         procedure push_copyout_para; override;

         procedure handleformalcopyoutpara(orgparadef: tdef); override;

         procedure load_arrayref_para(useparadef: tdef);
       end;

       { tjvmcallnode }

       tjvmcallnode = class(tcgcallnode)
        protected
         procedure extra_pre_call_code; override;
         procedure set_result_location(realresdef: tstoreddef); override;
         procedure do_release_unused_return_value;override;
         procedure extra_post_call_code; override;
         function dispatch_procvar: tnode;
         procedure remove_hidden_paras;
        public
         function pass_1: tnode; override;
       end;


implementation

    uses
      verbose,globtype,constexp,cutils,
      symconst,symtable,symsym,defutil,
      ncal,
      cgutils,tgobj,procinfo,
      cpubase,aasmdata,aasmcpu,
      hlcgobj,hlcgcpu,
      pass_1,nutils,nbas,ncnv,ncon,ninl,nld,nmem,
      jvmdef;

{*****************************************************************************
                           TJVMCALLPARANODE
*****************************************************************************}

    procedure tjvmcallparanode.load_arrayref_para(useparadef: tdef);
      var
        arrayloc: tlocation;
        arrayref: treference;
      begin
        { cannot be a regular array or record, because those are passed by
          plain reference (since they are reference types at the Java level,
          but not at the Pascal level) -> no special initialisation necessary }
        outcopybasereg:=hlcg.getaddressregister(current_asmdata.CurrAsmList,java_jlobject);
        thlcgjvm(hlcg).a_load_stack_reg(current_asmdata.CurrAsmList,java_jlobject,outcopybasereg);
        reference_reset_base(arrayref,outcopybasereg,0,4);
        arrayref.arrayreftype:=art_indexconst;
        arrayref.indexoffset:=0;
        { load the current parameter value into the array in case it's not an
          out-parameter; if it's an out-parameter the contents must be nil
          but that's already ok, since the anewarray opcode takes care of that }
        if (parasym.varspez<>vs_out) then
          hlcg.a_load_loc_ref(current_asmdata.CurrAsmList,useparadef,useparadef,left.location,arrayref);

        { store the array reference into the parameter location (don't change
          left.location, we may need it for copy-back after the call) }
        location_reset(arrayloc,LOC_REGISTER,OS_ADDR);
        arrayloc.register:=outcopybasereg;
        hlcg.gen_load_loc_cgpara(current_asmdata.CurrAsmList,java_jlobject,arrayloc,tempcgpara)
      end;


    procedure tjvmcallparanode.push_formal_para;
      begin
        { primitive values are boxed, so in all cases this is a pointer to
          something and since it cannot be changed (or is not supposed to be
          changed anyway), we don't have to create a temporary array to hold a
          pointer to this value and can just pass the pointer to this value
          directly.

          In case the value can be changed (formal var/out), then we have
          already created a temporary array of one element that holds the boxed
          (or in case of a non-primitive type: original) value. The reason is
          that copying it back out may be a complex operation which we don't
          want to handle at the code generator level.

          -> always push a value parameter (which is either an array of one
          element, or an object) }
        push_value_para
      end;


    procedure tjvmcallparanode.push_copyout_para;
      var
        mangledname: string;
        primitivetype: boolean;
        opc: tasmop;
      begin
        { create an array with one element of the parameter type }
        thlcgjvm(hlcg).a_load_const_stack(current_asmdata.CurrAsmList,s32inttype,1,R_INTREGISTER);
        mangledname:=jvmarrtype(left.resultdef,primitivetype);
        if primitivetype then
          opc:=a_newarray
        else
          opc:=a_anewarray;
        { doesn't change stack height: one int replaced by one reference }
        current_asmdata.CurrAsmList.concat(taicpu.op_sym(opc,current_asmdata.RefAsmSymbol(mangledname)));
        load_arrayref_para(left.resultdef);
      end;


    procedure getparabasenodes(p: tnode; out basenode: tnode; out parent: tunarynode);
      begin
        parent:=nil;
        while assigned(p) do
          begin
            case p.nodetype of
              inlinen:
                begin
                  if tinlinenode(p).inlinenumber=in_box_x then
                    begin
                      parent:=tunarynode(p);
                      p:=parent.left;
                    end
                  else
                    break;
                end;
              subscriptn,
              vecn:
                begin
                  break;
                end;
              typeconvn:
                begin
                  parent:=tunarynode(p);
                  { skip typeconversions that don't change the node type }
                  p:=p.actualtargetnode;
                end;
              derefn:
                begin
                  parent:=tunarynode(p);
                  p:=tunarynode(p).left;
                end
              else
                break;
            end;
          end;
        basenode:=p;
      end;


    function replacewithtemps(var orgnode, copiednode: tnode): ttempcreatenode;
      begin
        result:=ctempcreatenode.create_value(
          orgnode.resultdef,orgnode.resultdef.size,
          tt_persistent,true,orgnode);
        { this right is reused while constructing the temp }
        orgnode:=ctemprefnode.create(result);
        typecheckpass(orgnode);
        { this right is not reused }
        copiednode.free;
        copiednode:=ctemprefnode.create(result);
        typecheckpass(copiednode);
      end;


    procedure tjvmcallparanode.handleformalcopyoutpara(orgparadef: tdef);
      var
        paravaltemp,
        arraytemp,
        indextemp: ttempcreatenode;
        arrdef: tarraydef;
        initstat,
        finistat: tstatementnode;
        leftcopy: tnode;
        realpara, copyrealpara, tempn, assignmenttempn: tnode;
        realparaparent,copyrealparaparent: tunarynode;
        derefbasedef: tdef;
        deref: boolean;
      begin
        fparainit:=internalstatements(initstat);
        { In general, we now create a temp array of one element, assign left
          (or its address in case of a jvmimplicitpointertype) to it, replace
          the parameter with this array, and add code to paracopyback that
          extracts the value from the array again and assigns it to the original
          variable.

          Complications
            a) in case the parameter involves calling a function, it must not
               be called twice, so take the address of the location (since this
               is a var/out parameter, taking the address is conceptually
               always possible)
            b) in case this is an element of a string, we can't take the address
               in JVM code, so we then have to take the address of the string
               (which conceptually may not be possible since it can be a
                property or so) and store the index value into a temp, and
                reconstruct the vecn in te paracopyback code from this data
                (it's similar for normal var/out parameters)
        }

        { we'll replace a bunch of stuff in the parameter with temprefnodes,
          but we can't take a getcopy for the assignment afterwards of this
          result since a getcopy will always assume that we are copying the
          init/deletenodes too and that the temprefnodes have to point to the
          new temps -> get a copy of the parameter in advance, and then replace
          the nodes in the copy with temps just like in the original para }
        leftcopy:=left.getcopy;
        { get the real parameter source in case of type conversions. This is
          the same logic as for set_unique(). The parent is where we have to
          replace realpara with the temp that replaces it. }
        getparabasenodes(left,realpara,realparaparent);
        getparabasenodes(leftcopy,copyrealpara,copyrealparaparent);
        { assign either the parameter's address (in case it's an implicit
          pointer type) or the parameter itself (in case it's a primitive or
          actual pointer/object type) to the temp }
        deref:=false;
        if jvmimplicitpointertype(realpara.resultdef) then
          begin
            derefbasedef:=realpara.resultdef;
            realpara:=caddrnode.create_internal(realpara);
            include(realpara.flags,nf_typedaddr);
            typecheckpass(realpara);
            { we'll have to reference the parameter again in the expression }
            deref:=true;
          end;
        paravaltemp:=nil;
        { make sure we don't replace simple loadnodes with a temp, because
          in case of passing e.g. stringvar[3] to a formal var/out parameter,
          we add "stringvar[3]:=<result>" afterwards. Because Java strings are
          immutable, this is translated into "stringvar:=stringvar.setChar(3,
          <result>)". So if we replace stringvar with a temp, this will change
          the temp rather than stringvar. }
        indextemp:=nil;
        if (realpara.nodetype=vecn) then
          begin
            if node_complexity(tvecnode(realpara).left)>1 then
              begin
                paravaltemp:=replacewithtemps(tvecnode(realpara).left,
                  tvecnode(copyrealpara).left);
                addstatement(initstat,paravaltemp);
              end;
            { in case of an array index, also replace the index with a temp if
              necessary/useful }
            if (node_complexity(tvecnode(realpara).right)>1) then
              begin
                indextemp:=replacewithtemps(tvecnode(realpara).right,
                  tvecnode(copyrealpara).right);
                addstatement(initstat,indextemp);
              end;
          end
        else
          begin
            paravaltemp:=ctempcreatenode.create_value(
              realpara.resultdef,java_jlobject.size,tt_persistent,true,realpara);
            addstatement(initstat,paravaltemp);
            { replace the parameter in the parameter expression with this temp }
            tempn:=ctemprefnode.create(paravaltemp);
            assignmenttempn:=ctemprefnode.create(paravaltemp);
            { will be spliced in the middle of a tree that has already been
              typecheckpassed }
            typecheckpass(tempn);
            typecheckpass(assignmenttempn);
            if assigned(realparaparent) then
              begin
                { left has been reused in paravaltemp (it's realpara itself) ->
                  don't free }
                realparaparent.left:=tempn;
                { the left's copy is not reused }
                copyrealparaparent.left.free;
                copyrealparaparent.left:=assignmenttempn;
              end
            else
              begin
                { left has been reused in paravaltemp (it's realpara itself) ->
                  don't free }
                left:=tempn;
                { leftcopy can remain the same }
                assignmenttempn.free;
              end;
          end;
        { create the array temp that and assign the parameter value (typecasted
          to java_jlobject) }
        arrdef:=tarraydef.create(0,1,s32inttype);
        arrdef.elementdef:=java_jlobject;
        arraytemp:=ctempcreatenode.create(arrdef,java_jlobject.size,
          tt_persistent,true);
        addstatement(initstat,arraytemp);
        { wrap the primitive type in an object container
          if required }
        if (left.resultdef.typ in [orddef,floatdef]) then
          begin
            left:=cinlinenode.create(in_box_x,false,ccallparanode.create(left,nil));
            typecheckpass(left);
          end;
        addstatement(initstat,cassignmentnode.create(
          cvecnode.create(ctemprefnode.create(arraytemp),genintconstnode(0)),
          ctypeconvnode.create_explicit(left,java_jlobject)));
        { replace the parameter with the array }
        left:=ctemprefnode.create(arraytemp);
        { add the extraction of the parameter and assign it back to the
          original location }
        fparacopyback:=internalstatements(finistat);
        tempn:=cvecnode.create(ctemprefnode.create(arraytemp),genintconstnode(0));
        { unbox if necessary }
        if orgparadef.typ in [orddef,floatdef] then
          tempn:=cinlinenode.create(in_unbox_x_y,false,ccallparanode.create(
            ctypenode.create(orgparadef),ccallparanode.create(tempn,nil)));
        if (deref) then
          begin
            inserttypeconv_explicit(tempn,getpointerdef(derefbasedef));
            tempn:=cderefnode.create(tempn);
          end;
        addstatement(finistat,cassignmentnode.create(leftcopy,
          ctypeconvnode.create_explicit(tempn,orgparadef)));
        if assigned(indextemp) then
          addstatement(finistat,ctempdeletenode.create(indextemp));
        addstatement(finistat,ctempdeletenode.create(arraytemp));
        if assigned(paravaltemp) then
          addstatement(finistat,ctempdeletenode.create(paravaltemp));
        typecheckpass(fparainit);
        typecheckpass(left);
        typecheckpass(fparacopyback);
      end;


{*****************************************************************************
                             TJVMCALLNODE
*****************************************************************************}

    procedure tjvmcallnode.extra_pre_call_code;
      begin
        { when calling a constructor, first create a new instance, except
          when calling it from another constructor (because then this has
          already been done before calling the current constructor) }
        if procdefinition.typ<>procdef then
          exit;
        if tabstractprocdef(procdefinition).proctypeoption<>potype_constructor then
          exit;
        if not(methodpointer.resultdef.typ in [classrefdef,recorddef]) then
          exit;
        current_asmdata.CurrAsmList.concat(taicpu.op_sym(a_new,current_asmdata.RefAsmSymbol(tabstractrecorddef(tabstractprocdef(procdefinition).owner.defowner).jvm_full_typename(true))));
        { the constructor doesn't return anything, so put a duplicate of the
          self pointer on the evaluation stack for use as function result
          after the constructor has run }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_dup));
        thlcgjvm(hlcg).incstack(current_asmdata.CurrAsmList,2);
      end;


    procedure tjvmcallnode.set_result_location(realresdef: tstoreddef);
      begin
        location_reset_ref(location,LOC_REFERENCE,def_cgsize(realresdef),1);
        { in case of jvmimplicitpointertype(), the function will have allocated
          it already and we don't have to allocate it again here }
        if not jvmimplicitpointertype(realresdef) then
          tg.gethltemp(current_asmdata.CurrAsmList,realresdef,realresdef.size,tt_normal,location.reference)
        else
          tg.gethltemp(current_asmdata.CurrAsmList,java_jlobject,java_jlobject.size,tt_normal,location.reference);
      end;


    procedure tjvmcallnode.do_release_unused_return_value;
      begin
        if (tabstractprocdef(procdefinition).proctypeoption=potype_constructor) and
           (current_procinfo.procdef.proctypeoption=potype_constructor) then
          exit;
        if (location.loc=LOC_REFERENCE) then
          tg.ungetiftemp(current_asmdata.CurrAsmList,location.reference);
        if assigned(funcretnode) then
          exit;
        case resultdef.size of
          0:
            ;
          1..4:
            begin
              current_asmdata.CurrAsmList.concat(taicpu.op_none(a_pop));
              thlcgjvm(hlcg).decstack(current_asmdata.CurrAsmList,1);
            end;
          8:
            begin
              current_asmdata.CurrAsmList.concat(taicpu.op_none(a_pop2));
              thlcgjvm(hlcg).decstack(current_asmdata.CurrAsmList,2);
            end
          else
            internalerror(2011010305);
        end;
      end;


    procedure tjvmcallnode.extra_post_call_code;
      var
        totalremovesize: longint;
        realresdef: tdef;
        ppn: tjvmcallparanode;
        pararef: treference;
      begin
        if not assigned(typedef) then
          realresdef:=tstoreddef(resultdef)
        else
          realresdef:=tstoreddef(typedef);
        { a constructor doesn't actually return a value in the jvm }
        if (tabstractprocdef(procdefinition).proctypeoption=potype_constructor) then
          totalremovesize:=pushedparasize
        else
          { even a byte takes up a full stackslot -> align size to multiple of 4 }
          totalremovesize:=pushedparasize-(align(realresdef.size,4) shr 2);
        { remove parameters from internal evaluation stack counter (in case of
          e.g. no parameters and a result, it can also increase) }
        if totalremovesize>0 then
          thlcgjvm(hlcg).decstack(current_asmdata.CurrAsmList,totalremovesize)
        else if totalremovesize<0 then
          thlcgjvm(hlcg).incstack(current_asmdata.CurrAsmList,-totalremovesize);

        { if this was an inherited constructor call, initialise all fields that
          are wrapped types following it }
        if (tabstractprocdef(procdefinition).proctypeoption=potype_constructor) and
           (cnf_inherited in callnodeflags) then
          thlcgjvm(hlcg).gen_initialize_fields_code(current_asmdata.CurrAsmList);

        { copy back the copyout parameter values, if any }
        { Release temps from parameters }
        ppn:=tjvmcallparanode(left);
        while assigned(ppn) do
          begin
            if assigned(ppn.left) then
              begin
                { don't try to copy back vs_constref }
                if (ppn.parasym.varspez in [vs_var,vs_out]) and
                   (ppn.outcopybasereg<>NR_NO) then
                  begin
                    reference_reset_base(pararef,NR_NO,0,4);
                    pararef.arrayreftype:=art_indexconst;
                    pararef.base:=ppn.outcopybasereg;
                    pararef.indexoffset:=0;
                    { the value has to be copied back into persistent storage }
                    if (ppn.parasym.vardef.typ<>formaldef) then
                      begin
                        case ppn.left.location.loc of
                          LOC_REFERENCE:
                            hlcg.a_load_ref_ref(current_asmdata.CurrAsmList,ppn.left.resultdef,ppn.left.resultdef,pararef,ppn.left.location.reference);
                          LOC_CREGISTER:
                            hlcg.a_load_ref_reg(current_asmdata.CurrAsmList,ppn.left.resultdef,ppn.left.resultdef,pararef,ppn.left.location.register);
                        else
                          internalerror(2011051201);
                        end;
                      end
                    else
                      begin
                        { extracting values from foramldef parameters is done
                          by the generic code }
                      end;
                  end;
              end;
            ppn:=tjvmcallparanode(ppn.right);
          end;
      end;


  procedure tjvmcallnode.remove_hidden_paras;
    var
      prevpara, para, nextpara: tcallparanode;
    begin
      prevpara:=nil;
      para:=tcallparanode(left);
      while assigned(para) do
        begin
          nextpara:=tcallparanode(para.right);
          if vo_is_hidden_para in para.parasym.varoptions then
            begin
              if assigned(prevpara) then
                prevpara.right:=nextpara
              else
                left:=nextpara;
              para.right:=nil;
              para.free;
            end
          else
            prevpara:=para;
          para:=nextpara;
        end;
    end;


  function tjvmcallnode.dispatch_procvar: tnode;
    var
      pdclass: tobjectdef;
    begin
      pdclass:=tprocvardef(right.resultdef).classdef;
      { convert procvar type into corresponding class }
      if not tprocvardef(right.resultdef).is_addressonly then
        begin
          right:=caddrnode.create_internal(right);
          include(right.flags,nf_typedaddr);
        end;
      right:=ctypeconvnode.create_explicit(right,pdclass);
      include(right.flags,nf_load_procvar);
      typecheckpass(right);

      { call the invoke method with these parameters. It will take care of the
        wrapping and typeconversions; first filter out the automatically added
        hidden parameters though }
      remove_hidden_paras;
      result:=ccallnode.createinternmethod(right,'INVOKE',left);
      { reused }
      left:=nil;
      right:=nil;
    end;


  function tjvmcallnode.pass_1: tnode;
    var
      sym: tsym;
      wrappername: shortstring;
    begin
      { transform procvar calls }
      if assigned(right) then
        result:=dispatch_procvar
      else
        begin
          { replace virtual class method and constructor calls in case they may
            be indirect; make sure we don't replace the callthrough to the
            original constructor with another call to the wrapper }
          if (procdefinition.typ=procdef) and
             (current_procinfo.procdef.synthetickind<>tsk_callthrough) and
             not(cnf_inherited in callnodeflags) and
             ((procdefinition.proctypeoption=potype_constructor) or
              (po_classmethod in procdefinition.procoptions)) and
             (po_virtualmethod in procdefinition.procoptions) and
             (methodpointer.nodetype<>loadvmtaddrn) then
            begin
              wrappername:=symtableprocentry.name+'__FPCVIRTUALCLASSMETHOD__';
              sym:=
                search_struct_member(tobjectdef(procdefinition.owner.defowner),
                  wrappername);
              if not assigned(sym) or
                 (sym.typ<>procsym) then
                internalerror(2011072801);
                { do not simply replace the procsym/procdef in case we could
                  in theory do that, because the parameter nodes have already
                  been bound to the current procdef's parasyms }
                remove_hidden_paras;
                result:=ccallnode.create(left,tprocsym(sym),symtableproc,methodpointer,callnodeflags);
                result.flags:=flags;
                left:=nil;
                methodpointer:=nil;
                exit;
            end;
          result:=inherited pass_1;
          if assigned(result) then
            exit;
        end;
    end;


begin
  ccallnode:=tjvmcallnode;
  ccallparanode:=tjvmcallparanode;
end.
