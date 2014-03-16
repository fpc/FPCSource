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
      node,ncal,ncgcal;

    type
       tjvmcallparanode = class(tcgcallparanode)
        protected
         procedure push_formal_para; override;
         procedure push_copyout_para; override;

         procedure handlemanagedbyrefpara(orgparadef: tdef); override;
       end;

       { tjvmcallnode }

       tjvmcallnode = class(tcgcallnode)
        protected
         procedure wrapcomplexinlinepara(para: tcallparanode); override;
         procedure extra_pre_call_code; override;
         procedure set_result_location(realresdef: tstoreddef); override;
         procedure do_release_unused_return_value;override;
         procedure extra_post_call_code; override;
         function dispatch_procvar: tnode;
         procedure remove_hidden_paras;
        public
         function pass_typecheck: tnode; override;
         function pass_1: tnode; override;
       end;


implementation

    uses
      verbose,globals,globtype,constexp,cutils,
      symconst,symtable,symsym,defutil,
      cgutils,tgobj,procinfo,htypechk,
      cpubase,aasmdata,aasmcpu,
      hlcgobj,hlcgcpu,
      pass_1,nutils,nadd,nbas,ncnv,ncon,nflw,ninl,nld,nmem,
      jvmdef;

{*****************************************************************************
                           TJVMCALLPARANODE
*****************************************************************************}

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
      begin
        { everything is wrapped and replaced by handlemanagedbyrefpara() in
          pass_1 }
        push_value_para;
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
                  p:=actualtargetnode(@p)^;
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


    function replacewithtemp(var orgnode:tnode): ttempcreatenode;
      begin
        if valid_for_var(orgnode,false) then
          result:=ctempcreatenode.create_reference(
            orgnode.resultdef,orgnode.resultdef.size,
            tt_persistent,true,orgnode,true)
        else
          result:=ctempcreatenode.create_value(
            orgnode.resultdef,orgnode.resultdef.size,
            tt_persistent,true,orgnode);
        { this node is reused while constructing the temp }
        orgnode:=ctemprefnode.create(result);
        typecheckpass(orgnode);
      end;


    procedure tjvmcallparanode.handlemanagedbyrefpara(orgparadef: tdef);
      var
        arrdef: tarraydef;
        arreledef: tdef;
        initstat,
        copybackstat,
        finistat: tstatementnode;
        finiblock: tblocknode;
        realpara, tempn, unwrappedele0, unwrappedele1: tnode;
        realparaparent: tunarynode;
        realparatemp, arraytemp: ttempcreatenode;
        leftcopy: tnode;
        implicitptrpara,
        verifyout: boolean;
      begin
        { implicit pointer types are already pointers -> no need to stuff them
          in an array to pass them by reference (except in case of a formal
          parameter, in which case everything is passed in an array since the
          callee can't know what was passed in) }
        if jvmimplicitpointertype(orgparadef) and
           (parasym.vardef.typ<>formaldef) then
           exit;

        fparainit:=internalstatements(initstat);
        fparacopyback:=internalstatements(copybackstat);
        finiblock:=internalstatements(finistat);
        getparabasenodes(left,realpara,realparaparent);
        { make sure we can get a copy of left safely, so we can use it both
          to load the original parameter value and to assign the result again
          afterwards (if required) }

        { special case for access to string character, because those are
          translated into function calls that differ depending on which side of
          an assignment they are on }
        if (realpara.nodetype=vecn) and
           (tvecnode(realpara).left.resultdef.typ=stringdef) then
          begin
            if node_complexity(tvecnode(realpara).left)>1 then
              begin
                realparatemp:=replacewithtemp(tvecnode(realpara).left);
                addstatement(initstat,realparatemp);
                addstatement(finistat,ctempdeletenode.create(realparatemp));
              end;
            if node_complexity(tvecnode(realpara).right)>1 then
              begin
                realparatemp:=replacewithtemp(tvecnode(realpara).right);
                addstatement(initstat,realparatemp);
                addstatement(finistat,ctempdeletenode.create(realparatemp));
              end;
          end
        else
          begin
            { general case: if it's possible that there's a function call
              involved, use a temp to prevent double evaluations }
            if assigned(realparaparent) then
              begin
                realparatemp:=replacewithtemp(realparaparent.left);
                addstatement(initstat,realparatemp);
                addstatement(finistat,ctempdeletenode.create(realparatemp));
              end;
          end;
        { create a copy of the original left (with temps already substituted),
          so we can use it if required to handle copying the return value back }
        leftcopy:=left.getcopy;
        implicitptrpara:=jvmimplicitpointertype(orgparadef);
        { create the array temp that that will serve as the paramter }
        if parasym.vardef.typ=formaldef then
          arreledef:=java_jlobject
        else if implicitptrpara then
          arreledef:=getpointerdef(orgparadef)
        else
          arreledef:=parasym.vardef;
        arrdef:=getarraydef(arreledef,1+ord(cs_check_var_copyout in current_settings.localswitches));
        { the -1 means "use the array's element count to determine the number
          of elements" in the JVM temp generator }
        arraytemp:=ctempcreatenode.create(arrdef,-1,tt_persistent,true);
        addstatement(initstat,arraytemp);
        addstatement(finistat,ctempdeletenode.create(arraytemp));

        { we can also check out-parameters if we are certain that they'll be
          valid according to the JVM. That's basically everything except for
          local variables (fields, arrays etc are all initialized on creation) }
        verifyout:=
          (cs_check_var_copyout in current_settings.localswitches) and
          ((actualtargetnode(@left)^.nodetype<>loadn) or
           (tloadnode(actualtargetnode(@left)^).symtableentry.typ<>localvarsym));

        { in case of a non-out parameter, pass in the original value (also
          always in case of implicitpointer type, since that pointer points to
          the data that will be changed by the callee) }
        if (parasym.varspez<>vs_out) or
           verifyout or
           ((parasym.vardef.typ<>formaldef) and
            implicitptrpara) then
          begin
            if implicitptrpara then
              begin
                { pass pointer to the struct }
                left:=caddrnode.create_internal(left);
                include(left.flags,nf_typedaddr);
                typecheckpass(left);
              end;
            { wrap the primitive type in an object container
              if required }
            if parasym.vardef.typ=formaldef then
              begin
                if (left.resultdef.typ in [orddef,floatdef]) then
                  begin
                    left:=cinlinenode.create(in_box_x,false,ccallparanode.create(left,nil));
                    typecheckpass(left);
                  end;
                left:=ctypeconvnode.create_explicit(left,java_jlobject);
              end;
            { put the parameter value in the array }
            addstatement(initstat,cassignmentnode.create(
              cvecnode.create(ctemprefnode.create(arraytemp),genintconstnode(0)),
              left));
            { and the copy for checking }
            if (cs_check_var_copyout in current_settings.localswitches) then
              addstatement(initstat,cassignmentnode.create(
                cvecnode.create(ctemprefnode.create(arraytemp),genintconstnode(1)),
                cvecnode.create(ctemprefnode.create(arraytemp),genintconstnode(0))));
          end
        else
          left.free;
        { replace the parameter with the temp array }
        left:=ctemprefnode.create(arraytemp);
        { generate the code to copy back the changed value into the original
          parameter in case of var/out.

          In case of a formaldef, changes to the parameter in the callee change
          the pointer inside the array -> we have to copy back the changes in
          all cases.

          In case of a regular parameter, we only have to copy things back in
          case it's not an implicit pointer type. The reason is that for
          implicit pointer types, any changes will have been directly applied
          to the original parameter via the implicit pointer that we passed in }
        if (parasym.varspez in [vs_var,vs_out]) and
           ((parasym.vardef.typ=formaldef) or
            not implicitptrpara) then
          begin
            { add the extraction of the parameter and assign it back to the
              original location }
            tempn:=ctemprefnode.create(arraytemp);
            tempn:=cvecnode.create(tempn,genintconstnode(0));
            { unbox if necessary }
            if parasym.vardef.typ=formaldef then
              begin
                if orgparadef.typ in [orddef,floatdef] then
                  tempn:=cinlinenode.create(in_unbox_x_y,false,ccallparanode.create(
                    ctypenode.create(orgparadef),ccallparanode.create(tempn,nil)))
                else if implicitptrpara then
                  tempn:=ctypeconvnode.create_explicit(tempn,getpointerdef(orgparadef))
              end;
            if implicitptrpara then
              tempn:=cderefnode.create(tempn)
            else
              begin
                { add check to determine whether the location passed as
                  var-parameter hasn't been modified directly to a different
                  value than the returned var-parameter in the mean time }
                if ((parasym.varspez=vs_var) or
                    verifyout) and
                   (cs_check_var_copyout in current_settings.localswitches) then
                  begin
                    unwrappedele0:=cvecnode.create(ctemprefnode.create(arraytemp),genintconstnode(0));
                    unwrappedele1:=cvecnode.create(ctemprefnode.create(arraytemp),genintconstnode(1));
                    if (parasym.vardef.typ=formaldef) and
                       (orgparadef.typ in [orddef,floatdef]) then
                      begin
                        unwrappedele0:=cinlinenode.create(in_unbox_x_y,false,ccallparanode.create(
                          ctypenode.create(orgparadef),ccallparanode.create(unwrappedele0,nil)));
                        unwrappedele1:=cinlinenode.create(in_unbox_x_y,false,ccallparanode.create(
                          ctypenode.create(orgparadef),ccallparanode.create(unwrappedele1,nil)))
                      end;
                    addstatement(copybackstat,cifnode.create(
                      caddnode.create(andn,
                        caddnode.create(unequaln,leftcopy.getcopy,ctypeconvnode.create_explicit(unwrappedele0,orgparadef)),
                        caddnode.create(unequaln,leftcopy.getcopy,ctypeconvnode.create_explicit(unwrappedele1,orgparadef))),
                      ccallnode.createintern('fpc_var_copyout_mismatch',
                        ccallparanode.create(genintconstnode(fileinfo.column),
                          ccallparanode.create(genintconstnode(fileinfo.line),nil))
                      ),nil
                    ));
                  end;
              end;
            addstatement(copybackstat,cassignmentnode.create(leftcopy,
              ctypeconvnode.create_explicit(tempn,orgparadef)));
          end
        else
          leftcopy.free;
        addstatement(copybackstat,finiblock);
        firstpass(fparainit);
        firstpass(left);
        firstpass(fparacopyback);
      end;


{*****************************************************************************
                             TJVMCALLNODE
*****************************************************************************}

    procedure tjvmcallnode.wrapcomplexinlinepara(para: tcallparanode);
      var
        tempnode: ttempcreatenode;
      begin
        { don't use caddrnodes for the JVM target, because we can't take the
          address of every kind of type (e.g., of ansistrings). A temp-reference
          node does work for any kind of memory reference (and the expectloc
          is LOC_(C)REFERENCE when this routine is called), but is not (yet)
          supported for other targets }
        tempnode:=ctempcreatenode.create_reference(para.parasym.vardef,para.parasym.vardef.size,
          tt_persistent,tparavarsym(para.parasym).is_regvar(false),para.left,false);
        addstatement(inlineinitstatement,tempnode);
        addstatement(inlinecleanupstatement,ctempdeletenode.create(tempnode));
        para.left:=ctemprefnode.create(tempnode);
        { inherit addr_taken flag }
        if (tabstractvarsym(para.parasym).addr_taken) then
          include(tempnode.tempinfo^.flags,ti_addr_taken);
      end;


    procedure tjvmcallnode.extra_pre_call_code;
      begin
        { when calling a constructor, first create a new instance, except
          when calling it from another constructor (because then this has
          already been done before calling the current constructor) }
        if procdefinition.proctypeoption<>potype_constructor then
          exit;
        if not(methodpointer.resultdef.typ in [classrefdef,recorddef]) then
          exit;
        { in case of an inherited constructor call in a class, the methodpointer
          is an objectdef rather than a classrefdef. That's not true in case
          of records though, so we need an extra check }
        if (current_procinfo.procdef.proctypeoption=potype_constructor) and
           (cnf_inherited in callnodeflags) then
          exit;
        current_asmdata.CurrAsmList.concat(taicpu.op_sym(a_new,current_asmdata.RefAsmSymbol(tabstractrecorddef(procdefinition.owner.defowner).jvm_full_typename(true))));
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
        if is_void(resultdef) then
          exit;
        if (location.loc=LOC_REFERENCE) then
          tg.ungetiftemp(current_asmdata.CurrAsmList,location.reference);
        if assigned(funcretnode) then
          exit;
        if jvmimplicitpointertype(resultdef) or
           (resultdef.size in [1..4]) then
          begin
            current_asmdata.CurrAsmList.concat(taicpu.op_none(a_pop));
            thlcgjvm(hlcg).decstack(current_asmdata.CurrAsmList,1);
          end
        else if resultdef.size=8 then
          begin
            current_asmdata.CurrAsmList.concat(taicpu.op_none(a_pop2));
            thlcgjvm(hlcg).decstack(current_asmdata.CurrAsmList,2);
          end
        else
          internalerror(2011010305);
      end;


    procedure tjvmcallnode.extra_post_call_code;
      var
        realresdef: tdef;
      begin
        thlcgjvm(hlcg).g_adjust_stack_after_call(current_asmdata.CurrAsmList,procdefinition,pushedparasize,typedef);
        { a constructor doesn't actually return a value in the jvm }
        if (tabstractprocdef(procdefinition).proctypeoption<>potype_constructor) then
          begin
            if cnf_return_value_used in callnodeflags then
              begin
                if not assigned(typedef) then
                  realresdef:=tstoreddef(resultdef)
                else
                  realresdef:=tstoreddef(typedef);
                thlcgjvm(hlcg).maybe_resize_stack_para_val(current_asmdata.CurrAsmList,realresdef,false);
              end;
          end;

        { if this was an inherited constructor call, initialise all fields that
          are wrapped types following it }
        if (tabstractprocdef(procdefinition).proctypeoption=potype_constructor) and
           (cnf_inherited in callnodeflags) then
          thlcgjvm(hlcg).gen_initialize_fields_code(current_asmdata.CurrAsmList);
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


  function tjvmcallnode.pass_typecheck: tnode;
    begin
      result:=inherited pass_typecheck;
      if assigned(result) or
         codegenerror then
        exit;
      { unfortunately, we cannot handle a call to a virtual constructor for
        the current instance from inside another constructor. The reason is
        that these must be called via reflection, but before an instance has
        been fully initialized (which can only be done by calling either an
        inherited constructor or another constructor of this class) you can't
        perform reflection.

        Replacing virtual constructors with plain virtual methods that are
        called after the instance has been initialized causes problems if they
        in turn call plain constructors from inside the JDK (you cannot call
        constructors anymore once the instance has been constructed). It also
        causes problems regarding which other constructor to call then instead
        before to initialize the instance (we could add dummy constructors for
        that purpose to Pascal classes, but that scheme breaks when a class
        inherits from a JDK class other than JLObject).
      }
      if (current_procinfo.procdef.proctypeoption=potype_constructor) and
         not(cnf_inherited in callnodeflags) and
         (procdefinition.proctypeoption=potype_constructor) and
         (po_virtualmethod in procdefinition.procoptions) and
         (cnf_member_call in callnodeflags) then
        CGMessage(parser_e_jvm_invalid_virtual_constructor_call);
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
             not(current_procinfo.procdef.synthetickind in [tsk_callthrough,tsk_callthrough_nonabstract]) and
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
          { set fforcedprocname so that even virtual method calls will be
            name-based (instead of based on VMT entry numbers) }
          if procdefinition.typ=procdef then
            fforcedprocname:=stringdup(tprocdef(procdefinition).mangledname)
        end;
    end;


begin
  ccallnode:=tjvmcallnode;
  ccallparanode:=tjvmcallparanode;
end.
