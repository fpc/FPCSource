{
    This file implements the node for sub procedure calling.

    Copyright (c) 1998-2002 by Florian Klaempfl

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
unit ncal;

{$i fpcdefs.inc}

{ $define DEBUGINLINE}

interface

    uses
       cutils,cclasses,
       globtype,constexp,
       paramgr,parabase,cgbase,
       node,nbas,nutils,
       {$ifdef state_tracking}
       nstate,
       {$endif state_tracking}
       symbase,symtype,symsym,symdef,symtable;

    type
       tcallnodeflag = (
         cnf_typedefset,
         cnf_return_value_used,
         cnf_do_inline,
         cnf_inherited,
         cnf_anon_inherited,
         cnf_new_call,
         cnf_dispose_call,
         cnf_member_call,        { called with implicit methodpointer tree }
         cnf_uses_varargs,       { varargs are used in the declaration }
         cnf_create_failed,      { exception thrown in constructor -> don't call beforedestruction }
         cnf_objc_processed,     { the procedure name has been set to the appropriate objc_msgSend* variant -> don't process again }
         cnf_objc_id_call,       { the procedure is a member call via id -> any ObjC method of any ObjC type in scope is fair game }
         cnf_unit_specified,     { the unit in which the procedure has to be searched has been specified }
         cnf_call_never_returns  { information for the dfa that a subroutine never returns }
       );
       tcallnodeflags = set of tcallnodeflag;

       tcallparanode = class;

       tcallnode = class(tbinarynode)
       private
          { number of parameters passed from the source, this does not include the hidden parameters }
          paralength   : smallint;
          function  is_simple_para_load(p:tnode; may_be_in_reg: boolean):boolean;
          procedure maybe_load_in_temp(var p:tnode);
          function  gen_high_tree(var p:tnode;paradef:tdef):tnode;
          function  gen_procvar_context_tree:tnode;
          function  gen_self_tree:tnode;
          function  gen_vmt_tree:tnode;
          procedure gen_hidden_parameters;
          function  funcret_can_be_reused:boolean;
          procedure maybe_create_funcret_node;
          procedure bind_parasym;
          procedure add_init_statement(n:tnode);
          procedure add_done_statement(n:tnode);
          procedure convert_carg_array_of_const;
          procedure order_parameters;
          procedure check_inlining;
          function  pass1_normal:tnode;
          procedure register_created_object_types;
          function get_expect_loc: tcgloc;
       protected
          procedure objc_convert_to_message_send;virtual;

       protected
          { inlining support }
          inlinelocals            : TFPObjectList;
          inlineinitstatement,
          inlinecleanupstatement  : tstatementnode;
          procedure createinlineparas;
          procedure wrapcomplexinlinepara(para: tcallparanode); virtual;
          function  replaceparaload(var n: tnode; arg: pointer): foreachnoderesult;
          procedure createlocaltemps(p:TObject;arg:pointer);
          function  optimize_funcret_assignment(inlineblock: tblocknode): tnode;
          function  pass1_inline:tnode;
       protected
          pushedparasize : longint;
          { Objective-C support: force the call node to call the routine with
            this name rather than the name of symtableprocentry (don't store
            to ppu, is set while processing the node) }
          fobjcforcedprocname: pshortstring;
       public
          { the symbol containing the definition of the procedure }
          { to call                                               }
          symtableprocentry : tprocsym;
          symtableprocentryderef : tderef;
          { symtable where the entry was found, needed for with support }
          symtableproc   : TSymtable;
          { the definition of the procedure to call }
          procdefinition : tabstractprocdef;
          procdefinitionderef : tderef;
          { tree that contains the pointer to the object for this method }
          methodpointer  : tnode;
          { initialize/finalization of temps }
          callinitblock,
          callcleanupblock : tblocknode;

          { function return node for initialized types or supplied return variable.
            When the result is passed in a parameter then it is set to nil }
          funcretnode    : tnode;
          { varargs parasyms }
          varargsparas : tvarargsparalist;

          { separately specified resultdef for some compilerprocs (e.g. }
          { you can't have a function with an "array of char" resultdef }
          { the RTL) (JM)                                                }
          typedef: tdef;
          callnodeflags : tcallnodeflags;

          { only the processor specific nodes need to override this }
          { constructor                                             }
          constructor create(l:tnode; v : tprocsym;st : TSymtable; mp: tnode; callflags:tcallnodeflags);virtual;
          constructor create_procvar(l,r:tnode);
          constructor createintern(const name: string; params: tnode);
          constructor createinternfromunit(const fromunit, procname: string; params: tnode);
          constructor createinternres(const name: string; params: tnode; res:tdef);
          constructor createinternresfromunit(const fromunit, procname: string; params: tnode; res:tdef);
          constructor createinternreturn(const name: string; params: tnode; returnnode : tnode);
          constructor createinternmethod(mp: tnode; const name: string; params: tnode);
          constructor createinternmethodres(mp: tnode; const name: string; params: tnode; res:tdef);
          destructor destroy;override;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderefimpl;override;
          procedure derefimpl;override;
          function  dogetcopy : tnode;override;
          { Goes through all symbols in a class and subclasses and calls
            verify abstract for each .
          }
          procedure verifyabstractcalls;
          { called for each definition in a class and verifies if a method
            is abstract or not, if it is abstract, give out a warning
          }
          procedure verifyabstract(sym:TObject;arg:pointer);
          procedure insertintolist(l : tnodelist);override;
          function  pass_1 : tnode;override;
          function  pass_typecheck:tnode;override;
       {$ifdef state_tracking}
          function track_state_pass(exec_known:boolean):boolean;override;
       {$endif state_tracking}
          function  docompare(p: tnode): boolean; override;
          procedure printnodedata(var t:text);override;
          function  para_count:longint;
          function  required_para_count:longint;
          { checks if there are any parameters which end up at the stack, i.e.
            which have LOC_REFERENCE and set pi_has_stackparameter if this applies }
          procedure check_stack_parameters;
          { force the name of the to-be-called routine to a particular string,
            used for Objective-C message sending.  }
          property parameters : tnode read left write left;
          property pushed_parasize: longint read pushedparasize;
       private
          AbstractMethodsList : TFPHashList;
       end;
       tcallnodeclass = class of tcallnode;

       tcallparaflag = (
          cpf_is_colon_para,
          cpf_varargs_para       { belongs this para to varargs }
       );
       tcallparaflags = set of tcallparaflag;

       tcallparanode = class(ttertiarynode)
       private
          fcontains_stack_tainting_call_cached,
          ffollowed_by_stack_tainting_call_cached : boolean;
       protected
          { in case of copy-out parameters: initialization code, and the code to
            copy back the parameter value after the call (including any required
            finalization code }
          fparainit,
          fparacopyback: tnode;
          procedure handlemanagedbyrefpara(orgparadef: tdef);virtual;abstract;
       public
          callparaflags : tcallparaflags;
          parasym       : tparavarsym;
          { only the processor specific nodes need to override this }
          { constructor                                             }
          constructor create(expr,next : tnode);virtual;
          destructor destroy;override;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderefimpl; override;
          procedure derefimpl; override;
          function dogetcopy : tnode;override;
          procedure insertintolist(l : tnodelist);override;
          function pass_typecheck : tnode;override;
          function pass_1 : tnode;override;
          procedure get_paratype;
          procedure firstcallparan;
          procedure insert_typeconv;
          procedure secondcallparan;virtual;abstract;
          function docompare(p: tnode): boolean; override;
          procedure printnodetree(var t:text);override;
          { returns whether a parameter contains a type conversion from }
          { a refcounted into a non-refcounted type                     }
          function can_be_inlined: boolean;

          property nextpara : tnode read right write right;
          { third is reused to store the parameter name (only while parsing
            vardispatch calls, never in real node tree) and copy of 'high'
            parameter tree when the parameter is an open array of managed type }
          property parametername : tnode read third write third;

          { returns whether the evaluation of this parameter involves a
            stack tainting call }
          function contains_stack_tainting_call: boolean;
          { initialises the fcontains_stack_tainting_call_cached field with the
            result of contains_stack_tainting_call so that it can be quickly
            accessed via the contains_stack_tainting_call_cached property }
          procedure init_contains_stack_tainting_call_cache;
          { returns result of contains_stack_tainting_call cached during last
            call to init_contains_stack_tainting_call_cache }
          property contains_stack_tainting_call_cached: boolean read fcontains_stack_tainting_call_cached;
          { returns whether this parameter is followed by at least one other
            parameter whose evaluation involves a stack tainting parameter
            (result is only valid after order_parameters has been called) }
          property followed_by_stack_tainting_call_cached: boolean read ffollowed_by_stack_tainting_call_cached;
          property paracopyback: tnode read fparacopyback;
       end;
       tcallparanodeclass = class of tcallparanode;

       tdispcalltype = (
         dct_method,
         dct_propget,
         dct_propput
       );

    function reverseparameters(p: tcallparanode): tcallparanode;
    function translate_disp_call(selfnode,parametersnode: tnode; calltype: tdispcalltype; const methodname : ansistring;
      dispid : longint;resultdef : tdef) : tnode;

    var
      ccallnode : tcallnodeclass = tcallnode;
      ccallparanode : tcallparanodeclass = tcallparanode;

      { Current callnode, this is needed for having a link
       between the callparanodes and the callnode they belong to }
      aktcallnode : tcallnode;


implementation

    uses
      systems,
      verbose,globals,
      symconst,defutil,defcmp,
      htypechk,pass_1,
      ncnv,nld,ninl,nadd,ncon,nmem,nset,nobjc,
      ngenutil,objcutil,
      procinfo,cpuinfo,
      wpobase;

    type
     tobjectinfoitem = class(tlinkedlistitem)
       objinfo : tobjectdef;
       constructor create(def : tobjectdef);
     end;


{****************************************************************************
                             HELPERS
 ****************************************************************************}

    function reverseparameters(p: tcallparanode): tcallparanode;
      var
        hp1, hp2: tcallparanode;
      begin
        hp1:=nil;
        while assigned(p) do
          begin
             { pull out }
             hp2:=p;
             p:=tcallparanode(p.right);
             { pull in }
             hp2.right:=hp1;
             hp1:=hp2;
          end;
        reverseparameters:=hp1;
      end;

    function translate_disp_call(selfnode,parametersnode: tnode; calltype: tdispcalltype; const methodname : ansistring;
      dispid : longint;resultdef : tdef) : tnode;
      const
        DISPATCH_METHOD = $1;
        DISPATCH_PROPERTYGET = $2;
        DISPATCH_PROPERTYPUT = $4;
        DISPATCH_PROPERTYPUTREF = $8;
        DISPATCH_CONSTRUCT = $4000;

        calltypes: array[tdispcalltype] of byte = (
          DISPATCH_METHOD, DISPATCH_PROPERTYGET, DISPATCH_PROPERTYPUT
        );
      var
        statements : tstatementnode;
        result_data,
        params : ttempcreatenode;
        paramssize : cardinal;
        calldescnode : tdataconstnode;
        resultvalue : tnode;
        para : tcallparanode;
        namedparacount,
        paracount : longint;
        assignmenttype,
        vardatadef,
        pvardatadef : tdef;
        useresult: boolean;
        restype: byte;

        names : ansistring;
        variantdispatch : boolean;

      function is_byref_para(out assign_type: tdef): boolean;
        begin
          result:=(assigned(para.parasym) and (para.parasym.varspez in [vs_var,vs_out,vs_constref])) or
                  (variantdispatch and valid_for_var(para.left,false));

          if result or (para.left.resultdef.typ in [variantdef]) then
            assign_type:=voidpointertype
          else
            case para.left.resultdef.size of
              1..4:
                assign_type:=u32inttype;
              8:
                assign_type:=u64inttype;
              else
                internalerror(2007042801);
            end;
        end;

      function getvardef(sourcedef: TDef): longint;
        begin
          if is_ansistring(sourcedef) then
            result:=varStrArg
          else
          if is_unicodestring(sourcedef) then
            result:=varUStrArg
          else
          if is_interfacecom_or_dispinterface(sourcedef) then
            begin
              { distinct IDispatch and IUnknown interfaces }
              if def_is_related(tobjectdef(sourcedef),tobjectdef(search_system_type('IDISPATCH').typedef)) then
                result:=vardispatch
              else
                result:=varunknown;
            end
          else
            result:=sourcedef.getvardef;
        end;

      begin
        variantdispatch:=selfnode.resultdef.typ=variantdef;
        result:=internalstatements(statements);
        result_data:=nil;

        useresult := assigned(resultdef) and not is_void(resultdef);
        if useresult then
          begin
            { get temp for the result }
            result_data:=ctempcreatenode.create(colevarianttype,colevarianttype.size,tt_persistent,true);
            addstatement(statements,result_data);
          end;

        { first, count and check parameters }
        para:=tcallparanode(parametersnode);
        paracount:=0;
        namedparacount:=0;
        while assigned(para) do
          begin
            typecheckpass(para.left);

            { skip hidden dispinterface parameters like $self, $result,
              but count skipped variantdispatch parameters. }
            if (not variantdispatch) and (para.left.nodetype=nothingn) then
              begin
                para:=tcallparanode(para.nextpara);
                continue;
              end;
            inc(paracount);
            if assigned(para.parametername) then
              inc(namedparacount);

            { insert some extra casts }
            if para.left.nodetype=stringconstn then
              inserttypeconv_internal(para.left,cwidestringtype)

            { force automatable boolean type }
            else if is_boolean(para.left.resultdef) then
              inserttypeconv_internal(para.left,bool16type)

            { force automatable float type }
            else if is_extended(para.left.resultdef)
                and (current_settings.fputype<>fpu_none) then
              inserttypeconv_internal(para.left,s64floattype)

            else if is_shortstring(para.left.resultdef) then
              inserttypeconv_internal(para.left,cwidestringtype)

            { skip this check if we've already typecasted to automatable type }
            else if (para.left.nodetype<>nothingn) and (not is_automatable(para.left.resultdef)) then
              CGMessagePos1(para.left.fileinfo,type_e_not_automatable,para.left.resultdef.typename);

            para:=tcallparanode(para.nextpara);
          end;

        { create a temp to store parameter values }
        params:=ctempcreatenode.create(cformaltype,0,tt_persistent,false);
        addstatement(statements,params);

        calldescnode:=cdataconstnode.create;

        if not variantdispatch then  { generate a tdispdesc record }
        begin
          { dispid  }
          calldescnode.append(dispid,sizeof(dispid));
          { restype }
          if useresult then
            restype:=getvardef(resultdef)
          else
            restype:=0;
          calldescnode.appendbyte(restype);
        end;

        calldescnode.appendbyte(calltypes[calltype]);
        calldescnode.appendbyte(paracount);
        calldescnode.appendbyte(namedparacount);

        { build up parameters and description }
        para:=tcallparanode(parametersnode);
        paramssize:=0;
        names := '';
        while assigned(para) do
          begin
            { Skipped parameters are actually (varType=varError, vError=DISP_E_PARAMNOTFOUND).
              Generate only varType here, the value will be added by RTL. }
            if para.left.nodetype=nothingn then
            begin
              if variantdispatch then
                calldescnode.appendbyte(varError);
              para:=tcallparanode(para.nextpara);
              continue;
            end;

            if assigned(para.parametername) then
              begin
                if para.parametername.nodetype=stringconstn then
                  names:=names+tstringconstnode(para.parametername).value_str+#0
                else
                  internalerror(200611041);
              end;

            restype:=getvardef(para.left.resultdef);
            if is_byref_para(assignmenttype) then
              restype:=restype or $80;

            { assign the argument/parameter to the temporary location }
            { for Variants, we always pass a pointer, RTL helpers must handle it
              depending on byref bit }

            if assignmenttype=voidpointertype then
              addstatement(statements,cassignmentnode.create(
                ctypeconvnode.create_internal(ctemprefnode.create_offset(params,paramssize),
                  voidpointertype),
                ctypeconvnode.create_internal(caddrnode.create_internal(para.left),voidpointertype)))
            else
              addstatement(statements,cassignmentnode.create(
                ctypeconvnode.create_internal(ctemprefnode.create_offset(params,paramssize),
                  assignmenttype),
                ctypeconvnode.create_internal(para.left,assignmenttype)));

            inc(paramssize,max(voidpointertype.size,assignmenttype.size));
            calldescnode.appendbyte(restype);

            para.left:=nil;
            para:=tcallparanode(para.nextpara);
          end;

        { Set final size for parameter block }
        params.size:=paramssize;

        { old argument list skeleton isn't needed anymore }
        parametersnode.free;

        pvardatadef:=tpointerdef(search_system_type('PVARDATA').typedef);

        if useresult then
          resultvalue:=caddrnode.create(ctemprefnode.create(result_data))
        else
          resultvalue:=cpointerconstnode.create(0,voidpointertype);

        if variantdispatch then
          begin
            calldescnode.append(pointer(methodname)^,length(methodname));
            calldescnode.appendbyte(0);
            calldescnode.append(pointer(names)^,length(names));

            { actual call }
            vardatadef:=trecorddef(search_system_type('TVARDATA').typedef);

            addstatement(statements,ccallnode.createintern('fpc_dispinvoke_variant',
              { parameters are passed always reverted, i.e. the last comes first }
              ccallparanode.create(caddrnode.create(ctemprefnode.create(params)),
              ccallparanode.create(caddrnode.create(calldescnode),
              ccallparanode.create(ctypeconvnode.create_internal(selfnode,vardatadef),
              ccallparanode.create(ctypeconvnode.create_internal(resultvalue,pvardatadef),nil)))))
            );
          end
        else
          begin
            addstatement(statements,ccallnode.createintern('fpc_dispatch_by_id',
              { parameters are passed always reverted, i.e. the last comes first }
              ccallparanode.create(caddrnode.create(ctemprefnode.create(params)),
              ccallparanode.create(caddrnode.create(calldescnode),
              ccallparanode.create(ctypeconvnode.create_internal(selfnode,voidpointertype),
              ccallparanode.create(ctypeconvnode.create_internal(resultvalue,pvardatadef),nil)))))
            );
          end;
        addstatement(statements,ctempdeletenode.create(params));
        if useresult then
          begin
            { clean up }
            addstatement(statements,ctempdeletenode.create_normal_temp(result_data));
            addstatement(statements,ctemprefnode.create(result_data));
          end;
      end;


{****************************************************************************
                              TOBJECTINFOITEM
 ****************************************************************************}

    constructor tobjectinfoitem.create(def : tobjectdef);
      begin
        inherited create;
        objinfo := def;
      end;


{****************************************************************************
                             TCALLPARANODE
 ****************************************************************************}

    constructor tcallparanode.create(expr,next : tnode);

      begin
         inherited create(callparan,expr,next,nil);
         if not assigned(expr) then
           internalerror(200305091);
         expr.fileinfo:=fileinfo;
         callparaflags:=[];
         if expr.nodetype = typeconvn then
           ttypeconvnode(expr).warn_pointer_to_signed:=false;
      end;

    destructor tcallparanode.destroy;

      begin
         fparainit.free;
         fparacopyback.free;
         inherited destroy;
      end;


    constructor tcallparanode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        ppufile.getsmallset(callparaflags);
        fparainit:=ppuloadnode(ppufile);
        fparacopyback:=ppuloadnode(ppufile);
      end;


    procedure tcallparanode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putsmallset(callparaflags);
        ppuwritenode(ppufile,fparainit);
        ppuwritenode(ppufile,fparacopyback);
      end;


    procedure tcallparanode.buildderefimpl;
      begin
        inherited buildderefimpl;
        if assigned(fparainit) then
          fparainit.buildderefimpl;
        if assigned(fparacopyback) then
          fparacopyback.buildderefimpl;
      end;


    procedure tcallparanode.derefimpl;
      begin
        inherited derefimpl;
        if assigned(fparainit) then
          fparainit.derefimpl;
        if assigned(fparacopyback) then
          fparacopyback.derefimpl;
      end;


    function tcallparanode.dogetcopy : tnode;
      var
         n : tcallparanode;
         initcopy: tnode;
      begin
         initcopy:=nil;
         { must be done before calling inherited getcopy, because can create
           tempcreatenodes for values used in left }
         if assigned(fparainit) then
           initcopy:=fparainit.getcopy;
         n:=tcallparanode(inherited dogetcopy);
         n.callparaflags:=callparaflags;
         n.parasym:=parasym;
         n.fparainit:=initcopy;
         if assigned(fparacopyback) then
           n.fparacopyback:=fparacopyback.getcopy;
         result:=n;
      end;


    procedure tcallparanode.insertintolist(l : tnodelist);
      begin
      end;


    function tcallparanode.pass_typecheck : tnode;
      begin
        { need to use get_paratype }
        internalerror(200709251);
        result:=nil;
      end;


    function tcallparanode.pass_1 : tnode;
      begin
        { need to use firstcallparan }
        internalerror(200709252);
        result:=nil;
      end;


    procedure tcallparanode.get_paratype;
      var
        old_array_constructor : boolean;
      begin
         if assigned(right) then
          tcallparanode(right).get_paratype;
         old_array_constructor:=allow_array_constructor;
         allow_array_constructor:=true;
         if assigned(fparainit) then
          typecheckpass(fparainit);
         typecheckpass(left);
         if assigned(third) then
           typecheckpass(third);
         if assigned(fparacopyback) then
           typecheckpass(fparacopyback);
         allow_array_constructor:=old_array_constructor;
         if codegenerror then
          resultdef:=generrordef
         else
          resultdef:=left.resultdef;
      end;


    procedure tcallparanode.firstcallparan;
      begin
        if assigned(right) then
          tcallparanode(right).firstcallparan;
        if not assigned(left.resultdef) then
          get_paratype;
        if assigned(parasym) and
           (target_info.system in systems_managed_vm) and
           (parasym.varspez in [vs_var,vs_out,vs_constref]) and
           (parasym.vardef.typ<>formaldef) and
           { for record constructors }
           (left.nodetype<>nothingn) then
          handlemanagedbyrefpara(left.resultdef);

        { does it need to load RTTI? }
        if assigned(parasym) and (parasym.varspez=vs_out) and
           (cs_create_pic in current_settings.moduleswitches) and
           (
             is_rtti_managed_type(left.resultdef) or
             (
               is_open_array(resultdef) and
               is_managed_type(tarraydef(resultdef).elementdef)
             )
           ) and
           not(target_info.system in systems_garbage_collected_managed_types) then
          include(current_procinfo.flags,pi_needs_got);

        if assigned(fparainit) then
          firstpass(fparainit);
        firstpass(left);
        if assigned(fparacopyback) then
          firstpass(fparacopyback);
        if assigned(third) then
          firstpass(third);
        expectloc:=left.expectloc;
      end;


    procedure tcallparanode.insert_typeconv;
      var
        olddef  : tdef;
        hp      : tnode;
        block : tblocknode;
        statements : tstatementnode;
        temp : ttempcreatenode;
        owningprocdef: tprocdef;
      begin
         { Be sure to have the resultdef }
         if not assigned(left.resultdef) then
           typecheckpass(left);

         if (left.nodetype<>nothingn) then
           begin
             { convert loads of the function result variable into procvars
               representing the current function in case the formal parameter is
               a procvar (CodeWarrior Pascal contains the same kind of
               automatic disambiguation; you can use the function name in both
               meanings, so we cannot statically pick either the function result
               or the function definition in pexpr) }
             if (m_mac in current_settings.modeswitches) and
                (parasym.vardef.typ=procvardef) and
                is_ambiguous_funcret_load(left,owningprocdef) then
               begin
                 hp:=cloadnode.create_procvar(owningprocdef.procsym,owningprocdef,owningprocdef.procsym.owner);
                 typecheckpass(hp);
                 left.free;
                 left:=hp;
               end;

             { Convert tp procvars, this is needs to be done
               here to make the change permanent. in the overload
               choosing the changes are only made temporarily }
             if (left.resultdef.typ=procvardef) and
                not(parasym.vardef.typ in [procvardef,formaldef]) then
               begin
                 if maybe_call_procvar(left,true) then
                   resultdef:=left.resultdef
               end;

             { Remove implicitly inserted typecast to pointer for
               @procvar in macpas }
             if (m_mac_procvar in current_settings.modeswitches) and
                (parasym.vardef.typ=procvardef) and
                (left.nodetype=typeconvn) and
                is_voidpointer(left.resultdef) and
                (ttypeconvnode(left).left.nodetype=typeconvn) and
                (ttypeconvnode(ttypeconvnode(left).left).convtype=tc_proc_2_procvar) then
               begin
                 hp:=left;
                 left:=ttypeconvnode(left).left;
                 ttypeconvnode(hp).left:=nil;
                 hp.free;
               end;
             maybe_global_proc_to_nested(left,parasym.vardef);

             { Handle varargs and hidden paras directly, no typeconvs or }
             { pass_typechecking needed                                  }
             if (cpf_varargs_para in callparaflags) then
               begin
                 { this should only happen vor C varargs                    }
                 { the necessary conversions have already been performed in }
                 { tarrayconstructornode.insert_typeconvs                   }
                 set_varstate(left,vs_read,[vsf_must_be_valid]);
                 insert_varargstypeconv(left,true);
                 resultdef:=left.resultdef;
                 { also update parasym type to get the correct parameter location
                   for the new types }
                 parasym.vardef:=left.resultdef;
               end
             else
              if (vo_is_hidden_para in parasym.varoptions) then
               begin
                 set_varstate(left,vs_read,[vsf_must_be_valid]);
                 resultdef:=left.resultdef;
               end
             else
               begin

                 { Do we need arrayconstructor -> set conversion, then insert
                   it here before the arrayconstructor node breaks the tree
                   with its conversions of enum->ord }
                 if (left.nodetype=arrayconstructorn) and
                    (parasym.vardef.typ=setdef) then
                   inserttypeconv(left,parasym.vardef);

                 { set some settings needed for arrayconstructor }
                 if is_array_constructor(left.resultdef) then
                  begin
                    if left.nodetype<>arrayconstructorn then
                      internalerror(200504041);
                    if is_array_of_const(parasym.vardef) then
                     begin
                       { force variant array }
                       include(left.flags,nf_forcevaria);
                     end
                    else
                     begin
                       include(left.flags,nf_novariaallowed);
                       { now that the resultting type is know we can insert the required
                         typeconvs for the array constructor }
                       if parasym.vardef.typ=arraydef then
                         tarrayconstructornode(left).force_type(tarraydef(parasym.vardef).elementdef);
                     end;
                  end;

                 { check if local proc/func is assigned to procvar }
                 if left.resultdef.typ=procvardef then
                   test_local_to_procvar(tprocvardef(left.resultdef),parasym.vardef);

                 { test conversions }
                 if not(is_shortstring(left.resultdef) and
                        is_shortstring(parasym.vardef)) and
                    (parasym.vardef.typ<>formaldef) and
                    not(parasym.univpara) then
                   begin
                      { Process open parameters }
                      if paramanager.keep_para_array_range(parasym.varspez,parasym.vardef,aktcallnode.procdefinition.proccalloption) then
                       begin
                         { insert type conv but hold the ranges of the array }
                         olddef:=left.resultdef;
                         inserttypeconv(left,parasym.vardef);
                         left.resultdef:=olddef;
                       end
                      else
                       begin
                         check_ranges(left.fileinfo,left,parasym.vardef);
                         inserttypeconv(left,parasym.vardef);
                       end;
                      if codegenerror then
                        exit;
                   end;

                { truncate shortstring value parameters at the caller side if }
                { they are passed by value (if passed by reference, then the  }
                { callee will truncate when copying in the string)            }
                { This happens e.g. on x86_64 for small strings               }
                 if is_shortstring(left.resultdef) and
                    is_shortstring(parasym.vardef) and
                    (parasym.varspez=vs_value) and
                    not paramanager.push_addr_param(parasym.varspez,parasym.vardef,
                          aktcallnode.procdefinition.proccalloption) and
                    ((is_open_string(left.resultdef) and
                      (tstringdef(parasym.vardef).len < 255)) or
                     (not is_open_string(left.resultdef) and
                      { when a stringconstn is typeconverted, then only its  }
                      { def is modified, not the contents (needed because in }
                      { Delphi/TP, if you pass a longer string to a const    }
                      { parameter, then the callee has to see this longer    }
                      { string)                                              }
                      (((left.nodetype<>stringconstn) and
                        (tstringdef(parasym.vardef).len<tstringdef(left.resultdef).len)) or
                       ((left.nodetype=stringconstn) and
                        (tstringdef(parasym.vardef).len<tstringconstnode(left).len))))) then
                   begin
                     block:=internalstatements(statements);
                     { temp for the new string }
                     temp:=ctempcreatenode.create(parasym.vardef,parasym.vardef.size,
                       tt_persistent,true);
                     addstatement(statements,temp);
                     { assign parameter to temp }
                     addstatement(statements,cassignmentnode.create(ctemprefnode.create(temp),left));
                     left:=nil;
                     { release temp after next use }
                     addstatement(statements,ctempdeletenode.create_normal_temp(temp));
                     addstatement(statements,ctemprefnode.create(temp));
                     typecheckpass(tnode(block));
                     left:=block;
                   end;

                 { check var strings }
                 if (cs_strict_var_strings in current_settings.localswitches) and
                    is_shortstring(left.resultdef) and
                    is_shortstring(parasym.vardef) and
                    (parasym.varspez in [vs_out,vs_var,vs_constref]) and
                    not(is_open_string(parasym.vardef)) and
                    not(equal_defs(left.resultdef,parasym.vardef)) then
                   begin
                     CGMessagePos(left.fileinfo,type_e_strict_var_string_violation);
                   end;

                 { passing a value to an "univ" parameter implies an explicit
                   typecast to the parameter type. Must be done before the
                   valid_for_var() check, since the typecast can result in
                   an invalid lvalue in case of var/out parameters. }
                 if (parasym.univpara) then
                   begin
                     { load procvar if a procedure is passed }
                     if ((m_tp_procvar in current_settings.modeswitches) or
                         (m_mac_procvar in current_settings.modeswitches)) and
                        (left.nodetype=calln) and
                        (is_void(left.resultdef)) then
                       begin
                         load_procvar_from_calln(left);
                         { load_procvar_from_calln() creates a loadn for a
                           a procedure, which means that the type conversion
                           below will type convert the first instruction
                           bytes of the procedure -> convert to a procvar }
                         left:=ctypeconvnode.create_proc_to_procvar(left);
                         typecheckpass(left);
                       end;
                     inserttypeconv_explicit(left,parasym.vardef);
                   end;

                 { Handle formal parameters separate }
                 if (parasym.vardef.typ=formaldef) then
                   begin
                     { load procvar if a procedure is passed }
                     if ((m_tp_procvar in current_settings.modeswitches) or
                         (m_mac_procvar in current_settings.modeswitches)) and
                        (left.nodetype=calln) and
                        (is_void(left.resultdef)) then
                       load_procvar_from_calln(left);

                     case parasym.varspez of
                       vs_var,
                       vs_constref,
                       vs_out :
                         begin
                           if not valid_for_formal_var(left,true) then
                            CGMessagePos(left.fileinfo,parser_e_illegal_parameter_list)
                           else if (target_info.system in systems_managed_vm) then
                             handlemanagedbyrefpara(left.resultdef);
                         end;
                       vs_const :
                         begin
                           if not valid_for_formal_const(left,true) then
                            CGMessagePos(left.fileinfo,parser_e_illegal_parameter_list)
                           else if (target_info.system in systems_managed_vm) and
                              (left.resultdef.typ in [orddef,floatdef]) then
                             begin
                               left:=cinlinenode.create(in_box_x,false,ccallparanode.create(left,nil));
                               typecheckpass(left);
                             end;
                         end;
                     end;
                   end
                 else
                   begin
                     { check if the argument is allowed }
                     if (parasym.varspez in [vs_out,vs_var]) then
                       valid_for_var(left,true);
                   end;

                 if parasym.varspez in [vs_var,vs_out,vs_constref] then
                   set_unique(left);

                  case parasym.varspez of
                    vs_out :
                      begin
                        { first set written separately to avoid false }
                        { uninitialized warnings (tbs/tb0542)         }
                        set_varstate(left,vs_written,[]);
                        set_varstate(left,vs_readwritten,[]);
                      end;
                    vs_var,
                    vs_constref:
                      set_varstate(left,vs_readwritten,[vsf_must_be_valid,vsf_use_hints]);
                    else
                      set_varstate(left,vs_read,[vsf_must_be_valid]);
                  end;
                 { must only be done after typeconv PM }
                 resultdef:=parasym.vardef;
               end;
            end;

         { process next node }
         if assigned(right) then
           tcallparanode(right).insert_typeconv;
      end;


    function tcallparanode.can_be_inlined: boolean;
      var
        n: tnode;
      begin
        n:=left;
        result:=false;
        while assigned(n) and
              (n.nodetype=typeconvn) do
          begin
            { look for type conversion nodes which convert a }
            { refcounted type into a non-refcounted type     }
            if not is_managed_type(n.resultdef) and
               is_managed_type(ttypeconvnode(n).left.resultdef) then
              exit;
            n:=ttypeconvnode(n).left;
          end;
        { also check for dereferencing constant pointers, like }
        { tsomerecord(nil^) passed to a const r: tsomerecord   }
        { parameter                                           }
        if (n.nodetype=derefn) then
          begin
            repeat
              n:=tunarynode(n).left;
            until (n.nodetype<>typeconvn);
            if (n.nodetype in [niln,pointerconstn]) then
              exit
          end;
        result:=true;
      end;


    function check_contains_stack_tainting_call(var n: tnode; arg: pointer): foreachnoderesult;
      begin
        if (n.nodetype=calln) and
           tcallnode(n).procdefinition.stack_tainting_parameter(callerside) then
          result:=fen_norecurse_true
        else
          result:=fen_false;
      end;


    function tcallparanode.contains_stack_tainting_call: boolean;
      begin
        result:=foreachnodestatic(pm_postprocess,left,@check_contains_stack_tainting_call,nil);
      end;


    procedure tcallparanode.init_contains_stack_tainting_call_cache;
      begin
        fcontains_stack_tainting_call_cached:=contains_stack_tainting_call;
      end;


    function tcallparanode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
          fparainit.isequal(tcallparanode(p).fparainit) and
          fparacopyback.isequal(tcallparanode(p).fparacopyback) and
          (callparaflags = tcallparanode(p).callparaflags)
          ;
      end;


    procedure tcallparanode.printnodetree(var t:text);
      begin
        printnodelist(t);
      end;


{****************************************************************************
                                 TCALLNODE
 ****************************************************************************}

    constructor tcallnode.create(l:tnode;v : tprocsym;st : TSymtable; mp: tnode; callflags:tcallnodeflags);
      begin
         inherited create(calln,l,nil);
         symtableprocentry:=v;
         symtableproc:=st;
         callnodeflags:=callflags+[cnf_return_value_used];
         methodpointer:=mp;
         callinitblock:=nil;
         callcleanupblock:=nil;
         procdefinition:=nil;
         funcretnode:=nil;
         paralength:=-1;
         varargsparas:=nil;
      end;


    constructor tcallnode.create_procvar(l,r:tnode);
      begin
         inherited create(calln,l,r);
         symtableprocentry:=nil;
         symtableproc:=nil;
         methodpointer:=nil;
         callinitblock:=nil;
         callcleanupblock:=nil;
         procdefinition:=nil;
         callnodeflags:=[cnf_return_value_used];
         funcretnode:=nil;
         paralength:=-1;
         varargsparas:=nil;
      end;


     constructor tcallnode.createintern(const name: string; params: tnode);
       var
         srsym: tsym;
       begin
         srsym := tsym(systemunit.Find(name));
         if not assigned(srsym) and
            (cs_compilesystem in current_settings.moduleswitches) then
           srsym := tsym(systemunit.Find(upper(name)));
         if not assigned(srsym) or
            (srsym.typ<>procsym) then
           Message1(cg_f_unknown_compilerproc,name);
         create(params,tprocsym(srsym),srsym.owner,nil,[]);
       end;


     constructor tcallnode.createinternfromunit(const fromunit, procname: string; params: tnode);
       var
         srsym: tsym;
         srsymtable: tsymtable;
       begin
         srsym:=nil;
         if not searchsym_in_named_module(fromunit,procname,srsym,srsymtable) or
            (srsym.typ<>procsym) then
           Message1(cg_f_unknown_compilerproc,fromunit+'.'+procname);
         create(params,tprocsym(srsym),srsymtable,nil,[]);
       end;


    constructor tcallnode.createinternres(const name: string; params: tnode; res:tdef);
      var
        pd : tprocdef;
      begin
        createintern(name,params);
        typedef:=res;
        include(callnodeflags,cnf_typedefset);
        pd:=tprocdef(symtableprocentry.ProcdefList[0]);
        { both the normal and specified resultdef either have to be returned via a }
        { parameter or not, but no mixing (JM)                                      }
        if paramanager.ret_in_param(typedef,pd) xor
          paramanager.ret_in_param(pd.returndef,pd) then
          internalerror(2001082911);
      end;


    constructor tcallnode.createinternresfromunit(const fromunit, procname: string; params: tnode; res:tdef);
      var
        pd : tprocdef;
      begin
        createinternfromunit(fromunit,procname,params);
        typedef:=res;
        include(callnodeflags,cnf_typedefset);
        pd:=tprocdef(symtableprocentry.ProcdefList[0]);
        { both the normal and specified resultdef either have to be returned via a }
        { parameter or not, but no mixing (JM)                                      }
        if paramanager.ret_in_param(typedef,pd) xor
          paramanager.ret_in_param(pd.returndef,pd) then
          internalerror(200108291);
      end;


    constructor tcallnode.createinternreturn(const name: string; params: tnode; returnnode : tnode);
      begin
        createintern(name,params);
        funcretnode:=returnnode;
      end;


    constructor tcallnode.createinternmethod(mp: tnode; const name: string; params: tnode);
      var
        ps: tsym;
        recdef: tabstractrecorddef;
      begin
        typecheckpass(mp);
        if mp.resultdef.typ=classrefdef then
          recdef:=tabstractrecorddef(tclassrefdef(mp.resultdef).pointeddef)
        else
          recdef:=tabstractrecorddef(mp.resultdef);
        ps:=search_struct_member(recdef,name);
        if not assigned(ps) or
           (ps.typ<>procsym) then
          internalerror(2011062806);
        create(params,tprocsym(ps),ps.owner,mp,[]);
      end;


    constructor tcallnode.createinternmethodres(mp: tnode; const name: string; params: tnode; res: tdef);
      begin
        createinternmethod(mp,name,params);
        typedef:=res;
        include(callnodeflags,cnf_typedefset)
      end;


    destructor tcallnode.destroy;
      begin
         methodpointer.free;
         callinitblock.free;
         callcleanupblock.free;
         funcretnode.free;
         if assigned(varargsparas) then
           varargsparas.free;
         stringdispose(fobjcforcedprocname);
         inherited destroy;
      end;


    constructor tcallnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        callinitblock:=tblocknode(ppuloadnode(ppufile));
        methodpointer:=ppuloadnode(ppufile);
        callcleanupblock:=tblocknode(ppuloadnode(ppufile));
        funcretnode:=ppuloadnode(ppufile);
        inherited ppuload(t,ppufile);
        ppufile.getderef(symtableprocentryderef);
{ TODO: FIXME: No withsymtable support}
        symtableproc:=nil;
        ppufile.getderef(procdefinitionderef);
        ppufile.getsmallset(callnodeflags);
      end;


    procedure tcallnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        ppuwritenode(ppufile,callinitblock);
        ppuwritenode(ppufile,methodpointer);
        ppuwritenode(ppufile,callcleanupblock);
        ppuwritenode(ppufile,funcretnode);
        inherited ppuwrite(ppufile);
        ppufile.putderef(symtableprocentryderef);
        ppufile.putderef(procdefinitionderef);
        ppufile.putsmallset(callnodeflags);
      end;


    procedure tcallnode.buildderefimpl;
      begin
        inherited buildderefimpl;
        symtableprocentryderef.build(symtableprocentry);
        procdefinitionderef.build(procdefinition);
        if assigned(methodpointer) then
          methodpointer.buildderefimpl;
        if assigned(callinitblock) then
          callinitblock.buildderefimpl;
        if assigned(callcleanupblock) then
          callcleanupblock.buildderefimpl;
        if assigned(funcretnode) then
          funcretnode.buildderefimpl;
      end;


    procedure tcallnode.derefimpl;
      var
        pt : tcallparanode;
        i  : integer;
      begin
        inherited derefimpl;
        symtableprocentry:=tprocsym(symtableprocentryderef.resolve);
        if assigned(symtableprocentry) then
          symtableproc:=symtableprocentry.owner;
        procdefinition:=tabstractprocdef(procdefinitionderef.resolve);
        if assigned(methodpointer) then
          methodpointer.derefimpl;
        if assigned(callinitblock) then
          callinitblock.derefimpl;
        if assigned(callcleanupblock) then
          callcleanupblock.derefimpl;
        if assigned(funcretnode) then
          funcretnode.derefimpl;
        { generic method has no procdefinition }
        if assigned(procdefinition) then
          begin
            { Connect parasyms }
            pt:=tcallparanode(left);
            while assigned(pt) and
                  (cpf_varargs_para in pt.callparaflags) do
              pt:=tcallparanode(pt.right);
            for i:=procdefinition.paras.count-1 downto 0 do
              begin
                if not assigned(pt) then
                  internalerror(200311077);
                pt.parasym:=tparavarsym(procdefinition.paras[i]);
                pt:=tcallparanode(pt.right);
              end;
            if assigned(pt) then
              internalerror(200311078);
          end;
      end;


    function tcallnode.dogetcopy : tnode;
      var
        n : tcallnode;
        i : integer;
        hp,hpn : tparavarsym;
        oldleft : tnode;
      begin
        { Need to use a hack here to prevent the parameters from being copied.
          The parameters must be copied between callinitblock/callcleanupblock because
          they can reference methodpointer }
        oldleft:=left;
        left:=nil;
        n:=tcallnode(inherited dogetcopy);
        left:=oldleft;
        n.symtableprocentry:=symtableprocentry;
        n.symtableproc:=symtableproc;
        n.procdefinition:=procdefinition;
        n.typedef := typedef;
        n.callnodeflags := callnodeflags;
        if assigned(callinitblock) then
          n.callinitblock:=tblocknode(callinitblock.dogetcopy)
        else
          n.callinitblock:=nil;
        { callinitblock is copied, now references to the temp will also be copied
          correctly. We can now copy the parameters, funcret and methodpointer }
        if assigned(left) then
          n.left:=left.dogetcopy
        else
          n.left:=nil;
        if assigned(methodpointer) then
          n.methodpointer:=methodpointer.dogetcopy
        else
          n.methodpointer:=nil;
        { must be copied before the funcretnode, because the callcleanup block
          may contain a ttempdeletenode that sets the tempinfo of the
          corresponding temp to ti_nextref_set_hookoncopy_nil, and this nextref
          itself may be the funcretnode }
        if assigned(callcleanupblock) then
          n.callcleanupblock:=tblocknode(callcleanupblock.dogetcopy)
        else
          n.callcleanupblock:=nil;
        if assigned(funcretnode) then
          n.funcretnode:=funcretnode.dogetcopy
        else
          n.funcretnode:=nil;
        if assigned(varargsparas) then
         begin
           n.varargsparas:=tvarargsparalist.create(true);
           for i:=0 to varargsparas.count-1 do
             begin
               hp:=tparavarsym(varargsparas[i]);
               hpn:=tparavarsym.create(hp.realname,hp.paranr,hp.varspez,hp.vardef,[]);
               n.varargsparas.add(hpn);
             end;
         end
        else
         n.varargsparas:=nil;
        result:=n;
      end;


    function tcallnode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
          (symtableprocentry = tcallnode(p).symtableprocentry) and
          (procdefinition = tcallnode(p).procdefinition) and
          (methodpointer.isequal(tcallnode(p).methodpointer)) and
          (((cnf_typedefset in callnodeflags) and (cnf_typedefset in tcallnode(p).callnodeflags) and
            (equal_defs(typedef,tcallnode(p).typedef))) or
           (not(cnf_typedefset in callnodeflags) and not(cnf_typedefset in tcallnode(p).callnodeflags)));
      end;


    procedure tcallnode.printnodedata(var t:text);
      begin
        if assigned(procdefinition) and
           (procdefinition.typ=procdef) then
          writeln(t,printnodeindention,'proc = ',tprocdef(procdefinition).fullprocname(true))
        else
          begin
            if assigned(symtableprocentry) then
              writeln(t,printnodeindention,'proc = ',symtableprocentry.name)
            else
              writeln(t,printnodeindention,'proc = <nil>');
          end;

        if assigned(methodpointer) then
          begin
            writeln(t,printnodeindention,'methodpointer =');
            printnode(t,methodpointer);
          end;

        if assigned(callinitblock) then
          begin
            writeln(t,printnodeindention,'callinitblock =');
            printnode(t,callinitblock);
          end;

        if assigned(callcleanupblock) then
          begin
            writeln(t,printnodeindention,'callcleanupblock =');
            printnode(t,callcleanupblock);
          end;

        if assigned(right) then
          begin
            writeln(t,printnodeindention,'right =');
            printnode(t,right);
          end;

        if assigned(left) then
          begin
            writeln(t,printnodeindention,'left =');
            printnode(t,left);
          end;
      end;


    procedure tcallnode.insertintolist(l : tnodelist);
      begin
      end;


    procedure tcallnode.add_init_statement(n:tnode);
      var
        lastinitstatement : tstatementnode;
      begin
        if not assigned(callinitblock) then
          callinitblock:=internalstatements(lastinitstatement)
        else
          lastinitstatement:=laststatement(callinitblock);
        { all these nodes must be immediately typechecked, because this routine }
        { can be called from pass_1 (i.e., after typecheck has already run) and }
        { moreover, the entire blocks themselves are also only typechecked in   }
        { pass_1, while the the typeinfo is already required after the          }
        { typecheck pass for simplify purposes (not yet perfect, because the    }
        { statementnodes themselves are not typechecked this way)               }
        typecheckpass(n);
        addstatement(lastinitstatement,n);
      end;


    procedure tcallnode.add_done_statement(n:tnode);
      var
        lastdonestatement : tstatementnode;
      begin
        if not assigned(callcleanupblock) then
          callcleanupblock:=internalstatements(lastdonestatement)
        else
          lastdonestatement:=laststatement(callcleanupblock);
        { see comments in add_init_statement }
        typecheckpass(n);
        addstatement(lastdonestatement,n);
      end;


    function tcallnode.para_count:longint;
      var
        ppn : tcallparanode;
      begin
        result:=0;
        ppn:=tcallparanode(left);
        while assigned(ppn) do
          begin
            if not(assigned(ppn.parasym) and
                   (vo_is_hidden_para in ppn.parasym.varoptions)) then
              inc(result);
            ppn:=tcallparanode(ppn.right);
          end;
      end;


    function tcallnode.required_para_count: longint;
      var
        ppn : tcallparanode;
      begin
        result:=0;
        ppn:=tcallparanode(left);
        while assigned(ppn) do
          begin
            if not(assigned(ppn.parasym) and
                   ((vo_is_hidden_para in ppn.parasym.varoptions) or
                    assigned(ppn.parasym.defaultconstsym))) then
              inc(result);
            ppn:=tcallparanode(ppn.right);
          end;
      end;


    function tcallnode.is_simple_para_load(p:tnode; may_be_in_reg: boolean):boolean;
      var
        hp : tnode;
      begin
        hp:=p;
        while assigned(hp) and
              (hp.nodetype=typeconvn) and
              (ttypeconvnode(hp).convtype=tc_equal) do
          hp:=tunarynode(hp).left;
        result:=(hp.nodetype in [typen,loadvmtaddrn,loadn,temprefn,arrayconstructorn,addrn]);
        if result and
           not(may_be_in_reg) then
          case hp.nodetype of
            loadn:
              result:=(tabstractvarsym(tloadnode(hp).symtableentry).varregable in [vr_none,vr_addr]);
            temprefn:
              result:=not(ti_may_be_in_reg in ttemprefnode(hp).tempinfo^.flags);
          end;
      end;

    function look_for_call(var n: tnode; arg: pointer): foreachnoderesult;
      begin
        case n.nodetype of
          calln,asn:
            result := fen_norecurse_true;
          typen,loadvmtaddrn,loadn,temprefn,arrayconstructorn:
            result := fen_norecurse_false;
        else
          result := fen_false;
        end;
      end;

    procedure tcallnode.maybe_load_in_temp(var p:tnode);
      var
        loadp,
        refp  : tnode;
        hdef : tdef;
        ptemp : ttempcreatenode;
        usederef : boolean;
      begin
        { Load all complex loads into a temp to prevent
          double calls to a function. We can't simply check for a hp.nodetype=calln }
        if assigned(p) and
           foreachnodestatic(p,@look_for_call,nil) then
          begin
            { temp create }
            usederef:=(p.resultdef.typ in [arraydef,recorddef]) or
                      is_shortstring(p.resultdef) or
                      is_object(p.resultdef);

            if usederef then
              hdef:=getpointerdef(p.resultdef)
            else
              hdef:=p.resultdef;

            ptemp:=ctempcreatenode.create(hdef,hdef.size,tt_persistent,true);
            if usederef then
              begin
                loadp:=caddrnode.create_internal(p);
                refp:=cderefnode.create(ctemprefnode.create(ptemp));
              end
            else
              begin
                loadp:=p;
                refp:=ctemprefnode.create(ptemp)
              end;
            add_init_statement(ptemp);
            add_init_statement(cassignmentnode.create(
                ctemprefnode.create(ptemp),
                loadp));
            add_done_statement(ctempdeletenode.create(ptemp));
            { new tree is only a temp reference }
            p:=refp;
            typecheckpass(p);
          end;
      end;


    function tcallnode.gen_high_tree(var p:tnode;paradef:tdef):tnode;
      { When passing an array to an open array, or a string to an open string,
        some code is needed that generates the high bound of the array. This
        function returns a tree containing the nodes for it. }
      var
        temp: tnode;
        len : integer;
        loadconst : boolean;
        hightree,l,r : tnode;
        defkind: tdeftyp;
      begin
        len:=-1;
        loadconst:=true;
        hightree:=nil;
        { constant strings are internally stored as array of char, but if the
          parameter is a string also treat it like one  }
        defkind:=p.resultdef.typ;
        if (p.nodetype=stringconstn) and
           (paradef.typ=stringdef) then
          defkind:=stringdef;
        case defkind of
          arraydef :
            begin
              if (paradef.typ<>arraydef) then
                internalerror(200405241);
              { passing a string to an array of char }
              if (p.nodetype=stringconstn) and
                 is_char(tarraydef(paradef).elementdef) then
                begin
                  len:=tstringconstnode(p).len;
                  if len>0 then
                   dec(len);
                end
              else
              { handle special case of passing an single array to an array of array }
              if compare_defs(tarraydef(paradef).elementdef,p.resultdef,nothingn)>=te_equal then
                len:=0
              else
                begin
                  { handle via a normal inline in_high_x node }
                  loadconst:=false;
                  { slice? }
                  if (p.nodetype=inlinen) and (tinlinenode(p).inlinenumber=in_slice_x) then
                    with Tcallparanode(Tinlinenode(p).left) do
                      begin
                        {Array slice using slice builtin function.}
                        l:=Tcallparanode(right).left;
                        hightree:=caddnode.create(subn,l,genintconstnode(1));
                        Tcallparanode(right).left:=nil;

                        {Remove the inline node.}
                        temp:=p;
                        p:=left;
                        Tcallparanode(tinlinenode(temp).left).left:=nil;
                        temp.free;

                        typecheckpass(hightree);
                      end
                  else if (p.nodetype=vecn) and (Tvecnode(p).right.nodetype=rangen) then
                    begin
                      {Array slice using .. operator.}
                      with Trangenode(Tvecnode(p).right) do
                        begin
                          l:=left;  {Get lower bound.}
                          r:=right; {Get upper bound.}
                        end;
                      {In the procedure the array range is 0..(upper_bound-lower_bound).}
                      hightree:=caddnode.create(subn,r,l);

                      {Replace the rangnode in the tree by its lower_bound, and
                       dispose the rangenode.}
                      temp:=Tvecnode(p).right;
                      Tvecnode(p).right:=l.getcopy;

                      {Typecheckpass can only be performed *after* the l.getcopy since it
                       can modify the tree, and l is in the hightree.}
                      typecheckpass(hightree);

                      with Trangenode(temp) do
                        begin
                          left:=nil;
                          right:=nil;
                        end;
                      temp.free;

                      {Tree changed from p[l..h] to p[l], recalculate resultdef.}
                      p.resultdef:=nil;
                      typecheckpass(p);
                    end
                  else
                    begin
                      maybe_load_in_temp(p);
                      hightree:=geninlinenode(in_high_x,false,p.getcopy);
                      typecheckpass(hightree);
                      { only substract low(array) if it's <> 0 }
                      temp:=geninlinenode(in_low_x,false,p.getcopy);
                      typecheckpass(temp);
                      if (temp.nodetype <> ordconstn) or
                         (tordconstnode(temp).value <> 0) then
                        hightree := caddnode.create(subn,hightree,temp)
                      else
                        temp.free;
                    end;
                end;
            end;
          stringdef :
            begin
              if is_open_string(paradef) then
               begin
                 { a stringconstn is not a simple parameter and hence would be
                   loaded in a temp, but in that case the high() node
                     a) goes wrong (it cannot deal with a temp node)
                     b) would give a generic result instead of one specific to
                        this constant string
                 }
                 if p.nodetype<>stringconstn then
                   maybe_load_in_temp(p);
                 { handle via a normal inline in_high_x node }
                 loadconst := false;
                 hightree := geninlinenode(in_high_x,false,p.getcopy);
               end
              else
               { handle special case of passing an single string to an array of string }
               if compare_defs(tarraydef(paradef).elementdef,p.resultdef,nothingn)>=te_equal then
                len:=0
              else
               { passing a string to an array of char }
               if (p.nodetype=stringconstn) and
                  is_char(tarraydef(paradef).elementdef) then
                 begin
                   len:=tstringconstnode(p).len;
                   if len>0 then
                    dec(len);
                 end
              else
                begin
                  maybe_load_in_temp(p);
                  hightree:=caddnode.create(subn,geninlinenode(in_length_x,false,p.getcopy),
                                            cordconstnode.create(1,sinttype,false));
                  loadconst:=false;
                end;
           end;
        else
          len:=0;
        end;
        if loadconst then
          hightree:=cordconstnode.create(len,sinttype,true)
        else
          begin
            if not assigned(hightree) then
              internalerror(200304071);
            { Need to use explicit, because it can also be a enum }
            hightree:=ctypeconvnode.create_internal(hightree,sinttype);
          end;
        result:=hightree;
      end;


    function tcallnode.gen_procvar_context_tree:tnode;
      begin
        { Load tmehodpointer(right).self (either self or parentfp) }
        result:=genloadfield(ctypeconvnode.create_internal(
          right.getcopy,methodpointertype),
          'self');
      end;


    function tcallnode.gen_self_tree:tnode;
      var
        selftree : tnode;
        selfdef  : tdef;
        temp     : ttempcreatenode;
      begin
        selftree:=nil;

        { When methodpointer was a callnode we must load it first into a
          temp to prevent processing the callnode twice }
        if (methodpointer.nodetype=calln) then
          internalerror(200405121);

        { Objective-C: objc_convert_to_message_send() already did all necessary
          transformation on the methodpointer }
        if (procdefinition.typ=procdef) and
           (po_objc in tprocdef(procdefinition).procoptions) then
          selftree:=methodpointer.getcopy
        { inherited }
        else if (cnf_inherited in callnodeflags) then
          begin
            selftree:=load_self_node;
           { we can call an inherited class static/method from a regular method
             -> self node must change from instance pointer to vmt pointer)
           }
           if (procdefinition.procoptions*[po_classmethod,po_staticmethod] <> []) and
              (selftree.resultdef.typ<>classrefdef) then
             selftree:=cloadvmtaddrnode.create(selftree);
          end
        else
          { constructors }
          if (procdefinition.proctypeoption=potype_constructor) then
            begin
              { push 0 as self when allocation is needed }
              if (methodpointer.resultdef.typ=classrefdef) or
                 (cnf_new_call in callnodeflags) then
                if not is_javaclass(tdef(procdefinition.owner.defowner)) then
                  selftree:=cpointerconstnode.create(0,voidpointertype)
                else
                 { special handling for Java constructors, handled in
                   tjvmcallnode.extra_pre_call_code }
                  selftree:=cnothingnode.create
              else
                begin
                  if methodpointer.nodetype=typen then
                    if (methodpointer.resultdef.typ<>objectdef) then
                      begin
                        if not(target_info.system in systems_jvm) then
                          begin
                            { TSomeRecord.Constructor call. We need to allocate }
                            { self node as a temp node of the result type       }
                            temp:=ctempcreatenode.create(methodpointer.resultdef,methodpointer.resultdef.size,tt_persistent,false);
                            add_init_statement(temp);
                            add_done_statement(ctempdeletenode.create_normal_temp(temp));
                            selftree:=ctemprefnode.create(temp);
                          end
                        else
                          begin
                            { special handling for Java constructors, handled in
                              tjvmcallnode.extra_pre_call_code }
                            selftree:=cnothingnode.create
                          end;
                      end
                    else
                      selftree:=load_self_node
                  else
                    selftree:=methodpointer.getcopy;
                end;
            end
        else
          { Calling a static/class method }
          if (po_classmethod in procdefinition.procoptions) or
             (po_staticmethod in procdefinition.procoptions) then
            begin
              if (procdefinition.typ<>procdef) then
                internalerror(200305062);
              { if the method belongs to a helper then we need to use the
                extended type for references to Self }
              if is_objectpascal_helper(tprocdef(procdefinition).struct) then
                selfdef:=tobjectdef(tprocdef(procdefinition).struct).extendeddef
              else
                selfdef:=tprocdef(procdefinition).struct;
              if ((selfdef.typ in [recorddef,objectdef]) and
                  (oo_has_vmt in tabstractrecorddef(selfdef).objectoptions)) or
                 { all Java classes have a "VMT" }
                 (target_info.system in systems_jvm) then
                begin
                  { we only need the vmt, loading self is not required and there is no
                    need to check for typen, because that will always get the
                    loadvmtaddrnode added }
                  selftree:=methodpointer.getcopy;
                  if (methodpointer.resultdef.typ<>classrefdef) or
                     (methodpointer.nodetype = typen) then
                    selftree:=cloadvmtaddrnode.create(selftree);
                end
              else
                selftree:=cpointerconstnode.create(0,voidpointertype);
            end
        else
          begin
            if methodpointer.nodetype=typen then
              selftree:=load_self_node
            else
              selftree:=methodpointer.getcopy;
          end;
        result:=selftree;
      end;


    procedure tcallnode.register_created_object_types;

      function checklive(def: tdef): boolean;
        begin
          if assigned(current_procinfo) and
             not(po_inline in current_procinfo.procdef.procoptions) and
             not wpoinfomanager.symbol_live(current_procinfo.procdef.mangledname) then
            begin
{$ifdef debug_deadcode}
              writeln(' NOT adding creadion of ',def.typename,' because performed in dead stripped proc: ',current_procinfo.procdef.typename);
{$endif debug_deadcode}
              result:=false;
            end
          else
            result:=true;
        end;

      var
        crefdef,
        systobjectdef : tdef;
      begin
        { only makes sense for methods }
        if not assigned(methodpointer) then
          exit;
        if (methodpointer.resultdef.typ=classrefdef) then
          begin
            { constructor call via classreference => allocate memory }
            if (procdefinition.proctypeoption=potype_constructor) then
              begin
                { Only a typenode can be passed when it is called with <class of xx>.create }
                if (methodpointer.nodetype=typen) then
                  begin
                    if checklive(methodpointer.resultdef) then
                      { we know the exact class type being created }
                      tclassrefdef(methodpointer.resultdef).pointeddef.register_created_object_type
                  end
                else
                  begin
                    { the loadvmtaddrnode is already created in case of classtype.create }
                    if (methodpointer.nodetype=loadvmtaddrn) and
                       (tloadvmtaddrnode(methodpointer).left.nodetype=typen) then
                      begin
                        if checklive(methodpointer.resultdef) then
                          tclassrefdef(methodpointer.resultdef).pointeddef.register_created_object_type
                      end
                    else
                      begin
                        if checklive(methodpointer.resultdef) then
                          begin
                            { special case: if the classref comes from x.classtype (with classtype,
                              being tobject.classtype) then the created instance is x or a descendant
                              of x (rather than tobject or a descendant of tobject)
                            }
                            systobjectdef:=search_system_type('TOBJECT').typedef;
                            if (methodpointer.nodetype=calln) and
                               { not a procvar call }
                               not assigned(right) and
                               { procdef is owned by system.tobject }
                               (tprocdef(tcallnode(methodpointer).procdefinition).owner.defowner=systobjectdef) and
                               { we're calling system.tobject.classtype }
                               (tcallnode(methodpointer).symtableprocentry.name='CLASSTYPE') and
                               { could again be a classrefdef, but unlikely }
                               (tcallnode(methodpointer).methodpointer.resultdef.typ=objectdef) and
                               { don't go through this trouble if it was already a tobject }
                               (tcallnode(methodpointer).methodpointer.resultdef<>systobjectdef) then
                              begin
                                { register this object type as classref, so all descendents will also
                                  be marked as instantiatable (only the pointeddef will actually be
                                  recorded, so it's no problem that the clasrefdef is only temporary)
                                }
                                crefdef:=tclassrefdef.create(tcallnode(methodpointer).methodpointer.resultdef);
                                { and register it }
                                crefdef.register_created_object_type;
                              end
                             else
                               { the created class can be any child class as well -> register classrefdef }
                               methodpointer.resultdef.register_created_object_type;
                          end;
                      end;
                  end;
              end
          end
        else
        { Old style object }
         if is_object(methodpointer.resultdef) then
          begin
            { constructor with extended syntax called from new }
            if (cnf_new_call in callnodeflags) then
              begin
                if checklive(methodpointer.resultdef) then
                  methodpointer.resultdef.register_created_object_type;
              end
            else
            { normal object call like obj.proc }
              if not(cnf_dispose_call in callnodeflags) and
                 not(cnf_inherited in callnodeflags) and
                 not(cnf_member_call in callnodeflags) then
                begin
                  if (procdefinition.proctypeoption=potype_constructor) then
                    begin
                      if (methodpointer.nodetype<>typen) and
                         checklive(methodpointer.resultdef) then
                        methodpointer.resultdef.register_created_object_type;
                    end
                end;
          end;
       end;


    function tcallnode.get_expect_loc: tcgloc;
      var
        realresdef: tstoreddef;
      begin
        if not assigned(typedef) then
          realresdef:=tstoreddef(resultdef)
        else
          realresdef:=tstoreddef(typedef);
        if realresdef.is_intregable then
          result:=LOC_REGISTER
        else if (realresdef.typ=floatdef) and
          not(cs_fp_emulation in current_settings.moduleswitches) then
          if use_vectorfpu(realresdef) then
            result:=LOC_MMREGISTER
          else
            result:=LOC_FPUREGISTER
        else
          result:=LOC_REFERENCE
      end;


    procedure tcallnode.objc_convert_to_message_send;
      var
        block,
        selftree      : tnode;
        statements    : tstatementnode;
        field         : tfieldvarsym;
        temp          : ttempcreatenode;
        selfrestype,
        objcsupertype : tdef;
        srsym         : tsym;
        srsymtable    : tsymtable;
        msgsendname   : string;
      begin
        if not(m_objectivec1 in current_settings.modeswitches) then
          Message(parser_f_modeswitch_objc_required);
        { typecheck pass must already have run on the call node,
          because pass1 calls this method
        }

        { default behaviour: call objc_msgSend and friends;
          64 bit targets for Mac OS X can override this as they
          can call messages via an indirect function call similar to
          dynamically linked functions, ARM maybe as well (not checked)

          Which variant of objc_msgSend is used depends on the
          result type, and on whether or not it's an inherited call.
        }

        { make sure we don't perform this transformation twice in case
          firstpass would be called multiple times }
        include(callnodeflags,cnf_objc_processed);

        { make sure the methodpointer doesn't get translated into a call
          as well (endless loop) }
        if methodpointer.nodetype=loadvmtaddrn then
          tloadvmtaddrnode(methodpointer).forcall:=true;

        { A) set the appropriate objc_msgSend* variant to call }

        { record returned via implicit pointer }
        if paramanager.ret_in_param(resultdef,procdefinition) then
          begin
            if not(cnf_inherited in callnodeflags) then
              msgsendname:='OBJC_MSGSEND_STRET'
{$if defined(onlymacosx10_6) or defined(arm) }
            else if (target_info.system in systems_objc_nfabi) then
              msgsendname:='OBJC_MSGSENDSUPER2_STRET'
{$endif onlymacosx10_6 or arm}
            else
              msgsendname:='OBJC_MSGSENDSUPER_STRET'
          end
{$ifdef i386}
        { special case for fpu results on i386 for non-inherited calls }
        { TODO: also for x86_64 "extended" results }
        else if (resultdef.typ=floatdef) and
                not(cnf_inherited in callnodeflags) then
          msgsendname:='OBJC_MSGSEND_FPRET'
{$endif}
        { default }
        else if not(cnf_inherited in callnodeflags) then
          msgsendname:='OBJC_MSGSEND'
{$if defined(onlymacosx10_6) or defined(arm) }
        else if (target_info.system in systems_objc_nfabi) then
          msgsendname:='OBJC_MSGSENDSUPER2'
{$endif onlymacosx10_6 or arm}
        else
          msgsendname:='OBJC_MSGSENDSUPER';

        { get the mangled name }
        srsym:=nil;
        if not searchsym_in_named_module('OBJC',msgsendname,srsym,srsymtable) or
           (srsym.typ<>procsym) or
           (tprocsym(srsym).ProcdefList.count<>1) then
          Message1(cg_f_unknown_compilerproc,'objc.'+msgsendname);
        fobjcforcedprocname:=stringdup(tprocdef(tprocsym(srsym).ProcdefList[0]).mangledname);

        { B) Handle self }
        { 1) in case of sending a message to a superclass, self is a pointer to
             an objc_super record
        }
        if (cnf_inherited in callnodeflags) then
          begin
             block:=internalstatements(statements);
             objcsupertype:=search_named_unit_globaltype('OBJC','OBJC_SUPER',true).typedef;
             if (objcsupertype.typ<>recorddef) then
               internalerror(2009032901);
             { temp for the for the objc_super record }
             temp:=ctempcreatenode.create(objcsupertype,objcsupertype.size,tt_persistent,false);
             addstatement(statements,temp);
             { initialize objc_super record }
             selftree:=load_self_node;

             { we can call an inherited class static/method from a regular method
               -> self node must change from instance pointer to vmt pointer)
             }
             if (po_classmethod in procdefinition.procoptions) and
                (selftree.resultdef.typ<>classrefdef) then
               begin
                 selftree:=cloadvmtaddrnode.create(selftree);
                 { since we're in a class method of the current class, its
                   information has already been initialized (and that of all of
                   its parent classes too) }
                 tloadvmtaddrnode(selftree).forcall:=true;
                 typecheckpass(selftree);
               end;
             selfrestype:=selftree.resultdef;
             field:=tfieldvarsym(trecorddef(objcsupertype).symtable.find('RECEIVER'));
             if not assigned(field) then
               internalerror(2009032902);
            { first the destination object/class instance }
             addstatement(statements,
               cassignmentnode.create(
                 csubscriptnode.create(field,ctemprefnode.create(temp)),
                 selftree
               )
             );
             { and secondly, the class type in which the selector must be looked
               up (the parent class in case of an instance method, the parent's
               metaclass in case of a class method) }
             field:=tfieldvarsym(trecorddef(objcsupertype).symtable.find('_CLASS'));
             if not assigned(field) then
               internalerror(2009032903);
             addstatement(statements,
               cassignmentnode.create(
                 csubscriptnode.create(field,ctemprefnode.create(temp)),
                 objcsuperclassnode(selftree.resultdef)
               )
             );
             { result of this block is the address of this temp }
             addstatement(statements,ctypeconvnode.create_internal(
               caddrnode.create_internal(ctemprefnode.create(temp)),selfrestype)
             );
             { replace the method pointer with the address of this temp }
             methodpointer.free;
             methodpointer:=block;
             typecheckpass(block);
          end
        else
        { 2) regular call (not inherited) }
          begin
            { a) If we're calling a class method, use a class ref.  }
            if (po_classmethod in procdefinition.procoptions) and
               ((methodpointer.nodetype=typen) or
                (methodpointer.resultdef.typ<>classrefdef)) then
              begin
                methodpointer:=cloadvmtaddrnode.create(methodpointer);
                { no need to obtain the class ref by calling class(), sending
                  this message will initialize it if necessary }
                tloadvmtaddrnode(methodpointer).forcall:=true;
                firstpass(methodpointer);
              end;
          end;
      end;


    function tcallnode.gen_vmt_tree:tnode;
      var
        vmttree : tnode;
      begin
        vmttree:=nil;
        if not(procdefinition.proctypeoption in [potype_constructor,potype_destructor]) then
          internalerror(200305051);

        { When methodpointer was a callnode we must load it first into a
          temp to prevent the processing callnode twice }
        if (methodpointer.nodetype=calln) then
          internalerror(200405122);

        { Handle classes and legacy objects separate to make it
          more maintainable }
        if (methodpointer.resultdef.typ=classrefdef) then
          begin
            if not is_class(tclassrefdef(methodpointer.resultdef).pointeddef) then
              internalerror(200501041);

            { constructor call via classreference => allocate memory }
            if (procdefinition.proctypeoption=potype_constructor) then
              begin
                vmttree:=methodpointer.getcopy;
                { Only a typenode can be passed when it is called with <class of xx>.create }
                if vmttree.nodetype=typen then
                  begin
                    vmttree:=cloadvmtaddrnode.create(vmttree);
                    tloadvmtaddrnode(vmttree).forcall:=true;
                  end;
              end
            else
              begin
                { Call afterconstruction }
                vmttree:=cpointerconstnode.create(1,voidpointertype);
              end;
          end
        else
        { Class style objects }
         if is_class(methodpointer.resultdef) then
          begin
            { inherited call, no create/destroy }
            if (cnf_inherited in callnodeflags) then
              vmttree:=cpointerconstnode.create(0,voidpointertype)
            else
              { do not create/destroy when called from member function
                without specifying self explicit }
              if (cnf_member_call in callnodeflags) then
                begin
                  { destructor (in the same class, since cnf_member_call):
                    if not called from a destructor then
                      call beforedestruction and release instance, vmt=1
                    else
                      don't release instance, vmt=0
                    constructor (in the same class, since cnf_member_call):
                      if called from a constructor then
                        don't call afterconstruction, vmt=0
                      else
                        call afterconstrution, vmt=1 }
                  if (procdefinition.proctypeoption=potype_destructor) then
                    if (current_procinfo.procdef.proctypeoption<>potype_constructor) then
                      vmttree:=cpointerconstnode.create(1,voidpointertype)
                    else
                      vmttree:=cpointerconstnode.create(0,voidpointertype)
                  else if (current_procinfo.procdef.proctypeoption=potype_constructor) and
                          (procdefinition.proctypeoption=potype_constructor) then
                    vmttree:=cpointerconstnode.create(0,voidpointertype)
                  else
                    vmttree:=cpointerconstnode.create(1,voidpointertype);
                end
            else
            { normal call to method like cl1.proc }
              begin
                { destructor:
                     if not called from exception block in constructor
                       call beforedestruction and release instance, vmt=1
                     else
                       don't call beforedestruction and release instance, vmt=-1
                  constructor:
                    if called from a constructor in the same class using self.create then
                      don't call afterconstruction, vmt=0
                    else
                      call afterconstruction, vmt=1 }
                if (procdefinition.proctypeoption=potype_destructor) then
                  if not(cnf_create_failed in callnodeflags) then
                    vmttree:=cpointerconstnode.create(1,voidpointertype)
                  else
                    vmttree:=cpointerconstnode.create(TConstPtrUInt(-1),voidpointertype)
                else
                  begin
                    if (current_procinfo.procdef.proctypeoption=potype_constructor) and
                       (procdefinition.proctypeoption=potype_constructor) and
                       (methodpointer.nodetype=loadn) and
                       (loadnf_is_self in tloadnode(methodpointer).loadnodeflags) then
                      vmttree:=cpointerconstnode.create(0,voidpointertype)
                    else
                      vmttree:=cpointerconstnode.create(1,voidpointertype);
                  end;
              end;
          end
        else
        { Old style object }
          begin
            { constructor with extended syntax called from new }
            if (cnf_new_call in callnodeflags) then
                vmttree:=cloadvmtaddrnode.create(ctypenode.create(methodpointer.resultdef))
            else
              { destructor with extended syntax called from dispose }
              { value -1 is what fpc_help_constructor() changes VMT to when it allocates memory }
              if (cnf_dispose_call in callnodeflags) then
                vmttree:=cpointerconstnode.create(TConstPtrUInt(-1),voidpointertype)
            else
              { destructor called from exception block in constructor }
              if (cnf_create_failed in callnodeflags) then
                vmttree:=ctypeconvnode.create_internal(load_vmt_pointer_node,voidpointertype)
            else
              { inherited call, no create/destroy }
              if (cnf_inherited in callnodeflags) then
                vmttree:=cpointerconstnode.create(0,voidpointertype)
            else
              { do not create/destroy when called from member function
                without specifying self explicit }
              if (cnf_member_call in callnodeflags) then
                begin
                  { destructor: don't release instance, vmt=0
                    constructor: don't initialize instance, vmt=0 }
                  vmttree:=cpointerconstnode.create(0,voidpointertype)
                end
            else
            { normal object call like obj.proc }
             begin
               { destructor: direct call, no dispose, vmt=0
                 constructor: initialize object, load vmt }
               if (procdefinition.proctypeoption=potype_constructor) then
                 begin
                   { old styled inherited call? }
                   if (methodpointer.nodetype=typen) then
                     vmttree:=cpointerconstnode.create(0,voidpointertype)
                   else
                     vmttree:=cloadvmtaddrnode.create(ctypenode.create(methodpointer.resultdef))
                 end
               else
                 vmttree:=cpointerconstnode.create(0,voidpointertype);
             end;
          end;
        result:=vmttree;
      end;



    function check_funcret_used_as_para(var n: tnode; arg: pointer): foreachnoderesult;
      var
        destsym : tsym absolute arg;
      begin
        result := fen_false;
        if (n.nodetype=loadn) and
           (tloadnode(n).symtableentry = destsym) then
          result := fen_norecurse_true;
      end;


    function tcallnode.funcret_can_be_reused:boolean;
      var
        realassignmenttarget: tnode;
        alignment: longint;
      begin
        result:=false;

        { we are processing an assignment node? }
        if not(assigned(aktassignmentnode) and
               (aktassignmentnode.right=self) and
               (aktassignmentnode.left.resultdef=resultdef)) then
          exit;

        { destination must be able to be passed as var parameter }
        if not valid_for_var(aktassignmentnode.left,false) then
          exit;

        { destination must be a simple load so it doesn't need a temp when
          it is evaluated }
        if not is_simple_para_load(aktassignmentnode.left,false) then
          exit;

        { remove possible typecasts }
        realassignmenttarget:=actualtargetnode(@aktassignmentnode.left)^;

        { when it is not passed in a parameter it will only be used after the
          function call }
        if not paramanager.ret_in_param(resultdef,procdefinition) then
          begin
            { don't replace the function result if we are inlining and if the destination is complex, this
              could lead to lengthy code in case the function result is used often and it is assigned e.g.
              to a threadvar }
            result:=not(cnf_do_inline in callnodeflags) or
              (node_complexity(aktassignmentnode.left)<=1);
            exit;
          end;

        { if the result is the same as the self parameter (in case of objects),
          we can't optimise. We have to check this explicitly becaise
          hidden parameters such as self have not yet been inserted at this
          point
        }
        if assigned(methodpointer) and
           realassignmenttarget.isequal(actualtargetnode(@methodpointer)^) then
          exit;

        { when we substitute a function result inside an inlined function,
          we may take the address of this function result. Therefore the
          substituted function result may not be in a register, as we cannot
          take its address in that case                                      }
        if (realassignmenttarget.nodetype=temprefn) and
           not(ti_addr_taken in ttemprefnode(realassignmenttarget).tempinfo^.flags) and
           not(ti_may_be_in_reg in ttemprefnode(realassignmenttarget).tempinfo^.flags) then
          begin
            result:=true;
            exit;
          end;

        if (realassignmenttarget.nodetype=loadn) and
           { nested procedures may access the current procedure's locals }
           (procdefinition.parast.symtablelevel=normal_function_level) and
           { must be a local variable, a value para or a hidden function result }
           { parameter (which can be passed by address, but in that case it got }
           { through these same checks at the caller side and is thus safe      }
           (
            (tloadnode(realassignmenttarget).symtableentry.typ=localvarsym) or
            (
             (tloadnode(realassignmenttarget).symtableentry.typ=paravarsym) and
             ((tparavarsym(tloadnode(realassignmenttarget).symtableentry).varspez = vs_value) or
              (vo_is_funcret in tparavarsym(tloadnode(realassignmenttarget).symtableentry).varoptions))
            )
           ) and
           { the address may not have been taken of the variable/parameter, because }
           { otherwise it's possible that the called function can access it via a   }
           { global variable or other stored state                                  }
           (
            not(tabstractvarsym(tloadnode(realassignmenttarget).symtableentry).addr_taken) and
            (tabstractvarsym(tloadnode(realassignmenttarget).symtableentry).varregable in [vr_none,vr_addr])
           ) then
          begin
            { If the funcret is also used as a parameter we can't optimize because the funcret
              and the parameter will point to the same address. That means that a change of the result variable
              will result also in a change of the parameter value }
            result:=not foreachnodestatic(left,@check_funcret_used_as_para,tloadnode(realassignmenttarget).symtableentry);
            { ensure that it is aligned using the default alignment }
            alignment:=tabstractvarsym(tloadnode(realassignmenttarget).symtableentry).vardef.alignment;
            if (used_align(alignment,target_info.alignment.localalignmin,target_info.alignment.localalignmax)<>
                used_align(alignment,current_settings.alignment.localalignmin,current_settings.alignment.localalignmax)) then
              result:=false;
            exit;
          end;
      end;


    procedure tcallnode.maybe_create_funcret_node;
      var
        temp : ttempcreatenode;
      begin
        if procdefinition.proctypeoption=potype_constructor then
          exit;
        { For the function result we need to create a temp node for:
            - Inlined functions
            - Types requiring initialization/finalization
            - Types passed in parameters }
        if not is_void(resultdef) and
           not assigned(funcretnode) and
            (
             (cnf_do_inline in callnodeflags) or
             is_managed_type(resultdef) or
             paramanager.ret_in_param(resultdef,procdefinition)
            ) then
          begin
            { Optimize calls like x:=f() where we can use x directly as
              result instead of using a temp. Condition is that x cannot be accessed from f().
              This implies that x is a local variable or value parameter of the current block
              and its address is not passed to f. One problem: what if someone takes the
              address of x, puts it in a pointer variable/field and then accesses it that way
              from within the function? This is solved (in a conservative way) using the
              ti_addr_taken flag.

              When the result is not not passed in a parameter there are no problem because
              then it means only reference counted types (eg. ansistrings) that need a decr
              of the refcount before being assigned. This is all done after the call so there
              is no issue with exceptions and possible use of the old value in the called
              function }
            if funcret_can_be_reused then
              begin
                funcretnode:=aktassignmentnode.left.getcopy;
                include(funcretnode.flags,nf_is_funcret);
                { notify the assignment node that the assignment can be removed }
                include(aktassignmentnode.flags,nf_assign_done_in_right);
              end
            else
              begin
                temp:=ctempcreatenode.create(resultdef,resultdef.size,tt_persistent,
                  (cnf_do_inline in callnodeflags) and
                  not(tabstractvarsym(tprocdef(procdefinition).funcretsym).varregable in [vr_none,vr_addr]));
                include(temp.flags,nf_is_funcret);
                { if a managed type is returned by reference, assigning something
                  to the result on the caller side will take care of decreasing
                  the reference count }
                if paramanager.ret_in_param(resultdef,procdefinition) then
                  include(temp.tempinfo^.flags,ti_nofini);
                add_init_statement(temp);
                { When the function result is not used in an inlined function
                  we need to delete the temp. This can currently only be done by
                  a tempdeletenode and not after converting it to a normal temp }
                if not(cnf_return_value_used in callnodeflags) and
                   (cnf_do_inline in callnodeflags) then
                  add_done_statement(ctempdeletenode.create(temp))
                else
                  add_done_statement(ctempdeletenode.create_normal_temp(temp));
                funcretnode:=ctemprefnode.create(temp);
                include(funcretnode.flags,nf_is_funcret);
              end;
          end;
      end;


    procedure tcallnode.gen_hidden_parameters;
      var
        para : tcallparanode;
      begin
        para:=tcallparanode(left);
        while assigned(para) do
          begin
            { The processing of high() and typeinfo() is already
              done in the typecheckpass. We only need to process the
              nodes that still have a nothingn }
            if (vo_is_hidden_para in para.parasym.varoptions) and
               (para.left.nodetype=nothingn) then
              begin
                { remove dummy nothingn }
                para.left.free;
                para.left:=nil;
                { generate the corresponding nodes for the hidden parameter type }
                if (vo_is_funcret in para.parasym.varoptions) then
                 begin
                   if not assigned(funcretnode) then
                     internalerror(200709083);
                   para.left:=funcretnode;
                   funcretnode:=nil;
                 end
                else
                 if vo_is_self in para.parasym.varoptions then
                   begin
                     if assigned(right) then
                       para.left:=gen_procvar_context_tree
                     else
                       para.left:=gen_self_tree;
                     { make sure that e.g. the self pointer of an advanced
                       record does not become a regvar, because it's a vs_var
                       parameter }
                     if paramanager.push_addr_param(para.parasym.varspez,para.parasym.vardef,
                         procdefinition.proccalloption) then
                       make_not_regable(para.left,[ra_addr_regable]);
                   end
                else
                 if vo_is_vmt in para.parasym.varoptions then
                   begin
                     para.left:=gen_vmt_tree;
                   end
{$if defined(powerpc) or defined(m68k)}
                else
                 if vo_is_syscall_lib in para.parasym.varoptions then
                   begin
                     { lib parameter has no special type but proccalloptions must be a syscall }
                     para.left:=cloadnode.create(tprocdef(procdefinition).libsym,tprocdef(procdefinition).libsym.owner);
                   end
{$endif powerpc or m68k}
                else
                 if vo_is_parentfp in para.parasym.varoptions then
                   begin
                     if not assigned(right) then
                       begin
                         if assigned(procdefinition.owner.defowner) then
                           para.left:=cloadparentfpnode.create(tprocdef(procdefinition.owner.defowner))
                         { exceptfilters called from main level are not owned }
                         else if procdefinition.proctypeoption=potype_exceptfilter then
                           para.left:=cloadparentfpnode.create(current_procinfo.procdef)
                         else
                           internalerror(200309287);
                       end
                     else
                       para.left:=gen_procvar_context_tree;
                   end
                else
                 if vo_is_range_check in para.parasym.varoptions then
                   begin
                     para.left:=cordconstnode.create(Ord(cs_check_range in current_settings.localswitches),pasbool8type,false);
                   end
                else
                 if vo_is_overflow_check in para.parasym.varoptions then
                   begin
                     para.left:=cordconstnode.create(Ord(cs_check_overflow in current_settings.localswitches),pasbool8type,false);
                   end
                else
                  if vo_is_msgsel in para.parasym.varoptions then
                    begin
                      para.left:=cobjcselectornode.create(cstringconstnode.createstr(tprocdef(procdefinition).messageinf.str^));
                    end;
              end;
            if not assigned(para.left) then
              internalerror(200709084);
            para:=tcallparanode(para.right);
          end;
      end;


    procedure tcallnode.verifyabstract(sym:TObject;arg:pointer);
      var
        pd : tprocdef;
        i  : longint;
        j  : integer;
        hs : string;
      begin
        if (tsym(sym).typ<>procsym) then
          exit;
        for i:=0 to tprocsym(sym).ProcdefList.Count-1 do
          begin
            pd:=tprocdef(tprocsym(sym).ProcdefList[i]);
            hs:=pd.procsym.name+pd.typename_paras([]);
            j:=AbstractMethodsList.FindIndexOf(hs);
            if j<>-1 then
              AbstractMethodsList[j]:=pd
            else
              AbstractMethodsList.Add(hs,pd);
          end;
      end;


    procedure tcallnode.verifyabstractcalls;
      var
        objectdf : tobjectdef;
        parents : tlinkedlist;
        objectinfo : tobjectinfoitem;
        pd : tprocdef;
        i  : integer;
      begin
        objectdf := nil;
        { verify if trying to create an instance of a class which contains
          non-implemented abstract methods }

        { first verify this class type, no class than exit  }
        { also, this checking can only be done if the constructor is directly
          called, indirect constructor calls cannot be checked.
        }
        if assigned(methodpointer) and
           not((methodpointer.nodetype=loadn) and
               (loadnf_is_self in tloadnode(methodpointer).loadnodeflags)) then
          begin
            if (methodpointer.resultdef.typ = objectdef) then
              objectdf:=tobjectdef(methodpointer.resultdef)
            else
              if (methodpointer.resultdef.typ = classrefdef) and
                 (tclassrefdef(methodpointer.resultdef).pointeddef.typ = objectdef) and
                 (methodpointer.nodetype in [typen,loadvmtaddrn]) then
                objectdf:=tobjectdef(tclassrefdef(methodpointer.resultdef).pointeddef);
          end;
        if not assigned(objectdf) then
          exit;
        { quick exit if nothing to check }
        if objectdf.abstractcnt = 0 then
          exit;

        parents := tlinkedlist.create;
        AbstractMethodsList := TFPHashList.create;

        { insert all parents in this class : the first item in the
          list will be the base parent of the class .
        }
        while assigned(objectdf) do
          begin
            objectinfo:=tobjectinfoitem.create(objectdf);
            parents.insert(objectinfo);
            objectdf := objectdf.childof;
        end;
        { now all parents are in the correct order
          insert all abstract methods in the list, and remove
          those which are overridden by parent classes.
        }
        objectinfo:=tobjectinfoitem(parents.first);
        while assigned(objectinfo) do
          begin
             objectdf := objectinfo.objinfo;
             if assigned(objectdf.symtable) then
               objectdf.symtable.SymList.ForEachCall(@verifyabstract,nil);
             objectinfo:=tobjectinfoitem(objectinfo.next);
          end;
        if assigned(parents) then
          parents.free;
        { Finally give out a warning for each abstract method still in the list }
        for i:=0 to AbstractMethodsList.Count-1 do
          begin
            pd:=tprocdef(AbstractMethodsList[i]);
            if po_abstractmethod in pd.procoptions then
              begin
                Message2(type_w_instance_with_abstract,objectdf.objrealname^,pd.procsym.RealName);
                MessagePos1(pd.fileinfo,sym_h_abstract_method_list,pd.fullprocname(true));
              end;
          end;
        if assigned(AbstractMethodsList) then
          AbstractMethodsList.Free;
      end;


    procedure tcallnode.convert_carg_array_of_const;
      var
        hp : tarrayconstructornode;
        oldleft : tcallparanode;
      begin
        oldleft:=tcallparanode(left);
        if oldleft.left.nodetype<>arrayconstructorn then
          begin
            CGMessage1(type_e_wrong_type_in_array_constructor,oldleft.left.resultdef.typename);
            exit;
          end;
        include(callnodeflags,cnf_uses_varargs);
        { Get arrayconstructor node and insert typeconvs }
        hp:=tarrayconstructornode(oldleft.left);
        { Add c args parameters }
        { It could be an empty set }
        if assigned(hp) and
           assigned(hp.left) then
          begin
            while assigned(hp) do
              begin
                left:=ccallparanode.create(hp.left,left);
                { set callparanode resultdef and flags }
                left.resultdef:=hp.left.resultdef;
                include(tcallparanode(left).callparaflags,cpf_varargs_para);
                hp.left:=nil;
                hp:=tarrayconstructornode(hp.right);
              end;
          end;
        { Remove value of old array of const parameter, but keep it
          in the list because it is required for bind_parasym.
          Generate a nothign to keep callparanoed.left valid }
        oldleft.left.free;
        oldleft.left:=cnothingnode.create;
      end;


    procedure tcallnode.bind_parasym;
      type
        pcallparanode = ^tcallparanode;
      var
        i        : integer;
        pt       : tcallparanode;
        oldppt   : pcallparanode;
        varargspara,
        currpara : tparavarsym;
        hiddentree : tnode;
        paradef  : tdef;
      begin
        pt:=tcallparanode(left);
        oldppt:=pcallparanode(@left);

        { flag all callparanodes that belong to the varargs }
        i:=paralength;
        while (i>procdefinition.maxparacount) do
          begin
            include(pt.callparaflags,cpf_varargs_para);
            oldppt:=pcallparanode(@pt.right);
            pt:=tcallparanode(pt.right);
            dec(i);
          end;

        { skip varargs that are inserted by array of const }
        while assigned(pt) and
              (cpf_varargs_para in pt.callparaflags) do
          pt:=tcallparanode(pt.right);

        { process normal parameters and insert hidden parameter nodes, the content
          of the hidden parameters will be updated in pass1 }
        for i:=procdefinition.paras.count-1 downto 0 do
         begin
           currpara:=tparavarsym(procdefinition.paras[i]);
           if vo_is_hidden_para in currpara.varoptions then
            begin
               { Here we handle only the parameters that depend on
                 the types of the previous parameter. The typeconversion
                 can change the type in the next step. For example passing
                 an array can be change to a pointer and a deref }
               if vo_is_high_para in currpara.varoptions then
                begin
                  if not assigned(pt) or (i=0) then
                    internalerror(200304081);
                  { we need the information of the previous parameter }
                  paradef:=tparavarsym(procdefinition.paras[i-1]).vardef;
                  hiddentree:=gen_high_tree(pt.left,paradef);
                  { for open array of managed type, a copy of high parameter is
                    necessary to properly initialize before the call }
                  if is_open_array(paradef) and
                    (tparavarsym(procdefinition.paras[i-1]).varspez=vs_out) and
                     is_managed_type(tarraydef(paradef).elementdef) then
                    begin
                      typecheckpass(hiddentree);
                      {this eliminates double call to fpc_dynarray_high, if any}
                      maybe_load_in_temp(hiddentree);
                      oldppt^.third:=hiddentree.getcopy;
                    end;
                end
              else
                if vo_is_typinfo_para in currpara.varoptions then
                  begin
                    if not assigned(pt) or (i=0) then
                      internalerror(200304082);
                    hiddentree:=caddrnode.create_internal(
                      crttinode.create(Tstoreddef(pt.resultdef),fullrtti,rdt_normal)
                    );
                  end
              else
                hiddentree:=cnothingnode.create;
              pt:=ccallparanode.create(hiddentree,oldppt^);
              oldppt^:=pt;
            end;
           if not assigned(pt) then
             internalerror(200310052);
           pt.parasym:=currpara;
           oldppt:=pcallparanode(@pt.right);
           pt:=tcallparanode(pt.right);
         end;

        { Create parasyms for varargs, first count the number of varargs paras,
          then insert the parameters with numbering in reverse order. The SortParas
          will set the correct order at the end}
        pt:=tcallparanode(left);
        i:=0;
        while assigned(pt) do
          begin
            if cpf_varargs_para in pt.callparaflags then
              inc(i);
            pt:=tcallparanode(pt.right);
          end;
        if (i>0) then
          begin
            varargsparas:=tvarargsparalist.create;
            pt:=tcallparanode(left);
            while assigned(pt) do
              begin
                if cpf_varargs_para in pt.callparaflags then
                  begin
                    varargspara:=tparavarsym.create('va'+tostr(i),i,vs_value,pt.resultdef,[]);
                    dec(i);
                    { varargspara is left-right, use insert
                      instead of concat }
                    varargsparas.add(varargspara);
                    pt.parasym:=varargspara;
                  end;
                pt:=tcallparanode(pt.right);
              end;
            varargsparas.sortparas;
          end;
      end;


    function tcallnode.pass_typecheck:tnode;
      var
        candidates : tcallcandidates;
        oldcallnode : tcallnode;
        hpt : tnode;
        pt : tcallparanode;
        lastpara : longint;
        paraidx,
        cand_cnt : integer;
        i : longint;
        ignorevisibility,
        is_const : boolean;
        statements : tstatementnode;
        converted_result_data : ttempcreatenode;
        calltype: tdispcalltype;
      label
        errorexit;
      begin
         result:=nil;
         candidates:=nil;

         oldcallnode:=aktcallnode;
         aktcallnode:=self;

         { determine length of parameter list }
         pt:=tcallparanode(left);
         paralength:=0;
         while assigned(pt) do
          begin
            inc(paralength);
            pt:=tcallparanode(pt.right);
          end;

         { determine the type of the parameters }
         if assigned(left) then
          begin
            tcallparanode(left).get_paratype;
            if codegenerror then
              goto errorexit;
          end;

         if assigned(methodpointer) then
           typecheckpass(methodpointer);

         { procedure variable ? }
         if assigned(right) then
           begin
              set_varstate(right,vs_read,[vsf_must_be_valid]);
              typecheckpass(right);
              if codegenerror then
               exit;

              procdefinition:=tabstractprocdef(right.resultdef);

              { Compare parameters from right to left }
              paraidx:=procdefinition.Paras.count-1;
              { Skip default parameters }
              if not(po_varargs in procdefinition.procoptions) then
                begin
                  { ignore hidden parameters }
                  while (paraidx>=0) and (vo_is_hidden_para in tparavarsym(procdefinition.paras[paraidx]).varoptions) do
                    dec(paraidx);
                  for i:=1 to procdefinition.maxparacount-paralength do
                    begin
                      if paraidx<0 then
                        internalerror(200402261);
                      if not assigned(tparavarsym(procdefinition.paras[paraidx]).defaultconstsym) then
                        begin
                          CGMessage1(parser_e_wrong_parameter_size,'<Procedure Variable>');
                          goto errorexit;
                        end;
                      dec(paraidx);
                    end;
                end;
              while (paraidx>=0) and (vo_is_hidden_para in tparavarsym(procdefinition.paras[paraidx]).varoptions) do
                dec(paraidx);
              pt:=tcallparanode(left);
              lastpara:=paralength;
              while (paraidx>=0) and assigned(pt) do
                begin
                  { only goto next para if we're out of the varargs }
                  if not(po_varargs in procdefinition.procoptions) or
                     (lastpara<=procdefinition.maxparacount) then
                   begin
                     repeat
                       dec(paraidx);
                     until (paraidx<0) or not(vo_is_hidden_para in tparavarsym(procdefinition.paras[paraidx]).varoptions);
                   end;
                  pt:=tcallparanode(pt.right);
                  dec(lastpara);
                end;
              if assigned(pt) or
                 ((paraidx>=0) and
                  not assigned(tparavarsym(procdefinition.paras[paraidx]).defaultconstsym)) then
                begin
                   if assigned(pt) then
                     current_filepos:=pt.fileinfo;
                   CGMessage1(parser_e_wrong_parameter_size,'<Procedure Variable>');
                   goto errorexit;
                end;
           end
         else
         { not a procedure variable }
           begin
              { do we know the procedure to call ? }
              if not(assigned(procdefinition)) then
                begin
                  { ignore possible private for properties or in delphi mode for anon. inherited (FK) }
                  ignorevisibility:=(nf_isproperty in flags) or
                                    ((m_delphi in current_settings.modeswitches) and (cnf_anon_inherited in callnodeflags));
                  candidates:=tcallcandidates.create(symtableprocentry,symtableproc,left,ignorevisibility,
                    not(nf_isproperty in flags),cnf_objc_id_call in callnodeflags,cnf_unit_specified in callnodeflags,
                    callnodeflags*[cnf_anon_inherited,cnf_inherited]=[],cnf_anon_inherited in callnodeflags);

                   { no procedures found? then there is something wrong
                     with the parameter size or the procedures are
                     not accessible }
                   if candidates.count=0 then
                    begin
                      { when it's an auto inherited call and there
                        is no procedure found, but the procedures
                        were defined with overload directive and at
                        least two procedures are defined then we ignore
                        this inherited by inserting a nothingn. Only
                        do this ugly hack in Delphi mode as it looks more
                        like a bug. It's also not documented }
                      if (m_delphi in current_settings.modeswitches) and
                         (cnf_anon_inherited in callnodeflags) and
                         (symtableprocentry.owner.symtabletype=ObjectSymtable) and
                         (po_overload in tprocdef(symtableprocentry.ProcdefList[0]).procoptions) and
                         (symtableprocentry.ProcdefList.Count>=2) then
                        result:=cnothingnode.create
                      else
                        begin
                          { in tp mode we can try to convert to procvar if
                            there are no parameters specified }
                          if not(assigned(left)) and
                             not(cnf_inherited in callnodeflags) and
                             ((m_tp_procvar in current_settings.modeswitches) or
                              (m_mac_procvar in current_settings.modeswitches)) and
                             (not assigned(methodpointer) or
                              (methodpointer.nodetype <> typen)) then
                            begin
                              hpt:=cloadnode.create(tprocsym(symtableprocentry),symtableproc);
                              if assigned(methodpointer) then
                                tloadnode(hpt).set_mp(methodpointer.getcopy);
                              typecheckpass(hpt);
                              result:=hpt;
                            end
                          else
                            begin
                              CGMessagePos1(fileinfo,parser_e_wrong_parameter_size,symtableprocentry.realname);
                              symtableprocentry.write_parameter_lists(nil);
                            end;
                        end;
                      candidates.free;
                      goto errorexit;
                    end;

                   { Retrieve information about the candidates }
                   candidates.get_information;
{$ifdef EXTDEBUG}
                   { Display info when multiple candidates are found }
                   if candidates.count>1 then
                     candidates.dump_info(V_Debug);
{$endif EXTDEBUG}

                   { Choose the best candidate and count the number of
                     candidates left }
                   cand_cnt:=candidates.choose_best(procdefinition,
                     assigned(left) and
                     not assigned(tcallparanode(left).right) and
                     (tcallparanode(left).left.resultdef.typ=variantdef));

                   { All parameters are checked, check if there are any
                     procedures left }
                   if cand_cnt>0 then
                    begin
                      { Multiple candidates left? }
                      if cand_cnt>1 then
                       begin
                         CGMessage(type_e_cant_choose_overload_function);
{$ifdef EXTDEBUG}
                         candidates.dump_info(V_Hint);
{$else EXTDEBUG}
                         candidates.list(false);
{$endif EXTDEBUG}
                         { we'll just use the first candidate to make the
                           call }
                       end;

                      { assign procdefinition }
                      if symtableproc=nil then
                        symtableproc:=procdefinition.owner;
                    end
                   else
                    begin
                      { No candidates left, this must be a type error,
                        because wrong size is already checked. procdefinition
                        is filled with the first (random) definition that is
                        found. We use this definition to display a nice error
                        message that the wrong type is passed }
                      candidates.find_wrong_para;
                      candidates.list(true);
{$ifdef EXTDEBUG}
                      candidates.dump_info(V_Hint);
{$endif EXTDEBUG}

                      { We can not proceed, release all procs and exit }
                      candidates.free;
                      goto errorexit;
                    end;

                   candidates.free;
               end; { end of procedure to call determination }
           end;

          { check for hints (deprecated etc) }
          if procdefinition.typ = procdef then
            check_hints(tprocdef(procdefinition).procsym,tprocdef(procdefinition).symoptions,tprocdef(procdefinition).deprecatedmsg);

          { add reference to corresponding procsym; may not be the one
            originally found/passed to the constructor because of overloads }
          if procdefinition.typ = procdef then
            addsymref(tprocdef(procdefinition).procsym);

          { add needed default parameters }
          if (paralength<procdefinition.maxparacount) then
           begin
             paraidx:=0;
             i:=0;
             while (i<paralength) do
              begin
                if paraidx>=procdefinition.Paras.count then
                  internalerror(200306181);
                if not(vo_is_hidden_para in tparavarsym(procdefinition.paras[paraidx]).varoptions) then
                  inc(i);
                inc(paraidx);
              end;
             while (paraidx<procdefinition.paras.count) and (vo_is_hidden_para in tparavarsym(procdefinition.paras[paraidx]).varoptions) do
               inc(paraidx);
             while (paraidx<procdefinition.paras.count) do
              begin
                if not assigned(tparavarsym(procdefinition.paras[paraidx]).defaultconstsym) then
                 internalerror(200212142);
                left:=ccallparanode.create(genconstsymtree(
                    tconstsym(tparavarsym(procdefinition.paras[paraidx]).defaultconstsym)),left);
                { Ignore vs_hidden parameters }
                repeat
                  inc(paraidx);
                until (paraidx>=procdefinition.paras.count) or
                  not(vo_is_hidden_para in tparavarsym(procdefinition.paras[paraidx]).varoptions);
              end;
           end;

          { recursive call? }
          if assigned(current_procinfo) and
             (procdefinition=current_procinfo.procdef) then
            include(current_procinfo.flags,pi_is_recursive);

          { handle predefined procedures }
          is_const:=(po_internconst in procdefinition.procoptions) and
                    ((block_type in [bt_const,bt_type,bt_const_type,bt_var_type]) or
                     (assigned(left) and ((tcallparanode(left).left.nodetype in [realconstn,ordconstn])
                      and (not assigned(tcallparanode(left).right) or (tcallparanode(left).right.nodetype in [realconstn,ordconstn])))));
          if (procdefinition.proccalloption=pocall_internproc) or is_const then
           begin
             if assigned(left) then
              begin
                { convert types to those of the prototype, this is required by functions like ror, rol, sar
                  some use however a dummy type (Typedfile) so this would break them }
                if not(tprocdef(procdefinition).extnumber in [fpc_in_Reset_TypedFile,fpc_in_Rewrite_TypedFile]) then
                  begin
                    { bind parasyms to the callparanodes and insert hidden parameters }
                    bind_parasym;

                    { insert type conversions for parameters }
                    if assigned(left) then
                      tcallparanode(left).insert_typeconv;
                  end;

                { ptr and settextbuf need two args }
                if assigned(tcallparanode(left).right) then
                 begin
                   hpt:=geninlinenode(tprocdef(procdefinition).extnumber,is_const,left);
                   left:=nil;
                 end
                else
                 begin
                   hpt:=geninlinenode(tprocdef(procdefinition).extnumber,is_const,tcallparanode(left).left);
                   tcallparanode(left).left:=nil;
                 end;
              end
             else
              hpt:=geninlinenode(tprocdef(procdefinition).extnumber,is_const,nil);
             result:=hpt;
             goto errorexit;
           end;

         { ensure that the result type is set }
         if not(cnf_typedefset in callnodeflags) then
          begin
            { constructors return their current class type, not the type where the
              constructor is declared, this can be different because of inheritance }
            if (procdefinition.proctypeoption=potype_constructor) and
               assigned(methodpointer) and
               assigned(methodpointer.resultdef) and
               (methodpointer.resultdef.typ=classrefdef) then
              resultdef:=tclassrefdef(methodpointer.resultdef).pointeddef
            else
            { Member call to a (inherited) constructor from the class, the return
              value is always self, so we change it to voidtype to generate an
              error and to prevent users from generating non-working code
              when they expect to clone the current instance, see bug 3662 (PFV) }
              if (procdefinition.proctypeoption=potype_constructor) and
                 is_class(tprocdef(procdefinition).struct) and
                 assigned(methodpointer) and
                 (methodpointer.nodetype=loadn) and
                 (loadnf_is_self in tloadnode(methodpointer).loadnodeflags) then
                resultdef:=voidtype
              else
                resultdef:=procdefinition.returndef;
           end
         else
           resultdef:=typedef;

         { Check object/class for methods }
         if assigned(methodpointer) then
          begin
            { direct call to inherited abstract method, then we
              can already give a error in the compiler instead
              of a runtime error }
            if (cnf_inherited in callnodeflags) and
               (po_abstractmethod in procdefinition.procoptions) then
              begin
                if (m_delphi in current_settings.modeswitches) and
                  (cnf_anon_inherited in callnodeflags) then
                  begin
                    CGMessage(cg_h_inherited_ignored);
                    result:=cnothingnode.create;
                    exit;
                  end
                else
                  CGMessage(cg_e_cant_call_abstract_method);
              end;

            { directly calling an interface/protocol/category/class helper
              method via its type is not possible (always must be called via
              the actual instance) }
            if (methodpointer.nodetype=typen) and
               (is_interface(methodpointer.resultdef) or
                is_objc_protocol_or_category(methodpointer.resultdef)) then
              CGMessage1(type_e_class_type_expected,methodpointer.resultdef.typename);

            { if an inherited con- or destructor should be  }
            { called in a con- or destructor then a warning }
            { will be made                                  }
            { con- and destructors need a pointer to the vmt }
            if (cnf_inherited in callnodeflags) and
               (procdefinition.proctypeoption in [potype_constructor,potype_destructor]) and
               is_object(methodpointer.resultdef) and
               not(current_procinfo.procdef.proctypeoption in [potype_constructor,potype_destructor]) then
             CGMessage(cg_w_member_cd_call_from_method);

            if methodpointer.nodetype<>typen then
             begin
                { Remove all postfix operators }
                hpt:=methodpointer;
                while assigned(hpt) and (hpt.nodetype in [subscriptn,vecn]) do
                  hpt:=tunarynode(hpt).left;

                if ((hpt.nodetype=loadvmtaddrn) or
                   ((hpt.nodetype=loadn) and assigned(tloadnode(hpt).resultdef) and (tloadnode(hpt).resultdef.typ=classrefdef))) and
                   not (procdefinition.proctypeoption=potype_constructor) and
                   not (po_classmethod in procdefinition.procoptions) and
                   not (po_staticmethod in procdefinition.procoptions) then
                  { error: we are calling instance method from the class method/static method }
                  CGMessage(parser_e_only_class_members);

               if (procdefinition.proctypeoption=potype_constructor) and
                  assigned(symtableproc) and
                  (symtableproc.symtabletype=withsymtable) and
                  (tnode(twithsymtable(symtableproc).withrefnode).nodetype=temprefn) then
                 CGmessage(cg_e_cannot_call_cons_dest_inside_with);

               { skip (absolute and other simple) type conversions -- only now,
                 because the checks above have to take type conversions into
                 e.g. class reference types account }
               hpt:=actualtargetnode(@hpt)^;

               { R.Init then R will be initialized by the constructor,
                 Also allow it for simple loads }
               if (procdefinition.proctypeoption=potype_constructor) or
                  ((hpt.nodetype=loadn) and
                   (((methodpointer.resultdef.typ=objectdef) and
                     not(oo_has_virtual in tobjectdef(methodpointer.resultdef).objectoptions)) or
                    (methodpointer.resultdef.typ=recorddef)
                   )
                  ) then
                 { a constructor will and a method may write something to }
                 { the fields                                             }
                 set_varstate(methodpointer,vs_readwritten,[])
               else
                 set_varstate(methodpointer,vs_read,[vsf_must_be_valid]);
             end;

            { if we are calling the constructor check for abstract
              methods. Ignore inherited and member calls, because the
              class is then already created }
            if (procdefinition.proctypeoption=potype_constructor) and
               not(cnf_inherited in callnodeflags) and
               not(cnf_member_call in callnodeflags) then
              verifyabstractcalls;
          end
         else
          begin
            { When this is method the methodpointer must be available }
            if (right=nil) and
               (procdefinition.owner.symtabletype in [ObjectSymtable,recordsymtable]) and
               not procdefinition.no_self_node then
              internalerror(200305061);
          end;

         { Set flag that the procedure uses varargs, also if they are not passed it is still
           needed for x86_64 to pass the number of SSE registers used }
         if po_varargs in procdefinition.procoptions then
           include(callnodeflags,cnf_uses_varargs);

         { set the appropriate node flag if the call never returns }
         if po_noreturn in procdefinition.procoptions then
           include(callnodeflags,cnf_call_never_returns);

         { Change loading of array of const to varargs }
         if assigned(left) and
            is_array_of_const(tparavarsym(procdefinition.paras[procdefinition.paras.count-1]).vardef) and
            (procdefinition.proccalloption in cdecl_pocalls) then
           convert_carg_array_of_const;

         { bind parasyms to the callparanodes and insert hidden parameters }
         bind_parasym;

         { insert type conversions for parameters }
         if assigned(left) then
           tcallparanode(left).insert_typeconv;

         { dispinterface methode invoke? }
         if assigned(methodpointer) and is_dispinterface(methodpointer.resultdef) then
           begin
             case procdefinition.proctypeoption of
               potype_propgetter: calltype:=dct_propget;
               potype_propsetter: calltype:=dct_propput;
             else
               calltype:=dct_method;
             end;
             { if the result is used, we've to insert a call to convert the type to be on the "safe side" }
             if (cnf_return_value_used in callnodeflags) and not is_void(procdefinition.returndef) then
               begin
                 result:=internalstatements(statements);
                 converted_result_data:=ctempcreatenode.create(procdefinition.returndef,sizeof(procdefinition.returndef),
                   tt_persistent,true);
                 addstatement(statements,converted_result_data);
                 addstatement(statements,cassignmentnode.create(ctemprefnode.create(converted_result_data),
                   ctypeconvnode.create_internal(
                     translate_disp_call(methodpointer,parameters,calltype,'',tprocdef(procdefinition).dispid,procdefinition.returndef),
                   procdefinition.returndef)));
                 addstatement(statements,ctempdeletenode.create_normal_temp(converted_result_data));
                 addstatement(statements,ctemprefnode.create(converted_result_data));
               end
             else
               result:=translate_disp_call(methodpointer,parameters,calltype,'',tprocdef(procdefinition).dispid,voidtype);

             { don't free reused nodes }
             methodpointer:=nil;
             parameters:=nil;
           end;

      errorexit:
         aktcallnode:=oldcallnode;
      end;


    procedure tcallnode.order_parameters;
      var
        hp,hpcurr,hpnext,hpfirst,hpprev : tcallparanode;
        currloc : tcgloc;
      begin
        hpfirst:=nil;
        hpcurr:=tcallparanode(left);
        { cache all info about parameters containing stack tainting calls,
          since we will need it a lot below and calculting it can be expensive }
        while assigned(hpcurr) do
          begin
            hpcurr.init_contains_stack_tainting_call_cache;
            hpcurr:=tcallparanode(hpcurr.right);
          end;
        hpcurr:=tcallparanode(left);
        while assigned(hpcurr) do
          begin
            { pull out }
            hpnext:=tcallparanode(hpcurr.right);
            { pull in at the correct place.
              Used order:
                1. LOC_REFERENCE with smallest offset (i386 only)
                2. LOC_REFERENCE with least complexity (non-i386 only)
                3. LOC_REFERENCE with most complexity (non-i386 only)
                4. LOC_REGISTER with most complexity
                5. LOC_REGISTER with least complexity
              For the moment we only look at the first parameter field. Combining it
              with multiple parameter fields will make things a lot complexer (PFV)

              The reason for the difference regarding complexity ordering
              between LOC_REFERENCE and LOC_REGISTER is mainly for calls:
              we first want to treat the LOC_REFERENCE destinations whose
              calculation does not require a call, because their location
              may contain registers which might otherwise have to be saved
              if a call has to be evaluated first. The calculated value is
              stored on the stack and will thus no longer occupy any
              register.

              Similarly, for the register parameters we first want to
              evaluate the calls, because otherwise the already loaded
              register parameters will have to be saved so the intermediate
              call can be evaluated (JM) }
            if not assigned(hpcurr.parasym.paraloc[callerside].location) then
              internalerror(200412152);
            currloc:=hpcurr.parasym.paraloc[callerside].location^.loc;
            hpprev:=nil;
            hp:=hpfirst;
            { on fixed_stack targets, always evaluate parameters containing
              a call with stack parameters before all other parameters,
              because they will prevent any other parameters from being put
              in their final place; if both the current and the next para
              contain a stack tainting call, don't do anything to prevent
              them from keeping on chasing eachother's tail }
            while assigned(hp) do
              begin
                if paramanager.use_fixed_stack and
                   hpcurr.contains_stack_tainting_call_cached then
                  break;
                case currloc of
                  LOC_REFERENCE :
                    begin
                      case hp.parasym.paraloc[callerside].location^.loc of
                        LOC_REFERENCE :
                          begin
                            { Offset is calculated like:
                               sub esp,12
                               mov [esp+8],para3
                               mov [esp+4],para2
                               mov [esp],para1
                               call function
                              That means the for pushes the para with the
                              highest offset (see para3) needs to be pushed first
                            }
{$if defined(i386) or defined(i8086) or defined(m68k)}
                            { the i386, i8086, m68k and jvm code generators expect all reference }
                            { parameters to be in this order so they can use   }
                            { pushes in case of no fixed stack                 }
                            if (not paramanager.use_fixed_stack and
                                (hpcurr.parasym.paraloc[callerside].location^.reference.offset>
                                 hp.parasym.paraloc[callerside].location^.reference.offset)) or
                               (paramanager.use_fixed_stack and
                                (node_complexity(hpcurr)<node_complexity(hp))) then
{$elseif defined(jvm)}
                            if (hpcurr.parasym.paraloc[callerside].location^.reference.offset<hp.parasym.paraloc[callerside].location^.reference.offset) then
{$else jvm}
                            if (node_complexity(hpcurr)<node_complexity(hp)) then
{$endif jvm}
                              break;
                          end;
                        LOC_MMREGISTER,
                        LOC_REGISTER,
                        LOC_FPUREGISTER :
                          break;
                      end;
                    end;
                  LOC_MMREGISTER,
                  LOC_FPUREGISTER,
                  LOC_REGISTER :
                    begin
                      if (hp.parasym.paraloc[callerside].location^.loc<>LOC_REFERENCE) and
                         (node_complexity(hpcurr)>node_complexity(hp)) then
                        break;
                    end;
                end;
                hpprev:=hp;
                hp:=tcallparanode(hp.right);
              end;
            hpcurr.right:=hp;
            if assigned(hpprev) then
              hpprev.right:=hpcurr
            else
              hpfirst:=hpcurr;
            { next }
            hpcurr:=hpnext;
          end;
        left:=hpfirst;
        { now mark each parameter that is followed by a stack-tainting call,
          to determine on use_fixed_stack targets which ones can immediately be
          put in their final destination. Unforunately we can never put register
          parameters immediately in their final destination (even on register-
          rich architectures such as the PowerPC), because the code generator
          can still insert extra calls that only make use of register
          parameters (fpc_move() etc. }
        hpcurr:=hpfirst;
        while assigned(hpcurr) do
          begin
            if hpcurr.contains_stack_tainting_call_cached then
              begin
                { all parameters before this one are followed by a stack
                  tainting call }
                hp:=hpfirst;
                while hp<>hpcurr do
                  begin
                    hp.ffollowed_by_stack_tainting_call_cached:=true;
                    hp:=tcallparanode(hp.right);
                  end;
                hpfirst:=hpcurr;
              end;
            hpcurr:=tcallparanode(hpcurr.right);
          end;
      end;


    procedure tcallnode.check_stack_parameters;
      var
        hp : tcallparanode;
      begin
        hp:=tcallparanode(left);
        while assigned(hp) do
          begin
             if assigned(hp.parasym) and
                assigned(hp.parasym.paraloc[callerside].location) and
               (hp.parasym.paraloc[callerside].location^.loc=LOC_REFERENCE) then
               include(current_procinfo.flags,pi_has_stackparameter);
             hp:=tcallparanode(hp.right);
          end;
      end;


    procedure tcallnode.check_inlining;
      var
        st   : tsymtable;
        para : tcallparanode;
      begin
        { Can we inline the procedure? }
        if ([po_inline,po_has_inlininginfo] <= procdefinition.procoptions) then
          begin
             include(callnodeflags,cnf_do_inline);
            { Check if we can inline the procedure when it references proc/var that
              are not in the globally available }
            st:=procdefinition.owner;
            while (st.symtabletype in [ObjectSymtable,recordsymtable]) do
              st:=st.defowner.owner;
            if (pi_uses_static_symtable in tprocdef(procdefinition).inlininginfo^.flags) and
               (st.symtabletype=globalsymtable) and
               (not st.iscurrentunit) then
              begin
                Comment(V_lineinfo+V_Debug,'Not inlining "'+tprocdef(procdefinition).procsym.realname+'", references static symtable');
                exclude(callnodeflags,cnf_do_inline);
              end;
            para:=tcallparanode(parameters);
            while assigned(para) do
              begin
                if not para.can_be_inlined then
                  begin
                    Comment(V_lineinfo+V_Debug,'Not inlining "'+tprocdef(procdefinition).procsym.realname+
                      '", invocation parameter contains an unsafe/unsupported construct');
                    exclude(callnodeflags,cnf_do_inline);
                    break;
                  end;
                para:=tcallparanode(para.nextpara);
              end;
          end;
      end;


    function tcallnode.pass_1 : tnode;

      procedure mark_unregable_parameters;
        var
          hp : tcallparanode;
        begin
          hp:=tcallparanode(left);
          while assigned(hp) do
            begin
              do_typecheckpass(hp.left);
              { When the address needs to be pushed then the register is
                not regable. Exception is when the location is also a var
                parameter and we can pass the address transparently (but
                that is handled by make_not_regable if ra_addr_regable is
                passed, and make_not_regable always needs to called for
                the ra_addr_taken info for non-invisble parameters) }
              if (
                  not(
                      (vo_is_hidden_para in hp.parasym.varoptions) and
                      (hp.left.resultdef.typ in [pointerdef,classrefdef])
                     ) and
                  paramanager.push_addr_param(hp.parasym.varspez,hp.parasym.vardef,
                      self.procdefinition.proccalloption)
                 ) then
                { pushing the address of a variable to take the place of a temp
                  as the complex function result of a function does not make its
                  address escape the current block, as the "address of the
                  function result" is not something which can be stored
                  persistently by the callee (it becomes invalid when the callee
                  returns)                                                       }
                if not(vo_is_funcret in hp.parasym.varoptions) then
                  make_not_regable(hp.left,[ra_addr_regable,ra_addr_taken])
                else
                  make_not_regable(hp.left,[ra_addr_regable]);
              hp:=tcallparanode(hp.right);
            end;
        end;

      var
        para: tcallparanode;
      begin
         result:=nil;

         { as pass_1 is never called on the methodpointer node, we must check
           here that it's not a helper type }
         if assigned(methodpointer) and
             (methodpointer.nodetype=typen) and
             is_objectpascal_helper(ttypenode(methodpointer).typedef) and
             not ttypenode(methodpointer).helperallowed then
           Message(parser_e_no_category_as_types);

         { can we get rid of the call? }
         if (cs_opt_remove_emtpy_proc in current_settings.optimizerswitches) and
            not(cnf_return_value_used in callnodeflags) and
           (procdefinition.typ=procdef) and
           tprocdef(procdefinition).isempty and
           { allow only certain proc options }
           ((tprocdef(procdefinition).procoptions-[po_none,po_classmethod,po_staticmethod,
             po_interrupt,po_iocheck,po_assembler,po_msgstr,po_msgint,po_exports,po_external,po_overload,
             po_nostackframe,po_has_mangledname,po_has_public_name,po_forward,po_global,po_has_inlininginfo,
             po_inline,po_compilerproc,po_has_importdll,po_has_importname,po_kylixlocal,po_dispid,po_delphi_nested_cc,
             po_rtlproc,po_ignore_for_overload_resolution,po_auto_raised_visibility])=[]) then
           begin
             { check parameters for side effects }
             para:=tcallparanode(left);
             while assigned(para) do
               begin
                 if (para.parasym.typ = paravarsym) and
                    ((para.parasym.refs>0) or
                    { array of consts are converted later on so we need to skip them here
                      else no error detection is done }
                     is_array_of_const(para.parasym.vardef) or
                     not(cs_opt_dead_values in current_settings.optimizerswitches) or
                     might_have_sideeffects(para.left)) then
                     break;
                  para:=tcallparanode(para.right);
               end;
             { finally, remove it if no parameter with side effect has been found }
             if para=nil then
               begin
                 result:=cnothingnode.create;
                 exit;
               end;
           end;

         { convert Objective-C calls into a message call }
         if (procdefinition.typ=procdef) and
            (po_objc in tprocdef(procdefinition).procoptions) then
           begin
             if not(cnf_objc_processed in callnodeflags) then
               objc_convert_to_message_send;
           end
         else
           begin
             { The following don't apply to obj-c: obj-c methods can never be
               inlined because they're always virtual and the destination can
               change at run, and for the same reason we also can't perform
               WPO on them (+ they have no constructors) }

             { Check if the call can be inlined, sets the cnf_do_inline flag }
             check_inlining;

             { must be called before maybe_load_in_temp(methodpointer), because
               it converts the methodpointer into a temp in case it's a call
               (and we want to know the original call)
             }
             register_created_object_types;
           end;

         { Maybe optimize the loading of the methodpointer using a temp. When the methodpointer
           is a calln this is even required to not execute the calln twice.
           This needs to be done after the resulttype pass, because in the resulttype we can still convert the
           calln to a loadn (PFV) }
         if assigned(methodpointer) then
           maybe_load_in_temp(methodpointer);

         { Create destination (temp or assignment-variable reuse) for function result if it not yet set }
         maybe_create_funcret_node;

         { Insert the self,vmt,function result in the parameters }
         gen_hidden_parameters;

         { Remove useless nodes from init/final blocks }
         { (simplify depends on typecheck info)        }
         if assigned(callinitblock) then
           begin
             typecheckpass(tnode(callinitblock));
             doinlinesimplify(tnode(callinitblock));
           end;
         if assigned(callcleanupblock) then
           begin
             typecheckpass(tnode(callcleanupblock));
             doinlinesimplify(tnode(callcleanupblock));
           end;

         { If a constructor calls another constructor of the same or of an
           inherited class, some targets (jvm) have to generate different
           entry code for the constructor. }
         if (current_procinfo.procdef.proctypeoption=potype_constructor) and
            (procdefinition.typ=procdef) and
            (tprocdef(procdefinition).proctypeoption=potype_constructor) and
            ([cnf_member_call,cnf_inherited] * callnodeflags <> []) then
           current_procinfo.ConstructorCallingConstructor:=true;

         { object check helper will load VMT -> needs GOT }
         if (cs_check_object in current_settings.localswitches) and
            (cs_create_pic in current_settings.moduleswitches) then
           include(current_procinfo.flags,pi_needs_got);

         { Continue with checking a normal call or generate the inlined code }
         if cnf_do_inline in callnodeflags then
           result:=pass1_inline
         else
           begin
             mark_unregable_parameters;
             result:=pass1_normal;
           end;
      end;


    function tcallnode.pass1_normal : tnode;
      begin
         result:=nil;

         { calculate the parameter info for the procdef }
         procdefinition.init_paraloc_info(callerside);

         { calculate the parameter size needed for this call include varargs if they are available }
         if assigned(varargsparas) then
           pushedparasize:=paramanager.create_varargs_paraloc_info(procdefinition,varargsparas)
         else
           pushedparasize:=procdefinition.callerargareasize;

         { record maximum parameter size used in this proc }
         current_procinfo.allocate_push_parasize(pushedparasize);

         { check for stacked parameters }
         if assigned(left) and
            (current_settings.optimizerswitches*[cs_opt_stackframe,cs_opt_level1]<>[]) then
           check_stack_parameters;

         if assigned(callinitblock) then
           firstpass(tnode(callinitblock));

         { function result node (tempref or simple load) }
         if assigned(funcretnode) then
           firstpass(funcretnode);

         { parameters }
         if assigned(left) then
           tcallparanode(left).firstcallparan;

         { procedure variable ? }
         if assigned(right) then
           firstpass(right);

         if assigned(methodpointer) and
            (methodpointer.nodetype<>typen) then
           firstpass(methodpointer);

         if assigned(callcleanupblock) then
           firstpass(tnode(callcleanupblock));

         if not (block_type in [bt_const,bt_type,bt_const_type,bt_var_type]) then
           include(current_procinfo.flags,pi_do_call);

         { order parameters }
         order_parameters;

         { get a register for the return value }
         if (not is_void(resultdef)) then
           begin
              if paramanager.ret_in_param(resultdef,procdefinition) then
               begin
                 expectloc:=LOC_REFERENCE;
               end
             else
             { ansi/widestrings must be registered, so we can dispose them }
              if is_ansistring(resultdef) or
                 is_widestring(resultdef) or
                 is_unicodestring(resultdef) then
               begin
                 expectloc:=LOC_REFERENCE;
               end
             else
             { we have only to handle the result if it is used }
              if (cnf_return_value_used in callnodeflags) then
                expectloc:=get_expect_loc
             else
               expectloc:=LOC_VOID;
           end
         else
           expectloc:=LOC_VOID;
      end;

{$ifdef state_tracking}
    function Tcallnode.track_state_pass(exec_known:boolean):boolean;

    var hp:Tcallparanode;
        value:Tnode;

    begin
        track_state_pass:=false;
        hp:=Tcallparanode(left);
        while assigned(hp) do
            begin
                if left.track_state_pass(exec_known) then
                    begin
                        left.resultdef:=nil;
                        do_typecheckpass(left);
                    end;
                value:=aktstate.find_fact(hp.left);
                if value<>nil then
                    begin
                        track_state_pass:=true;
                        hp.left.destroy;
                        hp.left:=value.getcopy;
                        do_typecheckpass(hp.left);
                    end;
                hp:=Tcallparanode(hp.right);
            end;
    end;
{$endif}


{**************************************************************************
                       INLINING SUPPORT
**************************************************************************}

    function tcallnode.replaceparaload(var n: tnode; arg: pointer): foreachnoderesult;
      var
        paras: tcallparanode;
        temp: tnode;
        indexnr : integer;
      begin
        result := fen_false;
        n.fileinfo := pfileposinfo(arg)^;
        if (n.nodetype = loadn) then
          begin
            case tloadnode(n).symtableentry.typ of
              paravarsym :
                begin
                  paras := tcallparanode(left);
                  while assigned(paras) and
                        (paras.parasym <> tloadnode(n).symtableentry) do
                    paras := tcallparanode(paras.right);
                  if assigned(paras) then
                    begin
                      temp:=paras.left.getcopy;
                      { inherit modification information, this is needed by the dfa/cse }
                      temp.flags:=temp.flags+(n.flags*[nf_modify,nf_write]);
                      n.free;
                      n:=temp;
                      typecheckpass(n);
                      result := fen_true;
                    end;
                end;
              localvarsym :
                begin
                  { local? }
                  if (tloadnode(n).symtableentry.owner <> tprocdef(procdefinition).localst) then
                    exit;
                  indexnr:=tloadnode(n).symtableentry.owner.SymList.IndexOf(tloadnode(n).symtableentry);
                  if (indexnr >= inlinelocals.count) or
                     not assigned(inlinelocals[indexnr]) then
                    internalerror(20040720);
                  temp := tnode(inlinelocals[indexnr]).getcopy;
                  { inherit modification information, this is needed by the dfa/cse }
                  temp.flags:=temp.flags+(n.flags*[nf_modify,nf_write]);
                  n.free;
                  n:=temp;
                  typecheckpass(n);
                  result := fen_true;
                end;
            end;
          end;
      end;


    procedure tcallnode.createlocaltemps(p:TObject;arg:pointer);
      var
        tempnode: ttempcreatenode;
        indexnr : integer;
      begin
        if (TSym(p).typ <> localvarsym) then
          exit;
        indexnr:=TSym(p).Owner.SymList.IndexOf(p);
        if (indexnr >= inlinelocals.count) then
          inlinelocals.count:=indexnr+10;
        if (vo_is_funcret in tabstractvarsym(p).varoptions) then
          begin
            if not assigned(funcretnode) then
              internalerror(200709081);
            inlinelocals[indexnr] := funcretnode.getcopy
          end
        else
          begin
            tempnode :=ctempcreatenode.create(tabstractvarsym(p).vardef,
              tabstractvarsym(p).vardef.size,tt_persistent,tabstractvarsym(p).is_regvar(false));
            addstatement(inlineinitstatement,tempnode);

            if localvartrashing <> -1 then
              cnodeutils.maybe_trash_variable(inlineinitstatement,tabstractnormalvarsym(p),ctemprefnode.create(tempnode));

            addstatement(inlinecleanupstatement,ctempdeletenode.create(tempnode));
            { inherit addr_taken flag }
            if (tabstractvarsym(p).addr_taken) then
              include(tempnode.tempinfo^.flags,ti_addr_taken);
            inlinelocals[indexnr] := ctemprefnode.create(tempnode);
          end;
      end;


    function nonlocalvars(var n: tnode; arg: pointer): foreachnoderesult;
      begin
        result := fen_false;
        { this is just to play it safe, there are more safe situations }
        if (n.nodetype = derefn) or
           ((n.nodetype = loadn) and
            { globals and fields of (possibly global) objects could always be changed in the callee }
            ((tloadnode(n).symtable.symtabletype in [globalsymtable,ObjectSymtable]) or
            { statics can only be modified by functions in the same unit }
             ((tloadnode(n).symtable.symtabletype = staticsymtable) and
              (tloadnode(n).symtable = TSymtable(arg))))) or
           ((n.nodetype = subscriptn) and
            (tsubscriptnode(n).vs.owner.symtabletype = ObjectSymtable)) then
          result := fen_norecurse_true;
      end;


    procedure tcallnode.createinlineparas;
      var
        para: tcallparanode;
        tempnode: ttempcreatenode;
        n: tnode;
        paracomplexity: longint;
        pushconstaddr: boolean;
        trytotakeaddress : Boolean;
      begin
        { parameters }
        para := tcallparanode(left);
        pushconstaddr := false;
        while assigned(para) do
          begin
            if (para.parasym.typ = paravarsym) and
               ((para.parasym.refs>0) or
                not(cs_opt_dead_values in current_settings.optimizerswitches) or
                might_have_sideeffects(para.left)) then
              begin
                { must take copy of para.left, because if it contains a       }
                { temprefn pointing to a copied temp (e.g. methodpointer),    }
                { then this parameter must be changed to point to the copy of }
                { that temp (JM)                                              }
                n := para.left.getcopy;
                para.left.free;
                para.left := n;

                firstpass(para.left);

                { determine how a parameter is passed to the inlined body
                  There are three options:
                    - insert the node tree of the callparanode directly
                      If a parameter is used only once, this is the best option if we can do so
                    - get the address of the argument, store it in a temp and insert a dereference to this temp
                      If the node tree cannot be inserted directly, taking the address of the argument and using it
                      is the second best option, but even this is not always possible
                    - assign the value of the argument to a newly created temp
                      This is the fall back which works always
                  Notes:
                    - we need to take care that we use the type of the defined parameter and not of the
                      passed parameter, because these can be different in case of a formaldef (PFV)
                }

                { pre-compute some values }
                paracomplexity:=node_complexity(para.left);
                if para.parasym.varspez=vs_const then
                  pushconstaddr:=paramanager.push_addr_param(vs_const,para.parasym.vardef,procdefinition.proccalloption);

                { if the parameter is "complex", try to take the address
                  of the parameter expression, store it in a temp and replace
                  occurrences of the parameter with dereferencings of this
                  temp
                }
                trytotakeaddress:=
                  { don't create a temp. for function results }
                  not(nf_is_funcret in para.left.flags) and
                  { this makes only sense if the parameter is reasonable complex else inserting directly is a better solution }
                  ((paracomplexity>2) or
                  { don't create a temp. for the often seen case that p^ is passed to a var parameter }
                  ((paracomplexity>1) and not((para.left.nodetype=derefn) and (para.parasym.varspez = vs_var))));

                { check if we have to create a temp, assign the parameter's
                  contents to that temp and then substitute the parameter
                  with the temp everywhere in the function                  }
                if
                  ((tparavarsym(para.parasym).varregable in [vr_none,vr_addr]) and
                   not(para.left.expectloc in [LOC_REFERENCE,LOC_CREFERENCE]))  or
                  { we can't assign to formaldef temps }
                  ((para.parasym.vardef.typ<>formaldef) and
                   (
                    { can we take the address of the argument? }
                    (trytotakeaddress and not(para.left.expectloc in [LOC_REFERENCE,LOC_CREFERENCE])) or
                    (trytotakeaddress and
                     (not valid_for_addr(para.left,false) or
                      (para.left.nodetype = calln) or
                      is_constnode(para.left))) or
                    { we do not need to create a temp for value parameters }
                    { which are not modified in the inlined function       }
                    { const parameters can get vs_readwritten if their     }
                    { address is taken                                     }
                    ((((para.parasym.varspez = vs_value) and
                       (para.parasym.varstate in [vs_initialised,vs_declared,vs_read])) or
                      { in case of const, this is only necessary if the
                        variable would be passed by value normally and if it is modified or if
                        there is such a variable somewhere in an expression }
                       ((para.parasym.varspez = vs_const) and
                        (not pushconstaddr))) and
                     { however, if we pass a global variable, an object field or}
                     { an expression containing a pointer dereference as        }
                     { parameter, this value could be modified in other ways as }
                     { well and in such cases create a temp to be on the safe   }
                     { side                                                     }
                     foreachnodestatic(para.left,@nonlocalvars,pointer(symtableproc))) or
                    { value parameters of which we know they are modified by }
                    { definition have to be copied to a temp                 }
                    { the same goes for cases of "x:=f(x)" where x is passed }
                    { as value parameter to f(), at least if we optimized    }
                    { invocation by setting the funcretnode to x to avoid    }
                    { assignment afterwards (since x may be read inside the  }
                    { function after it modified result==x)                  }
                    ((para.parasym.varspez = vs_value) and
                     (not(para.parasym.varstate in [vs_initialised,vs_declared,vs_read]) or
                      (assigned(aktassignmentnode) and
                       (aktassignmentnode.right=self) and
                       (nf_assign_done_in_right in aktassignmentnode.flags) and
                       aktassignmentnode.left.isequal(para.left)))) or
                    { the compiler expects that it can take the address of parameters passed by reference in
                      the case of const so we can't replace the node simply by a constant node
                      When playing with this code, ensure that
                      function f(const a,b  : longint) : longint;inline;
                        begin
                          result:=a*b;
                        end;

                      [...]
                      ...:=f(10,20));
                      [...]

                      is still folded. (FK)
                      }
                    ((para.parasym.varspez = vs_const) and
                     { const para's can get vs_readwritten if their address   }
                     { is taken -> in case they are not passed by reference,  }
                     { to keep the same behaviour as without inlining we have }
                     { to make a copy in case the originally passed parameter }
                     { value gets changed inside the callee                   }
                     ((not pushconstaddr and
                       (para.parasym.varstate = vs_readwritten)
                      ) or
                      { call-by-reference const's may need to be passed by }
                      { reference to function called in the inlined code   }
                       (pushconstaddr and
                        not valid_for_addr(para.left,false))
                     )
                    )
                   )
                  ) then
                  begin
                    { don't create a new temp unnecessarily, but make sure we
                      do create a new one if the old one could be a regvar and
                      the new one cannot be one }
                    if not(tparavarsym(para.parasym).varspez in [vs_out,vs_var]) and (((para.left.nodetype<>temprefn) or
                       (((tparavarsym(para.parasym).varregable in [vr_none,vr_addr])) and
                        (ti_may_be_in_reg in ttemprefnode(para.left).tempinfo^.flags)))) then
                      begin
                        tempnode := ctempcreatenode.create(para.parasym.vardef,para.parasym.vardef.size,
                          tt_persistent,tparavarsym(para.parasym).is_regvar(false));
                        addstatement(inlineinitstatement,tempnode);

                        if localvartrashing <> -1 then
                          cnodeutils.maybe_trash_variable(inlineinitstatement,para.parasym,ctemprefnode.create(tempnode));

                        addstatement(inlinecleanupstatement,ctempdeletenode.create(tempnode));

                        addstatement(inlineinitstatement,cassignmentnode.create(ctemprefnode.create(tempnode),
                            para.left));
                        para.left := ctemprefnode.create(tempnode);
                        { inherit addr_taken flag }
                        if (tabstractvarsym(para.parasym).addr_taken) then
                          include(tempnode.tempinfo^.flags,ti_addr_taken);
                      end;
                  end
                else if trytotakeaddress then
                  wrapcomplexinlinepara(para);
              end;
            para := tcallparanode(para.right);
          end;
        { local variables }
        if not assigned(tprocdef(procdefinition).localst) or
           (tprocdef(procdefinition).localst.SymList.count = 0) then
          exit;
        inlinelocals.count:=tprocdef(procdefinition).localst.SymList.count;
        tprocdef(procdefinition).localst.SymList.ForEachCall(@createlocaltemps,nil);
      end;


    procedure tcallnode.wrapcomplexinlinepara(para: tcallparanode);
      var
        ptrtype: tdef;
        tempnode: ttempcreatenode;
        paraaddr: taddrnode;
      begin
        ptrtype:=getpointerdef(para.left.resultdef);
        tempnode:=ctempcreatenode.create(ptrtype,ptrtype.size,tt_persistent,true);
        addstatement(inlineinitstatement,tempnode);
        addstatement(inlinecleanupstatement,ctempdeletenode.create(tempnode));
        { inherit addr_taken flag }
        if (tabstractvarsym(para.parasym).addr_taken) then
          include(tempnode.tempinfo^.flags,ti_addr_taken);
        { inherit read only }
        if tabstractvarsym(para.parasym).varspez=vs_const then
          include(tempnode.tempinfo^.flags,ti_const);
        paraaddr:=caddrnode.create_internal(para.left);
        include(paraaddr.flags,nf_typedaddr);
        addstatement(inlineinitstatement,cassignmentnode.create(ctemprefnode.create(tempnode),
          paraaddr));
        para.left:=cderefnode.create(ctemprefnode.create(tempnode));
      end;


    function tcallnode.optimize_funcret_assignment(inlineblock: tblocknode): tnode;
      var
        hp  : tstatementnode;
        hp2 : tnode;
        resassign : tassignmentnode;
      begin
        result:=nil;
        if not assigned(funcretnode) or
           not(cnf_return_value_used in callnodeflags) then
          exit;

        { tempcreatenode for the function result }
        hp:=tstatementnode(inlineblock.left);
        if not(assigned(hp)) or
           (hp.left.nodetype <> tempcreaten) or
           not(nf_is_funcret in hp.left.flags) then
          exit;

        { constant assignment? right must be a constant (mainly to avoid trying
          to reuse local temps which may already be freed afterwards once these
          checks are made looser) }
        hp:=tstatementnode(hp.right);
        if not(assigned(hp)) or
           (hp.left.nodetype<>assignn) or
           not is_constnode(tassignmentnode(hp.left).right) then
          exit;

        { left must be function result }
        resassign:=tassignmentnode(hp.left);
        hp2:=resassign.left;
        { can have extra type conversion due to absolute mapping
          of <fucntionname> on function result var }
        if (hp2.nodetype=typeconvn) and (ttypeconvnode(hp2).convtype=tc_equal) then
          hp2:=ttypeconvnode(hp2).left;
        if (hp2.nodetype<>temprefn) or
           not(nf_is_funcret in hp2.flags) then
          exit;

        { tempdelete to normal of the function result }
        hp:=tstatementnode(hp.right);
        if not(assigned(hp)) or
           (hp.left.nodetype <> tempdeleten) then
          exit;

        { the function result once more }
        hp:=tstatementnode(hp.right);
        if not(assigned(hp)) or
           (hp.left.nodetype<>temprefn) or
           not(nf_is_funcret in hp.left.flags) then
          exit;

        { should be the end }
        if assigned(hp.right) then
          exit;

        { we made it! }
        result:=tassignmentnode(resassign).right.getcopy;
        firstpass(result);
      end;


    function tcallnode.pass1_inline:tnode;
      var
        n,
        body : tnode;
        para : tcallparanode;
        inlineblock,
        inlinecleanupblock : tblocknode;
      begin
        result:=nil;
        if not(assigned(tprocdef(procdefinition).inlininginfo) and
               assigned(tprocdef(procdefinition).inlininginfo^.code)) then
          internalerror(200412021);

        inlinelocals:=TFPObjectList.create(true);

        { inherit flags }
        current_procinfo.flags:=current_procinfo.flags+
          ((procdefinition as tprocdef).inlininginfo^.flags*inherited_inlining_flags);

        { Create new code block for inlining }
        inlineblock:=internalstatements(inlineinitstatement);
        { make sure that valid_for_assign() returns false for this block
          (otherwise assigning values to the block will result in assigning
           values to the inlined function's result) }
        include(inlineblock.flags,nf_no_lvalue);
        inlinecleanupblock:=internalstatements(inlinecleanupstatement);

        if assigned(callinitblock) then
          addstatement(inlineinitstatement,callinitblock.getcopy);

        { replace complex parameters with temps }
        createinlineparas;

        { create a copy of the body and replace parameter loads with the parameter values }
        body:=tprocdef(procdefinition).inlininginfo^.code.getcopy;
        foreachnode(pm_preprocess,body,@replaceparaload,@fileinfo);

        { Concat the body and finalization parts }
        addstatement(inlineinitstatement,body);
        addstatement(inlineinitstatement,inlinecleanupblock);
        inlinecleanupblock:=nil;

        if assigned(callcleanupblock) then
          addstatement(inlineinitstatement,callcleanupblock.getcopy);

        { the last statement of the new inline block must return the
          location and type of the function result.
          This is not needed when the result is not used, also the tempnode is then
          already destroyed  by a tempdelete in the callcleanupblock tree }
        if not is_void(resultdef) and
           (cnf_return_value_used in callnodeflags) then
          begin
            if assigned(funcretnode) then
              addstatement(inlineinitstatement,funcretnode.getcopy)
            else
              begin
                para:=tcallparanode(left);
                while assigned(para) do
                  begin
                    if (vo_is_hidden_para in para.parasym.varoptions) and
                       (vo_is_funcret in para.parasym.varoptions) then
                      begin
                        addstatement(inlineinitstatement,para.left.getcopy);
                        break;
                      end;
                    para:=tcallparanode(para.right);
                  end;
              end;
          end;

        { consider it must not be inlined if called
          again inside the args or itself }
        exclude(procdefinition.procoptions,po_inline);
        typecheckpass(tnode(inlineblock));
        doinlinesimplify(tnode(inlineblock));
        firstpass(tnode(inlineblock));
        include(procdefinition.procoptions,po_inline);
        result:=inlineblock;

        { if the function result is used then verify that the blocknode
          returns the same result type as the original callnode }
        if (cnf_return_value_used in callnodeflags) and
           (result.resultdef<>resultdef) then
          internalerror(200709171);

        { free the temps for the locals }
        inlinelocals.free;
        inlinelocals:=nil;
        inlineinitstatement:=nil;
        inlinecleanupstatement:=nil;

        { if all that's left of the inlined function is an constant assignment
          to the result, replace the whole block with the constant only }
        n:=optimize_funcret_assignment(inlineblock);
        if assigned(n) then
          begin
            inlineblock.free;
            result:=n;
          end;

{$ifdef DEBUGINLINE}
        writeln;
        writeln('**************************',tprocdef(procdefinition).mangledname);
        printnode(output,result);
{$endif DEBUGINLINE}
      end;

end.
