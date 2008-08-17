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

interface

    uses
       cutils,cclasses,
       globtype,constexp,
       paramgr,parabase,
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
         cnf_create_failed       { exception thrown in constructor -> don't call beforedestruction }
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
          function  gen_self_tree_methodpointer:tnode;
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

          { inlining support }
          inlinelocals            : TFPObjectList;
          inlineinitstatement,
          inlinecleanupstatement  : tstatementnode;
          procedure createinlineparas;
          function  replaceparaload(var n: tnode; arg: pointer): foreachnoderesult;
          procedure createlocaltemps(p:TObject;arg:pointer);
          function  optimize_funcret_assignment(inlineblock: tblocknode): tnode;
          function  pass1_inline:tnode;
       protected
          pushedparasize : longint;
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
          constructor createinternres(const name: string; params: tnode; res:tdef);
          constructor createinternreturn(const name: string; params: tnode; returnnode : tnode);
          destructor destroy;override;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure derefnode;override;
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
          { checks if there are any parameters which end up at the stack, i.e.
            which have LOC_REFERENCE and set pi_has_stackparameter if this applies }
          procedure check_stack_parameters;
          property parameters : tnode read left write left;
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
       public
          callparaflags : tcallparaflags;
          parasym       : tparavarsym;
          { only the processor specific nodes need to override this }
          { constructor                                             }
          constructor create(expr,next : tnode);virtual;
          destructor destroy;override;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
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
          property parametername : tnode read third write third;
       end;
       tcallparanodeclass = class of tcallparanode;

    function reverseparameters(p: tcallparanode): tcallparanode;
    function translate_disp_call(selfnode,parametersnode,putvalue : tnode;methodname : ansistring = '';dispid : longint = 0;useresult : boolean = false) : tnode;

    var
      ccallnode : tcallnodeclass;
      ccallparanode : tcallparanodeclass;

      { Current callnode, this is needed for having a link
       between the callparanodes and the callnode they belong to }
      aktcallnode : tcallnode;


implementation

    uses
      systems,
      verbose,globals,
      symconst,defutil,defcmp,
      htypechk,pass_1,
      ncnv,nld,ninl,nadd,ncon,nmem,nset,
      procinfo,cpuinfo,
      cgbase
      ;

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


    function translate_disp_call(selfnode,parametersnode,putvalue : tnode;methodname : ansistring = '';dispid : longint = 0;useresult : boolean = false) : tnode;
      const
        DISPATCH_METHOD = $1;
        DISPATCH_PROPERTYGET = $2;
        DISPATCH_PROPERTYPUT = $4;
        DISPATCH_PROPERTYPUTREF = $8;
        DISPATCH_CONSTRUCT = $4000;
      var
        statements : tstatementnode;
        result_data,
        params : ttempcreatenode;
        paramssize : cardinal;
        calldescnode : tdataconstnode;
        resultvalue : tnode;
        para : tcallparanode;
        currargpos,
        namedparacount,
        paracount : longint;
        assignmenttype,
        vardatadef,
        pvardatadef : tdef;
        dispatchbyref : boolean;

        calldesc : packed record
            calltype,argcount,namedargcount : byte;
            { size of argtypes is unknown at compile time
              so this is basically a dummy }
            argtypes : array[0..255] of byte;
            { argtypes is followed by method name
              names of named parameters, each being
              a zero terminated string
            }
        end;
        names : ansistring;
        dispintfinvoke,
        variantdispatch : boolean;

      procedure increase_paramssize;
        begin
          { for now we pass everything by reference
          case para.left.resultdef.typ of
            variantdef:
              inc(paramssize,para.left.resultdef.size);
            else
          }
              inc(paramssize,sizeof(voidpointertype.size));
          {
          end;
          }
        end;

      begin
        variantdispatch:=selfnode.resultdef.typ=variantdef;
        dispintfinvoke:=not(variantdispatch);

        result:=internalstatements(statements);
        fillchar(calldesc,sizeof(calldesc),0);

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
        paramssize:=0;
        while assigned(para) do
          begin
            inc(paracount);
            typecheckpass(para.left);

            { insert some extra casts }
            if is_constintnode(para.left) and not(is_64bitint(para.left.resultdef)) then
              begin
                para.left:=ctypeconvnode.create_internal(para.left,s32inttype);
                typecheckpass(para.left);
              end
            else if para.left.nodetype=stringconstn then
              begin
                para.left:=ctypeconvnode.create_internal(para.left,cwidestringtype);
                typecheckpass(para.left);
              end
            { force automatable boolean type }
            else if is_boolean(para.left.resultdef) then
              begin
                para.left:=ctypeconvnode.create_internal(para.left,bool16type);
                typecheckpass(para.left);
              end
            { force automatable float type }
            else if is_extended(para.left.resultdef)
                and (current_settings.fputype<>fpu_none) then
              begin
                para.left:=ctypeconvnode.create_internal(para.left,s64floattype);
                typecheckpass(para.left);
              end;

            if assigned(para.parametername) then
              begin
                typecheckpass(para.left);
                inc(namedparacount);
              end;

            if para.left.nodetype<>nothingn then
              if not is_automatable(para.left.resultdef) then
                CGMessagePos1(para.left.fileinfo,type_e_not_automatable,para.left.resultdef.typename);

            { we've to know the parameter size to allocate the temp. space }
            increase_paramssize;

            para:=tcallparanode(para.nextpara);
          end;
        if assigned(putvalue) then
          calldesc.calltype:=DISPATCH_PROPERTYPUT
        else
          calldesc.calltype:=DISPATCH_METHOD;
        calldesc.argcount:=paracount;

        { allocate space }
        params:=ctempcreatenode.create(voidtype,paramssize,tt_persistent,true);
        addstatement(statements,params);

        calldescnode:=cdataconstnode.create;

        if dispintfinvoke then
          calldescnode.append(dispid,sizeof(dispid));

        { build up parameters and description }
        para:=tcallparanode(parametersnode);
        currargpos:=0;
        paramssize:=0;
        names := '';
        while assigned(para) do
          begin
            if assigned(para.parametername) then
              begin
                if para.parametername.nodetype=stringconstn then
                  names:=names+tstringconstnode(para.parametername).value_str+#0
                else
                  internalerror(200611041);
              end;

            dispatchbyref:=para.left.resultdef.typ in [variantdef];

            { assign the argument/parameter to the temporary location }
            if para.left.nodetype<>nothingn then
              if dispatchbyref then
                addstatement(statements,cassignmentnode.create(
                  ctypeconvnode.create_internal(cderefnode.create(caddnode.create(addn,
                    caddrnode.create(ctemprefnode.create(params)),
                    cordconstnode.create(qword(paramssize),ptruinttype,false)
                  )),voidpointertype),
                  ctypeconvnode.create_internal(caddrnode.create_internal(para.left),voidpointertype)))
              else
                begin
                  case para.left.resultdef.size of
                    1..4:
                      assignmenttype:=u32inttype;
                    8:
                      assignmenttype:=u64inttype;
                    else
                      internalerror(2007042801);
                  end;
                  addstatement(statements,cassignmentnode.create(
                    ctypeconvnode.create_internal(cderefnode.create(caddnode.create(addn,
                      caddrnode.create(ctemprefnode.create(params)),
                      cordconstnode.create(paramssize,ptruinttype,false)
                    )),assignmenttype),
                    ctypeconvnode.create_internal(para.left,assignmenttype)));
                end;

            if is_ansistring(para.left.resultdef) then
              calldesc.argtypes[currargpos]:=varStrArg
            else
              calldesc.argtypes[currargpos]:=para.left.resultdef.getvardef;

            if dispatchbyref then
              calldesc.argtypes[currargpos]:=calldesc.argtypes[currargpos] or $80;

            increase_paramssize;

            para.left:=nil;
            inc(currargpos);
            para:=tcallparanode(para.nextpara);
          end;

        { old argument list skeleton isn't needed anymore }
        parametersnode.free;

        calldescnode.append(calldesc,3+calldesc.argcount);

        pvardatadef:=tpointerdef(search_system_type('PVARDATA').typedef);

        if useresult then
          resultvalue:=caddrnode.create(ctemprefnode.create(result_data))
        else
          resultvalue:=cpointerconstnode.create(0,voidpointertype);

        if variantdispatch then
          begin
            methodname:=methodname+#0;
            calldescnode.append(pointer(methodname)^,length(methodname));
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
         inherited destroy;
      end;


    constructor tcallparanode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        ppufile.getsmallset(callparaflags);
      end;


    procedure tcallparanode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putsmallset(callparaflags);
      end;


    function tcallparanode.dogetcopy : tnode;

      var
         n : tcallparanode;

      begin
         n:=tcallparanode(inherited dogetcopy);
         n.callparaflags:=callparaflags;
         n.parasym:=parasym;
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
         typecheckpass(left);
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
        firstpass(left);
        expectloc:=left.expectloc;
      end;


    procedure tcallparanode.insert_typeconv;
      var
        olddef  : tdef;
        hp      : tnode;
        block : tblocknode;
        statements : tstatementnode;
        temp : ttempcreatenode;
      begin
         { Be sure to have the resultdef }
         if not assigned(left.resultdef) then
           typecheckpass(left);

         if (left.nodetype<>nothingn) then
           begin
             { Convert tp procvars, this is needs to be done
               here to make the change permanent. in the overload
               choosing the changes are only made temporary }
             if (left.resultdef.typ=procvardef) and
                not(parasym.vardef.typ in [procvardef,formaldef]) then
               begin
                 if maybe_call_procvar(left,true) then
                   resultdef:=left.resultdef;
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

             { Handle varargs and hidden paras directly, no typeconvs or }
             { pass_typechecking needed                                       }
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
                    (parasym.vardef.typ<>formaldef) then
                   begin
                      { Process open parameters }
                      if paramanager.push_high_param(parasym.varspez,parasym.vardef,aktcallnode.procdefinition.proccalloption) then
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
                     typecheckpass(block);
                     left:=block;
                   end;

                 { check var strings }
                 if (cs_strict_var_strings in current_settings.localswitches) and
                    is_shortstring(left.resultdef) and
                    is_shortstring(parasym.vardef) and
                    (parasym.varspez in [vs_out,vs_var]) and
                    not(is_open_string(parasym.vardef)) and
                    not(equal_defs(left.resultdef,parasym.vardef)) then
                   begin
                     current_filepos:=left.fileinfo;
                     CGMessage(type_e_strict_var_string_violation);
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
                       vs_out :
                         begin
                           if not valid_for_formal_var(left,true) then
                            CGMessagePos(left.fileinfo,parser_e_illegal_parameter_list);
                         end;
                       vs_const :
                         begin
                           if not valid_for_formal_const(left,true) then
                            CGMessagePos(left.fileinfo,parser_e_illegal_parameter_list);
                         end;
                     end;
                   end
                 else
                   begin
                     { check if the argument is allowed }
                     if (parasym.varspez in [vs_out,vs_var]) then
                       valid_for_var(left,true);
                   end;

                 if parasym.varspez in [vs_var,vs_out] then
                   set_unique(left);

                 { When the address needs to be pushed then the register is
                   not regable. Exception is when the location is also a var
                   parameter and we can pass the address transparently (but
                   that is handled by make_not_regable if ra_addr_regable is
                   passed, and make_not_regable always needs to called for
                   the ra_addr_taken info for non-invisble parameters }
                 if (
                     not(
                         (vo_is_hidden_para in parasym.varoptions) and
                         (left.resultdef.typ in [pointerdef,classrefdef])
                        ) and
                     paramanager.push_addr_param(parasym.varspez,parasym.vardef,
                         aktcallnode.procdefinition.proccalloption)
                    ) then
                   { pushing the address of a variable to take the place of a temp  }
                   { as the complex function result of a function does not make its }
                   { address escape the current block, as the "address of the       }
                   { function result" is not something which can be stored          }
                   { persistently by the callee (it becomes invalid when the callee }
                   { returns)                                                       }
                   if not(vo_is_funcret in parasym.varoptions) then
                     make_not_regable(left,[ra_addr_regable,ra_addr_taken])
                   else
                     make_not_regable(left,[ra_addr_regable]);

                  case parasym.varspez of
                    vs_out :
                      begin
                        { first set written separately to avoid false }
                        { uninitialized warnings (tbs/tb0542)         }
                        set_varstate(left,vs_written,[]);
                        set_varstate(left,vs_readwritten,[]);
                      end;
                    vs_var :
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
            if (not n.resultdef.needs_inittable or
                is_class(n.resultdef)) and
               (ttypeconvnode(n).left.resultdef.needs_inittable and
                not is_class(ttypeconvnode(n).left.resultdef)) then
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


    function tcallparanode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
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


    constructor tcallnode.createinternres(const name: string; params: tnode; res:tdef);
      var
        pd : tprocdef;
      begin
        createintern(name,params);
        typedef := res;
        include(callnodeflags,cnf_typedefset);
        pd:=tprocdef(symtableprocentry.ProcdefList[0]);
        { both the normal and specified resultdef either have to be returned via a }
        { parameter or not, but no mixing (JM)                                      }
        if paramanager.ret_in_param(typedef,pd.proccalloption) xor
          paramanager.ret_in_param(pd.returndef,pd.proccalloption) then
          internalerror(200108291);
      end;


    constructor tcallnode.createinternreturn(const name: string; params: tnode; returnnode : tnode);
      begin
        createintern(name,params);
        funcretnode:=returnnode;
      end;


    destructor tcallnode.destroy;
      begin
         methodpointer.free;
         callinitblock.free;
         callcleanupblock.free;
         funcretnode.free;
         if assigned(varargsparas) then
           varargsparas.free;
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


    procedure tcallnode.derefnode;
      begin
        if assigned(callinitblock) then
          callinitblock.derefnode;
        if assigned(methodpointer) then
          methodpointer.derefnode;
        if assigned(callcleanupblock) then
          callcleanupblock.derefnode;
        if assigned(funcretnode) then
          funcretnode.derefnode;
        inherited derefnode;
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
        if assigned(funcretnode) then
          n.funcretnode:=funcretnode.dogetcopy
        else
          n.funcretnode:=nil;
        if assigned(callcleanupblock) then
          n.callcleanupblock:=tblocknode(callcleanupblock.dogetcopy)
        else
          n.callcleanupblock:=nil;
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


    function tcallnode.is_simple_para_load(p:tnode; may_be_in_reg: boolean):boolean;
      var
        hp : tnode;
      begin
        hp:=p;
        while assigned(hp) and
              (hp.nodetype=typeconvn) and
              (ttypeconvnode(hp).convtype=tc_equal) do
          hp:=tunarynode(hp).left;
        result:=(hp.nodetype in [typen,loadvmtaddrn,loadn,temprefn,arrayconstructorn]);
        if result and
           not(may_be_in_reg) then
          case hp.nodetype of
            loadn:
              result:=(tabstractvarsym(tloadnode(hp).symtableentry).varregable in [vr_none,vr_addr]);
            temprefn:
              result:=not(ti_may_be_in_reg in ttemprefnode(hp).tempinfo^.flags);
          end;
      end;


    procedure tcallnode.maybe_load_in_temp(var p:tnode);
      var
        loadp,
        refp  : tnode;
        hdef : tdef;
        ptemp : ttempcreatenode;
        usederef : boolean;
        usevoidpointer : boolean;
      begin
        { Load all complex loads into a temp to prevent
          double calls to a function. We can't simply check for a hp.nodetype=calln }
        if assigned(p) and
           not is_simple_para_load(p,true) then
          begin
            { temp create }
            usederef:=(p.resultdef.typ in [arraydef,recorddef]) or
                      is_shortstring(p.resultdef) or
                      is_object(p.resultdef);
            { avoid refcount increase }
            usevoidpointer:=is_interface(p.resultdef);

            if usederef then
              hdef:=tpointerdef.create(p.resultdef)
            else
              hdef:=p.resultdef;

            if usevoidpointer then
              begin
                ptemp:=ctempcreatenode.create(voidpointertype,voidpointertype.size,tt_persistent,true);
                loadp:=ctypeconvnode.create_internal(p,voidpointertype);
                refp:=ctypeconvnode.create_internal(ctemprefnode.create(ptemp),hdef);
              end
            else
              begin
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
                  end
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
      begin
        len:=-1;
        loadconst:=true;
        hightree:=nil;
        case p.resultdef.typ of
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


    function tcallnode.gen_self_tree_methodpointer:tnode;
      var
        hsym : tfieldvarsym;
      begin
        { find self field in methodpointer record }
        hsym:=tfieldvarsym(trecorddef(methodpointertype).symtable.Find('self'));
        if not assigned(hsym) then
          internalerror(200305251);
        { Load tmehodpointer(right).self }
        result:=csubscriptnode.create(
                     hsym,
                     ctypeconvnode.create_internal(right.getcopy,methodpointertype));
      end;


    function tcallnode.gen_self_tree:tnode;
      var
        selftree : tnode;
      begin
        selftree:=nil;

        { When methodpointer was a callnode we must load it first into a
          temp to prevent the processing callnode twice }
        if (methodpointer.nodetype=calln) then
          internalerror(200405121);

        { inherited }
        if (cnf_inherited in callnodeflags) then
          selftree:=load_self_node
        else
          { constructors }
          if (procdefinition.proctypeoption=potype_constructor) then
            begin
              { push 0 as self when allocation is needed }
              if (methodpointer.resultdef.typ=classrefdef) or
                 (cnf_new_call in callnodeflags) then
                selftree:=cpointerconstnode.create(0,voidpointertype)
              else
                begin
                  if methodpointer.nodetype=typen then
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
              if (oo_has_vmt in tprocdef(procdefinition)._class.objectoptions) then
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
                  vmttree:=cloadvmtaddrnode.create(vmttree);
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
                       (nf_is_self in methodpointer.flags) then
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
              if (cnf_dispose_call in callnodeflags) then
                vmttree:=cloadvmtaddrnode.create(methodpointer.getcopy)
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
        realassignmenttarget:=aktassignmentnode.left.actualtargetnode;

        { when it is not passed in a parameter it will only be used after the
          function call }
        if not paramanager.ret_in_param(resultdef,procdefinition.proccalloption) then
          begin
            result:=true;
            exit;
          end;

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
            { If the funcret is also used as a parameter we can't optimize becuase the funcret
              and the parameter will point to the same address. That means that a change of the result variable
              will result also in a change of the parameter value }
            result:=not foreachnodestatic(left,@check_funcret_used_as_para,tloadnode(realassignmenttarget).symtableentry);
            exit;
          end;
      end;


    procedure tcallnode.maybe_create_funcret_node;
      var
        temp : ttempcreatenode;
      begin
        { For the function result we need to create a temp node for:
            - Inlined functions
            - Types requiring initialization/finalization
            - Types passed in parameters }
        if not is_void(resultdef) and
           not assigned(funcretnode) and
            (
             (cnf_do_inline in callnodeflags) or
             resultdef.needs_inittable or
             paramanager.ret_in_param(resultdef,procdefinition.proccalloption)
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
                temp:=ctempcreatenode.create(resultdef,resultdef.size,tt_persistent,false);
                include(temp.flags,nf_is_funcret);
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
                       para.left:=gen_self_tree_methodpointer
                     else
                       para.left:=gen_self_tree;
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
                     if not(assigned(procdefinition.owner.defowner)) then
                       internalerror(200309287);
                     para.left:=cloadparentfpnode.create(tprocdef(procdefinition.owner.defowner));
                   end
                else
                 if vo_is_range_check in para.parasym.varoptions then
                   begin
                     para.left:=cordconstnode.create(Ord(cs_check_range in current_settings.localswitches),booltype,false);
                   end
                else
                 if vo_is_overflow_check in para.parasym.varoptions then
                   begin
                     para.left:=cordconstnode.create(Ord(cs_check_overflow in current_settings.localswitches),booltype,false);
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
            hs:=pd.procsym.name+pd.typename_paras(false);
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
           not (nf_is_self in methodpointer.flags) then
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
          those which are overriden by parent classes.
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
                  hiddentree:=gen_high_tree(pt.left,tparavarsym(procdefinition.paras[i-1]).vardef);
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
        is_const : boolean;
        statements : tstatementnode;
        converted_result_data : ttempcreatenode;
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
                   candidates:=tcallcandidates.create(symtableprocentry,symtableproc,left,(nf_isproperty in flags),
                     { ignore possible private in delphi mode for anon. inherited (FK) }
                     (m_delphi in current_settings.modeswitches) and (cnf_anon_inherited in callnodeflags));

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
                              if assigned(left) then
                               current_filepos:=left.fileinfo;
                              CGMessage1(parser_e_wrong_parameter_size,symtableprocentry.realname);
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
          if (procdefinition.typ = procdef) then
            check_hints(tprocdef(procdefinition).procsym,tprocdef(procdefinition).symoptions);

          { add needed default parameters }
          if assigned(procdefinition) and
             (paralength<procdefinition.maxparacount) then
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
                until (paraidx>=procdefinition.paras.count) or not(vo_is_hidden_para in tparavarsym(procdefinition.paras[paraidx]).varoptions);
              end;
           end;

          { recursive call? }
          if assigned(current_procinfo) and
             (procdefinition=current_procinfo.procdef) then
            include(current_procinfo.flags,pi_is_recursive);

          { handle predefined procedures }
          is_const:=(po_internconst in procdefinition.procoptions) and
                    ((block_type in [bt_const,bt_type]) or
                     (assigned(left) and (tcallparanode(left).left.nodetype in [realconstn,ordconstn])));
          if (procdefinition.proccalloption=pocall_internproc) or is_const then
           begin
             if assigned(left) then
              begin
                { ptr and settextbuf needs two args }
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
                 is_class(tprocdef(procdefinition)._class) and
                 assigned(methodpointer) and
                 (nf_is_self in methodpointer.flags) then
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

               if (procdefinition.proctypeoption=potype_constructor) and
                  assigned(symtableproc) and
                  (symtableproc.symtabletype=withsymtable) and
                  (tnode(twithsymtable(symtableproc).withrefnode).nodetype=temprefn) then
                 CGmessage(cg_e_cannot_call_cons_dest_inside_with);

               { R.Init then R will be initialized by the constructor,
                 Also allow it for simple loads }
               if (procdefinition.proctypeoption=potype_constructor) or
                  ((hpt.nodetype=loadn) and
                   (methodpointer.resultdef.typ=objectdef) and
                   not(oo_has_virtual in tobjectdef(methodpointer.resultdef).objectoptions)
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
               (procdefinition.owner.symtabletype=ObjectSymtable) then
              internalerror(200305061);
          end;

         { Set flag that the procedure uses varargs, also if they are not passed it is still
           needed for x86_64 to pass the number of SSE registers used }
         if po_varargs in procdefinition.procoptions then
           include(callnodeflags,cnf_uses_varargs);

         { Change loading of array of const to varargs }
         if assigned(left) and
            is_array_of_const(tparavarsym(procdefinition.paras[procdefinition.paras.count-1]).vardef) and
            (procdefinition.proccalloption in [pocall_cppdecl,pocall_cdecl]) then
           convert_carg_array_of_const;

         { bind parasyms to the callparanodes and insert hidden parameters }
         bind_parasym;

         { insert type conversions for parameters }
         if assigned(left) then
           tcallparanode(left).insert_typeconv;

         { dispinterface methode invoke? }
         if assigned(methodpointer) and is_dispinterface(methodpointer.resultdef) then
           begin
             { if the result is used, we've to insert a call to convert the type to be on the "safe side" }
             if cnf_return_value_used in callnodeflags then
               begin
                 result:=internalstatements(statements);
                 converted_result_data:=ctempcreatenode.create(procdefinition.returndef,sizeof(procdefinition.returndef),tt_persistent,true);
                 addstatement(statements,converted_result_data);
                 addstatement(statements,cassignmentnode.create(ctemprefnode.create(converted_result_data),
                   ctypeconvnode.create_internal(translate_disp_call(methodpointer,parameters,nil,'',tprocdef(procdefinition).dispid,true),
                   procdefinition.returndef)));
                 addstatement(statements,ctempdeletenode.create_normal_temp(converted_result_data));
                 addstatement(statements,ctemprefnode.create(converted_result_data));
               end
             else
               result:=translate_disp_call(methodpointer,parameters,nil,'',tprocdef(procdefinition).dispid,false);

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
            while assigned(hp) do
              begin
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
{$ifdef i386}
                            { the i386 code generator expects all reference }
                            { parameter to be in this order so it can use   }
                            { pushes                                        }
                            if (hpcurr.parasym.paraloc[callerside].location^.reference.offset>hp.parasym.paraloc[callerside].location^.reference.offset) then
{$else i386}
                            if (node_complexity(hpcurr)<node_complexity(hp)) then
{$endif i386}
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
            if (st.symtabletype=ObjectSymtable) then
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
                    Comment(V_lineinfo+V_Debug,'Not inlining "'+tprocdef(procdefinition).procsym.realname+'", invocation parameter contains an unsafe/unsupported construct');
                    exclude(callnodeflags,cnf_do_inline);
                    break;
                  end;
                para:=tcallparanode(para.nextpara);
              end;
          end;
      end;


    function tcallnode.pass_1 : tnode;
      begin
         result:=nil;

         { Check if the call can be inlined, sets the cnf_do_inline flag }
         check_inlining;

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
             typecheckpass(callinitblock);
             dosimplify(callinitblock);
           end;
         if assigned(callcleanupblock) then
           begin
             typecheckpass(callcleanupblock);
             dosimplify(callcleanupblock);
           end;

         { Continue with checking a normal call or generate the inlined code }
         if cnf_do_inline in callnodeflags then
           result:=pass1_inline
         else
           result:=pass1_normal;
      end;


    function tcallnode.pass1_normal : tnode;
      begin
         result:=nil;

         { calculate the parameter info for the procdef }
         if not procdefinition.has_paraloc_info then
           begin
             procdefinition.requiredargarea:=paramanager.create_paraloc_info(procdefinition,callerside);
             procdefinition.has_paraloc_info:=true;
           end;

         { calculate the parameter size needed for this call include varargs if they are available }
         if assigned(varargsparas) then
           pushedparasize:=paramanager.create_varargs_paraloc_info(procdefinition,varargsparas)
         else
           pushedparasize:=procdefinition.requiredargarea;

         { record maximum parameter size used in this proc }
         current_procinfo.allocate_push_parasize(pushedparasize);

         { check for stacked parameters }
         if assigned(left) and
            (current_settings.optimizerswitches*[cs_opt_stackframe,cs_opt_level1]<>[]) then
           check_stack_parameters;

         if assigned(callinitblock) then
           firstpass(callinitblock);

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
           firstpass(callcleanupblock);

         if not (block_type in [bt_const,bt_type]) then
           include(current_procinfo.flags,pi_do_call);

         { order parameters }
         order_parameters;

         { get a register for the return value }
         if (not is_void(resultdef)) then
           begin
              if paramanager.ret_in_param(resultdef,procdefinition.proccalloption) then
               begin
                 expectloc:=LOC_REFERENCE;
               end
             else
             { ansi/widestrings must be registered, so we can dispose them }
              if is_ansistring(resultdef) or
                 is_widestring(resultdef) then
               begin
                 expectloc:=LOC_REFERENCE;
               end
             else
             { we have only to handle the result if it is used }
              if (cnf_return_value_used in callnodeflags) then
               begin
                 case resultdef.typ of
                   enumdef,
                   orddef :
                     begin
                       expectloc:=LOC_REGISTER;
                     end;
                   floatdef :
                     begin
                       expectloc:=LOC_FPUREGISTER;
                     end;
                   else
                     begin
                       expectloc:=procdefinition.funcretloc[callerside].loc;
                     end;
                 end;
               end
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
                      n.free;
                      n := paras.left.getcopy;
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
                  n.free;
                  n := temp;
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
            tempnode :=ctempcreatenode.create(tabstractvarsym(p).vardef,tabstractvarsym(p).vardef.size,tt_persistent,tabstractvarsym(p).is_regvar(false));
            addstatement(inlineinitstatement,tempnode);
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
      begin
        { parameters }
        para := tcallparanode(left);
        while assigned(para) do
          begin
            if (para.parasym.typ = paravarsym) then
              begin
                { must take copy of para.left, because if it contains a       }
                { temprefn pointing to a copied temp (e.g. methodpointer),    }
                { then this parameter must be changed to point to the copy of }
                { that temp (JM)                                              }
                n := para.left.getcopy;
                para.left.free;
                para.left := n;

                firstpass(para.left);

                { create temps for value parameters, function result and also for    }
                { const parameters which are passed by value instead of by reference }
                { we need to take care that we use the type of the defined parameter and not of the
                  passed parameter, because these can be different in case of a formaldef (PFV) }
                paracomplexity := node_complexity(para.left);
                { check if we have to create a temp, assign the parameter's }
                { contents to that temp and then substitute the paramter    }
                { with the temp everywhere in the function                  }
                if
                  ((tparavarsym(para.parasym).varregable in [vr_none,vr_addr]) and
                   not(para.left.expectloc in [LOC_REFERENCE,LOC_CREFERENCE]))  or
                  { we can't assign to formaldef temps }
                  ((para.parasym.vardef.typ<>formaldef) and
                   (
                    { if paracomplexity > 1, we normally take the address of   }
                    { the parameter expression, store it in a temp and         }
                    { substitute the dereferenced temp in the inlined function }
                    { We can't do this if we can't take the address of the     }
                    { parameter expression, so in that case assign to a temp   }
                    not(para.left.expectloc in [LOC_REFERENCE,LOC_CREFERENCE,LOC_CONSTANT]) or
                    ((paracomplexity > 1) and
                     (not valid_for_addr(para.left,false) or
                      (para.left.nodetype = calln) or
                      is_constnode(para.left))) or
                    { we do not need to create a temp for value parameters }
                    { which are not modified in the inlined function       }
                    { const parameters can get vs_readwritten if their     }
                    { address is taken                                     }
                    ((((para.parasym.varspez = vs_value) and
                       (para.parasym.varstate in [vs_initialised,vs_declared,vs_read])) or
                      { in case of const, this is only necessary if the     }
                      { variable would be passed by value normally, or if   }
                      { there is such a variable somewhere in an expression }
                       ((para.parasym.varspez = vs_const) and
                        (not paramanager.push_addr_param(vs_const,para.parasym.vardef,procdefinition.proccalloption) or
                         (paracomplexity > 1)))) and
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
                     { const para's can get vs_readwritten if their address }
                     { is taken                                             }
                     ((para.parasym.varstate = vs_readwritten) or
                      { call-by-reference const's may need to be passed by }
                      { reference to function called in the inlined code   }
                      (paramanager.push_addr_param(vs_const,para.parasym.vardef,procdefinition.proccalloption) and
                       (not valid_for_addr(para.left,false) or
                        is_constnode(para.left)))))
                   )
                  ) then
                  begin
                    if para.left.nodetype<>temprefn then
                      begin
                        tempnode := ctempcreatenode.create(para.parasym.vardef,para.parasym.vardef.size,tt_persistent,tparavarsym(para.parasym).is_regvar(false));
                        addstatement(inlineinitstatement,tempnode);
                        addstatement(inlinecleanupstatement,ctempdeletenode.create(tempnode));
                        addstatement(inlineinitstatement,cassignmentnode.create(ctemprefnode.create(tempnode),
                            para.left));
                        para.left := ctemprefnode.create(tempnode);
                        { inherit addr_taken flag }
                        if (tabstractvarsym(para.parasym).addr_taken) then
                          include(tempnode.tempinfo^.flags,ti_addr_taken);
                      end;
                  end
                { otherwise if the parameter is "complex", take the address   }
                { of the parameter expression, store it in a temp and replace }
                { occurrences of the parameter with dereferencings of this    }
                { temp                                                        }
                else if (paracomplexity > 1) then
                  begin
                    tempnode := ctempcreatenode.create(voidpointertype,voidpointertype.size,tt_persistent,tparavarsym(para.parasym).is_regvar(true));
                    addstatement(inlineinitstatement,tempnode);
                    addstatement(inlinecleanupstatement,ctempdeletenode.create(tempnode));
                    { inherit addr_taken flag }
                    if (tabstractvarsym(para.parasym).addr_taken) then
                      include(tempnode.tempinfo^.flags,ti_addr_taken);
                    addstatement(inlineinitstatement,cassignmentnode.create(ctemprefnode.create(tempnode),
                      caddrnode.create_internal(para.left)));
                    para.left := ctypeconvnode.create_internal(cderefnode.create(ctemprefnode.create(tempnode)),para.left.resultdef);
                  end;
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
        current_procinfo.flags := current_procinfo.flags + ((procdefinition as tprocdef).inlininginfo^.flags*inherited_inlining_flags);

        { Create new code block for inlining }
        inlineblock:=internalstatements(inlineinitstatement);
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
        typecheckpass(inlineblock);
        dosimplify(inlineblock);
        firstpass(inlineblock);
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


begin
   ccallnode:=tcallnode;
   ccallparanode:=tcallparanode;
end.
