{
    Copyright (c) 2011 by Jonas Maebe

    This unit provides helpers for creating new syms/defs based on string
    representations.

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
{$i fpcdefs.inc}

unit symcreat;

interface

  uses
    finput,tokens,scanner,
    symconst,symbase,symtype,symdef;


  type
    tscannerstate = record
      old_scanner: tscannerfile;
      old_token: ttoken;
      old_c: char;
      valid: boolean;
    end;

  { save/restore the scanner state before/after injecting }
  procedure replace_scanner(const tempname: string; out sstate: tscannerstate);
  procedure restore_scanner(const sstate: tscannerstate);

  { parses a (class or regular) method/constructor/destructor declaration from
    str, as if it were declared in astruct's declaration body

    WARNING: save the scanner state before calling this routine, and restore
      when done. }
  function str_parse_method_dec(str: ansistring; potype: tproctypeoption; is_classdef: boolean; astruct: tabstractrecorddef; out pd: tprocdef): boolean;

  { parses a (class or regular)  method/constructor/destructor implementation
    from str, as if it appeared in the current unit's implementation section

      WARNING: save the scanner state before calling this routine, and restore
        when done. }
  function str_parse_method_impl(str: ansistring; usefwpd: tprocdef; is_classdef: boolean):boolean;


  { in the JVM, constructors are not automatically inherited (so you can hide
    them). To emulate the Pascal behaviour, we have to automatically add
    all parent constructors to the current class as well.}
  procedure add_missing_parent_constructors_intf(obj: tobjectdef);

  { goes through all defs in st to add implementations for synthetic methods
    added earlier }
  procedure add_synthetic_method_implementations(st: tsymtable);


  procedure finish_copied_procdef(var pd: tprocdef; const realname: string; newparentst: tsymtable; newstruct: tabstractrecorddef);

  { create "parent frame pointer" record skeleton for procdef, in which local
    variables and parameters from pd accessed from nested routines can be
    stored }
  procedure build_parentfpstruct(pd: tprocdef);
  { checks whether sym (a local or para of pd) already has a counterpart in
    pd's parentfpstruct, and if not adds a new field to the struct with type
    "vardef" (can be different from sym's type in case it's a call-by-reference
    parameter, which is indicated by addrparam). If it already has a field in
    the parentfpstruct, this field is returned. }
  function maybe_add_sym_to_parentfpstruct(pd: tprocdef; sym: tsym; vardef: tdef; addrparam: boolean): tsym;
  { given a localvarsym or paravarsym of pd, returns the field of the
    parentfpstruct corresponding to this sym }
  function find_sym_in_parentfpstruct(pd: tprocdef; sym: tsym): tsym;
  { replaces all local and paravarsyms that have been mirrored in the
    parentfpstruct with aliasvarsyms that redirect to these fields (used to
    make sure that references to these syms in the owning procdef itself also
    use the ones in the parentfpstructs) }
  procedure redirect_parentfpstruct_local_syms(pd: tprocdef);
  { finalises the parentfpstruct (alignment padding, ...) }
  procedure finish_parentfpstruct(pd: tprocdef);


implementation

  uses
    cutils,globtype,globals,verbose,systems,comphook,fmodule,
    symsym,symtable,defutil,
    pbase,pdecobj,pdecsub,psub,
    node,nbas,nld,nmem,
    defcmp,
    paramgr
    {$ifdef jvm}
    ,pjvm
    {$endif};

  procedure replace_scanner(const tempname: string; out sstate: tscannerstate);
    var
      old_block_type: tblock_type;
    begin
      { would require saving of idtoken, pattern etc }
      if (token=_ID) then
        internalerror(2011032201);
      sstate.old_scanner:=current_scanner;
      sstate.old_token:=token;
      sstate.old_c:=c;
      sstate.valid:=true;
      { creating a new scanner resets the block type, while we want to continue
        in the current one }
      old_block_type:=block_type;
      current_scanner:=tscannerfile.Create('_Macro_.'+tempname);
      block_type:=old_block_type;
    end;


  procedure restore_scanner(const sstate: tscannerstate);
    begin
      if sstate.valid then
        begin
          current_scanner.free;
          current_scanner:=sstate.old_scanner;
          token:=sstate.old_token;
          c:=sstate.old_c;
        end;
    end;


  function str_parse_method_dec(str: ansistring; potype: tproctypeoption; is_classdef: boolean; astruct: tabstractrecorddef; out pd: tprocdef): boolean;
    var
      oldparse_only: boolean;
    begin
      Message1(parser_d_internal_parser_string,str);
      oldparse_only:=parse_only;
      parse_only:=true;
      result:=false;
      { inject the string in the scanner }
      str:=str+'end;';
      current_scanner.substitutemacro('meth_head_macro',@str[1],length(str),current_scanner.line_no,current_scanner.inputfile.ref_index);
      current_scanner.readtoken(false);
      { and parse it... }
      case potype of
        potype_class_constructor:
          pd:=class_constructor_head(astruct);
        potype_class_destructor:
          pd:=class_destructor_head(astruct);
        potype_constructor:
          pd:=constructor_head;
        potype_destructor:
          pd:=destructor_head;
        else
          pd:=method_dec(astruct,is_classdef);
      end;
      if assigned(pd) then
        result:=true;
      parse_only:=oldparse_only;
    end;


  function str_parse_method_impl(str: ansistring; usefwpd: tprocdef; is_classdef: boolean):boolean;
     var
       oldparse_only: boolean;
       tmpstr: ansistring;
     begin
      if ((status.verbosity and v_debug)<>0) then
        begin
           if assigned(usefwpd) then
             Message1(parser_d_internal_parser_string,usefwpd.customprocname([pno_proctypeoption,pno_paranames,pno_noclassmarker,pno_noleadingdollar])+str)
           else
             begin
               if is_classdef then
                 tmpstr:='class '
               else
                 tmpstr:='';
               Message1(parser_d_internal_parser_string,tmpstr+str);
             end;
        end;
      oldparse_only:=parse_only;
      parse_only:=false;
      result:=false;
      { inject the string in the scanner }
      str:=str+'end;';
      current_scanner.substitutemacro('meth_impl_macro',@str[1],length(str),current_scanner.line_no,current_scanner.inputfile.ref_index);
      current_scanner.readtoken(false);
      { and parse it... }
      read_proc(is_classdef,usefwpd);
      parse_only:=oldparse_only;
      result:=true;
     end;


  procedure add_missing_parent_constructors_intf(obj: tobjectdef);
    var
      parent: tobjectdef;
      def: tdef;
      parentpd,
      childpd: tprocdef;
      i: longint;
      srsym: tsym;
      srsymtable: tsymtable;
    begin
      if (oo_is_external in obj.objectoptions) or
         not assigned(obj.childof) then
        exit;
      parent:=obj.childof;
      { find all constructor in the parent }
      for i:=0 to tobjectsymtable(parent.symtable).deflist.count-1 do
        begin
          def:=tdef(tobjectsymtable(parent.symtable).deflist[i]);
          if (def.typ<>procdef) or
             (tprocdef(def).proctypeoption<>potype_constructor) or
             not is_visible_for_object(tprocdef(def),obj) then
            continue;
          parentpd:=tprocdef(def);
          { do we have this constructor too? (don't use
            search_struct_member/searchsym_in_class, since those will
            search parents too) }
          if searchsym_in_record(obj,parentpd.procsym.name,srsym,srsymtable) then
            begin
              { there's a symbol with the same name, is it a constructor
                with the same parameters? }
              if srsym.typ=procsym then
                begin
                  childpd:=tprocsym(srsym).find_procdef_bytype_and_para(
                    potype_constructor,parentpd.paras,nil,
                    [cpo_ignorehidden,cpo_ignoreuniv,cpo_openequalisexact]);
                  if assigned(childpd) then
                    continue;
                end;
            end;
          { if we get here, we did not find it in the current objectdef ->
            add }
          childpd:=tprocdef(parentpd.getcopy);
          finish_copied_procdef(childpd,parentpd.procsym.realname,obj.symtable,obj);
          exclude(childpd.procoptions,po_external);
          include(childpd.procoptions,po_overload);
          childpd.synthetickind:=tsk_anon_inherited;
          include(obj.objectoptions,oo_has_constructor);
        end;
    end;


  procedure implement_anon_inherited(pd: tprocdef);
    var
      str: ansistring;
      isclassmethod: boolean;
    begin
      isclassmethod:=
        (po_classmethod in pd.procoptions) and
        not(pd.proctypeoption in [potype_constructor,potype_destructor]);
      str:='begin inherited end;';
      str_parse_method_impl(str,pd,isclassmethod);
    end;


  procedure implement_jvm_clone(pd: tprocdef);
    var
      struct: tabstractrecorddef;
      str: ansistring;
      i: longint;
      sym: tsym;
      fsym: tfieldvarsym;
    begin
      if not(pd.struct.typ in [recorddef,objectdef]) then
        internalerror(2011032802);
      struct:=pd.struct;
      { anonymous record types must get an artificial name, so we can generate
        a typecast at the scanner level }
      if (struct.typ=recorddef) and
         not assigned(struct.typesym) then
        internalerror(2011032812);
      { the inherited clone will already copy all fields in a shallow way ->
        copy records/regular arrays in a regular way }
      str:='type _fpc_ptrt = ^'+struct.typesym.realname+'; begin clone:=inherited;';
      for i:=0 to struct.symtable.symlist.count-1 do
        begin
          sym:=tsym(struct.symtable.symlist[i]);
          if (sym.typ=fieldvarsym) then
            begin
              fsym:=tfieldvarsym(sym);
              if (fsym.vardef.typ=recorddef) or
                 ((fsym.vardef.typ=arraydef) and
                  not is_dynamic_array(fsym.vardef)) or
                 ((fsym.vardef.typ=setdef) and
                  not is_smallset(fsym.vardef)) then
                str:=str+'_fpc_ptrt(clone)^.'+fsym.realname+':='+fsym.realname+';';
            end;
        end;
      str:=str+'end;';
      str_parse_method_impl(str,pd,false);
    end;


  procedure implement_record_deepcopy(pd: tprocdef);
    var
      struct: tabstractrecorddef;
      str: ansistring;
      i: longint;
      sym: tsym;
      fsym: tfieldvarsym;
    begin
      if not(pd.struct.typ in [recorddef,objectdef]) then
        internalerror(2011032810);
      struct:=pd.struct;
      { anonymous record types must get an artificial name, so we can generate
        a typecast at the scanner level }
      if (struct.typ=recorddef) and
         not assigned(struct.typesym) then
        internalerror(2011032811);
      { copy all fields }
      str:='begin ';
      for i:=0 to struct.symtable.symlist.count-1 do
        begin
          sym:=tsym(struct.symtable.symlist[i]);
          if (sym.typ=fieldvarsym) then
            begin
              fsym:=tfieldvarsym(sym);
              str:=str+'result.'+fsym.realname+':='+fsym.realname+';';
            end;
        end;
      str:=str+'end;';
      str_parse_method_impl(str,pd,false);
    end;


  procedure implement_empty(pd: tprocdef);
    var
      str: ansistring;
      isclassmethod: boolean;
    begin
      isclassmethod:=
        (po_classmethod in pd.procoptions) and
        not(pd.proctypeoption in [potype_constructor,potype_destructor]);
      str:='begin end;';
      str_parse_method_impl(str,pd,isclassmethod);
    end;



  procedure add_synthetic_method_implementations_for_struct(struct: tabstractrecorddef);
    var
      i   : longint;
      def : tdef;
      pd  : tprocdef;
    begin
      for i:=0 to struct.symtable.deflist.count-1 do
        begin
          def:=tdef(struct.symtable.deflist[i]);
          if (def.typ<>procdef) then
            continue;
          pd:=tprocdef(def);
          case pd.synthetickind of
            tsk_none:
              ;
            tsk_anon_inherited:
              implement_anon_inherited(pd);
            tsk_jvm_clone:
              implement_jvm_clone(pd);
            tsk_record_deepcopy:
              implement_record_deepcopy(pd);
            tsk_empty,
            { special handling for this one is done in tnodeutils.wrap_proc_body }
            tsk_tcinit:
              implement_empty(pd);
            else
              internalerror(2011032801);
          end;
        end;
    end;


  procedure add_synthetic_method_implementations(st: tsymtable);
    var
      i: longint;
      def: tdef;
      sstate: tscannerstate;
    begin
      { only necessary for the JVM target currently }
      if not (target_info.system in [system_jvm_java32]) then
        exit;
      sstate.valid:=false;
      for i:=0 to st.deflist.count-1 do
        begin
          def:=tdef(st.deflist[i]);
          if (def.typ=procdef) and
             assigned(tprocdef(def).localst) and
             { not true for the "main" procedure, whose localsymtable is the staticsymtable }
             (tprocdef(def).localst.symtabletype=localsymtable) then
            add_synthetic_method_implementations(tprocdef(def).localst)
          else if (is_javaclass(def) and
              not(oo_is_external in tobjectdef(def).objectoptions)) or
              (def.typ=recorddef) then
           begin
             if not sstate.valid then
               replace_scanner('synthetic_impl',sstate);
            add_synthetic_method_implementations_for_struct(tabstractrecorddef(def));
            { also complete nested types }
            add_synthetic_method_implementations(tabstractrecorddef(def).symtable);
           end;
        end;
      restore_scanner(sstate);
    end;


  procedure finish_copied_procdef(var pd: tprocdef; const realname: string; newparentst: tsymtable; newstruct: tabstractrecorddef);
    var
      sym: tsym;
      parasym: tparavarsym;
      ps: tprocsym;
      stname: string;
      i: longint;
    begin
      { associate the procdef with a procsym in the owner }
      if not(pd.proctypeoption in [potype_class_constructor,potype_class_destructor]) then
        stname:=upper(realname)
      else
        stname:=lower(realname);
      sym:=tsym(newparentst.find(stname));
      if assigned(sym) then
        begin
          if sym.typ<>procsym then
            internalerror(2011040601);
          ps:=tprocsym(sym);
        end
      else
        begin
          ps:=tprocsym.create(realname);
          newparentst.insert(ps);
        end;
      pd.procsym:=ps;
      pd.struct:=newstruct;
      { in case of methods, replace the special parameter types with new ones }
      if assigned(newstruct) then
        begin
          symtablestack.push(pd.parast);
          for i:=0 to pd.paras.count-1 do
            begin
              parasym:=tparavarsym(pd.paras[i]);
              if vo_is_self in parasym.varoptions then
                begin
                  if parasym.vardef.typ=classrefdef then
                    parasym.vardef:=tclassrefdef.create(newstruct)
                  else
                    parasym.vardef:=newstruct;
                end
            end;
          { also fix returndef in case of a constructor }
          if pd.proctypeoption=potype_constructor then
            pd.returndef:=newstruct;
          symtablestack.pop(pd.parast);
        end;
      proc_add_definition(pd);
    end;


  procedure build_parentfpstruct(pd: tprocdef);
    var
      nestedvars: tsym;
      nestedvarsst: tsymtable;
      pnestedvarsdef,
      nestedvarsdef: tdef;
      old_symtablestack: tsymtablestack;
    begin
      { make sure the defs are not registered in the current symtablestack,
        because they may be for a parent procdef (changeowner does remove a def
        from the symtable in which it was originally created, so that by itself
        is not enough) }
      old_symtablestack:=symtablestack;
      symtablestack:=old_symtablestack.getcopyuntil(current_module.localsymtable);
      { create struct to hold local variables and parameters that are
        accessed from within nested routines }
      nestedvarsst:=trecordsymtable.create(current_module.realmodulename^+'$$_fpc_nestedvars$'+tostr(pd.procsym.symid),current_settings.alignment.localalignmax);
      nestedvarsdef:=trecorddef.create(nestedvarsst.name^,nestedvarsst);
{$ifdef jvm}
      jvm_guarantee_record_typesym(nestedvarsdef,nestedvarsdef.owner);
      { don't add clone/FpcDeepCopy, because the field names are not all
        representable in source form and we don't need them anyway }
      symtablestack.push(trecorddef(nestedvarsdef).symtable);
      maybe_add_public_default_java_constructor(trecorddef(nestedvarsdef));
      symtablestack.pop(trecorddef(nestedvarsdef).symtable);
{$endif}
      symtablestack.free;
      symtablestack:=old_symtablestack.getcopyuntil(pd.localst);
      pnestedvarsdef:=tpointerdef.create(nestedvarsdef);
      nestedvars:=tlocalvarsym.create('$nestedvars',vs_var,nestedvarsdef,[]);
      pd.localst.insert(nestedvars);
      pd.parentfpstruct:=nestedvars;
      pd.parentfpstructptrtype:=pnestedvarsdef;

      pd.parentfpinitblock:=cblocknode.create(nil);
      symtablestack.free;
      symtablestack:=old_symtablestack;
    end;


  function maybe_add_sym_to_parentfpstruct(pd: tprocdef; sym: tsym; vardef: tdef; addrparam: boolean): tsym;
    var
      fieldvardef,
      nestedvarsdef: tdef;
      nestedvarsst: tsymtable;
      initcode: tnode;
      old_filepos: tfileposinfo;
    begin
      nestedvarsdef:=tlocalvarsym(pd.parentfpstruct).vardef;
      result:=search_struct_member(trecorddef(nestedvarsdef),sym.name);
      if not assigned(result) then
        begin
          { mark that this symbol is mirrored in the parentfpstruct }
          tabstractnormalvarsym(sym).inparentfpstruct:=true;
          { add field to the struct holding all locals accessed
            by nested routines }
          nestedvarsst:=trecorddef(nestedvarsdef).symtable;
          { indicate whether or not this is a var/out/constref/... parameter }
          if addrparam then
            fieldvardef:=tpointerdef.create(vardef)
          else
            fieldvardef:=vardef;
          result:=tfieldvarsym.create(sym.realname,vs_value,fieldvardef,[]);
          if nestedvarsst.symlist.count=0 then
            include(tfieldvarsym(result).varoptions,vo_is_first_field);
          nestedvarsst.insert(result);
          trecordsymtable(nestedvarsst).addfield(tfieldvarsym(result),vis_public);

          { add initialization with original value if it's a parameter }
          if (sym.typ=paravarsym) then
            begin
              old_filepos:=current_filepos;
              fillchar(current_filepos,sizeof(current_filepos),0);
              initcode:=cloadnode.create(sym,sym.owner);
              { indicate that this load should not be transformed into a load
                from the parentfpstruct, but instead should load the original
                value }
              include(initcode.flags,nf_internal);
              { in case it's a var/out/constref parameter, store the address of the
                parameter in the struct }
              if addrparam then
                begin
                  initcode:=caddrnode.create_internal(initcode);
                  include(initcode.flags,nf_typedaddr);
                end;
              initcode:=cassignmentnode.create(
                csubscriptnode.create(result,cloadnode.create(pd.parentfpstruct,pd.parentfpstruct.owner)),
                initcode);
              tblocknode(pd.parentfpinitblock).left:=cstatementnode.create
                (initcode,tblocknode(pd.parentfpinitblock).left);
              current_filepos:=old_filepos;
            end;
        end;
    end;


  procedure redirect_parentfpstruct_local_syms(pd: tprocdef);
    var
      nestedvarsdef: trecorddef;
      sl: tpropaccesslist;
      fsym,
      lsym,
      aliassym: tsym;
      i: longint;
    begin
      nestedvarsdef:=trecorddef(tlocalvarsym(pd.parentfpstruct).vardef);
      for i:=0 to nestedvarsdef.symtable.symlist.count-1 do
        begin
          fsym:=tsym(nestedvarsdef.symtable.symlist[i]);
          if fsym.typ<>fieldvarsym then
            continue;
          lsym:=tsym(pd.localst.find(fsym.name));
          if not assigned(lsym) then
            lsym:=tsym(pd.parast.find(fsym.name));
          if not assigned(lsym) then
            internalerror(2011060408);
          { add an absolute variable that redirects to the field }
          sl:=tpropaccesslist.create;
          sl.addsym(sl_load,pd.parentfpstruct);
          sl.addsym(sl_subscript,tfieldvarsym(fsym));
          aliassym:=tabsolutevarsym.create_ref(lsym.name,tfieldvarsym(fsym).vardef,sl);
          { hide the original variable (can't delete, because there
            may be other loadnodes that reference it)
            -- only for locals; hiding parameters changes the
            function signature }
          if lsym.typ<>paravarsym then
            hidesym(lsym);
          { insert the absolute variable in the localst of the
            routine; ignore duplicates, because this will also check the
            parasymtable and we want to override parameters with our local
            versions }
          pd.localst.insert(aliassym,false);
        end;
    end;


  function find_sym_in_parentfpstruct(pd: tprocdef; sym: tsym): tsym;
    var
      nestedvarsdef: tdef;
    begin
      nestedvarsdef:=tlocalvarsym(pd.parentfpstruct).vardef;
      result:=search_struct_member(trecorddef(nestedvarsdef),sym.name);
    end;


  procedure finish_parentfpstruct(pd: tprocdef);
    begin
      trecordsymtable(trecorddef(tlocalvarsym(pd.parentfpstruct).vardef).symtable).addalignmentpadding;
    end;




end.

