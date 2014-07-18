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
    finput,tokens,scanner,globtype,
    aasmdata,
    symconst,symbase,symtype,symdef,symsym;


  type
    tscannerstate = record
      old_scanner: tscannerfile;
      old_filepos: tfileposinfo;
      old_token: ttoken;
      old_c: char;
      old_orgpattern: string;
      old_modeswitches: tmodeswitches;
      old_idtoken: ttoken;
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

      WARNINGS:
        * save the scanner state before calling this routine, and restore when done.
        * the code *must* be written in objfpc style
  }
  function str_parse_method_impl(str: ansistring; usefwpd: tprocdef; is_classdef: boolean):boolean;

  { parses a typed constant assignment to ssym

      WARNINGS:
        * save the scanner state before calling this routine, and restore when done.
        * the code *must* be written in objfpc style
  }
  procedure str_parse_typedconst(list: TAsmList; str: ansistring; ssym: tstaticvarsym);



  { in the JVM, constructors are not automatically inherited (so you can hide
    them). To emulate the Pascal behaviour, we have to automatically add
    all parent constructors to the current class as well. We also have to do
    the same for the (emulated) virtual class methods }
  procedure add_missing_parent_constructors_intf(obj: tobjectdef; addvirtclassmeth: boolean; forcevis: tvisibility);

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

  procedure maybe_guarantee_record_typesym(var def: tdef; st: tsymtable);

  { turns a fieldvarsym into a class/static field definition, and returns the
    created staticvarsym that is responsible for allocating the global storage }
  function make_field_static(recst: tsymtable; fieldvs: tfieldvarsym): tstaticvarsym;


implementation

  uses
    cutils,cclasses,globals,verbose,systems,comphook,fmodule,
    symtable,defutil,
    pbase,pdecobj,pdecsub,psub,ptconst,
{$ifdef jvm}
    pjvm,jvmdef,
{$endif jvm}
    node,nbas,nld,nmem,ngenutil,
    defcmp,
    paramgr;

  procedure replace_scanner(const tempname: string; out sstate: tscannerstate);
    var
      old_block_type: tblock_type;
    begin
      { would require saving of cstringpattern, patternw }
      if (token=_CSTRING) or
         (token=_CWCHAR) or
         (token=_CWSTRING) then
        internalerror(2011032201);
      sstate.old_scanner:=current_scanner;
      sstate.old_filepos:=current_filepos;
      sstate.old_token:=token;
      sstate.old_c:=c;
      sstate.old_orgpattern:=orgpattern;
      sstate.old_modeswitches:=current_settings.modeswitches;
      sstate.old_idtoken:=idtoken;
      sstate.valid:=true;
      { creating a new scanner resets the block type, while we want to continue
        in the current one }
      old_block_type:=block_type;
      current_scanner:=tscannerfile.Create('_Macro_.'+tempname,true);
      block_type:=old_block_type;
      { required for e.g. FpcDeepCopy record method (uses "out" parameter; field
        names are escaped via &, so should not cause conflicts }
      current_settings.modeswitches:=objfpcmodeswitches;
    end;


  procedure restore_scanner(const sstate: tscannerstate);
    begin
      if sstate.valid then
        begin
          current_scanner.free;
          current_scanner:=sstate.old_scanner;
          current_filepos:=sstate.old_filepos;
          token:=sstate.old_token;
          current_settings.modeswitches:=sstate.old_modeswitches;
          c:=sstate.old_c;
          orgpattern:=sstate.old_orgpattern;
          pattern:=upper(sstate.old_orgpattern);
          idtoken:=sstate.old_idtoken;
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
      { in case multiple strings are injected, make sure to always close the
        previous macro inputfile to prevent memory leaks }
      if assigned(current_scanner.inputfile) and
         not(current_scanner.inputfile.closed) then
        current_scanner.closeinputfile;
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
        else if assigned(astruct) and
           (astruct.typ=recorddef) then
          pd:=parse_record_method_dec(astruct,is_classdef)
        else
          pd:=method_dec(astruct,is_classdef);
      end;
      if assigned(pd) then
        result:=true;
      parse_only:=oldparse_only;
      { remove the temporary macro input file again }
      current_scanner.closeinputfile;
      current_scanner.nextfile;
      current_scanner.tempopeninputfile;
    end;


  function str_parse_method_impl(str: ansistring; usefwpd: tprocdef; is_classdef: boolean):boolean;
     var
       oldparse_only: boolean;
       tmpstr: ansistring;
     begin
      if ((status.verbosity and v_debug)<>0) then
        begin
           if assigned(usefwpd) then
             Message1(parser_d_internal_parser_string,usefwpd.customprocname([pno_proctypeoption,pno_paranames,pno_ownername,pno_noclassmarker,pno_noleadingdollar])+str)
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
      str:=str+'const;';
      current_scanner.substitutemacro('meth_impl_macro',@str[1],length(str),current_scanner.line_no,current_scanner.inputfile.ref_index);
      current_scanner.readtoken(false);
      { and parse it... }
      read_proc(is_classdef,usefwpd);
      parse_only:=oldparse_only;
      { remove the temporary macro input file again }
      current_scanner.closeinputfile;
      current_scanner.nextfile;
      current_scanner.tempopeninputfile;
      result:=true;
     end;


  procedure str_parse_typedconst(list: TAsmList; str: ansistring; ssym: tstaticvarsym);
    var
      old_block_type: tblock_type;
      old_parse_only: boolean;
    begin
      Message1(parser_d_internal_parser_string,str);
      { a string that will be interpreted as the start of a new section ->
        typed constant parsing will stop }
      str:=str+'type ';
      old_parse_only:=parse_only;
      old_block_type:=block_type;
      parse_only:=true;
      block_type:=bt_const;
      current_scanner.substitutemacro('typed_const_macro',@str[1],length(str),current_scanner.line_no,current_scanner.inputfile.ref_index);
      current_scanner.readtoken(false);
      read_typed_const(list,ssym,ssym.owner.symtabletype in [recordsymtable,objectsymtable]);
      parse_only:=old_parse_only;
      block_type:=old_block_type;
      { remove the temporary macro input file again }
      current_scanner.closeinputfile;
      current_scanner.nextfile;
      current_scanner.tempopeninputfile;
    end;


  procedure add_missing_parent_constructors_intf(obj: tobjectdef; addvirtclassmeth: boolean; forcevis: tvisibility);
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
             ((tprocdef(def).proctypeoption<>potype_constructor) and
              (not addvirtclassmeth or
               not([po_classmethod,po_virtualmethod]<=tprocdef(def).procoptions))) or
             not is_visible_for_object(tprocdef(def),obj) then
            continue;
          parentpd:=tprocdef(def);
          { do we have this constructor too? (don't use
            search_struct_member/searchsym_in_class, since those will
            search parents too) }
          if searchsym_in_record(obj,parentpd.procsym.name,srsym,srsymtable) then
            begin
              { there's a symbol with the same name, is it a routine of the
                same type with the same parameters? }
              if srsym.typ=procsym then
                begin
                  childpd:=tprocsym(srsym).find_procdef_bytype_and_para(
                    tprocdef(def).proctypeoption,parentpd.paras,nil,
                    [cpo_ignorehidden,cpo_ignoreuniv,cpo_openequalisexact]);
                  if assigned(childpd) then
                    continue;
                end;
            end;
          { if we get here, we did not find it in the current objectdef ->
            add }
          childpd:=tprocdef(parentpd.getcopy);
          { get rid of the import name for inherited virtual class methods,
            it has to be regenerated rather than amended }
          if [po_classmethod,po_virtualmethod]<=childpd.procoptions then
            begin
              stringdispose(childpd.import_name);
              exclude(childpd.procoptions,po_has_importname);
            end;
          if forcevis<>vis_none then
            childpd.visibility:=forcevis;
          if po_virtualmethod in childpd.procoptions then
            include(childpd.procoptions,po_overridingmethod);
          { ignore this artificially added procdef when looking for overloads }
          include(childpd.procoptions,po_ignore_for_overload_resolution);
          finish_copied_procdef(childpd,parentpd.procsym.realname,obj.symtable,obj);
          exclude(childpd.procoptions,po_external);
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
      str:='begin ';
      if (pd.proctypeoption<>potype_constructor) and
         not is_void(pd.returndef) then
        str:=str+'result:=';
      str:=str+'inherited end;';
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
      { We cannot easily use the inherited clone in case we have to create a
        deep copy of certain fields. The reason is that e.g. sets are pointers
        at the JVM level, but not in Pascal. So the JVM clone routine will copy
        the pointer to the set from the old record (= class instance) to the new
        one, but we have no way to change this pointer itself from inside Pascal
        code.

        We solve this by relying on the fact that the JVM is garbage collected:
        we simply declare a temporary instance on the stack, which will be
        allocated/initialized by the temp generator. We return its address as
        the result of the clone routine, so it remains live. }
      str:='var __fpc_newcopy:'+ struct.typesym.realname+'; begin clone:=JLObject(@__fpc_newcopy);';
      { copy all field contents }
      for i:=0 to struct.symtable.symlist.count-1 do
        begin
          sym:=tsym(struct.symtable.symlist[i]);
          if (sym.typ=fieldvarsym) then
            begin
              fsym:=tfieldvarsym(sym);
              str:=str+'__fpc_newcopy.&'+fsym.realname+':=&'+fsym.realname+';';
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
      str:='type _fpc_ptrt = ^'+struct.typesym.realname+'; var res: _fpc_ptrt; begin res:=_fpc_ptrt(result);';
      for i:=0 to struct.symtable.symlist.count-1 do
        begin
          sym:=tsym(struct.symtable.symlist[i]);
          if (sym.typ=fieldvarsym) then
            begin
              fsym:=tfieldvarsym(sym);
              str:=str+'res^.&'+fsym.realname+':=&'+fsym.realname+';';
            end;
        end;
      str:=str+'end;';
      str_parse_method_impl(str,pd,false);
    end;


  procedure implement_record_initialize(pd: tprocdef);
    var
      struct: tabstractrecorddef;
      str: ansistring;
      i: longint;
      sym: tsym;
      fsym: tfieldvarsym;
    begin
      if not(pd.struct.typ in [recorddef,objectdef]) then
        internalerror(2011071710);
      struct:=pd.struct;
      { anonymous record types must get an artificial name, so we can generate
        a typecast at the scanner level }
      if (struct.typ=recorddef) and
         not assigned(struct.typesym) then
        internalerror(2011032811);
      { walk over all fields that need initialization }
      str:='begin ';
      for i:=0 to struct.symtable.symlist.count-1 do
        begin
          sym:=tsym(struct.symtable.symlist[i]);
          if (sym.typ=fieldvarsym) then
            begin
              fsym:=tfieldvarsym(sym);
              if fsym.vardef.needs_inittable then
                str:=str+'system.initialize(&'+fsym.realname+');';
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


  procedure addvisibibleparameters(var str: ansistring; pd: tprocdef);
    var
      currpara: tparavarsym;
      i: longint;
      firstpara: boolean;
    begin
      firstpara:=true;
      for i:=0 to pd.paras.count-1 do
        begin
          currpara:=tparavarsym(pd.paras[i]);
          if not(vo_is_hidden_para in currpara.varoptions) then
            begin
              if not firstpara then
                str:=str+',';
              firstpara:=false;
              str:=str+currpara.realname;
            end;
        end;
    end;



  procedure implement_callthrough(pd: tprocdef);
    var
      str: ansistring;
      callpd: tprocdef;
      isclassmethod: boolean;
    begin
      isclassmethod:=
        (po_classmethod in pd.procoptions) and
        not(pd.proctypeoption in [potype_constructor,potype_destructor]);
      callpd:=tprocdef(pd.skpara);
      str:='begin ';
      if pd.returndef<>voidtype then
        str:=str+'result:=';
      str:=str+callpd.procsym.realname+'(';
      addvisibibleparameters(str,pd);
      str:=str+') end;';
      str_parse_method_impl(str,pd,isclassmethod);
    end;


{$ifdef jvm}
  procedure implement_jvm_enum_values(pd: tprocdef);
    begin
      str_parse_method_impl('begin result:=__fpc_FVALUES end;',pd,true);
    end;


  procedure implement_jvm_enum_valuof(pd: tprocdef);
    begin
      str_parse_method_impl('begin result:=__FPC_TEnumClassAlias(inherited valueOf(JLClass(__FPC_TEnumClassAlias),__fpc_str)) end;',pd,true);
    end;


  procedure implement_jvm_enum_jumps_constr(pd: tprocdef);
    begin
      str_parse_method_impl('begin inherited create(__fpc_name,__fpc_ord); __fpc_fenumval:=__fpc_initenumval end;',pd,false);
    end;


  procedure implement_jvm_enum_fpcordinal(pd: tprocdef);
    var
      enumclass: tobjectdef;
      enumdef: tenumdef;
    begin
      enumclass:=tobjectdef(pd.owner.defowner);
      enumdef:=tenumdef(ttypesym(search_struct_member(enumclass,'__FPC_TENUMALIAS')).typedef);
      if not enumdef.has_jumps then
        str_parse_method_impl('begin result:=ordinal end;',pd,false)
      else
        str_parse_method_impl('begin result:=__fpc_fenumval end;',pd,false);
    end;


  procedure implement_jvm_enum_fpcvalueof(pd: tprocdef);
    var
      enumclass: tobjectdef;
      enumdef: tenumdef;
      isclassmethod: boolean;
    begin
      isclassmethod:=
        (po_classmethod in pd.procoptions) and
        not(pd.proctypeoption in [potype_constructor,potype_destructor]);
      enumclass:=tobjectdef(pd.owner.defowner);
      enumdef:=tenumdef(ttypesym(search_struct_member(enumclass,'__FPC_TENUMALIAS')).typedef);
      { convert integer to corresponding enum instance: in case of no jumps
        get it from the $VALUES array, otherwise from the __fpc_ord2enum
        hashmap }
      if not enumdef.has_jumps then
        str_parse_method_impl('begin result:=__fpc_FVALUES[__fpc_int] end;',pd,isclassmethod)
      else
        str_parse_method_impl('begin result:=__FPC_TEnumClassAlias(__fpc_ord2enum.get(JLInteger.valueOf(__fpc_int))) end;',pd,isclassmethod);
    end;


  function CompareEnumSyms(Item1, Item2: Pointer): Integer;
    var
      I1 : tenumsym absolute Item1;
      I2 : tenumsym absolute Item2;
    begin
      Result:=I1.value-I2.value;
    end;


  procedure implement_jvm_enum_classconstr(pd: tprocdef);
    var
      enumclass: tobjectdef;
      enumdef: tenumdef;
      enumname,
      str: ansistring;
      i: longint;
      enumsym: tenumsym;
      orderedenums: tfpobjectlist;
    begin
      enumclass:=tobjectdef(pd.owner.defowner);
      enumdef:=tenumdef(ttypesym(search_struct_member(enumclass,'__FPC_TENUMALIAS')).typedef);
      if not assigned(enumdef) then
        internalerror(2011062305);
      str:='begin ';
      if enumdef.has_jumps then
        { init hashmap for ordinal -> enum instance mapping; don't let it grow,
          and set the capacity to the next prime following the total number of
          enum elements to minimise the number of collisions }
        str:=str+'__fpc_ord2enum:=JUHashMap.Create('+tostr(next_prime(enumdef.symtable.symlist.count))+',1.0);';
      { iterate over all enum elements and initialise the class fields, and
        store them in the values array. Since the java.lang.Enum doCompare
        method is final and hardcoded to compare based on declaration order
        (= java.lang.Enum.ordinal() value), we have to create them in order of
        ascending FPC ordinal values (which may not be the same as the FPC
        declaration order in case of jumps }
      orderedenums:=tfpobjectlist.create(false);
      for i:=0 to enumdef.symtable.symlist.count-1 do
        orderedenums.add(enumdef.symtable.symlist[i]);
      if enumdef.has_jumps then
        orderedenums.sort(@CompareEnumSyms);
      for i:=0 to orderedenums.count-1 do
        begin
          enumsym:=tenumsym(orderedenums[i]);
          enumname:=enumsym.realname;
          str:=str+enumsym.name+':=__FPC_TEnumClassAlias.Create('''+enumname+''','+tostr(i);
          if enumdef.has_jumps then
            str:=str+','+tostr(enumsym.value);
          str:=str+');';
          { alias for $VALUES array used internally by the JDK, and also by FPC
            in case of no jumps }
          str:=str+'__fpc_FVALUES['+tostr(i)+']:='+enumname+';';
          if enumdef.has_jumps then
            str:=str+'__fpc_ord2enum.put(JLInteger.valueOf('+tostr(enumsym.value)+'),'+enumname+');';
        end;
      orderedenums.free;
      str:=str+' end;';
      str_parse_method_impl(str,pd,true);
    end;


  procedure implement_jvm_enum_long2set(pd: tprocdef);
    begin
      str_parse_method_impl(
        'var '+
          'i, setval: jint;'+
        'begin '+
          'result:=JUEnumSet.noneOf(JLClass(__FPC_TEnumClassAlias));'+
          'if __val<>0 then '+
            'begin '+
              '__setsize:=__setsize*8;'+
              'for i:=0 to __setsize-1 do '+
              // setsize-i because JVM = big endian
              'if (__val and (jlong(1) shl (__setsize-i)))<>0 then '+
                'result.add(fpcValueOf(i+__setbase));'+
            'end '+
          'end;',
        pd,true);
    end;


  procedure implement_jvm_enum_bitset2set(pd: tprocdef);
    begin
      str_parse_method_impl(
        'var '+
          'i, setval: jint;'+
        'begin '+
          'result:=JUEnumSet.noneOf(JLClass(__FPC_TEnumClassAlias));'+
          'i:=__val.nextSetBit(0);'+
          'while i>=0 do '+
            'begin '+
              'setval:=-__fromsetbase;'+
              'result.add(fpcValueOf(setval+__tosetbase));'+
              'i:=__val.nextSetBit(i+1);'+
            'end '+
          'end;',
        pd,true);
    end;


  procedure implement_jvm_enum_set2set(pd: tprocdef);
    begin
      str_parse_method_impl(
        'var '+
          'it: JUIterator;'+
          'ele: FpcEnumValueObtainable;'+
          'i: longint;'+
        'begin '+
          'result:=JUEnumSet.noneOf(JLClass(__FPC_TEnumClassAlias));'+
          'it:=__val.iterator;'+
          'while it.hasNext do '+
            'begin '+
              'ele:=FpcEnumValueObtainable(it.next);'+
              'i:=ele.fpcOrdinal-__fromsetbase;'+
              'result.add(fpcValueOf(i+__tosetbase));'+
            'end '+
          'end;',
        pd,true);
    end;


  procedure implement_jvm_procvar_invoke(pd: tprocdef);
    var
      pvclass: tobjectdef;
      procvar: tprocvardef;
      paraname,str,endstr: ansistring;
      pvs: tparavarsym;
      paradef,boxdef,boxargdef: tdef;
      i: longint;
      firstpara: boolean;
    begin
      pvclass:=tobjectdef(pd.owner.defowner);
      procvar:=tprocvardef(ttypesym(search_struct_member(pvclass,'__FPC_PROCVARALIAS')).typedef);
      { the procvar wrapper class has a tmethod member called "method", whose
        "code" field is a JLRMethod, and whose "data" field is the self pointer
        if any (if none is required, it's ignored by the JVM, so there's no
        problem with always passing it) }

      { force extended syntax to allow calling invokeObjectFunc() without using
        its result }
      str:='';
      endstr:='';
      { create local pointer to result type for typecasting in case of an
        implicit pointer type }
      if jvmimplicitpointertype(procvar.returndef) then
         str:=str+'type __FPC_returnptrtype = ^'+procvar.returndef.typename+';';
      str:=str+'begin ';
      { result handling (skip for generic definitions, we'll generate a new
        version for the specialized definition) ) }
      if not is_void(procvar.returndef) and
         (procvar.returndef.typ<>undefineddef) then
        begin
          str:=str+'invoke:=';
          if procvar.returndef.typ in [orddef,floatdef] then
            begin
              { primitivetype(boxtype(..).unboxmethod) }
              jvmgetboxtype(procvar.returndef,boxdef,boxargdef,false);
              str:=str+procvar.returndef.typename+'('+boxdef.typename+'(';
              endstr:=').'+jvmgetunboxmethod(procvar.returndef)+')';
            end
          else if jvmimplicitpointertype(procvar.returndef) then
            begin
              str:=str+'__FPC_returnptrtype(';
              { dereference }
              endstr:=')^';
            end
          else
            begin
              str:=str+procvar.returndef.typename+'(';
              endstr:=')';
            end;
        end;
      str:=str+'invokeObjectFunc([';
      { parameters are a constant array of jlobject }
      firstpara:=true;
      for i:=0 to procvar.paras.count-1 do
        begin
          { skip self/vmt/parentfp, passed separately }
          pvs:=tparavarsym(procvar.paras[i]);
          if ([vo_is_self,vo_is_vmt,vo_is_parentfp]*pvs.varoptions)<>[] then
            continue;
          if not firstpara then
            str:=str+',';
          firstpara:=false;
          paraname:=pvs.realname;
          paradef:=pvs.vardef;
          { Pascalize hidden high parameter }
          if vo_is_high_para in pvs.varoptions then
            paraname:='high('+tparavarsym(procvar.paras[i-1]).realname+')'
          else if vo_is_hidden_para in pvs.varoptions then
            begin
              if ([vo_is_range_check,vo_is_overflow_check]*pvs.varoptions)<>[] then
                { ok, simple boolean parameters }
              else
                internalerror(2011072403);
            end;
          { var/out/constref parameters -> pass address through (same for
            implicit pointer types) }
          if paramanager.push_copyout_param(pvs.varspez,paradef,procvar.proccalloption) or
             jvmimplicitpointertype(paradef) then
            begin
              paraname:='@'+paraname;
              paradef:=java_jlobject;
            end;
          if paradef.typ in [orddef,floatdef] then
            begin
              { box primitive types; use valueOf() rather than create because it
                can give better performance }
              jvmgetboxtype(paradef,boxdef,boxargdef,false);
              str:=str+boxdef.typename+'.valueOf('+boxargdef.typename+'('+paraname+'))'
            end
          else
            str:=str+'JLObject('+paraname+')';
        end;
      str:=str+'])'+endstr+' end;';
      str_parse_method_impl(str,pd,false)
    end;


  procedure implement_jvm_procvar_intconstr(pd: tprocdef);
    var
      pvdef: tprocvardef;
    begin
      { ideal, and most performant, would be to keep the interface instance
        passed to the constructor around and always call its method directly
        rather than working via reflection. Unfortunately, the procvar semantics
        that allow directly modifying the procvar via typecasting it to a
        tmethod make this very hard.

        So for now we simply take the address of the interface instance's
        method and assign it to the tmethod of this procvar }

      pvdef:=tprocvardef(pd.skpara);
      str_parse_method_impl('begin method:=System.TMethod(@__intf.'+pvdef.typesym.RealName+'Callback) end;',pd,false);
    end;


  procedure implement_jvm_virtual_clmethod(pd: tprocdef);
    var
      str: ansistring;
      callpd: tprocdef;
    begin
      callpd:=tprocdef(pd.skpara);
      str:='var pv: __fpc_virtualclassmethod_pv_t'+tostr(pd.defid)+'; begin '
        + 'pv:=@'+callpd.procsym.RealName+';';
      if (pd.proctypeoption<>potype_constructor) and
         not is_void(pd.returndef) then
        str:=str+'result:=';
      str:=str+'pv(';
      addvisibibleparameters(str,pd);
      str:=str+') end;';
      str_parse_method_impl(str,pd,true)
    end;
{$endif jvm}

  procedure implement_field_getter(pd: tprocdef);
    var
      i: longint;
      pvs: tparavarsym;
      str: ansistring;
      callthroughprop: tpropertysym;
      propaccesslist: tpropaccesslist;
      lastparanr: longint;
      firstpara: boolean;
    begin
      callthroughprop:=tpropertysym(pd.skpara);
      str:='begin result:='+callthroughprop.realname;
      if ppo_hasparameters in callthroughprop.propoptions then
        begin
          if not callthroughprop.getpropaccesslist(palt_read,propaccesslist) then
            internalerror(2012100701);
          str:=str+'[';
          firstpara:=true;
          lastparanr:=tprocdef(propaccesslist.procdef).paras.count-1;
          if ppo_indexed in callthroughprop.propoptions then
            dec(lastparanr);
          for i:=0 to lastparanr do
            begin
              { skip self/vmt/parentfp, passed implicitly }
              pvs:=tparavarsym(tprocdef(propaccesslist.procdef).paras[i]);
              if ([vo_is_self,vo_is_vmt,vo_is_parentfp]*pvs.varoptions)<>[] then
                continue;
              if not firstpara then
                str:=str+',';
              firstpara:=false;
              str:=str+pvs.realname;
            end;
          str:=str+']';
        end;
      str:=str+'; end;';
      str_parse_method_impl(str,pd,po_classmethod in pd.procoptions)
    end;


  procedure implement_field_setter(pd: tprocdef);
    var
      i, lastparaindex: longint;
      pvs: tparavarsym;
      paraname,  str: ansistring;
      callthroughprop: tpropertysym;
      propaccesslist: tpropaccesslist;
      firstpara: boolean;
    begin
      callthroughprop:=tpropertysym(pd.skpara);
      str:='begin '+callthroughprop.realname;
      if not callthroughprop.getpropaccesslist(palt_write,propaccesslist) then
        internalerror(2012100702);
      if ppo_hasparameters in callthroughprop.propoptions then
        begin
          str:=str+'[';
          firstpara:=true;
          { last parameter is the value to be set, skip (only add index
            parameters here) }
          lastparaindex:=tprocdef(propaccesslist.procdef).paras.count-2;
          if ppo_indexed in callthroughprop.propoptions then
            dec(lastparaindex);
          for i:=0 to lastparaindex do
            begin
              { skip self/vmt/parentfp/index, passed implicitly }
              pvs:=tparavarsym(tprocdef(propaccesslist.procdef).paras[i]);
              if ([vo_is_self,vo_is_vmt,vo_is_parentfp]*pvs.varoptions)<>[] then
                continue;
              if not firstpara then
                str:=str+',';
              firstpara:=false;
              str:=str+pvs.realname;
            end;
          str:=str+']';
        end;
      { the value-to-be-set }
      if assigned(propaccesslist.procdef) then
        begin
          pvs:=tparavarsym(tprocdef(propaccesslist.procdef).paras[tprocdef(propaccesslist.procdef).paras.count-1]);
          paraname:=pvs.realname;
        end
      else
        paraname:='__fpc_newval__';
      str:=str+':='+paraname+'; end;';
      str_parse_method_impl(str,pd,po_classmethod in pd.procoptions)
    end;


  procedure implement_block_invoke_procvar(pd: tprocdef);
    var
      str: ansistring;
    begin
      str:='';
      str:='begin ';
      if pd.returndef<>voidtype then
        str:=str+'result:=';
      str:=str+'__FPC_BLOCK_INVOKE_PV_TYPE(PFPC_Block_literal_complex_procvar(FPC_Block_Self)^.pv)(';
      addvisibibleparameters(str,pd);
      str:=str+') end;';
      str_parse_method_impl(str,pd,false);
    end;

  procedure add_synthetic_method_implementations_for_st(st: tsymtable);
    var
      i   : longint;
      def : tdef;
      pd  : tprocdef;
    begin
      for i:=0 to st.deflist.count-1 do
        begin
          def:=tdef(st.deflist[i]);
          if (def.typ<>procdef) then
            continue;
          { skip methods when processing unit symtable }
          if def.owner<>st then
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
            tsk_record_initialize:
              implement_record_initialize(pd);
            tsk_empty,
            { special handling for this one is done in tnodeutils.wrap_proc_body }
            tsk_tcinit:
              implement_empty(pd);
            tsk_callthrough:
              implement_callthrough(pd);
            tsk_callthrough_nonabstract:
              begin
                if (pd.owner.defowner.typ<>objectdef) or
                   (tobjectdef(pd.owner.defowner).abstractcnt=0) then
                  implement_callthrough(pd)
                else
                  implement_empty(pd);
              end;
{$ifdef jvm}
            tsk_jvm_enum_values:
              implement_jvm_enum_values(pd);
            tsk_jvm_enum_valueof:
              implement_jvm_enum_valuof(pd);
            tsk_jvm_enum_classconstr:
              implement_jvm_enum_classconstr(pd);
            tsk_jvm_enum_jumps_constr:
              implement_jvm_enum_jumps_constr(pd);
            tsk_jvm_enum_fpcordinal:
              implement_jvm_enum_fpcordinal(pd);
            tsk_jvm_enum_fpcvalueof:
              implement_jvm_enum_fpcvalueof(pd);
            tsk_jvm_enum_long2set:
              implement_jvm_enum_long2set(pd);
            tsk_jvm_enum_bitset2set:
              implement_jvm_enum_bitset2set(pd);
            tsk_jvm_enum_set2set:
              implement_jvm_enum_set2set(pd);
            tsk_jvm_procvar_invoke:
              implement_jvm_procvar_invoke(pd);
            tsk_jvm_procvar_intconstr:
              implement_jvm_procvar_intconstr(pd);
            tsk_jvm_virtual_clmethod:
              implement_jvm_virtual_clmethod(pd);
{$endif jvm}
            tsk_field_getter:
              implement_field_getter(pd);
            tsk_field_setter:
              implement_field_setter(pd);
            tsk_block_invoke_procvar:
              implement_block_invoke_procvar(pd);
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
      { skip if any errors have occurred, since then this can only cause more
        errors }
      if ErrorCount<>0 then
        exit;
      replace_scanner('synthetic_impl',sstate);
      add_synthetic_method_implementations_for_st(st);
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
      { add generic flag if required }
      if assigned(newstruct) and
         (df_generic in newstruct.defoptions) then
        include(pd.defoptions,df_generic);
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
          ps:=cprocsym.create(realname);
          newparentst.insert(ps);
        end;
      pd.procsym:=ps;
      pd.struct:=newstruct;
      { in case of methods, replace the special parameter types with new ones }
      if assigned(newstruct) then
        begin
          symtablestack.push(pd.parast);
          { may not be assigned in case we converted a procvar into a procdef }
          if assigned(pd.paras) then
            begin
              for i:=0 to pd.paras.count-1 do
                begin
                  parasym:=tparavarsym(pd.paras[i]);
                  if vo_is_self in parasym.varoptions then
                    begin
                      if parasym.vardef.typ=classrefdef then
                        parasym.vardef:=cclassrefdef.create(newstruct)
                      else
                        parasym.vardef:=newstruct;
                    end
                end;
            end;
          { also fix returndef in case of a constructor }
          if pd.proctypeoption=potype_constructor then
            pd.returndef:=newstruct;
          symtablestack.pop(pd.parast);
        end;
      pd.calcparas;
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
        accessed from within nested routines (start with extra dollar to prevent
        the JVM from thinking this is a nested class in the unit) }
      nestedvarsst:=trecordsymtable.create('$'+current_module.realmodulename^+'$$_fpc_nestedvars$'+tostr(pd.defid),current_settings.alignment.localalignmax);
      nestedvarsdef:=crecorddef.create(nestedvarsst.name^,nestedvarsst);
{$ifdef jvm}
      maybe_guarantee_record_typesym(nestedvarsdef,nestedvarsdef.owner);
      { don't add clone/FpcDeepCopy, because the field names are not all
        representable in source form and we don't need them anyway }
      symtablestack.push(trecorddef(nestedvarsdef).symtable);
      maybe_add_public_default_java_constructor(trecorddef(nestedvarsdef));
      insert_record_hidden_paras(trecorddef(nestedvarsdef));
      symtablestack.pop(trecorddef(nestedvarsdef).symtable);
{$endif}
      symtablestack.free;
      symtablestack:=old_symtablestack.getcopyuntil(pd.localst);
      pnestedvarsdef:=getpointerdef(nestedvarsdef);
      nestedvars:=clocalvarsym.create('$nestedvars',vs_var,nestedvarsdef,[]);
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
            fieldvardef:=getpointerdef(vardef)
          else
            fieldvardef:=vardef;
          result:=cfieldvarsym.create(sym.realname,vs_value,fieldvardef,[]);
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
          aliassym:=cabsolutevarsym.create_ref(lsym.name,tfieldvarsym(fsym).vardef,sl);
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


  procedure maybe_guarantee_record_typesym(var def: tdef; st: tsymtable);
    var
      ts: ttypesym;
    begin
      { create a dummy typesym for the JVM target, because the record
        has to be wrapped by a class }
      if (target_info.system in systems_jvm) and
         (def.typ=recorddef) and
         not assigned(def.typesym) then
        begin
          ts:=ctypesym.create(trecorddef(def).symtable.realname^,def);
          st.insert(ts);
          ts.visibility:=vis_strictprivate;
          { this typesym can't be used by any Pascal code, so make sure we don't
            print a hint about it being unused }
          addsymref(ts);
        end;
    end;


  function make_field_static(recst: tsymtable; fieldvs: tfieldvarsym): tstaticvarsym;
    var
      static_name: string;
      hstaticvs: tstaticvarsym;
      tmp: tabsolutevarsym;
      sl: tpropaccesslist;
    begin
      include(fieldvs.symoptions,sp_static);
      { generate the symbol which reserves the space }
      static_name:=lower(generate_nested_name(recst,'_'))+'_'+fieldvs.name;
      hstaticvs:=cstaticvarsym.create_from_fieldvar(static_name,fieldvs);
{$ifdef jvm}
      { for the JVM, static field accesses are name-based and
        hence we have to keep the original name of the field.
        Create a staticvarsym instead of a fieldvarsym so we can
        nevertheless use a loadn instead of a subscriptn though,
        since a subscriptn requires something to subscript and
        there is nothing in this case (class+field name will be
        encoded in the mangled symbol name) }
      recst.insert(hstaticvs);
      { only set the staticvarsym's basename (= field name, without any
        mangling), because generating the fully mangled name right now can
        result in a wrong string in case the field's type is a forward
        declared class whose external name will change when the actual
        definition is parsed }
      if (vo_has_mangledname in fieldvs.varoptions) then
        hstaticvs.set_mangledbasename(fieldvs.externalname^)
      else
        hstaticvs.set_mangledbasename(fieldvs.realname);
      { for definition in class file }
      hstaticvs.visibility:=fieldvs.visibility;
{$else jvm}
      include(hstaticvs.symoptions,sp_internal);
      tabstractrecordsymtable(recst).get_unit_symtable.insert(hstaticvs);
{$endif jvm}
      { generate the symbol for the access }
      sl:=tpropaccesslist.create;
      sl.addsym(sl_load,hstaticvs);
      { do *not* change the visibility of this absolutevarsym from vis_public
        to anything else, because its visibility is used by visibility checks
        after turning a class property referring to a class variable into a
        load node (handle_staticfield_access -> searchsym_in_class ->
        is_visible_for_object), which means that the load will fail if this
        symbol is e.g. "strict private" while the property is public }
      tmp:=cabsolutevarsym.create_ref('$'+static_name,fieldvs.vardef,sl);
      recst.insert(tmp);
      result:=hstaticvs;
    end;

end.

