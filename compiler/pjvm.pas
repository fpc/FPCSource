{
    Copyright (c) 2011 by Jonas Maebe

    This unit implements some JVM parser helper routines.

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

unit pjvm;

interface

    uses
      globtype,
      symconst,symtype,symbase,symdef,symsym;

    { the JVM specs require that you add a default parameterless
      constructor in case the programmer hasn't specified any }
    procedure maybe_add_public_default_java_constructor(obj: tabstractrecorddef);

    { records are emulated via Java classes. They require a default constructor
      to initialise temps, a deep copy helper for assignments, and clone()
      to initialse dynamic arrays }
    procedure add_java_default_record_methods_intf(def: trecorddef);

    procedure jvm_maybe_create_enum_class(const name: TIDString; def: tdef);

    procedure jvm_add_typed_const_initializer(csym: tconstsym);

    function jvm_wrap_method_with_vis(pd: tprocdef; vis: tvisibility): tprocdef;


implementation

  uses
    cutils,cclasses,
    verbose,systems,
    fmodule,
    parabase,aasmdata,
    pdecsub,
    symtable,symcreat,defcmp,jvmdef,
    defutil,paramgr;


    { the JVM specs require that you add a default parameterless
      constructor in case the programmer hasn't specified any }
    procedure maybe_add_public_default_java_constructor(obj: tabstractrecorddef);
      var
        sym: tsym;
        ps: tprocsym;
        pd: tprocdef;
        topowner: tdefentry;
        i: longint;
        sstate: symcreat.tscannerstate;
        needclassconstructor: boolean;
      begin
        { if there is at least one constructor for a class, do nothing (for
           records, we'll always also need a parameterless constructor) }
        if not is_javaclass(obj) or
           not (oo_has_constructor in obj.objectoptions) then
          begin
            { check whether the parent has a parameterless constructor that we can
              call (in case of a class; all records will derive from
              java.lang.Object or a shim on top of that with a parameterless
              constructor) }
            if is_javaclass(obj) then
              begin
                pd:=nil;
                sym:=tsym(tobjectdef(obj).childof.symtable.find('CREATE'));
                if assigned(sym) and
                   (sym.typ=procsym) then
                  pd:=tprocsym(sym).find_bytype_parameterless(potype_constructor);
                if not assigned(pd) then
                  begin
                    Message(sym_e_no_matching_inherited_parameterless_constructor);
                    exit
                  end;
              end;
            { we call all constructors CREATE, because they don't have a name in
              Java and otherwise we can't determine whether multiple overloads
              are created with the same parameters }
            sym:=tsym(obj.symtable.find('CREATE'));
            if assigned(sym) then
              begin
                { does another, non-procsym, symbol already exist with that name? }
                if (sym.typ<>procsym) then
                  begin
                    Message1(sym_e_duplicate_id_create_java_constructor,sym.realname);
                    exit;
                  end;
                ps:=tprocsym(sym);
                { is there already a parameterless function/procedure create? }
                pd:=ps.find_bytype_parameterless(potype_function);
                if not assigned(pd) then
                  pd:=ps.find_bytype_parameterless(potype_procedure);
                if assigned(pd) then
                  begin
                    Message1(sym_e_duplicate_id_create_java_constructor,pd.fullprocname(false));
                    exit;
                  end;
              end;
            if not assigned(sym) then
              begin
                ps:=tprocsym.create('Create');
                obj.symtable.insert(ps);
              end;
            { determine symtable level }
            topowner:=obj;
            while not(topowner.owner.symtabletype in [staticsymtable,globalsymtable]) do
              topowner:=topowner.owner.defowner;
            { create procdef }
            pd:=tprocdef.create(topowner.owner.symtablelevel+1);
            { method of this objectdef }
            pd.struct:=obj;
            { associated procsym }
            pd.procsym:=ps;
            { constructor }
            pd.proctypeoption:=potype_constructor;
            { needs to be exported }
            include(pd.procoptions,po_global);
            { for Delphi mode }
            include(pd.procoptions,po_overload);
            { generate anonymous inherited call in the implementation }
            pd.synthetickind:=tsk_anon_inherited;
            { public }
            pd.visibility:=vis_public;
            { result type }
            pd.returndef:=obj;
            { calling convention, self, ... }
            handle_calling_convention(pd);
            { register forward declaration with procsym }
            proc_add_definition(pd);
          end;

        { also add class constructor if class fields that need wrapping, and
          if none was defined }
        if obj.find_procdef_bytype(potype_class_constructor)=nil then
          begin
            needclassconstructor:=false;
            for i:=0 to obj.symtable.symlist.count-1 do
              begin
                if (tsym(obj.symtable.symlist[i]).typ=staticvarsym) and
                   jvmimplicitpointertype(tstaticvarsym(obj.symtable.symlist[i]).vardef) then
                  begin
                    needclassconstructor:=true;
                    break;
                  end;
              end;
            if needclassconstructor then
              begin
                replace_scanner('custom_class_constructor',sstate);
                if str_parse_method_dec('constructor fpc_jvm_class_constructor;',potype_class_constructor,true,obj,pd) then
                  pd.synthetickind:=tsk_empty
                else
                  internalerror(2011040501);
                restore_scanner(sstate);
              end;
          end;
      end;


    procedure add_java_default_record_methods_intf(def: trecorddef);
      var
        sstate: tscannerstate;
        pd: tprocdef;
      begin
        maybe_add_public_default_java_constructor(def);
        replace_scanner('record_jvm_helpers',sstate);
        { no override, because not supported in records; the parser will still
          accept "inherited" though }
        if str_parse_method_dec('function clone: JLObject;',potype_function,false,def,pd) then
          pd.synthetickind:=tsk_jvm_clone
        else
          internalerror(2011032806);
        { can't use def.typesym, not yet set at this point }
        if not assigned(def.symtable.realname) then
          internalerror(2011032803);
        if str_parse_method_dec('procedure fpcDeepCopy(out result:'+def.symtable.realname^+');',potype_procedure,false,def,pd) then
          pd.synthetickind:=tsk_record_deepcopy
        else
          internalerror(2011032807);
        restore_scanner(sstate);
      end;


    procedure jvm_maybe_create_enum_class(const name: TIDString; def: tdef);
      var
        arrdef: tarraydef;
        arrsym: ttypesym;
        juhashmap: tdef;
        enumclass: tobjectdef;
        pd: tprocdef;
        old_current_structdef: tabstractrecorddef;
        i: longint;
        sym: tstaticvarsym;
        fsym: tfieldvarsym;
        sstate: symcreat.tscannerstate;
        sl: tpropaccesslist;
      begin
        { if it's a subrange type, don't create a new class }
        if assigned(tenumdef(def).basedef) then
          exit;
        replace_scanner('jvm_enum_class',sstate);
        { create new class (different internal name than enum to prevent name clash) }
        enumclass:=tobjectdef.create(odt_javaclass,'$'+name+'$InternEnum',java_jlenum);
        tenumdef(def).classdef:=enumclass;
        include(enumclass.objectoptions,oo_is_enum_class);
        include(enumclass.objectoptions,oo_is_sealed);
        { create an alias for this type inside itself: this way we can choose a
          name that can be used in generated Pascal code without risking an
          identifier conflict (since it is local to this class; the global name
          is unique because it's an identifier that contains $-signs) }
        enumclass.symtable.insert(ttypesym.create('__FPC_TEnumClassAlias',enumclass));
        { also create an alias for the enum type so that we can iterate over
          all enum values when creating the body of the class constructor }
        enumclass.symtable.insert(ttypesym.create('__FPC_TEnumAlias',def));
        { but the name of the class as far as the JVM is concerned will match
          the enum's original name (the enum type itself won't be output in
          any class file, so no conflict there) }
        enumclass.objextname:=stringdup(name);
        { now add a bunch of extra things to the enum class }
        old_current_structdef:=current_structdef;
        current_structdef:=enumclass;
        symtablestack.push(enumclass.symtable);
        { create static fields representing all enums }
        for i:=0 to tenumdef(def).symtable.symlist.count-1 do
          begin
            sym:=tstaticvarsym.create(tenumsym(tenumdef(def).symtable.symlist[i]).realname,vs_final,enumclass,[]);
            enumclass.symtable.insert(sym);
            { alias for consistency with parsed staticvarsyms }
            sl:=tpropaccesslist.create;
            sl.addsym(sl_load,sym);
            enumclass.symtable.insert(tabsolutevarsym.create_ref('$'+internal_static_field_name(sym.name),enumclass,sl));
          end;
        { create local "array of enumtype" type for the "values" functionality
          (used internally by the JDK) }
        arrdef:=tarraydef.create(0,tenumdef(def).symtable.symlist.count-1,s32inttype);
        arrdef.elementdef:=enumclass;
        arrsym:=ttypesym.create('__FPC_TEnumValues',arrdef);
        enumclass.symtable.insert(arrsym);
        { insert "public static values: array of enumclass" that returns $VALUES.clone()
          (rather than a dynamic array and using clone --which we don't support yet for arrays--
           simply use a fixed length array and copy it) }
        if not str_parse_method_dec('function values: __FPC_TEnumValues;',potype_function,true,enumclass,pd) then
          internalerror(2011062301);
        include(pd.procoptions,po_staticmethod);
        pd.synthetickind:=tsk_jvm_enum_values;
        { do we have to store the ordinal value separately? (if no jumps, we can
          just call the default ordinal() java.lang.Enum function) }
        if tenumdef(def).has_jumps then
          begin
            { add field for the value }
            fsym:=tfieldvarsym.create('__fpc_fenumval',vs_final,s32inttype,[]);
            enumclass.symtable.insert(fsym);
            tobjectsymtable(enumclass.symtable).addfield(fsym,vis_strictprivate);
            { add class field with hash table that maps from FPC-declared ordinal value -> enum instance }
            juhashmap:=search_system_type('JUHASHMAP').typedef;
            sym:=tstaticvarsym.create('__fpc_ord2enum',vs_final,juhashmap,[]);
            enumclass.symtable.insert(sym);
            { alias for consistency with parsed staticvarsyms }
            sl:=tpropaccesslist.create;
            sl.addsym(sl_load,sym);
            enumclass.symtable.insert(tabsolutevarsym.create_ref('$'+internal_static_field_name(sym.name),enumclass,sl));
            { add custom constructor }
            if not str_parse_method_dec('constructor Create(const __fpc_name: JLString; const __fpc_ord, __fpc_initenumval: longint);',potype_constructor,false,enumclass,pd) then
              internalerror(2011062401);
            pd.synthetickind:=tsk_jvm_enum_jumps_constr;
            pd.visibility:=vis_strictprivate;
          end
        else
          begin
            { insert "private constructor(string,int,int)" that calls inherited and
              initialises the FPC value field }
            add_missing_parent_constructors_intf(enumclass,vis_strictprivate);
          end;
        { add instance method to get the enum's value as declared in FPC }
        if not str_parse_method_dec('function FPCOrdinal: longint;',potype_function,false,enumclass,pd) then
          internalerror(2011062402);
        pd.synthetickind:=tsk_jvm_enum_fpcordinal;
        { add static class method to convert an ordinal to the corresponding enum }
        if not str_parse_method_dec('function FPCValueOf(__fpc_int: longint): __FPC_TEnumClassAlias; static;',potype_function,true,enumclass,pd) then
          internalerror(2011062402);
        pd.synthetickind:=tsk_jvm_enum_fpcvalueof;

        { insert "public static valueOf(string): tenumclass" that returns tenumclass(inherited valueOf(tenumclass,string)) }
        if not str_parse_method_dec('function valueOf(const __fpc_str: JLString): __FPC_TEnumClassAlias; static;',potype_function,true,enumclass,pd) then
          internalerror(2011062302);
        include(pd.procoptions,po_staticmethod);
        pd.synthetickind:=tsk_jvm_enum_valueof;
        { create array called "$VALUES" that will contain a reference to all
          enum instances (JDK convention)
          Disable duplicate identifier checking when inserting, because it will
          check for a conflict with "VALUES" ($<id> normally means "check for
          <id> without uppercasing first"), which will conflict with the
          "Values" instance method -- that's also the reason why we insert the
          field only now, because we cannot disable duplicate identifier
          checking when creating the "Values" method }
        sym:=tstaticvarsym.create('$VALUES',vs_final,arrdef,[]);
        sym.visibility:=vis_strictprivate;
        enumclass.symtable.insert(sym,false);
        { alias for consistency with parsed staticvarsyms }
        sl:=tpropaccesslist.create;
        sl.addsym(sl_load,sym);
        enumclass.symtable.insert(tabsolutevarsym.create_ref('$'+internal_static_field_name(sym.name),arrdef,sl));
        { alias for accessing the field in generated Pascal code }
        sl:=tpropaccesslist.create;
        sl.addsym(sl_load,sym);
        enumclass.symtable.insert(tabsolutevarsym.create_ref('__fpc_FVALUES',arrdef,sl));
        { add initialization of the static class fields created above }
        if not str_parse_method_dec('constructor fpc_enum_class_constructor;',potype_class_constructor,true,enumclass,pd) then
          internalerror(2011062303);
        pd.synthetickind:=tsk_jvm_enum_classconstr;

        symtablestack.pop(enumclass.symtable);
        current_structdef:=old_current_structdef;
        restore_scanner(sstate);
      end;


    procedure jvm_add_typed_const_initializer(csym: tconstsym);
      var
        ssym: tstaticvarsym;
        esym: tenumsym;
        i: longint;
        sstate: symcreat.tscannerstate;
      begin
        case csym.constdef.typ of
          enumdef:
            begin
              replace_scanner('jvm_enum_const',sstate);
              { make sure we don't emit a definition for this field (we'll do
                that for the constsym already) -> mark as external }
              ssym:=tstaticvarsym.create(internal_static_field_name(csym.realname),vs_final,csym.constdef,[vo_is_external]);
              csym.owner.insert(ssym);
              { alias storage to the constsym }
              ssym.set_mangledname(csym.realname);
              for i:=0 to tenumdef(csym.constdef).symtable.symlist.count-1 do
                begin
                  esym:=tenumsym(tenumdef(csym.constdef).symtable.symlist[i]);
                  if esym.value=csym.value.valueord.svalue then
                    break;
                  esym:=nil;
                end;
              { can happen in case of explicit typecast from integer constant
                to enum type }
              if not assigned(esym) then
                begin
                  MessagePos(csym.fileinfo,parser_e_range_check_error);
                  exit;
                end;
              str_parse_typedconst(current_asmdata.asmlists[al_typedconsts],esym.name+';',ssym);
              restore_scanner(sstate);
            end
          else
            internalerror(2011062701);
        end;
      end;


    function jvm_wrap_method_with_vis(pd: tprocdef; vis: tvisibility): tprocdef;
      var
        obj: tabstractrecorddef;
        visname: string;
      begin
        obj:=current_structdef;
        { if someone gets the idea to add a property to an external class
          definition, don't try to wrap it since we cannot add methods to
          external classes }
        if oo_is_external in obj.objectoptions then
          begin
            result:=pd;
            exit
          end;
        result:=tprocdef(pd.getcopy);
        result.visibility:=vis;
        visname:=visibilityName[vis];
        replace(visname,' ','_');
        { create a name that is unique amongst all units (start with '$unitname$$') and
          unique in this unit (result.defid) }
        finish_copied_procdef(result,'$'+current_module.realmodulename^+'$$'+tostr(result.defid)+pd.procsym.realname+'$'+visname,obj.symtable,obj);
        { in case the referred method is from an external class }
        exclude(result.procoptions,po_external);
        { not virtual/override/abstract/... }
        result.procoptions:=result.procoptions*[po_classmethod,po_staticmethod,po_java,po_varargs,po_public];
        result.synthetickind:=tsk_callthrough;
        { so we know the name of the routine to call through to }
        result.skpara:=pd;
      end;

end.
