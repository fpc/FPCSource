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
    procedure jvm_create_procvar_class(const name: TIDString; def: tdef);

    procedure jvm_wrap_virtual_class_methods(obj: tobjectdef);

    function jvm_add_typed_const_initializer(csym: tconstsym): tstaticvarsym;

    function jvm_wrap_method_with_vis(pd: tprocdef; vis: tvisibility): tprocdef;

    { when a private/protected field is exposed via a property with a higher
      visibility, then we have to create a getter and/or setter with that same
      higher visibility to make sure that using the property does not result
      in JVM verification errors }
    procedure jvm_create_getter_for_property(p: tpropertysym; orgaccesspd: tprocdef);
    procedure jvm_create_setter_for_property(p: tpropertysym; orgaccesspd: tprocdef);


implementation

  uses
    cutils,cclasses,
    verbose,globals,systems,
    fmodule,
    parabase,aasmdata,
    pdecsub,ngenutil,pparautl,
    symtable,symcreat,defcmp,jvmdef,symcpu,nobj,
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
        sstate: tscannerstate;
        needclassconstructor: boolean;
      begin
        ps:=nil;
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
                { childof may not be assigned in case of a parser error }
                if not assigned(tobjectdef(obj).childof) then
                  exit;
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
                ps:=cprocsym.create('Create');
                obj.symtable.insert(ps);
              end;
            { determine symtable level }
            topowner:=obj;
            while not(topowner.owner.symtabletype in [staticsymtable,globalsymtable]) do
              topowner:=topowner.owner.defowner;
            { create procdef }
            pd:=cprocdef.create(topowner.owner.symtablelevel+1);
            if df_generic in obj.defoptions then
              include(pd.defoptions,df_generic);
            { method of this objectdef }
            pd.struct:=obj;
            { associated procsym }
            pd.procsym:=ps;
            { constructor }
            pd.proctypeoption:=potype_constructor;
            { needs to be exported }
            include(pd.procoptions,po_global);
            { by default do not include this routine when looking for overloads }
            include(pd.procoptions,po_ignore_for_overload_resolution);
            { generate anonymous inherited call in the implementation }
            pd.synthetickind:=tsk_anon_inherited;
            { public }
            pd.visibility:=vis_public;
            { result type }
            pd.returndef:=obj;
            { calling convention, self, ... (not for advanced records, for those
              this is handled later) }
            if obj.typ=recorddef then
              handle_calling_convention(pd,[hcc_check])
            else
              handle_calling_convention(pd,hcc_all);
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
        sym: tsym;
        i: longint;
      begin
        maybe_add_public_default_java_constructor(def);
        replace_scanner('record_jvm_helpers',sstate);
        { no override, because not supported in records. Only required in case
          some of the fields require deep copies (otherwise the default
          shallow clone is fine) }
        for i:=0 to def.symtable.symlist.count-1 do
          begin
            sym:=tsym(def.symtable.symlist[i]);
            if (sym.typ=fieldvarsym) and
               jvmimplicitpointertype(tfieldvarsym(sym).vardef) then
              begin
                if str_parse_method_dec('function clone: JLObject;',potype_function,false,def,pd) then
                  pd.synthetickind:=tsk_jvm_clone
                else
                  internalerror(2011032806);
                break;
              end;
          end;
        { can't use def.typesym, not yet set at this point }
        if not assigned(def.symtable.realname) then
          internalerror(2011032803);
        if str_parse_method_dec('procedure fpcDeepCopy(result: FpcBaseRecordType);',potype_procedure,false,def,pd) then
          begin
            pd.synthetickind:=tsk_record_deepcopy;
            { can't add to the declaration since record methods can't override;
              it is in fact an overriding method, because all records inherit
              from a Java base class }
            include(pd.procoptions,po_overridingmethod);
          end
        else
          internalerror(2011032807);
        if def.needs_inittable then
          begin
            { 'var' instead of 'out' parameter, because 'out' would trigger
               calling the initialize method recursively }
            if str_parse_method_dec('procedure fpcInitializeRec;',potype_procedure,false,def,pd) then
              pd.synthetickind:=tsk_record_initialize
            else
              internalerror(2011071711);
          end;
        restore_scanner(sstate);
      end;


    procedure setup_for_new_class(const scannername: string; out sstate: tscannerstate; out islocal: boolean; out oldsymtablestack: TSymtablestack);
      begin
        replace_scanner(scannername,sstate);
        oldsymtablestack:=symtablestack;
        islocal:=symtablestack.top.symtablelevel>=normal_function_level;
        if islocal then
          begin
            { we cannot add a class local to a procedure -> insert it in the
              static symtable. This is not ideal because this means that it will
              be saved to the ppu file for no good reason, and loaded again
              even though it contains a reference to a type that was never
              saved to the ppu file (the locally defined enum type). Since this
              alias for the locally defined enumtype is only used while
              implementing the class' methods, this is however no problem. }
            symtablestack:=symtablestack.getcopyuntil(current_module.localsymtable);
          end;
      end;


    procedure restore_after_new_class(const sstate: tscannerstate; const islocal: boolean; const oldsymtablestack: TSymtablestack);
      begin
        if islocal then
          begin
            symtablestack.free;
            symtablestack:=oldsymtablestack;
          end;
        restore_scanner(sstate);
      end;


    procedure jvm_maybe_create_enum_class(const name: TIDString; def: tdef);
      var
        vmtbuilder: tvmtbuilder;
        arrdef: tarraydef;
        arrsym: ttypesym;
        juhashmap: tdef;
        enumclass: tobjectdef;
        pd: tprocdef;
        old_current_structdef: tabstractrecorddef;
        i: longint;
        sym,
        aliassym: tstaticvarsym;
        fsym: tfieldvarsym;
        sstate: tscannerstate;
        sl: tpropaccesslist;
        temptypesym: ttypesym;
        oldsymtablestack: tsymtablestack;
        islocal: boolean;
      begin
        { if it's a subrange type, don't create a new class }
        if assigned(tenumdef(def).basedef) then
          exit;

        setup_for_new_class('jvm_enum_class',sstate,islocal,oldsymtablestack);

        { create new class (different internal name than enum to prevent name
          clash; at unit level because we don't want its methods to be nested
          inside a function in case its a local type) }
        enumclass:=cobjectdef.create(odt_javaclass,'$'+current_module.realmodulename^+'$'+name+'$InternEnum$'+tostr(def.defid),java_jlenum);
        tenumdef(def).classdef:=enumclass;
        include(enumclass.objectoptions,oo_is_enum_class);
        include(enumclass.objectoptions,oo_is_sealed);
        { implement FpcEnumValueObtainable interface }
        enumclass.ImplementedInterfaces.add(TImplementedInterface.Create(tobjectdef(search_system_type('FPCENUMVALUEOBTAINABLE').typedef)));
        { create an alias for this type inside itself: this way we can choose a
          name that can be used in generated Pascal code without risking an
          identifier conflict (since it is local to this class; the global name
          is unique because it's an identifier that contains $-signs) }
        enumclass.symtable.insert(ctypesym.create('__FPC_TEnumClassAlias',enumclass));

        { also create an alias for the enum type so that we can iterate over
          all enum values when creating the body of the class constructor }
        temptypesym:=ctypesym.create('__FPC_TEnumAlias',nil);
        { don't pass def to the ttypesym constructor, because then it
          will replace the current (real) typesym of that def with the alias }
        temptypesym.typedef:=def;
        enumclass.symtable.insert(temptypesym);
        { but the name of the class as far as the JVM is concerned will match
          the enum's original name (the enum type itself won't be output in
          any class file, so no conflict there)

          name can be empty in case of declaration such as "set of (ea,eb)"  }
        if not islocal and
           (name <> '')  then
          enumclass.objextname:=stringdup(name)
        else
          { for local types, use a unique name to prevent conflicts (since such
            types are not visible outside the routine anyway, it doesn't matter
          }
          begin
            enumclass.objextname:=stringdup(enumclass.objrealname^);
            { also mark it as private (not strict private, because the class
              is not a subclass of the unit in which it is declared, so then
              the unit's procedures would not be able to use it) }
            enumclass.typesym.visibility:=vis_private;
          end;
        { now add a bunch of extra things to the enum class }
        old_current_structdef:=current_structdef;
        current_structdef:=enumclass;

        symtablestack.push(enumclass.symtable);
        { create static fields representing all enums }
        for i:=0 to tenumdef(def).symtable.symlist.count-1 do
          begin
            fsym:=cfieldvarsym.create(tenumsym(tenumdef(def).symtable.symlist[i]).realname,vs_final,enumclass,[]);
            enumclass.symtable.insert(fsym);
            sym:=make_field_static(enumclass.symtable,fsym);
            { add alias for the field representing ordinal(0), for use in
              initialization code }
            if tenumsym(tenumdef(def).symtable.symlist[i]).value=0 then
              begin
                aliassym:=cstaticvarsym.create('__FPC_Zero_Initializer',vs_final,enumclass,[vo_is_external]);
                enumclass.symtable.insert(aliassym);
                aliassym.set_raw_mangledname(sym.mangledname);
              end;
          end;
        { create local "array of enumtype" type for the "values" functionality
          (used internally by the JDK) }
        arrdef:=carraydef.create(0,tenumdef(def).symtable.symlist.count-1,s32inttype);
        arrdef.elementdef:=enumclass;
        arrsym:=ctypesym.create('__FPC_TEnumValues',arrdef);
        enumclass.symtable.insert(arrsym);
        { insert "public static values: array of enumclass" that returns $VALUES.clone()
          (rather than a dynamic array and using clone --which we don't support yet for arrays--
           simply use a fixed length array and copy it) }
        if not str_parse_method_dec('function values: __FPC_TEnumValues;static;',potype_function,true,enumclass,pd) then
          internalerror(2011062301);
        include(pd.procoptions,po_staticmethod);
        pd.synthetickind:=tsk_jvm_enum_values;
        { do we have to store the ordinal value separately? (if no jumps, we can
          just call the default ordinal() java.lang.Enum function) }
        if tenumdef(def).has_jumps then
          begin
            { add field for the value }
            fsym:=cfieldvarsym.create('__fpc_fenumval',vs_final,s32inttype,[]);
            enumclass.symtable.insert(fsym);
            tobjectsymtable(enumclass.symtable).addfield(fsym,vis_strictprivate);
            { add class field with hash table that maps from FPC-declared ordinal value -> enum instance }
            juhashmap:=search_system_type('JUHASHMAP').typedef;
            fsym:=cfieldvarsym.create('__fpc_ord2enum',vs_final,juhashmap,[]);
            enumclass.symtable.insert(fsym);
            make_field_static(enumclass.symtable,fsym);
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
            add_missing_parent_constructors_intf(enumclass,false,vis_strictprivate);
          end;
        { add instance method to get the enum's value as declared in FPC }
        if not str_parse_method_dec('function FPCOrdinal: longint;',potype_function,false,enumclass,pd) then
          internalerror(2011062402);
        pd.synthetickind:=tsk_jvm_enum_fpcordinal;
        { add static class method to convert an ordinal to the corresponding enum }
        if not str_parse_method_dec('function FPCValueOf(__fpc_int: longint): __FPC_TEnumClassAlias; static;',potype_function,true,enumclass,pd) then
          internalerror(2011062402);
        pd.synthetickind:=tsk_jvm_enum_fpcvalueof;
        { similar (instance) function for use in set factories; implements FpcEnumValueObtainable interface }
        if not str_parse_method_dec('function fpcGenericValueOf(__fpc_int: longint): JLEnum;',potype_function,false,enumclass,pd) then
          internalerror(2011062402);
        pd.synthetickind:=tsk_jvm_enum_fpcvalueof;

        { insert "public static valueOf(string): tenumclass" that returns tenumclass(inherited valueOf(tenumclass,string)) }
        if not str_parse_method_dec('function valueOf(const __fpc_str: JLString): __FPC_TEnumClassAlias; static;',potype_function,true,enumclass,pd) then
          internalerror(2011062302);
        include(pd.procoptions,po_staticmethod);
        pd.synthetickind:=tsk_jvm_enum_valueof;

        { add instance method to convert an ordinal and an array into a set of
          (we always need/can use both in case of subrange types and/or array
           -> set type casts) }
        if not str_parse_method_dec('function fpcLongToEnumSet(__val: jlong; __setbase, __setsize: jint): JUEnumSet;',potype_function,true,enumclass,pd) then
          internalerror(2011070501);
        pd.synthetickind:=tsk_jvm_enum_long2set;

        if not str_parse_method_dec('function fpcBitSetToEnumSet(const __val: FpcBitSet; __fromsetbase, __tosetbase: jint): JUEnumSet; static;',potype_function,true,enumclass,pd) then
          internalerror(2011071004);
        pd.synthetickind:=tsk_jvm_enum_bitset2set;

        if not str_parse_method_dec('function fpcEnumSetToEnumSet(const __val: JUEnumSet; __fromsetbase, __tosetbase: jint): JUEnumSet; static;',potype_function,true,enumclass,pd) then
          internalerror(2011071005);
        pd.synthetickind:=tsk_jvm_enum_set2set;

        { create array called "$VALUES" that will contain a reference to all
          enum instances (JDK convention)
          Disable duplicate identifier checking when inserting, because it will
          check for a conflict with "VALUES" ($<id> normally means "check for
          <id> without uppercasing first"), which will conflict with the
          "Values" instance method -- that's also the reason why we insert the
          field only now, because we cannot disable duplicate identifier
          checking when creating the "Values" method }
        fsym:=cfieldvarsym.create('$VALUES',vs_final,arrdef,[]);
        fsym.visibility:=vis_strictprivate;
        enumclass.symtable.insert(fsym,false);
        sym:=make_field_static(enumclass.symtable,fsym);
        { alias for accessing the field in generated Pascal code }
        sl:=tpropaccesslist.create;
        sl.addsym(sl_load,sym);
        enumclass.symtable.insert(cabsolutevarsym.create_ref('__fpc_FVALUES',arrdef,sl));
        { add initialization of the static class fields created above }
        if not str_parse_method_dec('constructor fpc_enum_class_constructor;',potype_class_constructor,true,enumclass,pd) then
          internalerror(2011062303);
        pd.synthetickind:=tsk_jvm_enum_classconstr;

        symtablestack.pop(enumclass.symtable);

        vmtbuilder:=TVMTBuilder.Create(enumclass);
        vmtbuilder.generate_vmt;
        vmtbuilder.free;

        restore_after_new_class(sstate,islocal,oldsymtablestack);
        current_structdef:=old_current_structdef;
      end;


    procedure jvm_create_procvar_class_intern(const name: TIDString; def: tdef; force_no_callback_intf: boolean);
      var
        vmtbuilder: tvmtbuilder;
        oldsymtablestack: tsymtablestack;
        pvclass,
        pvintf: tobjectdef;
        temptypesym: ttypesym;
        sstate: tscannerstate;
        methoddef: tprocdef;
        old_current_structdef: tabstractrecorddef;
        islocal: boolean;
      begin
        { inlined definition of procvar -> generate name, derive from
          FpcBaseNestedProcVarType, pass nestedfpstruct to constructor and
          copy it }
        if name='' then
          internalerror(2011071901);

        setup_for_new_class('jvm_pvar_class',sstate,islocal,oldsymtablestack);

        { create new class (different internal name than pvar to prevent name
          clash; at unit level because we don't want its methods to be nested
          inside a function in case its a local type) }
        pvclass:=cobjectdef.create(odt_javaclass,'$'+current_module.realmodulename^+'$'+name+'$InternProcvar$'+tostr(def.defid),java_procvarbase);
        tcpuprocvardef(def).classdef:=pvclass;
        include(pvclass.objectoptions,oo_is_sealed);
        if df_generic in def.defoptions then
          include(pvclass.defoptions,df_generic);
        { associate typesym }
        pvclass.symtable.insert(ctypesym.create('__FPC_TProcVarClassAlias',pvclass));
        { set external name to match procvar type name }
        if not islocal then
          pvclass.objextname:=stringdup(name)
        else
          pvclass.objextname:=stringdup(pvclass.objrealname^);

        symtablestack.push(pvclass.symtable);

        { inherit constructor and keep public }
        add_missing_parent_constructors_intf(pvclass,true,vis_public);

        { add a method to call the procvar using unwrapped arguments, which
          then wraps them and calls through to JLRMethod.invoke }
        methoddef:=tprocdef(tprocvardef(def).getcopyas(procdef,pc_bareproc));
        finish_copied_procdef(methoddef,'invoke',pvclass.symtable,pvclass);
        insert_self_and_vmt_para(methoddef);
        methoddef.synthetickind:=tsk_jvm_procvar_invoke;
        methoddef.calcparas;

        { add local alias for the procvartype that we can use when implementing
          the invoke method }
        temptypesym:=ctypesym.create('__FPC_ProcVarAlias',nil);
        { don't pass def to the ttypesym constructor, because then it
          will replace the current (real) typesym of that def with the alias }
        temptypesym.typedef:=def;
        pvclass.symtable.insert(temptypesym);

        { in case of a procedure of object, add a nested interface type that
          has one method that conforms to the procvartype (with name
          procvartypename+'Callback') and an extra constructor that takes
          an instance conforming to this interface and which sets up the
          procvar by taking the address of its Callback method (convenient to
          use from Java code) }
        if (po_methodpointer in tprocvardef(def).procoptions) and
           not islocal and
           not force_no_callback_intf then
          begin
            pvintf:=cobjectdef.create(odt_interfacejava,'Callback',nil);
            pvintf.objextname:=stringdup('Callback');
            if df_generic in def.defoptions then
              include(pvintf.defoptions,df_generic);
            { associate typesym }
            pvclass.symtable.insert(ctypesym.create('Callback',pvintf));

            { add a method prototype matching the procvar (like the invoke
              in the procvarclass itself) }
            symtablestack.push(pvintf.symtable);
            methoddef:=tprocdef(tprocvardef(def).getcopyas(procdef,pc_bareproc));
            finish_copied_procdef(methoddef,name+'Callback',pvintf.symtable,pvintf);
            insert_self_and_vmt_para(methoddef);
            { can't be final/static/private/protected, and must be virtual
              since it's an interface method }
            methoddef.procoptions:=methoddef.procoptions-[po_staticmethod,po_finalmethod];
            include(methoddef.procoptions,po_virtualmethod);
            methoddef.visibility:=vis_public;
            symtablestack.pop(pvintf.symtable);

            { add an extra constructor to the procvarclass that takes an
              instance of this interface as parameter }
            old_current_structdef:=current_structdef;
            current_structdef:=pvclass;
            if not str_parse_method_dec('constructor Create(__intf:'+pvintf.objextname^+');overload;',potype_constructor,false,pvclass,methoddef) then
              internalerror(2011092401);
            methoddef.synthetickind:=tsk_jvm_procvar_intconstr;
            methoddef.skpara:=def;
            current_structdef:=old_current_structdef;
          end;

        symtablestack.pop(pvclass.symtable);

        vmtbuilder:=TVMTBuilder.Create(pvclass);
        vmtbuilder.generate_vmt;
        vmtbuilder.free;

        restore_after_new_class(sstate,islocal,oldsymtablestack);
      end;


    procedure jvm_create_procvar_class(const name: TIDString; def: tdef);
      begin
        jvm_create_procvar_class_intern(name,def,false);
      end;


    procedure jvm_wrap_virtual_class_method(pd: tprocdef);
      var
        wrapperpd: tprocdef;
        wrapperpv: tcpuprocvardef;
        typ: ttypesym;
        wrappername: shortstring;
      begin
        if (po_external in pd.procoptions) or
           (oo_is_external in pd.struct.objectoptions) then
          exit;
        { the JVM does not support virtual class methods -> we generate
          wrappers with the original name so they can be called normally,
          and these wrappers will then perform a dynamic lookup. To enable
          calling the class method by its intended name from external Java code,
          we have to change its external name so that we give that original
          name to the wrapper function -> "switch" the external names around for
          the original and wrapper methods }

        { replace importname of original procdef }
        include(pd.procoptions,po_has_importname);
        if not assigned(pd.import_name) then
          wrappername:=pd.procsym.realname
        else
          wrappername:=pd.import_name^;
        stringdispose(pd.import_name);
        pd.import_name:=stringdup(wrappername+'__fpcvirtualclassmethod__');

        { wrapper is part of the same symtable as the original procdef }
        symtablestack.push(pd.owner);
        { get a copy of the virtual class method }
        wrapperpd:=tprocdef(pd.getcopy);
        { this one is not virtual nor override }
        exclude(wrapperpd.procoptions,po_virtualmethod);
        exclude(wrapperpd.procoptions,po_overridingmethod);
        { import/external name = name of original class method }
        stringdispose(wrapperpd.import_name);
        wrapperpd.import_name:=stringdup(wrappername);
        include(wrapperpd.procoptions,po_has_importname);
        { associate with wrapper procsym (Pascal-level name = wrapper name ->
          in callnodes, we will have to replace the calls to virtual class
          methods with calls to the wrappers) }
        finish_copied_procdef(wrapperpd,pd.import_name^,pd.owner,tabstractrecorddef(pd.owner.defowner));

        { we only have to generate the dispatching routine for non-overriding
          methods; the overriding ones can use the original one, but generate
          a skeleton for that anyway because the overriding one may still
          change the visibility (but we can just call the inherited routine
          in that case) }
        if po_overridingmethod in pd.procoptions then
          begin
            { by default do not include this routine when looking for overloads }
            include(wrapperpd.procoptions,po_ignore_for_overload_resolution);
            wrapperpd.synthetickind:=tsk_anon_inherited;
            symtablestack.pop(pd.owner);
            exit;
          end;

        { implementation }
        wrapperpd.synthetickind:=tsk_jvm_virtual_clmethod;
        wrapperpd.skpara:=pd;
        { also create procvar type that we can use in the implementation }
        wrapperpv:=tcpuprocvardef(pd.getcopyas(procvardef,pc_normal));
        wrapperpv.calcparas;
        { no use in creating a callback wrapper here, this procvar type isn't
          for public consumption }
        jvm_create_procvar_class_intern('__fpc_virtualclassmethod_pv_t'+tostr(wrapperpd.defid),wrapperpv,true);
        { create alias for the procvar type so we can use it in generated
          Pascal code }
        typ:=ctypesym.create('__fpc_virtualclassmethod_pv_t'+tostr(wrapperpd.defid),wrapperpv);
        wrapperpv.classdef.typesym.visibility:=vis_strictprivate;
        symtablestack.top.insert(typ);
        symtablestack.pop(pd.owner);
      end;


    procedure jvm_wrap_virtual_constructor(pd: tprocdef);
      var
        wrapperpd: tprocdef;
      begin
        { to avoid having to implement procvar-like support for dynamically
          invoking constructors, call the constructors from virtual class
          methods and replace calls to the constructors with calls to the
          virtual class methods -> we can reuse lots of infrastructure }
        if (po_external in pd.procoptions) or
           (oo_is_external in pd.struct.objectoptions) then
          exit;
        { wrapper is part of the same symtable as the original procdef }
        symtablestack.push(pd.owner);
        { get a copy of the constructor }
        wrapperpd:=tprocdef(pd.getcopyas(procdef,pc_bareproc));
        { this one is a class method rather than a constructor }
        include(wrapperpd.procoptions,po_classmethod);
        wrapperpd.proctypeoption:=potype_function;
        wrapperpd.returndef:=tobjectdef(pd.owner.defowner);

        { import/external name = name of original constructor (since
          constructors don't have names in Java, this won't conflict with the
          original constructor definition) }
        stringdispose(wrapperpd.import_name);
        wrapperpd.import_name:=stringdup(pd.procsym.realname);
        { associate with wrapper procsym (Pascal-level name = wrapper name ->
          in callnodes, we will have to replace the calls to virtual
          constructors with calls to the wrappers) }
        finish_copied_procdef(wrapperpd,pd.procsym.realname+'__fpcvirtconstrwrapper__',pd.owner,tabstractrecorddef(pd.owner.defowner));
        { since it was a bare copy, insert the self parameter (we can't just
          copy the vmt parameter from the constructor, that's different) }
        insert_self_and_vmt_para(wrapperpd);
        wrapperpd.calcparas;
        { implementation: call through to the constructor
          Exception: if the current class is abstract, do not call the
            constructor, since abstract class cannot be constructed (and the
            Android verifier does not accept such code, even if it is
            unreachable) }
        wrapperpd.synthetickind:=tsk_callthrough_nonabstract;
        wrapperpd.skpara:=pd;
        symtablestack.pop(pd.owner);
        { and now wrap this generated virtual static method itself as well }
        jvm_wrap_virtual_class_method(wrapperpd);
      end;


    procedure jvm_wrap_virtual_class_methods(obj: tobjectdef);
      var
        i: longint;
        def: tdef;
      begin
        { new methods will be inserted while we do this, but since
          symtable.deflist.count is evaluated at the start of the loop that
          doesn't matter }
        for i:=0 to obj.symtable.deflist.count-1 do
          begin
            def:=tdef(obj.symtable.deflist[i]);
            if def.typ<>procdef then
              continue;
            if [po_classmethod,po_virtualmethod]<=tprocdef(def).procoptions then
              jvm_wrap_virtual_class_method(tprocdef(def))
            else if (tprocdef(def).proctypeoption=potype_constructor) and
               (po_virtualmethod in tprocdef(def).procoptions) then
              jvm_wrap_virtual_constructor(tprocdef(def));
          end;
      end;


    function jvm_add_typed_const_initializer(csym: tconstsym): tstaticvarsym;
      var
        ssym: tstaticvarsym;
        esym: tenumsym;
        i: longint;
        sstate: tscannerstate;
        elemdef: tdef;
        elemdefname,
        conststr: ansistring;
        first: boolean;
      begin
        result:=nil;
        esym:=nil;
        case csym.constdef.typ of
          enumdef:
            begin
              { make sure we don't emit a definition for this field (we'll do
                that for the constsym already) -> mark as external }
              ssym:=cstaticvarsym.create(internal_static_field_name(csym.realname),vs_final,csym.constdef,[vo_is_external]);
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
              replace_scanner('jvm_enum_const',sstate);
              str_parse_typedconst(current_asmdata.asmlists[al_typedconsts],esym.name+';',ssym);
              restore_scanner(sstate);
              result:=ssym;
            end;
          setdef:
            begin
              replace_scanner('jvm_set_const',sstate);
              { make sure we don't emit a definition for this field (we'll do
                that for the constsym already) -> mark as external;
                on the other hand, we don't create instances for constsyms in
                (or external syms) the program/unit initialization code -> add
                vo_has_local_copy to indicate that this should be done after all
                (in thlcgjvm.allocate_implicit_structs_for_st_with_base_ref) }

              { the constant can be defined in the body of a function and its
                def can also belong to that -> will be freed when the function
                has been compiler -> insert a copy in the unit's staticsymtable
              }
              symtablestack.push(current_module.localsymtable);
              ssym:=cstaticvarsym.create(internal_static_field_name(csym.realname),vs_final,tsetdef(csym.constdef).getcopy,[vo_is_external,vo_has_local_copy]);
              symtablestack.top.insert(ssym);
              symtablestack.pop(current_module.localsymtable);
              { alias storage to the constsym }
              ssym.set_mangledname(csym.realname);
              { ensure that we allocate space for global symbols (won't actually
                allocate space for this one, since it's external, but for the
                constsym) }
              cnodeutils.insertbssdata(ssym);
              elemdef:=tsetdef(csym.constdef).elementdef;
              if not assigned(elemdef) then
                begin
                  internalerror(2011070502);
                end
              else
                begin
                  elemdefname:=elemdef.typename;
                  conststr:='[';
                  first:=true;
                  for i:=0 to 255 do
                    if i in pnormalset(csym.value.valueptr)^ then
                      begin
                        if not first then
                          conststr:=conststr+',';
                        first:=false;
                        { instead of looking up all enum value names/boolean
                           names, type cast integers to the required type }
                        conststr:=conststr+elemdefname+'('+tostr(i)+')';
                      end;
                  conststr:=conststr+'];';
                end;
              str_parse_typedconst(current_asmdata.asmlists[al_typedconsts],conststr,ssym);
              restore_scanner(sstate);
              result:=ssym;
            end;
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
        symtablestack.push(obj.symtable);
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
        result.procoptions:=result.procoptions*[po_classmethod,po_staticmethod,po_varargs,po_public];
        result.synthetickind:=tsk_callthrough;
        { so we know the name of the routine to call through to }
        result.skpara:=pd;
        symtablestack.pop(obj.symtable);
      end;


    procedure jvm_create_getter_or_setter_for_property(p: tpropertysym; orgaccesspd: tprocdef; getter: boolean);
      var
        obj: tabstractrecorddef;
        ps: tprocsym;
        pvs: tparavarsym;
        sym: tsym;
        pd, parentpd, accessorparapd: tprocdef;
        tmpaccesslist: tpropaccesslist;
        callthroughpropname,
        name: string;
        callthroughprop: tpropertysym;
        accesstyp: tpropaccesslisttypes;
        sktype: tsynthetickind;
        procoptions: tprocoptions;
        paranr: word;
        explicitwrapper: boolean;
      begin
        obj:=current_structdef;
        { if someone gets the idea to add a property to an external class
          definition, don't try to wrap it since we cannot add methods to
          external classes }
        if oo_is_external in obj.objectoptions then
          exit;
        symtablestack.push(obj.symtable);

        try
          if getter then
            accesstyp:=palt_read
          else
            accesstyp:=palt_write;

          { we can't use str_parse_method_dec here because the type of the field
            may not be visible at the Pascal level }

          explicitwrapper:=
            { private methods are not visibile outside the current class, so
              no use in making life harder for us by introducing potential
              (future or current) naming conflicts }
            (p.visibility<>vis_private) and
            (getter and
             (prop_auto_getter_prefix<>'')) or
            (not getter and
             (prop_auto_setter_prefix<>''));
          sym:=nil;
          procoptions:=[];
          if explicitwrapper then
            begin
              if getter then
                name:=prop_auto_getter_prefix+p.realname
              else
                name:=prop_auto_setter_prefix+p.realname;
              sym:=search_struct_member_no_helper(obj,upper(name));
              if getter then
                sktype:=tsk_field_getter
              else
                sktype:=tsk_field_setter;
              if assigned(sym) then
                begin
                  if ((sym.typ<>procsym) or
                      (tprocsym(sym).procdeflist.count<>1) or
                      (tprocdef(tprocsym(sym).procdeflist[0]).synthetickind<>sktype)) and
                     (not assigned(orgaccesspd) or
                      (sym<>orgaccesspd.procsym)) then
                    begin
                      MessagePos2(p.fileinfo,parser_e_cannot_generate_property_getter_setter,name,FullTypeName(tdef(sym.owner.defowner),nil)+'.'+name);
                      exit;
                    end
                  else
                    begin
                      if name<>sym.realname then
                        MessagePos2(p.fileinfo,parser_w_case_difference_auto_property_getter_setter_prefix,sym.realname,name);
                      { is the specified getter/setter defined in the current
                        struct and was it originally specified as the getter/
                        setter for this property? If so, simply adjust its
                        visibility if necessary.
                      }
                      if assigned(orgaccesspd) then
                        parentpd:=orgaccesspd
                      else
                        parentpd:=tprocdef(tprocsym(sym).procdeflist[0]);
                      if parentpd.owner.defowner=p.owner.defowner then
                        begin
                          if parentpd.visibility<p.visibility then
                            begin
                              parentpd.visibility:=p.visibility;
                              include(parentpd.procoptions,po_auto_raised_visibility);
                            end;
                          { we are done, no need to create a wrapper }
                          exit
                        end
                      { a parent already included this getter/setter -> try to
                        override it }
                      else if parentpd.visibility<>vis_private then
                        begin
                          if po_virtualmethod in parentpd.procoptions then
                            begin
                              procoptions:=procoptions+[po_virtualmethod,po_overridingmethod];
                              Message2(parser_w_overriding_property_getter_setter,name,FullTypeName(tdef(parentpd.owner.defowner),nil));
                            end;
                          { otherwise we can't do anything, and
                            proc_add_definition will give an error }
                        end;
                      { add method with the correct visibility }
                      pd:=tprocdef(parentpd.getcopy);
                      { get rid of the import name for inherited virtual class methods,
                        it has to be regenerated rather than amended }
                      if [po_classmethod,po_virtualmethod]<=pd.procoptions then
                        begin
                          stringdispose(pd.import_name);
                          exclude(pd.procoptions,po_has_importname);
                        end;
                      pd.visibility:=p.visibility;
                      pd.procoptions:=pd.procoptions+procoptions;
                      { ignore this artificially added procdef when looking for overloads }
                      include(pd.procoptions,po_ignore_for_overload_resolution);
                      finish_copied_procdef(pd,parentpd.procsym.realname,obj.symtable,obj);
                      exclude(pd.procoptions,po_external);
                      pd.synthetickind:=tsk_anon_inherited;
                      exit;
                    end;
                end;
              { make the artificial getter/setter virtual so we can override it in
                children if necessary }
              if not(sp_static in p.symoptions) and
                 (obj.typ=objectdef) then
                include(procoptions,po_virtualmethod);
              { prevent problems in Delphi mode }
              include(procoptions,po_overload);
            end
          else
            begin
              { construct procsym name (unique for this access; reusing the same
                helper for multiple accesses to the same field is hard because the
                propacesslist can contain subscript nodes etc) }
              name:=visibilityName[p.visibility];
              replace(name,' ','_');
              if getter then
                name:=name+'$getter'
              else
                name:=name+'$setter';
            end;

          { create procdef }
          if not assigned(orgaccesspd) then
            begin
              pd:=cprocdef.create(normal_function_level);
              if df_generic in obj.defoptions then
                include(pd.defoptions,df_generic);
              { method of this objectdef }
              pd.struct:=obj;
              { can only construct the artificial name now, because it requires
                pd.defid }
              if not explicitwrapper then
                name:='$'+obj.symtable.realname^+'$'+p.realname+'$'+name+'$'+tostr(pd.defid);
            end
          else
            begin
              { getter/setter could have parameters in case of indexed access
                -> copy original procdef }
              pd:=tprocdef(orgaccesspd.getcopy);
              exclude(pd.procoptions,po_abstractmethod);
              { can only construct the artificial name now, because it requires
                pd.defid }
              if not explicitwrapper then
                name:='$'+obj.symtable.realname^+'$'+p.realname+'$'+name+'$'+tostr(pd.defid);
              finish_copied_procdef(pd,name,obj.symtable,obj);
              sym:=pd.procsym;
            end;
          { add previously collected procoptions }
          pd.procoptions:=pd.procoptions+procoptions;
          { visibility }
          pd.visibility:=p.visibility;

          { new procsym? }
          if not assigned(sym) or
             (sym.owner<>p.owner)  then
            begin
              ps:=cprocsym.create(name);
              obj.symtable.insert(ps);
            end
          else
            ps:=tprocsym(sym);
          { associate procsym with procdef}
          pd.procsym:=ps;



          { function/procedure }
          accessorparapd:=nil;
          if getter then
            begin
              pd.proctypeoption:=potype_function;
              pd.synthetickind:=tsk_field_getter;
              { result type }
              pd.returndef:=p.propdef;
              if (ppo_hasparameters in p.propoptions) and
                 not assigned(orgaccesspd) then
                accessorparapd:=pd;
            end
          else
            begin
              pd.proctypeoption:=potype_procedure;
              pd.synthetickind:=tsk_field_setter;
              pd.returndef:=voidtype;
              if not assigned(orgaccesspd) then
                begin
                  { parameter with value to set }
                  pvs:=cparavarsym.create('__fpc_newval__',10,vs_const,p.propdef,[]);
                  pd.parast.insert(pvs);
                end;
              if (ppo_hasparameters in p.propoptions) and
                 not assigned(orgaccesspd) then
                accessorparapd:=pd;
            end;

          { create a property for the old symaccesslist with a new name, so that
            we can reuse it in the implementation (rather than having to
            translate the symaccesslist back to Pascal code) }
          callthroughpropname:='__fpc__'+p.realname;
          if getter then
            callthroughpropname:=callthroughpropname+'__getter_wrapper'
          else
            callthroughpropname:=callthroughpropname+'__setter_wrapper';
          callthroughprop:=cpropertysym.create(callthroughpropname);
          callthroughprop.visibility:=p.visibility;

          if getter then
            p.makeduplicate(callthroughprop,accessorparapd,nil,paranr)
          else
            p.makeduplicate(callthroughprop,nil,accessorparapd,paranr);

          callthroughprop.default:=longint($80000000);
          callthroughprop.default:=0;
          callthroughprop.propoptions:=callthroughprop.propoptions-[ppo_stored,ppo_enumerator_current,ppo_overrides,ppo_defaultproperty];
          if sp_static in p.symoptions then
            include(callthroughprop.symoptions, sp_static);
          { copy original property target to callthrough property (and replace
            original one with the new empty list; will be filled in later) }
          tmpaccesslist:=callthroughprop.propaccesslist[accesstyp];
          callthroughprop.propaccesslist[accesstyp]:=p.propaccesslist[accesstyp];
          p.propaccesslist[accesstyp]:=tmpaccesslist;
          p.owner.insert(callthroughprop);

          pd.skpara:=callthroughprop;
          { needs to be exported }
          include(pd.procoptions,po_global);
          { class property -> static class method }
          if sp_static in p.symoptions then
            pd.procoptions:=pd.procoptions+[po_classmethod,po_staticmethod];

          { in case we made a copy of the original accessor, this has all been
            done already }
          if not assigned(orgaccesspd) then
            begin
              { calling convention, self, ... }
              if obj.typ=recorddef then
                handle_calling_convention(pd,[hcc_check])
              else
                handle_calling_convention(pd,hcc_all);
              { register forward declaration with procsym }
              proc_add_definition(pd);
            end;

          { make the property call this new function }
          p.propaccesslist[accesstyp].addsym(sl_call,ps);
          p.propaccesslist[accesstyp].procdef:=pd;
        finally
          symtablestack.pop(obj.symtable);
        end;
      end;


    procedure jvm_create_getter_for_property(p: tpropertysym; orgaccesspd: tprocdef);
      begin
        jvm_create_getter_or_setter_for_property(p,orgaccesspd,true);
      end;


    procedure jvm_create_setter_for_property(p: tpropertysym; orgaccesspd: tprocdef);
      begin
        jvm_create_getter_or_setter_for_property(p,orgaccesspd,false);
      end;

end.
