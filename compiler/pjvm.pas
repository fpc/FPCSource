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
     symtype,symbase,symdef;

    { the JVM specs require that you add a default parameterless
      constructor in case the programmer hasn't specified any }
    procedure maybe_add_public_default_java_constructor(obj: tabstractrecorddef);

    { records are emulated via Java classes. They require a default constructor
      to initialise temps, a deep copy helper for assignments, and clone()
      to initialse dynamic arrays }
    procedure add_java_default_record_methods_intf(def: trecorddef);

    procedure jvm_guarantee_record_typesym(var def: tdef; st: tsymtable);


implementation

  uses
    globtype,
    cutils,cclasses,
    verbose,systems,
    fmodule,
    parabase,
    pdecsub,
    symtable,symconst,symsym,symcreat,defcmp,jvmdef,
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


    procedure jvm_guarantee_record_typesym(var def: tdef; st: tsymtable);
      var
        ts: ttypesym;
      begin
        { create a dummy typesym for the JVM target, because the record
          has to be wrapped by a class }
        if (target_info.system=system_jvm_java32) and
           (def.typ=recorddef) and
           not assigned(def.typesym) then
          begin
            ts:=ttypesym.create(trecorddef(def).symtable.realname^,def);
            st.insert(ts);
            ts.visibility:=vis_strictprivate;
          end;
      end;

end.
