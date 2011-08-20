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
     symdef;

    { the JVM specs require that you add a default parameterless
      constructor in case the programmer hasn't specified any }
    procedure maybe_add_public_default_java_constructor(obj: tabstractrecorddef);

    { records are emulated via Java classes. They require a default constructor
      to initialise temps, a deep copy helper for assignments, and clone()
      to initialse dynamic arrays }
    procedure add_java_default_record_methods_intf(def: trecorddef);

implementation

  uses
    globtype,
    cutils,cclasses,
    verbose,systems,
    fmodule,
    parabase,
    pdecsub,
    symbase,symtype,symtable,symconst,symsym,symcreat,defcmp,jvmdef,
    defutil,paramgr;


    { the JVM specs require that you add a default parameterless
      constructor in case the programmer hasn't specified any }
    procedure maybe_add_public_default_java_constructor(obj: tabstractrecorddef);

      function find_parameterless_def(psym: tprocsym): tprocdef;
        var
          paras: tparalist;
        begin
          paras:=tparalist.create;
          result:=psym.find_procdef_bypara_no_rettype(paras,[cpo_ignorehidden,cpo_openequalisexact]);
          paras.free;
        end;

      var
        sym: tsym;
        ps: tprocsym;
        pd: tprocdef;
        topowner: tdefentry;
      begin
        { if there is at least one constructor for a class, do nothing (for
           records, we'll always also need a parameterless constructor) }
        if is_javaclass(obj) and
           (oo_has_constructor in obj.objectoptions) then
          exit;
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
              begin
                pd:=find_parameterless_def(tprocsym(sym));
                { make sure it's a constructor }
                if assigned(pd) and
                   (pd.proctypeoption<>potype_constructor) then
                  pd:=nil;
              end;
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
            pd:=find_parameterless_def(ps);
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
        while not(topowner.owner.symtabletype in [staticsymtable,globalsymtable,localsymtable]) do
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


    procedure add_java_default_record_methods_intf(def: trecorddef);
      var
        sstate: tscannerstate;
        pd: tprocdef;
      begin
        maybe_add_public_default_java_constructor(def);
        replace_scanner('record_jvm_helpers',sstate);
        { no override, because not supported in records; the parser will still
          accept "inherited" though }
        if str_parse_method_dec('function clone: JLObject;',false,def,pd) then
          pd.synthetickind:=tsk_jvm_clone
        else
          internalerror(2011032806);
        { can't use def.typesym, not yet set at this point }
        if def.symtable.realname^='' then
          internalerror(2011032803);
        if str_parse_method_dec('procedure fpcDeepCopy(out result:'+def.symtable.realname^+');',false,def,pd) then
          pd.synthetickind:=tsk_record_deepcopy
        else
          internalerror(2011032807);
        restore_scanner(sstate);
      end;


{******************************************************************
                    jvm type validity checking
*******************************************************************}

   function jvmencodetype(def: tdef): string;
     var
       errordef: tdef;
     begin
       if not jvmtryencodetype(def,result,errordef) then
         internalerror(2011012305);
     end;


   function jvmchecktype(def: tdef; out founderror: tdef): boolean;
      var
        encodedtype: string;
      begin
        { don't duplicate the code like in objcdef, since the resulting strings
          are much shorter here so it's not worth it }
        result:=jvmtryencodetype(def,encodedtype,founderror);
      end;


end.
