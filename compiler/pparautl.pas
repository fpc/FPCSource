{
    Copyright (c) 1998-2002 by Florian Klaempfl, Daniel Mantione

    Helpers for dealing with subroutine parameters during parsing

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
unit pparautl;

{$i fpcdefs.inc}

interface

    uses
      symdef;

    procedure insert_funcret_para(pd:tabstractprocdef);
    procedure insert_parentfp_para(pd:tabstractprocdef);
    procedure insert_self_and_vmt_para(pd:tabstractprocdef);
    procedure insert_funcret_local(pd:tprocdef);
    procedure insert_hidden_para(p:TObject;arg:pointer);
    procedure check_c_para(pd:Tabstractprocdef);

implementation

    uses
      globals,globtype,verbose,systems,
      symconst,symtype,symbase,symsym,symtable,symcreat,defutil,
      paramgr;


    procedure insert_funcret_para(pd:tabstractprocdef);
      var
        storepos : tfileposinfo;
        vs       : tparavarsym;
        paranr   : word;
      begin
        if not(pd.proctypeoption in [potype_constructor,potype_destructor]) and
           not is_void(pd.returndef) and
           not (df_generic in pd.defoptions) and
           paramanager.ret_in_param(pd.returndef,pd) then
         begin
           storepos:=current_tokenpos;
           if pd.typ=procdef then
            current_tokenpos:=tprocdef(pd).fileinfo;

{$if defined(i386)}
           { For left to right add it at the end to be delphi compatible.
             In the case of safecalls with safecal-exceptions support the
             funcret-para is (from the 'c'-point of view) a normal parameter
             which has to be added to the end of the parameter-list }
           if (pd.proccalloption in (pushleftright_pocalls)) or
              ((tf_safecall_exceptions in target_info.flags) and
               (pd.proccalloption=pocall_safecall)) then
             paranr:=paranr_result_leftright
           else
{$elseif defined(SUPPORT_SAFECALL)}
           if (tf_safecall_exceptions in target_info.flags) and
              (pd.proccalloption = pocall_safecall)  then
             paranr:=paranr_result_leftright
           else
{$endif}
             paranr:=paranr_result;
           { Generate result variable accessing function result }
           vs:=tparavarsym.create('$result',paranr,vs_var,pd.returndef,[vo_is_funcret,vo_is_hidden_para]);
           pd.parast.insert(vs);
           { Store this symbol as funcretsym for procedures }
           if pd.typ=procdef then
            tprocdef(pd).funcretsym:=vs;

           current_tokenpos:=storepos;
         end;
      end;


    procedure insert_parentfp_para(pd:tabstractprocdef);
      var
        storepos : tfileposinfo;
        vs       : tparavarsym;
        paranr   : longint;
      begin
        if pd.parast.symtablelevel>normal_function_level then
          begin
            storepos:=current_tokenpos;
            if pd.typ=procdef then
             current_tokenpos:=tprocdef(pd).fileinfo;

            { if no support for nested procvars is activated, use the old
              calling convention to pass the parent frame pointer for backwards
              compatibility }
            if not(m_nested_procvars in current_settings.modeswitches) then
              paranr:=paranr_parentfp
            { nested procvars require Delphi-style parentfp passing, see
              po_delphi_nested_cc declaration for more info }
{$if defined(i386) or defined(i8086)}
            else if (pd.proccalloption in pushleftright_pocalls) then
              paranr:=paranr_parentfp_delphi_cc_leftright
{$endif i386 or i8086}
            else
              paranr:=paranr_parentfp_delphi_cc;
            { Generate frame pointer. It can't be put in a register since it
              must be accessable from nested routines }
            if not(target_info.system in systems_fpnestedstruct) or
               { in case of errors, prevent invalid type cast }
               (pd.owner.defowner.typ<>procdef) then
              begin
                vs:=tparavarsym.create('$parentfp',paranr,vs_value
                      ,voidpointertype,[vo_is_parentfp,vo_is_hidden_para]);
                vs.varregable:=vr_none;
              end
            else
              begin
                if not assigned(tprocdef(pd.owner.defowner).parentfpstruct) then
                  build_parentfpstruct(tprocdef(pd.owner.defowner));
                vs:=tparavarsym.create('$parentfp',paranr,vs_value
                      ,tprocdef(pd.owner.defowner).parentfpstructptrtype,[vo_is_parentfp,vo_is_hidden_para]);
              end;
            pd.parast.insert(vs);

            current_tokenpos:=storepos;
          end;
      end;


    procedure insert_self_and_vmt_para(pd:tabstractprocdef);
      var
        storepos : tfileposinfo;
        vs       : tparavarsym;
        hdef     : tdef;
        selfdef  : tdef;
        vsp      : tvarspez;
        aliasvs  : tabsolutevarsym;
        sl       : tpropaccesslist;
      begin
        if (pd.typ=procdef) and
           is_objc_class_or_protocol(tprocdef(pd).struct) and
           (pd.parast.symtablelevel=normal_function_level) then
          begin
            { insert Objective-C self and selector parameters }
            vs:=tparavarsym.create('$_cmd',paranr_objc_cmd,vs_value,objc_seltype,[vo_is_msgsel,vo_is_hidden_para]);
            pd.parast.insert(vs);
            { make accessible to code }
            sl:=tpropaccesslist.create;
            sl.addsym(sl_load,vs);
            aliasvs:=tabsolutevarsym.create_ref('_CMD',objc_seltype,sl);
            include(aliasvs.varoptions,vo_is_msgsel);
            tlocalsymtable(tprocdef(pd).localst).insert(aliasvs);

            if (po_classmethod in pd.procoptions) then
              { compatible with what gcc does }
              hdef:=objc_idtype
            else
              hdef:=tprocdef(pd).struct;

            vs:=tparavarsym.create('$self',paranr_objc_self,vs_value,hdef,[vo_is_self,vo_is_hidden_para]);
            pd.parast.insert(vs);
          end
        else if (pd.typ=procvardef) and
           pd.is_methodpointer then
          begin
            { Generate self variable }
            vs:=tparavarsym.create('$self',paranr_self,vs_value,voidpointertype,[vo_is_self,vo_is_hidden_para]);
            pd.parast.insert(vs);
          end
        else
          begin
             if (pd.typ=procdef) and
                assigned(tprocdef(pd).struct) and
                (pd.parast.symtablelevel=normal_function_level) then
              begin
                { static class methods have no hidden self/vmt pointer }
                if pd.no_self_node then
                   exit;

                storepos:=current_tokenpos;
                current_tokenpos:=tprocdef(pd).fileinfo;

                { Generate VMT variable for constructor/destructor }
                if (pd.proctypeoption in [potype_constructor,potype_destructor]) and
                   not(is_cppclass(tprocdef(pd).struct) or
                       is_record(tprocdef(pd).struct) or
                       is_javaclass(tprocdef(pd).struct) or
                       (
                         { no vmt for record/type helper constructors }
                         is_objectpascal_helper(tprocdef(pd).struct) and
                         (tobjectdef(tprocdef(pd).struct).extendeddef.typ<>objectdef)
                       )) then
                 begin
                   { can't use classrefdef as type because inheriting
                     will then always file because of a type mismatch }
                   vs:=tparavarsym.create('$vmt',paranr_vmt,vs_value,voidpointertype,[vo_is_vmt,vo_is_hidden_para]);
                   pd.parast.insert(vs);
                 end;

                { for helpers the type of Self is equivalent to the extended
                  type or equal to an instance of it }
                if is_objectpascal_helper(tprocdef(pd).struct) then
                  selfdef:=tobjectdef(tprocdef(pd).struct).extendeddef
                else
                  selfdef:=tprocdef(pd).struct;
                { Generate self variable, for classes we need
                  to use the generic voidpointer to be compatible with
                  methodpointers }
                vsp:=vs_value;
                if (po_staticmethod in pd.procoptions) or
                   (po_classmethod in pd.procoptions) then
                  hdef:=tclassrefdef.create(selfdef)
                else
                  begin
                    if is_object(selfdef) or (selfdef.typ<>objectdef) then
                      vsp:=vs_var;
                    hdef:=selfdef;
                  end;
                vs:=tparavarsym.create('$self',paranr_self,vsp,hdef,[vo_is_self,vo_is_hidden_para]);
                pd.parast.insert(vs);

                current_tokenpos:=storepos;
              end;
          end;
      end;


    procedure insert_funcret_local(pd:tprocdef);
      var
        storepos : tfileposinfo;
        vs       : tlocalvarsym;
        aliasvs  : tabsolutevarsym;
        sl       : tpropaccesslist;
        hs       : string;
      begin
        { The result from constructors and destructors can't be accessed directly }
        if not(pd.proctypeoption in [potype_constructor,potype_destructor]) and
           not is_void(pd.returndef) then
         begin
           storepos:=current_tokenpos;
           current_tokenpos:=pd.fileinfo;

           { We need to insert a varsym for the result in the localst
             when it is returning in a register }
           { we also need to do this for a generic procdef as we didn't allow
             the creation of a result symbol in insert_funcret_para, but we need
             a valid funcretsym }
           if (df_generic in pd.defoptions) or
               not paramanager.ret_in_param(pd.returndef,pd) then
            begin
              vs:=tlocalvarsym.create('$result',vs_value,pd.returndef,[vo_is_funcret]);
              pd.localst.insert(vs);
              pd.funcretsym:=vs;
            end;

           { insert the name of the procedure as alias for the function result,
             we can't use realname because that will not work for compilerprocs
             as the name is lowercase and unreachable from the code }
           if (pd.proctypeoption<>potype_operator) or assigned(pd.resultname) then
             begin
               if assigned(pd.resultname) then
                 hs:=pd.resultname^
               else
                 hs:=pd.procsym.name;
               sl:=tpropaccesslist.create;
               sl.addsym(sl_load,pd.funcretsym);
               aliasvs:=tabsolutevarsym.create_ref(hs,pd.returndef,sl);
               include(aliasvs.varoptions,vo_is_funcret);
               tlocalsymtable(pd.localst).insert(aliasvs);
             end;

           { insert result also if support is on }
           if (m_result in current_settings.modeswitches) then
            begin
              sl:=tpropaccesslist.create;
              sl.addsym(sl_load,pd.funcretsym);
              aliasvs:=tabsolutevarsym.create_ref('RESULT',pd.returndef,sl);
              include(aliasvs.varoptions,vo_is_funcret);
              include(aliasvs.varoptions,vo_is_result);
              tlocalsymtable(pd.localst).insert(aliasvs);
            end;

           current_tokenpos:=storepos;
         end;
      end;


    procedure insert_hidden_para(p:TObject;arg:pointer);
      var
        hvs : tparavarsym;
        pd  : tabstractprocdef absolute arg;
      begin
        if (tsym(p).typ<>paravarsym) then
         exit;
        with tparavarsym(p) do
         begin
           { We need a local copy for a value parameter when only the
             address is pushed. Open arrays and Array of Const are
             an exception because they are allocated at runtime and the
             address that is pushed is patched }
           if (varspez=vs_value) and
              paramanager.push_addr_param(varspez,vardef,pd.proccalloption) and
              not(is_open_array(vardef) or
                  is_array_of_const(vardef)) then
             include(varoptions,vo_has_local_copy);

           { needs high parameter ? }
           if paramanager.push_high_param(varspez,vardef,pd.proccalloption) then
             begin
               hvs:=tparavarsym.create('$high'+name,paranr+1,vs_const,sinttype,[vo_is_high_para,vo_is_hidden_para]);
               hvs.symoptions:=[];
               owner.insert(hvs);
               { don't place to register if it will be accessed from implicit finally block }
               if (varspez=vs_value) and
                  is_open_array(vardef) and
                  is_managed_type(vardef) then
                 hvs.varregable:=vr_none;
             end
           else
            begin
              { Give a warning that cdecl routines does not include high()
                support }
              if (pd.proccalloption in cdecl_pocalls) and
                 paramanager.push_high_param(varspez,vardef,pocall_default) then
               begin
                 if is_open_string(vardef) then
                    MessagePos(fileinfo,parser_w_cdecl_no_openstring);
                 if not(po_external in pd.procoptions) and
                    (pd.typ<>procvardef) and
                    not is_objc_class_or_protocol(tprocdef(pd).struct) then
                   if is_array_of_const(vardef) then
                     MessagePos(fileinfo,parser_e_varargs_need_cdecl_and_external)
                   else
                     MessagePos(fileinfo,parser_w_cdecl_has_no_high);
               end;
              if (vardef.typ=formaldef) and (Tformaldef(vardef).typed) then
                begin
                  hvs:=tparavarsym.create('$typinfo'+name,paranr+1,vs_const,voidpointertype,
                                          [vo_is_typinfo_para,vo_is_hidden_para]);
                  owner.insert(hvs);
                end;
            end;
         end;
      end;


    procedure check_c_para(pd:Tabstractprocdef);
      var
        i,
        lastparaidx : longint;
        sym : TSym;
      begin
        lastparaidx:=pd.parast.SymList.Count-1;
        for i:=0 to pd.parast.SymList.Count-1 do
          begin
            sym:=tsym(pd.parast.SymList[i]);
            if (sym.typ=paravarsym) and
               (tparavarsym(sym).vardef.typ=arraydef) then
              begin
                if not is_variant_array(tparavarsym(sym).vardef) and
                   not is_array_of_const(tparavarsym(sym).vardef) and
                   (tparavarsym(sym).varspez<>vs_var) then
                  MessagePos(tparavarsym(sym).fileinfo,parser_h_c_arrays_are_references);
                if is_array_of_const(tparavarsym(sym).vardef) and
                   (i<lastparaidx) and
                   (tsym(pd.parast.SymList[i+1]).typ=paravarsym) and
                   not(vo_is_high_para in tparavarsym(pd.parast.SymList[i+1]).varoptions) then
                  MessagePos(tparavarsym(sym).fileinfo,parser_e_C_array_of_const_must_be_last);
              end;
          end;
      end;


end.
