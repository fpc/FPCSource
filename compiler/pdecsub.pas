{
    Copyright (c) 1998-2002 by Florian Klaempfl, Daniel Mantione

    Does the parsing of the procedures/functions

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
unit pdecsub;

{$i fpcdefs.inc}

interface

    uses
      tokens,symconst,symtype,symdef,symsym;

    type
      tpdflag=(
        pd_body,         { directive needs a body }
        pd_implemen,     { directive can be used implementation section }
        pd_interface,    { directive can be used interface section }
        pd_object,       { directive can be used object declaration }
        pd_procvar,      { directive can be used procvar declaration }
        pd_notobject,    { directive can not be used object declaration }
        pd_notobjintf,   { directive can not be used interface declaration }
        pd_notprocvar,   { directive can not be used procvar declaration }
        pd_dispinterface,{ directive can be used with dispinterface methods }
        pd_cppobject     { directive can be used with cppclass }
      );
      tpdflags=set of tpdflag;

    function  check_proc_directive(isprocvar:boolean):boolean;

    procedure insert_funcret_local(pd:tprocdef);

    function  proc_add_definition(var currpd:tprocdef):boolean;
    function  proc_get_importname(pd:tprocdef):string;
    procedure proc_set_mangledname(pd:tprocdef);

    procedure handle_calling_convention(pd:tabstractprocdef);

    procedure parse_parameter_dec(pd:tabstractprocdef);
    procedure parse_proc_directives(pd:tabstractprocdef;var pdflags:tpdflags);
    procedure parse_var_proc_directives(sym:tsym);
    procedure parse_object_proc_directives(pd:tabstractprocdef);
    function  parse_proc_head(aclass:tobjectdef;potype:tproctypeoption;var pd:tprocdef):boolean;
    function  parse_proc_dec(aclass:tobjectdef):tprocdef;

implementation

    uses
       SysUtils,
       { common }
       cutils,cclasses,
       { global }
       globtype,globals,verbose,constexp,
       systems,
       cpuinfo,
       { symtable }
       symbase,symtable,defutil,defcmp,paramgr,cpupara,
       { pass 1 }
       fmodule,node,htypechk,
       nmat,nadd,ncal,nset,ncnv,ninl,ncon,nld,nflw,
       { parser }
       scanner,
       pbase,pexpr,ptype,pdecl
       ;

    const
      { Please leave this here, this module should NOT use
        these variables.
        Declaring it as string here results in an error when compiling (PFV) }
      current_procinfo = 'error';


    procedure insert_funcret_para(pd:tabstractprocdef);
      var
        storepos : tfileposinfo;
        vs       : tparavarsym;
        paranr   : word;
      begin
        if not(pd.proctypeoption in [potype_constructor,potype_destructor]) and
           not is_void(pd.returndef) and
           paramanager.ret_in_param(pd.returndef,pd.proccalloption) then
         begin
           storepos:=current_tokenpos;
           if pd.typ=procdef then
            current_tokenpos:=tprocdef(pd).fileinfo;

{$if defined(i386)}
           { For left to right add it at the end to be delphi compatible }
           if pd.proccalloption in (pushleftright_pocalls+[pocall_safecall])  then
             paranr:=paranr_result_leftright
           else
{$elseif defined(x86) or defined(arm)}
           { other platforms don't have a "safecall" convention,
             and never reverse the parameter pushing order
           }
           if (pd.proccalloption = pocall_safecall)  then
             paranr:=paranr_result_leftright
           else
{$endif}
             paranr:=paranr_result;
           { Generate result variable accessing function result }
           vs:=tparavarsym.create('$result',paranr,vs_var,pd.returndef,[vo_is_funcret,vo_is_hidden_para]);
           pd.parast.insert(vs);
           { Store the this symbol as funcretsym for procedures }
           if pd.typ=procdef then
            tprocdef(pd).funcretsym:=vs;

           current_tokenpos:=storepos;
         end;
      end;


    procedure insert_parentfp_para(pd:tabstractprocdef);
      var
        storepos : tfileposinfo;
        vs       : tparavarsym;
      begin
        if pd.parast.symtablelevel>normal_function_level then
          begin
            storepos:=current_tokenpos;
            if pd.typ=procdef then
             current_tokenpos:=tprocdef(pd).fileinfo;

            { Generate result variable accessing function result, it
              can't be put in a register since it must be accessable
              from the framepointer }
            vs:=tparavarsym.create('$parentfp',paranr_parentfp,vs_value
                  ,voidpointertype,[vo_is_parentfp,vo_is_hidden_para]);
            vs.varregable:=vr_none;
            pd.parast.insert(vs);

            current_tokenpos:=storepos;
          end;
      end;


    procedure insert_self_and_vmt_para(pd:tabstractprocdef);
      var
        storepos : tfileposinfo;
        vs       : tparavarsym;
        hdef     : tdef;
        vsp      : tvarspez;
      begin
        if (pd.typ=procvardef) and
           pd.is_methodpointer then
          begin
            { Generate self variable }
            vs:=tparavarsym.create('$self',paranr_self,vs_value,voidpointertype,[vo_is_self,vo_is_hidden_para]);
            pd.parast.insert(vs);
          end
        else
          begin
             if (pd.typ=procdef) and
                assigned(tprocdef(pd)._class) and
                (pd.parast.symtablelevel=normal_function_level) then
              begin
                { static class methods have no hidden self/vmt pointer }
                if (po_staticmethod in pd.procoptions) and
                   (po_classmethod in pd.procoptions) then
                   exit;

                storepos:=current_tokenpos;
                current_tokenpos:=tprocdef(pd).fileinfo;

                { Generate VMT variable for constructor/destructor }
                if (pd.proctypeoption in [potype_constructor,potype_destructor]) and not(is_cppclass(tprocdef(pd)._class)) then
                 begin
                   { can't use classrefdef as type because inheriting
                     will then always file because of a type mismatch }
                   vs:=tparavarsym.create('$vmt',paranr_vmt,vs_value,voidpointertype,[vo_is_vmt,vo_is_hidden_para]);
                   pd.parast.insert(vs);
                 end;

                { Generate self variable, for classes we need
                  to use the generic voidpointer to be compatible with
                  methodpointers }
                vsp:=vs_value;
                if (po_staticmethod in pd.procoptions) or
                   (po_classmethod in pd.procoptions) then
                  hdef:=tclassrefdef.create(tprocdef(pd)._class)
                else
                  begin
                    if is_object(tprocdef(pd)._class) then
                      vsp:=vs_var;
                    hdef:=tprocdef(pd)._class;
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
           if not paramanager.ret_in_param(pd.returndef,pd.proccalloption) then
            begin
              vs:=tlocalvarsym.create('$result',vs_value,pd.returndef,[vo_is_funcret]);
              pd.localst.insert(vs);
              pd.funcretsym:=vs;
            end;

           { insert the name of the procedure as alias for the function result,
             we can't use realname because that will not work for compilerprocs
             as the name is lowercase and unreachable from the code }
           if assigned(pd.resultname) then
             hs:=pd.resultname^
           else
             hs:=pd.procsym.name;
           sl:=tpropaccesslist.create;
           sl.addsym(sl_load,pd.funcretsym);
           aliasvs:=tabsolutevarsym.create_ref(hs,pd.returndef,sl);
           include(aliasvs.varoptions,vo_is_funcret);
           tlocalsymtable(pd.localst).insert(aliasvs);

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
             end
           else
            begin
              { Give a warning that cdecl routines does not include high()
                support }
              if (pd.proccalloption in [pocall_cdecl,pocall_cppdecl]) and
                 paramanager.push_high_param(varspez,vardef,pocall_default) then
               begin
                 if is_open_string(vardef) then
                    Message(parser_w_cdecl_no_openstring);
                 if not (po_external in pd.procoptions) then
                   Message(parser_w_cdecl_has_no_high);
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
                  Message(parser_h_c_arrays_are_references);
                if is_array_of_const(tparavarsym(sym).vardef) and
                   (i<lastparaidx) and
                   (tsym(pd.parast.SymList[i+1]).typ=paravarsym) and
                   not(vo_is_high_para in tparavarsym(pd.parast.SymList[i+1]).varoptions) then
                  Message(parser_e_C_array_of_const_must_be_last);
              end;
          end;
      end;


    procedure check_msg_para(p:TObject;arg:pointer);
      begin
        if (tsym(p).typ<>paravarsym) then
         exit;
        with tparavarsym(p) do
          begin
            { Count parameters }
            if (paranr>=10) then
              inc(plongint(arg)^);
            { First parameter must be var }
            if (paranr=10) and
               (varspez<>vs_var) then
              Message(parser_e_ill_msg_param);
          end;
      end;


    procedure set_addr_param_regable(p:TObject;arg:pointer);
      begin
        if (tsym(p).typ<>paravarsym) then
         exit;
        with tparavarsym(p) do
         begin
           if not vardef.needs_inittable and
              paramanager.push_addr_param(varspez,vardef,tprocdef(arg).proccalloption) then
             varregable:=vr_intreg;
         end;
      end;


    procedure parse_parameter_dec(pd:tabstractprocdef);
      {
        handle_procvar needs the same changes
      }
      type
        tppv = (pv_none,pv_proc,pv_func);
      var
        sc      : TFPObjectList;
        hdef    : tdef;
        arrayelementdef : tdef;
        vs      : tparavarsym;
        i       : longint;
        srsym   : tsym;
        pv      : tprocvardef;
        varspez : Tvarspez;
        defaultvalue : tconstsym;
        defaultrequired : boolean;
        old_block_type : tblock_type;
        currparast : tparasymtable;
        parseprocvar : tppv;
        explicit_paraloc : boolean;
        locationstr : string;
        paranr : integer;
        dummytype : ttypesym;
      begin
        old_block_type:=block_type;
        explicit_paraloc:=false;
        consume(_LKLAMMER);
        { Delphi/Kylix supports nonsense like }
        { procedure p();                      }
        if try_to_consume(_RKLAMMER) and
          not(m_tp7 in current_settings.modeswitches) then
          exit;
        { parsing a proc or procvar ? }
        currparast:=tparasymtable(pd.parast);
        { reset }
        sc:=TFPObjectList.create(false);
        defaultrequired:=false;
        paranr:=0;
        inc(testcurobject);
        block_type:=bt_var;
        repeat
          parseprocvar:=pv_none;
          if try_to_consume(_VAR) then
            varspez:=vs_var
          else
            if try_to_consume(_CONST) then
              varspez:=vs_const
          else
            if (m_out in current_settings.modeswitches) and
               try_to_consume(_OUT) then
              varspez:=vs_out
          else
            if (m_mac in current_settings.modeswitches) and
               try_to_consume(_POINTPOINTPOINT) then
              begin
                include(pd.procoptions,po_varargs);
                break;
              end
          else
            if (m_mac in current_settings.modeswitches) and
               try_to_consume(_PROCEDURE) then
              begin
                parseprocvar:=pv_proc;
                varspez:=vs_const;
              end
          else
            if (m_mac in current_settings.modeswitches) and
               try_to_consume(_FUNCTION) then
              begin
                parseprocvar:=pv_func;
                varspez:=vs_const;
              end
          else
              varspez:=vs_value;
          defaultvalue:=nil;
          hdef:=nil;
          { read identifiers and insert with error type }
          sc.clear;
          repeat
            inc(paranr);
            vs:=tparavarsym.create(orgpattern,paranr*10,varspez,generrordef,[]);
            currparast.insert(vs);
            if assigned(vs.owner) then
             sc.add(vs)
            else
             vs.free;
            consume(_ID);
          until not try_to_consume(_COMMA);
          locationstr:='';
          { macpas anonymous procvar }
          if parseprocvar<>pv_none then
           begin
             pv:=tprocvardef.create(normal_function_level);
             if token=_LKLAMMER then
               parse_parameter_dec(pv);
             if parseprocvar=pv_func then
              begin
                block_type:=bt_var_type;
                consume(_COLON);
                single_type(pv.returndef,false,false);
                block_type:=bt_var;
              end;
             hdef:=pv;
             { possible proc directives }
             if check_proc_directive(true) then
               begin
                  dummytype:=ttypesym.create('unnamed',hdef);
                  parse_var_proc_directives(tsym(dummytype));
                  dummytype.typedef:=nil;
                  hdef.typesym:=nil;
                  dummytype.free;
               end;
             { Add implicit hidden parameters and function result }
             handle_calling_convention(pv);
           end
          else
          { read type declaration, force reading for value and const paras }
           if (token=_COLON) or (varspez=vs_value) then
           begin
             consume(_COLON);
             { check for an open array }
             if token=_ARRAY then
              begin
                consume(_ARRAY);
                consume(_OF);
                { define range and type of range }
                hdef:=tarraydef.create(0,-1,s32inttype);
                { array of const ? }
                if (token=_CONST) and (m_objpas in current_settings.modeswitches) then
                 begin
                   consume(_CONST);
                   srsym:=search_system_type('TVARREC');
                   tarraydef(hdef).elementdef:=ttypesym(srsym).typedef;
                   include(tarraydef(hdef).arrayoptions,ado_IsArrayOfConst);
                 end
                else
                 begin
                   { define field type }
                   single_type(arrayelementdef,false,false);
                   tarraydef(hdef).elementdef:=arrayelementdef;
                 end;
              end
             else
              begin
                if (m_mac in current_settings.modeswitches) then
                  try_to_consume(_UNIV); {currently does nothing}
                if try_to_consume(_TYPE) then
                  hdef:=ctypedformaltype
                else
                  begin
                    block_type:=bt_var_type;
                    single_type(hdef,false,false);
                    block_type:=bt_var;
                  end;

                { open string ? }
                if (varspez in [vs_out,vs_var]) and
                   (cs_openstring in current_settings.moduleswitches) and
                   is_shortstring(hdef) then
                  hdef:=openshortstringtype;

                if (target_info.system in [system_powerpc_morphos,system_m68k_amiga]) then
                  begin
                    if (idtoken=_LOCATION) then
                      begin
                        consume(_LOCATION);
                        locationstr:=pattern;
                        consume(_CSTRING);
                      end
                    else
                      begin
                        if explicit_paraloc then
                          Message(parser_e_paraloc_all_paras);
                        locationstr:='';
                      end;
                  end
                else
                  locationstr:='';

                { default parameter }
                if (m_default_para in current_settings.modeswitches) then
                 begin
                   if try_to_consume(_EQUAL) then
                    begin
                      vs:=tparavarsym(sc[0]);
                      if sc.count>1 then
                        Message(parser_e_default_value_only_one_para);
                      { prefix 'def' to the parameter name }
                      defaultvalue:=ReadConstant('$def'+vs.name,vs.fileinfo);
                      if assigned(defaultvalue) then
                        begin
                          include(defaultvalue.symoptions,sp_internal);
                          pd.parast.insert(defaultvalue);
                        end;
                      defaultrequired:=true;
                    end
                   else
                    begin
                      if defaultrequired then
                        Message1(parser_e_default_value_expected_for_para,vs.name);
                    end;
                 end;
              end;
           end
          else
           hdef:=cformaltype;

          { File types are only allowed for var and out parameters }
          if (hdef.typ=filedef) and
             not(varspez in [vs_out,vs_var]) then
            CGMessage(cg_e_file_must_call_by_reference);

          for i:=0 to sc.count-1 do
            begin
              vs:=tparavarsym(sc[i]);
              { update varsym }
              vs.vardef:=hdef;
              vs.defaultconstsym:=defaultvalue;

              if (target_info.system in [system_powerpc_morphos,system_m68k_amiga]) then
                begin
                  if locationstr<>'' then
                    begin
                      if sc.count>1 then
                        Message(parser_e_paraloc_only_one_para);
                      if (paranr>1) and not(explicit_paraloc) then
                        Message(parser_e_paraloc_all_paras);
                      explicit_paraloc:=true;
                      include(vs.varoptions,vo_has_explicit_paraloc);
                      if not(paramanager.parseparaloc(vs,upper(locationstr))) then
                        message(parser_e_illegal_explicit_paraloc);
                    end
                  else
                    if explicit_paraloc then
                      Message(parser_e_paraloc_all_paras);
                end;
            end;
        until not try_to_consume(_SEMICOLON);

        if explicit_paraloc then
          begin
            pd.has_paraloc_info:=true;
            include(pd.procoptions,po_explicitparaloc);
          end;
        { remove parasymtable from stack }
        sc.free;
        { reset object options }
        dec(testcurobject);
        block_type:=old_block_type;
        consume(_RKLAMMER);
      end;


    function parse_proc_head(aclass:tobjectdef;potype:tproctypeoption;var pd:tprocdef):boolean;
      var
        hs       : string;
        orgsp,sp : TIDString;
        srsym : tsym;
        srsymtable : TSymtable;
        checkstack : psymtablestackitem;
        storepos,
        procstartfilepos : tfileposinfo;
        searchagain : boolean;
        st,
        genericst : TSymtable;
        aprocsym : tprocsym;
        popclass : boolean;
        ImplIntf : TImplementedInterface;
        old_parse_generic : boolean;
      begin
        { Save the position where this procedure really starts }
        procstartfilepos:=current_tokenpos;
        old_parse_generic:=parse_generic;

        result:=false;
        pd:=nil;
        aprocsym:=nil;

        if (potype=potype_operator) then
          begin
            sp:=overloaded_names[optoken];
            orgsp:=sp;
          end
        else
          begin
            sp:=pattern;
            orgsp:=orgpattern;
            consume(_ID);
          end;

        { examine interface map: function/procedure iname.functionname=locfuncname }
        if assigned(aclass) and
           assigned(aclass.ImplementedInterfaces) and
           (aclass.ImplementedInterfaces.count>0) and
           try_to_consume(_POINT) then
         begin
           storepos:=current_tokenpos;
           current_tokenpos:=procstartfilepos;
           { get interface syms}
           searchsym(sp,srsym,srsymtable);
           if not assigned(srsym) then
            begin
              identifier_not_found(orgsp);
              srsym:=generrorsym;
            end;
           current_tokenpos:=storepos;
           { qualifier is interface? }
           ImplIntf:=nil;
           if (srsym.typ=typesym) and
              (ttypesym(srsym).typedef.typ=objectdef) then
             ImplIntf:=aclass.find_implemented_interface(tobjectdef(ttypesym(srsym).typedef));
           if ImplIntf=nil then
             Message(parser_e_interface_id_expected);
           consume(_ID);
           { Create unique name <interface>.<method> }
           hs:=sp+'.'+pattern;
           consume(_EQUAL);
           if assigned(ImplIntf) and
              (token=_ID) then
             ImplIntf.AddMapping(hs,pattern);
           consume(_ID);
           result:=true;
           exit;
         end;

        { method  ? }
        if not assigned(aclass) and
           (potype<>potype_operator) and
           (symtablestack.top.symtablelevel=main_program_level) and
           try_to_consume(_POINT) then
         begin
           { search for object name }
           storepos:=current_tokenpos;
           current_tokenpos:=procstartfilepos;
           searchsym(sp,srsym,srsymtable);
           if not assigned(srsym) then
            begin
              identifier_not_found(orgsp);
              srsym:=generrorsym;
            end;
           current_tokenpos:=storepos;
           { consume proc name }
           sp:=pattern;
           orgsp:=orgpattern;
           procstartfilepos:=current_tokenpos;
           consume(_ID);
           { qualifier is class name ? }
           if (srsym.typ=typesym) and
              (ttypesym(srsym).typedef.typ=objectdef) then
            begin
              aclass:=tobjectdef(ttypesym(srsym).typedef);
              srsym:=tsym(aclass.symtable.Find(sp));
              if assigned(srsym) then
               begin
                 if srsym.typ=procsym then
                   aprocsym:=tprocsym(srsym)
                 else
                   begin
                     {  we use a different error message for tp7 so it looks more compatible }
                     if (m_fpc in current_settings.modeswitches) then
                       Message1(parser_e_overloaded_no_procedure,srsym.realname)
                     else
                       Message(parser_e_methode_id_expected);
                     { rename the name to an unique name to avoid an
                       error when inserting the symbol in the symtable }
                     orgsp:=orgsp+'$'+tostr(current_filepos.line);
                   end;
               end
              else
               begin
                 Message(parser_e_methode_id_expected);
                 { recover by making it a normal procedure instead of method }
                 aclass:=nil;
               end;
            end
           else
            Message(parser_e_class_id_expected);
         end
        else
         begin
           { check for constructor/destructor which is not allowed here }
           if (not parse_only) and
              (potype in [potype_constructor,potype_destructor]) then
             Message(parser_e_constructors_always_objects);

           repeat
             searchagain:=false;
             current_tokenpos:=procstartfilepos;

             srsymtable:=symtablestack.top;
             srsym:=tsym(srsymtable.Find(sp));

             { Also look in the globalsymtable if we didn't found
               the symbol in the localsymtable }
             if not assigned(srsym) and
                not(parse_only) and
                (srsymtable=current_module.localsymtable) and
                assigned(current_module.globalsymtable) then
               srsym:=tsym(current_module.globalsymtable.Find(sp));

             { Check if overloaded is a procsym }
             if assigned(srsym) then
               begin
                 if srsym.typ=procsym then
                   aprocsym:=tprocsym(srsym)
                 else
                   begin
                     { when the other symbol is a unit symbol then hide the unit
                       symbol, this is not supported in tp7 }
                     if not(m_tp7 in current_settings.modeswitches) and
                        (srsym.typ=unitsym) then
                      begin
                        HideSym(srsym);
                        searchagain:=true;
                      end
                     else
                      begin
                        {  we use a different error message for tp7 so it looks more compatible }
                        if (m_fpc in current_settings.modeswitches) then
                          Message1(parser_e_overloaded_no_procedure,srsym.realname)
                        else
                          Message1(sym_e_duplicate_id,srsym.realname);
                        { rename the name to an unique name to avoid an
                          error when inserting the symbol in the symtable }
                        orgsp:=orgsp+'$'+tostr(current_filepos.line);
                      end;
                   end;
              end;
           until not searchagain;
         end;

        { test again if assigned, it can be reset to recover }
        if not assigned(aprocsym) then
         begin
           { create a new procsym and set the real filepos }
           current_tokenpos:=procstartfilepos;
           { for operator we have only one procsym for each overloaded
             operation }
           if (potype=potype_operator) then
             begin
               Aprocsym:=Tprocsym(symtablestack.top.Find(sp));
               if Aprocsym=nil then
                 Aprocsym:=tprocsym.create('$'+sp);
             end
            else
             aprocsym:=tprocsym.create(orgsp);
            symtablestack.top.insert(aprocsym);
         end;

        { to get the correct symtablelevel we must ignore ObjectSymtables }
        st:=nil;
        checkstack:=symtablestack.stack;
        while assigned(checkstack) do
          begin
            st:=checkstack^.symtable;
            if st.symtabletype in [staticsymtable,globalsymtable,localsymtable] then
              break;
            checkstack:=checkstack^.next;
          end;
        pd:=tprocdef.create(st.symtablelevel+1);
        pd._class:=aclass;
        pd.procsym:=aprocsym;
        pd.proctypeoption:=potype;

        { methods inherit df_generic or df_specialization from the objectdef }
        if assigned(pd._class) and
           (pd.parast.symtablelevel=normal_function_level) then
          begin
            if (df_generic in pd._class.defoptions) then
              begin
                include(pd.defoptions,df_generic);
                parse_generic:=true;
              end;
            if (df_specialization in pd._class.defoptions) then
              begin
                include(pd.defoptions,df_specialization);
                { Find corresponding genericdef, we need it later to
                  replay the tokens to generate the body }
                if not assigned(pd._class.genericdef) then
                  internalerror(200512113);
                genericst:=pd._class.genericdef.GetSymtable(gs_record);
                if not assigned(genericst) then
                  internalerror(200512114);
                { We are parsing the same objectdef, the def index numbers
                  are the same }
                pd.genericdef:=tstoreddef(genericst.DefList[pd.owner.DefList.IndexOf(pd)]);
                if not assigned(pd.genericdef) or
                   (pd.genericdef.typ<>procdef) then
                  internalerror(200512115);
              end;
          end;

        { methods need to be exported }
        if assigned(aclass) and
           (
            (symtablestack.top.symtabletype=ObjectSymtable) or
            (symtablestack.top.symtablelevel=main_program_level)
           ) then
          include(pd.procoptions,po_global);

        { symbol options that need to be kept per procdef }
        pd.fileinfo:=procstartfilepos;
        pd.visibility:=symtablestack.top.currentvisibility;

        { parse parameters }
        if token=_LKLAMMER then
          begin
            { Add ObjectSymtable to be able to find generic type definitions }
            popclass:=false;
            if assigned(pd._class) and
               (pd.parast.symtablelevel=normal_function_level) and
               (symtablestack.top.symtabletype<>ObjectSymtable) then
              begin
                symtablestack.push(pd._class.symtable);
                popclass:=true;
              end;
            { Add parameter symtable }
            if pd.parast.symtabletype<>staticsymtable then
              symtablestack.push(pd.parast);
            parse_parameter_dec(pd);
            if pd.parast.symtabletype<>staticsymtable then
              symtablestack.pop(pd.parast);
            if popclass then
              symtablestack.pop(pd._class.symtable);
          end;

        parse_generic:=old_parse_generic;
        result:=true;
      end;


    function parse_proc_dec(aclass:tobjectdef):tprocdef;
      var
        pd : tprocdef;
        isclassmethod : boolean;
        locationstr: string;
        old_parse_generic,
        popclass           : boolean;
      begin
        locationstr:='';
        pd:=nil;
        isclassmethod:=false;
        { read class method }
        if try_to_consume(_CLASS) then
         begin
           { class method only allowed for procedures and functions }
           if not(token in [_FUNCTION,_PROCEDURE]) then
             Message(parser_e_procedure_or_function_expected);

           if is_interface(aclass) then
             Message(parser_e_no_static_method_in_interfaces)
           else
             isclassmethod:=true;
         end;
        case token of
          _FUNCTION :
            begin
              consume(_FUNCTION);
              if parse_proc_head(aclass,potype_function,pd) then
                begin
                  { pd=nil when it is a interface mapping }
                  if assigned(pd) then
                    begin
                      if try_to_consume(_COLON) then
                       begin
                         old_parse_generic:=parse_generic;
                         inc(testcurobject);
                         { Add ObjectSymtable to be able to find generic type definitions }
                         popclass:=false;
                         if assigned(pd._class) and
                            (pd.parast.symtablelevel=normal_function_level) and
                            (symtablestack.top.symtabletype<>ObjectSymtable) then
                           begin
                             symtablestack.push(pd._class.symtable);
                             popclass:=true;
                             parse_generic:=(df_generic in pd._class.defoptions);
                           end;
                         single_type(pd.returndef,false,false);
                         if popclass then
                           symtablestack.pop(pd._class.symtable);
                         dec(testcurobject);
                         parse_generic:=old_parse_generic;

                         if (target_info.system in [system_m68k_amiga]) then
                          begin
                           if (idtoken=_LOCATION) then
                            begin
                             if po_explicitparaloc in pd.procoptions then
                              begin
                               consume(_LOCATION);
                               locationstr:=pattern;
                               consume(_CSTRING);
                              end
                             else
                              { I guess this needs a new message... (KB) }
                              Message(parser_e_paraloc_all_paras);
                            end
                           else
                            begin
                             if po_explicitparaloc in pd.procoptions then
                              { assign default locationstr, if none specified }
                              { and we've arguments with explicit paraloc }
                              locationstr:='D0';
                            end;
                          end;

                       end
                      else
                       begin
                          if (
                              parse_only and
                              not(is_interface(pd._class))
                             ) or
                             (m_repeat_forward in current_settings.modeswitches) then
                          begin
                            consume(_COLON);
                            consume_all_until(_SEMICOLON);
                          end;
                       end;
                      if isclassmethod then
                       include(pd.procoptions,po_classmethod);
                    end;
                end
              else
                begin
                  { recover }
                  consume(_COLON);
                  consume_all_until(_SEMICOLON);
                end;
            end;

          _PROCEDURE :
            begin
              consume(_PROCEDURE);
              if parse_proc_head(aclass,potype_procedure,pd) then
                begin
                  { pd=nil when it is a interface mapping }
                  if assigned(pd) then
                    begin
                      pd.returndef:=voidtype;
                      if isclassmethod then
                        include(pd.procoptions,po_classmethod);
                    end;
                end;
            end;

          _CONSTRUCTOR :
            begin
              consume(_CONSTRUCTOR);
              parse_proc_head(aclass,potype_constructor,pd);
              if assigned(pd) and
                 assigned(pd._class) then
                begin
                  { Set return type, class constructors return the
                    created instance, object constructors return boolean }
                  if is_class(pd._class) then
                    pd.returndef:=pd._class
                  else
{$ifdef CPU64bitaddr}
                    pd.returndef:=bool64type;
{$else CPU64bitaddr}
                    pd.returndef:=bool32type;
{$endif CPU64bitaddr}
                end;
            end;

          _DESTRUCTOR :
            begin
              consume(_DESTRUCTOR);
              parse_proc_head(aclass,potype_destructor,pd);
              if assigned(pd) then
                pd.returndef:=voidtype;
            end;

          _OPERATOR :
            begin
              consume(_OPERATOR);
              if (token in [first_overloaded..last_overloaded]) then
               begin
                 optoken:=token;
               end
              else
               begin
                 case token of
                   _CARET:
                     Message1(parser_e_overload_operator_failed,'**');
                   _UNEQUAL:
                     Message1(parser_e_overload_operator_failed,'=');
                   else
                     Message1(parser_e_overload_operator_failed,'');
                 end;
                 { Use the dummy NOTOKEN that is also declared
                   for the overloaded_operator[] }
                 optoken:=NOTOKEN;
               end;
              consume(token);
              parse_proc_head(aclass,potype_operator,pd);
              if assigned(pd) then
                begin
                  { operators always need to be searched in all units }
                  include(pd.procoptions,po_overload);
                  if pd.parast.symtablelevel>normal_function_level then
                    Message(parser_e_no_local_operator);
                  if token<>_ID then
                    begin
                       if not(m_result in current_settings.modeswitches) then
                         consume(_ID);
                    end
                  else
                    begin
                      pd.resultname:=stringdup(orgpattern);
                      consume(_ID);
                    end;
                  if not try_to_consume(_COLON) then
                    begin
                      consume(_COLON);
                      pd.returndef:=generrordef;
                      consume_all_until(_SEMICOLON);
                    end
                  else
                   begin
                     single_type(pd.returndef,false,false);
                     if (optoken in [_EQUAL,_GT,_LT,_GTE,_LTE]) and
                        ((pd.returndef.typ<>orddef) or
                         (torddef(pd.returndef).ordtype<>pasbool)) then
                        Message(parser_e_comparative_operator_return_boolean);
                     if (optoken=_ASSIGNMENT) and
                        equal_defs(pd.returndef,tparavarsym(pd.parast.SymList[0]).vardef) then
                       message(parser_e_no_such_assignment)
                     else if not isoperatoracceptable(pd,optoken) then
                       Message(parser_e_overload_impossible);
                   end;
                end
              else
                begin
                  { recover }
                  try_to_consume(_ID);
                  consume(_COLON);
                  consume_all_until(_SEMICOLON);
                end;
            end;
        end;
        { file types can't be function results }
        if assigned(pd) and
           (pd.returndef.typ=filedef) then
          message(parser_e_illegal_function_result);
        { support procedure proc stdcall export; }
        if not(check_proc_directive(false)) then
          begin
            if (token=_COLON) then
              begin
                message(parser_e_field_not_allowed_here);
                consume_all_until(_SEMICOLON);
              end;
            consume(_SEMICOLON);
          end;
        result:=pd;

        if locationstr<>'' then
         begin
           if not(paramanager.parsefuncretloc(pd,upper(locationstr))) then
             { I guess this needs a new message... (KB) }
             message(parser_e_illegal_explicit_paraloc);
         end;
      end;


{****************************************************************************
                        Procedure directive handlers
****************************************************************************}

procedure pd_far(pd:tabstractprocdef);
begin
  Message1(parser_w_proc_directive_ignored,'FAR');
end;

procedure pd_near(pd:tabstractprocdef);
begin
  Message1(parser_w_proc_directive_ignored,'NEAR');
end;

procedure pd_export(pd:tabstractprocdef);
begin
  if pd.typ<>procdef then
    internalerror(200304264);
  if assigned(tprocdef(pd)._class) then
    Message(parser_e_methods_dont_be_export);
  if pd.parast.symtablelevel>normal_function_level then
    Message(parser_e_dont_nest_export);
end;

procedure pd_forward(pd:tabstractprocdef);
begin
  if pd.typ<>procdef then
    internalerror(200304265);
  tprocdef(pd).forwarddef:=true;
end;


procedure pd_alias(pd:tabstractprocdef);
begin
  if pd.typ<>procdef then
    internalerror(200304266);
  consume(_COLON);
  tprocdef(pd).aliasnames.insert(get_stringconst);
  include(pd.procoptions,po_has_public_name);
end;


procedure pd_public(pd:tabstractprocdef);
begin
  if pd.typ<>procdef then
    internalerror(200304266);
  if try_to_consume(_NAME) then
    begin
      tprocdef(pd).aliasnames.insert(get_stringconst);
      include(pd.procoptions,po_has_public_name);
    end;
end;


procedure pd_asmname(pd:tabstractprocdef);
begin
  if pd.typ<>procdef then
    internalerror(200304267);
  tprocdef(pd).aliasnames.insert(target_info.Cprefix+pattern);
  if token=_CCHAR then
    consume(_CCHAR)
  else
    consume(_CSTRING);
  { we don't need anything else }
  tprocdef(pd).forwarddef:=false;
end;


procedure pd_internconst(pd:tabstractprocdef);

var v:Tconstexprint;

begin
  if pd.typ<>procdef then
    internalerror(200304268);
  consume(_COLON);
  v:=get_intconst;
  if (v<int64(low(longint))) or (v>int64(high(longint))) then
    message(parser_e_range_check_error)
  else
    Tprocdef(pd).extnumber:=longint(v.svalue);
end;


procedure pd_internproc(pd:tabstractprocdef);

var v:Tconstexprint;

begin
  if pd.typ<>procdef then
    internalerror(200304268);
  consume(_COLON);
  v:=get_intconst;
  if (v<int64(low(longint))) or (v>int64(high(longint))) then
    message(parser_e_range_check_error)
  else
    Tprocdef(pd).extnumber:=longint(v.svalue);
  { the proc is defined }
  tprocdef(pd).forwarddef:=false;
end;

procedure pd_interrupt(pd:tabstractprocdef);
begin
  if pd.parast.symtablelevel>normal_function_level then
    Message(parser_e_dont_nest_interrupt);
end;

procedure pd_abstract(pd:tabstractprocdef);
begin
  if pd.typ<>procdef then
    internalerror(200304269);
  if (po_virtualmethod in pd.procoptions) then
    include(pd.procoptions,po_abstractmethod)
  else
    Message(parser_e_only_virtual_methods_abstract);
  { the method is defined }
  tprocdef(pd).forwarddef:=false;
end;

procedure pd_virtual(pd:tabstractprocdef);
{$ifdef WITHDMT}
var
  pt : tnode;
{$endif WITHDMT}
begin
  if pd.typ<>procdef then
    internalerror(2003042610);
  if (pd.proctypeoption=potype_constructor) and
     is_object(tprocdef(pd)._class) then
    Message(parser_e_constructor_cannot_be_not_virtual);
{$ifdef WITHDMT}
  if is_object(tprocdef(pd)._class) and
     (token<>_SEMICOLON) then
    begin
       { any type of parameter is allowed here! }
       pt:=comp_expr(true);
       if is_constintnode(pt) then
         begin
           include(pd.procoptions,po_msgint);
           pd.messageinf.i:=pt.value;
         end
       else
         Message(parser_e_ill_msg_expr);
       disposetree(pt);
    end;
{$endif WITHDMT}
end;


procedure pd_dispid(pd:tabstractprocdef);

var pt:Tnode;

begin
  if pd.typ<>procdef then
    internalerror(200604301);
  pt:=comp_expr(true);
  if is_constintnode(pt) then
    if (Tordconstnode(pt).value<int64(low(longint))) or (Tordconstnode(pt).value>int64(high(longint))) then
      message(parser_e_range_check_error)
    else
      Tprocdef(pd).dispid:=Tordconstnode(pt).value.svalue
  else
    message(parser_e_dispid_must_be_ord_const);
  pt.free;
end;


procedure pd_static(pd:tabstractprocdef);
begin
  if (cs_static_keyword in current_settings.moduleswitches) then
    begin
      if pd.typ=procdef then
        include(tprocdef(pd).procsym.symoptions,sp_static);
      include(pd.procoptions,po_staticmethod);
    end;
end;

procedure pd_override(pd:tabstractprocdef);
begin
  if pd.typ<>procdef then
    internalerror(2003042611);
  if not(is_class_or_interface(tprocdef(pd)._class)) then
    Message(parser_e_no_object_override);
end;

procedure pd_overload(pd:tabstractprocdef);
begin
  if pd.typ<>procdef then
    internalerror(2003042612);
  include(tprocdef(pd).procsym.symoptions,sp_has_overloaded);
end;

procedure pd_message(pd:tabstractprocdef);
var
  pt : tnode;
  paracnt : longint;
begin
  if pd.typ<>procdef then
    internalerror(2003042613);
  if not is_class(tprocdef(pd)._class) then
    Message(parser_e_msg_only_for_classes);
  { check parameter type }
  paracnt:=0;
  pd.parast.SymList.ForEachCall(@check_msg_para,@paracnt);
  if paracnt<>1 then
    Message(parser_e_ill_msg_param);
  pt:=comp_expr(true);
  if pt.nodetype=stringconstn then
    begin
      include(pd.procoptions,po_msgstr);
      tprocdef(pd).messageinf.str:=stringdup(tstringconstnode(pt).value_str);
    end
  else
   if is_constintnode(pt) then
    begin
      include(pd.procoptions,po_msgint);
      if (Tordconstnode(pt).value<int64(low(Tprocdef(pd).messageinf.i))) or
         (Tordconstnode(pt).value>int64(high(Tprocdef(pd).messageinf.i))) then
        message(parser_e_range_check_error)
      else
        Tprocdef(pd).messageinf.i:=tordconstnode(pt).value.svalue;
    end
  else
    Message(parser_e_ill_msg_expr);
  pt.free;
end;


procedure pd_reintroduce(pd:tabstractprocdef);
begin
  if pd.typ<>procdef then
    internalerror(200401211);
  if not(is_class_or_interface(tprocdef(pd)._class)) then
    Message(parser_e_no_object_reintroduce);
end;


procedure pd_syscall(pd:tabstractprocdef);
{$if defined(powerpc) or defined(m68k)}
var
  vs  : tparavarsym;
  sym : tsym;
  symtable : TSymtable;
  v: Tconstexprint;
{$endif defined(powerpc) or defined(m68k)}
begin
  if (pd.typ<>procdef) and (target_info.system <> system_powerpc_amiga) then
    internalerror(2003042614);
  tprocdef(pd).forwarddef:=false;
{$ifdef m68k}
   if target_info.system in [system_m68k_amiga] then
    begin
      include(pd.procoptions,po_syscall_legacy);

      if consume_sym(sym,symtable) then
        begin
          if (sym.typ=staticvarsym) and
             (
              (tabstractvarsym(sym).vardef.typ=pointerdef) or
              is_32bitint(tabstractvarsym(sym).vardef)
             ) then
            begin
              tprocdef(pd).libsym:=sym;
              if po_syscall_legacy in tprocdef(pd).procoptions then
                begin
                  vs:=tparavarsym.create('$syscalllib',paranr_syscall_legacy,vs_value,tabstractvarsym(sym).vardef,[vo_is_syscall_lib,vo_is_hidden_para,vo_has_explicit_paraloc]);
                  paramanager.parseparaloc(vs,'A6');
                  pd.parast.insert(vs);
                end
            end
          else
            Message(parser_e_32bitint_or_pointer_variable_expected);
        end;
      (paramanager as tm68kparamanager).create_funcretloc_info(pd,calleeside);
      (paramanager as tm68kparamanager).create_funcretloc_info(pd,callerside);

      v:=get_intconst;
      if (v<low(Tprocdef(pd).extnumber)) or (v>high(Tprocdef(pd).extnumber)) then
        message(parser_e_range_check_error)
      else
        Tprocdef(pd).extnumber:=v.uvalue;
    end;
{$endif m68k}
{$ifdef powerpc}
   if target_info.system = system_powerpc_amiga then
    begin
      include(pd.procoptions,po_syscall_basesysv);

      if consume_sym(sym,symtable) then
        begin
          if (sym.typ=staticvarsym) and
             (
              (tabstractvarsym(sym).vardef.typ=pointerdef) or
              is_32bitint(tabstractvarsym(sym).vardef)
             ) then
            begin
              tprocdef(pd).libsym:=sym;
              vs:=tparavarsym.create('$syscalllib',paranr_syscall_basesysv,vs_value,tabstractvarsym(sym).vardef,[vo_is_syscall_lib,vo_is_hidden_para]);
              pd.parast.insert(vs);
            end
          else
            Message(parser_e_32bitint_or_pointer_variable_expected);
        end;

      (paramanager as tppcparamanager).create_funcretloc_info(pd,calleeside);
      (paramanager as tppcparamanager).create_funcretloc_info(pd,callerside);

      v:=get_intconst;
      if (v<low(Tprocdef(pd).extnumber)) or (v>high(Tprocdef(pd).extnumber)) then
        message(parser_e_range_check_error)
      else
        Tprocdef(pd).extnumber:=v.uvalue;
    end else

   if target_info.system = system_powerpc_morphos then
    begin
      if idtoken=_LEGACY then
        begin
          consume(_LEGACY);
          include(pd.procoptions,po_syscall_legacy);
        end
      else if idtoken=_SYSV then
        begin
          consume(_SYSV);
          include(pd.procoptions,po_syscall_sysv);
        end
      else if idtoken=_BASESYSV then
        begin
          consume(_BASESYSV);
          include(pd.procoptions,po_syscall_basesysv);
        end
      else if idtoken=_SYSVBASE then
        begin
          consume(_SYSVBASE);
          include(pd.procoptions,po_syscall_sysvbase);
        end
      else if idtoken=_R12BASE then
        begin
          consume(_R12BASE);
          include(pd.procoptions,po_syscall_r12base);
        end
      else
        if syscall_convention='LEGACY' then
          include(pd.procoptions,po_syscall_legacy)
        else if syscall_convention='SYSV' then
          include(pd.procoptions,po_syscall_sysv)
        else if syscall_convention='BASESYSV' then
          include(pd.procoptions,po_syscall_basesysv)
        else if syscall_convention='SYSVBASE' then
          include(pd.procoptions,po_syscall_sysvbase)
        else if syscall_convention='R12BASE' then
          include(pd.procoptions,po_syscall_r12base)
        else
          internalerror(2005010404);

      if consume_sym(sym,symtable) then
        begin
          if (sym.typ=staticvarsym) and
             (
              (tabstractvarsym(sym).vardef.typ=pointerdef) or
              is_32bitint(tabstractvarsym(sym).vardef)
             ) then
            begin
              tprocdef(pd).libsym:=sym;
              if po_syscall_legacy in tprocdef(pd).procoptions then
                begin
                  vs:=tparavarsym.create('$syscalllib',paranr_syscall_legacy,vs_value,tabstractvarsym(sym).vardef,[vo_is_syscall_lib,vo_is_hidden_para,vo_has_explicit_paraloc]);
                  paramanager.parseparaloc(vs,'A6');
                  pd.parast.insert(vs);
                end
              else if po_syscall_sysv in tprocdef(pd).procoptions then
                begin
                  { Nothing to be done for sysv here for now, but this might change }
                end
              else if po_syscall_basesysv in tprocdef(pd).procoptions then
                begin
                  vs:=tparavarsym.create('$syscalllib',paranr_syscall_basesysv,vs_value,tabstractvarsym(sym).vardef,[vo_is_syscall_lib,vo_is_hidden_para]);
                  pd.parast.insert(vs);
                end
              else if po_syscall_sysvbase in tprocdef(pd).procoptions then
                begin
                  vs:=tparavarsym.create('$syscalllib',paranr_syscall_sysvbase,vs_value,tabstractvarsym(sym).vardef,[vo_is_syscall_lib,vo_is_hidden_para]);
                  pd.parast.insert(vs);
                end
              else if po_syscall_r12base in tprocdef(pd).procoptions then
                begin
                  vs:=tparavarsym.create('$syscalllib',paranr_syscall_r12base,vs_value,tabstractvarsym(sym).vardef,[vo_is_syscall_lib,vo_is_hidden_para,vo_has_explicit_paraloc]);
                  paramanager.parseparaloc(vs,'R12');
                  pd.parast.insert(vs);
                end
              else
                internalerror(2005010501);
            end
          else
            Message(parser_e_32bitint_or_pointer_variable_expected);
        end;
      (paramanager as tppcparamanager).create_funcretloc_info(pd,calleeside);
      (paramanager as tppcparamanager).create_funcretloc_info(pd,callerside);

      v:=get_intconst;
      if (v<low(Tprocdef(pd).extnumber)) or (v>high(Tprocdef(pd).extnumber)) then
        message(parser_e_range_check_error)
      else
        Tprocdef(pd).extnumber:=v.uvalue;
    end;
{$endif powerpc}
end;


procedure pd_external(pd:tabstractprocdef);
{
  If import_dll=nil the procedure is assumed to be in another
  object file. In that object file it should have the name to
  which import_name is pointing to. Otherwise, the procedure is
  assumed to be in the DLL to which import_dll is pointing to. In
  that case either import_nr<>0 or import_name<>nil is true, so
  the procedure is either imported by number or by name. (DM)
}
var
  hs : string;
  v:Tconstexprint;

begin
  if pd.typ<>procdef then
    internalerror(2003042615);
  with tprocdef(pd) do
    begin
      forwarddef:=false;
      { forbid local external procedures }
      if parast.symtablelevel>normal_function_level then
        Message(parser_e_no_local_proc_external);
      { If the procedure should be imported from a DLL, a constant string follows.
        This isn't really correct, an contant string expression follows
        so we check if an semicolon follows, else a string constant have to
        follow (FK) }
      if not(token=_SEMICOLON) and not(idtoken=_NAME) then
        begin
          { Always add library prefix and suffix to create an uniform name }
          hs:=get_stringconst;
          if ExtractFileExt(hs)='' then
            hs:=ChangeFileExt(hs,target_info.sharedlibext);
          if Copy(hs,1,length(target_info.sharedlibprefix))<>target_info.sharedlibprefix then
            hs:=target_info.sharedlibprefix+hs;
          import_dll:=stringdup(hs);
          include(procoptions,po_has_importdll);
          if (idtoken=_NAME) then
           begin
             consume(_NAME);
             import_name:=stringdup(get_stringconst);
             include(procoptions,po_has_importname);
             if import_name^='' then
               message(parser_e_empty_import_name);
           end;
          if (idtoken=_INDEX) then
           begin
             {After the word index follows the index number in the DLL.}
             consume(_INDEX);
             v:=get_intconst;
             if (v<int64(low(import_nr))) or (v>int64(high(import_nr))) then
               message(parser_e_range_check_error)
             else
               import_nr:=longint(v.svalue);
           end;
          { default is to used the realname of the procedure }
          if (import_nr=0) and not assigned(import_name) then
            begin
              import_name:=stringdup(procsym.realname);
              include(procoptions,po_has_importname);
            end;
        end
      else
        begin
          if (idtoken=_NAME) then
           begin
             consume(_NAME);
             import_name:=stringdup(get_stringconst);
             include(procoptions,po_has_importname);
             if import_name^='' then
               message(parser_e_empty_import_name);
           end;
        end;
    end;
end;


procedure pd_weakexternal(pd:tabstractprocdef);
begin
  if not(target_info.system in system_weak_linking) then
    message(parser_e_weak_external_not_supported)
  else
    pd_external(pd);
end;


type
   pd_handler=procedure(pd:tabstractprocdef);
   proc_dir_rec=record
     idtok     : ttoken;
     pd_flags  : tpdflags;
     handler   : pd_handler;
     pocall    : tproccalloption;
     pooption  : tprocoptions;
     mutexclpocall : tproccalloptions;
     mutexclpotype : tproctypeoptions;
     mutexclpo     : tprocoptions;
   end;
const
  {Should contain the number of procedure directives we support.}
  num_proc_directives=40;
  proc_direcdata:array[1..num_proc_directives] of proc_dir_rec=
   (
    (
      idtok:_ABSTRACT;
      pd_flags : [pd_interface,pd_object,pd_notobjintf];
      handler  : @pd_abstract;
      pocall   : pocall_none;
      pooption : [po_abstractmethod];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_exports,po_interrupt,po_external,po_inline]
    ),(
      idtok:_ALIAS;
      pd_flags : [pd_implemen,pd_body,pd_notobjintf];
      handler  : @pd_alias;
      pocall   : pocall_none;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [];
      mutexclpo     : [po_external,po_inline]
    ),(
      idtok:_ASMNAME;
      pd_flags : [pd_interface,pd_implemen,pd_notobjintf];
      handler  : @pd_asmname;
      pocall   : pocall_cdecl;
      pooption : [po_external];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_external,po_inline]
    ),(
      idtok:_ASSEMBLER;
      pd_flags : [pd_interface,pd_implemen,pd_body,pd_notobjintf];
      handler  : nil;
      pocall   : pocall_none;
      pooption : [po_assembler];
      mutexclpocall : [];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_C; {same as cdecl for mode mac}
      pd_flags : [pd_interface,pd_implemen,pd_body,pd_procvar];
      handler  : nil;
      pocall   : pocall_cdecl;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_assembler,po_external]
    ),(
      idtok:_CDECL;
      pd_flags : [pd_interface,pd_implemen,pd_body,pd_procvar];
      handler  : nil;
      pocall   : pocall_cdecl;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_assembler,po_external]
    ),(
      idtok:_DISPID;
      pd_flags : [pd_dispinterface];
      handler  : @pd_dispid;
      pocall   : pocall_none;
      pooption : [po_dispid];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [potype_constructor,potype_destructor,potype_operator];
      mutexclpo     : [po_interrupt,po_external,po_inline]
    ),(
      idtok:_DYNAMIC;
      pd_flags : [pd_interface,pd_object,pd_notobjintf];
      handler  : @pd_virtual;
      pocall   : pocall_none;
      pooption : [po_virtualmethod];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_exports,po_interrupt,po_external,po_overridingmethod,po_inline]
    ),(
      idtok:_EXPORT;
      pd_flags : [pd_body,pd_interface,pd_implemen,pd_notobjintf];
      handler  : @pd_export;
      pocall   : pocall_none;
      pooption : [po_exports,po_global];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_external,po_interrupt,po_inline]
    ),(
      idtok:_EXTERNAL;
      pd_flags : [pd_implemen,pd_interface,pd_notobject,pd_notobjintf,pd_cppobject];
      handler  : @pd_external;
      pocall   : pocall_none;
      pooption : [po_external];
      mutexclpocall : [pocall_internproc,pocall_syscall];
      { allowed for external cpp classes }
      mutexclpotype : [{potype_constructor,potype_destructor}];
      mutexclpo     : [po_public,po_exports,po_interrupt,po_assembler,po_inline]
    ),(
      idtok:_FAR;
      pd_flags : [pd_implemen,pd_body,pd_interface,pd_procvar,pd_notobject,pd_notobjintf];
      handler  : @pd_far;
      pocall   : pocall_none;
      pooption : [];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_inline]
    ),(
      idtok:_FAR16;
      pd_flags : [pd_interface,pd_implemen,pd_body,pd_procvar,pd_notobject];
      handler  : nil;
      pocall   : pocall_far16;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_FORWARD;
      pd_flags : [pd_implemen,pd_notobject,pd_notobjintf];
      handler  : @pd_forward;
      pocall   : pocall_none;
      pooption : [];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_external,po_inline]
    ),(
      idtok:_OLDFPCCALL;
      pd_flags : [pd_interface,pd_implemen,pd_body,pd_procvar];
      handler  : nil;
      pocall   : pocall_oldfpccall;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [];
      mutexclpo     : []
    ),(
      idtok:_INLINE;
      pd_flags : [pd_interface,pd_implemen,pd_body,pd_notobjintf];
      handler  : nil;
      pocall   : pocall_none;
      pooption : [po_inline];
      mutexclpocall : [];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_exports,po_external,po_interrupt,po_virtualmethod]
    ),(
      idtok:_INTERNCONST;
      pd_flags : [pd_interface,pd_body,pd_notobject,pd_notobjintf];
      handler  : @pd_internconst;
      pocall   : pocall_none;
      pooption : [po_internconst];
      mutexclpocall : [];
      mutexclpotype : [potype_operator];
      mutexclpo     : []
    ),(
      idtok:_INTERNPROC;
      pd_flags : [pd_interface,pd_notobject,pd_notobjintf];
      handler  : @pd_internproc;
      pocall   : pocall_internproc;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [potype_constructor,potype_destructor,potype_operator];
      mutexclpo     : [po_exports,po_external,po_interrupt,po_assembler,po_iocheck,po_virtualmethod]
    ),(
      idtok:_INTERRUPT;
      pd_flags : [pd_implemen,pd_body,pd_notobject,pd_notobjintf];
      handler  : @pd_interrupt;
      pocall   : pocall_oldfpccall;
      pooption : [po_interrupt];
      mutexclpocall : [pocall_internproc,pocall_cdecl,pocall_cppdecl,pocall_stdcall,
                       pocall_pascal,pocall_far16,pocall_oldfpccall];
      mutexclpotype : [potype_constructor,potype_destructor,potype_operator];
      mutexclpo     : [po_external,po_inline]
    ),(
      idtok:_IOCHECK;
      pd_flags : [pd_implemen,pd_body,pd_notobjintf];
      handler  : nil;
      pocall   : pocall_none;
      pooption : [po_iocheck];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_LOCAL;
      pd_flags : [pd_implemen,pd_body];
      handler  : nil;
      pocall   : pocall_none;
      pooption : [po_kylixlocal];
      mutexclpocall : [pocall_internproc,pocall_far16];
      mutexclpotype : [];
      mutexclpo     : [po_external,po_exports]
    ),(
      idtok:_MESSAGE;
      pd_flags : [pd_interface,pd_object,pd_notobjintf];
      handler  : @pd_message;
      pocall   : pocall_none;
      pooption : []; { can be po_msgstr or po_msgint }
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [potype_constructor,potype_destructor,potype_operator];
      mutexclpo     : [po_interrupt,po_external,po_inline]
    ),(
      idtok:_MWPASCAL;
      pd_flags : [pd_interface,pd_implemen,pd_body,pd_procvar];
      handler  : nil;
      pocall   : pocall_mwpascal;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [];
      mutexclpo     : []
    ),(
      idtok:_NEAR;
      pd_flags : [pd_implemen,pd_body,pd_procvar,pd_notobjintf];
      handler  : @pd_near;
      pocall   : pocall_none;
      pooption : [];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : []
    ),(
      idtok:_NOSTACKFRAME;
      pd_flags : [pd_implemen,pd_body,pd_procvar,pd_notobjintf];
      handler  : nil;
      pocall   : pocall_none;
      pooption : [po_nostackframe];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : []
    ),(
      idtok:_OVERLOAD;
      pd_flags : [pd_implemen,pd_interface,pd_body];
      handler  : @pd_overload;
      pocall   : pocall_none;
      pooption : [po_overload];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : []
    ),(
      idtok:_OVERRIDE;
      pd_flags : [pd_interface,pd_object,pd_notobjintf];
      handler  : @pd_override;
      pocall   : pocall_none;
      pooption : [po_overridingmethod,po_virtualmethod];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_exports,po_external,po_interrupt,po_virtualmethod,po_inline]
    ),(
      idtok:_PASCAL;
      pd_flags : [pd_interface,pd_implemen,pd_body,pd_procvar];
      handler  : nil;
      pocall   : pocall_pascal;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_external]
    ),(
      idtok:_PUBLIC;
      pd_flags : [pd_interface,pd_implemen,pd_body,pd_notobject,pd_notobjintf];
      handler  : @pd_public;
      pocall   : pocall_none;
      pooption : [po_public,po_global];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_external,po_inline]
    ),(
      idtok:_REGISTER;
      pd_flags : [pd_interface,pd_implemen,pd_body,pd_procvar];
      handler  : nil;
      pocall   : pocall_register;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_external]
    ),(
      idtok:_REINTRODUCE;
      pd_flags : [pd_interface,pd_object,pd_notobjintf];
      handler  : @pd_reintroduce;
      pocall   : pocall_none;
      pooption : [po_reintroduce];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_external,po_interrupt,po_exports,po_overridingmethod,po_inline]
    ),(
      idtok:_SAFECALL;
      pd_flags : [pd_interface,pd_implemen,pd_body,pd_procvar];
      handler  : nil;
      pocall   : pocall_safecall;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_external]
    ),(
      idtok:_SOFTFLOAT;
      pd_flags : [pd_interface,pd_implemen,pd_body,pd_procvar];
      handler  : nil;
      pocall   : pocall_softfloat;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [potype_constructor,potype_destructor];
      { it's available with po_external because the libgcc floating point routines on the arm
        uses this calling convention }
      mutexclpo     : []
    ),(
      idtok:_STATIC;
      pd_flags : [pd_interface,pd_implemen,pd_body,pd_object,pd_notobjintf];
      handler  : @pd_static;
      pocall   : pocall_none;
      pooption : [po_staticmethod];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_external,po_interrupt,po_exports,po_inline]
    ),(
      idtok:_STDCALL;
      pd_flags : [pd_interface,pd_implemen,pd_body,pd_procvar];
      handler  : nil;
      pocall   : pocall_stdcall;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_external]
    ),(
      idtok:_SYSCALL;
      { Different kind of syscalls are valid for AOS68k, AOSPPC and MOS. }
      { FIX ME!!! MorphOS/AOS68k pd_flags should be:
        pd_interface, pd_implemen, pd_notobject, pd_notobjintf (KB) }
      pd_flags : [pd_interface,pd_implemen,pd_procvar];
      handler  : @pd_syscall;
      pocall   : pocall_syscall;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_external,po_assembler,po_interrupt,po_exports]
    ),(
      idtok:_VIRTUAL;
      pd_flags : [pd_interface,pd_object,pd_notobjintf];
      handler  : @pd_virtual;
      pocall   : pocall_none;
      pooption : [po_virtualmethod];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_external,po_interrupt,po_exports,po_overridingmethod,po_inline]
    ),(
      idtok:_CPPDECL;
      pd_flags : [pd_interface,pd_implemen,pd_body,pd_procvar];
      handler  : nil;
      pocall   : pocall_cppdecl;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_assembler,po_external,po_virtualmethod]
    ),(
      idtok:_VARARGS;
      pd_flags : [pd_interface,pd_implemen,pd_procvar];
      handler  : nil;
      pocall   : pocall_none;
      pooption : [po_varargs];
      mutexclpocall : [pocall_internproc,pocall_stdcall,pocall_register,
                       pocall_far16,pocall_oldfpccall,pocall_mwpascal];
      mutexclpotype : [];
      mutexclpo     : [po_assembler,po_interrupt,po_inline]
    ),(
      idtok:_COMPILERPROC;
      pd_flags : [pd_interface,pd_implemen,pd_body,pd_notobjintf];
      handler  : nil;
      pocall   : pocall_none;
      pooption : [po_compilerproc];
      mutexclpocall : [];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_interrupt]
    ),(
      idtok:_WEAKEXTERNAL;
      pd_flags : [pd_implemen,pd_interface,pd_notobject,pd_notobjintf,pd_cppobject];
      handler  : @pd_weakexternal;
      pocall   : pocall_none;
      { mark it both external and weak external, so we don't have to
        adapt all code for external symbols to also check for weak external
      }
      pooption : [po_external,po_weakexternal];
      mutexclpocall : [pocall_internproc,pocall_syscall];
      { allowed for external cpp classes }
      mutexclpotype : [{potype_constructor,potype_destructor}];
      mutexclpo     : [po_public,po_exports,po_interrupt,po_assembler,po_inline]
    )
   );


    function check_proc_directive(isprocvar:boolean):boolean;
      var
        i : longint;
      begin
        result:=false;
        for i:=1 to num_proc_directives do
         if proc_direcdata[i].idtok=idtoken then
          begin
            if ((not isprocvar) or
               (pd_procvar in proc_direcdata[i].pd_flags)) and
               { don't eat a public directive in classes }
               not((idtoken=_PUBLIC) and (symtablestack.top.symtabletype=ObjectSymtable)) then
              result:=true;
            exit;
          end;
      end;


    function parse_proc_direc(pd:tabstractprocdef;var pdflags:tpdflags):boolean;
      {
        Parse the procedure directive, returns true if a correct directive is found
      }
      var
        p     : longint;
        found : boolean;
        name  : TIDString;
      begin
        parse_proc_direc:=false;
        name:=tokeninfo^[idtoken].str;
        found:=false;

      { Hint directive? Then exit immediatly }
        if (m_hintdirective in current_settings.modeswitches) then
         begin
           case idtoken of
             _LIBRARY,
             _PLATFORM,
             _UNIMPLEMENTED,
             _EXPERIMENTAL,
             _DEPRECATED :
               exit;
           end;
         end;

        { C directive is MAC only, because it breaks too much existing code
          on other platforms (PFV) }
        if (idtoken=_C) and
           not(m_mac in current_settings.modeswitches) then
          exit;

      { retrieve data for directive if found }
        for p:=1 to num_proc_directives do
         if proc_direcdata[p].idtok=idtoken then
          begin
            found:=true;
            break;
          end;

      { Check if the procedure directive is known }
        if not found then
         begin
            { parsing a procvar type the name can be any
              next variable !! }
            if ((pdflags * [pd_procvar,pd_object])=[]) and
               not(idtoken=_PROPERTY) then
              Message1(parser_w_unknown_proc_directive_ignored,name);
            exit;
         end;

        { static needs a special treatment }
        if (idtoken=_STATIC) and not (cs_static_keyword in current_settings.moduleswitches) then
          exit;

        { check if method and directive not for object, like public.
          This needs to be checked also for procvars }
        if (pd_notobject in proc_direcdata[p].pd_flags) and
           (symtablestack.top.symtabletype=ObjectSymtable) and
           { directive allowed for cpp classes? }
           not(is_cppclass(tdef(symtablestack.top.defowner)) and (pd_cppobject in proc_direcdata[p].pd_flags)) then
           exit;

        { Conflicts between directives ? }
        if (pd.proctypeoption in proc_direcdata[p].mutexclpotype) or
           (pd.proccalloption in proc_direcdata[p].mutexclpocall) or
           ((pd.procoptions*proc_direcdata[p].mutexclpo)<>[]) then
         begin
           Message1(parser_e_proc_dir_conflict,name);
           exit;
         end;

        { set calling convention }
        if proc_direcdata[p].pocall<>pocall_none then
         begin
           if (po_hascallingconvention in pd.procoptions) then
            begin
              Message2(parser_w_proc_overriding_calling,
                proccalloptionStr[pd.proccalloption],
                proccalloptionStr[proc_direcdata[p].pocall]);
            end;
           { check if the target processor supports this calling convention }
           if not(proc_direcdata[p].pocall in supported_calling_conventions) then
             begin
               Message1(parser_e_illegal_calling_convention,proccalloptionStr[proc_direcdata[p].pocall]);
               { recover }
               proc_direcdata[p].pocall:=pocall_stdcall;
             end;
           pd.proccalloption:=proc_direcdata[p].pocall;
           include(pd.procoptions,po_hascallingconvention);
         end;

        if pd.typ=procdef then
         begin
           { Check if the directive is only for objects }
           if (pd_object in proc_direcdata[p].pd_flags) and
              not assigned(tprocdef(pd)._class) then
            exit;

           { check if method and directive not for interface }
           if (pd_notobjintf in proc_direcdata[p].pd_flags) and
              is_interface(tprocdef(pd)._class) then
            exit;
           { check if method and directive not for interface }
           if is_dispinterface(tprocdef(pd)._class) and
             not(pd_dispinterface in proc_direcdata[p].pd_flags) then
            exit;
         end;

        { consume directive, and turn flag on }
        consume(token);
        parse_proc_direc:=true;

        { Check the pd_flags if the directive should be allowed }
        if (pd_interface in pdflags) and
           not(pd_interface in proc_direcdata[p].pd_flags) then
          begin
            Message1(parser_e_proc_dir_not_allowed_in_interface,name);
            exit;
          end;
        if (pd_implemen in pdflags) and
           not(pd_implemen in proc_direcdata[p].pd_flags) then
          begin
            Message1(parser_e_proc_dir_not_allowed_in_implementation,name);
            exit;
          end;
        if (pd_procvar in pdflags) and
           not(pd_procvar in proc_direcdata[p].pd_flags) then
          begin
            Message1(parser_e_proc_dir_not_allowed_in_procvar,name);
            exit;
          end;

        { Return the new pd_flags }
        if not(pd_body in proc_direcdata[p].pd_flags) then
          exclude(pdflags,pd_body);

        { Add the correct flag }
        pd.procoptions:=pd.procoptions+proc_direcdata[p].pooption;

        { Call the handler }
        if pointer(proc_direcdata[p].handler)<>nil then
          proc_direcdata[p].handler(pd);
      end;



    function proc_get_importname(pd:tprocdef):string;

       function maybe_cprefix(const s:string):string;
         begin
           if not(pd.proccalloption in [pocall_cdecl,pocall_cppdecl]) then
             result:=s
           else
             result:=target_info.Cprefix+s;
         end;

      begin
        result:='';
        if not(po_external in pd.procoptions) then
          internalerror(200412151);
        { external name or number is specified }
        if assigned(pd.import_name) or (pd.import_nr<>0) then
          begin
            if assigned(pd.import_dll) then
              begin
                { If we are not using direct dll linking under win32 then imports
                  need to use the normal name since two functions can refer to the
                  same DLL function. This is also needed for compatability
                  with Delphi and TP7 }
(*
                case target_info.system of
                  system_i386_emx,
                  system_i386_os2 :
                    begin
                      { keep normal mangledname }
                      if not (Assigned (PD.Import_Name)) then
                       Result := PD.MangledName;
                    end;
                  else
*)
                if assigned(pd.import_name) then
                  begin
                    if target_info.system in (system_all_windows +
                                       [system_i386_emx, system_i386_os2]) then
                   { cprefix is not used in DLL imports under Windows or OS/2 }
                      result:=pd.import_name^
                    else
                      result:=maybe_cprefix(pd.import_name^);
                  end
                else
                  result:=ExtractFileName(pd.import_dll^)+'_index_'+tostr(pd.import_nr);
              end
            else
              result:=maybe_cprefix(pd.import_name^);
          end
        else
          begin
            { Default names when importing variables }
            case pd.proccalloption of
              pocall_cdecl :
                begin
                  if assigned(pd._class) then
                    result:=target_info.Cprefix+pd._class.objrealname^+'_'+pd.procsym.realname
                  else
                    result:=target_info.Cprefix+pd.procsym.realname;
                end;
              pocall_cppdecl :
                begin
                  result:=target_info.Cprefix+pd.cplusplusmangledname;
                end;
              else
                begin
                  {In MacPas a single "external" has the same effect as "external name 'xxx'" }
                  if (m_mac in current_settings.modeswitches) then
                    result:=tprocdef(pd).procsym.realname;
                end;
            end;
          end;
      end;


    procedure proc_set_mangledname(pd:tprocdef);
      var
        s : string;
      begin
        { When the mangledname is already set we aren't allowed to change
          it because it can already be used somewhere (PFV) }
        if not(po_has_mangledname in pd.procoptions) then
          begin
            if (po_external in pd.procoptions) then
              begin
                { External Procedures are only allowed to change the mangledname
                  in their first declaration }
                if (pd.forwarddef or (not pd.hasforward)) then
                  begin
                    s:=proc_get_importname(pd);
                    if s<>'' then
                      begin
                        { Replace ? and @ in import name, since GNU AS does not allow these characters in symbol names. }
                        { This allows to import VC++ mangled names from DLLs. }
                        { Do not perform replacement, if external symbol is not imported from DLL. }
                        if (target_info.system in system_all_windows) and (pd.import_dll<>nil) then
                          begin
                            Replace(s,'?','__q$$');
{$ifdef arm}
                            { @ symbol is not allowed in ARM assembler only }
                            Replace(s,'@','__a$$');
{$endif arm}
                          end;
                        pd.setmangledname(s);
                      end;
                  end;
              end
            else
            { Normal procedures }
              begin
                if (po_compilerproc in pd.procoptions) then
                  begin
                    pd.setmangledname(lower(pd.procsym.name));
                  end;
              end;
          end;

        { Public/exported alias names }
        if (([po_public,po_exports]*pd.procoptions)<>[]) and
           not(po_has_public_name in pd.procoptions) then
          begin
            case pd.proccalloption of
              pocall_cdecl :
                begin
                  if assigned(pd._class) then
                   pd.aliasnames.insert(target_info.Cprefix+pd._class.objrealname^+'_'+pd.procsym.realname)
                  else
                   pd.aliasnames.insert(target_info.Cprefix+pd.procsym.realname);
                end;
              pocall_cppdecl :
                begin
                  pd.aliasnames.insert(target_info.Cprefix+pd.cplusplusmangledname);
                end;
            end;
            { prevent adding the alias a second time }
            include(pd.procoptions,po_has_public_name);
          end;
      end;


    procedure handle_calling_convention(pd:tabstractprocdef);
      begin
        { set the default calling convention if none provided }
        if not(po_hascallingconvention in pd.procoptions) then
          pd.proccalloption:=current_settings.defproccall
        else
          begin
            if pd.proccalloption=pocall_none then
              internalerror(200309081);
          end;

        { handle proccall specific settings }
        case pd.proccalloption of
          pocall_cdecl,
          pocall_cppdecl :
            begin
              { check C cdecl para types }
              check_c_para(pd);
            end;
          pocall_far16 :
            begin
              { Temporary stub, must be rewritten to support OS/2 far16 }
              Message1(parser_w_proc_directive_ignored,'FAR16');
            end;
        end;

        { Inlining is enabled and supported? }
        if (po_inline in pd.procoptions) and
           not(cs_do_inline in current_settings.localswitches) then
          begin
            { Give an error if inline is not supported by the compiler mode,
              otherwise only give a warning that this procedure will not be inlined }
            if not(m_default_inline in current_settings.modeswitches) then
              Message(parser_e_proc_inline_not_supported)
            else
              Message(parser_w_inlining_disabled);
            exclude(pd.procoptions,po_inline);
          end;

        { For varargs directive also cdecl and external must be defined }
        if (po_varargs in pd.procoptions) then
         begin
           { check first for external in the interface, if available there
             then the cdecl must also be there since there is no implementation
             available to contain it }
           if parse_only then
            begin
              { if external is available, then cdecl must also be available,
                procvars don't need external }
              if not((po_external in pd.procoptions) or
                     (pd.typ=procvardef)) and
                 not(pd.proccalloption in [pocall_cdecl,pocall_cppdecl,pocall_mwpascal]) then
                Message(parser_e_varargs_need_cdecl_and_external);
            end
           else
            begin
              { both must be defined now }
              if not((po_external in pd.procoptions) or
                     (pd.typ=procvardef)) or
                 not(pd.proccalloption in [pocall_cdecl,pocall_cppdecl,pocall_mwpascal]) then
                Message(parser_e_varargs_need_cdecl_and_external);
            end;
         end;

        { insert hidden high parameters }
        pd.parast.SymList.ForEachCall(@insert_hidden_para,pd);

        { insert hidden self parameter }
        insert_self_and_vmt_para(pd);

        { insert funcret parameter if required }
        insert_funcret_para(pd);

        { Make var parameters regable, this must be done after the calling
          convention is set. }
        { this must be done before parentfp is insert, because getting all cases
          where parentfp must be in a memory location isn't catched properly so
          we put parentfp never in a register }
        pd.parast.SymList.ForEachCall(@set_addr_param_regable,pd);

        { insert parentfp parameter if required }
        insert_parentfp_para(pd);

        { Calculate parameter tlist }
        pd.calcparas;
      end;


    procedure parse_proc_directives(pd:tabstractprocdef;var pdflags:tpdflags);
      {
        Parse the procedure directives. It does not matter if procedure directives
        are written using ;procdir; or ['procdir'] syntax.
      }
      var
        res : boolean;
      begin
        if (m_mac in current_settings.modeswitches) and (cs_externally_visible in current_settings.localswitches) then
          begin
            tprocdef(pd).aliasnames.insert(tprocdef(pd).procsym.realname);
            include(pd.procoptions,po_public);
            include(pd.procoptions,po_has_public_name);
            include(pd.procoptions,po_global);
          end;

        while token in [_ID,_LECKKLAMMER] do
         begin
           if try_to_consume(_LECKKLAMMER) then
            begin
              repeat
                parse_proc_direc(pd,pdflags);
              until not try_to_consume(_COMMA);
              consume(_RECKKLAMMER);
              { we always expect at least '[];' }
              res:=true;
            end
           else
            begin
              res:=parse_proc_direc(pd,pdflags);
            end;
           { A procedure directive normally followed by a semicolon, but in
             a const section or reading a type we should stop when _EQUAL is found,
             because a constant/default value follows }
           if res then
            begin
              if (block_type=bt_const_type) and
                 (token=_EQUAL) then
               break;
              { support procedure proc;stdcall export; }
              if not(check_proc_directive((pd.typ=procvardef))) then
                begin
                  { support "record p : procedure stdcall end;" and
                    "var p : procedure stdcall = nil;" }
                  if (pd_procvar in pdflags) and
                     (token in [_END,_RKLAMMER,_EQUAL]) then
                    break
                  else
                    begin
                      if (token=_COLON) then
                        begin
                          Message(parser_e_field_not_allowed_here);
                          consume_all_until(_SEMICOLON);
                        end;
                      consume(_SEMICOLON)
                    end;
                end;
            end
           else
            break;
         end;
      end;


    procedure parse_var_proc_directives(sym:tsym);
      var
        pdflags : tpdflags;
        pd      : tabstractprocdef;
      begin
        pdflags:=[pd_procvar];
        pd:=nil;
        case sym.typ of
          fieldvarsym,
          staticvarsym,
          localvarsym,
          paravarsym :
            pd:=tabstractprocdef(tabstractvarsym(sym).vardef);
          typesym :
            pd:=tabstractprocdef(ttypesym(sym).typedef);
          else
            internalerror(2003042617);
        end;
        if pd.typ<>procvardef then
          internalerror(2003042618);
        { names should never be used anyway }
        parse_proc_directives(pd,pdflags);
      end;


    procedure parse_object_proc_directives(pd:tabstractprocdef);
      var
        pdflags : tpdflags;
      begin
        pdflags:=[pd_object];
        parse_proc_directives(pd,pdflags);
      end;


    function proc_add_definition(var currpd:tprocdef):boolean;
      {
        Add definition aprocdef to the overloaded definitions of aprocsym. If a
        forwarddef is found and reused it returns true
      }
      var
        fwpd    : tprocdef;
        currparasym,
        fwparasym : tsym;
        currparacnt,
        fwparacnt,
        curridx,
        fwidx,
        i       : longint;
        po_comp : tprocoptions;
        paracompopt: tcompare_paras_options;
        forwardfound : boolean;
      begin
        forwardfound:=false;

        { check overloaded functions if the same function already exists }
        for i:=0 to tprocsym(currpd.procsym).ProcdefList.Count-1 do
         begin
           fwpd:=tprocdef(tprocsym(currpd.procsym).ProcdefList[i]);

           { Skip overloaded definitions that are declared in other units }
           if fwpd.procsym<>currpd.procsym then
             continue;

           { check the parameters, for delphi/tp it is possible to
             leave the parameters away in the implementation (forwarddef=false).
             But for an overload declared function this is not allowed }
           if { check if empty implementation arguments match is allowed }
              (
               not(m_repeat_forward in current_settings.modeswitches) and
               not(currpd.forwarddef) and
               is_bareprocdef(currpd) and
               not(po_overload in fwpd.procoptions)
              ) or
              { check arguments, we need to check only the user visible parameters. The hidden parameters
                can be in a different location because of the calling convention, eg. L-R vs. R-L order (PFV) }
              (
               (compare_paras(currpd.paras,fwpd.paras,cp_none,[cpo_comparedefaultvalue,cpo_ignorehidden,cpo_openequalisexact])=te_exact) and
               (fwpd.returndef=currpd.returndef)
              ) then
             begin
               { Check if we've found the forwarddef, if found then
                 we need to update the forward def with the current
                 implementation settings }
               if fwpd.forwarddef then
                 begin
                   forwardfound:=true;

                   if not(m_repeat_forward in current_settings.modeswitches) and
                      (fwpd.proccalloption<>currpd.proccalloption) then
                     paracompopt:=[cpo_ignorehidden,cpo_comparedefaultvalue,cpo_openequalisexact]
                   else
                     paracompopt:=[cpo_comparedefaultvalue,cpo_openequalisexact];

                   { Check calling convention }
                   if (fwpd.proccalloption<>currpd.proccalloption) then
                    begin
                      { In delphi it is possible to specify the calling
                        convention in the interface or implementation if
                        there was no convention specified in the other
                        part }
                      if (m_delphi in current_settings.modeswitches) then
                        begin
                          if not(po_hascallingconvention in currpd.procoptions) then
                            currpd.proccalloption:=fwpd.proccalloption
                          else
                            if not(po_hascallingconvention in fwpd.procoptions) then
                              fwpd.proccalloption:=currpd.proccalloption
                          else
                            begin
                              MessagePos(currpd.fileinfo,parser_e_call_convention_dont_match_forward);
                              tprocsym(currpd.procsym).write_parameter_lists(currpd);
                              { restore interface settings }
                              currpd.proccalloption:=fwpd.proccalloption;
                            end;
                        end
                      else
                        begin
                          MessagePos(currpd.fileinfo,parser_e_call_convention_dont_match_forward);
                          tprocsym(currpd.procsym).write_parameter_lists(currpd);
                          { restore interface settings }
                          currpd.proccalloption:=fwpd.proccalloption;
                        end;
                    end;

                   { Check if the procedure type and return type are correct,
                     also the parameters must match also with the type }
                   if ((m_repeat_forward in current_settings.modeswitches) or
                       not is_bareprocdef(currpd)) and
                      ((compare_paras(currpd.paras,fwpd.paras,cp_all,paracompopt)<>te_exact) or
                       (fwpd.returndef<>currpd.returndef)) then
                     begin
                       MessagePos1(currpd.fileinfo,parser_e_header_dont_match_forward,
                                   fwpd.fullprocname(false));
                       tprocsym(currpd.procsym).write_parameter_lists(currpd);
                       break;
                     end;

                   { Check if both are declared forward }
                   if fwpd.forwarddef and currpd.forwarddef then
                    begin
                      MessagePos1(currpd.fileinfo,parser_e_function_already_declared_public_forward,
                                  currpd.fullprocname(false));
                    end;

                   { internconst or internproc only need to be defined once }
                   if (fwpd.proccalloption=pocall_internproc) then
                    currpd.proccalloption:=fwpd.proccalloption
                   else
                    if (currpd.proccalloption=pocall_internproc) then
                     fwpd.proccalloption:=currpd.proccalloption;

                   { Check procedure options, Delphi requires that class is
                     repeated in the implementation for class methods }
                   if (m_fpc in current_settings.modeswitches) then
                     po_comp:=[po_classmethod,po_varargs,po_methodpointer,po_interrupt]
                   else
                     po_comp:=[po_classmethod,po_methodpointer];

                   if ((po_comp * fwpd.procoptions)<>(po_comp * currpd.procoptions)) or
                      (fwpd.proctypeoption <> currpd.proctypeoption) then
                     begin
                       MessagePos1(currpd.fileinfo,parser_e_header_dont_match_forward,
                                   fwpd.fullprocname(false));
                       tprocsym(fwpd.procsym).write_parameter_lists(fwpd);
                       { This error is non-fatal, we can recover }
                     end;

                   { Forward declaration is external? }
                   if (po_external in fwpd.procoptions) then
                     MessagePos(currpd.fileinfo,parser_e_proc_already_external);

                   { Check parameters }
                   if (m_repeat_forward in current_settings.modeswitches) or
                      (currpd.minparacount>0) then
                    begin
                      { If mangled names are equal then they have the same amount of arguments }
                      { We can check the names of the arguments }
                      { both symtables are in the same order from left to right }
                      curridx:=0;
                      fwidx:=0;
                      currparacnt:=currpd.parast.SymList.Count;
                      fwparacnt:=fwpd.parast.SymList.Count;
                      repeat
                        { skip default parameter constsyms }
                        while (curridx<currparacnt) and
                              (tsym(currpd.parast.SymList[curridx]).typ<>paravarsym) do
                          inc(curridx);
                        while (fwidx<fwparacnt) and
                              (tsym(fwpd.parast.SymList[fwidx]).typ<>paravarsym) do
                          inc(fwidx);
                        { stop when one of the two lists is at the end }
                        if (fwidx>=fwparacnt) or (curridx>=currparacnt) then
                          break;
                        { compare names of parameters, ignore implictly
                          renamed parameters }
                        currparasym:=tsym(currpd.parast.SymList[curridx]);
                        fwparasym:=tsym(fwpd.parast.SymList[fwidx]);
                        if not(sp_implicitrename in currparasym.symoptions) and
                           not(sp_implicitrename in fwparasym.symoptions) then
                          begin
                            if (currparasym.name<>fwparasym.name) then
                              begin
                                MessagePos3(currpd.fileinfo,parser_e_header_different_var_names,
                                            tprocsym(currpd.procsym).realname,fwparasym.realname,currparasym.realname);
                                break;
                              end;
                          end;
                        { next parameter }
                        inc(curridx);
                        inc(fwidx);
                      until false;
                    end;
                   { Everything is checked, now we can update the forward declaration
                     with the new data from the implementation }
                   fwpd.forwarddef:=currpd.forwarddef;
                   fwpd.hasforward:=true;
                   fwpd.procoptions:=fwpd.procoptions+currpd.procoptions;

                   { marked as local but exported from unit? }
                   if (po_kylixlocal in fwpd.procoptions) and (fwpd.owner.symtabletype=globalsymtable) then
                     MessagePos(fwpd.fileinfo,type_e_cant_export_local);

                   if fwpd.extnumber=$ffff then
                     fwpd.extnumber:=currpd.extnumber;
                   while not currpd.aliasnames.empty do
                     fwpd.aliasnames.insert(currpd.aliasnames.getfirst);
                   { update fileinfo so position references the implementation,
                     also update funcretsym if it is already generated }
                   fwpd.fileinfo:=currpd.fileinfo;
                   if assigned(fwpd.funcretsym) then
                     fwpd.funcretsym.fileinfo:=currpd.fileinfo;
                   { import names }
                   if assigned(currpd.import_dll) then
                     begin
                       stringdispose(fwpd.import_dll);
                       fwpd.import_dll:=stringdup(currpd.import_dll^);
                     end;
                   if assigned(currpd.import_name) then
                     begin
                       stringdispose(fwpd.import_name);
                       fwpd.import_name:=stringdup(currpd.import_name^);
                     end;
                   fwpd.import_nr:=currpd.import_nr;
                   { for compilerproc defines we need to rename and update the
                     symbolname to lowercase }
                   if (po_compilerproc in fwpd.procoptions) then
                    begin
                      { rename to lowercase so users can't access it }
                      fwpd.procsym.realname:='$'+lower(fwpd.procsym.name);
                      { the mangeled name is already changed by the pd_compilerproc }
                      { handler. It must be done immediately because if we have a   }
                      { call to a compilerproc before it's implementation is        }
                      { encountered, it must already use the new mangled name (JM)  }
                    end;

                   { Release current procdef }
                   currpd.owner.deletedef(currpd);
                   currpd:=fwpd;
                 end
               else
                begin
                  { abstract methods aren't forward defined, but this }
                  { needs another error message                   }
                  if (po_abstractmethod in fwpd.procoptions) then
                    MessagePos(currpd.fileinfo,parser_e_abstract_no_definition)
                  else
                    begin
                      MessagePos(currpd.fileinfo,parser_e_overloaded_have_same_parameters);
                      tprocsym(currpd.procsym).write_parameter_lists(currpd);
                    end;
                 end;

               { we found one proc with the same arguments, there are no others
                 so we can stop }
               break;
             end;

           { check for allowing overload directive }
           if not(m_fpc in current_settings.modeswitches) then
            begin
              { overload directive turns on overloading }
              if ((po_overload in currpd.procoptions) or
                  (po_overload in fwpd.procoptions)) then
               begin
                 { check if all procs have overloading, but not if the proc is a method or
                   already declared forward, then the check is already done }
                 if not(fwpd.hasforward or
                        assigned(currpd._class) or
                        (currpd.forwarddef<>fwpd.forwarddef) or
                        ((po_overload in currpd.procoptions) and
                         (po_overload in fwpd.procoptions))) then
                  begin
                    MessagePos1(currpd.fileinfo,parser_e_no_overload_for_all_procs,currpd.procsym.realname);
                    break;
                  end;
               end
              else
               begin
                 if not(fwpd.forwarddef) then
                  begin
                    if (m_tp7 in current_settings.modeswitches) then
                      MessagePos(currpd.fileinfo,parser_e_procedure_overloading_is_off)
                    else
                      MessagePos1(currpd.fileinfo,parser_e_no_overload_for_all_procs,currpd.procsym.realname);
                    break;
                  end;
               end;
            end; { equal arguments }
         end;

        { if we didn't reuse a forwarddef then we add the procdef to the overloaded
          list }
        if not forwardfound then
          begin
            { can happen in Delphi mode }
            if (currpd.proctypeoption = potype_function) and
               is_void(currpd.returndef) then
              MessagePos1(currpd.fileinfo,parser_e_no_funcret_specified,currpd.procsym.realname);
            tprocsym(currpd.procsym).ProcdefList.Add(currpd);
          end;

        proc_add_definition:=forwardfound;
      end;

end.
