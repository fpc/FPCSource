{
    $Id$
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
        pd_body,       { directive needs a body }
        pd_implemen,   { directive can be used implementation section }
        pd_interface,  { directive can be used interface section }
        pd_object,     { directive can be used object declaration }
        pd_procvar,    { directive can be used procvar declaration }
        pd_notobject,  { directive can not be used object declaration }
        pd_notobjintf, { directive can not be used interface declaration }
        pd_notprocvar  { directive can not be used procvar declaration }
      );
      tpdflags=set of tpdflag;

    function is_proc_directive(tok:ttoken;isprocvar:boolean):boolean;

    procedure calc_parast(pd:tabstractprocdef);

    procedure insert_funcret_local(pd:tprocdef);

    function  proc_add_definition(var pd:tprocdef):boolean;

    procedure handle_calling_convention(pd:tabstractprocdef);

    procedure parse_parameter_dec(pd:tabstractprocdef);
    procedure parse_proc_directives(pd:tabstractprocdef;var pdflags:tpdflags);
    procedure parse_var_proc_directives(sym:tsym);
    procedure parse_object_proc_directives(pd:tabstractprocdef);
    function  parse_proc_head(aclass:tobjectdef;potype:tproctypeoption):tprocdef;
    function  parse_proc_dec(aclass:tobjectdef):tprocdef;



implementation

    uses
{$ifdef delphi}
       sysutils,
{$else delphi}
       strings,
{$endif delphi}
       { common }
       cutils,cclasses,
       { global }
       globtype,globals,verbose,
       systems,cpubase,
       { aasm }
       aasmbase,aasmtai,aasmcpu,
       { symtable }
       symbase,symtable,defutil,defcmp,paramgr,
       { pass 1 }
       node,htypechk,
       nmat,nadd,ncal,nset,ncnv,ninl,ncon,nld,nflw,
       { parser }
       fmodule,scanner,
       pbase,pexpr,ptype,pdecl,
       { linking }
       import,gendef,
       { codegen }
       cpuinfo,cgbase
       ;

    const
      { Please leave this here, this module should NOT use
        these variables.
        Declaring it as string here results in an error when compiling (PFV) }
      current_procinfo = 'error';


    procedure insert_funcret_para(pd:tabstractprocdef);
      var
        storepos : tfileposinfo;
        vs       : tvarsym;
      begin
        if not is_void(pd.rettype.def) and
           paramanager.ret_in_param(pd.rettype.def,pd.proccalloption) then
         begin
           storepos:=akttokenpos;
           if pd.deftype=procdef then
            akttokenpos:=tprocdef(pd).fileinfo;

           { Generate result variable accessing function result }
           vs:=tvarsym.create('$result',vs_var,pd.rettype);
           include(vs.varoptions,vo_is_funcret);
           include(vs.varoptions,vo_regable);
           pd.parast.insert(vs);
           pd.insertpara(vs.vartype,vs,nil,true);
           { Store the this symbol as funcretsym for procedures }
           if pd.deftype=procdef then
            tprocdef(pd).funcretsym:=vs;

           akttokenpos:=storepos;
         end;
      end;


    procedure insert_parentfp_para(pd:tabstractprocdef);
      var
        storepos : tfileposinfo;
        vs       : tvarsym;
      begin
        if pd.parast.symtablelevel>normal_function_level then
          begin
            storepos:=akttokenpos;
            if pd.deftype=procdef then
             akttokenpos:=tprocdef(pd).fileinfo;

            { Generate result variable accessing function result }
            vs:=tvarsym.create('$parentfp',vs_var,pd.rettype);
            include(vs.varoptions,vo_is_parentfp);
            pd.parast.insert(vs);
            pd.insertpara(vs.vartype,vs,nil,true);

            akttokenpos:=storepos;
          end;
      end;


    procedure insert_self_and_vmt_para(pd:tabstractprocdef);
      var
        storepos : tfileposinfo;
        vs       : tvarsym;
        tt       : ttype;
        vsp      : tvarspez;
      begin
        if (pd.deftype=procvardef) and
           pd.is_methodpointer then
          begin
            { Generate self variable }
            tt:=voidpointertype;
            vs:=tvarsym.create('$self',vs_value,tt);
            include(vs.varoptions,vo_is_self);
            { Insert as hidden parameter }
            pd.parast.insert(vs);
            pd.insertpara(vs.vartype,vs,nil,true);
          end
        else
          begin
             if (pd.deftype=procdef) and
                assigned(tprocdef(pd)._class) and
                (pd.parast.symtablelevel=normal_function_level) then
              begin
                storepos:=akttokenpos;
                akttokenpos:=tprocdef(pd).fileinfo;

                { Generate VMT variable for constructor/destructor }
                if pd.proctypeoption in [potype_constructor,potype_destructor] then
                 begin
                   { can't use classrefdef as type because inheriting
                     will then always file because of a type mismatch }
                   tt:=voidpointertype;
                   vs:=tvarsym.create('$vmt',vs_value,tt);
                   include(vs.varoptions,vo_is_vmt);
                   { Insert as hidden parameter }
                   pd.parast.insert(vs);
                   pd.insertpara(vs.vartype,vs,nil,true);
                 end;

                { Generate self variable, for classes we need
                  to use the generic voidpointer to be compatible with
                  methodpointers }
                vsp:=vs_value;
                if (po_staticmethod in pd.procoptions) or
                   (po_classmethod in pd.procoptions) then
                  begin
                    tt.setdef(tprocdef(pd)._class);
                    tt.setdef(tclassrefdef.create(tt));
                  end
                else
                  begin
                    if is_object(tprocdef(pd)._class) then
                      vsp:=vs_var;
                    tt.setdef(tprocdef(pd)._class);
                  end;
                vs:=tvarsym.create('$self',vsp,tt);
                include(vs.varoptions,vo_is_self);
                include(vs.varoptions,vo_regable);
                { Insert as hidden parameter }
                pd.parast.insert(vs);
                pd.insertpara(vs.vartype,vs,nil,true);

                akttokenpos:=storepos;
              end;
          end;
      end;


    procedure insert_funcret_local(pd:tprocdef);
      var
        storepos : tfileposinfo;
        vs       : tvarsym;
      begin
        if not is_void(pd.rettype.def) then
         begin
           storepos:=akttokenpos;
           akttokenpos:=pd.fileinfo;

           { We always need a localsymtable }
           if not assigned(pd.localst) then
            pd.insert_localst;

           { We need to insert a varsym for the result in the localst
             when it is returning in a register }
           if not paramanager.ret_in_param(pd.rettype.def,pd.proccalloption) then
            begin
              vs:=tvarsym.create('$result',vs_value,pd.rettype);
              include(vs.varoptions,vo_is_funcret);
              if tstoreddef(pd.rettype.def).is_intregable then
                include(vs.varoptions,vo_regable);
              if tstoreddef(pd.rettype.def).is_fpuregable then
                include(vs.varoptions,vo_fpuregable);
              pd.localst.insert(vs);
              pd.funcretsym:=vs;
            end;

           { insert the name of the procedure as alias for the function result,
             we can't use realname because that will not work for compilerprocs
             as the name is lowercase and unreachable from the code }
           if pd.resultname='' then
            pd.resultname:=pd.procsym.name;
           vs:=tabsolutesym.create_ref(pd.resultname,pd.rettype,tstoredsym(pd.funcretsym));
           include(vs.varoptions,vo_is_funcret);
           pd.localst.insert(vs);

           { insert result also if support is on }
           if (m_result in aktmodeswitches) then
            begin
              vs:=tabsolutesym.create_ref('RESULT',pd.rettype,tstoredsym(pd.funcretsym));
              include(vs.varoptions,vo_is_funcret);
              include(vs.varoptions,vo_is_result);
              pd.localst.insert(vs);
            end;

           akttokenpos:=storepos;
         end;
      end;


    procedure insert_hidden_para(pd:tabstractprocdef);
      var
        currpara : tparaitem;
        hvs : tvarsym;
      begin
        { walk from right to left, so we can insert the
          high parameters after the current parameter }
        currpara:=tparaitem(pd.para.last);
        while assigned(currpara) do
         begin
           { needs high parameter ? }
           if paramanager.push_high_param(currpara.paratyp,currpara.paratype.def,pd.proccalloption) then
            begin
              if assigned(currpara.parasym) then
               begin
                 hvs:=tvarsym.create('$high'+tvarsym(currpara.parasym).name,vs_const,s32bittype);
                 include(hvs.varoptions,vo_is_high_value);
                 tvarsym(currpara.parasym).owner.insert(hvs);
                 tvarsym(currpara.parasym).highvarsym:=hvs;
               end
              else
               hvs:=nil;
              pd.concatpara(currpara,s32bittype,hvs,nil,true);
            end
           else
            begin
              { Give a warning that cdecl routines does not include high()
                support }
              if (pd.proccalloption in [pocall_cdecl,pocall_cppdecl]) and
                 paramanager.push_high_param(currpara.paratyp,currpara.paratype.def,pocall_default) then
               begin
                 if is_open_string(currpara.paratype.def) then
                    Message(parser_w_cdecl_no_openstring);
                 if not (po_external in pd.procoptions) then
                   Message(parser_w_cdecl_has_no_high);
               end;
            end;
           currpara:=tparaitem(currpara.previous);
         end;
      end;


    procedure check_c_para(p:tnamedindexitem;arg:pointer);
      begin
        if (tsym(p).typ<>varsym) then
         exit;
        with tvarsym(p) do
         begin
           case vartype.def.deftype of
             arraydef :
               begin
                 if not is_variant_array(vartype.def) and
                    not is_array_of_const(vartype.def) then
                  begin
                    if (varspez<>vs_var) then
                      Message(parser_h_c_arrays_are_references);
                  end;
                 if is_array_of_const(vartype.def) and
                    assigned(indexnext) and
                    (tsym(indexnext).typ=varsym) and
                    not(vo_is_high_value in tvarsym(indexnext).varoptions) then
                   Message(parser_e_C_array_of_const_must_be_last);
               end;
            end;
         end;
      end;


    procedure parse_parameter_dec(pd:tabstractprocdef);
      {
        handle_procvar needs the same changes
      }
      var
        is_procvar : boolean;
        sc      : tsinglelist;
        tt      : ttype;
        arrayelementtype : ttype;
        vs      : tvarsym;
        srsym   : tsym;
        hs1 : string;
        varspez : Tvarspez;
        tdefaultvalue : tconstsym;
        defaultrequired : boolean;
        old_object_option : tsymoptions;
        currparast : tparasymtable;
      begin
        consume(_LKLAMMER);
        { Delphi/Kylix supports nonsense like }
        { procedure p();                      }
        if try_to_consume(_RKLAMMER) and
          not(m_tp7 in aktmodeswitches) then
          exit;
        { parsing a proc or procvar ? }
        is_procvar:=(pd.deftype=procvardef);
        currparast:=tparasymtable(pd.parast);
        { reset }
        sc:=tsinglelist.create;
        defaultrequired:=false;
        { the variables are always public }
        old_object_option:=current_object_option;
        current_object_option:=[sp_public];
        inc(testcurobject);
        repeat
          if try_to_consume(_VAR) then
            varspez:=vs_var
          else
            if try_to_consume(_CONST) then
              varspez:=vs_const
          else
            if (idtoken=_OUT) and (m_out in aktmodeswitches) then
              begin
                 consume(_OUT);
                 varspez:=vs_out
              end
          else
              varspez:=vs_value;
          tdefaultvalue:=nil;
          tt.reset;
          { read identifiers and insert with error type }
          sc.reset;
          repeat
            vs:=tvarsym.create(orgpattern,varspez,generrortype);
            currparast.insert(vs);
            if assigned(vs.owner) then
             sc.insert(vs)
            else
             vs.free;
            consume(_ID);
          until not try_to_consume(_COMMA);
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
                tt.setdef(tarraydef.create(0,-1,s32bittype));
                { array of const ? }
                if (token=_CONST) and (m_objpas in aktmodeswitches) then
                 begin
                   consume(_CONST);
                   srsym:=searchsymonlyin(systemunit,'TVARREC');
                   if not assigned(srsym) then
                    InternalError(1234124);
                   tarraydef(tt.def).setelementtype(ttypesym(srsym).restype);
                   tarraydef(tt.def).IsArrayOfConst:=true;
                 end
                else
                 begin
                   { define field type }
                   single_type(arrayelementtype,hs1,false);
                   tarraydef(tt.def).setelementtype(arrayelementtype);
                 end;
              end
             else
              begin
                { open string ? }
                if (varspez=vs_var) and
                        (
                          (
                            ((token=_STRING) or (idtoken=_SHORTSTRING)) and
                            (cs_openstring in aktmoduleswitches) and
                            not(cs_ansistrings in aktlocalswitches)
                          ) or
                        (idtoken=_OPENSTRING)) then
                 begin
                   consume(token);
                   tt:=openshortstringtype;
                   hs1:='openstring';
                 end
                else
                 begin
                   { everything else }
                   single_type(tt,hs1,false);
                 end;

                { default parameter }
                if (m_default_para in aktmodeswitches) then
                 begin
                   if try_to_consume(_EQUAL) then
                    begin
                      vs:=tvarsym(sc.first);
                      if assigned(vs.listnext) then
                        Message(parser_e_default_value_only_one_para);
                      { prefix 'def' to the parameter name }
                      tdefaultvalue:=ReadConstant('$def'+vs.name,vs.fileinfo);
                      if assigned(tdefaultvalue) then
                       tprocdef(pd).parast.insert(tdefaultvalue);
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
           begin
{$ifndef UseNiceNames}
             hs1:='$$$';
{$else UseNiceNames}
             hs1:='var';
{$endif UseNiceNames}
             tt:=cformaltype;
           end;

          { File types are only allowed for var parameters }
          if (tt.def.deftype=filedef) and
             (varspez<>vs_var) then
            CGMessage(cg_e_file_must_call_by_reference);

          vs:=tvarsym(sc.first);
          while assigned(vs) do
           begin
             { update varsym }
             vs.vartype:=tt;
             { For proc vars we only need the definitions }
             if not is_procvar then
              begin
                if (varspez in [vs_var,vs_const,vs_out]) and
                   paramanager.push_addr_param(varspez,tt.def,pd.proccalloption) then
                  include(vs.varoptions,vo_regable);
              end;
             pd.concatpara(nil,tt,vs,tdefaultvalue,false);
             vs:=tvarsym(vs.listnext);
           end;
        until not try_to_consume(_SEMICOLON);
        { remove parasymtable from stack }
        sc.free;
        { reset object options }
        dec(testcurobject);
        current_object_option:=old_object_option;
        consume(_RKLAMMER);
      end;


    function parse_proc_head(aclass:tobjectdef;potype:tproctypeoption):tprocdef;
      var
        orgsp,sp : stringid;
        sym : tsym;
        srsymtable : tsymtable;
        storepos,
        procstartfilepos : tfileposinfo;
        searchagain : boolean;
        i : longint;
        st : tsymtable;
        pd : tprocdef;
        aprocsym : tprocsym;
      begin
        { Save the position where this procedure really starts }
        procstartfilepos:=akttokenpos;

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
           assigned(aclass.implementedinterfaces) and
           (aclass.implementedinterfaces.count>0) and
           try_to_consume(_POINT) then
         begin
           storepos:=akttokenpos;
           akttokenpos:=procstartfilepos;
           { get interface syms}
           searchsym(sp,sym,srsymtable);
           if not assigned(sym) then
            begin
              identifier_not_found(orgsp);
              sym:=generrorsym;
            end;
           akttokenpos:=storepos;
           { qualifier is interface? }
           if (sym.typ=typesym) and
              (ttypesym(sym).restype.def.deftype=objectdef) then
             i:=aclass.implementedinterfaces.searchintf(ttypesym(sym).restype.def)
           else
             i:=-1;
           if (i=-1) then
             Message(parser_e_interface_id_expected);
           consume(_ID);
           consume(_EQUAL);
           if (token=_ID) then
             aclass.implementedinterfaces.addmappings(i,sp,pattern);
           consume(_ID);
           result:=nil;
           exit;
         end;

        { method  ? }
        if not assigned(aclass) and
           (potype<>potype_operator) and
           (symtablestack.symtablelevel=main_program_level) and
           try_to_consume(_POINT) then
         begin
           { search for object name }
           storepos:=akttokenpos;
           akttokenpos:=procstartfilepos;
           searchsym(sp,sym,srsymtable);
           if not assigned(sym) then
            begin
              identifier_not_found(orgsp);
              sym:=generrorsym;
            end;
           akttokenpos:=storepos;
           { consume proc name }
           sp:=pattern;
           orgsp:=orgpattern;
           procstartfilepos:=akttokenpos;
           consume(_ID);
           { qualifier is class name ? }
           if (sym.typ=typesym) and
              (ttypesym(sym).restype.def.deftype=objectdef) then
            begin
              aclass:=tobjectdef(ttypesym(sym).restype.def);
              aprocsym:=tprocsym(aclass.symtable.search(sp));
              { we solve this below }
              if assigned(aprocsym) then
               begin
                 if aprocsym.typ<>procsym then
                  begin
                    {  we use a different error message for tp7 so it looks more compatible }
                    if (m_fpc in aktmodeswitches) then
                      Message1(parser_e_overloaded_no_procedure,aprocsym.realname)
                    else
                      Message(parser_e_methode_id_expected);
                    { rename the name to an unique name to avoid an
                      error when inserting the symbol in the symtable }
                    orgsp:=orgsp+'$'+tostr(aktfilepos.line);
                    aprocsym:=nil;
                  end;
               end
              else
               Message(parser_e_methode_id_expected);
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
             akttokenpos:=procstartfilepos;
             aprocsym:=tprocsym(symtablestack.search(sp));

             if not(parse_only) and
                not assigned(aprocsym) and
                (symtablestack.symtabletype=staticsymtable) and
                assigned(symtablestack.next) and
                (symtablestack.next.unitid=0) then
               begin
                 { The procedure we prepare for is in the implementation
                   part of the unit we compile. It is also possible that we
                   are compiling a program, which is also some kind of
                   implementaion part.

                   We need to find out if the procedure is global. If it is
                   global, it is in the global symtable.}
                 aprocsym:=tprocsym(symtablestack.next.search(sp));
               end;

             { Check if overloaded is a procsym }
             if assigned(aprocsym) and
                (aprocsym.typ<>procsym) then
              begin
                { when the other symbol is a unit symbol then hide the unit
                  symbol. Only in tp mode because it's bad programming }
                if (m_duplicate_names in aktmodeswitches) and
                   (aprocsym.typ=unitsym) then
                 begin
                   aprocsym.owner.rename(aprocsym.name,'hidden'+aprocsym.name);
                   searchagain:=true;
                 end
                else
                 begin
                   {  we use a different error message for tp7 so it looks more compatible }
                   if (m_fpc in aktmodeswitches) then
                    Message1(parser_e_overloaded_no_procedure,aprocsym.realname)
                   else
                    DuplicateSym(aprocsym);
                   { rename the name to an unique name to avoid an
                     error when inserting the symbol in the symtable }
                   orgsp:=orgsp+'$'+tostr(aktfilepos.line);
                   { generate a new aktprocsym }
                   aprocsym:=nil;
                 end;
              end;
           until not searchagain;
         end;

        { test again if assigned, it can be reset to recover }
        if not assigned(aprocsym) then
         begin
           { create a new procsym and set the real filepos }
           akttokenpos:=procstartfilepos;
           { for operator we have only one procsym for each overloaded
             operation }
           if (potype=potype_operator) then
             begin
               { is the current overload sym already in the current unit }
               if assigned(overloaded_operators[optoken]) and
                  (overloaded_operators[optoken].owner=symtablestack) then
                 aprocsym:=overloaded_operators[optoken]
               else
                 begin
                   { create the procsym with saving the original case }
                   aprocsym:=tprocsym.create('$'+sp);
                   if assigned(overloaded_operators[optoken]) then
                     overloaded_operators[optoken].concat_procdefs_to(aprocsym);
                   overloaded_operators[optoken]:=aprocsym;
                 end;
             end
            else
             aprocsym:=tprocsym.create(orgsp);
            symtablestack.insert(aprocsym);
         end;

        { to get the correct symtablelevel we must ignore objectsymtables }
        st:=symtablestack;
        while not(st.symtabletype in [staticsymtable,globalsymtable,localsymtable]) do
         st:=st.next;
        pd:=tprocdef.create(st.symtablelevel+1);
        pd._class:=aclass;
        pd.procsym:=aprocsym;
        pd.proctypeoption:=potype;
        { methods need to be exported }
        if assigned(aclass) and
           (
            (symtablestack.symtabletype=objectsymtable) or
            (symtablestack.symtablelevel=main_program_level)
           ) then
          include(pd.procoptions,po_public);

        { symbol options that need to be kept per procdef }
        pd.fileinfo:=procstartfilepos;
        pd.symoptions:=current_object_option;

        { parse parameters }
        if token=_LKLAMMER then
          parse_parameter_dec(pd);

        { return created tprocdef }
        result:=pd;
      end;


    function parse_proc_dec(aclass:tobjectdef):tprocdef;
      var
        pd : tprocdef;
        hs : string;
        isclassmethod : boolean;
      begin
        pd:=nil;
        { read class method }
        if try_to_consume(_CLASS) then
         begin
           { class method only allowed for procedures and functions }
           if not(token in [_FUNCTION,_PROCEDURE]) then
             Message(parser_e_procedure_or_function_expected);

           isclassmethod:=true;
         end
        else
         isclassmethod:=false;
        case token of
          _FUNCTION :
            begin
              consume(_FUNCTION);
              pd:=parse_proc_head(aclass,potype_none);
              if assigned(pd) then
                begin
                  if try_to_consume(_COLON) then
                   begin
                     inc(testcurobject);
                     single_type(pd.rettype,hs,false);
                     pd.test_if_fpu_result;
                     dec(testcurobject);
                   end
                  else
                   begin
                      if (
                          not(is_interface(pd._class)) and
                          not(pd.forwarddef)
                         ) or
                         (m_repeat_forward in aktmodeswitches) then
                      begin
                        consume(_COLON);
                        consume_all_until(_SEMICOLON);
                      end;
                   end;
                  if isclassmethod then
                   include(pd.procoptions,po_classmethod);
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
              pd:=parse_proc_head(aclass,potype_none);
              if assigned(pd) then
                begin
                  pd.rettype:=voidtype;
                  if isclassmethod then
                    include(pd.procoptions,po_classmethod);
                end;
            end;

          _CONSTRUCTOR :
            begin
              consume(_CONSTRUCTOR);
              pd:=parse_proc_head(aclass,potype_constructor);
              if assigned(pd) and
                 assigned(pd._class) then
                begin
                  { Set return type, class constructors return the
                    created instance, object constructors return boolean }
                  if is_class(pd._class) then
                   pd.rettype.setdef(pd._class)
                  else
                   pd.rettype:=booltype;
                end;
            end;

          _DESTRUCTOR :
            begin
              consume(_DESTRUCTOR);
              pd:=parse_proc_head(aclass,potype_destructor);
              if assigned(pd) then
                pd.rettype:=voidtype;
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
                 Message(parser_e_overload_operator_failed);
                 { Use the dummy NOTOKEN that is also declared
                   for the overloaded_operator[] }
                 optoken:=NOTOKEN;
               end;
              consume(token);
              pd:=parse_proc_head(aclass,potype_operator);
              if assigned(pd) then
                begin
                  if pd.parast.symtablelevel>normal_function_level then
                    Message(parser_e_no_local_operator);
                  if token<>_ID then
                    begin
                       if not(m_result in aktmodeswitches) then
                         consume(_ID);
                    end
                  else
                    begin
                      pd.resultname:=orgpattern;
                      consume(_ID);
                    end;
                  if not try_to_consume(_COLON) then
                    begin
                      consume(_COLON);
                      pd.rettype:=generrortype;
                      consume_all_until(_SEMICOLON);
                    end
                  else
                   begin
                     single_type(pd.rettype,hs,false);
                     pd.test_if_fpu_result;
                     if (optoken in [_EQUAL,_GT,_LT,_GTE,_LTE]) and
                        ((pd.rettype.def.deftype<>orddef) or
                         (torddef(pd.rettype.def).typ<>bool8bit)) then
                        Message(parser_e_comparative_operator_return_boolean);
                     if (optoken=_ASSIGNMENT) and
                        equal_defs(pd.rettype.def,
                           tvarsym(pd.parast.symindex.first).vartype.def) then
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
        { support procedure proc stdcall export; }
        if not(is_proc_directive(token,false)) then
          consume(_SEMICOLON);
        result:=pd;
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
  if pd.deftype<>procdef then
    internalerror(200304264);
  if assigned(tprocdef(pd)._class) then
    Message(parser_e_methods_dont_be_export);
  if pd.parast.symtablelevel>normal_function_level then
    Message(parser_e_dont_nest_export);
  { only os/2 and emx need this }
  if target_info.system in [system_i386_os2,system_i386_emx] then
   begin
     tprocdef(pd).aliasnames.insert(tprocdef(pd).procsym.realname);
     if cs_link_deffile in aktglobalswitches then
       deffile.AddExport(tprocdef(pd).mangledname);
   end;
end;

procedure pd_forward(pd:tabstractprocdef);
begin
  if pd.deftype<>procdef then
    internalerror(200304265);
  tprocdef(pd).forwarddef:=true;
end;

procedure pd_alias(pd:tabstractprocdef);
begin
  if pd.deftype<>procdef then
    internalerror(200304266);
  consume(_COLON);
  tprocdef(pd).aliasnames.insert(get_stringconst);
end;

procedure pd_asmname(pd:tabstractprocdef);
begin
  if pd.deftype<>procdef then
    internalerror(200304267);
  tprocdef(pd).setmangledname(target_info.Cprefix+pattern);
  if token=_CCHAR then
    consume(_CCHAR)
  else
    consume(_CSTRING);
  { we don't need anything else }
  tprocdef(pd).forwarddef:=false;
end;

procedure pd_inline(pd:tabstractprocdef);
var
  hp : tparaitem;
begin
  { check if there is an array of const }
  hp:=tparaitem(pd.para.first);
  while assigned(hp) do
   begin
     if assigned(hp.paratype.def) and
        (hp.paratype.def.deftype=arraydef) then
      begin
        with tarraydef(hp.paratype.def) do
         if IsVariant or IsConstructor {or IsArrayOfConst} then
          begin
            Message1(parser_w_not_supported_for_inline,'array of const');
            Message(parser_w_inlining_disabled);
            pd.proccalloption:=pocall_default;
          end;
      end;
     hp:=tparaitem(hp.next);
   end;
end;

procedure pd_intern(pd:tabstractprocdef);
begin
  if pd.deftype<>procdef then
    internalerror(200304268);
  consume(_COLON);
  tprocdef(pd).extnumber:=get_intconst;
end;

procedure pd_interrupt(pd:tabstractprocdef);
begin
  if pd.parast.symtablelevel>normal_function_level then
    Message(parser_e_dont_nest_interrupt);
end;

procedure pd_abstract(pd:tabstractprocdef);
begin
  if pd.deftype<>procdef then
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
  if pd.deftype<>procdef then
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
           pd.messageinf.i:=pt^.value;
         end
       else
         Message(parser_e_ill_msg_expr);
       disposetree(pt);
    end;
{$endif WITHDMT}
end;

procedure pd_static(pd:tabstractprocdef);
begin
  if (cs_static_keyword in aktmoduleswitches) then
    begin
      if pd.deftype=procdef then
        include(tprocdef(pd).procsym.symoptions,sp_static);
      include(pd.procoptions,po_staticmethod);
    end;
end;

procedure pd_override(pd:tabstractprocdef);
begin
  if pd.deftype<>procdef then
    internalerror(2003042611);
  if not(is_class_or_interface(tprocdef(pd)._class)) then
    Message(parser_e_no_object_override);
end;

procedure pd_overload(pd:tabstractprocdef);
begin
  if pd.deftype<>procdef then
    internalerror(2003042612);
  include(tprocdef(pd).procsym.symoptions,sp_has_overloaded);
end;

procedure pd_message(pd:tabstractprocdef);
var
  pt : tnode;
begin
  if pd.deftype<>procdef then
    internalerror(2003042613);
  if not is_class(tprocdef(pd)._class) then
    Message(parser_e_msg_only_for_classes);
  { check parameter type }
  if ((pd.minparacount<>1) or
      (pd.maxparacount<>1) or
      (TParaItem(pd.Para.first).paratyp<>vs_var)) then
    Message(parser_e_ill_msg_param);
  pt:=comp_expr(true);
  if pt.nodetype=stringconstn then
    begin
      include(pd.procoptions,po_msgstr);
      tprocdef(pd).messageinf.str:=strnew(tstringconstnode(pt).value_str);
    end
  else
   if is_constintnode(pt) then
    begin
      include(pd.procoptions,po_msgint);
      tprocdef(pd).messageinf.i:=tordconstnode(pt).value;
    end
  else
    Message(parser_e_ill_msg_expr);
  pt.free;
end;


procedure pd_reintroduce(pd:tabstractprocdef);
begin
  Message1(parser_w_proc_directive_ignored,'REINTRODUCE');
end;


procedure pd_syscall(pd:tabstractprocdef);
begin
  if pd.deftype<>procdef then
    internalerror(2003042614);
  tprocdef(pd).forwarddef:=false;
  tprocdef(pd).extnumber:=get_intconst;
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
  import_dll,
  import_name : string;
  import_nr   : word;
  hpd         : tprocdef;
begin
  if pd.deftype<>procdef then
    internalerror(2003042615);
  tprocdef(pd).forwarddef:=false;
  { forbid local external procedures }
  if pd.parast.symtablelevel>normal_function_level then
    Message(parser_e_no_local_external);
  { If the procedure should be imported from a DLL, a constant string follows.
    This isn't really correct, an contant string expression follows
    so we check if an semicolon follows, else a string constant have to
    follow (FK) }
  import_nr:=0;
  import_name:='';
  if not(token=_SEMICOLON) and not(idtoken=_NAME) then
    begin
      import_dll:=get_stringconst;
      if (idtoken=_NAME) then
       begin
         consume(_NAME);
         import_name:=get_stringconst;
       end;
      if (idtoken=_INDEX) then
       begin
         {After the word index follows the index number in the DLL.}
         consume(_INDEX);
         import_nr:=get_intconst;
       end;
      { default is to used the realname of the procedure }
      if (import_nr=0) and (import_name='') then
        import_name:=tprocdef(pd).procsym.realname;
      { create importlib if not already done }
      if not(current_module.uses_imports) then
       begin
         current_module.uses_imports:=true;
         importlib.preparelib(current_module.modulename^);
       end;
      if not(m_repeat_forward in aktmodeswitches) then
       begin
         { we can only have one overloaded here ! }
         if tprocsym(tprocdef(pd).procsym).procdef_count>1 then
          hpd:=tprocsym(tprocdef(pd).procsym).procdef[2]
         else
          hpd:=tprocdef(pd);
       end
      else
       hpd:=tprocdef(pd);
      importlib.importprocedure(hpd,import_dll,import_nr,import_name);
    end
  else
    begin
      if (idtoken=_NAME) then
       begin
         consume(_NAME);
         import_name:=get_stringconst;
         tprocdef(pd).setmangledname(import_name);
       end;
    end;
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
  num_proc_directives=34;
  proc_direcdata:array[1..num_proc_directives] of proc_dir_rec=
   (
    (
      idtok:_ABSTRACT;
      pd_flags : [pd_interface,pd_object,pd_notobjintf];
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_abstract;
      pocall   : pocall_none;
      pooption : [po_abstractmethod];
      mutexclpocall : [pocall_internproc,pocall_inline];
      mutexclpotype : [];
      mutexclpo     : [po_exports,po_interrupt,po_external]
    ),(
      idtok:_ALIAS;
      pd_flags : [pd_implemen,pd_body,pd_notobjintf];
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_alias;
      pocall   : pocall_none;
      pooption : [];
      mutexclpocall : [pocall_inline];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_ASMNAME;
      pd_flags : [pd_interface,pd_implemen,pd_notobjintf];
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_asmname;
      pocall   : pocall_cdecl;
      pooption : [po_external];
      mutexclpocall : [pocall_internproc,pocall_inline];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_ASSEMBLER;
      pd_flags : [pd_implemen,pd_body,pd_notobjintf];
      handler  : nil;
      pocall   : pocall_none;
      pooption : [po_assembler];
      mutexclpocall : [];
      mutexclpotype : [];
      mutexclpo     : [po_external]
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
      idtok:_DYNAMIC;
      pd_flags : [pd_interface,pd_object,pd_notobjintf];
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_virtual;
      pocall   : pocall_none;
      pooption : [po_virtualmethod];
      mutexclpocall : [pocall_internproc,pocall_inline];
      mutexclpotype : [];
      mutexclpo     : [po_exports,po_interrupt,po_external]
    ),(
      idtok:_EXPORT;
      pd_flags : [pd_body,pd_interface,pd_implemen,pd_notobjintf];
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_export;
      pocall   : pocall_none;
      pooption : [po_exports,po_public];
      mutexclpocall : [pocall_internproc,pocall_inline];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_external,po_interrupt]
    ),(
      idtok:_EXTERNAL;
      pd_flags : [pd_implemen,pd_interface,pd_notobject,pd_notobjintf];
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_external;
      pocall   : pocall_none;
      pooption : [po_external];
      mutexclpocall : [pocall_internproc,pocall_inline,pocall_palmossyscall];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_exports,po_interrupt,po_assembler]
    ),(
      idtok:_FAR;
      pd_flags : [pd_implemen,pd_body,pd_interface,pd_procvar,pd_notobject,pd_notobjintf];
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_far;
      pocall   : pocall_none;
      pooption : [];
      mutexclpocall : [pocall_internproc,pocall_inline];
      mutexclpotype : [];
      mutexclpo     : []
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
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_forward;
      pocall   : pocall_none;
      pooption : [];
      mutexclpocall : [pocall_internproc,pocall_inline];
      mutexclpotype : [];
      mutexclpo     : [po_external]
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
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_inline;
      pocall   : pocall_inline;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_exports,po_external,po_interrupt]
    ),(
      idtok:_INTERNCONST;
      pd_flags : [pd_implemen,pd_body,pd_notobject,pd_notobjintf];
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_intern;
      pocall   : pocall_none;
      pooption : [po_internconst];
      mutexclpocall : [];
      mutexclpotype : [potype_operator];
      mutexclpo     : []
    ),(
      idtok:_INTERNPROC;
      pd_flags : [pd_implemen,pd_notobject,pd_notobjintf];
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_intern;
      pocall   : pocall_internproc;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [potype_constructor,potype_destructor,potype_operator];
      mutexclpo     : [po_exports,po_external,po_interrupt,po_assembler,po_iocheck]
    ),(
      idtok:_INTERRUPT;
      pd_flags : [pd_implemen,pd_body,pd_notobject,pd_notobjintf];
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_interrupt;
      pocall   : pocall_none;
      pooption : [po_interrupt];
      mutexclpocall : [pocall_internproc,pocall_cdecl,pocall_cppdecl,pocall_stdcall,
                       pocall_inline,pocall_pascal,pocall_far16,pocall_oldfpccall];
      mutexclpotype : [potype_constructor,potype_destructor,potype_operator];
      mutexclpo     : [po_external]
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
      idtok:_MESSAGE;
      pd_flags : [pd_interface,pd_object,pd_notobjintf];
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_message;
      pocall   : pocall_none;
      pooption : []; { can be po_msgstr or po_msgint }
      mutexclpocall : [pocall_inline,pocall_internproc];
      mutexclpotype : [potype_constructor,potype_destructor,potype_operator];
      mutexclpo     : [po_interrupt,po_external]
    ),(
      idtok:_NEAR;
      pd_flags : [pd_implemen,pd_body,pd_procvar,pd_notobjintf];
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_near;
      pocall   : pocall_none;
      pooption : [];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : []
    ),(
      idtok:_OVERLOAD;
      pd_flags : [pd_implemen,pd_interface,pd_body];
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_overload;
      pocall   : pocall_none;
      pooption : [po_overload];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : []
    ),(
      idtok:_OVERRIDE;
      pd_flags : [pd_interface,pd_object,pd_notobjintf];
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_override;
      pocall   : pocall_none;
      pooption : [po_overridingmethod,po_virtualmethod];
      mutexclpocall : [pocall_inline,pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_exports,po_external,po_interrupt]
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
      pd_flags : [pd_implemen,pd_body,pd_notobject,pd_notobjintf];
      handler  : nil;
      pocall   : pocall_none;
      pooption : [po_public];
      mutexclpocall : [pocall_internproc,pocall_inline];
      mutexclpotype : [];
      mutexclpo     : [po_external]
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
      pd_flags : [pd_interface,pd_object];
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_reintroduce;
      pocall   : pocall_none;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [];
      mutexclpo     : []
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
      idtok:_SAVEREGISTERS;
      pd_flags : [pd_interface,pd_implemen,pd_body,pd_procvar,pd_notobjintf];
      handler  : nil;
      pocall   : pocall_none;
      pooption : [po_saveregisters];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_external]
    ),(
      idtok:_STATIC;
      pd_flags : [pd_interface,pd_object,pd_notobjintf];
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_static;
      pocall   : pocall_none;
      pooption : [po_staticmethod];
      mutexclpocall : [pocall_inline,pocall_internproc];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_external,po_interrupt,po_exports]
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
      pd_flags : [pd_interface,pd_implemen,pd_notobjintf];
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_syscall;
      pocall   : pocall_palmossyscall;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_external,po_assembler,po_interrupt,po_exports]
    ),(
      idtok:_VIRTUAL;
      pd_flags : [pd_interface,pd_object,pd_notobjintf];
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_virtual;
      pocall   : pocall_none;
      pooption : [po_virtualmethod];
      mutexclpocall : [pocall_inline,pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_external,po_interrupt,po_exports]
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
                       pocall_inline,pocall_far16,pocall_oldfpccall];
      mutexclpotype : [];
      mutexclpo     : [po_assembler,po_interrupt]
    ),(
      idtok:_COMPILERPROC;
      pd_flags : [pd_interface,pd_implemen,pd_body,pd_notobjintf];
      handler  : nil;
      pocall   : pocall_compilerproc;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_interrupt]
    )
   );


    function is_proc_directive(tok:ttoken;isprocvar:boolean):boolean;
      var
        i : longint;
      begin
        is_proc_directive:=false;
        for i:=1 to num_proc_directives do
         if proc_direcdata[i].idtok=idtoken then
          begin
            if (not isprocvar) or
               (pd_procvar in proc_direcdata[i].pd_flags) then
              is_proc_directive:=true;
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
        name  : stringid;
      begin
        parse_proc_direc:=false;
        name:=tokeninfo^[idtoken].str;
        found:=false;

      { Hint directive? Then exit immediatly }
        if (m_hintdirective in aktmodeswitches) then
         begin
           case idtoken of
             _LIBRARY,
             _PLATFORM,
             _UNIMPLEMENTED,
             _DEPRECATED :
               exit;
           end;
         end;

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
            if (pdflags * [pd_procvar,pd_object])=[] then
              Message1(parser_w_unknown_proc_directive_ignored,name);
            exit;
         end;

        { static needs a special treatment }
        if (idtoken=_STATIC) and not (cs_static_keyword in aktmoduleswitches) then
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
           pd.proccalloption:=proc_direcdata[p].pocall;
           include(pd.procoptions,po_hascallingconvention);
         end;

        { check if method and directive not for object, like public.
          This needs to be checked also for procvars }
        if (pd_notobject in proc_direcdata[p].pd_flags) and
           (pd.owner.symtabletype=objectsymtable) then
           exit;

        if pd.deftype=procdef then
         begin
           { Check if the directive is only for objects }
           if (pd_object in proc_direcdata[p].pd_flags) and
              not assigned(tprocdef(pd)._class) then
            exit;

           { check if method and directive not for interface }
           if (pd_notobjintf in proc_direcdata[p].pd_flags) and
              is_interface(tprocdef(pd)._class) then
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
        if pointer({$ifndef FPCPROCVAR}@{$endif}proc_direcdata[p].handler)<>nil then
          proc_direcdata[p].handler(pd);
      end;


    procedure handle_calling_convention(pd:tabstractprocdef);
      begin
        { set the default calling convention if none provided }
        if not(po_hascallingconvention in pd.procoptions) then
          pd.proccalloption:=aktdefproccall
        else
          begin
            if pd.proccalloption=pocall_none then
              internalerror(200309081);
          end;

        { handle proccall specific settings }
        case pd.proccalloption of
          pocall_cdecl :
            begin
              { set mangledname }
              if (pd.deftype=procdef) then
               begin
                 if not tprocdef(pd).has_mangledname then
                  begin
                    if assigned(tprocdef(pd)._class) then
                     tprocdef(pd).setmangledname(target_info.Cprefix+tprocdef(pd)._class.objrealname^+'_'+tprocdef(pd).procsym.realname)
                    else
                     tprocdef(pd).setmangledname(target_info.Cprefix+tprocdef(pd).procsym.realname);
                  end;
                 { check C cdecl para types }
                 pd.parast.foreach_static({$ifdef FPCPROCVAR}@{$endif}check_c_para,nil);
                 { Adjust alignment to match cdecl or stdcall }
                 pd.paraalign:=std_param_align;
               end;
            end;
          pocall_cppdecl :
            begin
              { set mangledname }
              if (pd.deftype=procdef) then
               begin
                 if not tprocdef(pd).has_mangledname then
                  tprocdef(pd).setmangledname(target_info.Cprefix+tprocdef(pd).cplusplusmangledname);
                 { check C cdecl para types }
                 pd.parast.foreach_static({$ifdef FPCPROCVAR}@{$endif}check_c_para,nil);
                 { Adjust alignment to match cdecl or stdcall }
                 pd.paraalign:=std_param_align;
               end;
            end;
          pocall_stdcall :
            begin
              if (pd.deftype=procdef) then
               begin
                 { Adjust alignment to match cdecl or stdcall }
                 pd.paraalign:=std_param_align;
               end;
            end;
          pocall_compilerproc :
            begin
              if (pd.deftype<>procdef) then
               internalerror(200110232);
              tprocdef(pd).setmangledname(lower(tprocdef(pd).procsym.name));
            end;
          pocall_register :
            begin
              { Adjust alignment to match cdecl or stdcall }
              pd.paraalign:=std_param_align;
            end;
          pocall_far16 :
            begin
              { Temporary stub, must be rewritten to support OS/2 far16 }
              Message1(parser_w_proc_directive_ignored,'FAR16');
            end;
          pocall_palmossyscall :
            begin
              if (pd.deftype=procdef) then
               begin
                 { Adjust positions of args for cdecl or stdcall }
                 pd.paraalign:=std_param_align;
               end;
            end;
          pocall_inline :
            begin
              if not(cs_support_inline in aktmoduleswitches) then
               begin
                 Message(parser_e_proc_inline_not_supported);
                 pd.proccalloption:=pocall_default;
               end;
            end;
        end;

        { For varargs directive also cdecl and external must be defined }
        if (po_varargs in pd.procoptions) then
         begin
           { check first for external in the interface, if available there
             then the cdecl must also be there since there is no implementation
             available to contain it }
           if parse_only then
            begin
              { if external is available, then cdecl must also be available }
              if (po_external in pd.procoptions) and
                 not(pd.proccalloption in [pocall_cdecl,pocall_cppdecl]) then
                Message(parser_e_varargs_need_cdecl_and_external);
            end
           else
            begin
              { both must be defined now }
              if not(po_external in pd.procoptions) or
                 not(pd.proccalloption in [pocall_cdecl,pocall_cppdecl]) then
                Message(parser_e_varargs_need_cdecl_and_external);
            end;
         end;

        { add mangledname to external list }
        if (pd.deftype=procdef) and
           (po_external in pd.procoptions) and
           target_info.DllScanSupported then
          current_module.externals.insert(tExternalsItem.create(tprocdef(pd).mangledname));
      end;


    procedure calc_parast(pd:tabstractprocdef);
      var
        currpara : tparaitem;
      begin
        { insert hidden high parameters }
        insert_hidden_para(pd);
        { insert hidden self parameter }
        insert_self_and_vmt_para(pd);
        { insert funcret parameter if required }
        insert_funcret_para(pd);
        { insert parentfp parameter if required }
        insert_parentfp_para(pd);

        if pd.proccalloption=pocall_pascal then
          tparaitem(pd.para.first):=reverseparaitems(tparaitem(pd.para.first));

        currpara:=tparaitem(pd.para.first);
        while assigned(currpara) do
         begin
           if not(assigned(currpara.parasym) and (currpara.parasym.typ=varsym)) then
             internalerror(200304232);
           { connect parasym to paraitem }
           tvarsym(currpara.parasym).paraitem:=currpara;
           { We need a local copy for a value parameter when only the
             address is pushed. Open arrays and Array of Const are
             an exception because they are allocated at runtime and the
             address that is pushed is patched }
           if (currpara.paratyp=vs_value) and
              paramanager.push_addr_param(currpara.paratyp,currpara.paratype.def,pd.proccalloption) and
              not(is_open_array(currpara.paratype.def) or
                  is_array_of_const(currpara.paratype.def)) then
             include(tvarsym(currpara.parasym).varoptions,vo_has_local_copy);
           currpara:=tparaitem(currpara.next);
         end;
      end;



    procedure parse_proc_directives(pd:tabstractprocdef;var pdflags:tpdflags);
      {
        Parse the procedure directives. It does not matter if procedure directives
        are written using ;procdir; or ['procdir'] syntax.
      }
      var
        res : boolean;
      begin
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
              if (block_type in [bt_const,bt_type]) and
                 (token=_EQUAL) then
               break;
              { support procedure proc;stdcall export; }
              if not(is_proc_directive(token,(pd.deftype=procvardef))) then
               consume(_SEMICOLON);
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
          varsym :
            pd:=tabstractprocdef(tvarsym(sym).vartype.def);
          typedconstsym :
            pd:=tabstractprocdef(ttypedconstsym(sym).typedconsttype.def);
          typesym :
            pd:=tabstractprocdef(ttypesym(sym).restype.def);
          else
            internalerror(2003042617);
        end;
        if pd.deftype<>procvardef then
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


    function proc_add_definition(var pd:tprocdef):boolean;
      {
        Add definition aprocdef to the overloaded definitions of aprocsym. If a
        forwarddef is found and reused it returns true
      }
      var
        hd    : tprocdef;
        ad,fd : tsym;
        s1,s2 : stringid;
        i     : cardinal;
        forwardfound : boolean;
        po_comp : tprocoptions;
        aprocsym : tprocsym;
      begin
        forwardfound:=false;
        aprocsym:=tprocsym(pd.procsym);

        { check overloaded functions if the same function already exists }
        for i:=1 to aprocsym.procdef_count do
         begin
           hd:=aprocsym.procdef[i];

           { Skip overloaded definitions that are declared in other
             units }
           if hd.procsym<>aprocsym then
             continue;

           { check the parameters, for delphi/tp it is possible to
             leave the parameters away in the implementation (forwarddef=false).
             But for an overload declared function this is not allowed }
           if { check if empty implementation arguments match is allowed }
              (
               not(m_repeat_forward in aktmodeswitches) and
               not(pd.forwarddef) and
               (pd.maxparacount=0) and
               not(po_overload in hd.procoptions)
              ) or
              { check arguments }
              (
               (compare_paras(pd.para,hd.para,cp_none,false)>=te_equal) and
               { for operators equal_paras is not enough !! }
               ((pd.proctypeoption<>potype_operator) or (optoken<>_ASSIGNMENT) or
                equal_defs(hd.rettype.def,pd.rettype.def))
              ) then
             begin
               { Check if we've found the forwarddef, if found then
                 we need to update the forward def with the current
                 implementation settings }
               if hd.forwarddef then
                 begin
                   forwardfound:=true;

                   { Check if the procedure type and return type are correct,
                     also the parameters must match also with the type }
                   if (hd.proctypeoption<>pd.proctypeoption) or
                      (
                       (m_repeat_forward in aktmodeswitches) and
                       (not((pd.maxparacount=0) or
                            (compare_paras(pd.para,hd.para,cp_all,false)>=te_equal)))
                      ) or
                      (
                       ((m_repeat_forward in aktmodeswitches) or
                        not(is_void(pd.rettype.def))) and
                       (not equal_defs(hd.rettype.def,pd.rettype.def))) then
                     begin
                       MessagePos1(pd.fileinfo,parser_e_header_dont_match_forward,
                                   pd.fullprocname(false));
                       aprocsym.write_parameter_lists(pd);
                       break;
                     end;

                   { Check if both are declared forward }
                   if hd.forwarddef and pd.forwarddef then
                    begin
                      MessagePos1(pd.fileinfo,parser_e_function_already_declared_public_forward,
                                  pd.fullprocname(false));
                    end;

                   { internconst or internproc only need to be defined once }
                   if (hd.proccalloption=pocall_internproc) then
                    pd.proccalloption:=hd.proccalloption
                   else
                    if (pd.proccalloption=pocall_internproc) then
                     hd.proccalloption:=pd.proccalloption;
                   if (po_internconst in hd.procoptions) then
                    include(pd.procoptions,po_internconst)
                   else if (po_internconst in pd.procoptions) then
                    include(hd.procoptions,po_internconst);

                   { Check calling convention }
                   if (hd.proccalloption<>pd.proccalloption) then
                    begin
                      { In delphi it is possible to specify the calling
                        convention in the interface or implementation if
                        there was no convention specified in the other
                        part }
                      if (m_delphi in aktmodeswitches) then
                       begin
                         if not(po_hascallingconvention in pd.procoptions) then
                          pd.proccalloption:=hd.proccalloption
                         else
                          if not(po_hascallingconvention in hd.procoptions) then
                           hd.proccalloption:=pd.proccalloption
                         else
                          begin
                            MessagePos(pd.fileinfo,parser_e_call_convention_dont_match_forward);
                            aprocsym.write_parameter_lists(pd);
                            { restore interface settings }
                            pd.proccalloption:=hd.proccalloption;
                          end;
                       end
                      else
                       begin
                         MessagePos(pd.fileinfo,parser_e_call_convention_dont_match_forward);
                         aprocsym.write_parameter_lists(pd);
                         { restore interface settings }
                         pd.proccalloption:=hd.proccalloption;
                       end;
                    end;

                   { Check procedure options, Delphi requires that class is
                     repeated in the implementation for class methods }
                   if (m_fpc in aktmodeswitches) then
                     po_comp:=[po_varargs,po_methodpointer,po_interrupt]
                   else
                     po_comp:=[po_classmethod,po_methodpointer];

                   if ((po_comp * hd.procoptions)<>(po_comp * pd.procoptions)) then
                     begin
                       MessagePos1(pd.fileinfo,parser_e_header_dont_match_forward,
                                   pd.fullprocname(false));
                       aprocsym.write_parameter_lists(pd);
                       { This error is non-fatal, we can recover }
                     end;

                   { Check manglednames }
                   if (m_repeat_forward in aktmodeswitches) or
                      (pd.minparacount>0) then
                    begin
                      { If mangled names are equal then they have the same amount of arguments }
                      { We can check the names of the arguments }
                      { both symtables are in the same order from left to right }
                      ad:=tsym(hd.parast.symindex.first);
                      fd:=tsym(pd.parast.symindex.first);
                      repeat
                        { skip default parameter constsyms }
                        while assigned(ad) and (ad.typ<>varsym) do
                         ad:=tsym(ad.indexnext);
                        while assigned(fd) and (fd.typ<>varsym) do
                         fd:=tsym(fd.indexnext);
                        { stop when one of the two lists is at the end }
                        if not assigned(ad) or not assigned(fd) then
                         break;
                        { retrieve names, remove reg for register parameters }
                        s1:=ad.name;
                        s2:=fd.name;
{$ifdef i386}
                        if copy(s1,1,3)='reg' then
                          delete(s1,1,3);
                        if copy(s2,1,3)='reg' then
                          delete(s2,1,3);
{$endif i386}
                        { compare names }
                        if (s1<>s2) then
                         begin
                           MessagePos3(pd.fileinfo,parser_e_header_different_var_names,
                                       aprocsym.name,s1,s2);
                           break;
                         end;
                        ad:=tsym(ad.indexnext);
                        fd:=tsym(fd.indexnext);
                      until false;
                      if assigned(ad) xor assigned(fd) then
                        internalerror(200204178);
                    end;

                   { Everything is checked, now we can update the forward declaration
                     with the new data from the implementation }
                   hd.forwarddef:=pd.forwarddef;
                   hd.hasforward:=true;
                   hd.paraalign:=pd.paraalign;
                   hd.procoptions:=hd.procoptions+pd.procoptions;
                   if hd.extnumber=65535 then
                     hd.extnumber:=pd.extnumber;
                   while not pd.aliasnames.empty do
                    hd.aliasnames.insert(pd.aliasnames.getfirst);
                   { update fileinfo so position references the implementation,
                     also update funcretsym if it is already generated }
                   hd.fileinfo:=pd.fileinfo;
                   if assigned(hd.funcretsym) then
                     hd.funcretsym.fileinfo:=pd.fileinfo;
                   { update mangledname if the implementation has a fixed mangledname set }
                   if pd.has_mangledname then
                    begin
                      { rename also asmsymbol first, because the name can already be used }
                      objectlibrary.renameasmsymbol(hd.mangledname,pd.mangledname);
                      hd.setmangledname(pd.mangledname);
                    end;
                   { for compilerproc defines we need to rename and update the
                     symbolname to lowercase }
                   if (pd.proccalloption=pocall_compilerproc) then
                    begin
                      { rename to lowercase so users can't access it }
                      aprocsym.owner.rename(aprocsym.name,lower(aprocsym.name));
                      { also update the realname that is stored in the ppu }
                      stringdispose(aprocsym._realname);
                      aprocsym._realname:=stringdup('$'+aprocsym.name);
                      { the mangeled name is already changed by the pd_compilerproc }
                      { handler. It must be done immediately because if we have a   }
                      { call to a compilerproc before it's implementation is        }
                      { encountered, it must already use the new mangled name (JM)  }
                    end;

                   { the procdef will be released by the symtable, we release
                     at least the parast }
                   pd.releasemem;
                   pd:=hd;
                 end
               else
                begin
                  { abstract methods aren't forward defined, but this }
                  { needs another error message                   }
                  if (po_abstractmethod in hd.procoptions) then
                    MessagePos(pd.fileinfo,parser_e_abstract_no_definition)
                  else
                    MessagePos(pd.fileinfo,parser_e_overloaded_have_same_parameters);
                 end;

               { we found one proc with the same arguments, there are no others
                 so we can stop }
               break;
             end;

           { check for allowing overload directive }
           if not(m_fpc in aktmodeswitches) then
            begin
              { overload directive turns on overloading }
              if ((po_overload in pd.procoptions) or
                  (po_overload in hd.procoptions)) then
               begin
                 { check if all procs have overloading, but not if the proc was
                   already declared forward or abstract, then the check is already done }
                 if not(hd.hasforward or
                        (po_abstractmethod in hd.procoptions) or
                        (pd.forwarddef<>hd.forwarddef) or
                        ((po_overload in pd.procoptions) and
                         (po_overload in hd.procoptions))) then
                  begin
                    MessagePos1(pd.fileinfo,parser_e_no_overload_for_all_procs,aprocsym.realname);
                    break;
                  end;
               end
              else
               begin
                 if not(hd.forwarddef) then
                  begin
                    MessagePos(pd.fileinfo,parser_e_procedure_overloading_is_off);
                    break;
                  end;
               end;
            end; { equal arguments }
         end;

        { if we didn't reuse a forwarddef then we add the procdef to the overloaded
          list }
        if not forwardfound then
         begin
           aprocsym.addprocdef(pd);
           { add overloadnumber for unique naming, the overloadcount is
             counted per module and 0 for the first procedure }
           pd.overloadnumber:=aprocsym.overloadcount;
           inc(aprocsym.overloadcount);
         end;

        proc_add_definition:=forwardfound;
      end;

end.
{
  $Log$
  Revision 1.143  2003-10-02 21:13:09  peter
    * procvar directive parsing fixes

  Revision 1.142  2003/10/01 19:05:33  peter
    * searchsym_type to search for type definitions. It ignores
      records,objects and parameters

  Revision 1.141  2003/10/01 18:28:55  peter
    * don't look in objectsymtable when parsing the function return type

  Revision 1.140  2003/10/01 16:49:05  florian
    * para items are now reversed for pascal calling conventions

  Revision 1.139  2003/09/28 21:44:55  peter
    * fix check that filedef needs var para

  Revision 1.138  2003/09/28 17:55:04  peter
    * parent framepointer changed to hidden parameter
    * tloadparentfpnode added

  Revision 1.137  2003/09/25 21:24:09  peter
    * don't include vo_has_local_copy for open array/array of const

  Revision 1.136  2003/09/23 20:36:47  peter
    * remove obsolete code

  Revision 1.135  2003/09/23 17:56:05  peter
    * locals and paras are allocated in the code generation
    * tvarsym.localloc contains the location of para/local when
      generating code for the current procedure

  Revision 1.134  2003/09/16 16:17:01  peter
    * varspez in calls to push_addr_param

  Revision 1.133  2003/09/09 21:03:17  peter
    * basics for x86 register calling

  Revision 1.132  2003/09/09 15:54:10  peter
    * calling convention fix

  Revision 1.131  2003/09/07 22:09:35  peter
    * preparations for different default calling conventions
    * various RA fixes

  Revision 1.130  2003/09/03 11:18:37  florian
    * fixed arm concatcopy
    + arm support in the common compiler sources added
    * moved some generic cg code around
    + tfputype added
    * ...

  Revision 1.129  2003/07/02 22:18:04  peter
    * paraloc splitted in callerparaloc,calleeparaloc
    * sparc calling convention updates

  Revision 1.128  2003/06/13 21:19:31  peter
    * current_procdef removed, use current_procinfo.procdef instead

  Revision 1.127  2003/06/05 20:04:43  peter
    * set po_public also when parsing the object declaration

  Revision 1.126  2003/06/02 21:42:05  jonas
    * function results can now also be regvars
    - removed tprocinfo.return_offset, never use it again since it's invalid
      if the result is a regvar

  Revision 1.125  2003/05/22 21:31:35  peter
    * defer codegeneration for nested procedures

  Revision 1.124  2003/05/15 18:58:53  peter
    * removed selfpointer_offset, vmtpointer_offset
    * tvarsym.adjusted_address
    * address in localsymtable is now in the real direction
    * removed some obsolete globals

  Revision 1.123  2003/05/13 15:18:49  peter
    * fixed various crashes

  Revision 1.122  2003/05/09 17:47:03  peter
    * self moved to hidden parameter
    * removed hdisposen,hnewn,selfn

  Revision 1.121  2003/05/05 14:53:16  peter
    * vs_hidden replaced by is_hidden boolean

  Revision 1.120  2003/04/30 09:42:42  florian
    + first changes to make self a hidden parameter

  Revision 1.119  2003/04/27 11:21:33  peter
    * aktprocdef renamed to current_procinfo.procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.118  2003/04/27 07:29:50  peter
    * current_procinfo.procdef cleanup, current_procdef is now always nil when parsing
      a new procdef declaration
    * aktprocsym removed
    * lexlevel removed, use symtable.symtablelevel instead
    * implicit init/final code uses the normal genentry/genexit
    * funcret state checking updated for new funcret handling

  Revision 1.117  2003/04/26 00:33:07  peter
    * vo_is_result flag added for the special RESULT symbol

  Revision 1.116  2003/04/25 20:59:33  peter
    * removed funcretn,funcretsym, function result is now in varsym
      and aliases for result and function name are added using absolutesym
    * vs_hidden parameter for funcret passed in parameter
    * vs_hidden fixes
    * writenode changed to printnode and released from extdebug
    * -vp option added to generate a tree.log with the nodetree
    * nicer printnode for statements, callnode

  Revision 1.115  2003/04/24 13:03:01  florian
    * comp is now written with its bit pattern to the ppu instead as an extended

  Revision 1.114  2003/04/23 13:12:26  peter
    * fix po_comp setting for fpc mode

  Revision 1.113  2003/04/23 10:12:51  peter
    * don't check po_varargs for delphi

  Revision 1.112  2003/04/22 13:47:08  peter
    * fixed C style array of const
    * fixed C array passing
    * fixed left to right with high parameters

  Revision 1.111  2003/04/10 17:57:53  peter
    * vs_hidden released

  Revision 1.110  2003/03/28 19:16:56  peter
    * generic constructor working for i386
    * remove fixed self register
    * esi added as address register for i386

  Revision 1.109  2003/03/23 23:21:42  hajny
    + emx target added

  Revision 1.108  2003/03/19 17:34:04  peter
    * only allow class [procedure|function]

  Revision 1.107  2003/03/17 18:56:02  peter
    * fix crash with duplicate id

  Revision 1.106  2003/03/17 15:54:22  peter
    * store symoptions also for procdef
    * check symoptions (private,public) when calculating possible
      overload candidates

  Revision 1.105  2003/01/15 20:02:28  carl
    * fix highname problem

  Revision 1.104  2003/01/12 15:42:23  peter
    * m68k pathexist update from 1.0.x
    * palmos res update from 1.0.x

  Revision 1.103  2003/01/07 19:16:38  peter
    * removed some duplicate code when creating aktprocsym

  Revision 1.102  2003/01/05 18:17:45  peter
    * more conflicts for constructor/destructor types

  Revision 1.100  2003/01/02 19:49:00  peter
    * update self parameter only for methodpointer and methods

  Revision 1.99  2003/01/01 22:51:03  peter
    * high value insertion changed so it works also when 2 parameters
      are passed

  Revision 1.98  2003/01/01 14:35:33  peter
    * don't check for export directive repeat

  Revision 1.97  2002/12/29 18:16:06  peter
    * delphi allows setting calling convention in interface or
      implementation

  Revision 1.96  2002/12/29 14:55:44  peter
    * fix static method check
    * don't require class for class methods in the implementation for
      non delphi modes

  Revision 1.95  2002/12/27 15:25:14  peter
    * check procoptions when a forward is found
    * exclude some call directives for constructor/destructor

  Revision 1.94  2002/12/25 01:26:56  peter
    * duplicate procsym-unitsym fix

  Revision 1.93  2002/12/24 21:21:06  peter
    * remove code that skipped the _ prefix for win32 imports

  Revision 1.92  2002/12/23 21:24:22  peter
    * fix wrong internalerror when var names were different

  Revision 1.91  2002/12/23 20:58:52  peter
    * cdecl array fix, hack to change it to vs_var is not needed

  Revision 1.90  2002/12/17 22:19:33  peter
    * fixed pushing of records>8 bytes with stdcall
    * simplified hightree loading

  Revision 1.89  2002/12/15 21:07:30  peter
    * don't allow external in object declarations

  Revision 1.88  2002/12/15 19:34:31  florian
    + some front end stuff for vs_hidden added

  Revision 1.87  2002/12/07 14:27:07  carl
    * 3% memory optimization
    * changed some types
    + added type checking with different size for call node and for
       parameters

  Revision 1.86  2002/12/06 17:51:10  peter
    * merged cdecl and array fixes

  Revision 1.85  2002/12/01 22:06:14  carl
    * cleanup of error messages

  Revision 1.84  2002/11/29 22:31:19  carl
    + unimplemented hint directive added
    * hint directive parsing implemented
    * warning on these directives

  Revision 1.83  2002/11/27 02:35:28  peter
    * fixed typo in method comparing

  Revision 1.82  2002/11/25 17:43:21  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.81  2002/11/18 17:31:58  peter
    * pass proccalloption to ret_in_xxx and push_xxx functions

  Revision 1.80  2002/11/17 16:31:56  carl
    * memory optimization (3-4%) : cleanup of tai fields,
       cleanup of tdef and tsym fields.
    * make it work for m68k

  Revision 1.79  2002/11/16 14:20:50  peter
    * fix infinite loop in pd_inline

  Revision 1.78  2002/11/15 01:58:53  peter
    * merged changes from 1.0.7 up to 04-11
      - -V option for generating bug report tracing
      - more tracing for option parsing
      - errors for cdecl and high()
      - win32 import stabs
      - win32 records<=8 are returned in eax:edx (turned off by default)
      - heaptrc update
      - more info for temp management in .s file with EXTDEBUG

  Revision 1.77  2002/10/06 15:09:12  peter
    * variant:=nil supported

  Revision 1.76  2002/09/27 21:13:29  carl
    * low-highval always checked if limit ober 2GB is reached (to avoid overflow)

  Revision 1.75  2002/09/16 14:11:13  peter
    * add argument to equal_paras() to support default values or not

  Revision 1.74  2002/09/10 16:27:28  peter
    * don't insert parast in symtablestack, because typesyms should not be
      searched in the the parast

  Revision 1.73  2002/09/09 19:39:07  peter
    * check return type for forwarddefs also not delphi mode when
      the type is not void

  Revision 1.72  2002/09/09 17:34:15  peter
    * tdicationary.replace added to replace and item in a dictionary. This
      is only allowed for the same name
    * varsyms are inserted in symtable before the types are parsed. This
      fixes the long standing "var longint : longint" bug
    - consume_idlist and idstringlist removed. The loops are inserted
      at the callers place and uses the symtable for duplicate id checking

  Revision 1.71  2002/09/07 15:25:06  peter
    * old logs removed and tabs fixed

  Revision 1.70  2002/09/03 16:26:27  daniel
    * Make Tprocdef.defs protected

  Revision 1.69  2002/09/01 12:11:33  peter
    * calc param_offset after parameters are read, because the calculation
      depends on po_containself

  Revision 1.68  2002/08/25 19:25:20  peter
    * sym.insert_in_data removed
    * symtable.insertvardata/insertconstdata added
    * removed insert_in_data call from symtable.insert, it needs to be
      called separatly. This allows to deref the address calculation
    * procedures now calculate the parast addresses after the procedure
      directives are parsed. This fixes the cdecl parast problem
    * push_addr_param has an extra argument that specifies if cdecl is used
      or not

  Revision 1.67  2002/08/25 11:33:06  peter
    * also check the paratypes when a forward was found

  Revision 1.66  2002/08/19 19:36:44  peter
    * More fixes for cross unit inlining, all tnodes are now implemented
    * Moved pocall_internconst to po_internconst because it is not a
      calling type at all and it conflicted when inlining of these small
      functions was requested

  Revision 1.65  2002/08/18 20:06:24  peter
    * inlining is now also allowed in interface
    * renamed write/load to ppuwrite/ppuload
    * tnode storing in ppu
    * nld,ncon,nbas are already updated for storing in ppu

  Revision 1.64  2002/08/17 09:23:39  florian
    * first part of procinfo rewrite

  Revision 1.63  2002/08/11 14:32:27  peter
    * renamed current_library to objectlibrary

  Revision 1.62  2002/08/11 13:24:12  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.61  2002/07/26 21:15:40  florian
    * rewrote the system handling

  Revision 1.60  2002/07/20 11:57:55  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.59  2002/07/11 14:41:28  florian
    * start of the new generic parameter handling

  Revision 1.58  2002/07/01 18:46:25  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.57  2002/05/18 13:34:12  peter
    * readded missing revisions

  Revision 1.56  2002/05/16 19:46:42  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.54  2002/05/12 16:53:08  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.53  2002/04/21 19:02:04  peter
    * removed newn and disposen nodes, the code is now directly
      inlined from pexpr
    * -an option that will write the secondpass nodes to the .s file, this
      requires EXTDEBUG define to actually write the info
    * fixed various internal errors and crashes due recent code changes

  Revision 1.52  2002/04/20 21:32:24  carl
  + generic FPC_CHECKPOINTER
  + first parameter offset in stack now portable
  * rename some constants
  + move some cpu stuff to other units
  - remove unused constents
  * fix stacksize for some targets
  * fix generic size problems which depend now on EXTEND_SIZE constant

  Revision 1.51  2002/04/20 15:27:05  carl
  - remove ifdef i386 define

  Revision 1.50  2002/04/19 15:46:02  peter
    * mangledname rewrite, tprocdef.mangledname is now created dynamicly
      in most cases and not written to the ppu
    * add mangeledname_prefix() routine to generate the prefix of
      manglednames depending on the current procedure, object and module
    * removed static procprefix since the mangledname is now build only
      on demand from tprocdef.mangledname

}
