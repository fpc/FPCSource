{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Parses variable declarations. Used for var statement and record
    definitions

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
unit pdecvar;

{$i fpcdefs.inc}

interface

    uses
      symsym,symdef;

    type
      tvar_dec_option=(vd_record,vd_object,vd_threadvar);
      tvar_dec_options=set of tvar_dec_option;

    function  read_property_dec(aclass:tobjectdef):tpropertysym;

    procedure read_var_decls(options:Tvar_dec_options);

    procedure read_record_fields(options:Tvar_dec_options);


implementation

    uses
       { common }
       cutils,cclasses,
       { global }
       globtype,globals,tokens,verbose,
       systems,
       { symtable }
       symconst,symbase,symtype,symtable,defutil,defcmp,
       fmodule,
       { pass 1 }
       node,pass_1,aasmdata,
       nmat,nadd,ncal,nset,ncnv,ninl,ncon,nld,nflw,nmem,
       { codegen }
       ncgutil,
       { parser }
       scanner,
       pbase,pexpr,ptype,ptconst,pdecsub,
       { link }
       import
       ;


    function read_property_dec(aclass:tobjectdef):tpropertysym;

        { convert a node tree to symlist and return the last
          symbol }
        function parse_symlist(pl:tsymlist;var def:tdef):boolean;
          var
            idx : longint;
            sym : tsym;
            srsymtable : tsymtable;
            st  : tsymtable;
            p   : tnode;
          begin
            result:=true;
            def:=nil;
            if token=_ID then
             begin
               if assigned(aclass) then
                 sym:=search_class_member(aclass,pattern)
               else
                 searchsym(pattern,sym,srsymtable);
               if assigned(sym) then
                begin
                  case sym.typ of
                    fieldvarsym :
                      begin
                        pl.addsym(sl_load,sym);
                        def:=tfieldvarsym(sym).vartype.def;
                      end;
                    procsym :
                      begin
                        pl.addsym(sl_call,sym);
                      end;
                    else
                      begin
                        Message1(parser_e_illegal_field_or_method,orgpattern);
                        result:=false;
                      end;
                  end;
                end
               else
                begin
                  Message1(parser_e_illegal_field_or_method,orgpattern);
                  result:=false;
                end;
               consume(_ID);
               repeat
                 case token of
                   _ID,
                   _SEMICOLON :
                     begin
                       break;
                     end;
                   _POINT :
                     begin
                       consume(_POINT);
                       if assigned(def) then
                        begin
                          st:=def.getsymtable(gs_record);
                          if assigned(st) then
                           begin
                             sym:=tsym(st.search(pattern));
                             if assigned(sym) then
                              begin
                                pl.addsym(sl_subscript,sym);
                                case sym.typ of
                                  fieldvarsym :
                                    def:=tfieldvarsym(sym).vartype.def;
                                  else
                                    begin
                                      Message1(sym_e_illegal_field,orgpattern);
                                      result:=false;
                                    end;
                                end;
                              end
                             else
                              begin
                                Message1(sym_e_illegal_field,orgpattern);
                                result:=false;
                              end;
                           end
                          else
                           begin
                             Message(parser_e_invalid_qualifier);
                             result:=false;
                           end;
                        end
                       else
                        begin
                          Message(parser_e_invalid_qualifier);
                          result:=false;
                        end;
                       consume(_ID);
                     end;
                   _LECKKLAMMER :
                     begin
                       consume(_LECKKLAMMER);
                       repeat
                         if def.deftype=arraydef then
                          begin
                            idx:=0;
                            p:=comp_expr(true);
                            if (not codegenerror) then
                             begin
                               if (p.nodetype=ordconstn) then
                                 begin
                                   if compare_defs(p.resulttype.def,tarraydef(def).rangetype.def,nothingn)>=te_equal then
                                     idx:=tordconstnode(p).value
                                   else
                                     IncompatibleTypes(p.resulttype.def,tarraydef(def).rangetype.def);
                                 end
                               else
                                Message(type_e_ordinal_expr_expected)
                             end;
                            p.free;
                            pl.addconst(sl_vec,idx,p.resulttype);
                            def:=tarraydef(def).elementtype.def;
                          end
                         else
                          begin
                            Message(parser_e_invalid_qualifier);
                            result:=false;
                          end;
                       until not try_to_consume(_COMMA);
                       consume(_RECKKLAMMER);
                     end;
                   else
                     begin
                       Message(parser_e_ill_property_access_sym);
                       result:=false;
                       break;
                     end;
                 end;
               until false;
             end
            else
             begin
               Message(parser_e_ill_property_access_sym);
               result:=false;
             end;
          end;

      var
         sym : tsym;
         p : tpropertysym;
         overriden : tsym;
         varspez : tvarspez;
         tt : ttype;
         arraytype : ttype;
         def : tdef;
         pt : tnode;
         sc : TFPObjectList;
         paranr : word;
         i      : longint;
         hreadparavs,
         hparavs      : tparavarsym;
         readprocdef,
         writeprocdef : tprocvardef;
      begin
         { Generate temp procvardefs to search for matching read/write
           procedures. the readprocdef will store all definitions }
         paranr:=0;
         readprocdef:=tprocvardef.create(normal_function_level);
         writeprocdef:=tprocvardef.create(normal_function_level);

         { make it method pointers }
         if assigned(aclass) then
           begin
             include(readprocdef.procoptions,po_methodpointer);
             include(writeprocdef.procoptions,po_methodpointer);
           end;

         if token<>_ID then
           begin
              consume(_ID);
              consume(_SEMICOLON);
              exit;
           end;
         { Generate propertysym and insert in symtablestack }
         p:=tpropertysym.create(orgpattern);
         symtablestack.top.insert(p);
         consume(_ID);
         { property parameters ? }
         if try_to_consume(_LECKKLAMMER) then
           begin
              if (sp_published in current_object_option) then
                Message(parser_e_cant_publish_that_property);
              { create a list of the parameters }
              symtablestack.push(readprocdef.parast);
              sc:=TFPObjectList.create(false);
              inc(testcurobject);
              repeat
                if try_to_consume(_VAR) then
                  varspez:=vs_var
                else if try_to_consume(_CONST) then
                  varspez:=vs_const
                else if (m_out in aktmodeswitches) and try_to_consume(_OUT) then
                  varspez:=vs_out
                else
                  varspez:=vs_value;
                sc.clear;
                repeat
                  inc(paranr);
                  hreadparavs:=tparavarsym.create(orgpattern,10*paranr,varspez,generrortype,[]);
                  readprocdef.parast.insert(hreadparavs);
                  sc.add(hreadparavs);
                  consume(_ID);
                until not try_to_consume(_COMMA);
                if try_to_consume(_COLON) then
                  begin
                    if try_to_consume(_ARRAY) then
                      begin
                        consume(_OF);
                        { define range and type of range }
                        tt.setdef(tarraydef.create(0,-1,s32inttype));
                        { define field type }
                        single_type(arraytype,false);
                        tarraydef(tt.def).setelementtype(arraytype);
                      end
                    else
                      single_type(tt,false);
                  end
                else
                  tt:=cformaltype;
                for i:=0 to sc.count-1 do
                  begin
                    hreadparavs:=tparavarsym(sc[i]);
                    hreadparavs.vartype:=tt;
                    { also update the writeprocdef }
                    hparavs:=tparavarsym.create(hreadparavs.realname,hreadparavs.paranr,vs_value,tt,[]);
                    writeprocdef.parast.insert(hparavs);
                  end;
              until not try_to_consume(_SEMICOLON);
              sc.free;
              dec(testcurobject);
              symtablestack.pop(readprocdef.parast);
              consume(_RECKKLAMMER);

              { the parser need to know if a property has parameters, the
                index parameter doesn't count (PFV) }
              if paranr>0 then
                include(p.propoptions,ppo_hasparameters);
           end;
         { overriden property ?                                 }
         { force property interface
             there is a property parameter
             a global property }
         if (token=_COLON) or (paranr>0) or (aclass=nil) then
           begin
              consume(_COLON);
              single_type(p.proptype,false);
              if (idtoken=_INDEX) then
                begin
                   consume(_INDEX);
                   pt:=comp_expr(true);
                   { Only allow enum and integer indexes. Convert all integer
                     values to s32int to be compatible with delphi, because the
                     procedure matching requires equal parameters }
                   if is_constnode(pt) and
                      is_ordinal(pt.resulttype.def)
{$ifndef cpu64}
                      and (not is_64bitint(pt.resulttype.def))
{$endif cpu64}
                      then
                     begin
                       if is_integer(pt.resulttype.def) then
                         inserttypeconv_internal(pt,s32inttype);
                       p.index:=tordconstnode(pt).value;
                     end
                   else
                     begin
                       Message(parser_e_invalid_property_index_value);
                       p.index:=0;
                     end;
                   p.indextype:=pt.resulttype;
                   include(p.propoptions,ppo_indexed);
                   { concat a longint to the para templates }
                   inc(paranr);
                   hparavs:=tparavarsym.create('$index',10*paranr,vs_value,p.indextype,[]);
                   readprocdef.parast.insert(hparavs);
                   hparavs:=tparavarsym.create('$index',10*paranr,vs_value,p.indextype,[]);
                   writeprocdef.parast.insert(hparavs);
                   pt.free;
                end;
           end
         else
           begin
              { do an property override }
              overriden:=search_class_member(aclass.childof,p.name);
              if assigned(overriden) and (overriden.typ=propertysym) then
                begin
                  p.dooverride(tpropertysym(overriden));
                end
              else
                begin
                  p.proptype:=generrortype;
                  message(parser_e_no_property_found_to_override);
                end;
           end;
         if (sp_published in current_object_option) and
            not(p.proptype.def.is_publishable) then
           Message(parser_e_cant_publish_that_property);

         if try_to_consume(_READ) then
          begin
            p.readaccess.clear;
            if parse_symlist(p.readaccess,def) then
             begin
               sym:=p.readaccess.firstsym^.sym;
               case sym.typ of
                 procsym :
                   begin
                     { read is function returning the type of the property }
                     readprocdef.rettype:=p.proptype;
                     { Insert hidden parameters }
                     handle_calling_convention(readprocdef);
                     { search procdefs matching readprocdef }
                     { we ignore hidden stuff here because the property access symbol might have
                       non default calling conventions which might change the hidden stuff;
                       see tw3216.pp (FK) }
                     p.readaccess.procdef:=Tprocsym(sym).search_procdef_bypara(readprocdef.paras,p.proptype.def,[cpo_allowdefaults,cpo_ignorehidden]);
                     if not assigned(p.readaccess.procdef) then
                       Message(parser_e_ill_property_access_sym);
                   end;
                 fieldvarsym :
                   begin
                     if not assigned(def) then
                       internalerror(200310071);
                     if compare_defs(def,p.proptype.def,nothingn)>=te_equal then
                      begin
                        { property parameters are allowed if this is
                          an indexed property, because the index is then
                          the parameter.
                          Note: In the help of Kylix it is written
                          that it isn't allowed, but the compiler accepts it (PFV) }
                        if (ppo_hasparameters in p.propoptions) then
                         Message(parser_e_ill_property_access_sym);
                      end
                     else
                      IncompatibleTypes(def,p.proptype.def);
                   end;
                 else
                   Message(parser_e_ill_property_access_sym);
               end;
             end;
          end;
         if try_to_consume(_WRITE) then
          begin
            p.writeaccess.clear;
            if parse_symlist(p.writeaccess,def) then
             begin
               sym:=p.writeaccess.firstsym^.sym;
               case sym.typ of
                 procsym :
                   begin
                     { write is a procedure with an extra value parameter
                       of the of the property }
                     writeprocdef.rettype:=voidtype;
                     inc(paranr);
                     hparavs:=tparavarsym.create('$value',10*paranr,vs_value,p.proptype,[]);
                     writeprocdef.parast.insert(hparavs);
                     { Insert hidden parameters }
                     handle_calling_convention(writeprocdef);
                     { search procdefs matching writeprocdef }
                     p.writeaccess.procdef:=Tprocsym(sym).search_procdef_bypara(writeprocdef.paras,writeprocdef.rettype.def,[cpo_allowdefaults]);
                     if not assigned(p.writeaccess.procdef) then
                       Message(parser_e_ill_property_access_sym);
                   end;
                 fieldvarsym :
                   begin
                     if not assigned(def) then
                       internalerror(200310072);
                     if compare_defs(def,p.proptype.def,nothingn)>=te_equal then
                      begin
                        { property parameters are allowed if this is
                          an indexed property, because the index is then
                          the parameter.
                          Note: In the help of Kylix it is written
                          that it isn't allowed, but the compiler accepts it (PFV) }
                        if (ppo_hasparameters in p.propoptions) then
                         Message(parser_e_ill_property_access_sym);
                      end
                     else
                      IncompatibleTypes(def,p.proptype.def);
                   end;
                 else
                   Message(parser_e_ill_property_access_sym);
               end;
             end;
          end;
         if assigned(aclass) then
           begin
             include(p.propoptions,ppo_stored);
             if try_to_consume(_STORED) then
              begin
                p.storedaccess.clear;
                case token of
                  _ID:
                    begin
                      { in the case that idtoken=_DEFAULT }
                      { we have to do nothing except      }
                      { setting ppo_stored, it's the same }
                      { as stored true                    }
                      if idtoken<>_DEFAULT then
                       begin
                         if parse_symlist(p.storedaccess,def) then
                          begin
                            sym:=p.storedaccess.firstsym^.sym;
                            case sym.typ of
                              procsym :
                                begin
                                   p.storedaccess.procdef:=Tprocsym(sym).search_procdef_nopara_boolret;
                                   if not assigned(p.storedaccess.procdef) then
                                     message(parser_e_ill_property_storage_sym);
                                end;
                              fieldvarsym :
                                begin
                                  if not assigned(def) then
                                    internalerror(200310073);
                                  if (ppo_hasparameters in p.propoptions) or
                                     not(is_boolean(def)) then
                                   Message(parser_e_stored_property_must_be_boolean);
                                end;
                              else
                                Message(parser_e_ill_property_access_sym);
                            end;
                          end;
                       end;
                    end;
                  _FALSE:
                    begin
                      consume(_FALSE);
                      exclude(p.propoptions,ppo_stored);
                    end;
                  _TRUE:
                    consume(_TRUE);
                end;
              end;
           end;
         if try_to_consume(_DEFAULT) then
           begin
              if not(is_ordinal(p.proptype.def) or
{$ifndef cpu64bit}
                     is_64bitint(p.proptype.def) or
{$endif cpu64bit}
                     is_class(p.proptype.def) or
                     is_single(p.proptype.def) or
                     (p.proptype.def.deftype in [classrefdef,pointerdef]) or
                     ((p.proptype.def.deftype=setdef) and
                      (tsetdef(p.proptype.def).settype=smallset))) or
                     ((p.proptype.def.deftype=arraydef) and
                      (ppo_indexed in p.propoptions)) or
                 (ppo_hasparameters in p.propoptions) then
                begin
                  Message(parser_e_property_cant_have_a_default_value);
                  { Error recovery }
                  pt:=comp_expr(true);
                  pt.free;
                end
              else
                begin
                  { Get the result of the default, the firstpass is
                    needed to support values like -1 }
                  pt:=comp_expr(true);
                  if (p.proptype.def.deftype=setdef) and
                     (pt.nodetype=arrayconstructorn) then
                    begin
                      arrayconstructor_to_set(pt);
                      do_resulttypepass(pt);
                    end;
                  inserttypeconv(pt,p.proptype);
                  if not(is_constnode(pt)) then
                    Message(parser_e_property_default_value_must_const);
                  { Set default value }
                  case pt.nodetype of
                    setconstn :
                      p.default:=plongint(tsetconstnode(pt).value_set)^;
                    ordconstn :
                      p.default:=longint(tordconstnode(pt).value);
                    niln :
                      p.default:=0;
                    realconstn:
                      p.default:=longint(single(trealconstnode(pt).value_real));
                  end;
                  pt.free;
                end;
           end
         else if try_to_consume(_NODEFAULT) then
           begin
              p.default:=0;
           end;
         { remove temporary procvardefs }
         readprocdef.free;
         writeprocdef.free;
         result:=p;
      end;


     function maybe_parse_proc_directives(const tt:ttype):boolean;
       var
         newtype : ttypesym;
       begin
         result:=false;
         { Process procvar directives before = and ; }
         if (tt.def.deftype=procvardef) and
            (tt.def.typesym=nil) and
            check_proc_directive(true) then
           begin
              newtype:=ttypesym.create('unnamed',tt);
              parse_var_proc_directives(tsym(newtype));
              newtype.restype.def:=nil;
              tt.def.typesym:=nil;
              newtype.free;
              result:=true;
           end;
       end;


    const
       variantrecordlevel : longint = 0;

    procedure read_var_decls(options:Tvar_dec_options);

      procedure read_default_value(sc : TFPObjectList;tt : ttype;is_threadvar : boolean);
        var
          vs : tabstractnormalvarsym;
          tcsym : ttypedconstsym;
        begin
          vs:=tabstractnormalvarsym(sc[0]);
          if sc.count>1 then
             Message(parser_e_initialized_only_one_var);
          if is_threadvar then
             Message(parser_e_initialized_not_for_threadvar);
          if symtablestack.top.symtabletype=localsymtable then
            begin
              consume(_EQUAL);
              tcsym:=ttypedconstsym.createtype('$default'+vs.realname,tt,false);
              include(tcsym.symoptions,sp_internal);
              vs.defaultconstsym:=tcsym;
              symtablestack.top.insert(tcsym);
              readtypedconst(current_asmdata.asmlists[al_typedconsts],tt,tcsym,false);
              { The variable has a value assigned }
              vs.varstate:=vs_initialised;
            end
          else
            begin
              tcsym:=ttypedconstsym.createtype(vs.realname,tt,true);
              tcsym.fileinfo:=vs.fileinfo;
              symtablestack.top.replace(vs,tcsym);
              vs.free;
              consume(_EQUAL);
              readtypedconst(current_asmdata.asmlists[al_typedconsts],tt,tcsym,true);
            end;
        end;

      var
         sc : TFPObjectList;
         i  : longint;
         old_block_type : tblock_type;
         symdone : boolean;
         { to handle absolute }
         abssym : tabsolutevarsym;
         { c var }
         is_dll,
         hasdefaultvalue,
         is_gpc_name,is_cdecl,
         extern_var,export_var : boolean;
         old_current_object_option : tsymoptions;
         hs,sorg,C_name,dll_name : string;
         tt : ttype;
         hp,pt : tnode;
         vs    : tabstractvarsym;
         hintsymoptions : tsymoptions;
         semicolonatend,semicoloneaten: boolean;
      begin
         old_current_object_option:=current_object_option;
         { all variables are public if not in a object declaration }
         current_object_option:=[sp_public];
         old_block_type:=block_type;
         block_type:=bt_type;
         is_gpc_name:=false;
         { Force an expected ID error message }
         if not (token in [_ID,_CASE,_END]) then
           consume(_ID);
         { read vars }
         sc:=TFPObjectList.create(false);
         while (token=_ID) do
           begin
             sorg:=orgpattern;
             semicoloneaten:=false;
             hasdefaultvalue:=false;
             symdone:=false;
             sc.clear;
             repeat
               if (token = _ID) then
                 begin
                   case symtablestack.top.symtabletype of
                     localsymtable :
                       vs:=tlocalvarsym.create(orgpattern,vs_value,generrortype,[]);
                     staticsymtable,
                     globalsymtable :
                       vs:=tglobalvarsym.create(orgpattern,vs_value,generrortype,[]);
                     else
                       internalerror(200411064);
                   end;
                   sc.add(vs);
                   symtablestack.top.insert(vs);
                 end;
               consume(_ID);
             until not try_to_consume(_COMMA);
             consume(_COLON);

             if (m_gpc in aktmodeswitches) and
                (token=_ID) and
                (orgpattern='__asmname__') then
               begin
                 consume(_ID);
                 C_name:=get_stringconst;
                 Is_gpc_name:=true;
               end;

             { this is needed for Delphi mode at least
               but should be OK for all modes !! (PM) }
             ignore_equal:=true;
             read_anon_type(tt,false);
             ignore_equal:=false;

             { Process procvar directives }
             if maybe_parse_proc_directives(tt) then
               semicoloneaten:=true;

             if is_gpc_name then
               begin
                  vs:=tabstractvarsym(sc[0]);
                  if sc.count>1 then
                    Message(parser_e_absolute_only_one_var);
                  vs.vartype:=tt;
                  if vs.typ=globalvarsym then
                    begin
                      tglobalvarsym(vs).set_mangledname(target_info.Cprefix+sorg);
                      include(vs.varoptions,vo_is_C_var);
                      include(vs.varoptions,vo_is_external);
                    end
                  else
                    Message(parser_e_no_local_var_external);
                  symdone:=true;
               end;

             { check for absolute }
             if not symdone and
                try_to_consume(_ABSOLUTE) then
              begin
                abssym:=nil;
                { only allowed for one var }
                vs:=tabstractvarsym(sc[0]);
                if sc.count>1 then
                  Message(parser_e_absolute_only_one_var);
                { parse the rest }
                pt:=expr;
                { check allowed absolute types }
                if (pt.nodetype=stringconstn) or
                   (is_constcharnode(pt)) then
                 begin
                   abssym:=tabsolutevarsym.create(vs.realname,tt);
                   abssym.fileinfo:=vs.fileinfo;
                   if pt.nodetype=stringconstn then
                     hs:=strpas(tstringconstnode(pt).value_str)
                   else
                     hs:=chr(tordconstnode(pt).value);
                   consume(token);
                   abssym.abstyp:=toasm;
                   abssym.asmname:=stringdup(hs);
                   { replace the varsym }
                   symtablestack.top.replace(vs,abssym);
                   vs.free;
                 end
                { address }
                else if is_constintnode(pt) and
                        ((target_info.system in [system_i386_go32v2,system_i386_watcom,
                                                 system_i386_wdosx,system_i386_win32,system_arm_wince,system_i386_wince]) or
                         (m_objfpc in aktmodeswitches) or
                         (m_delphi in aktmodeswitches)) then
                 begin
                   abssym:=tabsolutevarsym.create(vs.realname,tt);
                   abssym.fileinfo:=vs.fileinfo;
                   abssym.abstyp:=toaddr;
                   abssym.addroffset:=tordconstnode(pt).value;
{$ifdef i386}
                   abssym.absseg:=false;
                   if (target_info.system in [system_i386_go32v2,system_i386_watcom]) and
                      try_to_consume(_COLON) then
                    begin
                      pt.free;
                      pt:=expr;
                      if is_constintnode(pt) then
                        begin
                          abssym.addroffset:=abssym.addroffset shl 4+tordconstnode(pt).value;
                          abssym.absseg:=true;
                        end
                      else
                         Message(type_e_ordinal_expr_expected);
                    end;
{$endif i386}
                   symtablestack.top.replace(vs,abssym);
                   vs.free;
                 end
                { variable }
                else
                  begin
                    { remove subscriptn before checking for loadn }
                    hp:=pt;
                    while (hp.nodetype in [subscriptn,typeconvn,vecn]) do
                      hp:=tunarynode(hp).left;
                    if (hp.nodetype=loadn) then
                     begin
                       { we should check the result type of loadn }
                       if not (tloadnode(hp).symtableentry.typ in [fieldvarsym,globalvarsym,localvarsym,
                                                                   paravarsym,typedconstsym]) then
                         Message(parser_e_absolute_only_to_var_or_const);
                       abssym:=tabsolutevarsym.create(vs.realname,tt);
                       abssym.fileinfo:=vs.fileinfo;
                       abssym.abstyp:=tovar;
                       abssym.ref:=node_to_symlist(pt);
                       symtablestack.top.replace(vs,abssym);
                       vs.free;
                     end
                    else
                     Message(parser_e_absolute_only_to_var_or_const);
                  end;
                if assigned(abssym) then
                 begin
                   { try to consume the hint directives with absolute symbols }
                   hintsymoptions:=[];
                   try_consume_hintdirective(hintsymoptions);
                   abssym.symoptions := abssym.symoptions + hintsymoptions;
                 end;
                pt.free;
                symdone:=true;
              end;

             { try to parse the hint directives }
             hintsymoptions:=[];
             try_consume_hintdirective(hintsymoptions);

             { Handling of Delphi typed const = initialized vars }
             if (token=_EQUAL) and
                not(m_tp7 in aktmodeswitches) and
                (symtablestack.top.symtabletype<>parasymtable) then
               begin
                 { Add calling convention for procvar }
                 if (tt.def.deftype=procvardef) and
                    (tt.def.typesym=nil) then
                   handle_calling_convention(tprocvardef(tt.def));
                 read_default_value(sc,tt,vd_threadvar in options);
                 consume(_SEMICOLON);
                 { for locals we've created typedconstsym with a different name }
                 if symtablestack.top.symtabletype<>localsymtable then
                   symdone:=true;
                 hasdefaultvalue:=true;
               end
             else
               begin
                 if not(semicoloneaten) then
                   consume(_SEMICOLON);
               end;

             { Support calling convention for procvars after semicolon }
             if not(hasdefaultvalue) and
                (tt.def.deftype=procvardef) and
                (tt.def.typesym=nil) then
               begin
                 { Parse procvar directives after ; }
                 maybe_parse_proc_directives(tt);
                 { Add calling convention for procvar }
                 handle_calling_convention(tprocvardef(tt.def));
                 { Handling of Delphi typed const = initialized vars }
                 if (token=_EQUAL) and
                    not(m_tp7 in aktmodeswitches) and
                    (symtablestack.top.symtabletype<>parasymtable) then
                   begin
                     read_default_value(sc,tt,vd_threadvar in options);
                     consume(_SEMICOLON);
                     symdone:=true;
                     hasdefaultvalue:=true;
                   end;
               end;

             { Check for EXTERNAL etc directives or, in macpas, if cs_external_var is set}
             if not symdone then
              begin
                if (
                     (token=_ID) and
                     (m_cvar_support in aktmodeswitches) and
                     (idtoken in [_EXPORT,_EXTERNAL,_PUBLIC,_CVAR])
                   ) or
                   (
                     (m_mac in aktmodeswitches) and
                     ((cs_external_var in aktlocalswitches) or (cs_externally_visible in aktlocalswitches))
                   ) then
                 begin
                   { only allowed for one var }
                   vs:=tabstractvarsym(sc[0]);
                   if sc.count>1 then
                     Message(parser_e_absolute_only_one_var);
                   { set type of the var }
                   vs.vartype:=tt;
                   vs.symoptions := vs.symoptions + hintsymoptions;
                   { defaults }
                   is_dll:=false;
                   is_cdecl:=false;
                   extern_var:=false;
                   export_var:=false;
                   C_name:=sorg;
                   semicolonatend:= false;
                   { cdecl }
                   if try_to_consume(_CVAR) then
                    begin
                      consume(_SEMICOLON);
                      is_cdecl:=true;
                      C_name:=target_info.Cprefix+sorg;
                    end;
                   { external }
                   if try_to_consume(_EXTERNAL) then
                    begin
                      extern_var:=true;
                      semicolonatend:= true;
                    end;
                   { macpas specific handling due to some switches}
                   if (m_mac in aktmodeswitches) then
                     begin
                       if (cs_external_var in aktlocalswitches) then
                         begin {The effect of this is the same as if cvar; external; has been given as directives.}
                           is_cdecl:=true;
                           C_name:=target_info.Cprefix+sorg;
                           extern_var:=true;
                         end
                       else if (cs_externally_visible in aktlocalswitches) then
                         begin {The effect of this is the same as if cvar has been given as directives.}
                           is_cdecl:=true;
                           C_name:=target_info.Cprefix+sorg;
                         end;
                       vs.varregable := vr_none;
                     end;
                   { export }
                   if idtoken in [_EXPORT,_PUBLIC] then
                    begin
                      consume(_ID);
                      if extern_var then
                       Message(parser_e_not_external_and_export)
                      else
                       begin
                         export_var:=true;
                         semicolonatend:= true;
                       end;
                    end;
                   { external and export need a name after when no cdecl is used }
                   if not is_cdecl then
                    begin
                      { dll name ? }
                      if (extern_var) and (idtoken<>_NAME) then
                       begin
                         is_dll:=true;
                         dll_name:=get_stringconst;
                       end;
                      if try_to_consume(_NAME) then
                        C_name:=get_stringconst
                      else
                        C_name:=sorg;
                    end;
                   { consume the ; when export or external is used }
                   if semicolonatend then
                    consume(_SEMICOLON);

                   { set some vars options }
                   if is_dll then
                    include(vs.varoptions,vo_is_dll_var)
                   else
                    include(vs.varoptions,vo_is_C_var);

                   if (is_dll) and
                      (target_info.system in [system_powerpc_darwin,system_i386_darwin]) then
                     C_Name := target_info.Cprefix+C_Name;

                   if export_var then
                    begin
                      inc(vs.refs);
                      include(vs.varoptions,vo_is_exported);
                    end;

                   if extern_var then
                    include(vs.varoptions,vo_is_external);

                   if vs.typ=globalvarsym then
                     begin
                       tglobalvarsym(vs).set_mangledname(C_Name);
                       { insert in the al_globals when it is not external }
                       if (not extern_var) then
                         insertbssdata(tglobalvarsym(vs));
                       { now we can insert it in the import lib if its a dll, or
                         add it to the externals }
                       if extern_var then
                        begin
                          vs.varregable := vr_none;
                          if is_dll then
                           begin
                             if not(current_module.uses_imports) then
                              begin
                                current_module.uses_imports:=true;
                                importlib.preparelib(current_module.realmodulename^);
                              end;
                             importlib.importvariable(tglobalvarsym(vs),C_name,dll_name);
                           end
                          else
                           if tf_has_dllscanner in target_info.flags then
                            current_module.Externals.insert(tExternalsItem.create(vs.mangledname));
                        end;
                     end
                   else
                     Message(parser_e_no_local_var_external);
                   symdone:=true;
                 end;
              end;

             { insert it in the symtable, if not done yet }
             if not symdone then
               begin
                 for i:=0 to sc.count-1 do
                   begin
                     vs:=tabstractvarsym(sc[i]);
                     vs.vartype:=tt;
                     { insert any additional hint directives }
                     vs.symoptions := vs.symoptions + hintsymoptions;
                     if vd_threadvar in options then
                       include(vs.varoptions,vo_is_thread_var);
                     { static data fields are inserted in the globalsymtable }
                     if vs.typ=globalvarsym then
                       insertbssdata(tglobalvarsym(vs));
                   end;
               end;
           end;
         block_type:=old_block_type;
         current_object_option:=old_current_object_option;
         { free the list }
         sc.free;
      end;


    procedure read_record_fields(options:Tvar_dec_options);
      var
         sc : TFPObjectList;
         i  : longint;
         old_block_type : tblock_type;
         old_current_object_option : tsymoptions;
         hs,sorg : string;
         tt,casetype : ttype;
         { maxsize contains the max. size of a variant }
         { startvarrec contains the start of the variant part of a record }
         maxsize, startvarrecsize : longint;
         usedalign,
         maxalignment,startvarrecalign,
         maxpadalign, startpadalign: shortint;
         pt : tnode;
         fieldvs   : tfieldvarsym;
         hstaticvs : tglobalvarsym;
         vs    : tabstractvarsym;
         srsym : tsym;
         srsymtable : tsymtable;
         recst : tabstractrecordsymtable;
         unionsymtable : trecordsymtable;
         offset : longint;
         uniondef : trecorddef;
         unionsym : tfieldvarsym;
         uniontype : ttype;
         hintsymoptions : tsymoptions;
         semicoloneaten: boolean;
{$ifdef powerpc}
         tempdef: tdef;
         is_first_field: boolean;
{$endif powerpc}
      begin
         recst:=tabstractrecordsymtable(symtablestack.top);
{$ifdef powerpc}
         is_first_field := true;
{$endif powerpc}
         old_current_object_option:=current_object_option;
         { all variables are public if not in a object declaration }
         if not(vd_object in options) then
          current_object_option:=[sp_public];
         old_block_type:=block_type;
         block_type:=bt_type;
         { Force an expected ID error message }
         if not (token in [_ID,_CASE,_END]) then
          consume(_ID);
         { read vars }
         sc:=TFPObjectList.create(false);
         while (token=_ID) and
            not((vd_object in options) and
                (idtoken in [_PUBLIC,_PRIVATE,_PUBLISHED,_PROTECTED,_STRICT])) do
           begin
             sorg:=orgpattern;
             semicoloneaten:=false;
             sc.clear;
             repeat
               if try_to_consume(_ID) then
                 begin
                   vs:=tfieldvarsym.create(orgpattern,vs_value,generrortype,[]);
                   sc.add(vs);
                   recst.insert(vs);
                 end;
             until not try_to_consume(_COMMA);
             consume(_COLON);

             { Don't search in the recordsymtable for types }
             if ([df_generic,df_specialization]*tdef(recst.defowner).defoptions=[]) then
               symtablestack.pop(recst);
             ignore_equal:=true;
             read_anon_type(tt,false);
             ignore_equal:=false;
             if ([df_generic,df_specialization]*tdef(recst.defowner).defoptions=[]) then
               symtablestack.push(recst);

             { Process procvar directives }
             if maybe_parse_proc_directives(tt) then
               semicoloneaten:=true;

{$ifdef powerpc}
             { from gcc/gcc/config/rs6000/rs6000.h:
              /* APPLE LOCAL begin Macintosh alignment 2002-1-22 ff */
              /* Return the alignment of a struct based on the Macintosh PowerPC
                 alignment rules.  In general the alignment of a struct is
                 determined by the greatest alignment of its elements.  However, the
                 PowerPC rules cause the alignment of a struct to peg at word
                 alignment except when the first field has greater than word
                 (32-bit) alignment, in which case the alignment is determined by
                 the alignment of the first field.  */
             }
             if (target_info.system in [system_powerpc_darwin, system_powerpc_macos]) and
                is_first_field and
                (trecordsymtable(symtablestack).usefieldalignment = -1) then
               begin
                 tempdef := tt.def;
                 while tempdef.deftype = arraydef do
                   tempdef := tarraydef(tempdef).elementtype.def;
                 if tempdef.deftype <> recorddef then
                   maxpadalign := tempdef.alignment
                 else
                   maxpadalign := trecorddef(tempdef).padalignment;

                 if (maxpadalign > 4) and
                    (maxpadalign > trecordsymtable(symtablestack).padalignment) then
                   trecordsymtable(symtablestack).padalignment := maxpadalign;
                 is_first_field := false;
               end;
{$endif powerpc}

             { types that use init/final are not allowed in variant parts, but
               classes are allowed }
             if (variantrecordlevel>0) and
                (tt.def.needs_inittable and not is_class(tt.def)) then
               Message(parser_e_cant_use_inittable_here);

             { try to parse the hint directives }
             hintsymoptions:=[];
             try_consume_hintdirective(hintsymoptions);

             { Records and objects can't have default values }
             { for a record there doesn't need to be a ; before the END or )    }
             if not(token in [_END,_RKLAMMER]) and
                not(semicoloneaten) then
               consume(_SEMICOLON);

             { Parse procvar directives after ; }
             maybe_parse_proc_directives(tt);

             { Add calling convention for procvar }
             if (tt.def.deftype=procvardef) and
                (tt.def.typesym=nil) then
               handle_calling_convention(tprocvardef(tt.def));

             { Check for STATIC directive }
             if (vd_object in options) and
                (cs_static_keyword in aktmoduleswitches) and
                (try_to_consume(_STATIC)) then
               begin
                 include(current_object_option,sp_static);
                 consume(_SEMICOLON);
               end;

             if (sp_published in current_object_option) and
                not(is_class(tt.def)) then
               begin
                 Message(parser_e_cant_publish_that);
                 exclude(current_object_option,sp_published);
                 { recover by changing access type to public }
                 for i:=0 to sc.count-1 do
                   begin
                     fieldvs:=tfieldvarsym(sc[i]);
                     exclude(fieldvs.symoptions,sp_published);
                     include(fieldvs.symoptions,sp_public);
                   end;
               end
             else
              if (sp_published in current_object_option) and
                 not(oo_can_have_published in tobjectdef(tt.def).objectoptions) then
               begin
                 Message(parser_e_only_publishable_classes_can__be_published);
                 exclude(current_object_option,sp_published);
               end;

             { update variable options }
             for i:=0 to sc.count-1 do
               begin
                 fieldvs:=tfieldvarsym(sc[i]);
                 fieldvs.vartype:=tt;
                 { insert any additional hint directives }
                 fieldvs.symoptions := fieldvs.symoptions + hintsymoptions;
                 if (sp_static in current_object_option) then
                   include(fieldvs.symoptions,sp_static);
                 { static data fields are inserted in the globalsymtable }
                 if (sp_static in current_object_option) then
                   begin
                      hstaticvs:=tglobalvarsym.create('$'+lower(symtablestack.top.name^)+'_'+fieldvs.name,vs_value,tt,[]);
                      recst.defowner.owner.insert(hstaticvs);
                      insertbssdata(hstaticvs);
                   end
                 else
                   recst.addfield(fieldvs);
               end;

             { restore current_object_option, it can be changed for
               publishing or static }
             current_object_option:=old_current_object_option;
           end;

         { Check for Case }
         if (vd_record in options) and
            try_to_consume(_CASE) then
           begin
              maxsize:=0;
              maxalignment:=0;
              maxpadalign:=0;
              { including a field declaration? }
              fieldvs:=nil;
              sorg:=orgpattern;
              hs:=pattern;
              searchsym(hs,srsym,srsymtable);
              if not(assigned(srsym) and (srsym.typ in [typesym,unitsym])) then
                begin
                  consume(_ID);
                  consume(_COLON);
                  fieldvs:=tfieldvarsym.create(sorg,vs_value,generrortype,[]);
                  symtablestack.top.insert(fieldvs);
                end;
              read_anon_type(casetype,true);
              if assigned(fieldvs) then
                begin
                  fieldvs.vartype:=casetype;
                  recst.addfield(fieldvs);
                end;
              if not(is_ordinal(casetype.def))
{$ifndef cpu64bit}
                 or is_64bitint(casetype.def)
{$endif cpu64bit}
                 then
                Message(type_e_ordinal_expr_expected);
              consume(_OF);

              UnionSymtable:=trecordsymtable.create(aktpackrecords);
              UnionDef:=trecorddef.create(unionsymtable);
              uniondef.isunion:=true;
              startvarrecsize:=UnionSymtable.datasize;
              startvarrecalign:=UnionSymtable.fieldalignment;
              startpadalign:=Unionsymtable.padalignment;
              symtablestack.push(UnionSymtable);
              repeat
                repeat
                  pt:=comp_expr(true);
                  if not(pt.nodetype=ordconstn) then
                    Message(parser_e_illegal_expression);
                  pt.free;
                  if token=_COMMA then
                    consume(_COMMA)
                  else
                    break;
                until false;
                consume(_COLON);
                { read the vars }
                consume(_LKLAMMER);
                inc(variantrecordlevel);
                if token<>_RKLAMMER then
                  read_record_fields([vd_record]);
                dec(variantrecordlevel);
                consume(_RKLAMMER);
                { calculates maximal variant size }
                maxsize:=max(maxsize,unionsymtable.datasize);
                maxalignment:=max(maxalignment,unionsymtable.fieldalignment);
                maxpadalign:=max(maxpadalign,unionsymtable.padalignment);
                { the items of the next variant are overlayed }
                unionsymtable.datasize:=startvarrecsize;
                unionsymtable.fieldalignment:=startvarrecalign;
                unionsymtable.padalignment:=startpadalign;
                if (token<>_END) and (token<>_RKLAMMER) then
                  consume(_SEMICOLON)
                else
                  break;
              until (token=_END) or (token=_RKLAMMER);
              symtablestack.pop(UnionSymtable);
              { at last set the record size to that of the biggest variant }
              unionsymtable.datasize:=maxsize;
              unionsymtable.fieldalignment:=maxalignment;
              uniontype.def:=uniondef;
              uniontype.sym:=nil;
              UnionSym:=tfieldvarsym.create('$case',vs_value,uniontype,[]);
              unionsymtable.addalignmentpadding;
{$ifdef powerpc}
              { parent inherits the alignment padding if the variant is the first "field" of the parent record/variant }
              if (target_info.system in [system_powerpc_darwin, system_powerpc_macos]) and
                 is_first_field and
                 (recst.usefieldalignment = -1) and
                 (maxpadalign > recst.padalignment) then
                recst.padalignment:=maxpadalign;
{$endif powerpc}
              { Align the offset where the union symtable is added }
              if (recst.usefieldalignment=-1) then
                usedalign:=used_align(unionsymtable.recordalignment,aktalignment.recordalignmin,aktalignment.maxCrecordalign)
              else
                usedalign:=used_align(unionsymtable.recordalignment,aktalignment.recordalignmin,aktalignment.recordalignmax);

              offset:=align(recst.datasize,usedalign);
              recst.datasize:=offset+unionsymtable.datasize;

              if unionsymtable.recordalignment>recst.fieldalignment then
                recst.fieldalignment:=unionsymtable.recordalignment;

              trecordsymtable(recst).insertunionst(Unionsymtable,offset);
              unionsym.free;
              uniondef.free;
           end;
         block_type:=old_block_type;
         current_object_option:=old_current_object_option;
         { free the list }
         sc.free;
{$ifdef powerpc}
         is_first_field := false;
{$endif powerpc}
      end;

end.
