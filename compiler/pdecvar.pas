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

    function read_property_dec(aclass:tobjectdef):tpropertysym;

    procedure read_var_decs(is_record,is_object,is_threadvar:boolean);


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
       node,pass_1,
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
                             sym:=searchsymonlyin(st,pattern);
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
         propname : stringid;
         sc : tsinglelist;
         paranr : word;
         oldregisterdef : boolean;
         hreadparavs,
         hparavs      : tparavarsym;
         readprocdef,
         writeprocdef : tprocvardef;
         oldsymtablestack : tsymtable;
      begin
         { Generate temp procvardefs to search for matching read/write
           procedures. the readprocdef will store all definitions }
         oldregisterdef:=registerdef;
         registerdef:=false;
         paranr:=0;
         readprocdef:=tprocvardef.create(normal_function_level);
         writeprocdef:=tprocvardef.create(normal_function_level);
         registerdef:=oldregisterdef;

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
         symtablestack.insert(p);
         propname:=pattern;
         consume(_ID);
         { Set the symtablestack to the parast of readprop so
           temp defs will be destroyed after declaration }
         readprocdef.parast.next:=symtablestack;
         symtablestack:=readprocdef.parast;
         { property parameters ? }
         if token=_LECKKLAMMER then
           begin
              if (sp_published in current_object_option) and
                not (m_delphi in aktmodeswitches) then
                Message(parser_e_cant_publish_that_property);

              { create a list of the parameters }
              sc:=tsinglelist.create;
              consume(_LECKKLAMMER);
              inc(testcurobject);
              repeat
                if token=_VAR then
                  begin
                     consume(_VAR);
                     varspez:=vs_var;
                  end
                else if token=_CONST then
                  begin
                     consume(_CONST);
                     varspez:=vs_const;
                  end
                else if (idtoken=_OUT) and (m_out in aktmodeswitches) then
                  begin
                     consume(_OUT);
                     varspez:=vs_out;
                  end
                else
                  varspez:=vs_value;
                sc.reset;
                repeat
                  inc(paranr);
                  hreadparavs:=tparavarsym.create(orgpattern,10*paranr,varspez,generrortype,[]);
                  readprocdef.parast.insert(hreadparavs);
                  sc.insert(hreadparavs);
                  consume(_ID);
                until not try_to_consume(_COMMA);
                if try_to_consume(_COLON) then
                  begin
                    { for records, don't search the recordsymtable for
                     the symbols of the types }
                    oldsymtablestack:=symtablestack;
                    symtablestack:=symtablestack.next;
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
                    symtablestack:=oldsymtablestack;
                  end
                else
                  tt:=cformaltype;
                hreadparavs:=tparavarsym(sc.first);
                while assigned(hreadparavs) do
                  begin
                    hreadparavs.vartype:=tt;
                    { also update the writeprocdef }
                    hparavs:=tparavarsym.create(hreadparavs.realname,hreadparavs.paranr,vs_value,tt,[]);
                    writeprocdef.parast.insert(hparavs);
                    hreadparavs:=tparavarsym(hreadparavs.listnext);
                  end;
              until not try_to_consume(_SEMICOLON);
              sc.free;
              dec(testcurobject);
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
              { insert types in global symtable }
              oldsymtablestack:=symtablestack;
              while not(symtablestack.symtabletype in [globalsymtable,staticsymtable]) do
                symtablestack:=symtablestack.next;
              single_type(p.proptype,false);
              symtablestack:=oldsymtablestack;
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
              overriden:=search_class_member(aclass.childof,propname);
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
             { ppo_stored might be not set by an overridden property }
             if not(ppo_is_override in p.propoptions) then
               include(p.propoptions,ppo_stored);
             if try_to_consume(_STORED) then
              begin
                include(p.propoptions,ppo_stored);
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
                    begin
                    p.default:=longint($80000000);
                    consume(_TRUE);
                    end;
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
              p.default:=longint($80000000);
           end;
         { remove temporary procvardefs }
         symtablestack:=symtablestack.next;
         readprocdef.free;
         writeprocdef.free;
         result:=p;
      end;


    const
       variantrecordlevel : longint = 0;

    procedure read_var_decs(is_record,is_object,is_threadvar:boolean);
    { reads the filed of a record into a        }
    { symtablestack, if record=false        }
    { variants are forbidden, so this procedure }
    { can be used to read object fields  }
    { if absolute is true, ABSOLUTE and file    }
    { types are allowed                  }
    { => the procedure is also used to read     }
    { a sequence of variable declaration        }

      procedure insert_syms(sc : tsinglelist;tt : ttype;is_threadvar : boolean; addsymopts : tsymoptions);
      { inserts the symbols of sc in st with def as definition or sym as ttypesym, sc is disposed }
        var
          vs : tabstractvarsym;
          hstaticvs : tglobalvarsym;
        begin
           vs:=tabstractvarsym(sc.first);
           while assigned(vs) do
             begin
                vs.vartype:=tt;
                { insert any additional hint directives }
                vs.symoptions := vs.symoptions + addsymopts;
                if (sp_static in current_object_option) then
                  include(vs.symoptions,sp_static);
                if is_threadvar then
                  include(vs.varoptions,vo_is_thread_var);
                { static data fields are inserted in the globalsymtable }
                if (symtablestack.symtabletype=objectsymtable) and
                   (sp_static in current_object_option) then
                  begin
                     hstaticvs:=tglobalvarsym.create('$'+lower(symtablestack.name^)+'_'+vs.name,vs_value,tt,[]);
                     symtablestack.defowner.owner.insert(hstaticvs);
                     insertbssdata(hstaticvs);
                  end
                else
                  begin
                    { external data is not possible here }
                    case symtablestack.symtabletype of
                      globalsymtable,
                      staticsymtable :
                        insertbssdata(tglobalvarsym(vs));
                      recordsymtable,
                      objectsymtable :
                        tabstractrecordsymtable(symtablestack).insertfield(tfieldvarsym(vs),false);
                    end;
                  end;
                vs:=tabstractvarsym(vs.listnext);
             end;
        end;


      procedure read_default_value(sc : tsinglelist;tt : ttype;is_threadvar : boolean);
        var
          vs : tabstractnormalvarsym;
          tcsym : ttypedconstsym;
        begin
          vs:=tabstractnormalvarsym(sc.first);
          if assigned(vs.listnext) then
             Message(parser_e_initialized_only_one_var);
          if is_threadvar then
             Message(parser_e_initialized_not_for_threadvar);
          if symtablestack.symtabletype=localsymtable then
            begin
              consume(_EQUAL);
              tcsym:=ttypedconstsym.createtype('$default'+vs.realname,tt,false);
              include(tcsym.symoptions,sp_internal);
              vs.defaultconstsym:=tcsym;
              symtablestack.insert(tcsym);
              readtypedconst(tt,tcsym,false);
              { The variable has a value assigned }
              vs.varstate:=vs_initialised;
            end
          else
            begin
              tcsym:=ttypedconstsym.createtype(vs.realname,tt,true);
              tcsym.fileinfo:=vs.fileinfo;
              symtablestack.replace(vs,tcsym);
              vs.free;
              consume(_EQUAL);
              readtypedconst(tt,tcsym,true);
            end;
        end;

      var
         sc : tsinglelist;
         old_block_type : tblock_type;
         symdone : boolean;
         { to handle absolute }
         abssym : tabsolutevarsym;
         { c var }
         newtype : ttypesym;
         is_dll,
         hasdefaultvalue,
         is_gpc_name,is_cdecl,
         extern_var,export_var : boolean;
         old_current_object_option : tsymoptions;
         hs,sorg,C_name,dll_name : string;
         tt,casetype : ttype;
         { maxsize contains the max. size of a variant }
         { startvarrec contains the start of the variant part of a record }
         maxsize, startvarrecsize : longint;
         usedalign,
         maxalignment,startvarrecalign,
         maxpadalign, startpadalign: shortint;
         hp,pt : tnode;
         fieldvs   : tfieldvarsym;
         vs,vs2    : tabstractvarsym;
         srsym : tsym;
         oldsymtablestack,
         srsymtable : tsymtable;
         unionsymtable : trecordsymtable;
         offset : longint;
         uniondef : trecorddef;
         unionsym : tfieldvarsym;
         uniontype : ttype;
         dummysymoptions : tsymoptions;
         semicolonatend,semicoloneaten: boolean;
{$ifdef powerpc}
         tempdef: tdef;
         is_first_field: boolean;
{$endif powerpc}
      begin
{$ifdef powerpc}
        is_first_field := true;
{$endif powerpc}
         old_current_object_option:=current_object_option;
         { all variables are public if not in a object declaration }
         if not is_object then
          current_object_option:=[sp_public];
         old_block_type:=block_type;
         block_type:=bt_type;
         is_gpc_name:=false;
         { Force an expected ID error message }
         if not (token in [_ID,_CASE,_END]) then
          consume(_ID);
         { read vars }
         sc:=tsinglelist.create;
         while (token=_ID) and
               not(is_object and (idtoken in [_PUBLIC,_PRIVATE,_PUBLISHED,_PROTECTED,_STRICT])) do
           begin
             sorg:=orgpattern;
             semicoloneaten:=false;
             hasdefaultvalue:=false;
             symdone:=false;
             sc.reset;
             repeat
               if (token = _ID) then
                 begin
                   case symtablestack.symtabletype of
                     localsymtable :
                       vs:=tlocalvarsym.create(orgpattern,vs_value,generrortype,[]);
                     staticsymtable,
                     globalsymtable :
                       vs:=tglobalvarsym.create(orgpattern,vs_value,generrortype,[]);
                     recordsymtable,
                     objectsymtable :
                       vs:=tfieldvarsym.create(orgpattern,vs_value,generrortype,[]);
                     else
                       internalerror(200411064);
                   end;
                   symtablestack.insert(vs);
                   if assigned(vs.owner) then
                     sc.insert(vs)
                   else
                     vs.free;
                 end;
               consume(_ID);
             until not try_to_consume(_COMMA);
             consume(_COLON);
             if (m_gpc in aktmodeswitches) and
                not(is_record or is_object or is_threadvar) and
                (token=_ID) and (orgpattern='__asmname__') then
               begin
                 consume(_ID);
                 C_name:=get_stringconst;
                 Is_gpc_name:=true;
               end;
             { this is needed for Delphi mode at least
               but should be OK for all modes !! (PM) }
             ignore_equal:=true;
             if is_record or is_object then
              begin
                { for records, don't search the recordsymtable for
                  the symbols of the types }
                oldsymtablestack:=symtablestack;
                symtablestack:=symtablestack.next;
                read_type(tt,'',false);
                symtablestack:=oldsymtablestack;
              end
             else
              read_type(tt,'',false);
             ignore_equal:=false;
             { Process procvar directives }
             if (tt.def.deftype=procvardef) and
                (tt.def.typesym=nil) and
                check_proc_directive(true) then
               begin
                  newtype:=ttypesym.create('unnamed',tt);
                  parse_var_proc_directives(tsym(newtype));
                  semicoloneaten:=true;
                  newtype.restype.def:=nil;
                  tt.def.typesym:=nil;
                  newtype.free;
               end;

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
                  is_record and
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

             if is_gpc_name then
               begin
                  vs:=tabstractvarsym(sc.first);
                  if assigned(vs.listnext) then
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
                (idtoken=_ABSOLUTE) and not(is_record or is_object or is_threadvar) then
              begin
                consume(_ABSOLUTE);
                abssym:=nil;
                { only allowed for one var }
                vs:=tabstractvarsym(sc.first);
                if assigned(vs.listnext) then
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
                   symtablestack.replace(vs,abssym);
                   vs.free;
                 end
                { address }
                else if is_constintnode(pt) and
                        ((target_info.system in [system_i386_go32v2,system_i386_watcom,
                                                 system_i386_wdosx,system_i386_win32]) or
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
                   symtablestack.replace(vs,abssym);
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
                       symtablestack.replace(vs,abssym);
                       vs.free;
                     end
                    else
                     Message(parser_e_absolute_only_to_var_or_const);
                  end;
                if assigned(abssym) then
                 begin
                   { try to consume the hint directives with absolute symbols }
                   dummysymoptions:=[];
                   try_consume_hintdirective(dummysymoptions);
                   abssym.symoptions := abssym.symoptions + dummysymoptions;
                 end;
                pt.free;
                symdone:=true;
              end;

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
               end;

             { try to parse the hint directives }
             dummysymoptions:=[];
             try_consume_hintdirective(dummysymoptions);

             { Records and objects can't have default values }
             if is_record or is_object then
               begin
                 { for a record there doesn't need to be a ; before the END or )    }
                 if not(token in [_END,_RKLAMMER]) and
                    not(semicoloneaten) then
                   consume(_SEMICOLON);
               end
             else
             { Handling of Delphi typed const = initialized vars }
               if (token=_EQUAL) and
                  not(m_tp7 in aktmodeswitches) and
                  (symtablestack.symtabletype<>parasymtable) then
                 begin
                   { Add calling convention for procvar }
                   if (tt.def.deftype=procvardef) and
                      (tt.def.typesym=nil) then
                     handle_calling_convention(tprocvardef(tt.def));
                   read_default_value(sc,tt,is_threadvar);
                   consume(_SEMICOLON);
                   { for locals we've created typedconstsym with a different name }
                   if symtablestack.symtabletype<>localsymtable then
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
                 if check_proc_directive(true) then
                   begin
                     newtype:=ttypesym.create('unnamed',tt);
                     parse_var_proc_directives(tsym(newtype));
                     newtype.restype.def:=nil;
                     tt.def.typesym:=nil;
                     newtype.free;
                   end;
                 { Add calling convention for procvar }
                 handle_calling_convention(tprocvardef(tt.def));
                 { Handling of Delphi typed const = initialized vars }
                 if (token=_EQUAL) and
                    not(is_record or is_object) and
                    not(m_tp7 in aktmodeswitches) and
                    (symtablestack.symtabletype<>parasymtable) then
                   begin
                     read_default_value(sc,tt,is_threadvar);
                     consume(_SEMICOLON);
                     symdone:=true;
                     hasdefaultvalue:=true;
                   end;
               end;

             { Check for EXTERNAL etc directives or, in macpas, if cs_external_var is set}
             if not symdone and not(is_record or is_object or is_threadvar) then
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
                   vs:=tabstractvarsym(sc.first);
                   if assigned(vs.listnext) then
                     Message(parser_e_absolute_only_one_var);
                   { set type of the var }
                   vs.vartype:=tt;
                   vs.symoptions := vs.symoptions + dummysymoptions;
                   { defaults }
                   is_dll:=false;
                   is_cdecl:=false;
                   extern_var:=false;
                   export_var:=false;
                   C_name:=sorg;
                   semicolonatend:= false;
                   { cdecl }
                   if idtoken=_CVAR then
                    begin
                      consume(_CVAR);
                      consume(_SEMICOLON);
                      is_cdecl:=true;
                      C_name:=target_info.Cprefix+sorg;
                    end;
                   { external }
                   if idtoken=_EXTERNAL then
                    begin
                      consume(_EXTERNAL);
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
                      (target_info.system = system_powerpc_darwin) then
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
                       { insert in the datasegment when it is not external }
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
                           if target_info.DllScanSupported then
                            current_module.Externals.insert(tExternalsItem.create(vs.mangledname));
                        end;
                     end
                   else
                     Message(parser_e_no_local_var_external);
                   symdone:=true;
                 end;
              end;

             { Check for STATIC directive }
             if not symdone and (is_object) and
               (cs_static_keyword in aktmoduleswitches) and (idtoken=_STATIC) then
                  begin
                    include(current_object_option,sp_static);
                    consume(_STATIC);
                    consume(_SEMICOLON);
                  end;

             { insert it in the symtable, if not done yet }
             if not symdone then
               begin
                  { save object option, because we can turn of the sp_published }
                  if (sp_published in current_object_option) and
                    not(is_class(tt.def)) then
                   begin
                     Message(parser_e_cant_publish_that);
                     exclude(current_object_option,sp_published);
                     { recover by changing access type to public }
                     vs2:=tabstractvarsym(sc.first);
                     while assigned (vs2) do
                       begin
                         exclude(vs2.symoptions,sp_published);
                         include(vs2.symoptions,sp_public);
                         vs2:=tabstractvarsym(vs2.listnext);
                       end;
                   end
                  else
                   if (sp_published in current_object_option) and
                      not(oo_can_have_published in tobjectdef(tt.def).objectoptions) then
                    begin
                      Message(parser_e_only_publishable_classes_can__be_published);
                      exclude(current_object_option,sp_published);
                    end;
                  insert_syms(sc,tt,is_threadvar,dummysymoptions);
                  current_object_option:=old_current_object_option;
               end;

           end;

         { Check for Case }
         if is_record and (token=_CASE) then
           begin
              maxsize:=0;
              maxalignment:=0;
              maxpadalign:=0;
              consume(_CASE);
              sorg:=orgpattern;
              hs:=pattern;
              searchsym(hs,srsym,srsymtable);
              { may be only a type: }
              if assigned(srsym) and (srsym.typ in [typesym,unitsym]) then
               begin
                 { for records, don't search the recordsymtable for
                   the symbols of the types }
                 oldsymtablestack:=symtablestack;
                 symtablestack:=symtablestack.next;
                 read_type(casetype,'',true);
                 symtablestack:=oldsymtablestack;
               end
              else
                begin
                  consume(_ID);
                  consume(_COLON);
                  { for records, don't search the recordsymtable for
                    the symbols of the types }
                  oldsymtablestack:=symtablestack;
                  symtablestack:=symtablestack.next;
                  read_type(casetype,'',true);
                  symtablestack:=oldsymtablestack;
                  fieldvs:=tfieldvarsym.create(sorg,vs_value,casetype,[]);
                  tabstractrecordsymtable(symtablestack).insertfield(fieldvs,true);
                end;
              if not(is_ordinal(casetype.def))
{$ifndef cpu64bit}
                 or is_64bitint(casetype.def)
{$endif cpu64bit}
                 then
                Message(type_e_ordinal_expr_expected);
              consume(_OF);
              UnionSymtable:=trecordsymtable.create(aktpackrecords);
              Unionsymtable.next:=symtablestack;
              registerdef:=false;
              UnionDef:=trecorddef.create(unionsymtable);
              uniondef.isunion:=true;
              if assigned(symtablestack.defowner) then
                Uniondef.owner:=symtablestack.defowner.owner;
              registerdef:=true;
              startvarrecsize:=UnionSymtable.datasize;
              startvarrecalign:=UnionSymtable.fieldalignment;
              startpadalign:=Unionsymtable.padalignment;
              symtablestack:=UnionSymtable;
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
                  read_var_decs(true,false,false);
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
              { at last set the record size to that of the biggest variant }
              unionsymtable.datasize:=maxsize;
              unionsymtable.fieldalignment:=maxalignment;
              uniontype.def:=uniondef;
              uniontype.sym:=nil;
              UnionSym:=tfieldvarsym.create('$case',vs_value,uniontype,[]);
              symtablestack:=symtablestack.next;
              unionsymtable.addalignmentpadding;
{$ifdef powerpc}
              { parent inherits the alignment padding if the variant is the first "field" of the parent record/variant }
              if (target_info.system in [system_powerpc_darwin, system_powerpc_macos]) and
                 is_first_field and
                 (trecordsymtable(symtablestack).usefieldalignment = -1) and
                 (maxpadalign > trecordsymtable(symtablestack).padalignment) then
                trecordsymtable(symtablestack).padalignment:=maxpadalign;
{$endif powerpc}
              { Align the offset where the union symtable is added }
              if (trecordsymtable(symtablestack).usefieldalignment=-1) then
                usedalign:=used_align(unionsymtable.recordalignment,aktalignment.recordalignmin,aktalignment.maxCrecordalign)
              else
                usedalign:=used_align(unionsymtable.recordalignment,aktalignment.recordalignmin,aktalignment.recordalignmax);

              offset:=align(trecordsymtable(symtablestack).datasize,usedalign);
              trecordsymtable(symtablestack).datasize:=offset+unionsymtable.datasize;

              if unionsymtable.recordalignment>trecordsymtable(symtablestack).fieldalignment then
                trecordsymtable(symtablestack).fieldalignment:=unionsymtable.recordalignment;

              trecordsymtable(symtablestack).insertunionst(Unionsymtable,offset);
              Unionsym.owner:=nil;
              unionsym.free;
              uniondef.owner:=nil;
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
