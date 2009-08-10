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

    procedure read_public_and_external(vs: tabstractvarsym);

implementation

    uses
       SysUtils,
       { common }
       cutils,cclasses,
       { global }
       globtype,globals,tokens,verbose,constexp,
       systems,
       { symtable }
       symconst,symbase,symtype,symtable,defutil,defcmp,
       fmodule,htypechk,
       { pass 1 }
       node,pass_1,aasmdata,
       nmat,nadd,ncal,nset,ncnv,ninl,ncon,nld,nflw,nmem,nutils,
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
        function parse_symlist(pl:tpropaccesslist;var def:tdef):boolean;
          var
            idx : longint;
            sym : tsym;
            srsymtable : TSymtable;
            st  : TSymtable;
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
                        if (symtablestack.top.currentvisibility<>vis_private) then
                          addsymref(sym);
                        pl.addsym(sl_load,sym);
                        def:=tfieldvarsym(sym).vardef;
                      end;
                    procsym :
                      begin
                        if (symtablestack.top.currentvisibility<>vis_private) then
                          addsymref(sym);
                        pl.addsym(sl_call,sym);
                      end;
                    else
                      begin
                        Message1(parser_e_illegal_field_or_method,orgpattern);
                        def:=generrordef;
                        result:=false;
                      end;
                  end;
                end
               else
                begin
                  Message1(parser_e_illegal_field_or_method,orgpattern);
                  def:=generrordef;
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
                          st:=def.GetSymtable(gs_record);
                          if assigned(st) then
                           begin
                             sym:=tsym(st.Find(pattern));
                             if not(assigned(sym)) and is_object(def) then
                               sym:=search_class_member(tobjectdef(def),pattern);
                             if assigned(sym) then
                              begin
                                pl.addsym(sl_subscript,sym);
                                case sym.typ of
                                  fieldvarsym :
                                    def:=tfieldvarsym(sym).vardef;
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
                         if def.typ=arraydef then
                          begin
                            idx:=0;
                            p:=comp_expr(true);
                            if (not codegenerror) then
                             begin
                               if (p.nodetype=ordconstn) then
                                 begin
                                   { type/range checking }
                                   inserttypeconv(p,tarraydef(def).rangedef);
                                   if (Tordconstnode(p).value<int64(low(longint))) or
                                      (Tordconstnode(p).value>int64(high(longint))) then
                                     message(parser_e_array_range_out_of_bounds)
                                   else
                                     idx:=Tordconstnode(p).value.svalue
                                 end
                               else
                                Message(type_e_ordinal_expr_expected)
                             end;
                            pl.addconst(sl_vec,idx,p.resultdef);
                            p.free;
                            def:=tarraydef(def).elementdef;
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

          function allow_default_property(p : tpropertysym) : boolean;

          begin
             allow_default_property:=
               (is_ordinal(p.propdef) or
{$ifndef cpu64bitaddr}
               is_64bitint(p.propdef) or
{$endif cpu64bitaddr}
               is_class(p.propdef) or
               is_single(p.propdef) or
               (p.propdef.typ in [classrefdef,pointerdef]) or
                 is_smallset(p.propdef)
               ) and not
               (
                (p.propdef.typ=arraydef) and
                (ppo_indexed in p.propoptions)
               ) and not
               (ppo_hasparameters in p.propoptions);
          end;

      var
         sym : tsym;
         srsymtable: tsymtable;
         p : tpropertysym;
         overriden : tsym;
         varspez : tvarspez;
         hdef : tdef;
         arraytype : tdef;
         def : tdef;
         pt : tnode;
         sc : TFPObjectList;
         paranr : word;
         i      : longint;
         ImplIntf     : TImplementedInterface;
         found        : boolean;
         hreadparavs,
         hparavs      : tparavarsym;
         storedprocdef,
         readprocdef,
         writeprocdef : tprocvardef;
      begin
         { Generate temp procvardefs to search for matching read/write
           procedures. the readprocdef will store all definitions }
         paranr:=0;
         readprocdef:=tprocvardef.create(normal_function_level);
         writeprocdef:=tprocvardef.create(normal_function_level);
         storedprocdef:=tprocvardef.create(normal_function_level);

         { make it method pointers }
         if assigned(aclass) then
           begin
             include(readprocdef.procoptions,po_methodpointer);
             include(writeprocdef.procoptions,po_methodpointer);
             include(storedprocdef.procoptions,po_methodpointer);
           end;

         { method for stored must return boolean }
         storedprocdef.returndef:=booltype;

         if token<>_ID then
           begin
              consume(_ID);
              consume(_SEMICOLON);
              exit;
           end;
         { Generate propertysym and insert in symtablestack }
         p:=tpropertysym.create(orgpattern);
         p.visibility:=symtablestack.top.currentvisibility;
         p.default:=longint($80000000);
         symtablestack.top.insert(p);
         consume(_ID);
         { property parameters ? }
         if try_to_consume(_LECKKLAMMER) then
           begin
              if (p.visibility=vis_published) and
                not (m_delphi in current_settings.modeswitches) then
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
                else if (m_out in current_settings.modeswitches) and try_to_consume(_OUT) then
                  varspez:=vs_out
                else
                  varspez:=vs_value;
                sc.clear;
                repeat
                  inc(paranr);
                  hreadparavs:=tparavarsym.create(orgpattern,10*paranr,varspez,generrordef,[]);
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
                        hdef:=tarraydef.create(0,-1,s32inttype);
                        { define field type }
                        single_type(arraytype,false,false);
                        tarraydef(hdef).elementdef:=arraytype;
                      end
                    else
                      single_type(hdef,false,false);
                  end
                else
                  hdef:=cformaltype;
                for i:=0 to sc.count-1 do
                  begin
                    hreadparavs:=tparavarsym(sc[i]);
                    hreadparavs.vardef:=hdef;
                    { also update the writeprocdef }
                    hparavs:=tparavarsym.create(hreadparavs.realname,hreadparavs.paranr,vs_value,hdef,[]);
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
              single_type(p.propdef,false,false);
              if (idtoken=_INDEX) then
                begin
                   consume(_INDEX);
                   pt:=comp_expr(true);
                   { Only allow enum and integer indexes. Convert all integer
                     values to s32int to be compatible with delphi, because the
                     procedure matching requires equal parameters }
                   if is_constnode(pt) and
                      is_ordinal(pt.resultdef)
{$ifndef cpu64bitaddr}
                      and (not is_64bitint(pt.resultdef))
{$endif cpu64bitaddr}
                      then
                     begin
                       if is_integer(pt.resultdef) then
                         inserttypeconv_internal(pt,s32inttype);
                       p.index:=tordconstnode(pt).value.svalue;
                     end
                   else
                     begin
                       Message(parser_e_invalid_property_index_value);
                       p.index:=0;
                     end;
                   p.indexdef:=pt.resultdef;
                   include(p.propoptions,ppo_indexed);
                   { concat a longint to the para templates }
                   inc(paranr);
                   hparavs:=tparavarsym.create('$index',10*paranr,vs_value,p.indexdef,[]);
                   readprocdef.parast.insert(hparavs);
                   hparavs:=tparavarsym.create('$index',10*paranr,vs_value,p.indexdef,[]);
                   writeprocdef.parast.insert(hparavs);
                   hparavs:=tparavarsym.create('$index',10*paranr,vs_value,p.indexdef,[]);
                   storedprocdef.parast.insert(hparavs);
                   pt.free;
                end;
           end
         else
           begin
              { do an property override }
              overriden:=search_class_member(aclass.childof,p.name);
              if assigned(overriden) and
                 (overriden.typ=propertysym) and
                 not(is_dispinterface(aclass)) then
                begin
                  p.overridenpropsym:=tpropertysym(overriden);
                  { inherit all type related entries }
                  p.indexdef:=tpropertysym(overriden).indexdef;
                  p.propdef:=tpropertysym(overriden).propdef;
                  p.index:=tpropertysym(overriden).index;
                  p.default:=tpropertysym(overriden).default;
                  p.propoptions:=tpropertysym(overriden).propoptions;
                end
              else
                begin
                  p.propdef:=generrordef;
                  message(parser_e_no_property_found_to_override);
                end;
           end;
         if ((p.visibility=vis_published) or is_dispinterface(aclass)) and
            not(p.propdef.is_publishable) then
           begin
             Message(parser_e_cant_publish_that_property);
             p.visibility:=vis_public;
           end;

         if not(is_dispinterface(aclass)) then
           begin
             if try_to_consume(_READ) then
               begin
                 p.propaccesslist[palt_read].clear;
                 if parse_symlist(p.propaccesslist[palt_read],def) then
                  begin
                    sym:=p.propaccesslist[palt_read].firstsym^.sym;
                    case sym.typ of
                      procsym :
                        begin
                          { read is function returning the type of the property }
                          readprocdef.returndef:=p.propdef;
                          { Insert hidden parameters }
                          handle_calling_convention(readprocdef);
                          { search procdefs matching readprocdef }
                          { we ignore hidden stuff here because the property access symbol might have
                            non default calling conventions which might change the hidden stuff;
                            see tw3216.pp (FK) }
                          p.propaccesslist[palt_read].procdef:=Tprocsym(sym).Find_procdef_bypara(readprocdef.paras,p.propdef,[cpo_allowdefaults,cpo_ignorehidden]);
                          if not assigned(p.propaccesslist[palt_read].procdef) then
                            Message(parser_e_ill_property_access_sym);
                        end;
                      fieldvarsym :
                        begin
                          if not assigned(def) then
                            internalerror(200310071);
                          if compare_defs(def,p.propdef,nothingn)>=te_equal then
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
                           IncompatibleTypes(def,p.propdef);
                        end;
                      else
                        Message(parser_e_ill_property_access_sym);
                    end;
                  end;
               end;
             if try_to_consume(_WRITE) then
               begin
                 p.propaccesslist[palt_write].clear;
                 if parse_symlist(p.propaccesslist[palt_write],def) then
                  begin
                    sym:=p.propaccesslist[palt_write].firstsym^.sym;
                    case sym.typ of
                      procsym :
                        begin
                          { write is a procedure with an extra value parameter
                            of the of the property }
                          writeprocdef.returndef:=voidtype;
                          inc(paranr);
                          hparavs:=tparavarsym.create('$value',10*paranr,vs_value,p.propdef,[]);
                          writeprocdef.parast.insert(hparavs);
                          { Insert hidden parameters }
                          handle_calling_convention(writeprocdef);
                          { search procdefs matching writeprocdef }
                          p.propaccesslist[palt_write].procdef:=Tprocsym(sym).Find_procdef_bypara(writeprocdef.paras,writeprocdef.returndef,[cpo_allowdefaults]);
                          if not assigned(p.propaccesslist[palt_write].procdef) then
                            Message(parser_e_ill_property_access_sym);
                        end;
                      fieldvarsym :
                        begin
                          if not assigned(def) then
                            internalerror(200310072);
                          if compare_defs(def,p.propdef,nothingn)>=te_equal then
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
                           IncompatibleTypes(def,p.propdef);
                        end;
                      else
                        Message(parser_e_ill_property_access_sym);
                    end;
                  end;
               end;
           end
         else
           begin
             if try_to_consume(_READONLY) then
               begin
               end
             else if try_to_consume(_WRITEONLY) then
               begin
               end;
             if try_to_consume(_DISPID) then
               begin
                 pt:=comp_expr(true);
                 if is_constintnode(pt) then
                   // tprocdef(pd).extnumber:=tordconstnode(pt).value
                 else
                   Message(parser_e_dispid_must_be_ord_const);
                 pt.free;
               end;
           end;

         if assigned(aclass) and not(is_dispinterface(aclass)) then
           begin
             { ppo_stored is default on for not overriden properties }
             if not assigned(p.overridenpropsym) then
               include(p.propoptions,ppo_stored);
             if try_to_consume(_STORED) then
              begin
                include(p.propoptions,ppo_stored);
                p.propaccesslist[palt_stored].clear;
                case token of
                  _ID:
                    begin
                      { in the case that idtoken=_DEFAULT }
                      { we have to do nothing except      }
                      { setting ppo_stored, it's the same }
                      { as stored true                    }
                      if idtoken<>_DEFAULT then
                       begin
                         { parse_symlist cannot deal with constsyms, and
                           we also don't want to put constsyms in symlists
                           since they have to be evaluated immediately rather
                           than each time the property is accessed

                           The proper fix would be to always create a parse tree
                           and then convert that one, if appropriate, to a symlist.
                           Currently, we e.g. don't support any constant expressions
                           yet either here, while Delphi does.

                         }
                         { make sure we don't let constants mask class fields/
                           methods
                         }
                         if (not assigned(aclass) or
                             (search_class_member(aclass,pattern)=nil)) and
                            searchsym(pattern,sym,srsymtable) and
                            (sym.typ = constsym) then
                           begin
                              addsymref(sym);
                              if not is_boolean(tconstsym(sym).constdef) then
                                Message(parser_e_stored_property_must_be_boolean)
                              else if (tconstsym(sym).value.valueord=0) then
                                { same as for _FALSE }
                                exclude(p.propoptions,ppo_stored)
                              else
                                { same as for _TRUE }
                                p.default:=longint($80000000);
                              consume(_ID);
                            end
                         else if parse_symlist(p.propaccesslist[palt_stored],def) then
                          begin
                            sym:=p.propaccesslist[palt_stored].firstsym^.sym;
                            case sym.typ of
                              procsym :
                                begin
                                   { Insert hidden parameters }
                                   handle_calling_convention(storedprocdef);
                                   p.propaccesslist[palt_stored].procdef:=Tprocsym(sym).Find_procdef_bypara(storedprocdef.paras,storedprocdef.returndef,[cpo_allowdefaults,cpo_ignorehidden]);
                                   if not assigned(p.propaccesslist[palt_stored].procdef) then
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
              if not allow_default_property(p) then
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
                  if (p.propdef.typ=setdef) and
                     (pt.nodetype=arrayconstructorn) then
                    begin
                      arrayconstructor_to_set(pt);
                      do_typecheckpass(pt);
                    end;
                  inserttypeconv(pt,p.propdef);
                  if not(is_constnode(pt)) then
                    Message(parser_e_property_default_value_must_const);
                  { Set default value }
                  case pt.nodetype of
                    setconstn :
                      p.default:=plongint(tsetconstnode(pt).value_set)^;
                    ordconstn :
                      if (Tordconstnode(pt).value<int64(low(longint))) or
                         (Tordconstnode(pt).value>int64(high(cardinal))) then
                        message(parser_e_range_check_error)
                      else
                        p.default:=longint(tordconstnode(pt).value.svalue);
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
(*
         else {if allow_default_property(p) then
           begin
              p.default:=longint($80000000);
           end;
*)
         { Parse possible "implements" keyword }
         if try_to_consume(_IMPLEMENTS) then
           begin
             single_type(def,false,false);

             if not(is_interface(def)) then
               message(parser_e_class_implements_must_be_interface);

             if is_interface(p.propdef) then
               begin
                 if compare_defs(def,p.propdef,nothingn)<te_equal then
                   begin
                     message2(parser_e_implements_must_have_correct_type,def.GetTypeName,p.propdef.GetTypeName);
                     exit;
                   end;
               end
             else if is_class(p.propdef) then
               begin
                 ImplIntf:=tobjectdef(p.propdef).find_implemented_interface(tobjectdef(def));
                 if assigned(ImplIntf) then
                   begin
                     if compare_defs(ImplIntf.IntfDef,def,nothingn)<te_equal then
                       begin
                         message2(parser_e_implements_must_have_correct_type,ImplIntf.IntfDef.GetTypeName,def.GetTypeName);
                         exit;
                       end;
                   end
                 else
                   begin
                     message2(parser_e_class_doesnt_implement_interface,p.propdef.GetTypeName,def.GetTypeName);
                     exit;
                   end;
               end
             else
               begin
                 message(parser_e_implements_must_be_class_or_interface);
                 exit;
               end;


             if not assigned(p.propaccesslist[palt_read].firstsym) then
               begin
                 message(parser_e_implements_must_read_specifier);
                 exit;
               end;
             if assigned(p.propaccesslist[palt_write].firstsym) then
               begin
                 message(parser_e_implements_must_not_have_write_specifier);
                 exit;
               end;
             if assigned(p.propaccesslist[palt_stored].firstsym) then
               begin
                 message(parser_e_implements_must_not_have_stored_specifier);
                 exit;
               end;
             found:=false;
             for i:=0 to aclass.ImplementedInterfaces.Count-1 do
               begin
                 ImplIntf:=TImplementedInterface(aclass.ImplementedInterfaces[i]);

                 if compare_defs(def,ImplIntf.IntfDef,nothingn)>=te_equal then
                   begin
                     found:=true;
                     break;
                   end;
               end;
             if found then
               begin
                 ImplIntf.ImplementsGetter:=p;
                 case p.propaccesslist[palt_read].firstsym^.sym.typ of
                   procsym :
                     begin
                       if (po_virtualmethod in tprocdef(p.propaccesslist[palt_read].procdef).procoptions) then
                         ImplIntf.IType:=etVirtualMethodResult
                       else
                         ImplIntf.IType:=etStaticMethodResult;
                     end;
                   fieldvarsym :
                     ImplIntf.IType:=etFieldValue;
                   else
                     internalerror(200802161);
                 end;
               end
             else
               message1(parser_e_implements_uses_non_implemented_interface,def.GetTypeName);
         end;

         { remove temporary procvardefs }
         readprocdef.owner.deletedef(readprocdef);
         writeprocdef.owner.deletedef(writeprocdef);

         result:=p;
      end;


     function maybe_parse_proc_directives(def:tdef):boolean;
       var
         newtype : ttypesym;
       begin
         result:=false;
         { Process procvar directives before = and ; }
         if (def.typ=procvardef) and
            (def.typesym=nil) and
            check_proc_directive(true) then
           begin
              newtype:=ttypesym.create('unnamed',def);
              parse_var_proc_directives(tsym(newtype));
              newtype.typedef:=nil;
              def.typesym:=nil;
              newtype.free;
              result:=true;
           end;
       end;


    const
       variantrecordlevel : longint = 0;


    procedure read_public_and_external_sc(sc:TFPObjectList);
    var
      vs: tabstractvarsym;
    begin
      { only allowed for one var }
      vs:=tabstractvarsym(sc[0]);
      if sc.count>1 then
        Message(parser_e_absolute_only_one_var);
      read_public_and_external(vs);
    end;


    procedure read_public_and_external(vs: tabstractvarsym);
    var
      is_dll,
      is_cdecl,
      is_external_var,
      is_weak_external,
      is_public_var  : boolean;
      dll_name,
      C_name      : string;
    begin
      { only allowed for one var }
      { only allow external and public on global symbols }
      if vs.typ<>staticvarsym then
        begin
          Message(parser_e_no_local_var_external);
          exit;
        end;
      { defaults }
      is_dll:=false;
      is_cdecl:=false;
      is_external_var:=false;
      is_public_var:=false;
      C_name:=vs.realname;

      { macpas specific handling due to some switches}
      if (m_mac in current_settings.modeswitches) then
        begin
          if (cs_external_var in current_settings.localswitches) then
            begin {The effect of this is the same as if cvar; external; has been given as directives.}
              is_cdecl:=true;
              is_external_var:=true;
            end
          else if (cs_externally_visible in current_settings.localswitches) then
            begin {The effect of this is the same as if cvar has been given as directives and it's made public.}
              is_cdecl:=true;
              is_public_var:=true;
            end;
        end;

      { cdecl }
      if try_to_consume(_CVAR) then
        begin
          consume(_SEMICOLON);
          is_cdecl:=true;
        end;

      { external }
      is_weak_external:=try_to_consume(_WEAKEXTERNAL);
      if is_weak_external or
         try_to_consume(_EXTERNAL) then
        begin
          is_external_var:=true;
          if (idtoken<>_NAME) and (token<>_SEMICOLON) then
            begin
              is_dll:=true;
              dll_name:=get_stringconst;
              if ExtractFileExt(dll_name)='' then
                dll_name:=ChangeFileExt(dll_name,target_info.sharedlibext);
            end;
          if not(is_cdecl) and try_to_consume(_NAME) then
            C_name:=get_stringconst;
          consume(_SEMICOLON);
        end;

      { export or public }
      if idtoken in [_EXPORT,_PUBLIC] then
        begin
          consume(_ID);
          if is_external_var then
            Message(parser_e_not_external_and_export)
          else
            is_public_var:=true;
          if try_to_consume(_NAME) then
            C_name:=get_stringconst;
          consume(_SEMICOLON);
        end;

      { Windows uses an indirect reference using import tables }
      if is_dll and
         (target_info.system in system_all_windows) then
        include(vs.varoptions,vo_is_dll_var);

      { Add C _ prefix }
      if is_cdecl or
         (
          is_dll and
          (target_info.system in systems_darwin)
         ) then
        C_Name := target_info.Cprefix+C_Name;

      if is_public_var then
        begin
          include(vs.varoptions,vo_is_public);
          vs.varregable := vr_none;
          { mark as referenced }
          inc(vs.refs);
        end;

      { now we can insert it in the import lib if its a dll, or
        add it to the externals }
      if is_external_var then
        begin
          if vo_is_typed_const in vs.varoptions then
            Message(parser_e_initialized_not_for_external);
          include(vs.varoptions,vo_is_external);
          if (is_weak_external) then
            begin
              if not(target_info.system in system_weak_linking) then
                message(parser_e_weak_external_not_supported);
              include(vs.varoptions,vo_is_weak_external);
            end;
          vs.varregable := vr_none;
          if is_dll then
            current_module.AddExternalImport(dll_name,C_Name,0,true,false)
          else
            if tf_has_dllscanner in target_info.flags then
              current_module.dllscannerinputlist.Add(vs.mangledname,vs);
        end;

      { Set the assembler name }
      tstaticvarsym(vs).set_mangledname(C_Name);
    end;


    procedure read_var_decls(options:Tvar_dec_options);

        procedure read_default_value(sc : TFPObjectList);
        var
          vs : tabstractnormalvarsym;
          tcsym : tstaticvarsym;
        begin
          vs:=tabstractnormalvarsym(sc[0]);
          if sc.count>1 then
            Message(parser_e_initialized_only_one_var);
          if vo_is_thread_var in vs.varoptions then
            Message(parser_e_initialized_not_for_threadvar);
          consume(_EQUAL);
          case vs.typ of
            localvarsym :
              begin
                tcsym:=tstaticvarsym.create('$default'+vs.realname,vs_const,vs.vardef,[]);
                include(tcsym.symoptions,sp_internal);
                vs.defaultconstsym:=tcsym;
                symtablestack.top.insert(tcsym);
                read_typed_const(current_asmdata.asmlists[al_typedconsts],tcsym);
              end;
            staticvarsym :
              begin
                read_typed_const(current_asmdata.asmlists[al_typedconsts],tstaticvarsym(vs));
              end;
            else
              internalerror(200611051);
          end;
          vs.varstate:=vs_initialised;
        end;

{$ifdef gpc_mode}
        procedure read_gpc_name(sc : TFPObjectList);
        var
          vs : tabstractnormalvarsym;
          C_Name : string;
        begin
          consume(_ID);
          C_Name:=get_stringconst;
          vs:=tabstractnormalvarsym(sc[0]);
          if sc.count>1 then
            Message(parser_e_absolute_only_one_var);
          if vs.typ=staticvarsym then
            begin
              tstaticvarsym(vs).set_mangledname(C_Name);
              include(vs.varoptions,vo_is_external);
            end
          else
            Message(parser_e_no_local_var_external);
        end;
{$endif}

        procedure read_absolute(sc : TFPObjectList);
        var
          vs     : tabstractvarsym;
          abssym : tabsolutevarsym;
          pt,hp  : tnode;
          st     : tsymtable;
        begin
          abssym:=nil;
          { only allowed for one var }
          vs:=tabstractvarsym(sc[0]);
          if sc.count>1 then
            Message(parser_e_absolute_only_one_var);
          if vo_is_typed_const in vs.varoptions then
            Message(parser_e_initialized_not_for_external);
          { parse the rest }
          pt:=expr;
          { check allowed absolute types }
          if (pt.nodetype=stringconstn) or
            (is_constcharnode(pt)) then
            begin
              abssym:=tabsolutevarsym.create(vs.realname,vs.vardef);
              abssym.fileinfo:=vs.fileinfo;
              if pt.nodetype=stringconstn then
                abssym.asmname:=stringdup(strpas(tstringconstnode(pt).value_str))
              else
                abssym.asmname:=stringdup(chr(tordconstnode(pt).value.svalue));
              consume(token);
              abssym.abstyp:=toasm;
            end
          { address }
          else if is_constintnode(pt) then
            begin
              abssym:=tabsolutevarsym.create(vs.realname,vs.vardef);
              abssym.fileinfo:=vs.fileinfo;
              abssym.abstyp:=toaddr;
{$ifndef cpu64bitaddr}
              { on 64 bit systems, abssym.addroffset is a qword and hence this
                test is useless (value is a 64 bit entity) and will always fail
                for positive values (since int64(high(abssym.addroffset))=-1
              }
              if (Tordconstnode(pt).value<int64(low(abssym.addroffset))) or
                 (Tordconstnode(pt).value>int64(high(abssym.addroffset))) then
                message(parser_e_range_check_error)
              else
{$endif}
                abssym.addroffset:=Tordconstnode(pt).value.svalue;
{$ifdef i386}
              abssym.absseg:=false;
              if (target_info.system in [system_i386_go32v2,system_i386_watcom]) and
                  try_to_consume(_COLON) then
                begin
                  pt.free;
                  pt:=expr;
                  if is_constintnode(pt) then
                    begin
                      if (Tordconstnode(pt).value<int64(low(abssym.addroffset))) or
                         (Tordconstnode(pt).value>int64(high(abssym.addroffset))) then
                        message(parser_e_range_check_error)
                      else
                        abssym.addroffset:=abssym.addroffset shl 4+tordconstnode(pt).value.svalue;
                      abssym.absseg:=true;
                    end
                  else
                    Message(type_e_ordinal_expr_expected);
                end;
{$endif i386}
            end
          { variable }
          else
            begin
              { we have to be able to take the address of the absolute
                expression
              }
              valid_for_addr(pt,true);
              { remove subscriptn before checking for loadn }
              hp:=pt;
              while (hp.nodetype in [subscriptn,typeconvn,vecn]) do
                begin
                  { check for implicit dereferencing and reject it }
                  if (hp.nodetype in [subscriptn,vecn]) then
                    begin
                      if (tunarynode(hp).left.resultdef.typ in [pointerdef,classrefdef]) then
                        break;
                      { catch, e.g., 'var b: char absolute pchar_var[5];"
                        (pchar_var[5] is a pchar_2_string typeconv ->
                         the vecn only sees an array of char)
                        I don't know if all of these type conversions are
                        possible, but they're definitely all bad.
                      }
                      if (tunarynode(hp).left.nodetype=typeconvn) and
                         (ttypeconvnode(tunarynode(hp).left).convtype in
                           [tc_pchar_2_string,tc_pointer_2_array,
                            tc_intf_2_string,tc_intf_2_guid,
                            tc_dynarray_2_variant,tc_interface_2_variant,
                            tc_array_2_dynarray]) then
                        break;

                      if (tunarynode(hp).left.resultdef.typ=stringdef) and
                         not(tstringdef(tunarynode(hp).left.resultdef).stringtype in [st_shortstring,st_longstring]) then
                        break;
                      if (tunarynode(hp).left.resultdef.typ=objectdef) and
                         (tobjectdef(tunarynode(hp).left.resultdef).objecttype<>odt_object) then
                        break;
                      if is_dynamic_array(tunarynode(hp).left.resultdef) then
                        break;
                    end;
                  hp:=tunarynode(hp).left;
                end;
              if (hp.nodetype=loadn) then
                begin
                  { we should check the result type of loadn }
                  if not (tloadnode(hp).symtableentry.typ in [fieldvarsym,staticvarsym,localvarsym,paravarsym]) then
                    Message(parser_e_absolute_only_to_var_or_const);
                  abssym:=tabsolutevarsym.create(vs.realname,vs.vardef);
                  abssym.fileinfo:=vs.fileinfo;
                  abssym.abstyp:=tovar;
                  abssym.ref:=node_to_propaccesslist(pt);

                  { if the sizes are different, can't be a regvar since you }
                  { can't be "absolute upper 8 bits of a register" (except  }
                  { if its a record field of the same size of a record      }
                  { regvar, but in that case pt.resultdef.size will have    }
                  { the same size since it refers to the field and not to   }
                  { the whole record -- which is why we use pt and not hp)  }

                  { we can't take the size of an open array }
                  if is_open_array(pt.resultdef) or
                     (vs.vardef.size <> pt.resultdef.size) then
                    make_not_regable(pt,[ra_addr_regable]);
                end
              else
                Message(parser_e_absolute_only_to_var_or_const);
            end;
          pt.free;
          { replace old varsym with the new absolutevarsym }
          if assigned(abssym) then
            begin
              st:=vs.owner;
              vs.owner.Delete(vs);
              st.insert(abssym);
              sc[0]:=abssym;
            end;
        end;

      var
         sc   : TFPObjectList;
         vs   : tabstractvarsym;
         hdef : tdef;
         i    : longint;
         semicoloneaten,
         allowdefaultvalue,
         hasdefaultvalue : boolean;
         hintsymoptions  : tsymoptions;
         old_block_type  : tblock_type;
      begin
         old_block_type:=block_type;
         block_type:=bt_var;
         { Force an expected ID error message }
         if not (token in [_ID,_CASE,_END]) then
           consume(_ID);
         { read vars }
         sc:=TFPObjectList.create(false);
         while (token=_ID) do
           begin
             semicoloneaten:=false;
             hasdefaultvalue:=false;
             allowdefaultvalue:=true;
             sc.clear;
             repeat
               if (token = _ID) then
                 begin
                   case symtablestack.top.symtabletype of
                     localsymtable :
                       vs:=tlocalvarsym.create(orgpattern,vs_value,generrordef,[]);
                     staticsymtable,
                     globalsymtable :
                       begin
                         vs:=tstaticvarsym.create(orgpattern,vs_value,generrordef,[]);
                         if vd_threadvar in options then
                           include(vs.varoptions,vo_is_thread_var);
                       end;
                     else
                       internalerror(200411064);
                   end;
                   sc.add(vs);
                   symtablestack.top.insert(vs);
                 end;
               consume(_ID);
             until not try_to_consume(_COMMA);

             { read variable type def }
             block_type:=bt_var_type;
             consume(_COLON);

{$ifdef gpc_mode}
             if (m_gpc in current_settings.modeswitches) and
                (token=_ID) and
                (orgpattern='__asmname__') then
               read_gpc_name(sc);
{$endif}

             read_anon_type(hdef,false);
             for i:=0 to sc.count-1 do
               begin
                 vs:=tabstractvarsym(sc[i]);
                 vs.vardef:=hdef;
               end;
             block_type:=bt_var;

             { Process procvar directives }
             if maybe_parse_proc_directives(hdef) then
               semicoloneaten:=true;

             { check for absolute }
             if try_to_consume(_ABSOLUTE) then
               begin
                 read_absolute(sc);
                 allowdefaultvalue:=false;
               end;

             { Check for EXTERNAL etc directives before a semicolon }
             if (idtoken in [_EXPORT,_EXTERNAL,_WEAKEXTERNAL,_PUBLIC,_CVAR]) then
               begin
                 read_public_and_external_sc(sc);
                 allowdefaultvalue:=false;
                 semicoloneaten:=true;
               end;

             { try to parse the hint directives }
             hintsymoptions:=[];
             try_consume_hintdirective(hintsymoptions);
             for i:=0 to sc.count-1 do
               begin
                 vs:=tabstractvarsym(sc[i]);
                 vs.symoptions := vs.symoptions + hintsymoptions;
               end;

             { Handling of Delphi typed const = initialized vars }
             if allowdefaultvalue and
                (token=_EQUAL) and
                not(m_tp7 in current_settings.modeswitches) and
                (symtablestack.top.symtabletype<>parasymtable) then
               begin
                 { Add calling convention for procvar }
                 if (hdef.typ=procvardef) and
                    (hdef.typesym=nil) then
                   handle_calling_convention(tprocvardef(hdef));
                 read_default_value(sc);
                 hasdefaultvalue:=true;
               end
             else
               begin
                 if not(semicoloneaten) then
                   consume(_SEMICOLON);
               end;

             { Support calling convention for procvars after semicolon }
             if not(hasdefaultvalue) and
                (hdef.typ=procvardef) and
                (hdef.typesym=nil) then
               begin
                 { Parse procvar directives after ; }
                 maybe_parse_proc_directives(hdef);
                 { Add calling convention for procvar }
                 handle_calling_convention(tprocvardef(hdef));
                 { Handling of Delphi typed const = initialized vars }
                 if (token=_EQUAL) and
                    not(m_tp7 in current_settings.modeswitches) and
                    (symtablestack.top.symtabletype<>parasymtable) then
                   begin
                     read_default_value(sc);
                     hasdefaultvalue:=true;
                   end;
               end;

             { Check for EXTERNAL etc directives or, in macpas, if cs_external_var is set}
             if (
                 (
                  (idtoken in [_EXPORT,_EXTERNAL,_WEAKEXTERNAL,_PUBLIC,_CVAR]) and
                  (m_cvar_support in current_settings.modeswitches)
                 ) or
                 (
                  (m_mac in current_settings.modeswitches) and
                  (
                   (cs_external_var in current_settings.localswitches) or
                   (cs_externally_visible in current_settings.localswitches)
                  )
                 )
                ) then
               read_public_and_external_sc(sc);

             { allocate normal variable (non-external and non-typed-const) staticvarsyms }
             for i:=0 to sc.count-1 do
               begin
                 vs:=tabstractvarsym(sc[i]);
                 if (vs.typ=staticvarsym) and
                    not(vo_is_typed_const in vs.varoptions) and
                    not(vo_is_external in vs.varoptions) then
                   insertbssdata(tstaticvarsym(vs));
               end;
           end;
         block_type:=old_block_type;
         { free the list }
         sc.free;
      end;


    procedure read_record_fields(options:Tvar_dec_options);
      var
         sc : TFPObjectList;
         i  : longint;
         hs,sorg : string;
         hdef,casetype : tdef;
         { maxsize contains the max. size of a variant }
         { startvarrec contains the start of the variant part of a record }
         maxsize, startvarrecsize : longint;
         usedalign,
         maxalignment,startvarrecalign,
         maxpadalign, startpadalign: shortint;
         pt : tnode;
         fieldvs   : tfieldvarsym;
         hstaticvs : tstaticvarsym;
         vs    : tabstractvarsym;
         srsym : tsym;
         srsymtable : TSymtable;
         visibility : tvisibility;
         recst : tabstractrecordsymtable;
         unionsymtable : trecordsymtable;
         offset : longint;
         uniondef : trecorddef;
         hintsymoptions : tsymoptions;
         semicoloneaten: boolean;
{$if defined(powerpc) or defined(powerpc64)}
         tempdef: tdef;
         is_first_field: boolean;
{$endif powerpc or powerpc64}
         sl       : tpropaccesslist;
      begin
         recst:=tabstractrecordsymtable(symtablestack.top);
{$if defined(powerpc) or defined(powerpc64)}
         is_first_field := true;
{$endif powerpc or powerpc64}
         { Force an expected ID error message }
         if not (token in [_ID,_CASE,_END]) then
          consume(_ID);
         { read vars }
         sc:=TFPObjectList.create(false);
         while (token=_ID) and
            not((vd_object in options) and
                (idtoken in [_PUBLIC,_PRIVATE,_PUBLISHED,_PROTECTED,_STRICT])) do
           begin
             visibility:=symtablestack.top.currentvisibility;
             semicoloneaten:=false;
             sc.clear;
             repeat
               sorg:=orgpattern;
               if token=_ID then
                 begin
                   vs:=tfieldvarsym.create(sorg,vs_value,generrordef,[]);
                   sc.add(vs);
                   recst.insert(vs);
                 end;
               consume(_ID);
             until not try_to_consume(_COMMA);
             consume(_COLON);

             { Don't search in the recordsymtable for types }
             if ([df_generic,df_specialization]*tdef(recst.defowner).defoptions=[]) then
               symtablestack.pop(recst);
             read_anon_type(hdef,false);
             if ([df_generic,df_specialization]*tdef(recst.defowner).defoptions=[]) then
               symtablestack.push(recst);

             { Process procvar directives }
             if maybe_parse_proc_directives(hdef) then
               semicoloneaten:=true;

{$if defined(powerpc) or defined(powerpc64)}
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
             if (target_info.system in [system_powerpc_darwin, system_powerpc_macos, system_powerpc64_darwin]) and
                is_first_field and
                (symtablestack.top.symtabletype=recordsymtable) and
                (trecordsymtable(symtablestack.top).usefieldalignment=C_alignment) then
               begin
                 tempdef:=hdef;
                 while tempdef.typ=arraydef do
                   tempdef:=tarraydef(tempdef).elementdef;
                 if tempdef.typ<>recorddef then
                   maxpadalign:=tempdef.alignment
                 else
                   maxpadalign:=trecorddef(tempdef).padalignment;

                 if (maxpadalign>4) and
                    (maxpadalign>trecordsymtable(symtablestack.top).padalignment) then
                   trecordsymtable(symtablestack.top).padalignment:=maxpadalign;
                 is_first_field:=false;
               end;
{$endif powerpc or powerpc64}

             { types that use init/final are not allowed in variant parts, but
               classes are allowed }
             if (variantrecordlevel>0) and
                (hdef.needs_inittable and not is_class(hdef)) then
               Message(parser_e_cant_use_inittable_here);

             { try to parse the hint directives }
             hintsymoptions:=[];
             try_consume_hintdirective(hintsymoptions);

             { update variable type and hints }
             for i:=0 to sc.count-1 do
               begin
                 fieldvs:=tfieldvarsym(sc[i]);
                 fieldvs.vardef:=hdef;
                 { insert any additional hint directives }
                 fieldvs.symoptions := fieldvs.symoptions + hintsymoptions;
               end;

             { Records and objects can't have default values }
             { for a record there doesn't need to be a ; before the END or )    }
             if not(token in [_END,_RKLAMMER]) and
                not(semicoloneaten) then
               consume(_SEMICOLON);

             { Parse procvar directives after ; }
             maybe_parse_proc_directives(hdef);

             { Add calling convention for procvar }
             if (hdef.typ=procvardef) and
                (hdef.typesym=nil) then
               handle_calling_convention(tprocvardef(hdef));

             { Check for STATIC directive }
             if (vd_object in options) and
                (cs_static_keyword in current_settings.moduleswitches) and
                (try_to_consume(_STATIC)) then
               begin
                 { add static flag and staticvarsyms }
                 for i:=0 to sc.count-1 do
                   begin
                     fieldvs:=tfieldvarsym(sc[i]);
                     include(fieldvs.symoptions,sp_static);
                     { generate the symbol which reserves the space }
                     hstaticvs:=tstaticvarsym.create('$_static_'+lower(symtablestack.top.name^)+'_'+fieldvs.name,vs_value,hdef,[]);
                     recst.defowner.owner.insert(hstaticvs);
                     insertbssdata(hstaticvs);
                     { generate the symbol for the access }
                     sl:=tpropaccesslist.create;
                     sl.addsym(sl_load,hstaticvs);
                     recst.insert(tabsolutevarsym.create_ref('$'+lower(symtablestack.top.name^)+'_'+fieldvs.name,hdef,sl));
                   end;
                 consume(_SEMICOLON);
               end;

             if (visibility=vis_published) and
                not(is_class(hdef)) then
               begin
                 Message(parser_e_cant_publish_that);
                 visibility:=vis_public;
               end;

             if (visibility=vis_published) and
                not(oo_can_have_published in tobjectdef(hdef).objectoptions) and
                not(m_delphi in current_settings.modeswitches) then
               begin
                 Message(parser_e_only_publishable_classes_can_be_published);
                 visibility:=vis_public;
               end;

             { Generate field in the recordsymtable }
             for i:=0 to sc.count-1 do
               begin
                 fieldvs:=tfieldvarsym(sc[i]);
                 { static data fields are already inserted in the globalsymtable }
                 if not(sp_static in fieldvs.symoptions) then
                   recst.addfield(fieldvs,visibility);
               end;
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
                  fieldvs:=tfieldvarsym.create(sorg,vs_value,generrordef,[]);
                  symtablestack.top.insert(fieldvs);
                end;
              read_anon_type(casetype,true);
              if assigned(fieldvs) then
                begin
                  fieldvs.vardef:=casetype;
                  recst.addfield(fieldvs,recst.currentvisibility);
                end;
              if not(is_ordinal(casetype))
{$ifndef cpu64bitaddr}
                 or is_64bitint(casetype)
{$endif cpu64bitaddr}
                 then
                Message(type_e_ordinal_expr_expected);
              consume(_OF);

              UnionSymtable:=trecordsymtable.create(current_settings.packrecords);
              UnionDef:=trecorddef.create(unionsymtable);
              uniondef.isunion:=true;
              startvarrecsize:=UnionSymtable.datasize;
              { align the bitpacking to the next byte }
              UnionSymtable.datasize:=startvarrecsize;
              startvarrecalign:=UnionSymtable.fieldalignment;
              startpadalign:=Unionsymtable.padalignment;
              symtablestack.push(UnionSymtable);
              repeat
                repeat
                  pt:=comp_expr(true);
                  if not(pt.nodetype=ordconstn) then
                    Message(parser_e_illegal_expression);
                  if try_to_consume(_POINTPOINT) then
                    pt:=crangenode.create(pt,comp_expr(true));
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
              unionsymtable.addalignmentpadding;
{$if defined(powerpc) or defined(powerpc64)}
              { parent inherits the alignment padding if the variant is the first "field" of the parent record/variant }
              if (target_info.system in [system_powerpc_darwin, system_powerpc_macos, system_powerpc64_darwin]) and
                 is_first_field and
                 (recst.usefieldalignment=C_alignment) and
                 (maxpadalign>recst.padalignment) then
                recst.padalignment:=maxpadalign;
{$endif powerpc or powerpc64}
              { Align the offset where the union symtable is added }
              case recst.usefieldalignment of
                { allow the unionsymtable to be aligned however it wants }
                { (within the global min/max limits)                     }
                0, { default }
                C_alignment:
                  usedalign:=used_align(unionsymtable.recordalignment,current_settings.alignment.recordalignmin,current_settings.alignment.maxCrecordalign);
                { 1 byte alignment if we are bitpacked }
                bit_alignment:
                  usedalign:=1;
                { otherwise alignment at the packrecords alignment of the }
                { current record                                          }
                else
                  usedalign:=used_align(recst.fieldalignment,current_settings.alignment.recordalignmin,current_settings.alignment.recordalignmax);
              end;
              offset:=align(recst.datasize,usedalign);
              recst.datasize:=offset+unionsymtable.datasize;

              if unionsymtable.recordalignment>recst.fieldalignment then
                recst.fieldalignment:=unionsymtable.recordalignment;

              trecordsymtable(recst).insertunionst(Unionsymtable,offset);
              uniondef.owner.deletedef(uniondef);
           end;
         { free the list }
         sc.free;
{$ifdef powerpc}
         is_first_field := false;
{$endif powerpc}
      end;

end.
