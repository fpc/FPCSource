{
    $Id$
    Copyright (c) 1998-2001 by Florian Klaempfl

    Does object types for Free Pascal

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
unit pdecobj;

{$i defines.inc}

interface

    uses
      globtype,symtype,symdef;

    { parses a object declaration }
    function object_dec(const n : stringid;fd : tobjectdef) : tdef;

implementation

    uses
      cutils,cclasses,
      globals,verbose,systems,tokens,
      aasm,symconst,symbase,symsym,symtable,types,
      cgbase,
      node,nld,nmem,ncon,ncnv,ncal,pass_1,
      scanner,
      pbase,pexpr,pdecsub,pdecvar,ptype;

    function object_dec(const n : stringid;fd : tobjectdef) : tdef;
    { this function parses an object or class declaration }
      var
         there_is_a_destructor : boolean;
         classtype : tobjectdeftype;
         childof : tobjectdef;
         aktclass : tobjectdef;

      procedure constructor_head;

        begin
           consume(_CONSTRUCTOR);
           { must be at same level as in implementation }
           inc(lexlevel);
           parse_proc_head(potype_constructor);
           dec(lexlevel);

           if (cs_constructor_name in aktglobalswitches) and (aktprocsym.name<>'INIT') then
            Message(parser_e_constructorname_must_be_init);

           include(aktclass.objectoptions,oo_has_constructor);
           consume(_SEMICOLON);
             begin
                if is_class(aktclass) then
                  begin
                     { CLASS constructors return the created instance }
                     aktprocdef.rettype.def:=aktclass;
                  end
                else
                  begin
                     { OBJECT constructors return a boolean }
                     aktprocdef.rettype:=booltype;
                  end;
             end;
        end;


      procedure property_dec;

        { convert a node tree to symlist and return the last
          symbol }
        function parse_symlist(pl:tsymlist):boolean;
          var
            idx : longint;
            sym : tsym;
            def : tdef;
            st  : tsymtable;
          begin
            parse_symlist:=true;
            def:=nil;
            if token=_ID then
             begin
               sym:=search_class_member(aktclass,pattern);
               if assigned(sym) then
                begin
                  case sym.typ of
                    varsym :
                      begin
                        pl.addsym(sl_load,sym);
                        def:=tvarsym(sym).vartype.def;
                      end;
                    procsym :
                      begin
                        pl.addsym(sl_call,sym);
                      end;
                  end;
                end
               else
                begin
                  Message1(parser_e_illegal_field_or_method,pattern);
                  parse_symlist:=false;
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
                                  varsym :
                                    def:=tvarsym(sym).vartype.def;
                                  else
                                    begin
                                      Message1(sym_e_illegal_field,pattern);
                                      parse_symlist:=false;
                                    end;
                                end;
                              end
                             else
                              begin
                                Message1(sym_e_illegal_field,pattern);
                                parse_symlist:=false;
                              end;
                           end
                          else
                           begin
                             Message(cg_e_invalid_qualifier);
                             parse_symlist:=false;
                           end;
                        end;
                       consume(_ID);
                     end;
                   _LECKKLAMMER :
                     begin
                       consume(_LECKKLAMMER);
                       repeat
                         if def.deftype=arraydef then
                          begin
                            idx:=get_intconst;
                            pl.addconst(sl_vec,idx);
                            def:=tarraydef(def).elementtype.def;
                          end
                         else
                          begin
                            Message(cg_e_invalid_qualifier);
                            parse_symlist:=false;
                          end;
                       until not try_to_consume(_COMMA);
                       consume(_RECKKLAMMER);
                     end;
                   else
                     begin
                       Message(parser_e_ill_property_access_sym);
                       parse_symlist:=false;
                       break;
                     end;
                 end;
               until false;
             end
            else
             begin
               Message(parser_e_ill_property_access_sym);
               parse_symlist:=false;
             end;
            pl.def:=def;
          end;

        var
           sym : tsym;
           propertyparas : tlinkedlist;

        { returns the matching procedure to access a property }
        function get_procdef : tprocdef;
          var
             p : pprocdeflist;
          begin
             get_procdef:=nil;
             p:=tprocsym(sym).defs;
             while assigned(p) do
               begin
                  if equal_paras(p^.def.para,propertyparas,cp_value_equal_const) or
                     convertable_paras(p^.def.para,propertyparas,cp_value_equal_const) then
                    begin
                      get_procdef:=p^.def;
                      exit;
                    end;
                  p:=p^.next;
               end;
          end;

        var
           hp2,datacoll : tparaitem;
           p : tpropertysym;
           overriden : tsym;
           hs : string;
           varspez : tvarspez;
           sc : tidstringlist;
           s : string;
           tt : ttype;
           declarepos : tfileposinfo;
           pp : pprocdeflist;
           pd : tprocdef;
           pt : tnode;
           propname : stringid;
        begin
           { check for a class }
           aktprocsym:=nil;
           aktprocdef:=nil;
           if not((is_class_or_interface(aktclass)) or
              ((m_delphi in aktmodeswitches) and (is_object(aktclass)))) then
             Message(parser_e_syntax_error);
           consume(_PROPERTY);
           propertyparas:=TParaLinkedList.Create;
           datacoll:=nil;
           if token=_ID then
             begin
                p:=tpropertysym.create(orgpattern);
                propname:=pattern;
                consume(_ID);
                { property parameters ? }
                if token=_LECKKLAMMER then
                  begin
                     if (sp_published in current_object_option) then
                       Message(parser_e_cant_publish_that_property);

                     { create a list of the parameters in propertyparas }
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
                       else varspez:=vs_value;
                       sc:=consume_idlist;
{$ifdef fixLeaksOnError}
                       strContStack.push(sc);
{$endif fixLeaksOnError}
                       if token=_COLON then
                         begin
                            consume(_COLON);
                            if token=_ARRAY then
                              begin
                                 {
                                 if (varspez<>vs_const) and
                                   (varspez<>vs_var) then
                                   begin
                                      varspez:=vs_const;
                                      Message(parser_e_illegal_open_parameter);
                                   end;
                                 }
                                 consume(_ARRAY);
                                 consume(_OF);
                                 { define range and type of range }
                                 tt.setdef(tarraydef.create(0,-1,s32bittype));
                                 { define field type }
                                 single_type(tarraydef(tt.def).elementtype,s,false);
                              end
                            else
                              single_type(tt,s,false);
                         end
                       else
                         tt:=cformaltype;
                       repeat
                         s:=sc.get(declarepos);
                         if s='' then
                          break;
                         hp2:=TParaItem.create;
                         hp2.paratyp:=varspez;
                         hp2.paratype:=tt;
                         propertyparas.insert(hp2);
                       until false;
{$ifdef fixLeaksOnError}
                       if strContStack.pop <> sc then
                         writeln('problem with strContStack in ptype');
{$endif fixLeaksOnError}
                       sc.free;
                     until not try_to_consume(_SEMICOLON);
                     dec(testcurobject);
                     consume(_RECKKLAMMER);

                     { the parser need to know if a property has parameters, the
                       index parameter doesn't count (PFV) }
                     if not(propertyparas.empty) then
                       include(p.propoptions,ppo_hasparameters);
                  end;
                { overriden property ?                                 }
                { force property interface, if there is a property parameter }
                if (token=_COLON) or not(propertyparas.empty) then
                  begin
                     consume(_COLON);
                     single_type(p.proptype,hs,false);
                     if (idtoken=_INDEX) then
                       begin
                          consume(_INDEX);
                          pt:=comp_expr(true);
                          if is_constnode(pt) and
                             is_ordinal(pt.resulttype.def) and
                             (not is_64bitint(pt.resulttype.def)) then
                            p.index:=tordconstnode(pt).value
                          else
                            begin
                              Message(parser_e_invalid_property_index_value);
                              p.index:=0;
                            end;
                          p.indextype.setdef(pt.resulttype.def);
                          include(p.propoptions,ppo_indexed);
                          { concat a longint to the para template }
                          hp2:=TParaItem.Create;
                          hp2.paratyp:=vs_value;
                          hp2.paratype:=p.indextype;
                          propertyparas.insert(hp2);
                          pt.free;
                       end;
                  end
                else
                  begin
                     { do an property override }
                     overriden:=search_class_member(aktclass,propname);
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

                { create data defcoll to allow correct parameter checks }
                datacoll:=TParaItem.Create;
                datacoll.paratyp:=vs_value;
                datacoll.paratype:=p.proptype;

                if try_to_consume(_READ) then
                 begin
                   p.readaccess.clear;
                   if parse_symlist(p.readaccess) then
                    begin
                      sym:=p.readaccess.firstsym^.sym;
                      case sym.typ of
                        procsym :
                          begin
                            pd:=get_procdef;
                            if not(assigned(pd)) or
                               not(is_equal(pd.rettype.def,p.proptype.def)) then
                              Message(parser_e_ill_property_access_sym);
                            p.readaccess.setdef(pd);
                          end;
                        varsym :
                          begin
                            if CheckTypes(p.readaccess.def,p.proptype.def) then
                             begin
                               { property parameters are allowed if this is
                                 an indexed property, because the index is then
                                 the parameter.
                                 Note: In the help of Kylix it is written
                                 that it isn't allowed, but the compiler accepts it (PFV) }
                               if (ppo_hasparameters in p.propoptions) then
                                Message(parser_e_ill_property_access_sym);
                             end;
                          end;
                        else
                          Message(parser_e_ill_property_access_sym);
                      end;
                    end;
                 end;
                if try_to_consume(_WRITE) then
                 begin
                   p.writeaccess.clear;
                   if parse_symlist(p.writeaccess) then
                    begin
                      sym:=p.writeaccess.firstsym^.sym;
                      case sym.typ of
                        procsym :
                          begin
                            { insert data entry to check access method }
                            propertyparas.insert(datacoll);
                            pd:=get_procdef;
                            { ... and remove it }
                            propertyparas.remove(datacoll);
                            if not(assigned(pd)) then
                              Message(parser_e_ill_property_access_sym);
                            p.writeaccess.setdef(pd);
                          end;
                        varsym :
                          begin
                            if CheckTypes(p.writeaccess.def,p.proptype.def) then
                             begin
                               { property parameters are allowed if this is
                                 an indexed property, because the index is then
                                 the parameter.
                                 Note: In the help of Kylix it is written
                                 that it isn't allowed, but the compiler accepts it (PFV) }
                               if (ppo_hasparameters in p.propoptions) then
                                Message(parser_e_ill_property_access_sym);
                             end;
                          end;
                        else
                          Message(parser_e_ill_property_access_sym);
                      end;
                    end;
                 end;
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
                            if parse_symlist(p.storedaccess) then
                             begin
                               sym:=p.storedaccess.firstsym^.sym;
                               case sym.typ of
                                 procsym :
                                   begin
                                     pp:=tprocsym(sym).defs;
                                     while assigned(pp) do
                                      begin
                                        { the stored function shouldn't have any parameters }
                                        if pp^.def.Para.empty then
                                         break;
                                        pp:=pp^.next;
                                      end;
                                     { found we a procedure and does it really return a bool? }
                                     if assigned(pp) and
                                        is_boolean(pp^.def.rettype.def) then
                                       p.storedaccess.setdef(pp^.def)
                                     else
                                       Message(parser_e_ill_property_storage_sym);
                                   end;
                                 varsym :
                                   begin
                                     if (ppo_hasparameters in p.propoptions) or
                                        not(is_boolean(p.storedaccess.def)) then
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
                if try_to_consume(_DEFAULT) then
                  begin
                     if not(is_ordinal(p.proptype.def) or
                            is_64bitint(p.proptype.def) or
                            ((p.proptype.def.deftype=setdef) and
                             (tsetdef(p.proptype.def).settype=smallset))) or
                            ((p.proptype.def.deftype=arraydef) and
                             (ppo_indexed in p.propoptions)) or
                        not(propertyparas.empty) then
                       Message(parser_e_property_cant_have_a_default_value);
                     { Get the result of the default, the firstpass is
                       needed to support values like -1 }
                     pt:=comp_expr(true);
                     if (p.proptype.def.deftype=setdef) and
                        (pt.nodetype=arrayconstructorn) then
                       begin
                         arrayconstructor_to_set(tarrayconstructornode(pt));
                         do_resulttypepass(pt);
                       end;
                     inserttypeconv(pt,p.proptype);
                     if not(is_constnode(pt)) then
                       Message(parser_e_property_default_value_must_const);

                     if pt.nodetype=setconstn then
                       p.default:=plongint(tsetconstnode(pt).value_set)^
                     else
                       p.default:=tordconstnode(pt).value;
                     pt.free;
                  end
                else if try_to_consume(_NODEFAULT) then
                  begin
                     p.default:=0;
                  end;
                symtablestack.insert(p);
                { default property ? }
                consume(_SEMICOLON);
                if (idtoken=_DEFAULT) then
                  begin
                     consume(_DEFAULT);
                     { overriding a default propertyp is allowed
                     p2:=search_default_property(aktclass);
                     if assigned(p2) then
                       message1(parser_e_only_one_default_property,
                         tobjectdef(p2.owner.defowner)^.objname^)
                     else
                     }
                       begin
                          include(p.propoptions,ppo_defaultproperty);
                          if propertyparas.empty then
                            message(parser_e_property_need_paras);
                       end;
                     consume(_SEMICOLON);
                  end;
                { clean up }
                if assigned(datacoll) then
                  datacoll.free;
             end
           else
             begin
                consume(_ID);
                consume(_SEMICOLON);
             end;
           propertyparas.free;
        end;


      procedure destructor_head;
        begin
           consume(_DESTRUCTOR);
           inc(lexlevel);
           parse_proc_head(potype_destructor);
           dec(lexlevel);
           if (cs_constructor_name in aktglobalswitches) and (aktprocsym.name<>'DONE') then
            Message(parser_e_destructorname_must_be_done);
           include(aktclass.objectoptions,oo_has_destructor);
           consume(_SEMICOLON);
           if not(aktprocdef.Para.empty) then
             if not (m_tp in aktmodeswitches) then
               Message(parser_e_no_paras_for_destructor);
           { no return value }
           aktprocdef.rettype:=voidtype;
        end;

      var
         hs      : string;
         pcrd       : tclassrefdef;
         tt     : ttype;
         old_object_option : tsymoptions;
         oldprocinfo : pprocinfo;
         oldprocsym : tprocsym;
         oldprocdef : tprocdef;
         oldparse_only : boolean;
         storetypecanbeforward : boolean;

      procedure setclassattributes;

        begin
           { publishable }
           if classtype in [odt_interfacecom,odt_class] then
             begin
                aktclass.objecttype:=classtype;
                if (cs_generate_rtti in aktlocalswitches) or
                    (assigned(aktclass.childof) and
                     (oo_can_have_published in aktclass.childof.objectoptions)) then
                  begin
                     include(aktclass.objectoptions,oo_can_have_published);
                     { in "publishable" classes the default access type is published }
                     current_object_option:=[sp_published];
                  end;
             end;
        end;

     procedure setclassparent;

        begin
           if assigned(fd) then
             aktclass:=fd
           else
             aktclass:=tobjectdef.create(classtype,n,nil);
           { is the current class tobject?   }
           { so you could define your own tobject }
           if (cs_compilesystem in aktmoduleswitches) and (classtype=odt_class) and (upper(n)='TOBJECT') then
             class_tobject:=aktclass
           else if (cs_compilesystem in aktmoduleswitches) and (classtype=odt_interfacecom) and (upper(n)='IUNKNOWN') then
             interface_iunknown:=aktclass
           else
             begin
                case classtype of
                  odt_class:
                    childof:=class_tobject;
                  odt_interfacecom:
                    childof:=interface_iunknown;
                end;
                if (oo_is_forward in childof.objectoptions) then
                  Message1(parser_e_forward_declaration_must_be_resolved,childof.objname^);
                aktclass.set_parent(childof);
             end;
         end;

      procedure setinterfacemethodoptions;

        var
          i: longint;
          defs: TIndexArray;
          pd: tprocdef;
        begin
          include(aktclass.objectoptions,oo_has_virtual);
          defs:=aktclass.symtable.defindex;
          for i:=1 to defs.count do
            begin
              pd:=tprocdef(defs.search(i));
              if pd.deftype=procdef then
                begin
                  pd.extnumber:=aktclass.lastvtableindex;
                  inc(aktclass.lastvtableindex);
                  include(pd.procoptions,po_virtualmethod);
                  pd.forwarddef:=false;
                end;
            end;
        end;

      function readobjecttype : boolean;

        begin
           readobjecttype:=true;
           { distinguish classes and objects }
           case token of
              _OBJECT:
                begin
                   classtype:=odt_object;
                   consume(_OBJECT)
                end;
              _CPPCLASS:
                begin
                   classtype:=odt_cppclass;
                   consume(_CPPCLASS);
                end;
              _INTERFACE:
                begin
                   if aktinterfacetype=it_interfacecom then
                     classtype:=odt_interfacecom
                   else {it_interfacecorba}
                     classtype:=odt_interfacecorba;
                   consume(_INTERFACE);
                   { forward declaration }
                   if not(assigned(fd)) and (token=_SEMICOLON) then
                     begin
                       { also anonym objects aren't allow (o : object a : longint; end;) }
                       if n='' then
                         Message(parser_f_no_anonym_objects);
                       aktclass:=tobjectdef.create(classtype,n,nil);
                       if (cs_compilesystem in aktmoduleswitches) and
                          (classtype=odt_interfacecom) and (upper(n)='IUNKNOWN') then
                         interface_iunknown:=aktclass;
                       include(aktclass.objectoptions,oo_is_forward);
                       object_dec:=aktclass;
                       typecanbeforward:=storetypecanbeforward;
                       readobjecttype:=false;
                       exit;
                     end;
                end;
              _CLASS:
                begin
                   classtype:=odt_class;
                   consume(_CLASS);
                   if not(assigned(fd)) and
                      (token=_OF) and
                      { Delphi only allows class of in type blocks.
                        Note that when parsing the type of a variable declaration
                        the blocktype is bt_type so the check for typecanbeforward
                        is also necessary (PFV) }
                      (((block_type=bt_type) and typecanbeforward) or
                       not(m_delphi in aktmodeswitches)) then
                     begin
                        { a hack, but it's easy to handle }
                        { class reference type }
                        consume(_OF);
                        single_type(tt,hs,typecanbeforward);

                        { accept hp1, if is a forward def or a class }
                        if (tt.def.deftype=forwarddef) or
                           is_class(tt.def) then
                          begin
                             pcrd:=tclassrefdef.create(tt);
                             object_dec:=pcrd;
                          end
                        else
                          begin
                             object_dec:=generrortype.def;
                             Message1(type_e_class_type_expected,generrortype.def.typename);
                          end;
                        typecanbeforward:=storetypecanbeforward;
                        readobjecttype:=false;
                        exit;
                     end
                   { forward class }
                   else if not(assigned(fd)) and (token=_SEMICOLON) then
                     begin
                        { also anonym objects aren't allow (o : object a : longint; end;) }
                        if n='' then
                          Message(parser_f_no_anonym_objects);
                        aktclass:=tobjectdef.create(odt_class,n,nil);
                        if (cs_compilesystem in aktmoduleswitches) and (upper(n)='TOBJECT') then
                          class_tobject:=aktclass;
                        aktclass.objecttype:=odt_class;
                        include(aktclass.objectoptions,oo_is_forward);
                        { all classes must have a vmt !!  at offset zero }
                        if not(oo_has_vmt in aktclass.objectoptions) then
                          aktclass.insertvmt;

                        object_dec:=aktclass;
                        typecanbeforward:=storetypecanbeforward;
                        readobjecttype:=false;
                        exit;
                     end;
                end;
              else
                begin
                   classtype:=odt_class; { this is error but try to recover }
                   consume(_OBJECT);
                end;
           end;
        end;

      procedure readimplementedinterfaces;
        var
          implintf: tobjectdef;
          tt      : ttype;
        begin
          while try_to_consume(_COMMA) do begin
            id_type(tt,pattern,false);
            implintf:=tobjectdef(tt.def);
            if (tt.def.deftype<>objectdef) then begin
              Message1(type_e_interface_type_expected,tt.def.typename);
              Continue; { omit }
            end;
            if not is_interface(implintf) then begin
              Message1(type_e_interface_type_expected,implintf.typename);
              Continue; { omit }
            end;
            if aktclass.implementedinterfaces.searchintf(tt.def)<>-1 then
              Message1(sym_e_duplicate_id,tt.def.name)
            else
              aktclass.implementedinterfaces.addintf(tt.def);
          end;
        end;

      procedure readinterfaceiid;
        var
          p : tnode;
        begin
          p:=comp_expr(true);
          if p.nodetype=stringconstn then
            begin
              aktclass.iidstr:=stringdup(strpas(tstringconstnode(p).value_str)); { or upper? }
              p.free;
              aktclass.isiidguidvalid:=string2guid(aktclass.iidstr^,aktclass.iidguid);
              if (classtype=odt_interfacecom) and not aktclass.isiidguidvalid then
                Message(parser_e_improper_guid_syntax);
            end
          else
            begin
              p.free;
              Message(cg_e_illegal_expression);
            end;
        end;


      procedure readparentclasses;
        begin
           { reads the parent class }
           if token=_LKLAMMER then
             begin
                consume(_LKLAMMER);
                id_type(tt,pattern,false);
                childof:=tobjectdef(tt.def);
                if (not assigned(childof)) or
                   (childof.deftype<>objectdef) then
                 begin
                   if assigned(childof) then
                    Message1(type_e_class_type_expected,childof.typename);
                   childof:=nil;
                   aktclass:=tobjectdef.create(classtype,n,nil);
                 end
                else
                 begin
                   { a mix of class, interfaces, objects and cppclasses
                     isn't allowed }
                   case classtype of
                      odt_class:
                        if not(is_class(childof)) and
                          not(is_interface(childof)) then
                          Message(parser_e_mix_of_classes_and_objects);
                      odt_interfacecorba,
                      odt_interfacecom:
                        if not(is_interface(childof)) then
                          Message(parser_e_mix_of_classes_and_objects);
                      odt_cppclass:
                        if not(is_cppclass(childof)) then
                          Message(parser_e_mix_of_classes_and_objects);
                      odt_object:
                        if not(is_object(childof)) then
                          Message(parser_e_mix_of_classes_and_objects);
                   end;
                   { the forward of the child must be resolved to get
                     correct field addresses }
                   if assigned(fd) then
                    begin
                      if (oo_is_forward in childof.objectoptions) then
                       Message1(parser_e_forward_declaration_must_be_resolved,childof.objname^);
                      aktclass:=fd;
                      { we must inherit several options !!
                        this was missing !!
                        all is now done in set_parent
                        including symtable datasize setting PM }
                      fd.set_parent(childof);
                    end
                   else
                    aktclass:=tobjectdef.create(classtype,n,childof);
                   if aktclass.objecttype=odt_class then
                    readimplementedinterfaces;
                 end;
                consume(_RKLAMMER);
             end
           { if no parent class, then a class get tobject as parent }
           else if classtype in [odt_class,odt_interfacecom] then
             setclassparent
           else
             aktclass:=tobjectdef.create(classtype,n,nil);
           { read GUID }
             if (classtype in [odt_interfacecom,odt_interfacecorba]) and
                try_to_consume(_LECKKLAMMER) then
               begin
                 readinterfaceiid;
                 consume(_RECKKLAMMER);
               end;
        end;

      procedure chkcpp;

        begin
           if is_cppclass(aktclass) then
             begin
                aktprocdef.proccalloption:=pocall_cppdecl;
                aktprocdef.setmangledname(
                  target_info.Cprefix+aktprocdef.cplusplusmangledname);
             end;
        end;

      begin
         {Nowadays aktprocsym may already have a value, so we need to save
          it.}
         oldprocdef:=aktprocdef;
         oldprocsym:=aktprocsym;
         old_object_option:=current_object_option;

         { forward is resolved }
         if assigned(fd) then
           exclude(fd.objectoptions,oo_is_forward);

         { objects and class types can't be declared local }
         if not(symtablestack.symtabletype in [globalsymtable,staticsymtable]) then
           Message(parser_e_no_local_objects);

         storetypecanbeforward:=typecanbeforward;
         { for tp mode don't allow forward types }
         if (m_tp in aktmodeswitches) and
            not (m_delphi in aktmodeswitches) then
           typecanbeforward:=false;

         if not(readobjecttype) then
           exit;

         { also anonym objects aren't allow (o : object a : longint; end;) }
         if n='' then
           Message(parser_f_no_anonym_objects);

         { read list of parent classes }
         readparentclasses;

         { default access is public }
         there_is_a_destructor:=false;
         current_object_option:=[sp_public];

         { set class flags and inherits published }
         setclassattributes;

         aktobjectdef:=aktclass;
         aktclass.symtable.next:=symtablestack;
         symtablestack:=aktclass.symtable;
         testcurobject:=1;
         curobjectname:=Upper(n);

         { new procinfo }
         oldprocinfo:=procinfo;
         new(procinfo,init);
         procinfo^._class:=aktclass;

         { short class declaration ? }
         if (classtype<>odt_class) or (token<>_SEMICOLON) then
          begin
          { Parse componenten }
            repeat
              case token of
                _ID :
                  begin
                    case idtoken of
                      _PRIVATE :
                        begin
                          if is_interface(aktclass) then
                             Message(parser_e_no_access_specifier_in_interfaces);
                           consume(_PRIVATE);
                           current_object_option:=[sp_private];
                           include(aktclass.objectoptions,oo_has_private);
                         end;
                       _PROTECTED :
                         begin
                           if is_interface(aktclass) then
                             Message(parser_e_no_access_specifier_in_interfaces);
                           consume(_PROTECTED);
                           current_object_option:=[sp_protected];
                           include(aktclass.objectoptions,oo_has_protected);
                         end;
                       _PUBLIC :
                         begin
                           if is_interface(aktclass) then
                             Message(parser_e_no_access_specifier_in_interfaces);
                           consume(_PUBLIC);
                           current_object_option:=[sp_public];
                         end;
                       _PUBLISHED :
                         begin
                           if is_interface(aktclass) then
                             Message(parser_e_no_access_specifier_in_interfaces)
                           else
                             if not(oo_can_have_published in aktclass.objectoptions) then
                               Message(parser_e_cant_have_published);
                           consume(_PUBLISHED);
                           current_object_option:=[sp_published];
                         end;
                       else
                         begin
                           if is_interface(aktclass) then
                            Message(parser_e_no_vars_in_interfaces);
                           read_var_decs(false,true,false);
                         end;
                    end;
                  end;
                _PROPERTY :
                  begin
                    property_dec;
                  end;
                _PROCEDURE,
                _FUNCTION,
                _CLASS :
                  begin
                    oldparse_only:=parse_only;
                    parse_only:=true;
                    parse_proc_dec;
                    { this is for error recovery as well as forward }
                    { interface mappings, i.e. mapping to a method  }
                    { which isn't declared yet                      }
                    if assigned(aktprocsym) then
                      begin
                          parse_object_proc_directives(aktprocsym);

                          { add definition to procsym }
                          proc_add_definition(aktprocsym,aktprocdef);

                          { add procdef options to objectdef options }
                          if (po_msgint in aktprocdef.procoptions) then
                           include(aktclass.objectoptions,oo_has_msgint);
                          if (po_msgstr in aktprocdef.procoptions) then
                            include(aktclass.objectoptions,oo_has_msgstr);
                          if (po_virtualmethod in aktprocdef.procoptions) then
                            include(aktclass.objectoptions,oo_has_virtual);

                          chkcpp;
                       end;

                    parse_only:=oldparse_only;
                  end;
                _CONSTRUCTOR :
                  begin
                    if not(sp_public in current_object_option) then
                      Message(parser_w_constructor_should_be_public);
                    if is_interface(aktclass) then
                      Message(parser_e_no_con_des_in_interfaces);
                    oldparse_only:=parse_only;
                    parse_only:=true;
                    constructor_head;
                    parse_object_proc_directives(aktprocsym);

                    { add definition to procsym }
                    proc_add_definition(aktprocsym,aktprocdef);

                    { add procdef options to objectdef options }
                    if (po_virtualmethod in aktprocdef.procoptions) then
                      include(aktclass.objectoptions,oo_has_virtual);

                    chkcpp;

                    parse_only:=oldparse_only;
                  end;
                _DESTRUCTOR :
                  begin
                    if there_is_a_destructor then
                      Message(parser_n_only_one_destructor);
                    if is_interface(aktclass) then
                      Message(parser_e_no_con_des_in_interfaces);
                    there_is_a_destructor:=true;
                    if not(sp_public in current_object_option) then
                      Message(parser_w_destructor_should_be_public);
                    oldparse_only:=parse_only;
                    parse_only:=true;
                    destructor_head;
                    parse_object_proc_directives(aktprocsym);

                    { add definition to procsym }
                    proc_add_definition(aktprocsym,aktprocdef);

                    { add procdef options to objectdef options }
                    if (po_virtualmethod in aktprocdef.procoptions) then
                      include(aktclass.objectoptions,oo_has_virtual);

                    chkcpp;

                    parse_only:=oldparse_only;
                  end;
                _END :
                  begin
                    consume(_END);
                    break;
                  end;
                else
                  consume(_ID); { Give a ident expected message, like tp7 }
              end;
            until false;
          end;

         { generate vmt space if needed }
         if not(oo_has_vmt in aktclass.objectoptions) and
            (([oo_has_virtual,oo_has_constructor,oo_has_destructor]*aktclass.objectoptions<>[]) or
             (classtype in [odt_class])
            ) then
           aktclass.insertvmt;

         if is_interface(aktclass) then
           setinterfacemethodoptions;

         { reset }
         testcurobject:=0;
         curobjectname:='';
         typecanbeforward:=storetypecanbeforward;
         { restore old state }
         symtablestack:=symtablestack.next;
         aktobjectdef:=nil;
         {Restore procinfo}
         dispose(procinfo,done);
         procinfo:=oldprocinfo;
         {Restore the aktprocsym.}
         aktprocsym:=oldprocsym;
         aktprocdef:=oldprocdef;
         current_object_option:=old_object_option;

         object_dec:=aktclass;
      end;

end.
{
  $Log$
  Revision 1.36  2002-01-06 12:08:15  peter
    * removed uauto from orddef, use new range_to_basetype generating
      the correct ordinal type for a range

  Revision 1.34  2001/12/06 17:57:35  florian
    + parasym to tparaitem added

  Revision 1.33  2001/11/02 22:58:02  peter
    * procsym definition rewrite

  Revision 1.32  2001/10/25 21:22:35  peter
    * calling convention rewrite

  Revision 1.31  2001/10/21 13:10:50  peter
    * better support for indexed properties

  Revision 1.30  2001/10/21 12:33:06  peter
    * array access for properties added

  Revision 1.29  2001/08/30 20:13:53  peter
    * rtti/init table updates
    * rttisym for reusable global rtti/init info
    * support published for interfaces

  Revision 1.28  2001/08/26 13:36:44  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.27  2001/08/22 21:16:20  florian
    * some interfaces related problems regarding
      mapping of interface implementions fixed

  Revision 1.26  2001/06/03 21:57:36  peter
    + hint directive parsing support

  Revision 1.25  2001/05/04 15:52:03  florian
    * some Delphi incompatibilities fixed:
       - out, dispose and new can be used as idenfiers now
       - const p = apointerype(nil); is supported now
    + support for const p = apointertype(pointer(1234)); added

  Revision 1.24  2001/04/21 15:36:00  peter
    * check for type block when parsing class of

  Revision 1.23  2001/04/21 13:37:16  peter
    * made tclassheader using class of to implement cpu dependent code

  Revision 1.22  2001/04/18 22:01:54  peter
    * registration of targets and assemblers

  Revision 1.21  2001/04/13 01:22:11  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.20  2001/04/04 22:43:51  peter
    * remove unnecessary calls to firstpass

  Revision 1.19  2001/04/04 21:30:43  florian
    * applied several fixes to get the DD8 Delphi Unit compiled
     e.g. "forward"-interfaces are working now

  Revision 1.18  2001/04/02 21:20:31  peter
    * resulttype rewrite

  Revision 1.17  2001/03/16 14:56:38  marco
   * Pavel's fixes commited (Peter asked). Cycled to test

  Revision 1.16  2001/03/11 22:58:49  peter
    * getsym redesign, removed the globals srsym,srsymtable

  Revision 1.15  2000/12/25 00:07:27  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.14  2000/11/29 00:30:35  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.13  2000/11/17 17:32:58  florian
    * properties can now be used in interfaces

  Revision 1.12  2000/11/17 08:21:07  florian
  *** empty log message ***

  Revision 1.11  2000/11/12 23:24:11  florian
    * interfaces are basically running

  Revision 1.10  2000/11/12 22:17:47  peter
    * some realname updates for messages

  Revision 1.9  2000/11/06 23:05:52  florian
    * more fixes

  Revision 1.8  2000/11/06 20:30:55  peter
    * more fixes to get make cycle working

  Revision 1.7  2000/11/04 18:03:57  florian
    * fixed upper/lower case problem

  Revision 1.6  2000/11/04 17:31:00  florian
    * fixed some problems of previous commit

  Revision 1.5  2000/11/04 14:25:20  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.4  2000/10/31 22:02:49  peter
    * symtable splitted, no real code changes

  Revision 1.3  2000/10/26 21:54:03  peter
    * fixed crash with error in child definition (merged)

  Revision 1.2  2000/10/21 18:16:11  florian
    * a lot of changes:
       - basic dyn. array support
       - basic C++ support
       - some work for interfaces done
       ....

  Revision 1.1  2000/10/14 10:14:51  peter
    * moehrendorf oct 2000 rewrite

}
