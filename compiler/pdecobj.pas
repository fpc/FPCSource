{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

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

{$i fpcdefs.inc}

interface

    uses
      globtype,symtype,symdef;

    { parses a object declaration }
    function object_dec(const n : stringid;fd : tobjectdef) : tdef;

implementation

    uses
      cutils,cclasses,
      globals,verbose,systems,tokens,
      symconst,symbase,symsym,symtable,defbase,
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
           propertyparas : tparalinkedlist;

        { returns the matching procedure to access a property }
{        function get_procdef : tprocdef;
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
          end;}

        var
           hp2,datacoll : tparaitem;
           p : tpropertysym;
           overriden : tsym;
           hs : string;
           varspez : tvarspez;
           s : string;
           tt : ttype;
           arraytype : ttype;
           declarepos : tfileposinfo;
           pp : Tprocdef;
           pd : tprocdef;
           pt : tnode;
           propname : stringid;
           dummyst : tparasymtable;
           vs : tvarsym;
           sc : tsinglelist;
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

                     dummyst:=tparasymtable.create;
                     dummyst.next:=symtablestack;
                     symtablestack:=dummyst;
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
                         vs:=tvarsym.create(orgpattern,generrortype);
                         dummyst.insert(vs);
                         sc.insert(vs);
                         consume(_ID);
                       until not try_to_consume(_COMMA);
                       if token=_COLON then
                         begin
                            consume(_COLON);
                            if token=_ARRAY then
                              begin
                                 consume(_ARRAY);
                                 consume(_OF);
                                 { define range and type of range }
                                 tt.setdef(tarraydef.create(0,-1,s32bittype));
                                 { define field type }
                                 single_type(arraytype,s,false);
                                 tarraydef(tt.def).setelementtype(arraytype);
                              end
                            else
                              single_type(tt,s,false);
                         end
                       else
                         tt:=cformaltype;
                       vs:=tvarsym(sc.first);
                       while assigned(vs) do
                        begin
                          hp2:=TParaItem.create;
                          hp2.paratyp:=varspez;
                          hp2.paratype:=tt;
                          propertyparas.insert(hp2);
                          vs:=tvarsym(vs.listnext);
                        end;
                     until not try_to_consume(_SEMICOLON);
                     dec(testcurobject);
                     consume(_RECKKLAMMER);

                     { remove dummy symtable }
                     symtablestack:=symtablestack.next;
                     dummyst.free;
                     sc.free;

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
                            pd:=Tprocsym(sym).search_procdef_bypara(propertyparas,true,false);
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
                            pd:=Tprocsym(sym).search_procdef_bypara(propertyparas,true,false);
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
                                      pp:=Tprocsym(sym).search_procdef_nopara_boolret;
                                      if assigned(pp) then
                                        p.storedaccess.setdef(pp)
                                      else
                                        message(parser_e_ill_property_storage_sym);
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
                        (ppo_hasparameters in p.propoptions) then
                       Message(parser_e_property_cant_have_a_default_value);
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
                if try_to_consume(_DEFAULT) then
                  begin
                     { overriding a default propertyp is allowed
                     p2:=search_default_property(aktclass);
                     if assigned(p2) then
                       message1(parser_e_only_one_default_property,
                         tobjectdef(p2.owner.defowner)^.objrealname^)
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
             if (m_fpc in aktmodeswitches) then
               Message(parser_e_no_paras_for_destructor);
           { no return value }
           aktprocdef.rettype:=voidtype;
        end;

      var
         hs      : string;
         pcrd       : tclassrefdef;
         tt     : ttype;
         old_object_option : tsymoptions;
         oldprocinfo : tprocinfo;
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
                  Message1(parser_e_forward_declaration_must_be_resolved,childof.objrealname^);
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

      procedure handleimplementedinterface(implintf : tobjectdef);

        begin
            if not is_interface(implintf) then
              begin
                 Message1(type_e_interface_type_expected,implintf.typename);
                 exit;
              end;
            if aktclass.implementedinterfaces.searchintf(implintf)<>-1 then
              Message1(sym_e_duplicate_id,implintf.name)
            else
              aktclass.implementedinterfaces.addintf(implintf);
        end;

      procedure readimplementedinterfaces;
        var
          tt      : ttype;
        begin
          while try_to_consume(_COMMA) do
            begin
               id_type(tt,pattern,false);
               if (tt.def.deftype<>objectdef) then
                 begin
                    Message1(type_e_interface_type_expected,tt.def.typename);
                    continue;
                 end;
               handleimplementedinterface(tobjectdef(tt.def));
            end;
        end;

      procedure readinterfaceiid;
        var
          p : tnode;
        begin
          p:=comp_expr(true);
          if p.nodetype=stringconstn then
            begin
              stringdispose(aktclass.iidstr);
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
        var
           hp : tobjectdef;
        begin
           hp:=nil;
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
                        if not(is_class(childof)) then
                          begin
                             if is_interface(childof) then
                               begin
                                  { we insert the interface after the child
                                    is set, see below
                                  }
                                  hp:=childof;
                                  childof:=class_tobject;
                               end
                             else
                               Message(parser_e_mix_of_classes_and_objects);
                          end;
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
                       Message1(parser_e_forward_declaration_must_be_resolved,childof.objrealname^);
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
                     begin
                        if assigned(hp) then
                          handleimplementedinterface(hp);
                        readimplementedinterfaces;
                     end;
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
         { for tp7 don't allow forward types }
         if (m_tp7 in aktmodeswitches) then
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
         procinfo:=cprocinfo.create;
         procinfo._class:=aktclass;

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
                           { we've to check for a pushlished section in non-  }
                           { publishable classes later, if a real declaration }
                           { this is the way, delphi does it                  }
                           if is_interface(aktclass) then
                             Message(parser_e_no_access_specifier_in_interfaces);
                           consume(_PUBLISHED);
                           current_object_option:=[sp_published];
                         end;
                       else
                         begin
                           if is_interface(aktclass) then
                             Message(parser_e_no_vars_in_interfaces);

                           if (sp_published in current_object_option) and
                             not(oo_can_have_published in aktclass.objectoptions) then
                             Message(parser_e_cant_have_published);

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
                    if (sp_published in current_object_option) and
                      not(oo_can_have_published in aktclass.objectoptions) then
                      Message(parser_e_cant_have_published);

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
                    if (sp_published in current_object_option) and
                      not(oo_can_have_published in aktclass.objectoptions) then
                      Message(parser_e_cant_have_published);

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
                    if (sp_published in current_object_option) and
                      not(oo_can_have_published in aktclass.objectoptions) then
                      Message(parser_e_cant_have_published);

                    if there_is_a_destructor then
                      Message(parser_n_only_one_destructor);

                    if is_interface(aktclass) then
                      Message(parser_e_no_con_des_in_interfaces);

                    if not(sp_public in current_object_option) then
                      Message(parser_w_destructor_should_be_public);

                    there_is_a_destructor:=true;
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
         procinfo.free;
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
  Revision 1.53  2002-09-27 21:13:28  carl
    * low-highval always checked if limit ober 2GB is reached (to avoid overflow)

  Revision 1.52  2002/09/16 14:11:13  peter
    * add argument to equal_paras() to support default values or not

  Revision 1.51  2002/09/09 17:34:15  peter
    * tdicationary.replace added to replace and item in a dictionary. This
      is only allowed for the same name
    * varsyms are inserted in symtable before the types are parsed. This
      fixes the long standing "var longint : longint" bug
    - consume_idlist and idstringlist removed. The loops are inserted
      at the callers place and uses the symtable for duplicate id checking

  Revision 1.50  2002/09/03 16:26:26  daniel
    * Make Tprocdef.defs protected

  Revision 1.49  2002/08/17 09:23:38  florian
    * first part of procinfo rewrite

  Revision 1.48  2002/08/09 07:33:02  florian
    * a couple of interface related fixes

  Revision 1.47  2002/07/20 11:57:55  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.46  2002/07/01 16:23:53  peter
    * cg64 patch
    * basics for currency
    * asnode updates for class and interface (not finished)

  Revision 1.45  2002/05/18 13:34:12  peter
    * readded missing revisions

  Revision 1.44  2002/05/16 19:46:42  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.42  2002/05/12 16:53:08  peter
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

  Revision 1.41  2002/04/21 19:02:04  peter
    * removed newn and disposen nodes, the code is now directly
      inlined from pexpr
    * -an option that will write the secondpass nodes to the .s file, this
      requires EXTDEBUG define to actually write the info
    * fixed various internal errors and crashes due recent code changes

  Revision 1.40  2002/04/19 15:46:02  peter
    * mangledname rewrite, tprocdef.mangledname is now created dynamicly
      in most cases and not written to the ppu
    * add mangeledname_prefix() routine to generate the prefix of
      manglednames depending on the current procedure, object and module
    * removed static procprefix since the mangledname is now build only
      on demand from tprocdef.mangledname

  Revision 1.39  2002/04/04 19:06:00  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.38  2002/01/25 17:38:19  peter
    * fixed default value for properties with index values

  Revision 1.37  2002/01/24 18:25:48  peter
   * implicit result variable generation for assembler routines
   * removed m_tp modeswitch, use m_tp7 or not(m_fpc) instead

  Revision 1.36  2002/01/06 12:08:15  peter
    * removed uauto from orddef, use new range_to_basetype generating
      the correct ordinal type for a range

}
