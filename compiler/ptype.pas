{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Does parsing types for Free Pascal

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
unit ptype;
interface

uses
  globtype,symtable;


    const
       { forward types should only be possible inside a TYPE statement }
       typecanbeforward : boolean = false;

    var
       { hack, which allows to use the current parsed }
       { object type as function argument type  }
       testcurobject : byte;
       curobjectname : stringid;

    { parses a string declaration }
    function string_dec : pdef;

    { parses a object declaration }
    function object_dec(const n : stringid;fd : pobjectdef) : pdef;


    { reads a string, file type or a type id and returns a name and }
    { pdef                                                        }
    procedure single_type(var tt:ttype;var s : string;isforwarddef:boolean);

    procedure read_type(var tt:ttype;const name : stringid);


implementation

uses
  cobjects,globals,verbose,systems,tokens,
  aasm,symconst,types,
{$ifdef GDB}
  gdb,
{$endif}
  tree,hcodegen,hcgdata,
  scanner,pbase,pexpr,pdecl,psub,
{$ifdef newcg}
  cgbase,
{$else}
  tccnv,
{$endif}
  pass_1;


    function string_dec : pdef;
    { reads a string type with optional length }
    { and returns a pointer to the string      }
    { definition                               }
      var
         p : ptree;
         d : pdef;
      begin
         consume(_STRING);
         if token=_LECKKLAMMER then
           begin
              consume(_LECKKLAMMER);
              p:=comp_expr(true);
              do_firstpass(p);
              if not is_constintnode(p) then
                Message(cg_e_illegal_expression);
              if (p^.value<=0) then
                begin
                   Message(parser_e_invalid_string_size);
                   p^.value:=255;
                end;
              consume(_RECKKLAMMER);
              if p^.value>255 then
                d:=new(pstringdef,longinit(p^.value))
              else
                if p^.value<>255 then
                  d:=new(pstringdef,shortinit(p^.value))
              else
                d:=cshortstringdef;
              disposetree(p);
           end
          else
            begin
               if cs_ansistrings in aktlocalswitches then
                 d:=cansistringdef
               else
                 d:=cshortstringdef;
            end;
          string_dec:=d;
       end;


    procedure id_type(var tt : ttype;var s : string;isforwarddef:boolean);
    { reads a type definition }
    { to a appropriating pdef, s gets the name of   }
    { the type to allow name mangling          }
      var
        is_unit_specific : boolean;
        pos : tfileposinfo;
      begin
         s:=pattern;
         pos:=tokenpos;
         { classes can be used also in classes }
         if (curobjectname=pattern) and aktobjectdef^.is_class then
           begin
              tt.setdef(aktobjectdef);
              consume(_ID);
              exit;
           end;
         { objects can be parameters }
         if (testcurobject=2) and (curobjectname=pattern) then
           begin
              tt.setdef(aktobjectdef);
              consume(_ID);
              exit;
           end;
         { try to load the symbol to see if it's a unitsym }
         is_unit_specific:=false;
         getsym(s,false);
         consume(_ID);
         if assigned(srsym) and
            (srsym^.typ=unitsym) then
           begin
              consume(_POINT);
              getsymonlyin(punitsym(srsym)^.unitsymtable,pattern);
              pos:=tokenpos;
              s:=pattern;
              consume(_ID);
              is_unit_specific:=true;
           end;
         { are we parsing a possible forward def ? }
         if isforwarddef and
            not(is_unit_specific) then
          begin
            tt.setdef(new(pforwarddef,init(s,pos)));
            exit;
          end;
         { unknown sym ? }
         if not assigned(srsym) then
          begin
            Message1(sym_e_id_not_found,s);
            tt.setdef(generrordef);
            exit;
          end;
         if (srsym^.typ<>typesym) then
          begin
            Message(type_e_type_id_expected);
            tt.setdef(generrordef);
            exit;
          end;
         { Only use the definitions for system/current unit, becuase
           they can be refered from the parameters and symbols are not
           loaded at that time. A symbol reference to an other unit
           is still possible, because it's already loaded (PFV)
           can't use in [] here, becuase unitid can be > 255 }
         if (ptypesym(srsym)^.owner^.unitid=0) or
            (ptypesym(srsym)^.owner^.unitid=1) then
          tt.setdef(ptypesym(srsym)^.restype.def)
         else
          tt.setsym(srsym);
      end;


    procedure single_type(var tt:ttype;var s : string;isforwarddef:boolean);
    { reads a string, file type or a type id and returns a name and }
    { pdef                                                        }
       var
          hs : string;
          t2 : ttype;
       begin
          case token of
            _STRING:
                begin
                   tt.setdef(string_dec);
                   s:='STRING';
                end;
            _FILE:
                begin
                   consume(_FILE);
                   if token=_OF then
                     begin
                        consume(_OF);
                        single_type(t2,hs,false);
                        tt.setdef(new(pfiledef,inittyped(t2)));
                        s:='FILE$OF$'+hs;
                     end
                   else
                     begin
                        tt.setdef(cfiledef);
                        s:='FILE';
                     end;
                end;
            else
              begin
                id_type(tt,s,isforwarddef);
              end;
         end;
      end;


    function object_dec(const n : stringid;fd : pobjectdef) : pdef;
    { this function parses an object or class declaration }
      var
         actmembertype : tsymoptions;
         there_is_a_destructor : boolean;
         is_a_class : boolean;
         childof : pobjectdef;
         aktclass : pobjectdef;

      procedure constructor_head;

        begin
           consume(_CONSTRUCTOR);
           { must be at same level as in implementation }
           inc(lexlevel);
           parse_proc_head(potype_constructor);
           dec(lexlevel);

           if (cs_constructor_name in aktglobalswitches) and (aktprocsym^.name<>'INIT') then
            Message(parser_e_constructorname_must_be_init);

{$ifdef INCLUDEOK}
           include(aktclass^.objectoptions,oo_has_constructor);
{$else}
           aktclass^.objectoptions:=aktclass^.objectoptions+[oo_has_constructor];
{$endif}
           consume(_SEMICOLON);
             begin
                if (aktclass^.is_class) then
                  begin
                     { CLASS constructors return the created instance }
                     aktprocsym^.definition^.rettype.def:=aktclass;
                  end
                else
                  begin
                     { OBJECT constructors return a boolean }
                     aktprocsym^.definition^.rettype.setdef(booldef);
                  end;
             end;
        end;


      procedure property_dec;

        var
           sym : psym;
           propertyparas : plinkedlist;

        { returns the matching procedure to access a property }
        function get_procdef : pprocdef;

          var
             p : pprocdef;

          begin
             p:=pprocsym(sym)^.definition;
             get_procdef:=nil;
             while assigned(p) do
               begin
                  if equal_paras(p^.para,propertyparas,true) then
                    break;
                  p:=p^.nextoverloaded;
               end;
             get_procdef:=p;
          end;

        var
           hp2,datacoll : pparaitem;
           p,p2 : ppropertysym;
           overriden : psym;
           hs : string;
           varspez : tvarspez;
           sc : pstringcontainer;
           s : string;
           tt : ttype;
           declarepos : tfileposinfo;
           pp : pprocdef;
           pt : ptree;
           propname : stringid;

        begin
           { check for a class }
           if not(aktclass^.is_class) then
            Message(parser_e_syntax_error);
           consume(_PROPERTY);
           new(propertyparas,init);
           datacoll:=nil;
           if token=_ID then
             begin
                p:=new(ppropertysym,init(pattern));
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
                       else varspez:=vs_value;
                       sc:=idlist;
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
                                 tt.setdef(new(parraydef,init(0,-1,s32bitdef)));
                                 { define field type }
                                 single_type(parraydef(tt.def)^.elementtype,s,false);
                              end
                            else
                              single_type(tt,s,false);
                         end
                       else
                         tt.setdef(cformaldef);
                       repeat
                         s:=sc^.get_with_tokeninfo(declarepos);
                         if s='' then
                          break;
                         new(hp2,init);
                         hp2^.paratyp:=varspez;
                         hp2^.paratype:=tt;
                         propertyparas^.insert(hp2);
                       until false;
{$ifdef fixLeaksOnError}
                       if strContStack.pop <> sc then
                         writeln('problem with strContStack in ptype');
{$endif fixLeaksOnError}
                       dispose(sc,done);
                     until not try_to_consume(_SEMICOLON);
                     dec(testcurobject);
                     consume(_RECKKLAMMER);
                  end;
                { overriden property ?                                 }
                { force property interface, if there is a property parameter }
                if (token=_COLON) or not(propertyparas^.empty) then
                  begin
                     consume(_COLON);
                     single_type(p^.proptype,hs,false);
                     if (idtoken=_INDEX) then
                       begin
                          consume(_INDEX);
                          pt:=comp_expr(true);
                          do_firstpass(pt);
                          if not(is_ordinal(pt^.resulttype)) or
                             is_64bitint(pt^.resulttype) then
                            Message(parser_e_invalid_property_index_value);
                          p^.index:=pt^.value;
                          p^.indextype.setdef(pt^.resulttype);
                          include(p^.propoptions,ppo_indexed);
                          { concat a longint to the para template }
                          new(hp2,init);
                          hp2^.paratyp:=vs_value;
                          hp2^.paratype:=p^.indextype;
                          propertyparas^.insert(hp2);
                          disposetree(pt);
                       end;
                     { the parser need to know if a property has parameters }
                     if not(propertyparas^.empty) then
                       p^.propoptions:=p^.propoptions+[ppo_hasparameters];
                  end
                else
                  begin
                     { do an property override }
                     overriden:=search_class_member(aktclass,propname);
                     if assigned(overriden) and (overriden^.typ=propertysym) then
                       begin
                         p^.dooverride(ppropertysym(overriden));
                       end
                     else
                       begin
                         p^.proptype.setdef(generrordef);
                         message(parser_e_no_property_found_to_override);
                       end;
                  end;
                if (sp_published in current_object_option) and
                   not(p^.proptype.def^.is_publishable) then
                  Message(parser_e_cant_publish_that_property);

                { create data defcoll to allow correct parameter checks }
                new(datacoll,init);
                datacoll^.paratyp:=vs_value;
                datacoll^.paratype:=p^.proptype;

                if (idtoken=_READ) then
                  begin
                     p^.readaccess^.clear;
                     consume(_READ);
                     sym:=search_class_member(aktclass,pattern);
                     if not(assigned(sym)) then
                       begin
                         Message1(sym_e_unknown_id,pattern);
                         consume(_ID);
                       end
                     else
                       begin
                          consume(_ID);
                          while (token=_POINT) and
                                ((sym^.typ=varsym) and
                                 (pvarsym(sym)^.vartype.def^.deftype=recorddef)) do
                           begin
                             p^.readaccess^.addsym(sym);
                             consume(_POINT);
                             getsymonlyin(precorddef(pvarsym(sym)^.vartype.def)^.symtable,pattern);
                             if not assigned(srsym) then
                               Message1(sym_e_illegal_field,pattern);
                             sym:=srsym;
                             consume(_ID);
                           end;
                       end;

                     if assigned(sym) then
                       begin
                          { search the matching definition }
                          case sym^.typ of
                            procsym :
                              begin
                                 pp:=get_procdef;
                                 if not(assigned(pp)) or
                                    not(is_equal(pp^.rettype.def,p^.proptype.def)) then
                                   Message(parser_e_ill_property_access_sym);
                                 p^.readaccess^.setdef(pp);
                              end;
                            varsym :
                              begin
                                if not(propertyparas^.empty) or
                                   not(is_equal(pvarsym(sym)^.vartype.def,p^.proptype.def)) then
                                  Message(parser_e_ill_property_access_sym);
                              end;
                            else
                              Message(parser_e_ill_property_access_sym);
                          end;
                          p^.readaccess^.addsym(sym);
                       end;
                  end;
                if (idtoken=_WRITE) then
                  begin
                     p^.writeaccess^.clear;
                     consume(_WRITE);
                     sym:=search_class_member(aktclass,pattern);
                     if not(assigned(sym)) then
                       begin
                         Message1(sym_e_unknown_id,pattern);
                         consume(_ID);
                       end
                     else
                       begin
                          consume(_ID);
                          while (token=_POINT) and
                                ((sym^.typ=varsym) and
                                 (pvarsym(sym)^.vartype.def^.deftype=recorddef)) do
                           begin
                             p^.writeaccess^.addsym(sym);
                             consume(_POINT);
                             getsymonlyin(precorddef(pvarsym(sym)^.vartype.def)^.symtable,pattern);
                             if not assigned(srsym) then
                               Message1(sym_e_illegal_field,pattern);
                             sym:=srsym;
                             consume(_ID);
                           end;
                       end;

                     if assigned(sym) then
                       begin
                          { search the matching definition }
                          case sym^.typ of
                            procsym :
                              begin
                                 { insert data entry to check access method }
                                 propertyparas^.insert(datacoll);
                                 pp:=get_procdef;
                                 { ... and remove it }
                                 propertyparas^.remove(datacoll);
                                 if not(assigned(pp)) then
                                   Message(parser_e_ill_property_access_sym);
                                 p^.writeaccess^.setdef(pp);
                              end;
                            varsym :
                              begin
                                 if not(propertyparas^.empty) or
                                    not(is_equal(pvarsym(sym)^.vartype.def,p^.proptype.def)) then
                                   Message(parser_e_ill_property_access_sym);
                              end
                            else
                              Message(parser_e_ill_property_access_sym);
                          end;
                          p^.writeaccess^.addsym(sym);
                       end;
                  end;
                include(p^.propoptions,ppo_stored);
                if (idtoken=_STORED) then
                  begin
                     consume(_STORED);
                     p^.storedaccess^.clear;
                     case token of
                        _ID:
                           { in the case that idtoken=_DEFAULT }
                           { we have to do nothing except      }
                           { setting ppo_stored, it's the same }
                           { as stored true                    }
                           if idtoken<>_DEFAULT then
                             begin
                                sym:=search_class_member(aktclass,pattern);
                                if not(assigned(sym)) then
                                  begin
                                    Message1(sym_e_unknown_id,pattern);
                                    consume(_ID);
                                  end
                                else
                                  begin
                                     consume(_ID);
                                     while (token=_POINT) and
                                           ((sym^.typ=varsym) and
                                            (pvarsym(sym)^.vartype.def^.deftype=recorddef)) do
                                      begin
                                        p^.storedaccess^.addsym(sym);
                                        consume(_POINT);
                                        getsymonlyin(precorddef(pvarsym(sym)^.vartype.def)^.symtable,pattern);
                                        if not assigned(srsym) then
                                          Message1(sym_e_illegal_field,pattern);
                                        sym:=srsym;
                                        consume(_ID);
                                      end;
                                  end;

                                if assigned(sym) then
                                  begin
                                     { only non array properties can be stored }
                                     case sym^.typ of
                                       procsym :
                                         begin
                                           pp:=pprocsym(sym)^.definition;
                                           while assigned(pp) do
                                             begin
                                                { the stored function shouldn't have any parameters }
                                                if pp^.para^.empty then
                                                  break;
                                                 pp:=pp^.nextoverloaded;
                                             end;
                                           { found we a procedure and does it really return a bool? }
                                           if not(assigned(pp)) or
                                              not(is_equal(pp^.rettype.def,booldef)) then
                                             Message(parser_e_ill_property_storage_sym);
                                           p^.storedaccess^.setdef(pp);
                                         end;
                                       varsym :
                                         begin
                                           if not(propertyparas^.empty) or
                                              not(is_equal(pvarsym(sym)^.vartype.def,booldef)) then
                                             Message(parser_e_stored_property_must_be_boolean);
                                         end;
                                       else
                                         Message(parser_e_ill_property_storage_sym);
                                     end;
                                     p^.storedaccess^.addsym(sym);
                                  end;
                             end;
                        _FALSE:
                          begin
                             consume(_FALSE);
                             exclude(p^.propoptions,ppo_stored);
                          end;
                        _TRUE:
                          consume(_TRUE);
                     end;
                  end;
                if (idtoken=_DEFAULT) then
                  begin
                     consume(_DEFAULT);
                     if not(is_ordinal(p^.proptype.def) or
                            is_64bitint(p^.proptype.def) or
                            ((p^.proptype.def^.deftype=setdef) and
                             (psetdef(p^.proptype.def)^.settype=smallset)) or
                            not(propertyparas^.empty)
                        ) then
                       Message(parser_e_property_cant_have_a_default_value);
                     { Get the result of the default, the firstpass is
                       needed to support values like -1 }
                     pt:=comp_expr(true);
                     do_firstpass(pt);
                     if p^.proptype.def^.deftype=setdef then
                       begin
{$ifndef newcg}
                         {!!!!!!!!!!}
                         arrayconstructor_to_set(pt);
{$endif newcg}
                         do_firstpass(pt);
                       end;
                     pt:=gentypeconvnode(pt,p^.proptype.def);
                     do_firstpass(pt);
                     if not(is_constnode(pt)) then
                       Message(parser_e_property_default_value_must_const);

                     if pt^.treetype=setconstn then
                       p^.default:=plongint(pt^.value_set)^
                     else
                       p^.default:=pt^.value;
                     disposetree(pt);
                  end
                else if (idtoken=_NODEFAULT) then
                  begin
                     consume(_NODEFAULT);
                     p^.default:=0;
                  end;
                symtablestack^.insert(p);
                { default property ? }
                consume(_SEMICOLON);
                if (idtoken=_DEFAULT) then
                  begin
                     consume(_DEFAULT);
                     { overriding a default propertyp is allowed
                     p2:=search_default_property(aktclass);
                     if assigned(p2) then
                       message1(parser_e_only_one_default_property,
                         pobjectdef(p2^.owner^.defowner)^.objname^)
                     else
                     }
                       begin
                          include(p^.propoptions,ppo_defaultproperty);
                          if propertyparas^.empty then
                            message(parser_e_property_need_paras);
                       end;
                     consume(_SEMICOLON);
                  end;
                { clean up }
                if assigned(datacoll) then
                  dispose(datacoll,done);
             end
           else
             begin
                consume(_ID);
                consume(_SEMICOLON);
             end;
           dispose(propertyparas,done);
        end;


      procedure destructor_head;
        begin
           consume(_DESTRUCTOR);
           inc(lexlevel);
           parse_proc_head(potype_destructor);
           dec(lexlevel);
           if (cs_constructor_name in aktglobalswitches) and (aktprocsym^.name<>'DONE') then
            Message(parser_e_destructorname_must_be_done);
{$ifdef INCLUDEOK}
           include(aktclass^.objectoptions,oo_has_destructor);
{$else}
           aktclass^.objectoptions:=aktclass^.objectoptions+[oo_has_destructor];
{$endif}
           consume(_SEMICOLON);
           if not(aktprocsym^.definition^.para^.empty) then
            Message(parser_e_no_paras_for_destructor);
           { no return value }
           aktprocsym^.definition^.rettype.def:=voiddef;
        end;

      var
         hs      : string;
         pcrd       : pclassrefdef;
         tt     : ttype;
         oldprocinfo : pprocinfo;
         oldprocsym : pprocsym;
         oldparse_only : boolean;
         methodnametable,intmessagetable,
         strmessagetable,classnamelabel : pasmlabel;
         storetypecanbeforward : boolean;
         vmtlist : taasmoutput;
{$ifdef WITHDMT}
         dmtlabel : pasmlabel;
{$endif WITHDMT}

      begin
         {Nowadays aktprocsym may already have a value, so we need to save
          it.}
         oldprocsym:=aktprocsym;
         { forward is resolved }
         if assigned(fd) then
           exclude(fd^.objectoptions,oo_is_forward);

         there_is_a_destructor:=false;
         actmembertype:=[sp_public];

         { objects and class types can't be declared local }
         if (symtablestack^.symtabletype<>globalsymtable) and
           (symtablestack^.symtabletype<>staticsymtable) then
           Message(parser_e_no_local_objects);

         storetypecanbeforward:=typecanbeforward;
         { for tp mode don't allow forward types }
         if m_tp in aktmodeswitches then
           typecanbeforward:=false;

         { distinguish classes and objects }
         if token=_OBJECT then
           begin
              is_a_class:=false;
              consume(_OBJECT)
           end
         else
           begin
              is_a_class:=true;
              consume(_CLASS);
              if not(assigned(fd)) and (token=_OF) then
                begin
                   { a hack, but it's easy to handle }
                   { class reference type }
                   consume(_OF);
                   single_type(tt,hs,typecanbeforward);

                   { accept hp1, if is a forward def or a class }
                   if (tt.def^.deftype=forwarddef) or
                      ((tt.def^.deftype=objectdef) and pobjectdef(tt.def)^.is_class) then
                     begin
                        pcrd:=new(pclassrefdef,init(tt.def));
                        object_dec:=pcrd;
                     end
                   else
                     begin
                        object_dec:=generrordef;
                        Message1(type_e_class_type_expected,generrordef^.typename);
                     end;
                   typecanbeforward:=storetypecanbeforward;
                   exit;
                end
              { forward class }
              else if not(assigned(fd)) and (token=_SEMICOLON) then
                begin
                   { also anonym objects aren't allow (o : object a : longint; end;) }
                   if n='' then
                    begin
                       Message(parser_f_no_anonym_objects)
                    end;
                   if n='TOBJECT' then
                     begin
                        aktclass:=new(pobjectdef,init(n,nil));
                        class_tobject:=aktclass;
                     end
                   else
                     aktclass:=new(pobjectdef,init(n,nil));
                   aktclass^.objectoptions:=aktclass^.objectoptions+[oo_is_class,oo_is_forward];
                   { all classes must have a vmt !!  at offset zero }
                   if not(oo_has_vmt in aktclass^.objectoptions) then
                     aktclass^.insertvmt;

                   object_dec:=aktclass;
                   typecanbeforward:=storetypecanbeforward;
                   exit;
                end;
           end;

         { also anonym objects aren't allow (o : object a : longint; end;) }
         if n='' then
           Message(parser_f_no_anonym_objects);

         { read the parent class }
         if token=_LKLAMMER then
           begin
              consume(_LKLAMMER);
              id_type(tt,pattern,false);
              childof:=pobjectdef(tt.def);
              if (childof^.deftype<>objectdef) then
               begin
                 Message1(type_e_class_type_expected,childof^.typename);
                 childof:=nil;
                 aktclass:=new(pobjectdef,init(n,nil));
               end
              else
               begin
                 { a mix of class and object isn't allowed }
                 if (childof^.is_class and not is_a_class) or
                    (not childof^.is_class and is_a_class) then
                  Message(parser_e_mix_of_classes_and_objects);
                 { the forward of the child must be resolved to get
                   correct field addresses }
                 if assigned(fd) then
                  begin
                    if (oo_is_forward in childof^.objectoptions) then
                     Message1(parser_e_forward_declaration_must_be_resolved,childof^.objname^);
                    aktclass:=fd;
                    { we must inherit several options !!
                      this was missing !!
                      all is now done in set_parent
                      including symtable datasize setting PM }
                    fd^.set_parent(childof);
                  end
                 else
                  aktclass:=new(pobjectdef,init(n,childof));
               end;
              consume(_RKLAMMER);
           end
         { if no parent class, then a class get tobject as parent }
         else if is_a_class then
           begin
              { is the current class tobject?   }
              { so you could define your own tobject }
              if n='TOBJECT' then
                begin
                   if assigned(fd) then
                     aktclass:=fd
                   else
                     aktclass:=new(pobjectdef,init(n,nil));
                   class_tobject:=aktclass;
                end
              else
                begin
                   childof:=class_tobject;
                   if assigned(fd) then
                     begin
                        { the forward of the child must be resolved to get
                          correct field addresses
                        }
                        if (oo_is_forward in childof^.objectoptions) then
                          Message1(parser_e_forward_declaration_must_be_resolved,childof^.objname^);
                        aktclass:=fd;
                        aktclass^.set_parent(childof);
                     end
                   else
                     begin
                        aktclass:=new(pobjectdef,init(n,childof));
                        aktclass^.set_parent(childof);
                     end;
                end;
           end
         else
           aktclass:=new(pobjectdef,init(n,nil));

         { default access is public }
         actmembertype:=[sp_public];

         { set the class attribute }
         if is_a_class then
           begin
{$ifdef INCLUDEOK}
              include(aktclass^.objectoptions,oo_is_class);
{$else}
              aktclass^.objectoptions:=aktclass^.objectoptions+[oo_is_class];
{$endif}
              if (cs_generate_rtti in aktlocalswitches) or
                  (assigned(aktclass^.childof) and
                   (oo_can_have_published in aktclass^.childof^.objectoptions)) then
                begin
                   include(aktclass^.objectoptions,oo_can_have_published);
                   { in "publishable" classes the default access type is published }
                   actmembertype:=[sp_published];
                   { don't know if this is necessary (FK) }
                   current_object_option:=[sp_published];
                end;
           end;

         aktobjectdef:=aktclass;
         aktclass^.symtable^.next:=symtablestack;
         symtablestack:=aktclass^.symtable;
         testcurobject:=1;
         curobjectname:=n;

         { new procinfo }
         oldprocinfo:=procinfo;
         new(procinfo,init);
         procinfo^._class:=aktclass;


       { short class declaration ? }
         if (not is_a_class) or (token<>_SEMICOLON) then
          begin
          { Parse componenten }
            repeat
              if (sp_private in actmembertype) then
{$ifdef INCLUDEOK}
                include(aktclass^.objectoptions,oo_has_private);
{$else}
                aktclass^.objectoptions:=aktclass^.objectoptions+[oo_has_private];
{$endif}
              if (sp_protected in actmembertype) then
{$ifdef INCLUDEOK}
                include(aktclass^.objectoptions,oo_has_protected);
{$else}
                aktclass^.objectoptions:=aktclass^.objectoptions+[oo_has_protected];
{$endif}
              case token of
              _ID : begin
                      case idtoken of
                       _PRIVATE : begin
                                    consume(_PRIVATE);
                                    actmembertype:=[sp_private];
                                    current_object_option:=[sp_private];
                                  end;
                     _PROTECTED : begin
                                    consume(_PROTECTED);
                                    current_object_option:=[sp_protected];
                                    actmembertype:=[sp_protected];
                                  end;
                        _PUBLIC : begin
                                    consume(_PUBLIC);
                                    current_object_option:=[sp_public];
                                    actmembertype:=[sp_public];
                                  end;
                     _PUBLISHED : begin
                                    if not(oo_can_have_published in aktclass^.objectoptions) then
                                     Message(parser_e_cant_have_published);
                                    consume(_PUBLISHED);
                                    current_object_option:=[sp_published];
                                    actmembertype:=[sp_published];
                                  end;
                      else
                        read_var_decs(false,true,false);
                      end;
                    end;
        _PROPERTY : property_dec;
       _PROCEDURE,
        _FUNCTION,
           _CLASS : begin
                      oldparse_only:=parse_only;
                      parse_only:=true;
                      parse_proc_dec;
{$ifndef newcg}
                      parse_object_proc_directives(aktprocsym);
{$endif newcg}
                      if (po_msgint in aktprocsym^.definition^.procoptions) then
{$ifdef INCLUDEOK}
                        include(aktclass^.objectoptions,oo_has_msgint);
{$else}
                        aktclass^.objectoptions:=aktclass^.objectoptions+[oo_has_msgint];
{$endif}
                      if (po_msgstr in aktprocsym^.definition^.procoptions) then
{$ifdef INCLUDEOK}
                        include(aktclass^.objectoptions,oo_has_msgstr);
{$else}
                        aktclass^.objectoptions:=aktclass^.objectoptions+[oo_has_msgstr];
{$endif}
                      if (po_virtualmethod in aktprocsym^.definition^.procoptions) then
{$ifdef INCLUDEOK}
                        include(aktclass^.objectoptions,oo_has_virtual);
{$else}
                        aktclass^.objectoptions:=aktclass^.objectoptions+[oo_has_virtual];
{$endif}
                      parse_only:=oldparse_only;
                    end;
     _CONSTRUCTOR : begin
                      if not(sp_public in actmembertype) then
                        Message(parser_w_constructor_should_be_public);
                      oldparse_only:=parse_only;
                      parse_only:=true;
                      constructor_head;
{$ifndef newcg}
                      parse_object_proc_directives(aktprocsym);
{$endif newcg}
                      if (po_virtualmethod in aktprocsym^.definition^.procoptions) then
{$ifdef INCLUDEOK}
                        include(aktclass^.objectoptions,oo_has_virtual);
{$else}
                        aktclass^.objectoptions:=aktclass^.objectoptions+[oo_has_virtual];
{$endif}
                      parse_only:=oldparse_only;
                    end;
      _DESTRUCTOR : begin
                      if there_is_a_destructor then
                        Message(parser_n_only_one_destructor);
                      there_is_a_destructor:=true;
                      if not(sp_public in actmembertype) then
                        Message(parser_w_destructor_should_be_public);
                      oldparse_only:=parse_only;
                      parse_only:=true;
                      destructor_head;
{$ifndef newcg}
                      parse_object_proc_directives(aktprocsym);
{$endif newcg}
                      if (po_virtualmethod in aktprocsym^.definition^.procoptions) then
{$ifdef INCLUDEOK}
                        include(aktclass^.objectoptions,oo_has_virtual);
{$else}
                        aktclass^.objectoptions:=aktclass^.objectoptions+[oo_has_virtual];
{$endif}
                      parse_only:=oldparse_only;
                    end;
             _END : begin
                      consume(_END);
                      break;
                    end;
              else
               consume(_ID); { Give a ident expected message, like tp7 }
              end;
            until false;
            current_object_option:=[sp_public];
          end;
         testcurobject:=0;
         curobjectname:='';
         typecanbeforward:=storetypecanbeforward;

         { generate vmt space if needed }
         if not(oo_has_vmt in aktclass^.objectoptions) and
            ([oo_has_virtual,oo_has_constructor,oo_has_destructor,oo_is_class]*aktclass^.objectoptions<>[]) then
           aktclass^.insertvmt;
         if (cs_create_smart in aktmoduleswitches) then
           datasegment^.concat(new(pai_cut,init));

         { Write the start of the VMT, wich is equal for classes and objects }
         if (oo_has_vmt in aktclass^.objectoptions) then
           begin
{$ifdef WITHDMT}
              dmtlabel:=gendmt(aktclass);
{$endif WITHDMT}
              { this generates the entries }
              vmtlist.init;
              genvmt(@vmtlist,aktclass);

              { write tables for classes, this must be done before the actual
                class is written, because we need the labels defined }
              if is_a_class then
               begin
                 methodnametable:=genpublishedmethodstable(aktclass);
                 { rtti }
                 if (oo_can_have_published in aktclass^.objectoptions) then
                  aktclass^.generate_rtti;
                 { write class name }
                 getdatalabel(classnamelabel);
                 datasegment^.concat(new(pai_label,init(classnamelabel)));
                 datasegment^.concat(new(pai_const,init_8bit(length(aktclass^.objname^))));
                 datasegment^.concat(new(pai_string,init(aktclass^.objname^)));
                 { generate message and dynamic tables }
                 if (oo_has_msgstr in aktclass^.objectoptions) then
                   strmessagetable:=genstrmsgtab(aktclass);
                 if (oo_has_msgint in aktclass^.objectoptions) then
                   intmessagetable:=genintmsgtab(aktclass)
                 else
                   datasegment^.concat(new(pai_const,init_32bit(0)));
               end;

             { write debug info }
{$ifdef GDB}
             if (cs_debuginfo in aktmoduleswitches) then
              begin
                do_count_dbx:=true;
                if assigned(aktclass^.owner) and assigned(aktclass^.owner^.name) then
                  datasegment^.concat(new(pai_stabs,init(strpnew('"vmt_'+aktclass^.owner^.name^+n+':S'+
                    typeglobalnumber('__vtbl_ptr_type')+'",'+tostr(N_STSYM)+',0,0,'+aktclass^.vmt_mangledname))));
              end;
{$endif GDB}
              datasegment^.concat(new(pai_symbol,initname_global(aktclass^.vmt_mangledname,0)));

              { determine the size with symtable^.datasize, because }
              { size gives back 4 for classes                    }
              datasegment^.concat(new(pai_const,init_32bit(aktclass^.symtable^.datasize)));
              datasegment^.concat(new(pai_const,init_32bit(-aktclass^.symtable^.datasize)));
{$ifdef WITHDMT}
              if not(is_a_class) then
                begin
                   if assigned(dmtlabel) then
                     datasegment^.concat(new(pai_const_symbol,init(dmtlabel)))
                   else
                     datasegment^.concat(new(pai_const,init_32bit(0)));
                end;
{$endif WITHDMT}
              { write pointer to parent VMT, this isn't implemented in TP }
              { but this is not used in FPC ? (PM) }
              { it's not used yet, but the delphi-operators as and is need it (FK) }
              { it is not written for parents that don't have any vmt !! }
              if assigned(aktclass^.childof) and
                 (oo_has_vmt in aktclass^.childof^.objectoptions) then
                datasegment^.concat(new(pai_const_symbol,initname(aktclass^.childof^.vmt_mangledname)))
              else
                datasegment^.concat(new(pai_const,init_32bit(0)));

              { write extended info for classes, for the order see rtl/inc/objpash.inc }
              if is_a_class then
               begin
                 { pointer to class name string }
                 datasegment^.concat(new(pai_const_symbol,init(classnamelabel)));
                 { pointer to dynamic table }
                 if (oo_has_msgint in aktclass^.objectoptions) then
                   datasegment^.concat(new(pai_const_symbol,init(intmessagetable)))
                 else
                   datasegment^.concat(new(pai_const,init_32bit(0)));
                 { pointer to method table }
                 if assigned(methodnametable) then
                   datasegment^.concat(new(pai_const_symbol,init(methodnametable)))
                 else
                   datasegment^.concat(new(pai_const,init_32bit(0)));
                 { pointer to field table }
                 datasegment^.concat(new(pai_const,init_32bit(0)));
                 { pointer to type info of published section }
                 if (oo_can_have_published in aktclass^.objectoptions) then
                   datasegment^.concat(new(pai_const_symbol,initname(aktclass^.rtti_name)))
                 else
                   datasegment^.concat(new(pai_const,init_32bit(0)));
                 { inittable for con-/destruction }
                 {
                 if aktclass^.needs_inittable then
                 }
                 { we generate the init table for classes always, because needs_inittable }
                 { for classes is always false, it applies only for objects               }
                 datasegment^.concat(new(pai_const_symbol,init(aktclass^.get_inittable_label)));
                 {
                 else
                   datasegment^.concat(new(pai_const,init_32bit(0)));
                 }
                 { auto table }
                 datasegment^.concat(new(pai_const,init_32bit(0)));
                 { interface table }
                 datasegment^.concat(new(pai_const,init_32bit(0)));
                 { table for string messages }
                 if (oo_has_msgstr in aktclass^.objectoptions) then
                   datasegment^.concat(new(pai_const_symbol,init(strmessagetable)))
                 else
                   datasegment^.concat(new(pai_const,init_32bit(0)));
               end;
              datasegment^.concatlist(@vmtlist);
              vmtlist.done;
              { write the size of the VMT }
              datasegment^.concat(new(pai_symbol_end,initname(aktclass^.vmt_mangledname)));
           end;

         { restore old state }
         symtablestack:=symtablestack^.next;
         aktobjectdef:=nil;
         {Restore procinfo}
         dispose(procinfo,done);
         procinfo:=oldprocinfo;
         {Restore the aktprocsym.}
         aktprocsym:=oldprocsym;

         object_dec:=aktclass;
      end;


    { reads a record declaration }
    function record_dec : pdef;

      var
         symtable : psymtable;
         storetypecanbeforward : boolean;

      begin
         { create recdef }
         symtable:=new(psymtable,init(recordsymtable));
         record_dec:=new(precorddef,init(symtable));
         { update symtable stack }
         symtable^.next:=symtablestack;
         symtablestack:=symtable;
         { parse record }
         consume(_RECORD);
         storetypecanbeforward:=typecanbeforward;
         { for tp mode don't allow forward types }
         if m_tp in aktmodeswitches then
           typecanbeforward:=false;
         read_var_decs(true,false,false);
         consume(_END);
         typecanbeforward:=storetypecanbeforward;
         { may be scale record size to a size of n*4 ? }
         symtablestack^.datasize:=align(symtablestack^.datasize,symtablestack^.dataalignment);
         { restore symtable stack }
         symtablestack:=symtable^.next;
      end;


    { reads a type definition and returns a pointer to it }
    procedure read_type(var tt : ttype;const name : stringid);
      var
        pt : ptree;
        tt2 : ttype;
        aufdef : penumdef;
        {aufsym : penumsym;}
        ap : parraydef;
        s : stringid;
        l,v : longint;
        oldaktpackrecords : tpackrecords;
        hs : string;
        defpos,storepos : tfileposinfo;

        procedure expr_type;
        var
           pt1,pt2 : ptree;
        begin
           { use of current parsed object ? }
           if (token=_ID) and (testcurobject=2) and (curobjectname=pattern) then
             begin
                consume(_ID);
                tt.setdef(aktobjectdef);
                exit;
             end;
           { classes can be used also in classes }
           if (curobjectname=pattern) and aktobjectdef^.is_class then
             begin
                tt.setdef(aktobjectdef);
                consume(_ID);
                exit;
             end;
           { we can't accept a equal in type }
           pt1:=comp_expr(not(ignore_equal));
           do_firstpass(pt1);
           if (token=_POINTPOINT) then
             begin
               consume(_POINTPOINT);
               { get high value of range }
               pt2:=comp_expr(not(ignore_equal));
               do_firstpass(pt2);
               { both must be evaluated to constants now }
               if (pt1^.treetype<>ordconstn) or (pt2^.treetype<>ordconstn) then
                 Message(sym_e_error_in_type_def)
               else
                 begin
                 { check types }
                   if CheckTypes(pt1^.resulttype,pt2^.resulttype) then
                     begin
                     { Check bounds }
                       if pt2^.value<pt1^.value then
                         Message(cg_e_upper_lower_than_lower)
                       else
                        begin
                        { All checks passed, create the new def }
                          case pt1^.resulttype^.deftype of
                           enumdef : tt.setdef(new(penumdef,init_subrange(penumdef(pt1^.resulttype),pt1^.value,pt2^.value)));
                            orddef : begin
                                       if is_char(pt1^.resulttype) then
                                         tt.setdef(new(porddef,init(uchar,pt1^.value,pt2^.value)))
                                       else
                                        if is_boolean(pt1^.resulttype) then
                                         tt.setdef(new(porddef,init(bool8bit,pt1^.value,pt2^.value)))
                                       else
                                        tt.setdef(new(porddef,init(uauto,pt1^.value,pt2^.value)));
                                     end;
                          end;
                        end;
                     end;
                 end;
               disposetree(pt2);
             end
           else
             begin
               { a simple type renaming }
               if (pt1^.treetype=typen) then
                 begin
                   if assigned(pt1^.typenodesym) then
                     tt.setsym(pt1^.typenodesym)
                   else
                     tt.setdef(pt1^.resulttype);
                 end
               else
                 Message(sym_e_error_in_type_def);
             end;
           disposetree(pt1);
        end;

        procedure array_dec;
        var
          lowval,
          highval   : longint;
          arraytype : pdef;
        begin
           consume(_ARRAY);
           consume(_LECKKLAMMER);
           { defaults }
           arraytype:=generrordef;
           lowval:=$80000000;
           highval:=$7fffffff;
           tt.reset;
           repeat
             { read the expression and check it }
             pt:=expr;
             if pt^.treetype=typen then
               begin
                 case pt^.resulttype^.deftype of
                   enumdef :
                     begin
                       lowval:=penumdef(pt^.resulttype)^.min;
                       highval:=penumdef(pt^.resulttype)^.max;
                       arraytype:=pt^.resulttype;
                     end;
                   orddef :
                     begin
                       lowval:=porddef(pt^.resulttype)^.low;
                       highval:=porddef(pt^.resulttype)^.high;
                       arraytype:=pt^.resulttype;
                     end;
                   else
                     Message(sym_e_error_in_type_def);
                 end;
               end
             else
               begin
                  do_firstpass(pt);
                  if (pt^.treetype=rangen) then
                   begin
                     if (pt^.left^.treetype=ordconstn) and
                        (pt^.right^.treetype=ordconstn) then
                      begin
                        lowval:=pt^.left^.value;
                        highval:=pt^.right^.value;
                        if highval<lowval then
                         begin
                           Message(parser_e_array_lower_less_than_upper_bound);
                           highval:=lowval;
                         end;
                        arraytype:=pt^.right^.resulttype;
                      end
                     else
                      Message(type_e_cant_eval_constant_expr);
                   end
                  else
                   Message(sym_e_error_in_type_def)
               end;
             disposetree(pt);

           { create arraydef }
             if not assigned(tt.def) then
              begin
                ap:=new(parraydef,init(lowval,highval,arraytype));
                tt.setdef(ap);
              end
             else
              begin
                ap^.elementtype.setdef(new(parraydef,init(lowval,highval,arraytype)));
                ap:=parraydef(ap^.elementtype.def);
              end;

             if token=_COMMA then
               consume(_COMMA)
             else
               break;
           until false;
           consume(_RECKKLAMMER);
           consume(_OF);
           read_type(tt2,'');
           { if no error, set element type }
           if assigned(ap) then
             ap^.elementtype:=tt2;
        end;

      begin
         tt.reset;
         case token of
            _STRING,_FILE:
              begin
                single_type(tt,hs,false);
              end;
           _LKLAMMER:
              begin
                consume(_LKLAMMER);
                { allow negativ value_str }
                l:=-1;
                {aufsym := Nil;}
                aufdef:=new(penumdef,init);
                repeat
                  s:=pattern;
                  defpos:=tokenpos;
                  consume(_ID);
                  if token=_ASSIGNMENT then
                    begin
                       consume(_ASSIGNMENT);
                       v:=get_intconst;
                       { please leave that a note, allows type save }
                       { declarations in the win32 units !       }
                       if v<=l then
                        Message(parser_n_duplicate_enum);
                       l:=v;
                    end
                  else
                    inc(l);
                  storepos:=tokenpos;
                  tokenpos:=defpos;
                  constsymtable^.insert(new(penumsym,init(s,aufdef,l)));
                  tokenpos:=storepos;
                  if token=_COMMA then
                    consume(_COMMA)
                  else
                    break;
                until false;
                {aufdef^.max:=l;
                if we allow unordered enums
                this can be wrong
                min and max are now set in tenumsym.init PM }
                tt.setdef(aufdef);
                consume(_RKLAMMER);
              end;
            _ARRAY:
              begin
                array_dec;
              end;
            _SET:
              begin
                consume(_SET);
                consume(_OF);
                read_type(tt2,'');
                if assigned(tt2.def) then
                 begin
                   case tt2.def^.deftype of
                     { don't forget that min can be negativ  PM }
                     enumdef :
                       if penumdef(tt2.def)^.min>=0 then
                        tt.setdef(new(psetdef,init(tt2.def,penumdef(tt2.def)^.max)))
                       else
                        Message(sym_e_ill_type_decl_set);
                     orddef :
                       begin
                         case porddef(tt2.def)^.typ of
                           uchar :
                             tt.setdef(new(psetdef,init(tt2.def,255)));
                           u8bit,u16bit,u32bit,
                           s8bit,s16bit,s32bit :
                             begin
                               if (porddef(tt2.def)^.low>=0) then
                                tt.setdef(new(psetdef,init(tt2.def,porddef(tt2.def)^.high)))
                               else
                                Message(sym_e_ill_type_decl_set);
                             end;
                           else
                             Message(sym_e_ill_type_decl_set);
                         end;
                       end;
                     else
                       Message(sym_e_ill_type_decl_set);
                   end;
                 end
                else
                 tt.setdef(generrordef);
              end;
           _CARET:
              begin
                consume(_CARET);
                single_type(tt2,hs,typecanbeforward);
                tt.setdef(new(ppointerdef,init(tt2)));
              end;
            _RECORD:
              begin
                tt.setdef(record_dec);
              end;
            _PACKED:
              begin
                consume(_PACKED);
                if token=_ARRAY then
                  array_dec
                else
                  begin
                    oldaktpackrecords:=aktpackrecords;
                    aktpackrecords:=packrecord_1;
                    if token in [_CLASS,_OBJECT] then
                      tt.setdef(object_dec(name,nil))
                    else
                      tt.setdef(record_dec);
                    aktpackrecords:=oldaktpackrecords;
                  end;
              end;
            _CLASS,
            _OBJECT:
              begin
                tt.setdef(object_dec(name,nil));
              end;
            _PROCEDURE:
              begin
                consume(_PROCEDURE);
                tt.setdef(new(pprocvardef,init));
                if token=_LKLAMMER then
                 parameter_dec(pprocvardef(tt.def));
                if token=_OF then
                  begin
                    consume(_OF);
                    consume(_OBJECT);
{$ifdef INCLUDEOK}
                    include(pprocvardef(tt.def)^.procoptions,po_methodpointer);
{$else}
                    pprocvardef(tt.def)^.procoptions:=pprocvardef(tt.def)^.procoptions+[po_methodpointer];
{$endif}
                  end;
              end;
            _FUNCTION:
              begin
                consume(_FUNCTION);
                tt.def:=new(pprocvardef,init);
                if token=_LKLAMMER then
                 parameter_dec(pprocvardef(tt.def));
                consume(_COLON);
                single_type(pprocvardef(tt.def)^.rettype,hs,false);
                if token=_OF then
                  begin
                    consume(_OF);
                    consume(_OBJECT);
{$ifdef INCLUDEOK}
                    include(pprocvardef(tt.def)^.procoptions,po_methodpointer);
{$else}
                    pprocvardef(tt.def)^.procoptions:=pprocvardef(tt.def)^.procoptions+[po_methodpointer];
{$endif}
                  end;
              end;
            else
              expr_type;
         end;
         if tt.def=nil then
          tt.setdef(generrordef);
      end;

end.
{
  $Log$
  Revision 1.19  2000-02-21 22:17:49  florian
    * fixed 819

  Revision 1.18  2000/02/09 13:23:01  peter
    * log truncated

  Revision 1.17  2000/02/05 14:33:32  florian
    * fixed init table generation for classes and arrays

  Revision 1.16  2000/01/28 23:17:53  florian
    * virtual XXXX; support for objects, only if -dWITHDMT is defined

  Revision 1.15  2000/01/27 16:31:40  florian
    * bug 738 fixed

  Revision 1.14  2000/01/11 17:16:06  jonas
    * removed a lot of memory leaks when an error is encountered (caused by
      procinfo and pstringcontainers). There are still plenty left though :)

  Revision 1.13  2000/01/07 01:14:34  peter
    * updated copyright to 2000

  Revision 1.12  1999/11/30 10:40:52  peter
    + ttype, tsymlist

  Revision 1.11  1999/11/26 00:19:12  peter
    * property overriding dereference fix, but it need a bigger redesign
      which i'll do tomorrow. This quick hack is for the lazarus ppl so
      they can hack on mwcustomedit.

  Revision 1.10  1999/11/17 17:05:03  pierre
   * Notes/hints changes

  Revision 1.9  1999/11/11 00:56:54  pierre
   * Enum element reference location corrected

  Revision 1.8  1999/11/09 23:43:09  pierre
   * better browser info

  Revision 1.7  1999/11/08 14:02:16  florian
    * problem with "index X"-properties solved
    * typed constants of class references are now allowed

  Revision 1.6  1999/11/07 23:16:49  florian
    * finally bug 517 solved ...

  Revision 1.5  1999/10/27 16:04:06  peter
    * fixed property reading

  Revision 1.4  1999/10/27 14:17:08  florian
    * property overriding fixed

  Revision 1.3  1999/10/26 12:30:45  peter
    * const parameter is now checked
    * better and generic check if a node can be used for assigning
    * export fixes
    * procvar equal works now (it never had worked at least from 0.99.8)
    * defcoll changed to linkedlist with pparaitem so it can easily be
      walked both directions

  Revision 1.2  1999/10/22 14:37:30  peter
    * error when properties are passed to var parameters

  Revision 1.1  1999/10/22 10:39:35  peter
    * split type reading from pdecl to ptype unit
    * parameter_dec routine is now used for procedure and procvars

}