{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

    Does declaration parsing for Free Pascal

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
unit pdecl;

  interface

    uses
      globals,symtable;

    var
       { pointer to the last read type symbol, (for "forward" }
       { types)                                               }
       lasttypesym : ptypesym;

       { hack, which allows to use the current parsed }
       { object type as function argument type        }
       testcurobject : byte;
       curobjectname : stringid;

    { reads a string type with optional length }
    { and returns a pointer to the string      }
    { definition                               }
    function stringtype : pdef;

    { reads a string, file type or a type id and returns a name and }
    { pdef                                                          }
    function single_type(var s : string) : pdef;

    { reads the declaration blocks }
    procedure read_declarations(islibrary : boolean);

    { reads declarations in the interface part of a unit }
    procedure read_interface_declarations;

  implementation

    uses
       cobjects,scanner,aasm,tree,pass_1,
       files,types,hcodegen,verbose,systems
{$ifdef GDB}
       ,gdb
{$endif GDB}
       { parser specific stuff }
       ,pbase,ptconst,pexpr,psub,pexports
       { processor specific stuff }
{$ifdef i386}
       ,i386
{$endif}
{$ifdef m68k}
       ,m68k
{$endif}
       ;

    function read_type(const name : stringid) : pdef;forward;
    procedure read_var_decs(is_record : boolean;do_absolute : boolean);forward;

    procedure const_dec;

      var
         name : stringid;
         p : ptree;
         def : pdef;
         sym : psym;
         ps : pconstset;
         pd : pdouble;

      begin
         consume(_CONST);
         repeat
           name:=pattern;
           consume(ID);
           case token of
              EQUAL:
                begin
                   consume(EQUAL);
                   p:=comp_expr(true);
                   do_firstpass(p);
                   case p^.treetype of
                      ordconstn:
                        begin
                           if is_constintnode(p) then
                             symtablestack^.insert(new(pconstsym,init(name,constint,p^.value,nil)))
                           else if is_constcharnode(p) then
                             symtablestack^.insert(new(pconstsym,init(name,constchar,p^.value,nil)))
                           else if is_constboolnode(p) then
                             symtablestack^.insert(new(pconstsym,init(name,constbool,p^.value,nil)))
                           else if p^.resulttype^.deftype=enumdef then
                             symtablestack^.insert(new(pconstsym,init(name,constord,p^.value,p^.resulttype)))
                           else internalerror(111);
                        end;
                      stringconstn:
                        {values is disposed with p so I need a copy !}
                        symtablestack^.insert(new(pconstsym,init(name,conststring,longint(stringdup(p^.values^)),nil)));
                      realconstn : begin
                                      new(pd);
                                      pd^:=p^.valued;
                                      symtablestack^.insert(new(pconstsym,init(name,constreal,longint(pd),nil)));
                                   end;
                      setconstrn : begin
                                      new(ps);
                                      ps^:=p^.constset^;
                                      symtablestack^.insert(new(pconstsym,init(name,
                                        constseta,longint(ps),p^.resulttype)));
                                   end;
                      else Message(cg_e_illegal_expression);
                   end;
                   consume(SEMICOLON);
                end;
              COLON:
                begin
                   { this was missed, so const s : ^string = nil gives an
                     error (FK)
                   }
                   block_type:=bt_type;
                   consume(COLON);
                   ignore_equal:=true;
                   def:=read_type('');
                   block_type:=bt_general;
                   ignore_equal:=false;
                   sym:=new(ptypedconstsym,init(name,def));
                   symtablestack^.insert(sym);
                   consume(EQUAL);
                   readtypedconst(def,ptypedconstsym(sym));
                   consume(SEMICOLON);
                end;
              else consume(EQUAL);
           end;
         until token<>ID;
      end;

    procedure label_dec;

      var
         hl : plabel;

      begin
         consume(_LABEL);
         if not(cs_support_goto in aktswitches) then
           Message(sym_e_goto_and_label_not_supported);
         repeat
           if not(token in [ID,INTCONST]) then
             consume(ID)
           else
             begin
                getlabel(hl);
                symtablestack^.insert(new(plabelsym,init(pattern,hl)));
                consume(token);
             end;
           if token<>SEMICOLON then consume(COMMA);
         until not(token in [ID,INTCONST]);
         consume(SEMICOLON);
      end;

    { reads a string type with optional length }
    { and returns a pointer to the string      }
    { definition                               }
    function stringtype : pdef;

      var
         p : ptree;
         d : pdef;

      begin
         consume(_STRING);
         if token=LECKKLAMMER then
           begin
              consume(LECKKLAMMER);
              p:=comp_expr(true);
              do_firstpass(p);
              if not is_constintnode(p) then
                Message(cg_e_illegal_expression);
{$ifndef UseAnsiString}
              if (p^.value<1) or (p^.value>255) then
                begin
                   Message(parser_e_string_too_long);
                   p^.value:=255;
                end;
              consume(RECKKLAMMER);
              if p^.value<>255 then
                d:=new(pstringdef,init(p^.value))
{$ifndef GDB}
                 else d:=new(pstringdef,init(255));
{$else GDB}
                 else d:=globaldef('STRING');
{$endif GDB}
{$else UseAnsiString}
              if p^.value>255 then
                d:=new(pstringdef,ansiinit(p^.value))
              else if p^.value<>255 then
                d:=new(pstringdef,init(p^.value))
{$ifndef GDB}
                 else d:=new(pstringdef,init(255));
{$else GDB}
                 else d:=globaldef('STRING');
{$endif GDB}
              consume(RECKKLAMMER);
{$endif UseAnsiString}
              disposetree(p);
           end
           { should string without suffix be an ansistring also
             in ansistring mode ?? (PM) }
{$ifndef GDB}
                 else d:=new(pstringdef,init(255));
{$else GDB}
                 else d:=globaldef('STRING');
{$endif GDB}
                 stringtype:=d;
          end;

    { reads a type definition and returns a pointer }
    { to a appropriating pdef, s gets the name of   }
    { the type to allow name mangling               }
    function id_type(var s : string) : pdef;

      begin
         s:=pattern;
         consume(ID);
         { classes can be used also in classes }
         if (curobjectname=pattern) and aktobjectdef^.isclass then
           begin
              id_type:=aktobjectdef;
              exit;
           end;
         { objects can be parameters }
         if (testcurobject=2) and (curobjectname=pattern) then
           begin
              id_type:=aktobjectdef;
              exit;
           end;
         getsym(s,true);
         if assigned(srsym) then
           begin
                  if srsym^.typ=unitsym then
                        begin
                           consume(POINT);
                           getsymonlyin(punitsym(srsym)^.unitsymtable,pattern);
                           s:=pattern;
                           consume(ID);
                        end;
                  if srsym^.typ<>typesym then
                        begin
                           Message(sym_e_type_id_expected);
                           lasttypesym:=ptypesym(srsym);
                           id_type:=generrordef;
                           exit;
                        end;
           end;
         lasttypesym:=ptypesym(srsym);
         id_type:=ptypesym(srsym)^.definition;
      end;

    { reads a string, file type or a type id and returns a name and }
    { pdef                                                          }
    function single_type(var s : string) : pdef;

       var
          hs : string;

       begin
          case token of
            _STRING:
                begin
                   single_type:=stringtype;
                   s:='STRING';
                   lasttypesym:=nil;
                end;
            _FILE:
                begin
                   consume(_FILE);
                   if token=_OF then
                     begin
                        consume(_OF);
                        single_type:=new(pfiledef,init(ft_typed,single_type(hs)));
                        s:='FILE$OF$'+hs;
                     end
                   else
                     begin
                        { single_type:=new(pfiledef,init(ft_untyped,nil));}
                        single_type:=cfiledef;
                        s:='FILE';
                     end;
                   lasttypesym:=nil;
                end;
            else single_type:=id_type(s);
         end;
      end;

    { this function parses an object or class declaration }
    function object_dec(const n : stringid;fd : pobjectdef) : pdef;

      var
         actmembertype : symprop;
         there_is_a_destructor : boolean;
         is_a_class : boolean;
         childof : pobjectdef;
         aktclass : pobjectdef;

      procedure constructor_head;

        begin
           consume(_CONSTRUCTOR);
           { must be at same level as in implementation }
           _proc_head(poconstructor);

           if (cs_checkconsname in aktswitches) and (aktprocsym^.name<>'INIT') then
            Message(parser_e_constructorname_must_be_init);

           consume(SEMICOLON);
             begin
                if (aktclass^.options and oois_class)<>0 then
                  begin
                     { CLASS constructors return the created instance }
                     aktprocsym^.definition^.retdef:=aktclass;
                  end
                else
                  begin
                     { OBJECT constructors return a boolean }
{$IfDef GDB}
                     {GDB doesn't like unnamed types !}
                     aktprocsym^.definition^.retdef:=
                       globaldef('boolean');
{$Else GDB}
                     aktprocsym^.definition^.retdef:=
                        new(porddef,init(bool8bit,0,1));

{$Endif GDB}
                  end;
             end;
        end;

      procedure property_dec;

        var
           sym : psym;
           propertyparas : pdefcoll;

        { returns the matching procedure to access a property }
        function get_procdef : pprocdef;

          var
             p : pprocdef;

          begin
             p:=pprocsym(sym)^.definition;
             get_procdef:=nil;
             while assigned(p) do
               begin
                  if equal_paras(p^.para1,propertyparas,true) then
                    break;
                  p:=p^.nextoverloaded;
               end;
             get_procdef:=p;
          end;

        var
           hp2,datacoll : pdefcoll;
           p,p2 : ppropertysym;
           overriden : psym;
           hs : string;
           code : word;
           varspez : tvarspez;
           sc : pstringcontainer;
           hp : pdef;
           s : string;
           filepos : tfileposinfo;
           pp : pprocdef;

        begin
           { check for a class }
           if (aktclass^.options and oois_class=0) then
            Message(parser_e_syntax_error);
           consume(_PROPERTY);
           if token=ID then
             begin
                p:=new(ppropertysym,init(pattern));
                consume(ID);
                propertyparas:=nil;
                datacoll:=nil;
                { property parameters ? }
                if token=LECKKLAMMER then
                  begin
                     { create a list of the parameters in propertyparas }
                     consume(LECKKLAMMER);
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
                       if token=COLON then
                         begin
                            consume(COLON);
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
                                 hp:=new(parraydef,init(0,-1,s32bitdef));
                                 { define field type }
                                 parraydef(hp)^.definition:=single_type(s);
                              end
                            else
                              hp:=single_type(s);
                         end
                       else
                         hp:=new(pformaldef,init);
                       s:=sc^.get_with_tokeninfo(filepos);
                       while s<>'' do
                         begin
                            new(hp2);
                            hp2^.paratyp:=varspez;
                            hp2^.data:=hp;
                            hp2^.next:=propertyparas;
                            propertyparas:=hp2;
                            s:=sc^.get_with_tokeninfo(filepos);
                         end;
                       dispose(sc,done);
                       if token=SEMICOLON then consume(SEMICOLON)
                     else break;
                     until false;
                     dec(testcurobject);
                     consume(RECKKLAMMER);
                  end;
                { overriden property ?                                       }
                { force property interface, if there is a property parameter }
                if (token=COLON) or assigned(propertyparas) then
                  begin
                     consume(COLON);
                     p^.proptype:=single_type(hs);
                     if (token=ID) and (pattern='INDEX') then
                       begin
                          consume(ID);
                          p^.options:=p^.options or ppo_indexed;
                          if token=INTCONST then
                            val(pattern,p^.index,code);
                          consume(INTCONST);
                          { concat a longint to the para template }
                          new(hp2);
                          hp2^.paratyp:=vs_value;
                          hp2^.data:=s32bitdef;
                          hp2^.next:=propertyparas;
                          propertyparas:=hp2;
                       end;
                  end
                else
                  begin
                     { do an property override }
                     overriden:=search_class_member(aktclass,pattern);
                     if assigned(overriden) and (overriden^.typ=propertysym) then
                       begin
                          { take the whole info: }
                          p^.options:=ppropertysym(overriden)^.options;
                          p^.index:=ppropertysym(overriden)^.index;
                          p^.proptype:=ppropertysym(overriden)^.proptype;
                          p^.writeaccesssym:=ppropertysym(overriden)^.writeaccesssym;
                          p^.readaccesssym:=ppropertysym(overriden)^.readaccesssym;
                          p^.writeaccessdef:=ppropertysym(overriden)^.writeaccessdef;
                          p^.readaccessdef:=ppropertysym(overriden)^.readaccessdef;
                       end
                     else
                       begin
                          p^.proptype:=generrordef;
                          message(parser_e_no_property_found_to_override);
                       end;
                  end;
                { create data defcoll to allow correct parameter checks }
                new(datacoll);
                datacoll^.paratyp:=vs_value;
                datacoll^.data:=p^.proptype;
                datacoll^.next:=nil;

                if (token=ID) and (pattern='READ') then
                  begin
                     consume(ID);
                     sym:=search_class_member(aktclass,pattern);
                     if not(assigned(sym)) then
                       Message1(sym_e_unknown_id,pattern)
                     else
                       begin
                          { varsym aren't allowed for an indexed property
                            or an property with parameters }
                          if ((sym^.typ=varsym) and
                             { not necessary, an index forces propertyparas
                               to be assigned
                             }
                             { (((p^.options and ppo_indexed)<>0) or }
                             assigned(propertyparas)) or
                             not(sym^.typ in [varsym,procsym]) then
                            Message(parser_e_ill_property_access_sym);
                          { search the matching definition }
                          if sym^.typ=procsym then
                            begin
                               pp:=get_procdef;
                               if not(assigned(pp)) or
                                 not(is_equal(pp^.retdef,p^.proptype)) then
                                 Message(parser_e_ill_property_access_sym);
                               p^.readaccessdef:=pp;
                            end
                          else if sym^.typ=varsym then
                            begin
                               if not(is_equal(pvarsym(sym)^.definition,
                                 p^.proptype)) then
                                 Message(parser_e_ill_property_access_sym);
                            end;
                          p^.readaccesssym:=sym;
                       end;
                     consume(ID);
                  end;
                if (token=ID) and (pattern='WRITE') then
                  begin
                     consume(ID);
                     sym:=search_class_member(aktclass,pattern);
                     if not(assigned(sym)) then
                       Message1(sym_e_unknown_id,pattern)
                     else
                       begin
                          if ((sym^.typ=varsym) and
                             assigned(propertyparas)) or
                             not(sym^.typ in [varsym,procsym]) then
                            Message(parser_e_ill_property_access_sym);
                          { search the matching definition }
                          if sym^.typ=procsym then
                            begin
                               { insert data entry to check access method }
                               datacoll^.next:=propertyparas;
                               propertyparas:=datacoll;
                               pp:=get_procdef;
                               { ... and remove it }
                               propertyparas:=propertyparas^.next;
                               if not(assigned(pp)) then
                                 Message(parser_e_ill_property_access_sym);
                               p^.writeaccessdef:=pp;
                            end
                          else if sym^.typ=varsym then
                            begin
                               if not(is_equal(pvarsym(sym)^.definition,
                                 p^.proptype)) then
                                 Message(parser_e_ill_property_access_sym);
                            end;
                          p^.writeaccesssym:=sym;
                       end;
                     consume(ID);
                  end;
                if (token=ID) and (pattern='STORED') then
                  begin
                     consume(ID);
                     { !!!!!!!! }
                  end;
                if (token=ID) and (pattern='DEFAULT') then
                  begin
                     consume(ID);
                     { !!!!!!! storage }
                     consume(SEMICOLON);
                  end
                else if (token=ID) and (pattern='NODEFAULT') then
                  begin
                     consume(ID);
                     { !!!!!!!! }
                  end;
                symtablestack^.insert(p);
                { default property ? }
                consume(SEMICOLON);
                if (token=ID) and (pattern='DEFAULT') then
                  begin
                     consume(ID);
                     p2:=search_default_property(aktclass);
                     if assigned(p2) then
                       message1(parser_e_only_one_default_property,
                         pobjectdef(p2^.owner^.defowner)^.name^)
                     else
                       begin
                          p^.options:=p^.options or ppo_defaultproperty;
                          if not(assigned(propertyparas)) then
                            message(parser_e_property_need_paras);
                       end;
                     consume(SEMICOLON);
                  end;
                { clean up }
                if assigned(datacoll) then
                  dispose(datacoll);
             end
           else
             begin
                consume(ID);
                consume(SEMICOLON);
             end;
        end;

      procedure destructor_head;

        begin
           consume(_DESTRUCTOR);
           _proc_head(podestructor);
           if (cs_checkconsname in aktswitches) and (aktprocsym^.name<>'DONE') then
            Message(parser_e_destructorname_must_be_done);
           consume(SEMICOLON);
           if assigned(aktprocsym^.definition^.para1) then
            Message(parser_e_no_paras_for_destructor);
           { no return value }
           aktprocsym^.definition^.retdef:=voiddef;
        end;

      procedure object_komponenten;

        var
           oldparse_only : boolean;

        begin
           repeat
             if actmembertype=sp_private then
               aktclass^.options:=aktclass^.options or oo_hasprivate;
             if actmembertype=sp_protected then
               aktclass^.options:=aktclass^.options or oo_hasprotected;

             case token of
                ID:
                  begin
                     if (pattern='PUBLIC') or
                       (pattern='PUBLISHED') or
                       (pattern='PROTECTED') or
                       (pattern='PRIVATE') then
                       exit;
                     read_var_decs(false,false);
                  end;
                _PROPERTY:
                  property_dec;
                _PROCEDURE,_FUNCTION,_CLASS:
                  begin
                     oldparse_only:=parse_only;
                     parse_only:=true;
                     proc_head;
                     parse_only:=oldparse_only;
                     if (token=ID) and
                       ((pattern='VIRTUAL') or (pattern='DYNAMIC')) then
                       begin
                          if actmembertype=sp_private then
                           Message(parser_w_priv_meth_not_virtual);
                          consume(ID);
                          consume(SEMICOLON);
                          aktprocsym^.definition^.options:=
                            aktprocsym^.definition^.options or povirtualmethod;
                          aktclass^.options:=aktclass^.options or oo_hasvirtual;
                       end
                     else if (token=ID) and (pattern='OVERRIDE') then
                       begin
                          consume(ID);
                          consume(SEMICOLON);
                          aktprocsym^.definition^.options:=
                            aktprocsym^.definition^.options or pooverridingmethod or povirtualmethod;
                       end;
                     { Delphi II extension }
                     if (token=ID) and (pattern='ABSTRACT') then
                       begin
                          consume(ID);
                          consume(SEMICOLON);
                          if (aktprocsym^.definition^.options and povirtualmethod)<>0 then
                            begin
                               aktprocsym^.definition^.options:=
                                aktprocsym^.definition^.options or
                                  poabstractmethod;
                            end
                          else
                            Message(parser_e_only_virtual_methods_abstract);
                          { the method is defined }
                          aktprocsym^.definition^.forwarddef:=false;
                       end;
                     if (token=ID) and (pattern='STATIC') and
                        (cs_static_keyword in aktswitches) then
                       begin
                          consume(ID);
                          consume(SEMICOLON);
                          aktprocsym^.properties:=
                            aktprocsym^.properties or
                              sp_static;
                          aktprocsym^.definition^.options:=
                            aktprocsym^.definition^.options or
                               postaticmethod;
                       end;
                  end;
                _CONSTRUCTOR:
                  begin
                     if actmembertype<>sp_public then
                       Message(parser_w_constructor_should_be_public);
                     oldparse_only:=parse_only;
                     parse_only:=true;
                     constructor_head;
                     parse_only:=oldparse_only;
                     if (token=ID) and
                       ((pattern='VIRTUAL') or (pattern='DYNAMIC')) then
                       begin
                          consume(ID);
                          consume(SEMICOLON);
                          if not(aktclass^.isclass) then
                            Message(parser_e_constructor_cannot_be_not_virtual)
                          else
                            begin
                               aktprocsym^.definition^.options:=
                                 aktprocsym^.definition^.options or povirtualmethod;
                               aktclass^.options:=aktclass^.options or oo_hasvirtual;
                            end
                       end
                     else if (token=ID) and (pattern='OVERRIDE') then
                       begin
                          consume(ID);
                          consume(SEMICOLON);
                          if (aktclass^.options and oois_class=0) then
                            Message(parser_e_constructor_cannot_be_not_virtual)
                          else
                            begin
                               aktprocsym^.definition^.options:=
                                 aktprocsym^.definition^.options or pooverridingmethod or povirtualmethod;
                            end;
                       end;
                  end;
                _DESTRUCTOR:
                  begin
                     if there_is_a_destructor then
                      Message(parser_n_only_one_destructor);
                     there_is_a_destructor:=true;

                     if actmembertype<>sp_public then
                      Message(parser_w_destructor_should_be_public);
                     oldparse_only:=parse_only;
                     parse_only:=true;
                     destructor_head;
                     parse_only:=oldparse_only;
                     if (token=ID) and
                       ((pattern='VIRTUAL') or (pattern='DYNAMIC')) then
                       begin
                          consume(ID);
                          consume(SEMICOLON);
                          aktprocsym^.definition^.options:=
                            aktprocsym^.definition^.options or povirtualmethod;
                       end
                     else if (token=ID) and (pattern='OVERRIDE') then
                       begin
                          consume(ID);
                          consume(SEMICOLON);
                          aktprocsym^.definition^.options:=
                            aktprocsym^.definition^.options or pooverridingmethod or povirtualmethod;
                       end;
                  end;
                _END : exit;
                else Message(parser_e_syntax_error);
             end;
           until false;
        end;

      var
         hs : string;
         pcrd : pclassrefdef;
         hp1 : pdef;
         oldprocsym:Pprocsym;

      begin
         {Nowadays aktprocsym may already have a value, so we need to save
          it.}
         oldprocsym:=aktprocsym;
         { forward is resolved }
         if assigned(fd) then
           fd^.options:=fd^.options and not(oo_isforward);

         there_is_a_destructor:=false;
         actmembertype:=sp_public;

         { objects and class types can't be declared local }
         if (symtablestack^.symtabletype<>globalsymtable) and
           (symtablestack^.symtabletype<>staticsymtable) then
           Message(parser_e_no_local_objects);

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
                   if typecanbeforward then
                     forwardsallowed:=true;
                   hp1:=single_type(hs);

                   { accept hp1, if is a forward def ...}
                   if ((lasttypesym<>nil)
                       and ((lasttypesym^.properties and sp_forwarddef)<>0)) or
                   { or a class
                     (if the foward defined type is a class is checked, when
                      the forward is resolved)
                   }
                     ((hp1^.deftype=objectdef) and (
                     (pobjectdef(hp1)^.options and oois_class)<>0)) then
                     begin
                        pcrd:=new(pclassrefdef,init(hp1));
                        object_dec:=pcrd;
                        {I add big troubles here
                        with var p : ^byte in graph.putimage
                        because a save_forward was called and
                        no resolve forward
                        => so the definition was rewritten after
                        having been disposed !!
                        Strange problems appeared !!!!}
                        {Anyhow forwards should only be allowed
                        inside a type statement ??
                        don't you think so }
                        if (lasttypesym<>nil)
                          and ((lasttypesym^.properties and sp_forwarddef)<>0) then
                            lasttypesym^.forwardpointer:=ppointerdef(pcrd);
                        forwardsallowed:=false;
                     end
                   else
                     begin
                        Message(parser_e_class_type_expected);
                        object_dec:=new(perrordef,init);
                     end;
                   exit;
                end
              { forward class }
              else if not(assigned(fd)) and (token=SEMICOLON) then
                begin
                   { also anonym objects aren't allow (o : object a : longint; end;) }
                   if n='' then
                    Message(parser_e_no_anonym_objects);
                   if n='TOBJECT' then
                     begin
                        aktclass:=new(pobjectdef,init(n,nil));
                        class_tobject:=aktclass;
                     end
                   else
                     aktclass:=new(pobjectdef,init(n,class_tobject));
                   aktclass^.options:=aktclass^.options or oois_class or oo_isforward;
                   object_dec:=aktclass;
                   exit;
                end;
           end;

         { also anonym objects aren't allow (o : object a : longint; end;) }
         if n='' then
           Message(parser_e_no_anonym_objects);

         { read the parent class }
         if token=LKLAMMER then
           begin
              consume(LKLAMMER);
              { does not allow objects.tobject !! }
              {if token<>ID then
                consume(ID);
              getsym(pattern,true);}
              childof:=pobjectdef(id_type(pattern));
              if (childof^.deftype<>objectdef) then
                 begin
                    Message(parser_e_class_type_expected);
                    childof:=nil;
                 end;
                   { a mix of class and object isn't allowed }
              if (((childof^.options and oois_class)<>0) and not is_a_class) or
                 (((childof^.options and oois_class)=0) and is_a_class) then
                Message(parser_e_mix_of_classes_and_objects);
              consume(RKLAMMER);
              if assigned(fd) then
                begin
                   fd^.childof:=childof;
                   aktclass:=fd;
                end
              else
                aktclass:=new(pobjectdef,init(n,childof));
           end
         { if no parent class, then a class get tobject as parent }
         else if is_a_class then
           begin
              { is the current class tobject?        }
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
                        aktclass:=fd;
                        aktclass^.childof:=childof;
                     end
                   else
                     aktclass:=new(pobjectdef,init(n,childof));
                end;
           end
         else aktclass:=new(pobjectdef,init(n,nil));

         { set the class attribute }
         if is_a_class then
           begin
              aktclass^.options:=aktclass^.options or oois_class;

              if (cs_generate_rtti in aktswitches) or
                  (assigned(aktclass^.childof) and
                   ((aktclass^.childof^.options and oo_can_have_published)<>0)
                  ) then
                aktclass^.options:=aktclass^.options or oo_can_have_published;
           end;

         aktobjectdef:=aktclass;

         { default access is public }
         actmembertype:=sp_public;
         aktclass^.publicsyms^.next:=symtablestack;
         symtablestack:=aktclass^.publicsyms;
         procinfo._class:=aktclass;
         testcurobject:=1;
         curobjectname:=n;

         { short class declaration ? }
         if token<>SEMICOLON then
           begin
              while token<>_END do
                begin
                   if (token=ID) and (pattern='PRIVATE') then
                     begin
                        consume(ID);
                        actmembertype:=sp_private;
                        current_object_option:=sp_private;
                     end;
                   if (token=ID) and (pattern='PROTECTED') then
                     begin
                        consume(ID);
                        current_object_option:=sp_protected;
                        actmembertype:=sp_protected;
                     end;
                   if (token=ID) and (pattern='PUBLIC') then
                     begin
                        consume(ID);
                        current_object_option:=sp_public;
                        actmembertype:=sp_public;
                     end;
                   if (token=ID) and (pattern='PUBLISHED') then
                     begin
                        if (aktclass^.options and oo_can_have_published)=0 then
                          Message(parser_e_cant_have_published);
                        consume(ID);
                        current_object_option:=sp_published;
                        actmembertype:=sp_published;
                     end;
                   object_komponenten;
                end;
              current_object_option:=sp_public;
              consume(_END);
           end;
         testcurobject:=0;
         curobjectname:='';

         if (cs_smartlink in aktswitches) then
           datasegment^.concat(new(pai_cut,init));
{$ifdef GDB}
         { generate the VMT }
         if cs_debuginfo in aktswitches then
           begin
              do_count_dbx:=true;
              if assigned(aktclass^.owner) and assigned(aktclass^.owner^.name) then
               datasegment^.concat(new(pai_stabs,init(strpnew('"vmt_'+aktclass^.owner^.name^+n+':S'+
                typeglobalnumber('__vtbl_ptr_type')+'",'+tostr(N_STSYM)+',0,0,'+aktclass^.vmt_mangledname))));
           end;
{$endif GDB}
         datasegment^.concat(new(pai_symbol,init_global(aktclass^.vmt_mangledname)));

         { determine the size with publicsyms^.datasize, because }
         { size gives back 4 for CLASSes                         }
         datasegment^.concat(new(pai_const,init_32bit(aktclass^.publicsyms^.datasize)));
         datasegment^.concat(new(pai_const,init_32bit(-aktclass^.publicsyms^.datasize)));

         { write pointer to parent VMT, this isn't implemented in TP }
         { but this is not used in FPC ? (PM) }
         { it's not used yet, but the delphi-operators as and is need it (FK) }
         if assigned(aktclass^.childof) then
           begin
              datasegment^.concat(new(pai_const,init_symbol(strpnew(aktclass^.childof^.vmt_mangledname))));
              if aktclass^.childof^.owner^.symtabletype=unitsymtable then
                concat_external(aktclass^.childof^.vmt_mangledname,EXT_NEAR);
           end
         else
           datasegment^.concat(new(pai_const,init_32bit(0)));

         { this generates the entries }
         genvmt(aktclass);

         { restore old state }
         symtablestack:=symtablestack^.next;
         procinfo._class:=nil;
         {Restore the aktprocsym.}
         aktprocsym:=oldprocsym;

         object_dec:=aktclass;
      end;

    { reads a record declaration }
    function record_dec : pdef;

      var
         symtable : psymtable;

      begin
         symtable:=new(psymtable,init(recordsymtable));
         symtable^.next:=symtablestack;
         symtablestack:=symtable;
         consume(_RECORD);
         read_var_decs(true,false);

         { may be scale record size to a size of n*4 ? }
         if ((symtablestack^.datasize mod aktpackrecords)<>0) then
           inc(symtablestack^.datasize,aktpackrecords-(symtablestack^.datasize mod aktpackrecords));

         consume(_END);
         symtablestack:=symtable^.next;
         record_dec:=new(precdef,init(symtable));
      end;

    { reads a type definition and returns a pointer to it }
    function read_type(const name : stringid) : pdef;

    function handle_procvar:Pprocvardef;

    var
       sc : pstringcontainer;
       s : string;
       p : pdef;
       varspez : tvarspez;
       procvardef : pprocvardef;

    begin
       procvardef:=new(pprocvardef,init);
       if token=LKLAMMER then
         begin
            consume(LKLAMMER);
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
              if token=COLON then
                begin
                   consume(COLON);
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
                        p:=new(parraydef,init(0,-1,s32bitdef));
                        { define field type }
                        parraydef(p)^.definition:=single_type(s);
                     end
                   else
                     p:=single_type(s);
                end
              else
                p:=new(pformaldef,init);
              s:=sc^.get;
              while s<>'' do
                begin
                   procvardef^.concatdef(p,varspez);
                   s:=sc^.get;
                end;
              dispose(sc,done);
              if token=SEMICOLON then consume(SEMICOLON)
            else break;
            until false;
            dec(testcurobject);
            consume(RKLAMMER);
         end;
       handle_procvar:=procvardef;
    end;

      var
         hp1,p : pdef;
         aufdef : penumdef;
         aufsym : penumsym;
         ap : parraydef;
         s : stringid;
         l,v,oldaktpackrecords : longint;
         hs : string;

      procedure expr_type;

        var
           pt1,pt2 : ptree;

        begin
           { use of current parsed object ? }
           if (token=ID) and (testcurobject=2) and (curobjectname=pattern) then
             begin
                consume(ID);
                p:=aktobjectdef;
                exit;
             end;
           { we can't accept a equal in type }
           pt1:=comp_expr(not(ignore_equal));
           if (pt1^.treetype=typen) and (token<>POINTPOINT) then
             begin
                { a simple type renaming }
                p:=pt1^.resulttype;
             end
           else
             begin
                { range type }
                consume(POINTPOINT);
                { range type declaration }
                do_firstpass(pt1);
                pt2:=comp_expr(not(ignore_equal));
                do_firstpass(pt2);
                { valid expression ? }
                if (pt1^.treetype<>ordconstn) or
                   (pt2^.treetype<>ordconstn) then
                  Begin
                    Message(sym_e_error_in_type_def);
                    { Here we create a node type with a range of 0  }
                    { To make sure that no crashes will occur later }
                    { on in the compiler.                           }
                    p:=new(porddef,init(uauto,0,0));
                  end
                else
                  p:=new(porddef,init(uauto,pt1^.value,pt2^.value));
                disposetree(pt2);
             end;
           disposetree(pt1);
        end;

      var
         pt : ptree;

      procedure array_dec;

        begin
           consume(_ARRAY);
           consume(LECKKLAMMER);
           p:=nil;
           repeat
             { read the expression and check it }
             pt:=expr;
             if pt^.treetype=typen then
               begin
                  if pt^.resulttype^.deftype=enumdef then
                    begin
                       if p=nil then
                         begin
                            ap:=new(parraydef,
                              init(0,penumdef(pt^.resulttype)^.max,pt^.resulttype));
                            p:=ap;
                         end
                       else
                         begin
                            ap^.definition:=new(parraydef,
                              init(0,penumdef(pt^.resulttype)^.max,pt^.resulttype));
                            ap:=parraydef(ap^.definition);
                         end;
                    end
                  else if pt^.resulttype^.deftype=orddef then
                    begin
                       case porddef(pt^.resulttype)^.typ of
                          s8bit,u8bit,s16bit,u16bit,s32bit :
                            begin
                               if p=nil then
                                 begin
                                    ap:=new(parraydef,init(porddef(pt^.resulttype)^.von,
                                      porddef(pt^.resulttype)^.bis,pt^.resulttype));
                                    p:=ap;
                                 end
                               else
                                 begin
                                    ap^.definition:=new(parraydef,init(porddef(pt^.resulttype)^.von,
                                      porddef(pt^.resulttype)^.bis,pt^.resulttype));
                                    ap:=parraydef(ap^.definition);
                                 end;
                            end;
                          bool8bit:
                            begin
                               if p=nil then
                                 begin
                                    ap:=new(parraydef,init(0,1,pt^.resulttype));
                                    p:=ap;
                                 end
                               else
                                 begin
                                    ap^.definition:=new(parraydef,init(0,1,pt^.resulttype));
                                    ap:=parraydef(ap^.definition);
                                 end;
                            end;
                          uchar:
                            begin
                               if p=nil then
                                 begin
                                    ap:=new(parraydef,init(0,255,pt^.resulttype));
                                    p:=ap;
                                 end
                               else
                                 begin
                                    ap^.definition:=new(parraydef,init(0,255,pt^.resulttype));
                                    ap:=parraydef(ap^.definition);
                                 end;
                            end;
                          else Message(sym_e_error_in_type_def);
                       end;
                    end
                  else Message(sym_e_error_in_type_def);
               end
             else
               begin
                  do_firstpass(pt);

                  if (pt^.treetype<>rangen) or
                     (pt^.left^.treetype<>ordconstn) then
                    Message(sym_e_error_in_type_def);
                  { force the registration of the ranges }
{$ifndef GDB}
                  if pt^.right^.resulttype=pdef(s32bitdef) then
                    pt^.right^.resulttype:=new(porddef,init(
                      s32bit,$80000000,$7fffffff));
{$endif GDB}
                  if p=nil then
                    begin
                       ap:=new(parraydef,init(pt^.left^.value,pt^.right^.value,pt^.right^.resulttype));
                       p:=ap;
                    end
                  else
                    begin
                       ap^.definition:=new(parraydef,init(pt^.left^.value,pt^.right^.value,pt^.right^.resulttype));
                       ap:=parraydef(ap^.definition);
                    end;
               end;
             disposetree(pt);

             if token=COMMA then consume(COMMA)
               else break;
           until false;
           consume(RECKKLAMMER);
           consume(_OF);
           hp1:=read_type('');
           { if no error, set element type }
           if assigned(ap) then
             ap^.definition:=hp1;
        end;

      begin
         case token of
            _STRING,_FILE:
              p:=single_type(hs);
            LKLAMMER:
              begin
                 consume(LKLAMMER);
                 l:=-1;
                 aufsym := Nil;
                 aufdef:=new(penumdef,init);
                 repeat
                   s:=pattern;
                   consume(ID);
                   if token=ASSIGNMENT then
                     begin
                        consume(ASSIGNMENT);
                        v:=get_intconst;
                        { please leave that a note, allows type save }
                        { declarations in the win32 units !          }
                        if v<=l then
                         Message(parser_n_duplicate_enum);
                        l:=v;
                     end
                   else
                     inc(l);
                   constsymtable^.insert(new(penumsym,init(s,aufdef,l)));
                   if token=COMMA then
                     consume(COMMA)
                   else
                     break;
                 until false;
                 aufdef^.max:=l;
                 p:=aufdef;
                 consume(RKLAMMER);
              end;
            _ARRAY:
              array_dec;
            _SET:
              begin
                 consume(_SET);
                 consume(_OF);
                 hp1:=read_type('');
                 case hp1^.deftype of
                    enumdef : p:=new(psetdef,init(hp1,penumdef(hp1)^.max));
                    orddef : begin
                                  case porddef(hp1)^.typ of
                                     uchar : p:=new(psetdef,init(hp1,255));
                                     u8bit,s8bit,u16bit,s16bit,s32bit :
                                       begin
                                          if (porddef(hp1)^.von>=0) then
                                            p:=new(psetdef,init(hp1,porddef(hp1)^.bis))
                                          else Message(sym_e_ill_type_decl_set);
                                       end;
                                  else Message(sym_e_ill_type_decl_set);
                                  end;
                               end;
                    else Message(sym_e_ill_type_decl_set);
                 end;
              end;
            CARET:
              begin
                 consume(CARET);
                 { forwards allowed only inside TYPE statements }
                 if typecanbeforward then
                    forwardsallowed:=true;
                 hp1:=single_type(hs);
                 p:=new(ppointerdef,init(hp1));
                 {I add big troubles here
                 with var p : ^byte in graph.putimage
                 because a save_forward was called and
                 no resolve forward
                 => so the definition was rewritten after
                 having been disposed !!
                 Strange problems appeared !!!!}
                 {Anyhow forwards should only be allowed
                 inside a type statement ??
                 don't you think so }
                 if (lasttypesym<>nil)
                   and ((lasttypesym^.properties and sp_forwarddef)<>0) then
                     lasttypesym^.forwardpointer:=ppointerdef(p);
                 forwardsallowed:=false;
              end;
            _RECORD:
              p:=record_dec;
            _PACKED:
              begin
                 consume(_PACKED);
                 if token=_ARRAY then
                   array_dec
                 else
                   begin
                      oldaktpackrecords:=aktpackrecords;
                      aktpackrecords:=1;
                      if token in [_CLASS,_OBJECT] then
                        p:=object_dec(name,nil)
                      else
                        p:=record_dec;
                      aktpackrecords:=oldaktpackrecords;
                   end;
              end;
            _CLASS,
            _OBJECT:
              p:=object_dec(name,nil);
            _PROCEDURE:
              begin
                 consume(_PROCEDURE);
                 p:=handle_procvar;
                 if token=_OF then
                   begin
                      consume(_OF);
                      consume(_OBJECT);
                      pprocvardef(p)^.options:=pprocvardef(p)^.options or pomethodpointer;
                   end;
              end;
            _FUNCTION:
              begin
                 consume(_FUNCTION);
                 p:=handle_procvar;
                 consume(COLON);
                 pprocvardef(p)^.retdef:=single_type(hs);
                 if token=_OF then
                   begin
                      consume(_OF);
                      consume(_OBJECT);
                      pprocvardef(p)^.options:=pprocvardef(p)^.options or pomethodpointer;
                   end;
              end;
            else
              expr_type;
         end;
         read_type:=p;
      end;

    { search in symtablestack used, but not defined type }
    procedure testforward_types(p : psym);{$ifndef FPC}far;{$endif}

      begin
         if (p^.typ=typesym) and ((p^.properties and sp_forwarddef)<>0) then
           Message(sym_e_type_id_not_defined);
      end;

    { reads a type declaration to the symbol table }
    procedure type_dec;

      var
         typename : stringid;
{$ifdef dummy}
         olddef,newdef : pdef;
         s : string;
{$endif dummy}

      begin
         block_type:=bt_type;
         consume(_TYPE);
         typecanbeforward:=true;
         repeat
           typename:=pattern;
           consume(ID);
           consume(EQUAL);
             { here you loose the strictness of pascal
             for which a redefinition like
               childtype = parenttype;
                           child2type = parenttype;
             does not make the two child types equal !!
             here all vars from childtype and child2type
             get the definition of parenttype !!            }
{$ifdef testequaltype}
           if (token = ID) or (token=_FILE) or (token=_STRING) then
             begin
                olddef := single_type(s);
                { make a clone of olddef }
                { is that ok ??? }
                getmem(newdef,SizeOf(olddef));
                move(olddef^,newdef^,SizeOf(olddef));
                symtablestack^.insert(new(ptypesym,init(typename,newdef)));
             end
           else
{$endif testequaltype}
             begin
                getsym(typename,false);
                { check if it is the definition of a forward defined class }
                if assigned(srsym) and (token=_CLASS) and
                  (srsym^.typ=typesym) and
                  (ptypesym(srsym)^.definition^.deftype=objectdef) and
                  ((pobjectdef(ptypesym(srsym)^.definition)^.options and oo_isforward)<>0) and
                  ((pobjectdef(ptypesym(srsym)^.definition)^.options and oois_class)<>0) then
                  begin
                     { we can ignore the result   }
                     { the definition is modified }
                     object_dec(typename,pobjectdef(ptypesym(srsym)^.definition));
                  end
                else
                  symtablestack^.insert(new(ptypesym,init(typename,read_type(typename))));
             end;
           consume(SEMICOLON);
         until token<>ID;
         typecanbeforward:=false;
{$ifdef tp}
         symtablestack^.foreach(testforward_types);
{$else}
         symtablestack^.foreach(@testforward_types);
{$endif}
         resolve_forwards;
         block_type:=bt_general;
      end;

    { parses varaible declarations and inserts them in }
    { the top symbol table of symtablestack            }
    procedure var_dec;

      {var
         p : pdef;
         sc : pstringcontainer;      }

      begin
         consume(_VAR);
         read_var_decs(false,true);
      end;

    { reads the filed of a record into a        }
    { symtablestack, if record=false            }
    { variants are forbidden, so this procedure }
    { can be used to read object fields         }
    { if absolute is true, ABSOLUTE and file    }
    { types are allowed                         }
    { => the procedure is also used to read     }
    { a sequence of variable declaration        }
    procedure read_var_decs(is_record : boolean;do_absolute : boolean);

      var
         sc : pstringcontainer;
         s : stringid;
         l    : longint;
         code : word;
         hs : string;
         p,casedef : pdef;
         { maxsize contains the max. size of a variant }
         { startvarrec contains the start of the variant part of a record }
         maxsize,startvarrec : longint;
         pt : ptree;
         old_block_type : tblock_type;
         { to handle absolute }
         abssym : pabsolutesym;
         filepos : tfileposinfo;


      begin
         hs:='';
         old_block_type:=block_type;
         block_type:=bt_type;
       { Force an expected ID error message }
         if not (token in [ID,_CASE,_END]) then
          consume(ID);
       { read vars }
         while (token=ID) and
           (pattern<>'PUBLIC') and
           (pattern<>'PRIVATE') and
           (pattern<>'PUBLISHED') and
           (pattern<>'PROTECTED') do
           begin
              sc:=idlist;
              consume(COLON);
              p:=read_type('');
              if do_absolute and (token=ID) and (pattern='ABSOLUTE') then
                begin
                   s:=sc^.get_with_tokeninfo(filepos);
                   if sc^.get<>'' then
                    Message(parser_e_absolute_only_one_var);
                   dispose(sc,done);
                   consume(ID);
                   if token=ID then
                     begin
                        getsym(pattern,true);
                        consume(ID);
                        { we should check the result type of srsym }
                        if not (srsym^.typ in [varsym,typedconstsym]) then
                         Message(parser_e_absolute_only_to_var_or_const);
                        abssym:=new(pabsolutesym,init(s,p));
                        abssym^.typ:=absolutesym;
                        abssym^.abstyp:=tovar;
                        abssym^.ref:=srsym;
                        abssym^.line_no:=filepos.line;
                        symtablestack^.insert(abssym);
                     end
                   else
                   if token=CSTRING then
                     begin
                        abssym:=new(pabsolutesym,init(s,p));
                        s:=pattern;
                        consume(CSTRING);
                        abssym^.typ:=absolutesym;
                        abssym^.abstyp:=toasm;
                        abssym^.asmname:=stringdup(s);
                        abssym^.line_no:=filepos.line;
                        symtablestack^.insert(abssym);
                     end
                   else
                   { absolute address ?!? }
                   if token=INTCONST then
                     begin
                       if (target_info.target=target_GO32V2) then
                        begin
                          abssym:=new(pabsolutesym,init(s,p));
                          abssym^.typ:=absolutesym;
                          abssym^.abstyp:=toaddr;
                          abssym^.absseg:=false;
                          abssym^.line_no:=filepos.line;
                          s:=pattern;
                          consume(INTCONST);
                          val(s,abssym^.address,code);
                          if token=COLON then
                           begin
                             consume(token);
                             s:=pattern;
                             consume(INTCONST);
                             val(s,l,code);
                             abssym^.address:=abssym^.address shl 4+l;
                             abssym^.absseg:=true;
                           end;
                          symtablestack^.insert(abssym);
                        end
                       else
                        Message(parser_e_absolute_only_to_var_or_const);
                     end
                   else
                     Message(parser_e_absolute_only_to_var_or_const);
                end
              else
                begin
                   if token=SEMICOLON then
                     begin
                        if (symtablestack^.symtabletype=objectsymtable) then
                          begin
                             consume(SEMICOLON);
                             if (token=ID) and (pattern='STATIC') and
                                (cs_static_keyword in aktswitches) then
                               begin
                                  current_object_option:=current_object_option or sp_static;
                                  insert_syms(symtablestack,sc,p);
                                  current_object_option:=current_object_option - sp_static;
                                  consume(ID);
                                  consume(SEMICOLON);
                               end
                             else
                               { this will still be a the wrong line !! }
                               insert_syms(symtablestack,sc,p);
                          end
                        else
                          begin
                             { at the right line }
                             insert_syms(symtablestack,sc,p);
                             consume(SEMICOLON);
                          end
                     end
                   else
                     begin
                        insert_syms(symtablestack,sc,p);
                        if not(is_record) then
                          consume(SEMICOLON);
                     end;
                end;
              while token=SEMICOLON do
                consume(SEMICOLON);
           end;
         if (token=_CASE) and is_record then
           begin
              maxsize:=0;
              consume(_CASE);
              s:=pattern;
              getsym(s,false);
              { may be only a type: }
              if assigned(srsym) and ((srsym^.typ=typesym) or
              { and with unit qualifier: }
                (srsym^.typ=unitsym)) then
                begin
                   casedef:=read_type('');
                end
              else
                begin
                   consume(ID);
                   consume(COLON);

                   casedef:=read_type('');
                   symtablestack^.insert(new(pvarsym,init(s,casedef)));
                end;
              if not is_ordinal(casedef) then
               Message(parser_e_ordinal_expected);

              consume(_OF);
              startvarrec:=symtablestack^.datasize;
              repeat
                repeat
                  pt:=comp_expr(true);
                  do_firstpass(pt);
                  if not(pt^.treetype=ordconstn) then
                    Message(cg_e_illegal_expression);
                  disposetree(pt);
                  if token=COMMA then consume(COMMA)
                    else break;
                until false;
                consume(COLON);
                consume(LKLAMMER);
                if token<>RKLAMMER then
                  read_var_decs(true,false);

                { calculates maximal variant size }
                maxsize:=max(maxsize,symtablestack^.datasize);

                { the items of the next variant are overlayed }
                symtablestack^.datasize:=startvarrec;
                consume(RKLAMMER);
                if token<>SEMICOLON then
                  break
                else
                  consume(SEMICOLON);
                while token=SEMICOLON do
                  consume(SEMICOLON);
              until (token=_END) or (token=RKLAMMER);

              { at last set the record size to that of the biggest variant }
              symtablestack^.datasize:=maxsize;
           end;
         block_type:=old_block_type;
      end;

    procedure read_declarations(islibrary : boolean);

      begin
         repeat
           case token of
              _LABEL:
                label_dec;
              _CONST:
                const_dec;
              _TYPE:
                type_dec;
              _VAR:
                var_dec;
              _CONSTRUCTOR,_DESTRUCTOR,
              _FUNCTION,_PROCEDURE,_OPERATOR,_CLASS:
                unter_dec;
              _EXPORTS:
                if islibrary then
                  read_exports
                else
                  break;
              else break;
           end;
         until false;
      end;

    procedure read_interface_declarations;

      begin
         {Since the body is now parsed at lexlevel 1, and the declarations
          must be parsed at the same lexlevel we increase the lexlevel.}
         inc(lexlevel);
         repeat
           case token of
              _CONST : const_dec;
              _TYPE : type_dec;
              _VAR : var_dec;
              { should we allow operator in interface ? }
              { of course otherwise you cannot          }
              { declare an operator usable by other     }
              { units or progs                       PM }
              _FUNCTION,_PROCEDURE,_OPERATOR : unter_dec;
              else
                 break;
           end;
         until false;
         dec(lexlevel);
      end;
end.
{
  $Log$
  Revision 1.20  1998-05-28 14:35:54  peter
    * nicer error message when no id is used after var

  Revision 1.19  1998/05/23 01:21:19  peter
    + aktasmmode, aktoptprocessor, aktoutputformat
    + smartlink per module $SMARTLINK-/+ (like MMX) and moved to aktswitches
    + $LIBNAME to set the library name where the unit will be put in
    * splitted cgi386 a bit (codeseg to large for bp7)
    * nasm, tasm works again. nasm moved to ag386nsm.pas

  Revision 1.18  1998/05/20 09:42:35  pierre
    + UseTokenInfo now default
    * unit in interface uses and implementation uses gives error now
    * only one error for unknown symbol (uses lastsymknown boolean)
      the problem came from the label code !
    + first inlined procedures and function work
      (warning there might be allowed cases were the result is still wrong !!)
    * UseBrower updated gives a global list of all position of all used symbols
      with switch -gb

  Revision 1.17  1998/05/11 13:07:55  peter
    + $ifdef NEWPPU for the new ppuformat
    + $define GDB not longer required
    * removed all warnings and stripped some log comments
    * no findfirst/findnext anymore to remove smartlink *.o files

  Revision 1.16  1998/05/05 12:05:42  florian
    * problems with properties fixed
    * crash fixed:  i:=l when i and l are undefined, was a problem with
      implementation of private/protected

  Revision 1.15  1998/05/01 09:01:23  florian
    + correct semantics of private and protected
    * small fix in variable scope:
       a id can be used in a parameter list of a method, even it is used in
       an anchestor class as field id

  Revision 1.14  1998/05/01 07:43:56  florian
    + basics for rtti implemented
    + switch $m (generate rtti for published sections)

  Revision 1.13  1998/04/30 15:59:41  pierre
    * GDB works again better :
      correct type info in one pass
    + UseTokenInfo for better source position
    * fixed one remaining bug in scanner for line counts
    * several little fixes

  Revision 1.12  1998/04/29 10:33:57  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.11  1998/04/28 11:45:52  florian
    * make it compilable with TP
    + small COM problems solved to compile classes.pp

  Revision 1.10  1998/04/27 23:10:28  peter
    + new scanner
    * $makelib -> if smartlink
    * small filename fixes pmodule.setfilename
    * moved import from files.pas -> import.pas

  Revision 1.9  1998/04/10 21:36:56  florian
    + some stuff to support method pointers (procedure of object) added
      (declaration, parameter handling)

  Revision 1.8  1998/04/10 15:39:48  florian
    * more fixes to get classes.pas compiled

  Revision 1.7  1998/04/09 23:02:15  florian
    * small problems solved to get remake3 work

  Revision 1.6  1998/04/09 22:16:35  florian
    * problem with previous REGALLOC solved
    * improved property support

  Revision 1.5  1998/04/08 14:59:20  florian
    * problem with new expr_type solved

  Revision 1.4  1998/04/08 10:26:09  florian
    * correct error handling of virtual constructors
    * problem with new type declaration handling fixed

  Revision 1.3  1998/04/07 22:45:05  florian
    * bug0092, bug0115 and bug0121 fixed
    + packed object/class/array

  Revision 1.2  1998/04/05 13:58:35  peter
    * fixed the -Ss bug
    + warning for Virtual constructors
    * helppages updated with -TGO32V1
}
