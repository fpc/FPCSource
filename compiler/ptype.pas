{
    $Id$
    Copyright (c) 1999 by Florian Klaempfl

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
       { ttypesym read by read_type, this is needed to be
         stored in the ppu for resolving purposes }
       readtypesym : ptypesym;

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
    function single_type(var s : string;isforwarddef:boolean) : pdef;

    function read_type(const name : stringid) : pdef;


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


    function id_type(var s : string;isforwarddef:boolean) : pdef;
    { reads a type definition and returns a pointer }
    { to a appropriating pdef, s gets the name of   }
    { the type to allow name mangling          }
      var
        is_unit_specific : boolean;
      begin
         s:=pattern;
         consume(_ID);
         { classes can be used also in classes }
         if (curobjectname=pattern) and aktobjectdef^.is_class then
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
         { try to load the symbol to see if it's a unitsym }
         is_unit_specific:=false;
         getsym(s,false);
         if assigned(srsym) and
            (srsym^.typ=unitsym) then
           begin
              consume(_POINT);
              getsymonlyin(punitsym(srsym)^.unitsymtable,pattern);
              s:=pattern;
              consume(_ID);
              is_unit_specific:=true;
           end;
         { are we parsing a possible forward def ? }
         if isforwarddef and
            not(is_unit_specific) then
          begin
            id_type:=new(pforwarddef,init(s));
            exit;
          end;
         { unknown sym ? }
         if not assigned(srsym) then
          begin
            Message1(sym_e_id_not_found,s);
            id_type:=generrordef;
            exit;
          end;
         if (srsym^.typ<>typesym) then
          begin
            Message(type_e_type_id_expected);
            id_type:=generrordef;
            exit;
          end;
         { can't use in [] here, becuase unitid can be > 255 }
         if (ptypesym(srsym)^.owner^.unitid=0) or
            (ptypesym(srsym)^.owner^.unitid=1) then
          readtypesym:=nil
         else
          readtypesym:=ptypesym(srsym);
         { return the definition of the type }
         id_type:=ptypesym(srsym)^.definition;
      end;


    function single_type(var s : string;isforwarddef:boolean) : pdef;
    { reads a string, file type or a type id and returns a name and }
    { pdef                                                        }
       var
          hs : string;
       begin
          readtypesym:=nil;
          case token of
            _STRING:
                begin
                   single_type:=string_dec;
                   s:='STRING';
                   readtypesym:=nil;
                end;
            _FILE:
                begin
                   consume(_FILE);
                   if token=_OF then
                     begin
                        consume(_OF);
                        single_type:=new(pfiledef,init(ft_typed,single_type(hs,false)));
                        s:='FILE$OF$'+hs;
                     end
                   else
                     begin
                        single_type:=cfiledef;
                        s:='FILE';
                     end;
                   readtypesym:=nil;
                end;
            else
              begin
                single_type:=id_type(s,isforwarddef);
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

          procedure deletepropsymlist(p : ppropsymlist);

            var
               hp : ppropsymlist;

            begin
               while assigned(p) do
                 begin
                    hp:=p;
                    p:=p^.next;
                    dispose(hp);
                 end;
            end;

          procedure addpropsymlist(var root:ppropsymlist;s:psym);
          var
            last,hp : ppropsymlist;
          begin
            if not assigned(s) then
             exit;
            last:=root;
            new(hp);
            hp^.sym:=s;
            hp^.next:=nil;
            if assigned(last) then
             begin
               while assigned(last^.next) do
                last:=last^.next;
               last^.next:=hp;
             end
            else
             root:=hp;
          end;

          function copypropsymlist(s:ppropsymlist):ppropsymlist;
          var
            root,last,hp : ppropsymlist;
          begin
            copypropsymlist:=nil;
            if not assigned(s) then
             exit;
            last:=nil;
            root:=nil;
            while assigned(s) do
             begin
               new(hp);
               hp^.sym:=s^.sym;
               hp^.next:=nil;
               if assigned(last) then
                last^.next:=hp;
               last:=hp;
               if not assigned(root) then
                root:=hp;
               s:=s^.next;
             end;
            copypropsymlist:=root;
          end;

        var
           hp2,datacoll : pparaitem;
           p,p2 : ppropertysym;
           overriden : psym;
           hs : string;
           varspez : tvarspez;
           sc : pstringcontainer;
           hp : pdef;
           s : string;
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
                                 hp:=new(parraydef,init(0,-1,s32bitdef));
                                 { define field type }
                                 parraydef(hp)^.definition:=single_type(s,false);
                              end
                            else
                              hp:=single_type(s,false);
                         end
                       else
                         hp:=cformaldef;
                       repeat
                         s:=sc^.get_with_tokeninfo(declarepos);
                         if s='' then
                          break;
                         new(hp2,init);
                         hp2^.paratyp:=varspez;
                         hp2^.data:=hp;
                         propertyparas^.insert(hp2);
                       until false;
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
                     p^.proptype:=single_type(hs,false);
                     if (idtoken=_INDEX) then
                       begin
                          consume(_INDEX);
                          pt:=comp_expr(true);
                          do_firstpass(pt);
                          if not(is_ordinal(pt^.resulttype)) or
                             is_64bitint(pt^.resulttype) then
                            Message(parser_e_invalid_property_index_value);
                          p^.index:=pt^.value;
{$ifdef INCLUDEOK}
                          include(p^.propoptions,ppo_indexed);
{$else}
                          p^.propoptions:=p^.propoptions+[ppo_indexed];
{$endif}
                          { concat a longint to the para template }
                          new(hp2,init);
                          hp2^.paratyp:=vs_value;
                          hp2^.data:=pt^.resulttype;
                          propertyparas^.insert(hp2);
                          disposetree(pt);
                       end;
                  end
                else
                  begin
                     { do an property override }
                     overriden:=search_class_member(aktclass,propname);
                     if assigned(overriden) and (overriden^.typ=propertysym) then
                       begin
                          { take the whole info: }
                          p^.propoptions:=ppropertysym(overriden)^.propoptions;
                          p^.index:=ppropertysym(overriden)^.index;
                          p^.proptype:=ppropertysym(overriden)^.proptype;
                          p^.writeaccesssym:=copypropsymlist(ppropertysym(overriden)^.writeaccesssym);
                          p^.readaccesssym:=copypropsymlist(ppropertysym(overriden)^.readaccesssym);
                          p^.storedsym:=copypropsymlist(ppropertysym(overriden)^.storedsym);
                          p^.writeaccessdef:=ppropertysym(overriden)^.writeaccessdef;
                          p^.readaccessdef:=ppropertysym(overriden)^.readaccessdef;
                          p^.storeddef:=ppropertysym(overriden)^.storeddef;
                          p^.default:=ppropertysym(overriden)^.default;
                       end
                     else
                       begin
                          p^.proptype:=generrordef;
                          message(parser_e_no_property_found_to_override);
                       end;
                  end;

                if (sp_published in current_object_option) and
                   not(p^.proptype^.is_publishable) then
                  Message(parser_e_cant_publish_that_property);

                { create data defcoll to allow correct parameter checks }
                new(datacoll,init);
                datacoll^.paratyp:=vs_value;
                datacoll^.data:=p^.proptype;

                if (idtoken=_READ) then
                  begin
                     if assigned(p^.readaccesssym) then
                       deletepropsymlist(p^.readaccesssym);
                     p^.readaccesssym:=nil;
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
                                 (pvarsym(sym)^.definition^.deftype=recorddef)) do
                           begin
                             addpropsymlist(p^.readaccesssym,sym);
                             consume(_POINT);
                             getsymonlyin(precorddef(pvarsym(sym)^.definition)^.symtable,pattern);
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
                                    not(is_equal(pp^.retdef,p^.proptype)) then
                                   Message(parser_e_ill_property_access_sym);
                                 p^.readaccessdef:=pp;
                              end;
                            varsym :
                              begin
                                if not(propertyparas^.empty) or
                                   not(is_equal(pvarsym(sym)^.definition,p^.proptype)) then
                                  Message(parser_e_ill_property_access_sym);
                              end;
                            else
                              Message(parser_e_ill_property_access_sym);
                          end;
                          addpropsymlist(p^.readaccesssym,sym);
                       end;
                  end;
                if (idtoken=_WRITE) then
                  begin
                     if assigned(p^.writeaccesssym) then
                       deletepropsymlist(p^.writeaccesssym);
                     p^.writeaccesssym:=nil;
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
                                 (pvarsym(sym)^.definition^.deftype=recorddef)) do
                           begin
                             addpropsymlist(p^.writeaccesssym,sym);
                             consume(_POINT);
                             getsymonlyin(precorddef(pvarsym(sym)^.definition)^.symtable,pattern);
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
                                 p^.writeaccessdef:=pp;
                              end;
                            varsym :
                              begin
                                 if not(propertyparas^.empty) or
                                    not(is_equal(pvarsym(sym)^.definition,p^.proptype)) then
                                   Message(parser_e_ill_property_access_sym);
                              end
                            else
                              Message(parser_e_ill_property_access_sym);
                          end;
                          addpropsymlist(p^.writeaccesssym,sym);
                       end;
                  end;
                include(p^.propoptions,ppo_stored);
                if (idtoken=_STORED) then
                  begin
                     consume(_STORED);
                     if assigned(p^.storedsym) then
                       deletepropsymlist(p^.storedsym);
                     p^.storedsym:=nil;
                     p^.storeddef:=nil;
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
                                            (pvarsym(sym)^.definition^.deftype=recorddef)) do
                                      begin
                                        addpropsymlist(p^.storedsym,sym);
                                        consume(_POINT);
                                        getsymonlyin(precorddef(pvarsym(sym)^.definition)^.symtable,pattern);
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
                                              not(is_equal(pp^.retdef,booldef)) then
                                             Message(parser_e_ill_property_storage_sym);
                                           p^.storeddef:=pp;
                                         end;
                                       varsym :
                                         begin
                                           if not(propertyparas^.empty) or
                                              not(is_equal(pvarsym(sym)^.definition,booldef)) then
                                             Message(parser_e_stored_property_must_be_boolean);
                                         end;
                                       else
                                         Message(parser_e_ill_property_storage_sym);
                                     end;
                                     addpropsymlist(p^.storedsym,sym);
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
                     if not(is_ordinal(p^.proptype) or
                         is_64bitint(p^.proptype) or
                       ((p^.proptype^.deftype=setdef) and
                        (psetdef(p^.proptype)^.settype=smallset)
                       ) or
                       not(propertyparas^.empty)
                       ) then
                       Message(parser_e_property_cant_have_a_default_value);
                     { Get the result of the default, the firstpass is
                       needed to support values like -1 }
                     pt:=comp_expr(true);
                     do_firstpass(pt);
                     if p^.proptype^.deftype=setdef then
                       begin
{$ifndef newcg}
                         {!!!!!!!!!!}
                         arrayconstructor_to_set(pt);
{$endif newcg}
                         do_firstpass(pt);
                       end;
                     pt:=gentypeconvnode(pt,p^.proptype);
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
                     p2:=search_default_property(aktclass);
                     if assigned(p2) then
                       message1(parser_e_only_one_default_property,
                         pobjectdef(p2^.owner^.defowner)^.objname^)
                     else
                       begin
{$ifdef INCLUDEOK}
                          include(p^.propoptions,ppo_defaultproperty);
{$else}
                          p^.propoptions:=p^.propoptions+[ppo_defaultproperty];
{$endif}
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
           aktprocsym^.definition^.retdef:=voiddef;
        end;

      var
         hs      : string;
         pcrd       : pclassrefdef;
         hp1    : pdef;
         oldprocinfo : pprocinfo;
         oldprocsym : pprocsym;
         oldparse_only : boolean;
         methodnametable,intmessagetable,
         strmessagetable,classnamelabel : pasmlabel;
         storetypecanbeforward : boolean;
         vmtlist : taasmoutput;

      begin
         {Nowadays aktprocsym may already have a value, so we need to save
          it.}
         oldprocsym:=aktprocsym;
         { forward is resolved }
         if assigned(fd) then
{$ifdef INCLUDEOK}
           exclude(fd^.objectoptions,oo_is_forward);
{$else}
           fd^.objectoptions:=fd^.objectoptions-[oo_is_forward];
{$endif}
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
                   hp1:=single_type(hs,typecanbeforward);

                   { accept hp1, if is a forward def or a class }
                   if (hp1^.deftype=forwarddef) or
                      ((hp1^.deftype=objectdef) and pobjectdef(hp1)^.is_class) then
                     begin
                        pcrd:=new(pclassrefdef,init(hp1));
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
              childof:=pobjectdef(id_type(pattern,false));
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
         new(procinfo);
         fillchar(procinfo^,sizeof(tprocinfo),0);
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
                 if aktclass^.needs_inittable then
                   datasegment^.concat(new(pai_const_symbol,init(aktclass^.get_inittable_label)))
                 else
                   datasegment^.concat(new(pai_const,init_32bit(0)));
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
         dispose(procinfo);
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
    function read_type(const name : stringid) : pdef;
      var
        pt : ptree;
        hp1,p : pdef;
        aufdef : penumdef;
        aufsym : penumsym;
        ap : parraydef;
        s : stringid;
        l,v : longint;
        oldaktpackrecords : tpackrecords;
        hs : string;

        procedure expr_type;
        var
           pt1,pt2 : ptree;
        begin
           { use of current parsed object ? }
           if (token=_ID) and (testcurobject=2) and (curobjectname=pattern) then
             begin
                consume(_ID);
                p:=aktobjectdef;
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
                           enumdef : p:=new(penumdef,init_subrange(penumdef(pt1^.resulttype),pt1^.value,pt2^.value));
                            orddef : begin
                                       if is_char(pt1^.resulttype) then
                                         p:=new(porddef,init(uchar,pt1^.value,pt2^.value))
                                       else
                                        if is_boolean(pt1^.resulttype) then
                                         p:=new(porddef,init(bool8bit,pt1^.value,pt2^.value))
                                       else
                                        p:=new(porddef,init(uauto,pt1^.value,pt2^.value));
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
                   p:=pt1^.resulttype;
                   readtypesym:=pt1^.typenodesym;
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
           p:=nil;
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
             if p=nil then
              begin
                ap:=new(parraydef,init(lowval,highval,arraytype));
                p:=ap;
              end
             else
              begin
                ap^.definition:=new(parraydef,init(lowval,highval,arraytype));
                ap:=parraydef(ap^.definition);
              end;

             if token=_COMMA then
               consume(_COMMA)
             else
               break;
           until false;
           consume(_RECKKLAMMER);
           consume(_OF);
           hp1:=read_type('');
           { if no error, set element type }
           if assigned(ap) then
             ap^.definition:=hp1;
        end;

      begin
         readtypesym:=nil;
         p:=nil;
         case token of
            _STRING,_FILE:
              begin
                p:=single_type(hs,false);
                readtypesym:=nil;
              end;
           _LKLAMMER:
              begin
                 consume(_LKLAMMER);
                 { allow negativ value_str }
                 l:=-1;
                 aufsym := Nil;
                 aufdef:=new(penumdef,init);
                 repeat
                   s:=pattern;
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
                   constsymtable^.insert(new(penumsym,init(s,aufdef,l)));
                   if token=_COMMA then
                     consume(_COMMA)
                   else
                     break;
                 until false;
                 {aufdef^.max:=l;
                 if we allow unordered enums
                 this can be wrong
                 min and max are now set in tenumsym.init PM }
                 p:=aufdef;
                 consume(_RKLAMMER);
                readtypesym:=nil;
              end;
            _ARRAY:
              begin
                array_dec;
                readtypesym:=nil;
              end;
            _SET:
              begin
                consume(_SET);
                consume(_OF);
                hp1:=read_type('');
                if assigned(hp1) then
                 begin
                   case hp1^.deftype of
                     { don't forget that min can be negativ  PM }
                     enumdef :
                       if penumdef(hp1)^.min>=0 then
                        p:=new(psetdef,init(hp1,penumdef(hp1)^.max))
                       else
                        Message(sym_e_ill_type_decl_set);
                     orddef :
                       begin
                         case porddef(hp1)^.typ of
                           uchar :
                             p:=new(psetdef,init(hp1,255));
                           u8bit,u16bit,u32bit,
                           s8bit,s16bit,s32bit :
                             begin
                               if (porddef(hp1)^.low>=0) then
                                p:=new(psetdef,init(hp1,porddef(hp1)^.high))
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
                 p:=generrordef;
                readtypesym:=nil;
              end;
           _CARET:
              begin
                consume(_CARET);
                hp1:=single_type(hs,typecanbeforward);
                p:=new(ppointerdef,init(hp1));
                readtypesym:=nil;
              end;
            _RECORD:
              begin
                p:=record_dec;
                readtypesym:=nil;
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
                      p:=object_dec(name,nil)
                    else
                      p:=record_dec;
                    aktpackrecords:=oldaktpackrecords;
                  end;
                readtypesym:=nil;
              end;
            _CLASS,
            _OBJECT:
              begin
                p:=object_dec(name,nil);
                readtypesym:=nil;
              end;
            _PROCEDURE:
              begin
                consume(_PROCEDURE);
                p:=new(pprocvardef,init);
                if token=_LKLAMMER then
                 parameter_dec(pprocvardef(p));
                if token=_OF then
                  begin
                    consume(_OF);
                    consume(_OBJECT);
{$ifdef INCLUDEOK}
                    include(pprocvardef(p)^.procoptions,po_methodpointer);
{$else}
                    pprocvardef(p)^.procoptions:=pprocvardef(p)^.procoptions+[po_methodpointer];
{$endif}
                  end;
                readtypesym:=nil;
              end;
            _FUNCTION:
              begin
                consume(_FUNCTION);
                p:=new(pprocvardef,init);
                if token=_LKLAMMER then
                 parameter_dec(pprocvardef(p));
                consume(_COLON);
                pprocvardef(p)^.retdef:=single_type(hs,false);
                if token=_OF then
                  begin
                    consume(_OF);
                    consume(_OBJECT);
{$ifdef INCLUDEOK}
                    include(pprocvardef(p)^.procoptions,po_methodpointer);
{$else}
                    pprocvardef(p)^.procoptions:=pprocvardef(p)^.procoptions+[po_methodpointer];
{$endif}
                  end;
                readtypesym:=nil;
              end;
            else
              expr_type;
         end;
         if p=nil then
          p:=generrordef;
         read_type:=p;
      end;

end.
{
  $Log$
  Revision 1.5  1999-10-27 16:04:06  peter
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
