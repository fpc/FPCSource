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


    procedure const_dec;
      var
         name : stringid;
         p : ptree;
         def : pdef;
         sym : psym;
         ps : pconstset;
         pd : pbestreal;
{$ifdef USEANSISTRING}

         sp : pstring;
{$endif USEANSISTRING}

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
                           else if p^.resulttype^.deftype=pointerdef then
                             symtablestack^.insert(new(pconstsym,init(name,constord,p^.value,p^.resulttype)))
                           else internalerror(111);
                        end;
                      stringconstn:
                        {values is disposed with p so I need a copy !}
{$ifdef USEANSISTRING}  begin
                           getmem(sp,p^.length+1);
                           move(p^.values^,sp^[1],p^.length);
                           sp^[0]:=chr(p^.length);
                           symtablestack^.insert(new(pconstsym,init(name,conststring,longint(sp),nil)));
                        end;
{$else USEANSISTRING}
                        symtablestack^.insert(new(pconstsym,init(name,conststring,longint(stringdup(p^.values^)),nil)));
{$endif USEANSISTRING}
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
         if not(cs_support_goto in aktmoduleswitches) then
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


    procedure read_var_decs(is_record,is_object:boolean);
    { reads the filed of a record into a        }
    { symtablestack, if record=false            }
    { variants are forbidden, so this procedure }
    { can be used to read object fields         }
    { if absolute is true, ABSOLUTE and file    }
    { types are allowed                         }
    { => the procedure is also used to read     }
    { a sequence of variable declaration        }
      var
         sc : pstringcontainer;
         s : stringid;
         old_block_type : tblock_type;
         filepos : tfileposinfo;
         symdone : boolean;
         { to handle absolute }
         abssym : pabsolutesym;
{$ifdef i386}

         l    : longint;
         code : word;
{$endif i386}
         { c var }
         Csym : pvarsym;
         newtype : ptypesym;
         is_gpc_name,is_cdecl,extern_Csym,export_Csym : boolean;
         C_name : string;
         { case }
         p,casedef : pdef;
         { maxsize contains the max. size of a variant }
         { startvarrec contains the start of the variant part of a record }
         maxsize,startvarrec : longint;
         pt : ptree;
      begin
         old_block_type:=block_type;
         block_type:=bt_type;
         is_gpc_name:=false;
       { Force an expected ID error message }
         if not (token in [ID,_CASE,_END]) then
          consume(ID);
       { read vars }
         while (token=ID) and
           not(is_object and
              ((pattern='PUBLIC') or (pattern='PRIVATE') or
               (pattern='PUBLISHED') or (pattern='PROTECTED'))) do
           begin
             C_name:=orgpattern;
             sc:=idlist;
             consume(COLON);
             if (cs_gpc_compatible in aktmoduleswitches) and
               not(is_record or is_object) and
               (token=ID) and (orgpattern='__asmname__') then
                 begin
                    consume(ID);
                    C_name:=get_stringconst;
                    Is_gpc_name:=true;
                 end;
             p:=read_type('');
             symdone:=false;
             if is_gpc_name then
               begin
                  s:=sc^.get_with_tokeninfo(filepos);
                  if not sc^.empty then
                   Message(parser_e_absolute_only_one_var);
                  dispose(sc,done);
                  Csym:=new(pvarsym,init_C(s,target_os.Cprefix+C_name,p));
                  Csym^.fileinfo:=filepos;
                  Csym^.var_options:=Csym^.var_options or vo_is_external;
                  externals^.concat(new(pai_external,init(Csym^.mangledname,EXT_NEAR)));
                  symtablestack^.insert(Csym);
                  symdone:=true;
               end;
           { check for absolute }
             if not symdone and (token=ID) and
                (pattern='ABSOLUTE') and not(is_record or is_object) then
              begin
                consume(ID);
              { only allowed for one var }
                s:=sc^.get_with_tokeninfo(filepos);
                if not sc^.empty then
                 Message(parser_e_absolute_only_one_var);
                dispose(sc,done);
              { parse the rest }
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
                   abssym^.fileinfo:=filepos;
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
                    abssym^.fileinfo:=filepos;
                    symtablestack^.insert(abssym);
                  end
                else
                { absolute address ?!? }
                 if token=INTCONST then
                  begin
{$ifdef i386}
                    if (target_info.target=target_GO32V2) then
                     begin
                       abssym:=new(pabsolutesym,init(s,p));
                       abssym^.typ:=absolutesym;
                       abssym^.abstyp:=toaddr;
                       abssym^.absseg:=false;
                       abssym^.fileinfo:=filepos;
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
{$endif i386}
                     Message(parser_e_absolute_only_to_var_or_const);
                  end
                else
                 Message(parser_e_absolute_only_to_var_or_const);
                symdone:=true;
              end;
             { for a record there doesn't need to be a ; before the END or ) }
             if not((is_record or is_object) and (token in [_END,RKLAMMER])) then
               consume(SEMICOLON);
             if (p^.deftype=procvardef) and (p^.sym=nil) then
               begin
                  newtype:=new(ptypesym,init('unnamed',p));
                  parse_var_proc_directives(newtype);
                  newtype^.definition:=nil;
                  p^.sym:=nil;
                  dispose(newtype,done);
               end;
             { Check for variable directives }
             if not symdone and (token=ID) then
              begin
                { Check for C Variable declarations }
                if (cs_support_c_var in aktmoduleswitches) and
                   not(is_record or is_object) and
                   ((pattern='EXPORT') or
                    (pattern='EXTERNAL') or
                    (pattern='PUBLIC') or
                    (pattern='CVAR')) then
                 begin
                   { only allowed for one var }
                   s:=sc^.get_with_tokeninfo(filepos);
                   if not sc^.empty then
                    Message(parser_e_absolute_only_one_var);
                   dispose(sc,done);
                   { defaults }
                   is_cdecl:=false;
                   extern_csym:=false;
                   export_Csym:=false;
                   { cdecl }
                   if pattern='CVAR' then
                    begin
                      consume(ID);
                      consume(SEMICOLON);
                      is_cdecl:=true;
                      C_name:=target_os.Cprefix+C_name;
                    end;
                   { external }
                   if pattern='EXTERNAL' then
                    begin
                      consume(ID);
                      extern_csym:=true;
                    end;
                   { export }
                   if (pattern='EXPORT') or (pattern='PUBLIC') then
                    begin
                      consume(ID);
                      if extern_csym then
                       Message(parser_e_not_external_and_export)
                      else
                       export_Csym:=true;
                    end;
                 { external and export need a name after when no cdecl is used }
                   if not is_cdecl then
                    begin
                      if (token=ID) and (pattern='NAME') then
                       consume(ID)
                      else
                       Message(parser_e_name_keyword_expected);
                      C_name:=pattern;
                    { allow also char }
                      if token=CCHAR then
                       consume(CCHAR)
                      else
                       consume(CSTRING);
                    end;
                 { consume the ; when export or external is used }
                   if extern_csym or export_csym then
                    consume(SEMICOLON);
                   { insert in the symtable }
                   Csym:=new(pvarsym,init_C(s,C_name,p));
                   Csym^.fileinfo:=filepos;
                   if export_Csym then
                    inc(Csym^.refs);
                   if extern_Csym then
                    begin
                      Csym^.var_options:=Csym^.var_options or vo_is_external;
                      { correct type ?? }
                      externals^.concat(new(pai_external,init(Csym^.mangledname,EXT_NEAR)));
                    end;
                   symtablestack^.insert(Csym);
                   symdone:=true;
                 end
                else
                 if (is_object) and (cs_static_keyword in aktglobalswitches) and (pattern='STATIC') then
                  begin
                    current_object_option:=current_object_option or sp_static;
                    insert_syms(symtablestack,sc,p);
                    current_object_option:=current_object_option - sp_static;
                    consume(ID);
                    consume(SEMICOLON);
                    symdone:=true;
                  end;
              end;
             { insert it in the symtable, if not done yet }
             if not symdone then
              insert_syms(symtablestack,sc,p);
           end;
       { Check for Case }
         if is_record and (token=_CASE) then
           begin
              maxsize:=0;
              consume(_CASE);
              s:=pattern;
              getsym(s,false);
              { may be only a type: }
              if assigned(srsym) and (srsym^.typ in [typesym,unitsym]) then
                casedef:=read_type('')
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
                  if token=COMMA then
                   consume(COMMA)
                  else
                   break;
                until false;
                consume(COLON);
              { read the vars }
                consume(LKLAMMER);
                if token<>RKLAMMER then
                  read_var_decs(true,false);
                consume(RKLAMMER);
              { calculates maximal variant size }
                maxsize:=max(maxsize,symtablestack^.datasize);
              { the items of the next variant are overlayed }
                symtablestack^.datasize:=startvarrec;
                if (token<>_END) and (token<>RKLAMMER) then
                  consume(SEMICOLON)
                else
                  break;
              until (token=_END) or (token=RKLAMMER);
            { at last set the record size to that of the biggest variant }
              symtablestack^.datasize:=maxsize;
           end;
         block_type:=old_block_type;
      end;


    function stringtype : pdef;
    { reads a string type with optional length }
    { and returns a pointer to the string      }
    { definition                               }
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
             in ansistring mode ?? (PM) Yes!!! (FK) }
          else
            begin
               if cs_ansistrings in aktlocalswitches then
                 d:=new(pstringdef,ansiinit(0))
               else
{$ifndef GDB}
                 d:=new(pstringdef,init(255));
{$else GDB}
                 d:=globaldef('STRING');
{$endif GDB}
            end;
          stringtype:=d;
       end;


    function id_type(var s : string) : pdef;
    { reads a type definition and returns a pointer }
    { to a appropriating pdef, s gets the name of   }
    { the type to allow name mangling               }
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


    function single_type(var s : string) : pdef;
    { reads a string, file type or a type id and returns a name and }
    { pdef                                                          }
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


    function object_dec(const n : stringid;fd : pobjectdef) : pdef;
    { this function parses an object or class declaration }
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

           if (cs_constructor_name in aktglobalswitches) and (aktprocsym^.name<>'INIT') then
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
           if (cs_constructor_name in aktglobalswitches) and (aktprocsym^.name<>'DONE') then
            Message(parser_e_destructorname_must_be_done);
           consume(SEMICOLON);
           if assigned(aktprocsym^.definition^.para1) then
            Message(parser_e_no_paras_for_destructor);
           { no return value }
           aktprocsym^.definition^.retdef:=voiddef;
        end;

      var
         hs         : string;
         pcrd       : pclassrefdef;
         hp1        : pdef;
         oldprocsym : Pprocsym;
         oldparse_only : boolean;
         classnamelabel : plabel;

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
              if assigned(fd) then
                begin
                   { the forward of the child must be resolved to get
                     correct field addresses
                   }
                   if (childof^.options and oo_isforward)<>0 then
                     Message1(parser_forward_declaration_must_be_resolved,childof^.name^);
                   fd^.childof:=childof;
                   aktclass:=fd;
                   { ajust the size, because the child could be also
                     forward defined
                   }
                   aktclass^.publicsyms^.datasize:=
                     aktclass^.publicsyms^.datasize-4+childof^.publicsyms^.datasize;
                end
              else
                aktclass:=new(pobjectdef,init(n,childof));
              consume(RKLAMMER);
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
                        { the forward of the child must be resolved to get
                          correct field addresses
                        }
                        if (childof^.options and oo_isforward)<>0 then
                          Message1(parser_forward_declaration_must_be_resolved,childof^.name^);
                        aktclass:=fd;
                        aktclass^.childof:=childof;
                        { ajust the size, because the child could be also
                          forward defined
                        }
                        aktclass^.publicsyms^.datasize:=
                          aktclass^.publicsyms^.datasize-4+childof^.publicsyms^.datasize;
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

              if (cs_generate_rtti in aktmoduleswitches) or
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
         if (not is_a_class) or (token<>SEMICOLON) then
          begin
          { Parse componenten }
            repeat
              if actmembertype=sp_private then
                aktclass^.options:=aktclass^.options or oo_hasprivate;
              if actmembertype=sp_protected then
                aktclass^.options:=aktclass^.options or oo_hasprotected;
              case token of
               ID : begin
                      if (pattern='PRIVATE') then
                       begin
                         consume(ID);
                         actmembertype:=sp_private;
                         current_object_option:=sp_private;
                       end
                      else
                       if (pattern='PROTECTED') then
                        begin
                          consume(ID);
                          current_object_option:=sp_protected;
                          actmembertype:=sp_protected;
                        end
                      else
                       if (pattern='PUBLIC') then
                        begin
                          consume(ID);
                          current_object_option:=sp_public;
                          actmembertype:=sp_public;
                        end
                      else
                       if (pattern='PUBLISHED') then
                        begin
                          if (aktclass^.options and oo_can_have_published)=0 then
                           Message(parser_e_cant_have_published);
                          consume(ID);
                          current_object_option:=sp_published;
                          actmembertype:=sp_published;
                        end
                      else
                       read_var_decs(false,true);
                    end;
        _PROPERTY : property_dec;
       _PROCEDURE,
        _FUNCTION,
           _CLASS : begin
                      oldparse_only:=parse_only;
                      parse_only:=true;
                      proc_head;
                      parse_only:=oldparse_only;
                      if (token=ID) then
                       begin
                         if (pattern='VIRTUAL') or (pattern='DYNAMIC') then
                          begin
                            if actmembertype=sp_private then
                             Message(parser_w_priv_meth_not_virtual);
                            consume(ID);
                            consume(SEMICOLON);
                            aktprocsym^.definition^.options:=aktprocsym^.definition^.options or povirtualmethod;
                            aktclass^.options:=aktclass^.options or oo_hasvirtual;
                          end
                         else
                          if (pattern='OVERRIDE') then
                           begin
                             consume(ID);
                             consume(SEMICOLON);
                             aktprocsym^.definition^.options:=aktprocsym^.definition^.options or
                               pooverridingmethod or povirtualmethod;
                           end;
                      { Delphi II extension }
                         if (pattern='ABSTRACT') then
                          begin
                            consume(ID);
                            consume(SEMICOLON);
                            if (aktprocsym^.definition^.options and povirtualmethod)<>0 then
                             aktprocsym^.definition^.options:=aktprocsym^.definition^.options or poabstractmethod
                            else
                             Message(parser_e_only_virtual_methods_abstract);
                          { the method is defined }
                            aktprocsym^.definition^.forwarddef:=false;
                          end;
                         if (cs_static_keyword in aktglobalswitches) and (pattern='STATIC') then
                          begin
                            consume(ID);
                            consume(SEMICOLON);
                            aktprocsym^.properties:=aktprocsym^.properties or sp_static;
                            aktprocsym^.definition^.options:=aktprocsym^.definition^.options or postaticmethod;
                          end;
                       end;
                    end;
     _CONSTRUCTOR : begin
                      if actmembertype<>sp_public then
                        Message(parser_w_constructor_should_be_public);
                      oldparse_only:=parse_only;
                      parse_only:=true;
                      constructor_head;
                      parse_only:=oldparse_only;
                      if (token=ID) then
                       begin
                         if (pattern='VIRTUAL') or (pattern='DYNAMIC') then
                          begin
                            consume(ID);
                            consume(SEMICOLON);
                            if not(aktclass^.isclass) then
                             Message(parser_e_constructor_cannot_be_not_virtual)
                            else
                             begin
                               aktprocsym^.definition^.options:=aktprocsym^.definition^.options or povirtualmethod;
                               aktclass^.options:=aktclass^.options or oo_hasvirtual;
                             end;
                          end
                         else
                          if (pattern='OVERRIDE') then
                           begin
                             consume(ID);
                             consume(SEMICOLON);
                             if (aktclass^.options and oois_class=0) then
                              Message(parser_e_constructor_cannot_be_not_virtual)
                             else
                              aktprocsym^.definition^.options:=aktprocsym^.definition^.options or
                                pooverridingmethod or povirtualmethod;
                          end;
                       end;
                    end;
      _DESTRUCTOR : begin
                      if there_is_a_destructor then
                       Message(parser_n_only_one_destructor);
                      there_is_a_destructor:=true;
                      if actmembertype<>sp_public then
                       Message(parser_w_destructor_should_be_public);
                      oldparse_only:=parse_only;
                      parse_only:=true;
                      destructor_head;
                      parse_only:=oldparse_only;
                      if (token=ID) then
                       begin
                         if (pattern='VIRTUAL') or (pattern='DYNAMIC') then
                          begin
                            consume(ID);
                            consume(SEMICOLON);
                            aktprocsym^.definition^.options:=aktprocsym^.definition^.options or povirtualmethod;
                          end
                         else
                          if (pattern='OVERRIDE') then
                           begin
                             consume(ID);
                             consume(SEMICOLON);
                             aktprocsym^.definition^.options:=aktprocsym^.definition^.options or
                               pooverridingmethod or povirtualmethod;
                           end;
                       end;
                    end;
             _END : begin
                      consume(_END);
                      break;
                    end;
              else
               consume(ID); { Give a ident expected message, like tp7 }
              end;
            until false;
            current_object_option:=sp_public;
          end;
         testcurobject:=0;
         curobjectname:='';

         if (cs_smartlink in aktmoduleswitches) then
           datasegment^.concat(new(pai_cut,init));
         { write extended info for classes }
         if is_a_class then
           begin
              { write class name }
              getlabel(classnamelabel);
              datasegment^.concat(new(pai_label,init(classnamelabel)));
              datasegment^.concat(new(pai_const,init_8bit(length(aktclass^.name^))));
              datasegment^.concat(new(pai_string,init(aktclass^.name^)));

              { interface table }
              datasegment^.concat(new(pai_const,init_32bit(0)));
              { auto table }
              datasegment^.concat(new(pai_const,init_32bit(0)));

              { inittable for con-/destruction }
              if aktclass^.needs_inittable then
                datasegment^.concat(new(pai_const,init_symbol(strpnew(lab2str(aktclass^.get_inittable_label)))))
              else
                datasegment^.concat(new(pai_const,init_32bit(0)));

              { pointer to type info of published section }
              datasegment^.concat(new(pai_const,init_symbol(strpnew(lab2str(aktclass^.get_rtti_label)))));

              { pointer to field table }
              datasegment^.concat(new(pai_const,init_32bit(0)));
              { pointer to method table }
              datasegment^.concat(new(pai_const,init_32bit(0)));
              { pointer to dynamic table }
              datasegment^.concat(new(pai_const,init_32bit(0)));
              { pointer to class name string }
              datasegment^.concat(new(pai_const,init_symbol(strpnew(lab2str(classnamelabel)))));
           end;
{$ifdef GDB}
         { generate the VMT }
         if cs_debuginfo in aktmoduleswitches then
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

    { search in symtablestack used, but not defined type }
    procedure testforward_types(p : psym);{$ifndef FPC}far;{$endif}

      begin
         if (p^.typ=typesym) and ((p^.properties and sp_forwarddef)<>0) then
           Message(sym_e_type_id_not_defined);
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
           p:=nil;
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
                if (pt1^.treetype<>ordconstn) or (pt2^.treetype<>ordconstn) then
                  Message(sym_e_error_in_type_def)
                else
                  begin
                  { Check bounds }
                    if pt2^.value<pt1^.value then
                      Message(cg_e_upper_lower_than_lower)
                    else
                     begin
                     { is one an enum ? }
                       if (pt1^.resulttype^.deftype=enumdef) or (pt2^.resulttype^.deftype=enumdef) then
                        begin
                        { both must be the have the same (enumdef) definition, else its a type mismatch }
                          if (pt1^.resulttype=pt2^.resulttype) then
                            p:=new(penumdef,init_subrange(penumdef(pt1^.resulttype),pt1^.value,pt2^.value))
                          else
                            Message(sym_e_type_mismatch);
                        end
                       else
                        begin
                        { both must be are orddefs, create an uauto orddef }
                          p:=new(porddef,init(uauto,pt1^.value,pt2^.value));
                        end;
                     end;
                  end;
                disposetree(pt2);
             end;
           disposetree(pt1);
        end;

      var
         pt : ptree;

      procedure array_dec;
        var
          lowval,
          highval   : longint;
          arraytype : pdef;
        begin
           consume(_ARRAY);
           consume(LECKKLAMMER);
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
               enumdef : begin
                           lowval:=penumdef(pt^.resulttype)^.min;
                           highval:=penumdef(pt^.resulttype)^.max;
                           arraytype:=pt^.resulttype;
                         end;
                orddef : begin
                           case porddef(pt^.resulttype)^.typ of
                            s8bit,u8bit,
                          s16bit,u16bit,
                                 s32bit : begin
                                            lowval:=porddef(pt^.resulttype)^.low;
                                            highval:=porddef(pt^.resulttype)^.high;
                                            arraytype:=pt^.resulttype;
                                          end;

                               bool8bit,
                              bool16bit,
                              bool32bit : begin
                                            lowval:=0;
                                            highval:=1;
                                            arraytype:=pt^.resulttype;
                                          end;
                                  uchar : begin
                                            lowval:=0;
                                            highval:=255;
                                            arraytype:=pt^.resulttype;
                                          end;
                           else
                             Message(sym_e_error_in_type_def);
                           end;
                         end;
                 else
                   Message(sym_e_error_in_type_def);
                 end
               end

             else
               begin
                  do_firstpass(pt);

                  if (pt^.treetype<>rangen) or
                     (pt^.left^.treetype<>ordconstn) then
                    Message(sym_e_error_in_type_def)
                  else
                    begin
{$ifndef GDB}
                      if pt^.right^.resulttype=pdef(s32bitdef) then
                        pt^.right^.resulttype:=new(porddef,init(s32bit,$80000000,$7fffffff));
{$endif GDB}
                      lowval:=pt^.left^.value;
                      highval:=pt^.right^.value;
                      arraytype:=pt^.right^.resulttype;
                    end;

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

             if token=COMMA then
               consume(COMMA)
             else
               break;
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
                 { allow negativ values }
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
                 {aufdef^.max:=l;
                 if we allow unordered enums
                 this can be wrong
                 min and max are now set in tenumsym.init PM }
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
                    { don't forget that min can be negativ  PM }
                    enumdef : if penumdef(hp1)^.min>=0 then
                                p:=new(psetdef,init(hp1,penumdef(hp1)^.max))
                              else
                                Message(sym_e_ill_type_decl_set);
                    orddef : begin
                                  case porddef(hp1)^.typ of
                                     uchar : p:=new(psetdef,init(hp1,255));
                                     u8bit,s8bit,u16bit,s16bit,s32bit :
                                       begin
                                          if (porddef(hp1)^.low>=0) then
                                            p:=new(psetdef,init(hp1,porddef(hp1)^.high))
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


    { reads a type declaration to the symbol table }
    procedure type_dec;

      var
         typename : stringid;
         newtype : ptypesym;
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
                newtype:=new(ptypesym,init(typename,newdef));
                symtablestack^.insert(newtype);
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
                     newtype:=ptypesym(srsym);
                  end
                else
                  begin
                     newtype:=new(ptypesym,init(typename,read_type(typename)));
                     { load newtype with the new pointer to the inserted type

                       because it can be an already defined forwarded type !! }
                     newtype:=ptypesym(symtablestack^.insert(newtype));
                  end;
             end;
           consume(SEMICOLON);
           if assigned(newtype^.definition) and (newtype^.definition^.deftype=procvardef) then
             parse_var_proc_directives(newtype);
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


    procedure var_dec;
    { parses varaible declarations and inserts them in }
    { the top symbol table of symtablestack            }
      begin
        consume(_VAR);
        read_var_decs(false,false);
      end;


    procedure Not_supported_for_inline(t : ttoken);

      begin
         if assigned(aktprocsym) and
            ((aktprocsym^.definition^.options and poinline)<>0) then
           Begin
              Message1(parser_w_not_supported_for_inline,tokenstring(t));
              Message(parser_w_inlining_disabled);
              aktprocsym^.definition^.options:= aktprocsym^.definition^.options and not poinline;
           End;
      end;


    procedure read_declarations(islibrary : boolean);

      begin
         repeat
           case token of
              _LABEL:
                begin
                   Not_supported_for_inline(token);
                   label_dec;
                end;
              _CONST:
                begin
                   Not_supported_for_inline(token);
                   const_dec;
                end;
              _TYPE:
                begin
                   Not_supported_for_inline(token);
                   type_dec;
                end;
              _VAR:
                var_dec;
              _CONSTRUCTOR,_DESTRUCTOR,
              _FUNCTION,_PROCEDURE,_OPERATOR,_CLASS:
                begin
                   Not_supported_for_inline(token);
                   unter_dec;
                end;
              _EXPORTS:
                begin
                   { here we should be at lexlevel 1, no ? PM }
                   Not_supported_for_inline(token);
                   if islibrary then
                     read_exports
                   else
                     break;
                end
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
         _FUNCTION,
        _PROCEDURE,
         _OPERATOR : unter_dec;
           else
             break;
           end;
         until false;
         dec(lexlevel);
      end;

end.
{
  $Log$
  Revision 1.47  1998-09-03 16:03:18  florian
    + rtti generation
    * init table generation changed

  Revision 1.46  1998/09/01 17:39:48  peter
    + internal constant functions

  Revision 1.45  1998/08/31 12:20:28  peter
    * fixed array_dec when unknown type was used

  Revision 1.44  1998/08/28 10:57:01  peter
    * removed warnings

  Revision 1.43  1998/08/25 13:09:25  pierre
    * corrected mangling sheme :
      cvar add Cprefix to the mixed case name whereas
      export or public use direct name

  Revision 1.42  1998/08/25 12:42:41  pierre
    * CDECL changed to CVAR for variables
      specifications are read in structures also
    + started adding GPC compatibility mode ( option  -Sp)
    * names changed to lowercase

  Revision 1.41  1998/08/23 21:04:36  florian
    + rtti generation for classes added
    + new/dispose do now also a call to INITIALIZE/FINALIZE, if necessaray

  Revision 1.40  1998/08/21 15:48:58  pierre
    * more cdecl chagnes
      - better line info
      - changes the definition options of a procvar
        if it is a unnamed type

  Revision 1.39  1998/08/19 00:42:40  peter
    + subrange types for enums
    + checking for bounds type with ranges

  Revision 1.38  1998/08/12 19:20:39  peter
    + public is the same as export for c_vars
    * a exported/public c_var incs now the refcount

  Revision 1.37  1998/08/11 15:31:38  peter
    * write extended to ppu file
    * new version 0.99.7

  Revision 1.36  1998/08/10 14:50:09  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.35  1998/07/26 21:59:00  florian
   + better support for switch $H
   + index access to ansi strings added
   + assigment of data (records/arrays) containing ansi strings

  Revision 1.34  1998/07/20 22:17:15  florian
    * hex constants in numeric char (#$54#$43 ...) are now allowed
    * there was a bug in record_var_dec which prevents the used
      of nested variant records (for example drivers.tevent of tv)

  Revision 1.33  1998/07/18 17:11:11  florian
    + ansi string constants fixed
    + switch $H partial implemented

  Revision 1.32  1998/07/14 21:46:50  peter
    * updated messages file

  Revision 1.31  1998/07/14 14:46:53  peter
    * released NEWINPUT

  Revision 1.30  1998/07/10 00:00:00  peter
    * fixed ttypesym bug finally
    * fileinfo in the symtable and better using for unused vars

  Revision 1.29  1998/06/25 14:04:21  peter
    + internal inc/dec

  Revision 1.28  1998/06/24 12:26:45  peter
    * stricter var parsing like tp7 and some optimizes with directive
      parsing

  Revision 1.27  1998/06/12 16:15:34  pierre
    * external name 'C_var';
      export name 'intern_C_var';
      cdecl;
      cdecl;external;
      are now supported only with -Sv switch

  Revision 1.25  1998/06/09 16:01:45  pierre
    + added procedure directive parsing for procvars
      (accepted are popstack cdecl and pascal)
    + added C vars with the following syntax
      var C calias 'true_c_name';(can be followed by external)
      reason is that you must add the Cprefix

      which is target dependent

  Revision 1.24  1998/06/05 14:37:32  pierre
    * fixes for inline for operators
    * inline procedure more correctly restricted

  Revision 1.23  1998/06/04 23:51:50  peter
    * m68k compiles
    + .def file creation moved to gendef.pas so it could also be used
      for win32

  Revision 1.22  1998/06/03 22:48:59  peter
    + wordbool,longbool
    * rename bis,von -> high,low
    * moved some systemunit loading/creating to psystem.pas

  Revision 1.21  1998/06/03 22:14:19  florian
    * problem with sizes of classes fixed (if the anchestor was declared
      forward, the compiler doesn't update the child classes size)

  Revision 1.20  1998/05/28 14:35:54  peter
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
