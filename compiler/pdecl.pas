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
      globtype,tokens,globals,symtable;

    var
       { pointer to the last read type symbol, (for "forward" }
       { types)                                        }
       lasttypesym : ptypesym;
       readtypesym : ptypesym; { ttypesym read by read_type }

       { hack, which allows to use the current parsed }
       { object type as function argument type  }
       testcurobject : byte;
       curobjectname : stringid;

    { reads a string type with optional length }
    { and returns a pointer to the string      }
    { definition                               }
    function stringtype : pdef;

    { reads a string, file type or a type id and returns a name and }
    { pdef                                                        }
    function single_type(var s : string) : pdef;

    { reads the declaration blocks }
    procedure read_declarations(islibrary : boolean);

    { reads declarations in the interface part of a unit }
    procedure read_interface_declarations;

  implementation

    uses
       cobjects,scanner,
       symconst,aasm,tree,pass_1,strings,
       files,types,verbose,systems,import,
       cpubase
{$ifndef newcg}
       ,tccnv
{$endif newcg}
{$ifdef GDB}
       ,gdb
{$endif GDB}
       { parser specific stuff }
       ,pbase,ptconst,pexpr,psub,pexports
       { processor specific stuff }
       { codegen }
       ,hcodegen,hcgdata
       ;

    function read_type(const name : stringid) : pdef;forward;

    { search in symtablestack used, but not defined type }
    procedure testforward_type(p : pnamedindexobject);{$ifndef FPC}far;{$endif}
      var
        reaktvarsymtable : psymtable;
        oldaktfilepos : tfileposinfo;
      begin
         if not(psym(p)^.typ=typesym) then
          exit;
         if (sp_forwarddef in psym(p)^.symoptions) then
           begin
             oldaktfilepos:=aktfilepos;
             aktfilepos:=psym(p)^.fileinfo;
             Message1(sym_e_forward_type_not_resolved,p^.name);
             aktfilepos:=oldaktfilepos;
             { try to recover }
             ptypesym(p)^.definition:=generrordef;
{$ifdef INCLUDEOK}
             exclude(psym(p)^.symoptions,sp_forwarddef);
{$else}
             psym(p)^.symoptions:=psym(p)^.symoptions-[sp_forwarddef];
{$endif}
           end
         else
          if (ptypesym(p)^.definition^.deftype in [recorddef,objectdef]) then
           begin
             if (ptypesym(p)^.definition^.deftype=recorddef) then
               reaktvarsymtable:=precorddef(ptypesym(p)^.definition)^.symtable
             else
               reaktvarsymtable:=pobjectdef(ptypesym(p)^.definition)^.symtable;
             reaktvarsymtable^.foreach({$ifndef TP}@{$endif}testforward_type);
           end;
      end;


    procedure const_dec;
      var
         name : stringid;
         p : ptree;
         def : pdef;
         sym : psym;
         storetokenpos,filepos : tfileposinfo;
         old_block_type : tblock_type;
         ps : pconstset;
         pd : pbestreal;
         sp : pchar;
      begin
         consume(_CONST);
         old_block_type:=block_type;
         block_type:=bt_const;
         repeat
           name:=pattern;
           filepos:=tokenpos;
           consume(_ID);
           case token of
             _EQUAL:
                begin
                   consume(_EQUAL);
                   p:=comp_expr(true);
                   do_firstpass(p);
                   storetokenpos:=tokenpos;
                   tokenpos:=filepos;
                   case p^.treetype of
                      ordconstn:
                        begin
                           if is_constintnode(p) then
                             symtablestack^.insert(new(pconstsym,init_def(name,constint,p^.value,nil)))
                           else if is_constcharnode(p) then
                             symtablestack^.insert(new(pconstsym,init_def(name,constchar,p^.value,nil)))
                           else if is_constboolnode(p) then
                             symtablestack^.insert(new(pconstsym,init_def(name,constbool,p^.value,nil)))
                           else if p^.resulttype^.deftype=enumdef then
                             symtablestack^.insert(new(pconstsym,init_def(name,constord,p^.value,p^.resulttype)))
                           else if p^.resulttype^.deftype=pointerdef then
                             symtablestack^.insert(new(pconstsym,init_def(name,constord,p^.value,p^.resulttype)))
                           else internalerror(111);
                        end;
                      stringconstn:
                        begin
                           getmem(sp,p^.length+1);
                           move(p^.value_str^,sp^,p^.length+1);
                           symtablestack^.insert(new(pconstsym,init_string(name,conststring,sp,p^.length)));
                        end;
                      realconstn :
                        begin
                           new(pd);
                           pd^:=p^.value_real;
                           symtablestack^.insert(new(pconstsym,init(name,constreal,longint(pd))));
                        end;
                      setconstn :
                        begin
                          new(ps);
                          ps^:=p^.value_set^;
                          symtablestack^.insert(new(pconstsym,init_def(name,constset,longint(ps),p^.resulttype)));
                        end;
                      niln :
                        begin
                          symtablestack^.insert(new(pconstsym,init_def(name,constnil,0,p^.resulttype)));
                        end;
                      else
                        Message(cg_e_illegal_expression);
                   end;
                   tokenpos:=storetokenpos;
                   consume(_SEMICOLON);
                   disposetree(p);
                end;
             _COLON:
                begin
                   { set the blocktype first so a consume also supports a
                     caret, to support const s : ^string = nil }
                   block_type:=bt_type;
                   consume(_COLON);
                   ignore_equal:=true;
                   def:=read_type('');
                   ignore_equal:=false;
                   block_type:=bt_const;
                   storetokenpos:=tokenpos;
                   tokenpos:=filepos;
{$ifdef DELPHI_CONST_IN_RODATA}
                   if m_delphi in aktmodeswitches then
                     begin
                       if assigned(readtypesym) then
                        sym:=new(ptypedconstsym,initsym(name,readtypesym,true))
                       else
                        sym:=new(ptypedconstsym,init(name,def,true))
                     end
                   else
{$endif DELPHI_CONST_IN_RODATA}
                     begin
                       if assigned(readtypesym) then
                        sym:=new(ptypedconstsym,initsym(name,readtypesym,false))
                       else
                        sym:=new(ptypedconstsym,init(name,def,false))
                     end;
                   tokenpos:=storetokenpos;
                   symtablestack^.insert(sym);
                   consume(_EQUAL);
{$ifdef DELPHI_CONST_IN_RODATA}
                   if m_delphi in aktmodeswitches then
                     readtypedconst(def,ptypedconstsym(sym),true)
                   else
{$endif DELPHI_CONST_IN_RODATA}
                     readtypedconst(def,ptypedconstsym(sym),false);
                   consume(_SEMICOLON);
                end;
              else consume(_EQUAL);
           end;
         until token<>_ID;
         block_type:=old_block_type;
      end;

    procedure label_dec;

      var
         hl : pasmlabel;

      begin
         consume(_LABEL);
         if not(cs_support_goto in aktmoduleswitches) then
           Message(sym_e_goto_and_label_not_supported);
         repeat
           if not(token in [_ID,_INTCONST]) then
             consume(_ID)
           else
             begin
                getlabel(hl);
                symtablestack^.insert(new(plabelsym,init(pattern,hl)));
                consume(token);
             end;
           if token<>_SEMICOLON then consume(_COMMA);
         until not(token in [_ID,_INTCONST]);
         consume(_SEMICOLON);
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

      procedure insert_syms(st : psymtable;sc : pstringcontainer;def : pdef;sym:ptypesym;is_threadvar : boolean);
      { inserts the symbols of sc in st with def as definition or sym as ptypesym, sc is disposed }
        var
           s : string;
           filepos : tfileposinfo;
           ss : pvarsym;
        begin
           { can't have a definition and ttypesym }
           if assigned(def) and assigned(sym) then
            internalerror(5438257);
           filepos:=tokenpos;
           while not sc^.empty do
             begin
                s:=sc^.get_with_tokeninfo(tokenpos);
                if assigned(sym) then
                 ss:=new(pvarsym,initsym(s,sym))
                else
                 ss:=new(pvarsym,init(s,def));
                if is_threadvar then
{$ifdef INCLUDEOK}
                  include(ss^.varoptions,vo_is_thread_var);
{$else}
                  ss^.varoptions:=ss^.varoptions+[vo_is_thread_var];
{$endif}
                st^.insert(ss);
                { static data fields are inserted in the globalsymtable }
                if (st^.symtabletype=objectsymtable) and
                   (sp_static in current_object_option) then
                  begin
                     s:=lower(st^.name^)+'_'+s;
                     st^.defowner^.owner^.insert(new(pvarsym,init(s,def)));
                  end;
             end;
           dispose(sc,done);
           tokenpos:=filepos;
        end;

      var
         sc : pstringcontainer;
         s : stringid;
         old_block_type : tblock_type;
         declarepos,storetokenpos : tfileposinfo;
         symdone : boolean;
         { to handle absolute }
         abssym : pabsolutesym;
         l    : longint;
         code : integer;
         { c var }
         newtype : ptypesym;
         is_dll,
         is_gpc_name,is_cdecl,extern_aktvarsym,export_aktvarsym : boolean;
         dll_name,
         C_name : string;
         { case }
         p,casedef : pdef;
         { Delphi initialized vars }
         pconstsym : ptypedconstsym;
         { maxsize contains the max. size of a variant }
         { startvarrec contains the start of the variant part of a record }
         maxsize,startvarrec : longint;
         pt : ptree;
      begin
         old_block_type:=block_type;
         block_type:=bt_type;
         is_gpc_name:=false;
         { Force an expected ID error message }
         if not (token in [_ID,_CASE,_END]) then
          consume(_ID);
         { read vars }
         while (token=_ID) and
               not(is_object and (idtoken in [_PUBLIC,_PRIVATE,_PUBLISHED,_PROTECTED])) do
           begin
             C_name:=orgpattern;
             sc:=idlist;
             consume(_COLON);
             if (m_gpc in aktmodeswitches) and
                not(is_record or is_object or is_threadvar) and
                (token=_ID) and (orgpattern='__asmname__') then
               begin
                 consume(_ID);
                 C_name:=pattern;
                 if token=_CCHAR then
                  consume(_CCHAR)
                 else
                  consume(_CSTRING);
                 Is_gpc_name:=true;
               end;
             { this is needed for Delphi mode at least
               but should be OK for all modes !! (PM) }
             ignore_equal:=true;
             p:=read_type('');
             if (variantrecordlevel>0) and p^.needs_inittable then
               Message(parser_e_cant_use_inittable_here);
             ignore_equal:=false;
             symdone:=false;
             if is_gpc_name then
               begin
                  storetokenpos:=tokenpos;
                  s:=sc^.get_with_tokeninfo(tokenpos);
                  if not sc^.empty then
                   Message(parser_e_absolute_only_one_var);
                  dispose(sc,done);
                  aktvarsym:=new(pvarsym,init_C(s,target_os.Cprefix+C_name,p));
{$ifdef INCLUDEOK}
                  include(aktvarsym^.varoptions,vo_is_external);
{$else}
                  aktvarsym^.varoptions:=aktvarsym^.varoptions+[vo_is_external];
{$endif}
                  symtablestack^.insert(aktvarsym);
                  tokenpos:=storetokenpos;
                  symdone:=true;
               end;
             { check for absolute }
             if not symdone and
                (idtoken=_ABSOLUTE) and not(is_record or is_object or is_threadvar) then
              begin
                consume(_ABSOLUTE);
                { only allowed for one var }
                s:=sc^.get_with_tokeninfo(declarepos);
                if not sc^.empty then
                 Message(parser_e_absolute_only_one_var);
                dispose(sc,done);
                { parse the rest }
                if token=_ID then
                 begin
                   getsym(pattern,true);
                   consume(_ID);
                   { we should check the result type of srsym }
                   if not (srsym^.typ in [varsym,typedconstsym]) then
                     Message(parser_e_absolute_only_to_var_or_const);
                   storetokenpos:=tokenpos;
                   tokenpos:=declarepos;
                   abssym:=new(pabsolutesym,init(s,p));
                   abssym^.abstyp:=tovar;
                   abssym^.ref:=srsym;
                   symtablestack^.insert(abssym);
                   tokenpos:=storetokenpos;
                 end
                else
                 if (token=_CSTRING) or (token=_CCHAR) then
                  begin
                    storetokenpos:=tokenpos;
                    tokenpos:=declarepos;
                    abssym:=new(pabsolutesym,init(s,p));
                    s:=pattern;
                    consume(token);
                    abssym^.abstyp:=toasm;
                    abssym^.asmname:=stringdup(s);
                    symtablestack^.insert(abssym);
                    tokenpos:=storetokenpos;
                  end
                else
                { absolute address ?!? }
                 if token=_INTCONST then
                  begin
                    if (target_info.target=target_i386_go32v2) then
                     begin
                       storetokenpos:=tokenpos;
                       tokenpos:=declarepos;
                       abssym:=new(pabsolutesym,init(s,p));
                       abssym^.abstyp:=toaddr;
                       abssym^.absseg:=false;
                       s:=pattern;
                       consume(_INTCONST);
                       val(s,abssym^.address,code);
                       if token=_COLON then
                        begin
                          consume(token);
                          s:=pattern;
                          consume(_INTCONST);
                          val(s,l,code);
                          abssym^.address:=abssym^.address shl 4+l;
                          abssym^.absseg:=true;
                        end;
                       symtablestack^.insert(abssym);
                       tokenpos:=storetokenpos;
                     end
                    else
                     Message(parser_e_absolute_only_to_var_or_const);
                  end
                else
                 Message(parser_e_absolute_only_to_var_or_const);
                symdone:=true;
              end;
             { Handling of Delphi typed const = initialized vars ! }
             { When should this be rejected ?
               - in parasymtable
               - in record or object
               - ... (PM) }
             if (m_delphi in aktmodeswitches) and (token=_EQUAL) and
                not (symtablestack^.symtabletype in [parasymtable]) and
                not is_record and not is_object then
               begin
                  storetokenpos:=tokenpos;
                  s:=sc^.get_with_tokeninfo(tokenpos);
                  if not sc^.empty then
                    Message(parser_e_initialized_only_one_var);
                  if assigned(readtypesym) then
                   pconstsym:=new(ptypedconstsym,initsym(s,readtypesym,false))
                  else
                   pconstsym:=new(ptypedconstsym,init(s,p,false));
                  symtablestack^.insert(pconstsym);
                  tokenpos:=storetokenpos;
                  consume(_EQUAL);
                  readtypedconst(p,pconstsym,false);
                  symdone:=true;
               end;
             { for a record there doesn't need to be a ; before the END or ) }
             if not((is_record or is_object) and (token in [_END,_RKLAMMER])) then
               consume(_SEMICOLON);
             { procvar handling }
             if (p^.deftype=procvardef) and (p^.sym=nil) then
               begin
                  newtype:=new(ptypesym,init('unnamed',p));
                  parse_var_proc_directives(newtype);
                  newtype^.definition:=nil;
                  p^.sym:=nil;
                  dispose(newtype,done);
               end;
             { Check for variable directives }
             if not symdone and (token=_ID) then
              begin
                { Check for C Variable declarations }
                if (m_cvar_support in aktmodeswitches) and
                   not(is_record or is_object or is_threadvar) and
                   (idtoken in [_EXPORT,_EXTERNAL,_PUBLIC,_CVAR]) then
                 begin
                   { only allowed for one var }
                   s:=sc^.get_with_tokeninfo(declarepos);
                   if not sc^.empty then
                    Message(parser_e_absolute_only_one_var);
                   dispose(sc,done);
                   { defaults }
                   is_dll:=false;
                   is_cdecl:=false;
                   extern_aktvarsym:=false;
                   export_aktvarsym:=false;
                   { cdecl }
                   if idtoken=_CVAR then
                    begin
                      consume(_CVAR);
                      consume(_SEMICOLON);
                      is_cdecl:=true;
                      C_name:=target_os.Cprefix+C_name;
                    end;
                   { external }
                   if idtoken=_EXTERNAL then
                    begin
                      consume(_EXTERNAL);
                      extern_aktvarsym:=true;
                    end;
                   { export }
                   if idtoken in [_EXPORT,_PUBLIC] then
                    begin
                      consume(_ID);
                      if extern_aktvarsym then
                       Message(parser_e_not_external_and_export)
                      else
                       export_aktvarsym:=true;
                    end;
                   { external and export need a name after when no cdecl is used }
                   if not is_cdecl then
                    begin
                      { dll name ? }
                      if (extern_aktvarsym) and (idtoken<>_NAME) then
                       begin
                         is_dll:=true;
                         dll_name:=get_stringconst;
                       end;
                      consume(_NAME);
                      C_name:=get_stringconst;
                    end;
                   { consume the ; when export or external is used }
                   if extern_aktvarsym or export_aktvarsym then
                    consume(_SEMICOLON);
                   { insert in the symtable }
                   storetokenpos:=tokenpos;
                   tokenpos:=declarepos;
                   if is_dll then
                    begin
                      if assigned(readtypesym) then
                       aktvarsym:=new(pvarsym,initsym_dll(s,readtypesym))
                      else
                       aktvarsym:=new(pvarsym,init_dll(s,p))
                    end
                   else
                    begin
                      if assigned(readtypesym) then
                       aktvarsym:=new(pvarsym,initsym_C(s,C_name,readtypesym))
                      else
                       aktvarsym:=new(pvarsym,init_C(s,C_name,p));
                    end;
                   { set some vars options }
                   if export_aktvarsym then
                    inc(aktvarsym^.refs);
                   if extern_aktvarsym then
{$ifdef INCLUDEOK}
                    include(aktvarsym^.varoptions,vo_is_external);
{$else}
                    aktvarsym^.varoptions:=aktvarsym^.varoptions+[vo_is_external];
{$endif}
                   { insert in the stack/datasegment }
                   symtablestack^.insert(aktvarsym);
                   tokenpos:=storetokenpos;
                   { now we can insert it in the import lib if its a dll, or
                     add it to the externals }
                   if extern_aktvarsym then
                    begin
                      if is_dll then
                       begin
                         if not(current_module^.uses_imports) then
                          begin
                            current_module^.uses_imports:=true;
                            importlib^.preparelib(current_module^.modulename^);
                          end;
                         importlib^.importvariable(aktvarsym^.mangledname,dll_name,C_name)
                       end
                    end;
                   symdone:=true;
                 end
                else
                 if (is_object) and (cs_static_keyword in aktmoduleswitches) and (idtoken=_STATIC) then
                  begin
{$ifdef INCLUDEOK}
                    include(current_object_option,sp_static);
{$else}
                    current_object_option:=current_object_option+[sp_static];
{$endif}
                    if assigned(readtypesym) then
                     insert_syms(symtablestack,sc,nil,readtypesym,false)
                    else
                     insert_syms(symtablestack,sc,p,nil,false);
{$ifdef INCLUDEOK}
                    exclude(current_object_option,sp_static);
{$else}
                    current_object_option:=current_object_option-[sp_static];
{$endif}
                    consume(_STATIC);
                    consume(_SEMICOLON);
                    symdone:=true;
                  end;
              end;
             { insert it in the symtable, if not done yet }
             if not symdone then
               begin
                  if (sp_published in current_object_option) and
                    (not((p^.deftype=objectdef) and (pobjectdef(p)^.is_class))) then
                    Message(parser_e_cant_publish_that);
                  if assigned(readtypesym) then
                   insert_syms(symtablestack,sc,nil,readtypesym,is_threadvar)
                  else
                   insert_syms(symtablestack,sc,p,nil,is_threadvar);
               end;
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
                  consume(_ID);
                  consume(_COLON);
                  casedef:=read_type('');
                  symtablestack^.insert(new(pvarsym,init(s,casedef)));
                end;
              if not(is_ordinal(casedef)) or is_64bitint(casedef)  then
               Message(type_e_ordinal_expr_expected);
              consume(_OF);
              startvarrec:=symtablestack^.datasize;
              repeat
                repeat
                  pt:=comp_expr(true);
                  do_firstpass(pt);
                  if not(pt^.treetype=ordconstn) then
                    Message(cg_e_illegal_expression);
                  disposetree(pt);
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
                maxsize:=max(maxsize,symtablestack^.datasize);
                { the items of the next variant are overlayed }
                symtablestack^.datasize:=startvarrec;
                if (token<>_END) and (token<>_RKLAMMER) then
                  consume(_SEMICOLON)
                else
                  break;
              until (token=_END) or (token=_RKLAMMER);
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
          stringtype:=d;
       end;


    function id_type(var s : string) : pdef;
    { reads a type definition and returns a pointer }
    { to a appropriating pdef, s gets the name of   }
    { the type to allow name mangling          }
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
         getsym(s,true);
         if assigned(srsym) then
           begin
              if srsym^.typ=unitsym then
                begin
                   consume(_POINT);
                   getsymonlyin(punitsym(srsym)^.unitsymtable,pattern);
                   s:=pattern;
                   consume(_ID);
                end;
              if not assigned(srsym) or
                 (srsym^.typ<>typesym) then
                begin
                   Message(type_e_type_id_expected);
                   lasttypesym:=ptypesym(srsym);
                   id_type:=generrordef;
                   exit;
                end;
              if not forwardsallowed then
                testforward_type(srsym);
           end;
         lasttypesym:=ptypesym(srsym);
         if (ptypesym(srsym)^.owner^.unitid=0) or
            (ptypesym(srsym)^.owner^.unitid=1) then
          readtypesym:=nil
         else
          readtypesym:=ptypesym(srsym);
         id_type:=ptypesym(srsym)^.definition;
      end;


    function single_type(var s : string) : pdef;
    { reads a string, file type or a type id and returns a name and }
    { pdef                                                        }
       var
          hs : string;
       begin
          readtypesym:=nil;
          case token of
            _STRING:
                begin
                   single_type:=stringtype;
                   s:='STRING';
                   lasttypesym:=nil;
                   readtypesym:=nil;
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
                   readtypesym:=nil;
                end;
            else
              begin
                single_type:=id_type(s);
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

        var
           hp2,datacoll : pdefcoll;
           p,p2 : ppropertysym;
           overriden : psym;
           hs : string;
           code : integer;
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
           propertyparas:=nil;
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
                                 parraydef(hp)^.definition:=single_type(s);
                              end
                            else
                              hp:=single_type(s);
                         end
                       else
                         hp:=cformaldef;
                       s:=sc^.get_with_tokeninfo(declarepos);
                       while s<>'' do
                         begin
                            new(hp2);
                            hp2^.paratyp:=varspez;
                            hp2^.data:=hp;
                            hp2^.next:=propertyparas;
                            propertyparas:=hp2;
                            s:=sc^.get_with_tokeninfo(declarepos);
                         end;
                       dispose(sc,done);
                       if token=_SEMICOLON then consume(_SEMICOLON)
                     else break;
                     until false;
                     dec(testcurobject);
                     consume(_RECKKLAMMER);
                  end;
                { overriden property ?                                 }
                { force property interface, if there is a property parameter }
                if (token=_COLON) or assigned(propertyparas) then
                  begin
                     consume(_COLON);
                     p^.proptype:=single_type(hs);
                     if (idtoken=_INDEX) then
                       begin
                          consume(_INDEX);
{$ifdef INCLUDEOK}
                          include(p^.propoptions,ppo_indexed);
{$else}
                          p^.propoptions:=p^.propoptions+[ppo_indexed];
{$endif}
                          if token=_INTCONST then
                            val(pattern,p^.index,code);
                          consume(_INTCONST);
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
                     overriden:=search_class_member(aktclass,propname);
                     if assigned(overriden) and (overriden^.typ=propertysym) then
                       begin
                          { take the whole info: }
                          p^.propoptions:=ppropertysym(overriden)^.propoptions;
                          p^.index:=ppropertysym(overriden)^.index;
                          p^.proptype:=ppropertysym(overriden)^.proptype;
                          p^.writeaccesssym:=ppropertysym(overriden)^.writeaccesssym;
                          p^.readaccesssym:=ppropertysym(overriden)^.readaccesssym;
                          p^.writeaccessdef:=ppropertysym(overriden)^.writeaccessdef;
                          p^.readaccessdef:=ppropertysym(overriden)^.readaccessdef;
                          p^.storedsym:=ppropertysym(overriden)^.storedsym;
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
                new(datacoll);
                datacoll^.paratyp:=vs_value;
                datacoll^.data:=p^.proptype;
                datacoll^.next:=nil;

                if (idtoken=_READ) then
                  begin
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
                                if not(is_equal(pvarsym(sym)^.definition,p^.proptype)) then
                                  Message(parser_e_ill_property_access_sym);
                              end;
                          end;
                          addpropsymlist(p^.readaccesssym,sym);
                       end;
                  end;
                if (idtoken=_WRITE) then
                  begin
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
                               datacoll^.next:=nil;
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
                          addpropsymlist(p^.writeaccesssym,sym);
                       end;
                  end;
                if (idtoken=_STORED) then
                  begin
                     consume(_STORED);
                     Message(parser_w_stored_not_implemented);
                     { !!!!!!!! }
                  end;
                if (idtoken=_DEFAULT) then
                  begin
                     consume(_DEFAULT);
                     if not(is_ordinal(p^.proptype) or
                         is_64bitint(p^.proptype) or
                       ((p^.proptype^.deftype=setdef) and
                        (psetdef(p^.proptype)^.settype=smallset)
                       ) or
                       assigned(propertyparas)
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
                          if not(assigned(propertyparas)) then
                            message(parser_e_property_need_paras);
                       end;
                     consume(_SEMICOLON);
                  end;
                { clean up }
                if assigned(datacoll) then
                  disposepdefcoll(datacoll);
             end
           else
             begin
                consume(_ID);
                consume(_SEMICOLON);
             end;
           if assigned(propertyparas) then
             disposepdefcoll(propertyparas);
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
           if assigned(aktprocsym^.definition^.para1) then
            Message(parser_e_no_paras_for_destructor);
           { no return value }
           aktprocsym^.definition^.retdef:=voiddef;
        end;

      var
         hs      : string;
         pcrd       : pclassrefdef;
         hp1    : pdef;
         oldprocsym : pprocsym;
         oldparse_only : boolean;
         intmessagetable,strmessagetable,classnamelabel : pasmlabel;
         storetypeforwardsallowed : boolean;

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

         storetypeforwardsallowed:=typecanbeforward;
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
                   if typecanbeforward then
                     forwardsallowed:=true;
                   hp1:=single_type(hs);

                   { accept hp1, if is a forward def ...}
                   if ((lasttypesym<>nil) and
                       (sp_forwarddef in lasttypesym^.symoptions)) or
                   { or a class
                     (if the foward defined type is a class is checked, when
                      the forward is resolved)
                   }
                     ((hp1^.deftype=objectdef) and pobjectdef(hp1)^.is_class) then
                     begin
                        pcrd:=new(pclassrefdef,init(hp1));
                        object_dec:=pcrd;
                        if assigned(lasttypesym) and (sp_forwarddef in lasttypesym^.symoptions) then
                         lasttypesym^.addforwardpointer(ppointerdef(pcrd));
                        forwardsallowed:=false;
                     end
                   else
                     begin
                        object_dec:=generrordef;
                        Message1(type_e_class_type_expected,generrordef^.typename);
                     end;
                   typecanbeforward:=storetypeforwardsallowed;
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
                   typecanbeforward:=storetypeforwardsallowed;
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
              childof:=pobjectdef(id_type(pattern));
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
{$ifdef INCLUDEOK}
                include(aktclass^.objectoptions,oo_can_have_published);
{$else}
                aktclass^.objectoptions:=aktclass^.objectoptions+[oo_can_have_published];
{$endif}
           end;

         aktobjectdef:=aktclass;

         { default access is public }
         actmembertype:=[sp_public];
         aktclass^.symtable^.next:=symtablestack;
         symtablestack:=aktclass^.symtable;
         procinfo._class:=aktclass;
         testcurobject:=1;
         curobjectname:=n;

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
         typecanbeforward:=storetypeforwardsallowed;

         { generate vmt space if needed }
         if not(oo_has_vmt in aktclass^.objectoptions) and
            ([oo_has_virtual,oo_has_constructor,oo_has_destructor,oo_is_class]*aktclass^.objectoptions<>[]) then
           aktclass^.insertvmt;
         if (cs_smartlink in aktmoduleswitches) then
           datasegment^.concat(new(pai_cut,init));

         { Write the start of the VMT, wich is equal for classes and objects }
         if (oo_has_vmt in aktclass^.objectoptions) then
           begin
              { write tables for classes, this must be done before the actual
                class is written, because we need the labels defined }
              if is_a_class then
               begin
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

              { this generates the entries }
              genvmt(aktclass);

              { write the size of the VMT }
              datasegment^.concat(new(pai_symbol_end,initname(aktclass^.vmt_mangledname)));
           end;

         { restore old state }
         symtablestack:=symtablestack^.next;
         procinfo._class:=nil;
         aktobjectdef:=nil;
         {Restore the aktprocsym.}
         aktprocsym:=oldprocsym;

         object_dec:=aktclass;
      end;

    { reads a record declaration }
    function record_dec : pdef;

      var
         symtable : psymtable;
         storetypeforwardsallowed : boolean;

      begin
         { create recdef }
         symtable:=new(psymtable,init(recordsymtable));
         record_dec:=new(precorddef,init(symtable));
         { update symtable stack }
         symtable^.next:=symtablestack;
         symtablestack:=symtable;
         { parse record }
         consume(_RECORD);
         storetypeforwardsallowed:=typecanbeforward;
         if m_tp in aktmodeswitches then
           typecanbeforward:=false;
         read_var_decs(true,false,false);
         consume(_END);
         typecanbeforward:=storetypeforwardsallowed;
         { may be scale record size to a size of n*4 ? }
         symtablestack^.datasize:=align(symtablestack^.datasize,symtablestack^.dataalignment);
         { restore symtable stack }
         symtablestack:=symtable^.next;
      end;


    { reads a type definition and returns a pointer to it }
    function read_type(const name : stringid) : pdef;

        function handle_procvar:Pprocvardef;
        var
           sc : pstringcontainer;
           hs1,s : string;
           p : pdef;
           varspez : tvarspez;
           procvardef : pprocvardef;
        begin
           procvardef:=new(pprocvardef,init);
           if token=_LKLAMMER then
             begin
                consume(_LKLAMMER);
                inc(testcurobject);
                repeat
                  if try_to_consume(_VAR) then
                   varspez:=vs_var
                  else
                   if try_to_consume(_CONST) then
                     varspez:=vs_const
                   else
                     varspez:=vs_value;
                  { self method ? }
                  if idtoken=_SELF then
                   begin
{$ifdef INCLUDEOK}
                     include(procvardef^.procoptions,po_containsself);
{$else}
                     procvardef^.procoptions:=procvardef^.procoptions+[po_containsself];
{$endif}
                     consume(idtoken);
                     consume(_COLON);
                     p:=single_type(hs1);
                     procvardef^.concatdef(p,vs_value);
                   end
                  else
                   begin
                     sc:=idlist;
                     if (token=_COLON) or (varspez=vs_value) then
                       begin
                          consume(_COLON);
                          if token=_ARRAY then
                            begin
                              consume(_ARRAY);
                              consume(_OF);
                            { define range and type of range }
                              p:=new(Parraydef,init(0,-1,s32bitdef));
                            { array of const ? }
                              if (token=_CONST) and (m_objpas in aktmodeswitches) then
                               begin
                                 consume(_CONST);
                                 srsym:=nil;
                                 if assigned(objpasunit) then
                                  getsymonlyin(objpasunit,'TVARREC');
                                 if not assigned(srsym) then
                                  InternalError(1234124);
                                 Parraydef(p)^.definition:=ptypesym(srsym)^.definition;
                                 Parraydef(p)^.IsArrayOfConst:=true;
                               end
                              else
                               begin
                               { define field type }
                                 Parraydef(p)^.definition:=single_type(s);
                               end;
                            end
                          else
                            p:=single_type(s);
                       end
                     else
                       p:=cformaldef;
                     while not sc^.empty do
                       begin
                          s:=sc^.get;
                          procvardef^.concatdef(p,varspez);
                       end;
                     dispose(sc,done);
                   end;
                until not try_to_consume(_SEMICOLON);
                dec(testcurobject);
                consume(_RKLAMMER);
             end;
           handle_procvar:=procvardef;
        end;

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
                p:=single_type(hs);
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
                { forwards allowed only inside TYPE statements }
                if typecanbeforward then
                  forwardsallowed:=true;
                hp1:=single_type(hs);
                p:=new(ppointerdef,init(hp1));
                if (lasttypesym<>nil) and (sp_forwarddef in lasttypesym^.symoptions) then
                  lasttypesym^.addforwardpointer(ppointerdef(p));
                forwardsallowed:=false;
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
                p:=handle_procvar;
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
                p:=handle_procvar;
                consume(_COLON);
                pprocvardef(p)^.retdef:=single_type(hs);
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


    { reads a type declaration to the symbol table }
    procedure type_dec;

      var
         typename : stringid;
         newtype  : ptypesym;
         sym      : psym;
      begin
         block_type:=bt_type;
         consume(_TYPE);
         typecanbeforward:=true;
         repeat
           typename:=pattern;
           consume(_ID);
           consume(_EQUAL);
             { here you loose the strictness of pascal
             for which a redefinition like
               childtype = parenttype;
                           child2type = parenttype;
             does not make the two child types equal !!
             here all vars from childtype and child2type
             get the definition of parenttype !!            }
{$ifdef testequaltype}
           if (token = _ID) or (token=_FILE) or (token=_STRING) then
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
                sym:=srsym;
                newtype:=nil;
                { found a symbol with this name? }
                if assigned(sym) then
                 begin
                   if (sym^.typ=typesym) then
                    begin
                      if (token=_CLASS) and
                         (assigned(ptypesym(sym)^.definition)) and
                         (ptypesym(sym)^.definition^.deftype=objectdef) and
                         pobjectdef(ptypesym(sym)^.definition)^.is_class and
                         (oo_is_forward in pobjectdef(ptypesym(sym)^.definition)^.objectoptions) then
                       begin
                         { we can ignore the result   }
                         { the definition is modified }
                         object_dec(typename,pobjectdef(ptypesym(sym)^.definition));
                         newtype:=ptypesym(sym);
                       end
                      else
                       if (sp_forwarddef in sym^.symoptions) then
                        begin
                          ptypesym(sym)^.updateforwarddef(read_type(typename));
                          newtype:=ptypesym(sym);
                        end;
                    end;
                 end;
                { no old type reused ? Then insert this new type }
                if not assigned(newtype) then
                 begin
                   newtype:=new(ptypesym,init(typename,read_type(typename)));
                   newtype:=ptypesym(symtablestack^.insert(newtype));
                 end;
             end;
           consume(_SEMICOLON);
           if assigned(newtype^.definition) and (newtype^.definition^.deftype=procvardef) then
             parse_var_proc_directives(newtype);
         until token<>_ID;
         typecanbeforward:=false;
         symtablestack^.foreach({$ifndef TP}@{$endif}testforward_type);
         resolve_forwards;
         block_type:=bt_general;
      end;


    procedure var_dec;
    { parses varaible declarations and inserts them in }
    { the top symbol table of symtablestack         }
      begin
        consume(_VAR);
        read_var_decs(false,false,false);
      end;

    procedure threadvar_dec;
    { parses thread variable declarations and inserts them in }
    { the top symbol table of symtablestack                }
      begin
        consume(_THREADVAR);
        if not(symtablestack^.symtabletype in [staticsymtable,globalsymtable]) then
          message(parser_e_threadvars_only_sg);
        read_var_decs(false,false,true);
      end;

    procedure resourcestring_dec;

      var
         name : stringid;
         p : ptree;
         storetokenpos,filepos : tfileposinfo;
         old_block_type : tblock_type;
         sp : pchar;

      begin
         consume(_RESOURCESTRING);
         if not(symtablestack^.symtabletype in [staticsymtable,globalsymtable]) then
           message(parser_e_resourcestring_only_sg);
         old_block_type:=block_type;
         block_type:=bt_const;
         repeat
           name:=pattern;
           filepos:=tokenpos;
           consume(_ID);
           case token of
             _EQUAL:
                begin
                   consume(_EQUAL);
                   p:=comp_expr(true);
                   do_firstpass(p);
                   storetokenpos:=tokenpos;
                   tokenpos:=filepos;
                   case p^.treetype of
                      ordconstn:
                        begin
                           if is_constcharnode(p) then
                             begin
                                getmem(sp,2);
                                sp[0]:=chr(p^.value);
                                sp[1]:=#0;
                                symtablestack^.insert(new(pconstsym,init_string(name,constresourcestring,sp,1)));
                             end
                           else
                             Message(cg_e_illegal_expression);
                        end;
                      stringconstn:
                        begin
                           getmem(sp,p^.length+1);
                           move(p^.value_str^,sp^,p^.length+1);
                           symtablestack^.insert(new(pconstsym,init_string(name,constresourcestring,sp,p^.length)));
                        end;
                      else
                        Message(cg_e_illegal_expression);
                   end;
                   tokenpos:=storetokenpos;
                   consume(_SEMICOLON);
                   disposetree(p);
                end;
              else consume(_EQUAL);
           end;
         until token<>_ID;
         block_type:=old_block_type;

      end;

    procedure Not_supported_for_inline(t : ttoken);

      begin
         if assigned(aktprocsym) and
            (pocall_inline in aktprocsym^.definition^.proccalloptions) then
           Begin
              Message1(parser_w_not_supported_for_inline,tokenstring(t));
              Message(parser_w_inlining_disabled);
{$ifdef INCLUDEOK}
              exclude(aktprocsym^.definition^.proccalloptions,pocall_inline);
{$else}
              aktprocsym^.definition^.proccalloptions:=aktprocsym^.definition^.proccalloptions-[pocall_inline];
{$endif}
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
              _THREADVAR:
                threadvar_dec;
              _CONSTRUCTOR,_DESTRUCTOR,
              _FUNCTION,_PROCEDURE,_OPERATOR,_CLASS:
                begin
                   Not_supported_for_inline(token);
                   read_proc;
                end;
              _RESOURCESTRING:
                resourcestring_dec;
              _EXPORTS:
                begin
                   Not_supported_for_inline(token);
                   { here we should be at lexlevel 1, no ? PM }
                   if (lexlevel<>main_program_level) or
                      (not islibrary and not DLLsource) then
                     begin
                        Message(parser_e_syntax_error);
                        consume_all_until(_SEMICOLON);
                     end
                   else if islibrary then
                     read_exports;
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
              _THREADVAR : threadvar_dec;
              _RESOURCESTRING:
                resourcestring_dec;
         _FUNCTION,
        _PROCEDURE,
         _OPERATOR : read_proc;
           else
             break;
           end;
         until false;
         dec(lexlevel);
      end;

end.
{
  $Log$
  Revision 1.145  1999-08-26 21:17:39  peter
    * fixed crash when childof was nil

  Revision 1.144  1999/08/14 00:38:53  peter
    * hack to support property with record fields

  Revision 1.143  1999/08/09 22:19:53  peter
    * classes vmt changed to only positive addresses
    * sharedlib creation is working

  Revision 1.142  1999/08/05 16:53:02  peter
    * V_Fatal=1, all other V_ are also increased
    * Check for local procedure when assigning procvar
    * fixed comment parsing because directives
    * oldtp mode directives better supported
    * added some messages to errore.msg

  Revision 1.141  1999/08/04 13:02:51  jonas
    * all tokens now start with an underscore
    * PowerPC compiles!!

  Revision 1.140  1999/08/04 00:23:11  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.139  1999/08/03 22:02:56  peter
    * moved bitmask constants to sets
    * some other type/const renamings

  Revision 1.138  1999/08/01 18:28:11  florian
    * modifications for the new code generator

  Revision 1.137  1999/07/29 20:54:02  peter
    * write .size also

  Revision 1.136  1999/07/27 23:42:11  peter
    * indirect type referencing is now allowed

  Revision 1.135  1999/07/23 16:05:23  peter
    * alignment is now saved in the symtable
    * C alignment added for records
    * PPU version increased to solve .12 <-> .13 probs

  Revision 1.134  1999/07/22 09:37:50  florian
    + resourcestring implemented
    + start of longstring support

  Revision 1.133  1999/07/16 10:04:34  peter
    * merged

  Revision 1.132  1999/07/11 21:24:31  michael
  + Fixed integer message table

  Revision 1.131  1999/07/06 21:48:23  florian
    * a lot bug fixes:
       - po_external isn't any longer necessary for procedure compatibility
       - m_tp_procvar is in -Sd now available
       - error messages of procedure variables improved
       - return values with init./finalization fixed
       - data types with init./finalization aren't any longer allowed in variant
         record

  Revision 1.130  1999/07/05 20:25:39  peter
    * merged

  Revision 1.129  1999/07/02 13:02:26  peter
    * merged

  Revision 1.128  1999/06/30 22:16:19  florian
    * use of is_ordinal checked: often a qword/int64 isn't allowed (case/for ...)
    * small qword problems fixed

  Revision 1.127.2.4  1999/07/11 21:48:01  michael
  + merged dispatch fix

  Revision 1.127.2.3  1999/07/07 07:53:22  michael
  + Merged patches from florian

  Revision 1.127.2.2  1999/07/05 20:03:27  peter
    * removed warning/notes

  Revision 1.127.2.1  1999/07/02 12:59:49  peter
    * fixed parsing of message directive

  Revision 1.127  1999/06/02 22:44:10  pierre
   * previous wrong log corrected

  Revision 1.126  1999/06/02 22:25:42  pierre
  * changed $ifdef FPC @ into $ifndef TP

  Revision 1.125  1999/06/01 19:27:53  peter
    * better checks for procvar and methodpointer

  Revision 1.124  1999/06/01 14:45:51  peter
    * @procvar is now always needed for FPC

  Revision 1.123  1999/05/27 19:44:45  peter
    * removed oldasm
    * plabel -> pasmlabel
    * -a switches to source writing automaticly
    * assembler readers OOPed
    * asmsymbol automaticly external
    * jumptables and other label fixes for asm readers

  Revision 1.122  1999/05/21 20:08:22  florian
    * hopefully the default property bug fixed

  Revision 1.121  1999/05/21 13:55:04  peter
    * NEWLAB for label as symbol

  Revision 1.120  1999/05/20 22:19:52  pierre
   * better stabs line info for vars

  Revision 1.119  1999/05/19 12:41:56  florian
    * made source compilable with TP (too long line)
    * default values for set properties fixed

  Revision 1.118  1999/05/18 14:15:51  peter
    * containsself fixes
    * checktypes()

  Revision 1.117  1999/05/17 21:57:12  florian
    * new temporary ansistring handling

  Revision 1.116  1999/05/13 21:59:34  peter
    * removed oldppu code
    * warning if objpas is loaded from uses
    * first things for new deref writing

  Revision 1.115  1999/05/07 10:36:09  peter
    * fixed crash

  Revision 1.114  1999/05/04 21:44:54  florian
    * changes to compile it with Delphi 4.0

  Revision 1.113  1999/05/01 13:24:30  peter
    * merged nasm compiler
    * old asm moved to oldasm/

  Revision 1.112  1999/04/28 06:02:07  florian
    * changes of Bruessel:
       + message handler can now take an explicit self
       * typinfo fixed: sometimes the type names weren't written
       * the type checking for pointer comparisations and subtraction
         and are now more strict (was also buggy)
       * small bug fix to link.pas to support compiling on another
         drive
       * probable bug in popt386 fixed: call/jmp => push/jmp
         transformation didn't count correctly the jmp references
       + threadvar support
       * warning if ln/sqrt gets an invalid constant argument

  Revision 1.111  1999/04/26 13:31:37  peter
    * release storenumber,double_checksum

  Revision 1.110  1999/04/25 22:42:16  pierre
   + code for initialized vars in Delphi mode

  Revision 1.109  1999/04/21 09:43:45  peter
    * storenumber works
    * fixed some typos in double_checksum
    + incompatible types type1 and type2 message (with storenumber)

  Revision 1.108  1999/04/17 13:16:19  peter
    * fixes for storenumber

  Revision 1.107  1999/04/14 09:14:50  peter
    * first things to store the symbol/def number in the ppu

  Revision 1.106  1999/04/07 15:31:15  pierre
    * all formaldefs are now a sinlge definition
      cformaldef (this was necessary for double_checksum)
    + small part of double_checksum code

  Revision 1.105  1999/03/26 00:05:34  peter
    * released valintern
    + deffile is now removed when compiling is finished
    * ^( compiles now correct
    + static directive
    * shrd fixed

  Revision 1.104  1999/03/24 23:17:13  peter
    * fixed bugs 212,222,225,227,229,231,233

  Revision 1.103  1999/03/22 22:10:25  florian
    * typecanbeforward wasn't always restored in object_dec which
      sometimes caused strange effects

  Revision 1.102  1999/03/05 01:14:26  pierre
    * bug0198 : call conventions for methods
      not yet implemented is the control of same calling convention
      for virtual and child's virtual
    * msgstr and msgint only created if message was found
      who implemented this by the way ?
      it leaks lots of plabels !!!! (check with heaptrc !)

  Revision 1.101  1999/02/25 21:02:41  peter
    * ag386bin updates
    + coff writer

  Revision 1.100  1999/02/24 00:59:14  peter
    * small updates for ag386bin

  Revision 1.99  1999/02/22 23:33:29  florian
    + message directive for integers added

  Revision 1.98  1999/02/22 20:13:36  florian
    + first implementation of message keyword

  Revision 1.97  1999/02/22 02:44:10  peter
    * ag386bin doesn't use i386.pas anymore

  Revision 1.96  1999/02/17 14:20:40  pierre
   * Reference specific bug in recompiling unit solved

  Revision 1.95  1999/01/25 20:13:48  peter
    * fixed crash with forward declared class of ...

  Revision 1.94  1999/01/19 12:17:00  peter
    * fixed constant strings > 255 chars

  Revision 1.93  1999/01/15 13:08:23  peter
    * error if upper<lower in array decl

  Revision 1.92  1999/01/14 21:49:58  peter
    * fixed forwardpointer problem with multiple forwards for the same
      typesym. It now uses a linkedlist instead of a single pointer

  Revision 1.91  1998/12/30 22:15:46  peter
    + farpointer type
    * absolutesym now also stores if its far

  Revision 1.90  1998/12/15 17:16:00  peter
    * fixed const s : ^string
    * first things for const pchar : @string[1]

  Revision 1.89  1998/12/11 00:03:30  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.88  1998/11/30 09:43:20  pierre
    * some range check bugs fixed (still not working !)
    + added DLL writing support for win32 (also accepts variables)
    + TempAnsi for code that could be used for Temporary ansi strings
      handling

  Revision 1.87  1998/11/29 12:42:24  peter
    * check for constants with array decl

  Revision 1.86  1998/11/28 16:20:52  peter
    + support for dll variables

  Revision 1.85  1998/11/27 14:34:43  peter
    * give error when string[0] decl is found

  Revision 1.84  1998/11/17 10:40:15  peter
    * H+ fixes

  Revision 1.83  1998/11/16 11:28:59  pierre
    * stackcheck removed for i386_win32
    * exportlist does not crash at least !!
      (was need for tests dir !)z

  Revision 1.82  1998/11/16 10:18:07  peter
    * fixes for ansistrings

  Revision 1.81  1998/11/13 15:40:22  pierre
    + added -Se in Makefile cvstest target
    + lexlevel cleanup
      normal_function_level main_program_level and unit_init_level defined
    * tins_cache grown to A_EMMS (gave range check error in asm readers)
      (test added in code !)
    * -Un option was wrong
    * _FAIL and _SELF only keyword inside
      constructors and methods respectively

  Revision 1.80  1998/11/13 10:18:09  peter
    + nil constants

  Revision 1.79  1998/11/05 12:02:51  peter
    * released useansistring
    * removed -Sv, its now available in fpc modes

  Revision 1.78  1998/10/27 13:45:33  pierre
    * classes get a vmt allways
    * better error info (tried to remove
      several error strings introduced by the tpexcept handling)

  Revision 1.77  1998/10/26 22:58:20  florian
    * new introduded problem with classes fix, the parent class wasn't set
      correct, if the class was defined forward before

  Revision 1.76  1998/10/25 23:31:18  peter
    * procvar parsing updated just like psub.pas routine

  Revision 1.75  1998/10/21 08:39:59  florian
    + ansistring operator +
    + $h and string[n] for n>255 added
    * small problem with TP fixed

  Revision 1.74  1998/10/20 13:09:13  peter
    * fixed object(unknown) crash

  Revision 1.73  1998/10/19 08:54:56  pierre
    * wrong stabs info corrected once again !!
    + variable vmt offset with vmt field only if required
      implemented now !!!

  Revision 1.72  1998/10/16 13:12:51  pierre
    * added vmt_offsets in destructors code also !!!
    * vmt_offset code for m68k

  Revision 1.71  1998/10/15 15:13:25  pierre
    + added oo_hasconstructor and oo_hasdestructor
      for objects options

  Revision 1.70  1998/10/13 13:10:22  peter
    * new style for m68k/i386 infos and enums

  Revision 1.69  1998/10/09 12:07:49  pierre
    * typo error for propertyparas dispose corrected

  Revision 1.68  1998/10/09 11:47:54  pierre
    * still more memory leaks fixes !!

  Revision 1.67  1998/10/08 13:48:46  peter
    * fixed memory leaks for do nothing source
    * fixed unit interdependency

  Revision 1.66  1998/10/06 20:43:31  peter
    * fixed set of bugs. like set of false..true set of #1..#255 and
      set of #1..true which was allowed

  Revision 1.65  1998/10/05 22:43:35  peter
    * commited the wrong file :(

  Revision 1.64  1998/10/05 21:33:24  peter
    * fixed 161,165,166,167,168

  Revision 1.63  1998/10/05 13:57:13  peter
    * crash preventions

  Revision 1.62  1998/10/02 17:06:02  peter
    * better error message for unresolved forward types

  Revision 1.61  1998/10/02 09:23:24  peter
    * fixed error msg with type l=<var>
    * block_type bt_const is now set in read_const_dec

  Revision 1.60  1998/09/30 07:40:33  florian
    * better error recovering

  Revision 1.59  1998/09/26 17:45:33  peter
    + idtoken and only one token table

  Revision 1.58  1998/09/25 00:04:01  florian
    * problems when calling class methods fixed

  Revision 1.57  1998/09/24 23:49:09  peter
    + aktmodeswitches

  Revision 1.56  1998/09/23 15:39:09  pierre
    * browser bugfixes
      was adding a reference when looking for the symbol
      if -bSYM_NAME was used

  Revision 1.55  1998/09/21 13:24:44  daniel
  * Memory leak fixed.

  Revision 1.54  1998/09/17 13:41:16  pierre
  sizeof(TPOINT) problem

  Revision 1.53.2.1  1998/09/17 13:12:09  pierre
    * virtual destructor did not set oo_hasvirtual
      (detected with the sizeof(TPoint) problem
    * genloadcallnode was missing

  Revision 1.53  1998/09/09 11:50:52  pierre
    * forward def are not put in record or objects
    + added check for forwards also in record and objects
    * dummy parasymtable for unit initialization removed from
    symtable stack

  Revision 1.52  1998/09/07 23:10:22  florian
    * a lot of stuff fixed regarding rtti and publishing of properties,
      basics should now work

  Revision 1.51  1998/09/07 19:33:22  florian
    + some stuff for property rtti added:
       - NameIndex of the TPropInfo record is now written correctly
       - the DEFAULT/NODEFAULT keyword is supported now
       - the default value and the storedsym/def are now written to
         the PPU fiel

  Revision 1.50  1998/09/07 18:46:08  peter
    * update smartlinking, uses getdatalabel
    * renamed ptree.value vars to value_str,value_real,value_set

  Revision 1.49  1998/09/07 17:37:00  florian
    * first fixes for published properties

  Revision 1.48  1998/09/04 08:42:02  peter
    * updated some error messages

  Revision 1.47  1998/09/03 16:03:18  florian
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
