{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Does declaration (but not type) parsing for Free Pascal

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

{$define UseUnionSymtable}

  interface

    uses
      globtype,tokens,globals,symtable;

    procedure parameter_dec(aktprocdef:pabstractprocdef);

    procedure read_var_decs(is_record,is_object,is_threadvar:boolean);

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
       ,pbase,ptconst,pexpr,ptype,psub,pexports
       { processor specific stuff }
       { codegen }
{$ifdef newcg}
       ,cgbase
{$else}
       ,hcodegen
{$endif}

       ,hcgdata
       ;


    procedure parameter_dec(aktprocdef:pabstractprocdef);
      {
        handle_procvar needs the same changes
      }
      var
        is_procvar : boolean;
        sc      : Pstringcontainer;
        s       : string;
        storetokenpos : tfileposinfo;
        tt      : ttype;
        hvs,
        vs      : Pvarsym;
        hs1,hs2 : string;
        varspez : Tvarspez;
        inserthigh : boolean;
      begin
        { parsing a proc or procvar ? }
        is_procvar:=(aktprocdef^.deftype=procvardef);
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
          inserthigh:=false;
          tt.reset;
          if idtoken=_SELF then
            begin
               { only allowed in procvars and class methods }
               if is_procvar or
                  (assigned(procinfo^._class) and procinfo^._class^.is_class) then
                begin
                  if not is_procvar then
                   begin
{$ifndef UseNiceNames}
                     hs2:=hs2+'$'+'self';
{$else UseNiceNames}
                     hs2:=hs2+tostr(length('self'))+'self';
{$endif UseNiceNames}
                     vs:=new(Pvarsym,initdef('@',procinfo^._class));
                     vs^.varspez:=vs_var;
                   { insert the sym in the parasymtable }
                     pprocdef(aktprocdef)^.parast^.insert(vs);
{$ifdef INCLUDEOK}
                     include(aktprocdef^.procoptions,po_containsself);
{$else}
                     aktprocdef^.procoptions:=aktprocdef^.procoptions+[po_containsself];
{$endif}
                     inc(procinfo^.selfpointer_offset,vs^.address);
                   end;
                  consume(idtoken);
                  consume(_COLON);
                  single_type(tt,hs1,false);
                  aktprocdef^.concatpara(tt,vs_value);
                  { check the types for procedures only }
                  if not is_procvar then
                   CheckTypes(tt.def,procinfo^._class);
                end
               else
                consume(_ID);
            end
          else
            begin
             { read identifiers }
               sc:=idlist;
{$ifdef fixLeaksOnError}
               strContStack.push(sc);
{$endif fixLeaksOnError}
             { read type declaration, force reading for value and const paras }
               if (token=_COLON) or (varspez=vs_value) then
                begin
                  consume(_COLON);
                { check for an open array }
                  if token=_ARRAY then
                   begin
                     consume(_ARRAY);
                     consume(_OF);
                   { define range and type of range }
                     tt.setdef(new(Parraydef,init(0,-1,s32bitdef)));
                   { array of const ? }
                     if (token=_CONST) and (m_objpas in aktmodeswitches) then
                      begin
                        consume(_CONST);
                        srsym:=nil;
                        getsymonlyin(systemunit,'TVARREC');
                        if not assigned(srsym) then
                         InternalError(1234124);
                        Parraydef(tt.def)^.elementtype:=ptypesym(srsym)^.restype;
                        Parraydef(tt.def)^.IsArrayOfConst:=true;
                        hs1:='array_of_const';
                      end
                     else
                      begin
                        { define field type }
                        single_type(parraydef(tt.def)^.elementtype,hs1,false);
                        hs1:='array_of_'+hs1;
                      end;
                     inserthigh:=true;
                   end
                  { open string ? }
                  else if (varspez=vs_var) and
                          (
                            (
                              ((token=_STRING) or (idtoken=_SHORTSTRING)) and
                              (cs_openstring in aktmoduleswitches) and
                              not(cs_ansistrings in aktlocalswitches)
                            ) or
                          (idtoken=_OPENSTRING)) then
                   begin
                     consume(token);
                     tt.setdef(openshortstringdef);
                     hs1:='openstring';
                     inserthigh:=true;
                   end
                  { everything else }
                  else
                   single_type(tt,hs1,false);
                end
               else
                begin
{$ifndef UseNiceNames}
                  hs1:='$$$';
{$else UseNiceNames}
                  hs1:='var';
{$endif UseNiceNames}
                  tt.setdef(cformaldef);
                end;
               if not is_procvar then
                hs2:=pprocdef(aktprocdef)^.mangledname;
               storetokenpos:=tokenpos;
               while not sc^.empty do
                begin
                  s:=sc^.get_with_tokeninfo(tokenpos);
                  aktprocdef^.concatpara(tt,varspez);
                  { For proc vars we only need the definitions }
                  if not is_procvar then
                   begin
{$ifndef UseNiceNames}
                     hs2:=hs2+'$'+hs1;
{$else UseNiceNames}
                     hs2:=hs2+tostr(length(hs1))+hs1;
{$endif UseNiceNames}
                     vs:=new(pvarsym,init(s,tt));
                     vs^.varspez:=varspez;
                   { we have to add this to avoid var param to be in registers !!!}
                     if (varspez in [vs_var,vs_const]) and push_addr_param(tt.def) then
                       include(vs^.varoptions,vo_regable);

                   { insert the sym in the parasymtable }
                     pprocdef(aktprocdef)^.parast^.insert(vs);

                   { do we need a local copy? Then rename the varsym, do this after the
                     insert so the dup id checking is done correctly }
                     if (varspez=vs_value) and
                        push_addr_param(tt.def) and
                        not(is_open_array(tt.def) or is_array_of_const(tt.def)) then
                       pprocdef(aktprocdef)^.parast^.rename(vs^.name,'val'+vs^.name);

                   { also need to push a high value? }
                     if inserthigh then
                      begin
                        hvs:=new(Pvarsym,initdef('high'+s,s32bitdef));
                        hvs^.varspez:=vs_const;
                        pprocdef(aktprocdef)^.parast^.insert(hvs);
                      end;

                   end;
                end;
{$ifdef fixLeaksOnError}
               if PStringContainer(strContStack.pop) <> sc then
                  writeln('problem with strContStack in pdecl (1)');
{$endif fixLeaksOnError}
               dispose(sc,done);
               tokenpos:=storetokenpos;
            end;
          { set the new mangled name }
          if not is_procvar then
            pprocdef(aktprocdef)^.setmangledname(hs2);
        until not try_to_consume(_SEMICOLON);
        dec(testcurobject);
        consume(_RKLAMMER);
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

      procedure insert_syms(st : psymtable;sc : pstringcontainer;tt : ttype;is_threadvar : boolean);
      { inserts the symbols of sc in st with def as definition or sym as ptypesym, sc is disposed }
        var
           s : string;
           filepos : tfileposinfo;
           ss : pvarsym;
        begin
           filepos:=tokenpos;
           while not sc^.empty do
             begin
                s:=sc^.get_with_tokeninfo(tokenpos);
                ss:=new(pvarsym,init(s,tt));
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
                     st^.defowner^.owner^.insert(new(pvarsym,init(s,tt)));
                  end;
             end;
{$ifdef fixLeaksOnError}
             if strContStack.pop <> sc then
               writeln('problem with strContStack in pdecl (2)');
{$endif fixLeaksOnError}
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
         old_current_object_option : tsymoptions;
         dll_name,
         C_name : string;
         tt,casetype : ttype;
         { Delphi initialized vars }
         pconstsym : ptypedconstsym;
         { maxsize contains the max. size of a variant }
         { startvarrec contains the start of the variant part of a record }
         maxsize,maxalignment,startvarrecalign,startvarrecsize : longint;
         pt : ptree;
{$ifdef UseUnionSymtable}
         unionsymtable : psymtable;
         offset : longint;
         uniondef : precorddef;
         unionsym : pvarsym;
         uniontype : ttype;
{$endif UseUnionSymtable}
      begin
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
         while (token=_ID) and
               not(is_object and (idtoken in [_PUBLIC,_PRIVATE,_PUBLISHED,_PROTECTED])) do
           begin
             C_name:=orgpattern;
             sc:=idlist;
{$ifdef fixLeaksOnError}
             strContStack.push(sc);
{$endif fixLeaksOnError}
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
             read_type(tt,'');
             if (variantrecordlevel>0) and tt.def^.needs_inittable then
               Message(parser_e_cant_use_inittable_here);
             ignore_equal:=false;
             symdone:=false;
             if is_gpc_name then
               begin
                  storetokenpos:=tokenpos;
                  s:=sc^.get_with_tokeninfo(tokenpos);
                  if not sc^.empty then
                   Message(parser_e_absolute_only_one_var);
{$ifdef fixLeaksOnError}
                   if strContStack.pop <> sc then
                     writeln('problem with strContStack in pdecl (3)');
{$endif fixLeaksOnError}
                  dispose(sc,done);
                  aktvarsym:=new(pvarsym,init_C(s,target_os.Cprefix+C_name,tt));
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
{$ifdef fixLeaksOnError}
                 if strContStack.pop <> sc then
                   writeln('problem with strContStack in pdecl (4)');
{$endif fixLeaksOnError}
                dispose(sc,done);
                { parse the rest }
                if token=_ID then
                 begin
                   getsym(pattern,true);
                   consume(_ID);
                   { support unit.variable }
                   if srsym^.typ=unitsym then
                    begin
                      consume(_POINT);
                      getsymonlyin(punitsym(srsym)^.unitsymtable,pattern);
                      consume(_ID);
                    end;
                   { we should check the result type of srsym }
                   if not (srsym^.typ in [varsym,typedconstsym]) then
                     Message(parser_e_absolute_only_to_var_or_const);
                   storetokenpos:=tokenpos;
                   tokenpos:=declarepos;
                   abssym:=new(pabsolutesym,init(s,tt));
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
                    abssym:=new(pabsolutesym,init(s,tt));
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
                       abssym:=new(pabsolutesym,init(s,tt));
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
                  pconstsym:=new(ptypedconstsym,inittype(s,tt,false));
                  symtablestack^.insert(pconstsym);
                  tokenpos:=storetokenpos;
                  consume(_EQUAL);
                  readtypedconst(tt.def,pconstsym,false);
                  symdone:=true;
               end;
             { for a record there doesn't need to be a ; before the END or ) }
             if not((is_record or is_object) and (token in [_END,_RKLAMMER])) then
               consume(_SEMICOLON);
             { procvar handling }
             if (tt.def^.deftype=procvardef) and (tt.def^.typesym=nil) then
               begin
                  newtype:=new(ptypesym,init('unnamed',tt));
                  parse_var_proc_directives(psym(newtype));
                  newtype^.restype.def:=nil;
                  tt.def^.typesym:=nil;
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
{$ifdef fixLeaksOnError}
                   if strContStack.pop <> sc then
                     writeln('problem with strContStack in pdecl (5)');
{$endif fixLeaksOnError}
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
                      if extern_aktvarsym or
                         (symtablestack^.symtabletype in [parasymtable,localsymtable]) then
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
                    aktvarsym:=new(pvarsym,init_dll(s,tt))
                   else
                    aktvarsym:=new(pvarsym,init_C(s,C_name,tt));
                   { set some vars options }
                   if export_aktvarsym then
                    begin
                      inc(aktvarsym^.refs);
{$ifdef INCLUDEOK}
                      include(aktvarsym^.varoptions,vo_is_exported);
{$else}
                      aktvarsym^.varoptions:=aktvarsym^.varoptions+[vo_is_exported];
{$endif}
                    end;
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
                    insert_syms(symtablestack,sc,tt,false);
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
                  { save object option, because we can turn of the sp_published }
                  if (sp_published in current_object_option) and
                    (not((tt.def^.deftype=objectdef) and (pobjectdef(tt.def)^.is_class))) then
                   begin
                     Message(parser_e_cant_publish_that);
                     exclude(current_object_option,sp_published);
                   end
                  else
                   if (sp_published in current_object_option) and
                      not(oo_can_have_published in pobjectdef(tt.def)^.objectoptions) then
                    begin
                      Message(parser_e_only_publishable_classes_can__be_published);
                      exclude(current_object_option,sp_published);
                    end;
                  insert_syms(symtablestack,sc,tt,is_threadvar);
                  current_object_option:=old_current_object_option;
               end;
           end;
         { Check for Case }
         if is_record and (token=_CASE) then
           begin
              maxsize:=0;
              maxalignment:=0;
              consume(_CASE);
              s:=pattern;
              getsym(s,false);
              { may be only a type: }
              if assigned(srsym) and (srsym^.typ in [typesym,unitsym]) then
                read_type(casetype,'')
              else
                begin
                  consume(_ID);
                  consume(_COLON);
                  read_type(casetype,'');
                  symtablestack^.insert(new(pvarsym,init(s,casetype)));
                end;
              if not(is_ordinal(casetype.def)) or is_64bitint(casetype.def)  then
               Message(type_e_ordinal_expr_expected);
              consume(_OF);
{$ifdef UseUnionSymtable}
              UnionSymtable:=new(psymtable,init(recordsymtable));
              UnionSymtable^.next:=symtablestack;
              registerdef:=false;
              UnionDef:=new(precorddef,init(unionsymtable));
              registerdef:=true;
              symtablestack:=UnionSymtable;
{$endif UseUnionSymtable}
              startvarrecsize:=symtablestack^.datasize;
              startvarrecalign:=symtablestack^.dataalignment;
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
                maxalignment:=max(maxalignment,symtablestack^.dataalignment);
                { the items of the next variant are overlayed }
                symtablestack^.datasize:=startvarrecsize;
                symtablestack^.dataalignment:=startvarrecalign;
                if (token<>_END) and (token<>_RKLAMMER) then
                  consume(_SEMICOLON)
                else
                  break;
              until (token=_END) or (token=_RKLAMMER);
              { at last set the record size to that of the biggest variant }
              symtablestack^.datasize:=maxsize;
              symtablestack^.dataalignment:=maxalignment;
{$ifdef UseUnionSymtable}
              uniontype.def:=uniondef;
              uniontype.sym:=nil;
              UnionSym:=new(pvarsym,init('case',uniontype));
              symtablestack:=symtablestack^.next;
              { we do NOT call symtablestack^.insert
               on purpose PM }
              offset:=align_from_size(symtablestack^.datasize,maxalignment);
              symtablestack^.datasize:=offset+unionsymtable^.datasize;
              if maxalignment>symtablestack^.dataalignment then
                symtablestack^.dataalignment:=maxalignment;
              UnionSymtable^.Insert_in(symtablestack,offset);
              UnionSym^.owner:=nil;
              dispose(unionsym,done);
              dispose(uniondef,done);
{$endif UseUnionSymtable}
           end;
         block_type:=old_block_type;
         current_object_option:=old_current_object_option;
      end;


    procedure const_dec;
      var
         name : stringid;
         p : ptree;
         tt  : ttype;
         sym : psym;
         storetokenpos,filepos : tfileposinfo;
         old_block_type : tblock_type;
         ps : pconstset;
         pd : pbestreal;
         sp : pchar;
         skipequal : boolean;
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
                      pointerconstn :
                        begin
                          symtablestack^.insert(new(pconstsym,init_def(name,constpointer,p^.value,p^.resulttype)))
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
                   read_type(tt,'');
                   ignore_equal:=false;
                   block_type:=bt_const;
                   skipequal:=false;
                   { create symbol }
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
                       sym:=new(ptypedconstsym,inittype(name,tt,false))
                     end;
                   tokenpos:=storetokenpos;
                   symtablestack^.insert(sym);
                   { procvar can have proc directives }
                   if (tt.def^.deftype=procvardef) then
                    begin
                      { support p : procedure;stdcall=nil; }
                      if (token=_SEMICOLON) then
                       begin
                         consume(_SEMICOLON);
                         if is_proc_directive(token) then
                          parse_var_proc_directives(sym)
                         else
                          begin
                            Message(parser_e_proc_directive_expected);
                            skipequal:=true;
                          end;
                       end
                      else
                      { support p : procedure stdcall=nil; }
                       begin
                         if is_proc_directive(token) then
                          parse_var_proc_directives(sym);
                       end;
                    end;
                   if not skipequal then
                    begin
                      { get init value }
                      consume(_EQUAL);
{$ifdef DELPHI_CONST_IN_RODATA}
                      if m_delphi in aktmodeswitches then
                       readtypedconst(tt.def,ptypedconstsym(sym),true)
                      else
{$endif DELPHI_CONST_IN_RODATA}
                       readtypedconst(tt.def,ptypedconstsym(sym),false);
                      consume(_SEMICOLON);
                    end;
                end;

              else
                { generate an error }
                consume(_EQUAL);
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
                if (cs_create_smart in aktmoduleswitches) then
                  begin
                    getdatalabel(hl);
                    { we still want a warning if unused }
                    hl^.refs:=0;
                  end
                else
                  getlabel(hl);
                symtablestack^.insert(new(plabelsym,init(pattern,hl)));
                consume(token);
             end;
           if token<>_SEMICOLON then consume(_COMMA);
         until not(token in [_ID,_INTCONST]);
         consume(_SEMICOLON);
      end;


    { search in symtablestack used, but not defined type }
    procedure resolve_type_forward(p : pnamedindexobject);{$ifndef FPC}far;{$endif}
      var
        hpd,pd : pdef;
        stpos  : tfileposinfo;
        again  : boolean;
      begin
         { Check only typesyms or record/object fields }
         case psym(p)^.typ of
           typesym :
             pd:=ptypesym(p)^.restype.def;
           varsym :
             if (psym(p)^.owner^.symtabletype in [objectsymtable,recordsymtable]) then
               pd:=pvarsym(p)^.vartype.def
             else
               exit;
           else
             exit;
         end;
         repeat
           again:=false;
           case pd^.deftype of
             arraydef :
               begin
                 { elementtype could also be defined using a forwarddef }
                 pd:=parraydef(pd)^.elementtype.def;
                 again:=true;
               end;
             pointerdef,
             classrefdef :
               begin
                 { classrefdef inherits from pointerdef }
                 hpd:=ppointerdef(pd)^.pointertype.def;
                 { still a forward def ? }
                 if hpd^.deftype=forwarddef then
                  begin
                    { try to resolve the forward }
                    { get the correct position for it }
                    stpos:=tokenpos;
                    tokenpos:=pforwarddef(hpd)^.forwardpos;
                    resolving_forward:=true;
                    make_ref:=false;
                    getsym(pforwarddef(hpd)^.tosymname,false);
                    make_ref:=true;
                    resolving_forward:=false;
                    tokenpos:=stpos;
                    { we don't need the forwarddef anymore, dispose it }
                    dispose(hpd,done);
                    { was a type sym found ? }
                    if assigned(srsym) and
                       (srsym^.typ=typesym) then
                     begin
                       ppointerdef(pd)^.pointertype.setsym(srsym);
                       { avoid wrong unused warnings web bug 801 PM }
                       inc(srsym^.refs);
{$ifdef GDB}
                       if (cs_debuginfo in aktmoduleswitches) and assigned(debuglist) and
                          (psym(p)^.owner^.symtabletype in [globalsymtable,staticsymtable]) then
                        begin
                          ptypesym(p)^.isusedinstab := true;
                          psym(p)^.concatstabto(debuglist);
                        end;
{$endif GDB}
                       { we need a class type for classrefdef }
                       if (pd^.deftype=classrefdef) and
                          not((ptypesym(srsym)^.restype.def^.deftype=objectdef) and
                              pobjectdef(ptypesym(srsym)^.restype.def)^.is_class) then
                         Message1(type_e_class_type_expected,ptypesym(srsym)^.restype.def^.typename);
                     end
                    else
                     begin
                       MessagePos1(psym(p)^.fileinfo,sym_e_forward_type_not_resolved,p^.name);
                       { try to recover }
                       ppointerdef(pd)^.pointertype.def:=generrordef;
                     end;
                  end;
               end;
             recorddef :
               precorddef(pd)^.symtable^.foreach({$ifndef TP}@{$endif}resolve_type_forward);
             objectdef :
               { Don't check objectdefs in objects/records, because these can't
                 exist (anonymous objects aren't allowed) }
               if not(psym(p)^.owner^.symtabletype in [objectsymtable,recordsymtable]) then
                pobjectdef(pd)^.symtable^.foreach({$ifndef TP}@{$endif}resolve_type_forward);
          end;
        until not again;
      end;

    { reads a type declaration to the symbol table }
    procedure type_dec;

      var
         typename : stringid;
         newtype  : ptypesym;
         sym      : psym;
         tt       : ttype;
         defpos,storetokenpos : tfileposinfo;
         old_block_type : tblock_type;
      begin
         old_block_type:=block_type;
         block_type:=bt_type;
         consume(_TYPE);
         typecanbeforward:=true;
         repeat
           typename:=pattern;
           defpos:=tokenpos;
           consume(_ID);
           consume(_EQUAL);
           { support 'ttype=type word' syntax }
           if token=_TYPE then
            Consume(_TYPE);
           { is the type already defined? }
           getsym(typename,false);
           sym:=srsym;
           newtype:=nil;
           { found a symbol with this name? }
           if assigned(sym) then
            begin
              if (sym^.typ=typesym) then
               begin
                 if (token=_CLASS) and
                    (assigned(ptypesym(sym)^.restype.def)) and
                    (ptypesym(sym)^.restype.def^.deftype=objectdef) and
                    pobjectdef(ptypesym(sym)^.restype.def)^.is_class and
                    (oo_is_forward in pobjectdef(ptypesym(sym)^.restype.def)^.objectoptions) then
                  begin
                    { we can ignore the result   }
                    { the definition is modified }
                    object_dec(typename,pobjectdef(ptypesym(sym)^.restype.def));
                    newtype:=ptypesym(sym);
                  end;
               end;
            end;
           { no old type reused ? Then insert this new type }
           if not assigned(newtype) then
            begin
              read_type(tt,typename);
              storetokenpos:=tokenpos;
              tokenpos:=defpos;
              newtype:=new(ptypesym,init(typename,tt));
              symtablestack^.insert(newtype);
              tokenpos:=storetokenpos;
            end;
           if assigned(newtype^.restype.def) and
              (newtype^.restype.def^.deftype=procvardef) then
            begin
              if not is_proc_directive(token) then
               consume(_SEMICOLON);
              parse_var_proc_directives(psym(newtype));
            end
           else
            consume(_SEMICOLON);
         until token<>_ID;
         typecanbeforward:=false;
         symtablestack^.foreach({$ifndef TP}@{$endif}resolve_type_forward);
         block_type:=old_block_type;
      end;


    procedure var_dec;
    { parses variable declarations and inserts them in }
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
                      (current_module^.is_unit) then
                     begin
                        Message(parser_e_syntax_error);
                        consume_all_until(_SEMICOLON);
                     end
                   else if islibrary or (target_info.target=target_i386_WIN32) then
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
  Revision 1.189  2000-07-03 13:26:48  pierre
   * fix for bug 1023

  Revision 1.188  2000/06/23 21:34:09  pierre
   * align all variants to same start address

  Revision 1.187  2000/06/23 20:14:39  peter
    * reset current_object_option when reading other symtables than
      object declarations

  Revision 1.186  2000/06/18 18:11:32  peter
    * C record packing fixed to also check first entry of the record
      if bigger than the recordalignment itself
    * variant record alignment uses alignment per variant and saves the
      highest alignment value

  Revision 1.185  2000/06/11 06:59:36  peter
    * support procvar directive without ; before the directives

  Revision 1.184  2000/06/09 21:34:40  peter
    * checking for dup id with para of methods fixed for delphi mode

  Revision 1.183  2000/06/02 21:18:13  pierre
   + set vo_is_exported for vars

  Revision 1.182  2000/06/01 19:14:09  peter
    * symtable.insert changed to procedure

  Revision 1.181  2000/04/17 18:44:22  peter
    * fixed forward resolving with redefined types

  Revision 1.180  2000/02/09 13:22:56  peter
    * log truncated

  Revision 1.179  2000/01/20 12:29:02  pierre
   * bug 801 fixed

  Revision 1.178  2000/01/11 17:16:05  jonas
    * removed a lot of memory leaks when an error is encountered (caused by
      procinfo and pstringcontainers). There are still plenty left though :)

  Revision 1.177  2000/01/10 11:14:19  peter
    * fixed memory leak with options, you must use StopOptions instead of
      Stop
    * fixed memory leak with forward resolving, make_ref is now false

  Revision 1.176  2000/01/07 01:14:28  peter
    * updated copyright to 2000

  Revision 1.175  1999/12/10 10:04:21  peter
    * also check elementtype of arraydef for forwarddef

  Revision 1.174  1999/12/01 12:42:32  peter
    * fixed bug 698
    * removed some notes about unused vars

  Revision 1.173  1999/11/30 10:40:44  peter
    + ttype, tsymlist

  Revision 1.172  1999/11/29 15:18:27  pierre
   + allow exports in win32 executables

  Revision 1.171  1999/11/09 23:43:08  pierre
   * better browser info

  Revision 1.170  1999/11/09 23:06:45  peter
    * esi_offset -> selfpointer_offset to be newcg compatible
    * hcogegen -> cgbase fixes for newcg

  Revision 1.169  1999/11/09 12:58:29  peter
    * support absolute unit.variable

  Revision 1.168  1999/11/06 14:34:21  peter
    * truncated log to 20 revs

  Revision 1.167  1999/10/26 12:30:44  peter
    * const parameter is now checked
    * better and generic check if a node can be used for assigning
    * export fixes
    * procvar equal works now (it never had worked at least from 0.99.8)
    * defcoll changed to linkedlist with pparaitem so it can easily be
      walked both directions

  Revision 1.166  1999/10/22 10:39:34  peter
    * split type reading from pdecl to ptype unit
    * parameter_dec routine is now used for procedure and procvars

  Revision 1.165  1999/10/21 16:41:41  florian
    * problems with readln fixed: esi wasn't restored correctly when
      reading ordinal fields of objects futher the register allocation
      didn't take care of the extra register when reading ordinal values
    * enumerations can now be used in constant indexes of properties

  Revision 1.164  1999/10/14 14:57:52  florian
    - removed the hcodegen use in the new cg, use cgbase instead

  Revision 1.163  1999/10/06 17:39:14  peter
    * fixed stabs writting for forward types

  Revision 1.162  1999/10/03 19:44:42  peter
    * removed objpasunit reference, tvarrec is now searched in systemunit
      where it already was located

  Revision 1.161  1999/10/01 11:18:02  peter
    * class/record type forward checking fixed

  Revision 1.159  1999/10/01 10:05:42  peter
    + procedure directive support in const declarations, fixes bug 232

}