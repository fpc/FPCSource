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
       cutils,symconst,aasm,tree,pass_1,strings,
       fmodule,types,verbose,systems,import,
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


    function readconstant(const name:string;const filepos:tfileposinfo):pconstsym;
      var
        hp : pconstsym;
        p : ptree;
        ps : pconstset;
        pd : pbestreal;
        sp : pchar;
        storetokenpos : tfileposinfo;
      begin
        readconstant:=nil;
        if name='' then
         internalerror(9584582);
        hp:=nil;
        p:=comp_expr(true);
        do_firstpass(p);
        storetokenpos:=tokenpos;
        tokenpos:=filepos;
        case p^.treetype of
           ordconstn:
             begin
                if is_constintnode(p) then
                  hp:=new(pconstsym,init_def(name,constint,p^.value,nil))
                else if is_constcharnode(p) then
                  hp:=new(pconstsym,init_def(name,constchar,p^.value,nil))
                else if is_constboolnode(p) then
                  hp:=new(pconstsym,init_def(name,constbool,p^.value,nil))
                else if p^.resulttype^.deftype=enumdef then
                  hp:=new(pconstsym,init_def(name,constord,p^.value,p^.resulttype))
                else if p^.resulttype^.deftype=pointerdef then
                  hp:=new(pconstsym,init_def(name,constord,p^.value,p^.resulttype))
                else internalerror(111);
             end;
           stringconstn:
             begin
                getmem(sp,p^.length+1);
                move(p^.value_str^,sp^,p^.length+1);
                hp:=new(pconstsym,init_string(name,conststring,sp,p^.length));
             end;
           realconstn :
             begin
                new(pd);
                pd^:=p^.value_real;
                hp:=new(pconstsym,init(name,constreal,longint(pd)));
             end;
           setconstn :
             begin
               new(ps);
               ps^:=p^.value_set^;
               hp:=new(pconstsym,init_def(name,constset,longint(ps),p^.resulttype));
             end;
           pointerconstn :
             begin
               hp:=new(pconstsym,init_def(name,constpointer,p^.value,p^.resulttype));
             end;
           niln :
             begin
               hp:=new(pconstsym,init_def(name,constnil,0,p^.resulttype));
             end;
           else
             Message(cg_e_illegal_expression);
        end;
        tokenpos:=storetokenpos;
        disposetree(p);
        readconstant:=hp;
      end;


    procedure parameter_dec(aktprocdef:pabstractprocdef);
      {
        handle_procvar needs the same changes
      }
      var
        is_procvar : boolean;
        sc      : Pstringcontainer;
        s       : string;
        hpos,
        storetokenpos : tfileposinfo;
        tt      : ttype;
        hvs,
        vs      : Pvarsym;
        hs1,hs2 : string;
        varspez : Tvarspez;
        inserthigh : boolean;
        pdefaultvalue : pconstsym;
        defaultrequired : boolean;
      begin
        { reset }
        defaultrequired:=false;
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
            if try_to_consume(_OUT) then
              varspez:=vs_out
          else
              varspez:=vs_value;
          inserthigh:=false;
          pdefaultvalue:=nil;
          tt.reset;
          { self is only allowed in procvars and class methods }
          if (idtoken=_SELF) and
             (is_procvar or
              (assigned(procinfo^._class) and procinfo^._class^.is_class)) then
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
                 include(aktprocdef^.procoptions,po_containsself);
                 inc(procinfo^.selfpointer_offset,vs^.address);
               end;
              consume(idtoken);
              consume(_COLON);
              single_type(tt,hs1,false);
              aktprocdef^.concatpara(tt,vs_value,nil);
              { check the types for procedures only }
              if not is_procvar then
               CheckTypes(tt.def,procinfo^._class);
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
                  else
                   begin
                     { open string ? }
                     if (varspez=vs_var) and
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
                     else
                      begin
                        { everything else }
                        single_type(tt,hs1,false);
                      end;
                     { default parameter }
                     if (m_default_para in aktmodeswitches) then
                      begin
                        if try_to_consume(_EQUAL) then
                         begin
                           s:=sc^.get_with_tokeninfo(hpos);
                           if not sc^.empty then
                            Comment(V_Error,'default value only allowed for one parameter');
                           sc^.insert_with_tokeninfo(s,hpos);
                           { prefix 'def' to the parameter name }
                           pdefaultvalue:=ReadConstant('$def'+Upper(s),hpos);
                           if assigned(pdefaultvalue) then
                            pprocdef(aktprocdef)^.parast^.insert(pdefaultvalue);
                           defaultrequired:=true;
                         end
                        else
                         begin
                           if defaultrequired then
                            Comment(V_Error,'default parameter required');
                         end;
                      end;
                   end;
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
                  aktprocdef^.concatpara(tt,varspez,pdefaultvalue);
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
                   { I don't understand the comment above,                          }
                   { but I suppose the comment is wrong and                         }
                   { it means that the address of var parameters can be placed      }
                   { in a register (FK)                                             }
                     if (varspez in [vs_var,vs_const,vs_out]) and push_addr_param(tt.def) then
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
                        hvs:=new(Pvarsym,initdef('$high'+Upper(s),s32bitdef));
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
                  include(ss^.varoptions,vo_is_thread_var);
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
                  include(aktvarsym^.varoptions,vo_is_external);
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
                   if not (srsym^.typ in [varsym,typedconstsym,funcretsym]) then
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
                      include(aktvarsym^.varoptions,vo_is_exported);
                    end;
                   if extern_aktvarsym then
                    include(aktvarsym^.varoptions,vo_is_external);
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
                    include(current_object_option,sp_static);
                    insert_syms(symtablestack,sc,tt,false);
                    exclude(current_object_option,sp_static);
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
         tt  : ttype;
         sym : psym;
         storetokenpos,filepos : tfileposinfo;
         old_block_type : tblock_type;
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
                   sym:=readconstant(name,filepos);
                   if assigned(sym) then
                    symtablestack^.insert(sym);
                   consume(_SEMICOLON);
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
               begin
                 if not(m_fpc in aktmodeswitches) and
                    (oo_is_forward in pobjectdef(pd)^.objectoptions) then
                  begin
                    { only give an error as the implementation may follow in an
                      other type block which is allowed by FPC modes }
                    MessagePos1(psym(p)^.fileinfo,sym_e_forward_type_not_resolved,p^.name);
                  end
                 else
                  begin
                    { Check all fields of the object declaration, but don't
                      check objectdefs in objects/records, because these
                      can't exist (anonymous objects aren't allowed) }
                    if not(psym(p)^.owner^.symtabletype in [objectsymtable,recordsymtable]) then
                     pobjectdef(pd)^.symtable^.foreach({$ifndef TP}@{$endif}resolve_type_forward);
                  end;
               end;
          end;
        until not again;
      end;

    { reads a type declaration to the symbol table }
    procedure type_dec;

      var
         typename,orgtypename : stringid;
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
           orgtypename:=orgpattern;
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
                    object_dec(orgtypename,pobjectdef(ptypesym(sym)^.restype.def));
                    newtype:=ptypesym(sym);
                  end;
               end;
            end;
           { no old type reused ? Then insert this new type }
           if not assigned(newtype) then
            begin
              { insert the new type first with an errordef, so that
                referencing the type before it's really set it
                will give an error (PFV) }
              tt.setdef(generrordef);
              storetokenpos:=tokenpos;
              newtype:=new(ptypesym,init(orgtypename,tt));
              symtablestack^.insert(newtype);
              tokenpos:=defpos;
              tokenpos:=storetokenpos;
              { read the type definition }
              read_type(tt,orgtypename);
              { update the definition of the type }
              newtype^.restype:=tt;
              if not assigned(tt.sym) then
                tt.sym:=newtype;
              if assigned(tt.def) and not assigned(tt.def^.typesym) then
                tt.def^.typesym:=newtype;
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
              exclude(aktprocsym^.definition^.proccalloptions,pocall_inline);
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
                   else if islibrary or (target_info.target=target_i386_WIN32)
                   or (target_info.target=target_i386_Netware) then  // AD
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
  Revision 1.14  2000-09-11 17:00:23  florian
    + first implementation of Netware Module support, thanks to
      Armin Diehl (diehl@nordrhein.de) for providing the patches

  Revision 1.13  2000/08/27 20:19:39  peter
    * store strings with case in ppu, when an internal symbol is created
      a '$' is prefixed so it's not automatic uppercased

  Revision 1.12  2000/08/27 16:11:51  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.11  2000/08/20 15:01:17  peter
    * don't allow forward class in separate type blocks for delphi (merged)

  Revision 1.10  2000/08/17 09:17:19  pierre
   * fix go32v2 cycle problem

  Revision 1.9  2000/08/16 18:33:53  peter
    * splitted namedobjectitem.next into indexnext and listnext so it
      can be used in both lists
    * don't allow "word = word" type definitions (merged)

  Revision 1.8  2000/08/13 13:11:28  peter
    * put defaultpara values in parast and changed the name to
      'def<Parameter name>'

  Revision 1.7  2000/08/13 08:42:59  peter
    * support absolute refering to funcret (merged)

  Revision 1.6  2000/08/02 19:49:59  peter
    * first things for default parameters

  Revision 1.5  2000/07/30 17:04:43  peter
    * merged fixes

  Revision 1.4  2000/07/14 05:11:49  michael
  + Patch to 1.1

  Revision 1.3  2000/07/13 12:08:26  michael
  + patched to 1.1.0 with former 1.09patch from peter

  Revision 1.2  2000/07/13 11:32:44  michael
  + removed logs

}