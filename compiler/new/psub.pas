{
    $Id$
    Copyright (c) 1998 by Florian Klaempfl, Daniel Mantione

    Does the parsing of the procedures/functions

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
unit psub;
interface

uses cobjects,symtable;

const
  pd_global    = $1;    { directive must be global }
  pd_body      = $2;    { directive needs a body }
  pd_implemen  = $4;    { directive can be used implementation section }
  pd_interface = $8;    { directive can be used interface section }
  pd_object    = $10;   { directive can be used object declaration }
  pd_procvar   = $20;   { directive can be used procvar declaration }

procedure compile_proc_body(const proc_names:Tstringcontainer;
                            make_global,parent_has_class:boolean);
procedure parse_proc_head(options : word);
procedure parse_proc_dec;
procedure parse_var_proc_directives(var sym : ptypesym);
procedure read_proc;


implementation

uses
  globtype,systems,tokens,
  strings,globals,verbose,comphook,files,
  scanner,aasm,tree,types,
  import,gendef,
  hcodegen,temp_gen,pass_1,pass_2
{$ifdef GDB}
  ,gdb
{$endif GDB}
{$ifdef i386}
  ,i386,tgeni386
  {$ifndef NoOpt}
  ,aopt386
  {$endif}
{$endif}
{$ifdef m68k}
  ,m68k,tgen68k,cga68k
{$endif}
  { parser specific stuff }
  ,pbase,pdecl,pexpr,pstatmnt
  ;

var
  realname:string;  { contains the real name of a procedure as it's typed }


procedure formal_parameter_list;
{
  handle_procvar needs the same changes
}
var
  sc      : Pstringcontainer;
  s       : string;
  filepos : tfileposinfo;
  p       : Pdef;
  vs      : Pvarsym;
{$ifdef VALUEPARA}
  l       : longint;
{$endif}
  hs1,hs2 : string;
  varspez : Tvarspez;
begin
  consume(LKLAMMER);
  inc(testcurobject);
  repeat
    case token of
     _VAR : begin
              consume(_VAR);
              varspez:=vs_var;
            end;
   _CONST : begin
              consume(_CONST);
              varspez:=vs_const;
            end;
    else
      varspez:=vs_value;
    end;

  { read identifiers }

    sc:=idlist;
  { read type declaration, force reading for value and const paras }

    if (token=COLON) or (varspez=vs_value) then
     begin
       consume(COLON);
     { check for an open array }
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
             hs1:='array_of_const';
           end
          else
           begin
           { define field type }
             Parraydef(p)^.definition:=single_type(hs1);
             hs1:='array_of_'+hs1;
           end;
        end
       { open string ? }
       else if ((token=_STRING) or (idtoken=_SHORTSTRING)) and
               (varspez=vs_var) and
               (cs_openstring in aktmoduleswitches) and
               not(cs_ansistrings in aktlocalswitches) then
        begin
          consume(token);
          p:=openshortstringdef;
          hs1:='openstring';
        end
       { everything else }
       else
        p:=single_type(hs1);
     end
    else
     begin
{$ifndef UseNiceNames}
       hs1:='$$$';
{$else UseNiceNames}
       hs1:='var';
{$endif UseNiceNames}
       p:=new(Pformaldef,init);
     end;
    hs2:=aktprocsym^.definition^.mangledname;
    while not sc^.empty do
     begin
       s:=sc^.get_with_tokeninfo(filepos);
       aktprocsym^.definition^.concatdef(p,varspez);
{$ifndef UseNiceNames}
       hs2:=hs2+'$'+hs1;
{$else UseNiceNames}
       hs2:=hs2+tostr(length(hs1))+hs1;
{$endif UseNiceNames}
       vs:=new(Pvarsym,init(s,p));
       vs^.fileinfo:=filepos;
       vs^.varspez:=varspez;
     { we have to add this to avoid var param to be in registers !!!}
{$ifndef VALUEPARA}
       if (varspez in [vs_var,vs_const]) and dont_copy_const_param(p) then
         vs^.var_options := vs^.var_options or vo_regable;
     { search for duplicate ids in object members/methods    }
     { but only the current class, I don't know why ...      }
     { at least TP and Delphi do it in that way         (FK) }
       if assigned(procinfo._class) and (lexlevel=normal_function_level) and
          (procinfo._class^.publicsyms^.search(vs^.name)<>nil) then
      {   (search_class_member(procinfo._class,vs^.name)<>nil) then }
         Message1(sym_e_duplicate_id,vs^.name);
       aktprocsym^.definition^.parast^.insert(vs);
{$else}
       if (varspez in [vs_var,vs_const]) and push_addr_param(p) then
         vs^.var_options := vs^.var_options or vo_regable;

       { search for duplicate ids in object members/methods    }
       { but only the current class, I don't know why ...      }
       { at least TP and Delphi do it in that way         (FK) }
       if assigned(procinfo._class) and (lexlevel=normal_function_level) and
          (procinfo._class^.publicsyms^.search(vs^.name)<>nil) then
      {   (search_class_member(procinfo._class,vs^.name)<>nil) then }
         Message1(sym_e_duplicate_id,vs^.name);

       { when it is a value para and it needs a local copy then rename
         the parameter and insert a copy in the localst }
       if (varspez=vs_value) and push_addr_param(p) then
         begin
           vs^.islocalcopy:=true;
           aktprocsym^.definition^.localst^.insert(vs);
           vs^.is_valid:=1;
           l:=vs^.address; { save local address }
           vs:=new(Pvarsym,init('val'+s,p));
           vs^.fileinfo:=filepos;
           vs^.varspez:=varspez;
           vs^.localaddress:=l;
           aktprocsym^.definition^.parast^.insert(vs);
         end
       else
         aktprocsym^.definition^.parast^.insert(vs);
{$endif}
     end;
    dispose(sc,done);
    aktprocsym^.definition^.setmangledname(hs2);
    if token=SEMICOLON then
      consume(SEMICOLON)
    else
      break;
  until false;
  dec(testcurobject);
  consume(RKLAMMER);
end;



procedure parse_proc_head(options : word);
var sp:stringid;
    pd:Pprocdef;
    paramoffset:longint;
    sym:Psym;
    hs:string;
    overloaded_level:word;
    realfilepos : tfileposinfo;
begin
  if (options and pooperator) <> 0 then
    begin
      sp:=overloaded_names[optoken];
      realname:=sp;
    end
  else
    begin
      sp:=pattern;
      realname:=orgpattern;
      realfilepos:=aktfilepos;
      consume(ID);
    end;

{ method ? }
  if (token=POINT) and not(parse_only) then
   begin
     consume(POINT);
     getsym(sp,true);
     sym:=srsym;
     { qualifier is class name ? }
     if (sym^.typ<>typesym) or
        (ptypesym(sym)^.definition^.deftype<>objectdef) then
       begin
          Message(parser_e_class_id_expected);
          aktprocsym:=nil;
          consume(ID);
       end
     else
       begin
          { used to allow private syms to be seen }
          aktobjectdef:=pobjectdef(ptypesym(sym)^.definition);
          sp:=pattern;
          realname:=orgpattern;
          consume(ID);
          procinfo._class:=pobjectdef(ptypesym(sym)^.definition);
          aktprocsym:=pprocsym(procinfo._class^.publicsyms^.search(sp));
          aktobjectdef:=nil;
          { we solve this below }
          if not(assigned(aktprocsym)) then
            Message(parser_e_methode_id_expected);
       end;
   end
  else
   begin
     { check for constructor/destructor which is not allowed here }
     if (not parse_only) and
        ((options and (poconstructor or podestructor))<>0) then
        Message(parser_e_constructors_always_objects);

     aktprocsym:=pprocsym(symtablestack^.search(sp));

     if lexlevel=normal_function_level then
{$ifdef UseNiceNames}
       hs:=procprefix+'_'+tostr(length(sp))+sp
{$else UseNiceNames}
       hs:=procprefix+'_'+sp
{$endif UseNiceNames}
     else
{$ifdef UseNiceNames}
       hs:=lowercase(procprefix)+'_'+tostr(length(sp))+sp;
{$else UseNiceNames}
       hs:=procprefix+'_$'+sp;
{$endif UseNiceNames}
     if not(parse_only) then
       begin
         {The procedure we prepare for is in the implementation
          part of the unit we compile. It is also possible that we
          are compiling a program, which is also some kind of
          implementaion part.

          We need to find out if the procedure is global. If it is
          global, it is in the global symtable.}
         if not assigned(aktprocsym) then
          begin
            {Search the procedure in the global symtable.}
            aktprocsym:=Pprocsym(search_a_symtable(sp,globalsymtable));
            if assigned(aktprocsym) then
             begin
               {Check if it is a procedure.}
               if aktprocsym^.typ<>procsym then
                Message1(sym_e_duplicate_id,aktprocsym^.Name);
               {The procedure has been found. So it is
                a global one. Set the flags to mark this.}
               procinfo.flags:=procinfo.flags or pi_is_global;
             end;
          end;
       end;
   end;
  { problem with procedures inside methods }
{$ifndef UseNiceNames}
  if assigned(procinfo._class) then
    if (pos('_$$_',procprefix)=0) then
      hs:=procprefix+'_$$_'+procinfo._class^.name^+'_'+sp
    else
      hs:=procprefix+'_$'+sp;
{$else UseNiceNames}
  if assigned(procinfo._class) then
    if (pos('_5Class_',procprefix)=0) then
      hs:=procprefix+'_5Class_'+procinfo._class^.name^+'_'+tostr(length(sp))+sp
    else
      hs:=procprefix+'_'+tostr(length(sp))+sp;
{$endif UseNiceNames}

  if assigned(aktprocsym) then
   begin
     { Check if overloading is enabled }
     if not(m_fpc in aktmodeswitches) then
      begin
        if aktprocsym^.typ<>procsym then
         begin
           Message1(sym_e_duplicate_id,aktprocsym^.name);
           { try to recover by creating a new aktprocsym }
           aktprocsym:=new(pprocsym,init(sp));
         end
        else
         begin
           if not(aktprocsym^.definition^.forwarddef) then
            Message(parser_e_procedure_overloading_is_off);
         end;
      end
     else
      begin
        { Check if the overloaded sym is realy a procsym }
        if aktprocsym^.typ<>procsym then
         begin
           Message1(parser_e_overloaded_no_procedure,aktprocsym^.name);
           { try to recover by creating a new aktprocsym }
           aktprocsym:=new(pprocsym,init(sp));
         end;
      end;
   end
  else
   begin
     { create a new procsym and set the real filepos }
     aktprocsym:=new(pprocsym,init(sp));
     aktprocsym^.fileinfo:=realfilepos;
     { for operator we have only one definition for each overloaded
       operation }
     if ((options and pooperator) <> 0) then
       begin
          { the only problem is that nextoverloaded might not be in a unit
            known for the unit itself }
          if assigned(overloaded_operators[optoken]) then
            aktprocsym^.definition:=overloaded_operators[optoken]^.definition;
       end;
     symtablestack^.insert(aktprocsym);
   end;

{ create a new procdef }
  pd:=new(pprocdef,init);
  if assigned(procinfo._class) then
    pd^._class := procinfo._class;

  { set the options from the caller (podestructor or poconstructor) }
  pd^.options:=pd^.options or options;

  { calculate the offset of the parameters }
  paramoffset:=8;

  { calculate frame pointer offset }
  if lexlevel>normal_function_level then
    begin
      procinfo.framepointer_offset:=paramoffset;
      inc(paramoffset,target_os.size_of_pointer);
    end;

  if assigned (Procinfo._Class) and not(procinfo._class^.isclass) and
     (((pd^.options and poconstructor)<>0) or ((pd^.options and podestructor)<>0)) then
     inc(paramoffset,target_os.size_of_pointer);

  { self pointer offset                              }
  { self isn't pushed in nested procedure of methods }
  if assigned(procinfo._class) and (lexlevel=normal_function_level) then
    begin
      procinfo.ESI_offset:=paramoffset;
      inc(paramoffset,target_os.size_of_pointer);
    end;

  procinfo.call_offset:=paramoffset;

  pd^.parast^.datasize:=0;

  pd^.nextoverloaded:=aktprocsym^.definition;
  aktprocsym^.definition:=pd;
  aktprocsym^.definition^.setmangledname(hs);

  overloaded_level:=1;
  if assigned(pd^.nextoverloaded) and
     (pd^.nextoverloaded^.owner^.symtabletype in [globalsymtable,staticsymtable]) then
    begin
       { we need another procprefix !!! }
       { count, but only those in the same unit !!}
       while assigned(pd^.nextoverloaded) and
        (pd^.nextoverloaded^.owner^.symtabletype in [globalsymtable,staticsymtable]) do
        begin
          { only count already implemented functions }
          if  not(pd^.forwarddef) then
            inc(overloaded_level);
          pd:=pd^.nextoverloaded;
        end;
     end;
  if not parse_only then
    procprefix:=hs+'$'+tostr(overloaded_level)+'$';

  if token=LKLAMMER then
    formal_parameter_list;
  if ((options and pooperator)<>0) {and (overloaded_operators[optoken]=nil) } then
    overloaded_operators[optoken]:=aktprocsym;
end;


procedure parse_proc_dec;
var
  hs : string;
  isclassmethod : boolean;
begin
  inc(lexlevel);
{ read class method }
  if token=_CLASS then
   begin
     consume(_CLASS);
     isclassmethod:=true;
   end
  else
   isclassmethod:=false;
  case token of
     _FUNCTION : begin
                   consume(_FUNCTION);
                   parse_proc_head(0);
                   if token<>COLON then
                    begin
                      if not(aktprocsym^.definition^.forwarddef) and
                         not(m_repeat_forward in aktmodeswitches) then
                       begin
                         consume(COLON);
                         consume_all_until(SEMICOLON);
                       end;
                    end
                   else
                    begin
                      consume(COLON);
                      aktprocsym^.definition^.retdef:=single_type(hs);
                      aktprocsym^.definition^.test_if_fpu_result;
                    end;
                 end;
    _PROCEDURE : begin
                   consume(_PROCEDURE);
                   parse_proc_head(0);
                   aktprocsym^.definition^.retdef:=voiddef;
                 end;
  _CONSTRUCTOR : begin
                   consume(_CONSTRUCTOR);
                   parse_proc_head(poconstructor);
                   if (procinfo._class^.options and oo_is_class)<>0 then
                    begin
                      { CLASS constructors return the created instance }
                      aktprocsym^.definition^.retdef:=procinfo._class;
                    end
                   else
                    begin
                      { OBJECT constructors return a boolean }
{$IfDef GDB}
                      { GDB doesn't like unnamed types !}
                      aktprocsym^.definition^.retdef:=globaldef('boolean');
{$Else GDB}
                      aktprocsym^.definition^.retdef:=new(porddef,init(bool8bit,0,1));
{$Endif GDB}
                    end;
                 end;
   _DESTRUCTOR : begin
                   consume(_DESTRUCTOR);
                   parse_proc_head(podestructor);
                   aktprocsym^.definition^.retdef:=voiddef;
                 end;
     _OPERATOR : begin
                   if lexlevel>normal_function_level then
                     Message(parser_e_no_local_operator);
                   consume(_OPERATOR);
                   if not(token in [PLUS..last_overloaded]) then
                     Message(parser_e_overload_operator_failed);
                   optoken:=token;
                   consume(Token);
                   procinfo.flags:=procinfo.flags or pi_operator;
                   parse_proc_head(pooperator);
                   if token<>ID then
                     consume(ID)
                   else
                     begin
                       opsym:=new(pvarsym,init(pattern,voiddef));
                       consume(ID);
                     end;
                   if token<>COLON then
                     begin
                       consume(COLON);
                       aktprocsym^.definition^.retdef:=generrordef;
                       consume_all_until(SEMICOLON);
                     end
                   else
                    begin
                      consume(COLON);
                      aktprocsym^.definition^.retdef:=
                       single_type(hs);
                      aktprocsym^.definition^.test_if_fpu_result;
                      if (optoken in [EQUAL,GT,LT,GTE,LTE]) and
                         ((aktprocsym^.definition^.retdef^.deftype<>
                         orddef) or (porddef(aktprocsym^.definition^.
                         retdef)^.typ<>bool8bit)) then
                        Message(parser_e_comparative_operator_return_boolean);
                       opsym^.definition:=aktprocsym^.definition^.retdef;
                     end;
                 end;
  end;
  if isclassmethod and
     assigned(aktprocsym) then
    aktprocsym^.definition^.options:=aktprocsym^.definition^.options or poclassmethod;
  consume(SEMICOLON);
  dec(lexlevel);
end;


{****************************************************************************
                        Procedure directive handlers
****************************************************************************}

{$ifdef tp}
  {$F+}
{$endif}

procedure pd_far(const procnames:Tstringcontainer);
begin
  Message(parser_w_proc_far_ignored);
end;

procedure pd_near(const procnames:Tstringcontainer);
begin
  Message(parser_w_proc_near_ignored);
end;

procedure pd_export(const procnames:Tstringcontainer);
begin
  procnames.insert(realname);
  procinfo.exported:=true;
  if cs_link_deffile in aktglobalswitches then
    deffile.AddExport(aktprocsym^.definition^.mangledname);
  if assigned(procinfo._class) then
    Message(parser_e_methods_dont_be_export);
  if lexlevel<>normal_function_level then
    Message(parser_e_dont_nest_export);
end;

procedure pd_inline(const procnames:Tstringcontainer);
begin
  if not(cs_support_inline in aktmoduleswitches) then
   Message(parser_e_proc_inline_not_supported);
end;

procedure pd_forward(const procnames:Tstringcontainer);
begin
  aktprocsym^.definition^.forwarddef:=true;
  aktprocsym^.properties:=aktprocsym^.properties or sp_forwarddef;
end;

procedure pd_stdcall(const procnames:Tstringcontainer);
begin
end;

procedure pd_alias(const procnames:Tstringcontainer);
begin
  consume(COLON);
  procnames.insert(get_stringconst);
end;

procedure pd_asmname(const procnames:Tstringcontainer);
begin
  aktprocsym^.definition^.setmangledname(target_os.Cprefix+pattern);
  if token=CCHAR then
    consume(CCHAR)
  else
    consume(CSTRING);
  { we don't need anything else }
  aktprocsym^.definition^.forwarddef:=false;
end;

procedure pd_intern(const procnames:Tstringcontainer);
begin
  consume(COLON);
  aktprocsym^.definition^.extnumber:=get_intconst;
end;

procedure pd_system(const procnames:Tstringcontainer);
begin
  aktprocsym^.definition^.setmangledname(realname);
end;


procedure pd_cdecl(const procnames:Tstringcontainer);
begin
  if aktprocsym^.definition^.deftype<>procvardef then
    aktprocsym^.definition^.setmangledname(target_os.Cprefix+realname);
end;


procedure pd_register(const procnames:Tstringcontainer);
begin
  Message(parser_w_proc_register_ignored);
end;


procedure pd_syscall(const procnames:Tstringcontainer);
begin
  aktprocsym^.definition^.options:=aktprocsym^.definition^.options or poclearstack;
  aktprocsym^.definition^.forwarddef:=false;
  aktprocsym^.definition^.extnumber:=get_intconst;
end;


procedure pd_external(const procnames:Tstringcontainer);
{
  If import_dll=nil the procedure is assumed to be in another
  object file. In that object file it should have the name to
  which import_name is pointing to. Otherwise, the procedure is
  assumed to be in the DLL to which import_dll is pointing to. In
  that case either import_nr<>0 or import_name<>nil is true, so
  the procedure is either imported by number or by name. (DM)
}
var
  import_dll,
  import_name : string;
  import_nr   : word;
begin
  aktprocsym^.definition^.forwarddef:=false;
{ If the procedure should be imported from a DLL, a constant string follows.
  This isn't really correct, an contant string expression follows
  so we check if an semicolon follows, else a string constant have to
  follow (FK) }
  import_nr:=0;
  import_name:='';
  if not(token=SEMICOLON) and not(idtoken=_NAME) then
    begin
      import_dll:=get_stringconst;
      if (idtoken=_NAME) then
       begin
         consume(_NAME);
         import_name:=get_stringconst;
       end;
      if (idtoken=_INDEX) then
       begin
         {After the word index follows the index number in the DLL.}
         consume(_INDEX);
         import_nr:=get_intconst;
       end;
      if (import_nr=0) and (import_name='') then
        Message(parser_w_empty_import_name);
      if not(current_module^.uses_imports) then
       begin
         current_module^.uses_imports:=true;
         importlib^.preparelib(current_module^.modulename^);
       end;
      importlib^.importprocedure(aktprocsym^.mangledname,import_dll,import_nr,import_name)
    end
  else
    begin
      if (idtoken=_NAME) then
       begin
         consume(_NAME);
         aktprocsym^.definition^.setmangledname(get_stringconst);
       end
      else
       begin
         { external shouldn't override the cdecl/system name }
         if (aktprocsym^.definition^.options and poclearstack)=0 then
           aktprocsym^.definition^.setmangledname(aktprocsym^.name);
         externals^.concat(new(pai_external,init(aktprocsym^.mangledname,EXT_NEAR)));
       end;
    end;
end;

{$ifdef TP}
  {$F-}
{$endif}

function parse_proc_direc(const name:string;const proc_names:Tstringcontainer;var pdflags:word):boolean;
{
  Parse the procedure directive, returns true if a correct directive is found
}
const
   namelength=15;
type
   pd_handler=procedure(const procnames:Tstringcontainer);
   proc_dir_rec=record
     name     : string[namelength]; {15 letters should be enough.}
     handler  : pd_handler;         {Handler.}
     flag     : longint;            {Procedure flag. May be zero}
     pd_flags : longint;             {Parse options}
     mut_excl : longint;             {List of mutually exclusive flags.}
   end;
const
  {Should contain the number of procedure directives we support.}
  num_proc_directives=21;
  {Should contain the largest power of 2 lower than
   num_proc_directives, the int value of the 2-log of it. Cannot be
   calculated using an constant expression, as far as I know.}
  num_proc_directives_2log=16;

  proc_direcdata:array[1..num_proc_directives] of proc_dir_rec=
   ((name:'ALIAS'     ;handler:{$ifdef FPC}@{$endif}pd_alias;
      flag:0            ;pd_flags:pd_implemen+pd_body;
      mut_excl:poinline+poexternal),
    (name:'ASMNAME' ;handler:{$ifdef FPC}@{$endif}pd_asmname;
      flag:pocdecl+poclearstack+poexternal;pd_flags:pd_interface+pd_implemen;
      mut_excl:pointernproc+poexternal),
    (name:'ASSEMBLER' ;handler:nil;
      flag:poassembler  ;pd_flags:pd_implemen+pd_body;
      mut_excl:pointernproc+poexternal),
    (name:'CDECL'     ;handler:{$ifdef FPC}@{$endif}pd_cdecl;
      flag:pocdecl+poclearstack;pd_flags:pd_interface+pd_implemen+pd_body+pd_procvar;
      mut_excl:poleftright+poinline+poassembler+pointernproc+poexternal),
    (name:'EXPORT'    ;handler:{$ifdef FPC}@{$endif}pd_export;
      flag:poexports    ;pd_flags:pd_body+pd_global+pd_interface+pd_implemen{??};
      mut_excl:poexternal+poinline+pointernproc+pointerrupt),
    (name:'EXTERNAL'  ;handler:{$ifdef FPC}@{$endif}pd_external;
      flag:poexternal   ;pd_flags:pd_implemen;
      mut_excl:poexports+poinline+pointernproc+pointerrupt+poassembler+popalmossyscall),
    (name:'FAR'       ;handler:{$ifdef FPC}@{$endif}pd_far;
      flag:0            ;pd_flags:pd_implemen+pd_body+pd_interface+pd_procvar;
      mut_excl:pointernproc),
    (name:'FORWARD'   ;handler:{$ifdef FPC}@{$endif}pd_forward;
      flag:0            ;pd_flags:pd_implemen;
      mut_excl:pointernproc+poexternal),
    (name:'INLINE'    ;handler:{$ifdef FPC}@{$endif}pd_inline;
      flag:poinline     ;pd_flags:pd_implemen+pd_body;
      mut_excl:poexports+poexternal+pointernproc+pointerrupt+poconstructor+podestructor),
    (name:'INTERNCONST';handler:{$ifdef FPC}@{$endif}pd_intern;
      flag:pointernconst;pd_flags:pd_implemen+pd_body;
      mut_excl:pointernproc+pooperator),
    (name:'INTERNPROC';handler:{$ifdef FPC}@{$endif}pd_intern;
      flag:pointernproc ;pd_flags:pd_implemen;
      mut_excl:poexports+poexternal+pointerrupt+poassembler+poclearstack+poleftright+poiocheck+
               poconstructor+podestructor+pooperator),
    (name:'INTERRUPT' ;handler:nil;
      flag:pointerrupt  ;pd_flags:pd_implemen+pd_body;
      mut_excl:pointernproc+poclearstack+poleftright+poinline+
        poconstructor+podestructor+pooperator+poexternal),
    (name:'IOCHECK'   ;handler:nil;
      flag:poiocheck    ;pd_flags:pd_implemen+pd_body;
      mut_excl:pointernproc+poexternal),
    (name:'NEAR'      ;handler:{$ifdef FPC}@{$endif}pd_near;
      flag:0            ;pd_flags:pd_implemen+pd_body+pd_procvar;
      mut_excl:pointernproc),
    (name:'PASCAL'    ;handler:nil;
      flag:poleftright  ;pd_flags:pd_implemen+pd_body+pd_procvar;
      mut_excl:pointernproc+poexternal),
    (name:'POPSTACK'  ;handler:nil;
      flag:poclearstack ;pd_flags:pd_interface+pd_implemen+pd_body+pd_procvar;
      mut_excl:poinline+pointernproc+poassembler+poexternal),
    (name:'PUBLIC'    ;handler:nil;
      flag:0            ;pd_flags:pd_implemen+pd_body+pd_global;
      mut_excl:pointernproc+poinline+poexternal),
    (name:'REGISTER'    ;handler:{$ifdef FPC}@{$endif}pd_register;
      flag:poregister   ;pd_flags:pd_interface+pd_implemen+pd_body+pd_procvar;
      mut_excl:poleftright+pocdecl+pointernproc+poexternal),
    (name:'STDCALL'    ;handler:{$ifdef FPC}@{$endif}pd_stdcall;
      flag:postdcall    ;pd_flags:pd_interface+pd_implemen+pd_body+pd_procvar;
      mut_excl:poleftright+pocdecl+pointernproc+poinline+poexternal),
    (name:'SYSCALL'    ;handler:{$ifdef FPC}@{$endif}pd_syscall;
      flag:popalmossyscall;pd_flags:pd_interface;
      mut_excl:poexports+poinline+pointernproc+pointerrupt+poassembler+poexternal),
    (name:'SYSTEM'     ;handler:{$ifdef FPC}@{$endif}pd_system;
      flag:poclearstack ;pd_flags:pd_implemen;
      mut_excl:poleftright+poinline+poassembler+pointernproc+poexternal));

var
  p,w : longint;
begin
  parse_proc_direc:=false;
{ Search the procedure directive in the array. We shoot with a bazooka
  on a bug, that is, we release a binary search }
  p:=1;
  if (length(name)<=namelength) then
   begin
     w:=num_proc_directives_2log;
     while w<>0 do
       begin
         if proc_direcdata[p+w].name<=name then
          inc(p,w);
         w:=w shr 1;
         while p+w>num_proc_directives do
          w:=w shr 1;
       end;
   end;
{ Check if the procedure directive is known }
  if name<>proc_direcdata[p].name then
   begin
      { parsing a procvar type the name can be any
        next variable !! }
      if (pdflags and pd_procvar)=0 then
        Message1(parser_w_unknown_proc_directive_ignored,name);
      exit;
   end;

{ consume directive, and turn flag on }
  consume(token);
  parse_proc_direc:=true;

{ Conflicts between directives ? }
  if (aktprocsym^.definition^.options and proc_direcdata[p].mut_excl)<>0 then
   begin
     Message1(parser_e_proc_dir_conflict,name);
     exit;
   end;

{ Check the pd_flags if the directive should be allowed }
  if ((pdflags and pd_interface)<>0) and
     ((proc_direcdata[p].pd_flags and pd_interface)=0) then
    begin
      Message1(parser_e_proc_dir_not_allowed_in_interface,name);
      exit;
    end;
  if ((pdflags and pd_implemen)<>0) and
     ((proc_direcdata[p].pd_flags and pd_implemen)=0) then
    begin
      Message1(parser_e_proc_dir_not_allowed_in_implementation,name);
      exit;
    end;
  if ((pdflags and pd_procvar)<>0) and
     ((proc_direcdata[p].pd_flags and pd_procvar)=0) then
    begin
      Message1(parser_e_proc_dir_not_allowed_in_procvar,name);
      exit;
    end;

{ Return the new pd_flags }
  if (proc_direcdata[p].pd_flags and pd_body)=0 then
    pdflags:=pdflags and (not pd_body);
  if (proc_direcdata[p].pd_flags and pd_global)<>0 then
    pdflags:=pdflags or pd_global;

{ Add the correct flag }
  aktprocsym^.definition^.options:=aktprocsym^.definition^.options or proc_direcdata[p].flag;

{ Call the handler }
  if pointer({$ifndef FPC}@{$endif}proc_direcdata[p].handler)<>nil then
    proc_direcdata[p].handler(proc_names);
end;

{***************************************************************************}

function check_identical:boolean;
{
  Search for idendical definitions,
  if there is a forward, then kill this.

  Returns the result of the forward check.

  Removed from unter_dec to keep the source readable
}
const
{List of procedure options that affect the procedure type.}
  po_type_params=poconstructor+podestructor+pooperator;

  po_call_params=pocdecl+poclearstack+poleftright+poregister;

var
  hd,pd : Pprocdef;
  storeparast : psymtable;
  ad,fd : psym;
begin
  check_identical:=false;
  pd:=aktprocsym^.definition;
  if assigned(pd) then
   begin
   { Is there an overload/forward ? }
     if assigned(pd^.nextoverloaded) then
      begin
      { walk the procdef list }
        while (assigned(pd)) and (assigned(pd^.nextoverloaded)) do
         begin
           if not(m_repeat_forward in aktmodeswitches) or
              equal_paras(aktprocsym^.definition^.para1,pd^.nextoverloaded^.para1,false) then
             begin
               if pd^.nextoverloaded^.forwarddef then
               { remove the forward definition  but don't delete it,          }
               { the symtable is the owner !!  }
                 begin
                   hd:=pd^.nextoverloaded;
                 { Check if the procedure type and return type are correct }
                   if ((hd^.options and po_type_params)<>(aktprocsym^.definition^.options and po_type_params)) or
                      (not(is_equal(hd^.retdef,aktprocsym^.definition^.retdef)) and
                      (m_repeat_forward in aktmodeswitches)) then
                     begin
                       Message1(parser_e_header_dont_match_forward,aktprocsym^.demangledName);
                       exit;
                     end;
                 { Check calling convention }
                   if ((hd^.options and po_call_params)<>(aktprocsym^.definition^.options and po_call_params)) then
                    begin
                    { only trigger a error, becuase it doesn't hurt }
                      Message(parser_e_call_convention_dont_match_forward);
                    end;
                 { manglednames are equal? }
                   if (m_repeat_forward in aktmodeswitches) or
                      assigned(aktprocsym^.definition^.parast^.root) then
                    if (hd^.mangledname<>aktprocsym^.definition^.mangledname) then
                     begin
                       if (aktprocsym^.definition^.options and poexternal)=0 then
                         Message(parser_n_interface_name_diff_implementation_name);
                     { reset the mangledname of the interface part to be sure }
                     { this is wrong because the mangled name might have been used already !! }
                     { hd^.setmangledname(aktprocsym^.definition^.mangledname);}
                     { so we need to keep the name of interface !! }
                       aktprocsym^.definition^.setmangledname(hd^.mangledname);
                     end
                   else
                     begin
                     { If mangled names are equal, therefore    }
                     { they have the same number of parameters  }
                     { Therefore we can check the name of these }
                     { parameters...                            }
                       if hd^.forwarddef and aktprocsym^.definition^.forwarddef then
                         begin
                           Message1(parser_e_function_already_declared_public_forward,aktprocsym^.demangledName);
                           Check_identical:=true;
                         { Remove other forward from the list to reduce errors }
                           pd^.nextoverloaded:=pd^.nextoverloaded^.nextoverloaded;
                           exit;
                         end;

                       ad:=hd^.parast^.root;
                       fd:=aktprocsym^.definition^.parast^.root;
                       if assigned(ad) and assigned(fd) then
                         begin
                           while assigned(ad) and assigned(fd) do
                             begin
                               if ad^.name<>fd^.name then
                                 begin
                                   Message3(parser_e_header_different_var_names,
                                     aktprocsym^.name,ad^.name,fd^.name);
                                   break;
                                 end;
                             { it is impossible to have a nil pointer }
                             { for only one parameter - since they    }
                             { have the same number of parameters.    }
                             { Left = next parameter.                 }
                               ad:=ad^.left;
                               fd:=fd^.left;
                             end;
                         end;
                     end;
                 { also the call_offset }
                   hd^.parast^.call_offset:=aktprocsym^.definition^.parast^.call_offset;

                 { remove pd^.nextoverloaded from the list }
                 { and add aktprocsym^.definition }
                   pd^.nextoverloaded:=pd^.nextoverloaded^.nextoverloaded;
                   hd^.nextoverloaded:=aktprocsym^.definition^.nextoverloaded;
                 { Alert! All fields of aktprocsym^.definition that are modified
                   by the procdir handlers must be copied here!.}
                   hd^.forwarddef:=false;
                   hd^.options:=hd^.options or aktprocsym^.definition^.options;
                   if aktprocsym^.definition^.extnumber=-1 then
                     aktprocsym^.definition^.extnumber:=hd^.extnumber
                   else
                     if hd^.extnumber=-1 then
                       hd^.extnumber:=aktprocsym^.definition^.extnumber;
                   { switch parast for warning in implementation  PM }
                   if (m_repeat_forward in aktmodeswitches) or
                      assigned(aktprocsym^.definition^.parast^.root) then
                     begin
                        storeparast:=hd^.parast;
                        hd^.parast:=aktprocsym^.definition^.parast;
                        aktprocsym^.definition^.parast:=storeparast;
                     end;
                   aktprocsym^.definition:=hd;
                   check_identical:=true;
                 end
               else
               { abstract methods aren't forward defined, but this }
               { needs another error message                       }
                 if (pd^.nextoverloaded^.options and poabstractmethod)=0 then
                   Message(parser_e_overloaded_have_same_parameters)
                 else
                   Message(parser_e_abstract_no_definition);
               break;
             end;
           pd:=pd^.nextoverloaded;
         end;
      end
     else
      begin
      { there is no overloaded, so its always identical with itself }
        check_identical:=true;
      end;
   end;
{ insert opsym only in the right symtable }
  if ((procinfo.flags and pi_operator)<>0) and not parse_only then
    begin
      if ret_in_param(aktprocsym^.definition^.retdef) then
        begin
          pprocdef(aktprocsym^.definition)^.parast^.insert(opsym);
        { this increases the data size }
        { correct this to get the right ret $value }
          dec(pprocdef(aktprocsym^.definition)^.parast^.datasize,opsym^.getsize);
          { this allows to read the funcretoffset }
          opsym^.address:=-4;
          opsym^.varspez:=vs_var;
        end
      else
        pprocdef(aktprocsym^.definition)^.localst^.insert(opsym);
    end;
end;

procedure compile_proc_body(const proc_names:Tstringcontainer;
                            make_global,parent_has_class:boolean);
{
  Compile the body of a procedure
}
var
   oldexitlabel,oldexit2label,oldquickexitlabel:Plabel;
   _class,hp:Pobjectdef;
   { switches can change inside the procedure }
   entryswitches, exitswitches : tlocalswitches;
   { code for the subroutine as tree }
   code:ptree;
   { size of the local strackframe }
   stackframe:longint;
   { true when no stackframe is required }
   nostackframe:boolean;
   { number of bytes which have to be cleared by RET }
   parasize:longint;
   { filepositions }
   entrypos,
   savepos,
   exitpos   : tfileposinfo;
begin
   { calculate the lexical level }
   inc(lexlevel);
   if lexlevel>32 then
    Message(parser_e_too_much_lexlevel);
   { save old labels }
   oldexitlabel:=aktexitlabel;
   oldexit2label:=aktexit2label;
   oldquickexitlabel:=quickexitlabel;
   { get new labels }
   getlabel(aktexitlabel);
   getlabel(aktexit2label);
   { exit for fail in constructors }
   if (aktprocsym^.definition^.options and poconstructor)<>0 then
     getlabel(quickexitlabel);
   { reset break and continue labels }
   in_except_block:=false;
   aktbreaklabel:=nil;
   aktcontinuelabel:=nil;

   { insert symtables for the class, by only if it is no nested function }
   if assigned(procinfo._class) and not(parent_has_class) then
     begin
       { insert them in the reverse order ! }
       hp:=nil;
       repeat
         _class:=procinfo._class;
         while _class^.childof<>hp do
           _class:=_class^.childof;
         hp:=_class;
         _class^.publicsyms^.next:=symtablestack;
         symtablestack:=_class^.publicsyms;
       until hp=procinfo._class;
     end;

   { insert parasymtable in symtablestack}
   { only if lexlevel > 1 !!! global symtable should be right after staticsymtazble
     for checking of same names used in interface and implementation !! }
   if lexlevel>=normal_function_level then
     begin
        aktprocsym^.definition^.parast^.next:=symtablestack;
        symtablestack:=aktprocsym^.definition^.parast;
        symtablestack^.symtablelevel:=lexlevel;
     end;
   { insert localsymtable in symtablestack}
   aktprocsym^.definition^.localst^.next:=symtablestack;
   symtablestack:=aktprocsym^.definition^.localst;
   symtablestack^.symtablelevel:=lexlevel;
   { constant symbols are inserted in this symboltable }
   constsymtable:=symtablestack;

   { reset the temporary memory }
   cleartempgen;
   { no registers are used }
   usedinproc:=0;

   { save entry info }
   entrypos:=aktfilepos;
   entryswitches:=aktlocalswitches;

   { parse the code ... }
   if (aktprocsym^.definition^.options and poassembler)<> 0 then
     code:=assembler_block
   else
     code:=block(current_module^.islibrary);

   { get a better entry point }
   if assigned(code) then
     entrypos:=code^.fileinfo;

   { save exit info }
   exitswitches:=aktlocalswitches;
   exitpos:=last_endtoken_filepos;

   { save current filepos }
   savepos:=aktfilepos;

   {When we are called to compile the body of a unit, aktprocsym should
    point to the unit initialization. If the unit has no initialization,
    aktprocsym=nil. But in that case code=nil. hus we should check for
    code=nil, when we use aktprocsym.}

   { set the framepointer to esp for assembler functions }
   { but only if the are no local variables              }
   { already done in assembler_block }
   setfirsttemp(procinfo.firsttemp);

   { ... and generate assembler }
   { but set the right switches for entry !! }
   aktlocalswitches:=entryswitches;
   if assigned(code) then
     generatecode(code);
   { set switches to status at end of procedure }
   aktlocalswitches:=exitswitches;

   if assigned(code) then
     begin
        aktprocsym^.definition^.code:=code;

        { the procedure is now defined }
        aktprocsym^.definition^.forwarddef:=false;
        aktprocsym^.definition^.usedregisters:=usedinproc;
     end;

   stackframe:=gettempsize;
   { only now we can remove the temps }
   resettempgen;

   { first generate entry code with the correct position and switches }
   aktfilepos:=entrypos;
   aktlocalswitches:=entryswitches;
   if assigned(code) then
     genentrycode(procinfo.aktentrycode,proc_names,make_global,stackframe,parasize,nostackframe,false);

   { now generate exit code with the correct position and switches }
   aktfilepos:=exitpos;
   aktlocalswitches:=exitswitches;
   if assigned(code) then
     begin
       genexitcode(procinfo.aktexitcode,parasize,nostackframe,false);
       procinfo.aktproccode^.insertlist(procinfo.aktentrycode);
       procinfo.aktproccode^.concatlist(procinfo.aktexitcode);
{$ifdef i386}
 {$ifndef NoOpt}
       if (cs_optimize in aktglobalswitches) and
       { no asm block allowed }
         ((procinfo.flags and pi_uses_asm)=0)  then
           Optimize(procinfo.aktproccode);
 {$endif NoOpt}
{$endif}
       { save local data (casetable) also in the same file }
       if assigned(procinfo.aktlocaldata) and
          (not procinfo.aktlocaldata^.empty) then
         begin
            procinfo.aktproccode^.concat(new(pai_section,init(sec_data)));
            procinfo.aktproccode^.concatlist(procinfo.aktlocaldata);
         end;
       { now we can insert a cut }
       if (cs_smartlink in aktmoduleswitches) then
         codesegment^.concat(new(pai_cut,init));

       { add the procedure to the codesegment }
       codesegment^.concatlist(procinfo.aktproccode);
     end;

   { ... remove symbol tables, for the browser leave the static table }
{    if (cs_browser in aktmoduleswitches) and (symtablestack^.symtabletype=staticsymtable) then
    symtablestack^.next:=symtablestack^.next^.next
   else }
   if lexlevel>=normal_function_level then
     symtablestack:=symtablestack^.next^.next
   else
     symtablestack:=symtablestack^.next;

   { ... check for unused symbols      }
   { but only if there is no asm block }
   if assigned(code) then
     begin
       if (status.errorcount=0) then
         begin
           aktprocsym^.definition^.localst^.check_forwards;
           aktprocsym^.definition^.localst^.checklabels;
         end;
       if (procinfo.flags and pi_uses_asm)=0 then
         begin
            { not for unit init, becuase the var can be used in finalize,
              it will be done in proc_unit }
            if (aktprocsym^.definition^.options and (pounitinit or pounitfinalize))=0 then
              aktprocsym^.definition^.localst^.allsymbolsused;
            aktprocsym^.definition^.parast^.allsymbolsused;
         end;
     end;

   { the local symtables can be deleted, but the parast   }
   { doesn't, (checking definitons when calling a         }
   { function                                             }
   { not for a inline procedure !!                 (PM)   }
   { at lexlevel = 1 localst is the staticsymtable itself }
   { so no dispose here !!                                }
   if assigned(code) and
      not(cs_browser in aktmoduleswitches) and
      ((aktprocsym^.definition^.options and poinline)=0) then
     begin
       if lexlevel>=normal_function_level then
         dispose(aktprocsym^.definition^.localst,done);
       aktprocsym^.definition^.localst:=nil;
     end;

    { remove code tree, if not inline procedure }
    if assigned(code) and ((aktprocsym^.definition^.options and poinline)=0) then
      disposetree(code);

   { remove class member symbol tables }
   while symtablestack^.symtabletype=objectsymtable do
     symtablestack:=symtablestack^.next;

   { restore filepos, the switches are already set }
   aktfilepos:=savepos;
   { free labels }
   freelabel(aktexitlabel);
   freelabel(aktexit2label);
   if (aktprocsym^.definition^.options and poconstructor)<>0 then
    freelabel(quickexitlabel);
   { restore labels }
   aktexitlabel:=oldexitlabel;
   aktexit2label:=oldexit2label;
   quickexitlabel:=oldquickexitlabel;
   { previous lexlevel }
   dec(lexlevel);
end;


procedure parse_proc_directives(Anames:Pstringcontainer;var pdflags:word);
{
  Parse the procedure directives. It does not matter if procedure directives
  are written using ;procdir; or ['procdir'] syntax.
}
var
  name : string;
  res  : boolean;
begin
  while token in [ID,LECKKLAMMER] do
   begin
     if token=LECKKLAMMER then
      begin
        consume(LECKKLAMMER);
        repeat
          name:=pattern;
          { consume(ID);
          now done in the function }
          parse_proc_direc(name,Anames^,pdflags);
          if token=COMMA then
           consume(COMMA)
          else
           break;
        until false;
        consume(RECKKLAMMER);
        { we always expect at least '[];' }
        res:=true;
      end
     else
      begin
        name:=pattern;
        res:=parse_proc_direc(name,Anames^,pdflags);
      end;
   { A procedure directive is always followed by a semicolon }
     if res then
      consume(SEMICOLON)
     else
      break;
   end;
end;

procedure parse_var_proc_directives(var sym : ptypesym);
var
  anames : pstringcontainer;
  pdflags : word;
  oldsym : pprocsym;
begin
  oldsym:=aktprocsym;
  anames:=new(pstringcontainer,init);
  pdflags:=pd_procvar;
  { we create a temporary aktprocsym to read the directives }
  aktprocsym:=new(pprocsym,init(sym^.name));
  aktprocsym^.definition:=pprocdef(sym^.definition);
  { anmes should never be used anyway }
  inc(lexlevel);
  parse_proc_directives(anames,pdflags);
  dec(lexlevel);
  aktprocsym^.definition:=nil;
  dispose(aktprocsym,done);
  dispose(anames,done);
  aktprocsym:=oldsym;
end;


procedure read_proc;
{
  Parses the procedure directives, then parses the procedure body, then
  generates the code for it
}
var
  oldprefix        : string;
  oldprocsym       : Pprocsym;
  oldprocinfo      : tprocinfo;
  oldconstsymtable : Psymtable;
  names            : Pstringcontainer;
  pdflags          : word;
begin
{ save old state }
   oldprocsym:=aktprocsym;
   oldprefix:=procprefix;
   oldconstsymtable:=constsymtable;
   oldprocinfo:=procinfo;
{ create a new procedure }
   new(names,init);
   codegen_newprocedure;
   with procinfo do
    begin
      parent:=@oldprocinfo;
    { clear flags }
      flags:=0;
    { standard frame pointer }
      framepointer:=frame_pointer;
      funcret_is_valid:=false;
    { is this a nested function of a method ? }
      _class:=oldprocinfo._class;
    end;

   parse_proc_dec;

{ set the default function options }
   if parse_only then
    begin
      aktprocsym^.properties:=aktprocsym^.properties or sp_forwarddef;
      aktprocsym^.definition^.forwarddef:=true;
      pdflags:=pd_interface;
    end
   else
    begin
      pdflags:=pd_body;
      if current_module^.in_implementation then
       pdflags:=pdflags or pd_implemen;
      if (not current_module^.is_unit) or (cs_smartlink in aktmoduleswitches) then
       pdflags:=pdflags or pd_global;
      procinfo.exported:=false;
      aktprocsym^.definition^.forwarddef:=false;
    end;

{ parse the directives that may follow }
   inc(lexlevel);
   parse_proc_directives(names,pdflags);
   dec(lexlevel);

{ search for forward declarations }
   if (not check_identical) then
     begin
     { A method must be forward defined (in the object declaration) }
       if assigned(procinfo._class) and (not assigned(oldprocinfo._class)) then
         Message(parser_e_header_dont_match_any_member);
     { check the global flag }
       if (procinfo.flags and pi_is_global)<>0 then
         Message(parser_e_overloaded_must_be_all_global);
     end;

{ set return type here, becuase the aktprocsym^.definition can be
  changed by check_identical (PFV) }
   procinfo.retdef:=aktprocsym^.definition^.retdef;

   { pointer to the return value ? }
   if ret_in_param(procinfo.retdef) then
    begin
      procinfo.retoffset:=procinfo.call_offset;
      inc(procinfo.call_offset,target_os.size_of_pointer);
    end;
   { allows to access the parameters of main functions in nested functions }
   aktprocsym^.definition^.parast^.call_offset:=procinfo.call_offset;

{ compile procedure when a body is needed }
   if (pdflags and pd_body)<>0 then
     begin
       Message1(parser_p_procedure_start,aktprocsym^.demangledname);
       names^.insert(aktprocsym^.definition^.mangledname);
      { set _FAIL as keyword if constructor }
      if (aktprocsym^.definition^.options and poconstructor)<>0 then
        tokeninfo[_FAIL].keyword:=m_all;
      if assigned(aktprocsym^.definition^._class) then
        tokeninfo[_SELF].keyword:=m_all;
       compile_proc_body(names^,((pdflags and pd_global)<>0),assigned(oldprocinfo._class));
      { reset _FAIL as normal }
      if (aktprocsym^.definition^.options and poconstructor)<>0 then
        tokeninfo[_FAIL].keyword:=m_none;
      if assigned(aktprocsym^.definition^._class) and (lexlevel=main_program_level) then
        tokeninfo[_SELF].keyword:=m_none;
       consume(SEMICOLON);
     end;
{ close }
   dispose(names,done);
   codegen_doneprocedure;
{ Restore old state }
   constsymtable:=oldconstsymtable;
   aktprocsym:=oldprocsym;
   procprefix:=oldprefix;
   procinfo:=oldprocinfo;
   opsym:=nil;
end;

end.

{
  $Log$
  Revision 1.1  1998-12-26 15:20:31  florian
    + more changes for the new version

}
