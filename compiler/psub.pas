{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl, Daniel Mantione

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

{$i defines.inc}

interface

uses
  cobjects,
  symconst,tokens,symtable;

const
  pd_global    = $1;    { directive must be global }
  pd_body      = $2;    { directive needs a body }
  pd_implemen  = $4;    { directive can be used implementation section }
  pd_interface = $8;    { directive can be used interface section }
  pd_object    = $10;   { directive can be used object declaration }
  pd_procvar   = $20;   { directive can be used procvar declaration }
  pd_notobject    = $40;{ directive can not be used object declaration }

procedure compile_proc_body(const proc_names:Tstringcontainer;
                            make_global,parent_has_class:boolean);
procedure parse_proc_head(options:tproctypeoption);
procedure parse_proc_dec;
function  is_proc_directive(tok:ttoken):boolean;
procedure parse_var_proc_directives(var sym : psym);
procedure parse_object_proc_directives(var sym : pprocsym);
procedure read_proc;
function check_identical_proc(var p : pprocdef) : boolean;

implementation

uses
{$ifdef delphi}
  sysutils,
{$else}
  strings,
{$endif}
  globtype,systems,
  cutils,globals,verbose,fmodule,
  scanner,aasm,tree,types,
  import,gendef,htypechk,
{$ifdef newcg}
  cgbase,
{$else newcg}
  hcodegen,temp_gen,
{$endif newcg}
  pass_1,cpubase,cpuasm
{$ifndef NOPASS2}
  ,pass_2
{$endif}
{$ifdef GDB}
  ,gdb
{$endif GDB}
{$ifdef newcg}
  {$ifndef NOOPT}
    ,aopt
  {$endif}
{$else}
  {$ifdef i386}
    ,tgeni386
    ,cgai386
    {$ifndef NOOPT}
      ,aopt386
    {$endif}
  {$endif}
  {$ifdef m68k}
    ,tgen68k,cga68k
  {$endif}
{$endif newcg}
  { parser specific stuff }
  ,pbase,ptype,pdecl,pexpr,pstatmnt
{$ifdef newcg}
  ,tgcpu,convtree,cgobj,tgeni386  { for the new code generator tgeni386 is only a dummy }
{$endif newcg}
  ;

var
  realname:string;  { contains the real name of a procedure as it's typed }


procedure parse_proc_head(options:tproctypeoption);
var sp:stringid;
    pd:Pprocdef;
    paramoffset:longint;
    sym:Psym;
    hs:string;
    st : psymtable;
    overloaded_level:word;
    storepos,procstartfilepos : tfileposinfo;
begin
{ Save the position where this procedure really starts and set col to 1 which
  looks nicer }
  procstartfilepos:=tokenpos;
{  procstartfilepos.column:=1; I do not agree here !!
   lets keep excat position PM }

  if (options=potype_operator) then
    begin
      sp:=overloaded_names[optoken];
      realname:=sp;
    end
  else
    begin
      sp:=pattern;
      realname:=orgpattern;
      consume(_ID);
    end;

{ method ? }
  if not(parse_only) and
     (lexlevel=normal_function_level) and
     try_to_consume(_POINT) then
   begin
     storepos:=tokenpos;
     tokenpos:=procstartfilepos;
     getsym(sp,true);
     sym:=srsym;
     tokenpos:=storepos;
     { load proc name }
     sp:=pattern;
     realname:=orgpattern;
     procstartfilepos:=tokenpos;
     { qualifier is class name ? }
     if (sym^.typ<>typesym) or
        (ptypesym(sym)^.restype.def^.deftype<>objectdef) then
       begin
          Message(parser_e_class_id_expected);
          aktprocsym:=nil;
          consume(_ID);
       end
     else
       begin
          { used to allow private syms to be seen }
          aktobjectdef:=pobjectdef(ptypesym(sym)^.restype.def);
          procinfo^._class:=pobjectdef(ptypesym(sym)^.restype.def);
          aktprocsym:=pprocsym(procinfo^._class^.symtable^.search(sp));
          consume(_ID);
          {The procedure has been found. So it is
           a global one. Set the flags to mark this.}
          procinfo^.flags:=procinfo^.flags or pi_is_global;
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
        (options in [potype_constructor,potype_destructor]) then
        Message(parser_e_constructors_always_objects);

     tokenpos:=procstartfilepos;
     aktprocsym:=pprocsym(symtablestack^.search(sp));

     if not(parse_only) then
       begin
         {The procedure we prepare for is in the implementation
          part of the unit we compile. It is also possible that we
          are compiling a program, which is also some kind of
          implementaion part.

          We need to find out if the procedure is global. If it is
          global, it is in the global symtable.}
         if not assigned(aktprocsym) and
            (symtablestack^.symtabletype=staticsymtable) then
          begin
            {Search the procedure in the global symtable.}
            aktprocsym:=Pprocsym(search_a_symtable(sp,globalsymtable));
            if assigned(aktprocsym) then
             begin
               {Check if it is a procedure.}
               if aktprocsym^.typ<>procsym then
                DuplicateSym(aktprocsym);
               {The procedure has been found. So it is
                a global one. Set the flags to mark this.}
               procinfo^.flags:=procinfo^.flags or pi_is_global;
             end;
          end;
       end;
   end;

{ Create the mangledname }
{$ifndef UseNiceNames}
  if assigned(procinfo^._class) then
   begin
     if (pos('_$$_',procprefix)=0) then
      hs:=procprefix+'_$$_'+procinfo^._class^.objname^+'_$$_'+sp
     else
      hs:=procprefix+'_$'+sp;
   end
  else
   begin
     if lexlevel=normal_function_level then
      hs:=procprefix+'_'+sp
     else
      hs:=procprefix+'_$'+sp;
   end;
{$else UseNiceNames}
  if assigned(procinfo^._class) then
   begin
     if (pos('_5Class_',procprefix)=0) then
      hs:=procprefix+'_5Class_'+procinfo^._class^.name^+'_'+tostr(length(sp))+sp
     else
      hs:=procprefix+'_'+tostr(length(sp))+sp;
   end
  else
   begin
     if lexlevel=normal_function_level then
      hs:=procprefix+'_'+tostr(length(sp))+sp
     else
      hs:=lowercase(procprefix)+'_'+tostr(length(sp))+sp;
   end;
{$endif UseNiceNames}

  if assigned(aktprocsym) then
   begin
     { Check if overloaded is a procsym, we use a different error message
       for tp7 so it looks more compatible }
     if aktprocsym^.typ<>procsym then
      begin
        if (m_fpc in aktmodeswitches) then
         Message1(parser_e_overloaded_no_procedure,aktprocsym^.name)
        else
         DuplicateSym(aktprocsym);
        { try to recover by creating a new aktprocsym }
        tokenpos:=procstartfilepos;
        aktprocsym:=new(pprocsym,init(sp));
      end;
   end
  else
   begin
     { create a new procsym and set the real filepos }
     tokenpos:=procstartfilepos;
     { for operator we have only one definition for each overloaded
       operation }
     if (options=potype_operator) then
       begin
          { create the procsym with saving the original case }
          aktprocsym:=new(pprocsym,init('$'+sp));
          { the only problem is that nextoverloaded might not be in a unit
            known for the unit itself }
          { not anymore PM }
          if assigned(overloaded_operators[optoken]) then
            aktprocsym^.definition:=overloaded_operators[optoken]^.definition;
{$ifndef DONOTCHAINOPERATORS}
          overloaded_operators[optoken]:=aktprocsym;
{$endif DONOTCHAINOPERATORS}
       end
      else
       aktprocsym:=new(pprocsym,init(sp));
     symtablestack^.insert(aktprocsym);
   end;

  st:=symtablestack;
  pd:=new(pprocdef,init);
  pd^.symtablelevel:=symtablestack^.symtablelevel;

  if assigned(procinfo^._class) then
    pd^._class := procinfo^._class;

  { set the options from the caller (podestructor or poconstructor) }
  pd^.proctypeoption:=options;

  { calculate the offset of the parameters }
  paramoffset:=8;

  { calculate frame pointer offset }
  if lexlevel>normal_function_level then
    begin
      procinfo^.framepointer_offset:=paramoffset;
      inc(paramoffset,target_os.size_of_pointer);
      { this is needed to get correct framepointer push for local
        forward functions !! }
      pd^.parast^.symtablelevel:=lexlevel;
    end;

  if assigned (procinfo^._Class)  and
     not(procinfo^._Class^.is_class) and
     (pd^.proctypeoption in [potype_constructor,potype_destructor]) then
    inc(paramoffset,target_os.size_of_pointer);

  { self pointer offset                       }
  { self isn't pushed in nested procedure of methods }
  if assigned(procinfo^._class) and (lexlevel=normal_function_level) then
    begin
      procinfo^.selfpointer_offset:=paramoffset;
      if assigned(aktprocsym^.definition) and
         not(po_containsself in aktprocsym^.definition^.procoptions) then
        inc(paramoffset,target_os.size_of_pointer);
    end;

  { con/-destructor flag ? }
  if assigned (procinfo^._Class) and
     procinfo^._class^.is_class and
     (pd^.proctypeoption in [potype_destructor,potype_constructor]) then
    inc(paramoffset,target_os.size_of_pointer);

  procinfo^.para_offset:=paramoffset;

  pd^.parast^.datasize:=0;

  pd^.nextoverloaded:=aktprocsym^.definition;
  aktprocsym^.definition:=pd;
  { this is probably obsolete now PM }
  aktprocsym^.definition^.fileinfo:=procstartfilepos;
  aktprocsym^.definition^.setmangledname(hs);
  aktprocsym^.definition^.procsym:=aktprocsym;

  if not parse_only then
    begin
       overloaded_level:=0;
       { we need another procprefix !!! }
       { count, but only those in the same unit !!}
       while assigned(pd) and
          (pd^.owner^.symtabletype in [globalsymtable,staticsymtable]) do
         begin
            { only count already implemented functions }
            if  not(pd^.forwarddef) then
              inc(overloaded_level);
            pd:=pd^.nextoverloaded;
         end;
       if overloaded_level>0 then
         procprefix:=hs+'$'+tostr(overloaded_level)+'$'
       else
         procprefix:=hs+'$';
    end;

  { this must also be inserted in the right symtable !! PM }
  { otherwise we get subbtle problems with
    definitions of args defs in staticsymtable for
    implementation of a global method }
  if token=_LKLAMMER then
    parameter_dec(aktprocsym^.definition);

  { so we only restore the symtable now }
  symtablestack:=st;
  if (options=potype_operator) then
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
                   parse_proc_head(potype_none);
                   if token<>_COLON then
                    begin
                       if not(aktprocsym^.definition^.forwarddef) or
                         (m_repeat_forward in aktmodeswitches) then
                       begin
                         consume(_COLON);
                         consume_all_until(_SEMICOLON);
                       end;
                    end
                   else
                    begin
                      consume(_COLON);
                      inc(testcurobject);
                      single_type(aktprocsym^.definition^.rettype,hs,false);
                      aktprocsym^.definition^.test_if_fpu_result;
                      dec(testcurobject);
                    end;
                 end;
    _PROCEDURE : begin
                   consume(_PROCEDURE);
                   parse_proc_head(potype_none);
                   aktprocsym^.definition^.rettype.def:=voiddef;
                 end;
  _CONSTRUCTOR : begin
                   consume(_CONSTRUCTOR);
                   parse_proc_head(potype_constructor);
                   if assigned(procinfo^._class) and
                      procinfo^._class^.is_class then
                    begin
                      { CLASS constructors return the created instance }
                      aktprocsym^.definition^.rettype.def:=procinfo^._class;
                    end
                   else
                    begin
                      { OBJECT constructors return a boolean }
{$IfDef GDB}
                      { GDB doesn't like unnamed types !}
                      aktprocsym^.definition^.rettype.def:=globaldef('boolean');
{$else GDB}
                      aktprocsym^.definition^.rettype.def:=new(porddef,init(bool8bit,0,1));
{$Endif GDB}
                    end;
                 end;
   _DESTRUCTOR : begin
                   consume(_DESTRUCTOR);
                   parse_proc_head(potype_destructor);
                   aktprocsym^.definition^.rettype.def:=voiddef;
                 end;
     _OPERATOR : begin
                   if lexlevel>normal_function_level then
                     Message(parser_e_no_local_operator);
                   consume(_OPERATOR);
                   if not(token in [_PLUS..last_overloaded]) then
                     Message(parser_e_overload_operator_failed);
                   optoken:=token;
                   consume(Token);
                   procinfo^.flags:=procinfo^.flags or pi_operator;
                   parse_proc_head(potype_operator);
                   if token<>_ID then
                     begin
                        opsym:=nil;
                        if not(m_result in aktmodeswitches) then
                          consume(_ID);
                     end
                   else
                     begin
                       opsym:=new(pvarsym,initdef(pattern,voiddef));
                       consume(_ID);
                     end;
                   if not try_to_consume(_COLON) then
                     begin
                       consume(_COLON);
                       aktprocsym^.definition^.rettype.def:=generrordef;
                       consume_all_until(_SEMICOLON);
                     end
                   else
                    begin
                      single_type(aktprocsym^.definition^.rettype,hs,false);
                      aktprocsym^.definition^.test_if_fpu_result;
                      if (optoken in [_EQUAL,_GT,_LT,_GTE,_LTE]) and
                         ((aktprocsym^.definition^.rettype.def^.deftype<>
                         orddef) or (porddef(aktprocsym^.definition^.
                         rettype.def)^.typ<>bool8bit)) then
                        Message(parser_e_comparative_operator_return_boolean);
                       if assigned(opsym) then
                         opsym^.vartype.def:=aktprocsym^.definition^.rettype.def;
                       { We need to add the return type in the mangledname
                         to allow overloading with just different results !! (PM) }
                       aktprocsym^.definition^.setmangledname(
                         aktprocsym^.definition^.mangledname+'$$'+hs);
                       if (optoken=_ASSIGNMENT) and
                          is_equal(aktprocsym^.definition^.rettype.def,
                             pvarsym(aktprocsym^.definition^.parast^.symindex^.first)^.vartype.def) then
                         message(parser_e_no_such_assignment)
                       else if not isoperatoracceptable(aktprocsym^.definition,optoken) then
                         Message(parser_e_overload_impossible);
                     end;
                 end;
  end;
  if isclassmethod and
     assigned(aktprocsym) then
    include(aktprocsym^.definition^.procoptions,po_classmethod);
  { support procedure proc;stdcall export; in Delphi mode only }
  if not((m_delphi in aktmodeswitches) and
     is_proc_directive(token)) then
   consume(_SEMICOLON);
  dec(lexlevel);
end;


{****************************************************************************
                        Procedure directive handlers
****************************************************************************}

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
  if assigned(procinfo^._class) then
    Message(parser_e_methods_dont_be_export);
  if lexlevel<>normal_function_level then
    Message(parser_e_dont_nest_export);
  { only os/2 needs this }
  if target_info.target=target_i386_os2 then
   begin
     procnames.insert(realname);
     procinfo^.exported:=true;
     if cs_link_deffile in aktglobalswitches then
       deffile.AddExport(aktprocsym^.definition^.mangledname);
   end;
end;

procedure pd_inline(const procnames:Tstringcontainer);
begin
  if not(cs_support_inline in aktmoduleswitches) then
   Message(parser_e_proc_inline_not_supported);
end;

procedure pd_forward(const procnames:Tstringcontainer);
begin
  aktprocsym^.definition^.forwarddef:=true;
end;

procedure pd_stdcall(const procnames:Tstringcontainer);
begin
end;

procedure pd_safecall(const procnames:Tstringcontainer);
begin
end;

procedure pd_alias(const procnames:Tstringcontainer);
begin
  consume(_COLON);
  procnames.insert(get_stringconst);
end;

procedure pd_asmname(const procnames:Tstringcontainer);
begin
  aktprocsym^.definition^.setmangledname(target_os.Cprefix+pattern);
  if token=_CCHAR then
    consume(_CCHAR)
  else
    consume(_CSTRING);
  { we don't need anything else }
  aktprocsym^.definition^.forwarddef:=false;
end;

procedure pd_intern(const procnames:Tstringcontainer);
begin
  consume(_COLON);
  aktprocsym^.definition^.extnumber:=get_intconst;
end;

procedure pd_interrupt(const procnames:Tstringcontainer);
begin
{$ifndef i386}
  Message(parser_w_proc_interrupt_ignored);
{$else i386}
  if lexlevel<>normal_function_level then
    Message(parser_e_dont_nest_interrupt);
{$endif i386}
end;

procedure pd_system(const procnames:Tstringcontainer);
begin
  aktprocsym^.definition^.setmangledname(realname);
end;

procedure pd_abstract(const procnames:Tstringcontainer);
begin
  if (po_virtualmethod in aktprocsym^.definition^.procoptions) then
    include(aktprocsym^.definition^.procoptions,po_abstractmethod)
  else
    Message(parser_e_only_virtual_methods_abstract);
  { the method is defined }
  aktprocsym^.definition^.forwarddef:=false;
end;

procedure pd_virtual(const procnames:Tstringcontainer);
{$ifdef WITHDMT}
var
  pt : ptree;
{$endif WITHDMT}
begin
  if (aktprocsym^.definition^.proctypeoption=potype_constructor) and
     not(aktprocsym^.definition^._class^.is_class) then
    Message(parser_e_constructor_cannot_be_not_virtual);
{$ifdef WITHDMT}
  if not(aktprocsym^.definition^._class^.is_class) and
    (token<>_SEMICOLON) then
    begin
       { any type of parameter is allowed here! }

       pt:=comp_expr(true);
       do_firstpass(pt);
       if is_constintnode(pt) then
         begin
           include(aktprocsym^.definition^.procoptions,po_msgint);
           aktprocsym^.definition^.messageinf.i:=pt^.value;
         end
       else
         Message(parser_e_ill_msg_expr);
       disposetree(pt);
    end;
{$endif WITHDMT}
end;

procedure pd_static(const procnames:Tstringcontainer);
begin
  if (cs_static_keyword in aktmoduleswitches) then
    begin
      include(aktprocsym^.symoptions,sp_static);
      include(aktprocsym^.definition^.procoptions,po_staticmethod);
    end;
end;

procedure pd_override(const procnames:Tstringcontainer);
begin
  if not(aktprocsym^.definition^._class^.is_class) then
    Message(parser_e_no_object_override);
end;

procedure pd_overload(const procnames:Tstringcontainer);
begin
end;

procedure pd_message(const procnames:Tstringcontainer);
var
  pt : ptree;
begin
  { check parameter type }
  if not(po_containsself in aktprocsym^.definition^.procoptions) and
     ((aktprocsym^.definition^.minparacount<>1) or
      (aktprocsym^.definition^.maxparacount<>1) or
      (pparaitem(aktprocsym^.definition^.para^.first)^.paratyp<>vs_var)) then
   Message(parser_e_ill_msg_param);
  pt:=comp_expr(true);
  do_firstpass(pt);
  if pt^.treetype=stringconstn then
    begin
      include(aktprocsym^.definition^.procoptions,po_msgstr);
      aktprocsym^.definition^.messageinf.str:=strnew(pt^.value_str);
    end
  else
   if is_constintnode(pt) then
    begin
      include(aktprocsym^.definition^.procoptions,po_msgint);
      aktprocsym^.definition^.messageinf.i:=pt^.value;
    end
  else
    Message(parser_e_ill_msg_expr);
  disposetree(pt);
end;


procedure resetvaluepara(p:pnamedindexobject);
begin
  if psym(p)^.typ=varsym then
    with pvarsym(p)^ do
       if copy(name,1,3)='val' then
          aktprocsym^.definition^.parast^.symsearch^.rename(name,copy(name,4,length(name)));
end;


procedure pd_cdecl(const procnames:Tstringcontainer);
begin
  if aktprocsym^.definition^.deftype<>procvardef then
    aktprocsym^.definition^.setmangledname(target_os.Cprefix+realname);
  { do not copy on local !! }
  if (aktprocsym^.definition^.deftype=procdef) and
     assigned(aktprocsym^.definition^.parast) then
    aktprocsym^.definition^.parast^.foreach({$ifdef FPCPROCVAR}@{$endif}resetvaluepara);
end;


procedure pd_pascal(const procnames:Tstringcontainer);
var st,parast : psymtable;
    lastps,ps : psym;
begin
   new(st,init(parasymtable));
   parast:=aktprocsym^.definition^.parast;
   lastps:=nil;
   while assigned(parast^.symindex^.first) and (lastps<>psym(parast^.symindex^.first)) do
     begin
       ps:=psym(parast^.symindex^.first);
       while assigned(ps^.indexnext) and (psym(ps^.indexnext)<>lastps) do
         ps:=psym(ps^.indexnext);
       ps^.owner:=st;
       { recalculate the corrected offset }
       { the really_insert_in_data procedure
         for parasymtable should only calculateoffset PM }
       ps^.insert_in_data;
       { reset the owner correctly }
       ps^.owner:=parast;
       lastps:=ps;
     end;
end;


procedure pd_register(const procnames:Tstringcontainer);
begin
  Message1(parser_w_proc_directive_ignored,'REGISTER');
end;


procedure pd_reintroduce(const procnames:Tstringcontainer);
begin
  Message1(parser_w_proc_directive_ignored,'REINTRODUCE');
end;


procedure pd_syscall(const procnames:Tstringcontainer);
begin
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
  if not(token=_SEMICOLON) and not(idtoken=_NAME) then
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
        {if (aktprocsym^.definition^.options and pocdecl)<>0 then
          import_name:=aktprocsym^.definition^.mangledname
        else
          Message(parser_w_empty_import_name);}
        { this should work both for win32 and Linux !! PM }
        import_name:=realname;
      if not(current_module^.uses_imports) then
       begin
         current_module^.uses_imports:=true;
         importlib^.preparelib(current_module^.modulename^);
       end;
      if not(m_repeat_forward in aktmodeswitches) then
        begin
          { we can only have one overloaded here ! }
          if assigned(aktprocsym^.definition^.nextoverloaded) then
            importlib^.importprocedure(aktprocsym^.definition^.nextoverloaded^.mangledname,
              import_dll,import_nr,import_name)
          else
            importlib^.importprocedure(aktprocsym^.mangledname,import_dll,import_nr,import_name);
        end
      else
        importlib^.importprocedure(aktprocsym^.mangledname,import_dll,import_nr,import_name);
    end
  else
    begin
      if (idtoken=_NAME) then
       begin
         consume(_NAME);
         import_name:=get_stringconst;
         aktprocsym^.definition^.setmangledname(import_name);
       end
      else
       begin
         { external shouldn't override the cdecl/system name }
         if not (pocall_clearstack in aktprocsym^.definition^.proccalloptions) then
           aktprocsym^.definition^.setmangledname(aktprocsym^.name);
       end;
    end;
end;

type
   pd_handler=procedure(const procnames:Tstringcontainer);
   proc_dir_rec=record
     idtok     : ttoken;
     pd_flags  : longint;
     handler   : pd_handler;
     pocall    : tproccalloptions;
     pooption  : tprocoptions;
     mutexclpocall : tproccalloptions;
     mutexclpotype : tproctypeoptions;
     mutexclpo     : tprocoptions;
   end;
const
  {Should contain the number of procedure directives we support.}
  num_proc_directives=31;
  proc_direcdata:array[1..num_proc_directives] of proc_dir_rec=
   (
    (
      idtok:_ABSTRACT;
      pd_flags : pd_interface+pd_object;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_abstract;
      pocall   : [];
      pooption : [po_abstractmethod];
      mutexclpocall : [pocall_internproc,pocall_inline];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_exports,po_interrupt,po_external]
    ),(
      idtok:_ALIAS;
      pd_flags : pd_implemen+pd_body;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_alias;
      pocall   : [];
      pooption : [];
      mutexclpocall : [pocall_inline];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_ASMNAME;
      pd_flags : pd_interface+pd_implemen;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_asmname;
      pocall   : [pocall_cdecl,pocall_clearstack];
      pooption : [po_external];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_ASSEMBLER;
      pd_flags : pd_implemen+pd_body;
      handler  : nil;
      pocall   : [];
      pooption : [po_assembler];
      mutexclpocall : [];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_CDECL;
      pd_flags : pd_interface+pd_implemen+pd_body+pd_procvar;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_cdecl;
      pocall   : [pocall_cdecl,pocall_clearstack];
      pooption : [po_savestdregs];
      mutexclpocall : [pocall_internproc,pocall_leftright,pocall_inline];
      mutexclpotype : [];
      mutexclpo     : [po_assembler,po_external]
    ),(
      idtok:_DYNAMIC;
      pd_flags : pd_interface+pd_object;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_virtual;
      pocall   : [];
      pooption : [po_virtualmethod];
      mutexclpocall : [pocall_internproc,pocall_inline];
      mutexclpotype : [];
      mutexclpo     : [po_exports,po_interrupt,po_external]
    ),(
      idtok:_EXPORT;
      pd_flags : pd_body+pd_global+pd_interface+pd_implemen{??};
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_export;
      pocall   : [];
      pooption : [po_exports];
      mutexclpocall : [pocall_internproc,pocall_inline];
      mutexclpotype : [];
      mutexclpo     : [po_external,po_interrupt]
    ),(
      idtok:_EXTERNAL;
      pd_flags : pd_implemen+pd_interface;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_external;
      pocall   : [];
      pooption : [po_external];
      mutexclpocall : [pocall_internproc,pocall_inline,pocall_palmossyscall];
      mutexclpotype : [];
      mutexclpo     : [po_exports,po_interrupt,po_assembler]
    ),(
      idtok:_FAR;
      pd_flags : pd_implemen+pd_body+pd_interface+pd_procvar;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_far;
      pocall   : [];
      pooption : [];
      mutexclpocall : [pocall_internproc,pocall_inline];
      mutexclpotype : [];
      mutexclpo     : []
    ),(
      idtok:_FORWARD;
      pd_flags : pd_implemen;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_forward;
      pocall   : [];
      pooption : [];
      mutexclpocall : [pocall_internproc,pocall_inline];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_INLINE;
      pd_flags : pd_implemen+pd_body;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_inline;
      pocall   : [pocall_inline];
      pooption : [];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_exports,po_external,po_interrupt]
    ),(
      idtok:_INTERNCONST;
      pd_flags : pd_implemen+pd_body;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_intern;
      pocall   : [pocall_internconst];
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [potype_operator];
      mutexclpo     : []
    ),(
      idtok:_INTERNPROC;
      pd_flags : pd_implemen;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_intern;
      pocall   : [pocall_internproc];
      pooption : [];
      mutexclpocall : [pocall_inline,pocall_clearstack,pocall_leftright,pocall_cdecl];
      mutexclpotype : [potype_constructor,potype_destructor,potype_operator];
      mutexclpo     : [po_exports,po_external,po_interrupt,po_assembler,po_iocheck]
    ),(
      idtok:_INTERRUPT;
      pd_flags : pd_implemen+pd_body;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_interrupt;
      pocall   : [];
      pooption : [po_interrupt];
      mutexclpocall : [pocall_internproc,pocall_cdecl,pocall_clearstack,pocall_leftright,pocall_inline];
      mutexclpotype : [potype_constructor,potype_destructor,potype_operator];
      mutexclpo     : [po_external]
    ),(
      idtok:_IOCHECK;
      pd_flags : pd_implemen+pd_body;
      handler  : nil;
      pocall   : [];
      pooption : [po_iocheck];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_MESSAGE;
      pd_flags : pd_interface+pd_object;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_message;
      pocall   : [];
      pooption : []; { can be po_msgstr or po_msgint }
      mutexclpocall : [pocall_inline,pocall_internproc];
      mutexclpotype : [potype_constructor,potype_destructor,potype_operator];
      mutexclpo     : [po_interrupt,po_external]
    ),(
      idtok:_NEAR;
      pd_flags : pd_implemen+pd_body+pd_procvar;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_near;
      pocall   : [];
      pooption : [];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : []
    ),(
      idtok:_OVERLOAD;
      pd_flags : pd_implemen+pd_interface+pd_body;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_overload;
      pocall   : [];
      pooption : [po_overload];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : []
    ),(
      idtok:_OVERRIDE;
      pd_flags : pd_interface+pd_object;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_override;
      pocall   : [];
      pooption : [po_overridingmethod,po_virtualmethod];
      mutexclpocall : [pocall_inline,pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_exports,po_external,po_interrupt]
    ),(
      idtok:_PASCAL;
      pd_flags : pd_implemen+pd_body+pd_procvar;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_pascal;
      pocall   : [pocall_leftright];
      pooption : [];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_POPSTACK;
      pd_flags : pd_interface+pd_implemen+pd_body+pd_procvar;
      handler  : nil;
      pocall   : [pocall_clearstack];
      pooption : [];
      mutexclpocall : [pocall_inline,pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_assembler,po_external]
    ),(
      idtok:_PUBLIC;
      pd_flags : pd_implemen+pd_body+pd_global+pd_notobject;
      handler  : nil;
      pocall   : [];
      pooption : [];
      mutexclpocall : [pocall_internproc,pocall_inline];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_REGISTER;
      pd_flags : pd_interface+pd_implemen+pd_body+pd_procvar;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_register;
      pocall   : [pocall_register];
      pooption : [];
      mutexclpocall : [pocall_leftright,pocall_cdecl,pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_REINTRODUCE;
      pd_flags : pd_interface+pd_object;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_reintroduce;
      pocall   : [];
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [];
      mutexclpo     : []
    ),(
      idtok:_SAFECALL;
      pd_flags : pd_interface+pd_implemen+pd_body+pd_procvar;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_safecall;
      pocall   : [pocall_safecall];
      pooption : [po_savestdregs];
      mutexclpocall : [pocall_leftright,pocall_cdecl,pocall_internproc,pocall_inline];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_SAVEREGISTERS;
      pd_flags : pd_interface+pd_implemen+pd_body+pd_procvar;
      handler  : nil;
      pocall   : [];
      pooption : [po_saveregisters];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_STATIC;
      pd_flags : pd_interface+pd_object;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_static;
      pocall   : [];
      pooption : [po_staticmethod];
      mutexclpocall : [pocall_inline,pocall_internproc];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_external,po_interrupt,po_exports]
    ),(
      idtok:_STDCALL;
      pd_flags : pd_interface+pd_implemen+pd_body+pd_procvar;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_stdcall;
      pocall   : [pocall_stdcall];
      pooption : [po_savestdregs];
      mutexclpocall : [pocall_leftright,pocall_cdecl,pocall_inline,pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_SYSCALL;
      pd_flags : pd_interface;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_syscall;
      pocall   : [pocall_palmossyscall];
      pooption : [];
      mutexclpocall : [pocall_cdecl,pocall_inline,pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_external,po_assembler,po_interrupt,po_exports]
    ),(
      idtok:_SYSTEM;
      pd_flags : pd_implemen;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_system;
      pocall   : [pocall_clearstack];
      pooption : [];
      mutexclpocall : [pocall_leftright,pocall_inline,pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_external,po_assembler,po_interrupt]
    ),(
      idtok:_VIRTUAL;
      pd_flags : pd_interface+pd_object;
      handler  : {$ifdef FPCPROCVAR}@{$endif}pd_virtual;
      pocall   : [];
      pooption : [po_virtualmethod];
      mutexclpocall : [pocall_inline,pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_external,po_interrupt,po_exports]
    )
   );


function is_proc_directive(tok:ttoken):boolean;
var
  i : longint;
begin
  is_proc_directive:=false;
  for i:=1 to num_proc_directives do
   if proc_direcdata[i].idtok=idtoken then
    begin
      is_proc_directive:=true;
      exit;
    end;
end;


function parse_proc_direc(const proc_names:Tstringcontainer;var pdflags:word):boolean;
{
  Parse the procedure directive, returns true if a correct directive is found
}
var
  p     : longint;
  found : boolean;
  name  : string;
begin
  parse_proc_direc:=false;
  name:=pattern;
  found:=false;
  for p:=1 to num_proc_directives do
   if proc_direcdata[p].idtok=idtoken then
    begin
      found:=true;
      break;
    end;

{ Check if the procedure directive is known }
  if not found then
   begin
      { parsing a procvar type the name can be any
        next variable !! }
      if (pdflags and (pd_procvar or pd_object))=0 then
        Message1(parser_w_unknown_proc_directive_ignored,name);
      exit;
   end;

  { static needs a special treatment }
  if (idtoken=_STATIC) and not (cs_static_keyword in aktmoduleswitches) then
    exit;

{ Conflicts between directives ? }
  if (aktprocsym^.definition^.proctypeoption in proc_direcdata[p].mutexclpotype) or
     ((aktprocsym^.definition^.proccalloptions*proc_direcdata[p].mutexclpocall)<>[]) or
     ((aktprocsym^.definition^.procoptions*proc_direcdata[p].mutexclpo)<>[]) then
   begin
     Message1(parser_e_proc_dir_conflict,name);
     exit;
   end;

{ Check if the directive is only for objects }
  if ((proc_direcdata[p].pd_flags and pd_object)<>0) and
     not assigned(aktprocsym^.definition^._class) then
    begin
      exit;
    end;
{ check if method and directive not for object public }
  if ((proc_direcdata[p].pd_flags and pd_notobject)<>0) and
     assigned(aktprocsym^.definition^._class) then
    begin
      exit;
    end;

{ consume directive, and turn flag on }
  consume(token);
  parse_proc_direc:=true;

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
  aktprocsym^.definition^.proccalloptions:=aktprocsym^.definition^.proccalloptions+proc_direcdata[p].pocall;
  aktprocsym^.definition^.procoptions:=aktprocsym^.definition^.procoptions+proc_direcdata[p].pooption;

 { Adjust positions of args for cdecl or stdcall }
   if (aktprocsym^.definition^.deftype=procdef) and
      (([pocall_cdecl,pocall_stdcall]*aktprocsym^.definition^.proccalloptions)<>[]) then
     aktprocsym^.definition^.parast^.set_alignment(target_os.size_of_longint);

{ Call the handler }
  if pointer({$ifndef FPC}@{$endif}proc_direcdata[p].handler)<>nil then
    proc_direcdata[p].handler(proc_names);
end;

{***************************************************************************}

function check_identical_proc(var p : pprocdef) : boolean;
{
  Search for idendical definitions,
  if there is a forward, then kill this.

  Returns the result of the forward check.

  Removed from unter_dec to keep the source readable
}
var
  hd,pd : Pprocdef;
  storeparast : psymtable;
  ad,fd : psym;
  s : string;
begin
  check_identical_proc:=false;
  p:=nil;
  pd:=aktprocsym^.definition;
  if assigned(pd) then
   begin
   { Is there an overload/forward ? }
     if assigned(pd^.nextoverloaded) then
      begin
      { walk the procdef list }
        while (assigned(pd)) and (assigned(pd^.nextoverloaded)) do
         begin
           hd:=pd^.nextoverloaded;

           { check the parameters }
           if (not(m_repeat_forward in aktmodeswitches) and
               (aktprocsym^.definition^.maxparacount=0)) or
              (equal_paras(aktprocsym^.definition^.para,hd^.para,cp_none) and
              { for operators equal_paras is not enough !! }
              ((aktprocsym^.definition^.proctypeoption<>potype_operator) or (optoken<>_ASSIGNMENT) or
               is_equal(hd^.rettype.def,aktprocsym^.definition^.rettype.def))) then
             begin
               if not equal_paras(aktprocsym^.definition^.para,hd^.para,cp_all) and
                  ((m_repeat_forward in aktmodeswitches) or
                   (aktprocsym^.definition^.maxparacount>0)) then
                 begin
                    MessagePos1(aktprocsym^.definition^.fileinfo,parser_e_header_dont_match_forward,
                                aktprocsym^.demangledName);
                    exit;
                 end;
               if hd^.forwarddef then
               { remove the forward definition  but don't delete it,      }
               { the symtable is the owner !!  }
                 begin
                 { Check if the procedure type and return type are correct }
                   if (hd^.proctypeoption<>aktprocsym^.definition^.proctypeoption) or
                      (not(is_equal(hd^.rettype.def,aktprocsym^.definition^.rettype.def)) and
                      (m_repeat_forward in aktmodeswitches)) then
                     begin
                       MessagePos1(aktprocsym^.definition^.fileinfo,parser_e_header_dont_match_forward,
                                   aktprocsym^.demangledName);
                       exit;
                     end;
                   { Check calling convention, no check for internconst,internproc which
                     are only defined in interface or implementation }
                   if (hd^.proccalloptions-[pocall_internconst,pocall_internproc]<>
                       aktprocsym^.definition^.proccalloptions-[pocall_internconst,pocall_internproc]) then
                    begin
                      { only trigger an error, becuase it doesn't hurt }
                      MessagePos(aktprocsym^.definition^.fileinfo,parser_e_call_convention_dont_match_forward);
                      { set the mangledname to the interface name so it doesn't trigger
                        the Note about different manglednames (PFV) }
                      aktprocsym^.definition^.setmangledname(hd^.mangledname);
                    end;
                   { manglednames are equal? }
                   hd^.count:=false;
                   if (m_repeat_forward in aktmodeswitches) or
                      aktprocsym^.definition^.haspara then
                    begin
                      if (hd^.mangledname<>aktprocsym^.definition^.mangledname) then
                       begin
                         if not(po_external in aktprocsym^.definition^.procoptions) then
                           MessagePos2(aktprocsym^.definition^.fileinfo,parser_n_interface_name_diff_implementation_name,hd^.mangledname,
                             aktprocsym^.definition^.mangledname);
                       { reset the mangledname of the interface part to be sure }
                       { this is wrong because the mangled name might have been used already !! }
                          if hd^.is_used then
                            renameasmsymbol(hd^.mangledname,aktprocsym^.definition^.mangledname);
                          hd^.setmangledname(aktprocsym^.definition^.mangledname);
                       { so we need to keep the name of interface !!
                         No!!!! The procedure directives can change the mangledname.
                         I fixed this by first calling check_identical_proc and then doing
                         the proc directives, but this is not a good solution.(DM)}
                         { this is also wrong (PM)
                         aktprocsym^.definition^.setmangledname(hd^.mangledname);}
                       end
                      else
                       begin
                       { If mangled names are equal, therefore    }
                       { they have the same number of parameters  }
                       { Therefore we can check the name of these }
                       { parameters...                      }
                         if hd^.forwarddef and aktprocsym^.definition^.forwarddef then
                           begin
                             MessagePos1(aktprocsym^.definition^.fileinfo,
                                         parser_e_function_already_declared_public_forward,aktprocsym^.demangledName);
                             check_identical_proc:=true;
                           { Remove other forward from the list to reduce errors }
                             pd^.nextoverloaded:=pd^.nextoverloaded^.nextoverloaded;
                             exit;
                           end;
                         ad:=psym(hd^.parast^.symindex^.first);
                         fd:=psym(aktprocsym^.definition^.parast^.symindex^.first);
                         if assigned(ad) and assigned(fd) then
                           begin
                             while assigned(ad) and assigned(fd) do
                               begin
                                 s:=ad^.name;
                                 if s<>fd^.name then
                                   begin
                                     MessagePos3(aktprocsym^.definition^.fileinfo,parser_e_header_different_var_names,
                                       aktprocsym^.name,s,fd^.name);
                                     break;
                                   end;
                               { it is impossible to have a nil pointer }
                               { for only one parameter - since they    }
                               { have the same number of parameters.    }
                               { Left = next parameter.          }
                                 ad:=psym(ad^.left);
                                 fd:=psym(fd^.left);
                               end;
                           end;
                       end;
                    end;
                 { also the para_offset }
                   hd^.parast^.address_fixup:=aktprocsym^.definition^.parast^.address_fixup;
                   hd^.count:=true;

                 { remove pd^.nextoverloaded from the list }
                 { and add aktprocsym^.definition }
                   pd^.nextoverloaded:=pd^.nextoverloaded^.nextoverloaded;
                   hd^.nextoverloaded:=aktprocsym^.definition^.nextoverloaded;
                 { Alert! All fields of aktprocsym^.definition that are modified
                   by the procdir handlers must be copied here!.}
                   hd^.forwarddef:=false;
                   hd^.hasforward:=true;
                   hd^.proccalloptions:=hd^.proccalloptions + aktprocsym^.definition^.proccalloptions;
                   hd^.procoptions:=hd^.procoptions + aktprocsym^.definition^.procoptions;
                   if aktprocsym^.definition^.extnumber=-1 then
                     aktprocsym^.definition^.extnumber:=hd^.extnumber
                   else
                     if hd^.extnumber=-1 then
                       hd^.extnumber:=aktprocsym^.definition^.extnumber;
                   { switch parast for warning in implementation  PM }
                   if (m_repeat_forward in aktmodeswitches) or
                      aktprocsym^.definition^.haspara then
                     begin
                        storeparast:=hd^.parast;
                        hd^.parast:=aktprocsym^.definition^.parast;
                        aktprocsym^.definition^.parast:=storeparast;
                     end;
                   if pd=aktprocsym^.definition then
                     p:=nil
                   else
                     p:=pd;
                   aktprocsym^.definition:=hd;
                   check_identical_proc:=true;
                 end
               else
               { abstract methods aren't forward defined, but this }
               { needs another error message                   }
                 if not(po_abstractmethod in pd^.nextoverloaded^.procoptions) then
                   MessagePos(aktprocsym^.definition^.fileinfo,parser_e_overloaded_have_same_parameters)
                 else
                   MessagePos(aktprocsym^.definition^.fileinfo,parser_e_abstract_no_definition);
               break;
             end;

           { check for allowing overload directive }
           if not(m_fpc in aktmodeswitches) then
            begin
              { overload directive turns on overloading }
              if ((po_overload in aktprocsym^.definition^.procoptions) or
                  ((po_overload in hd^.procoptions))) then
               begin
                 { check if all procs have overloading, but not if the proc was
                   already declared forward, then the check is already done }
                 if not(hd^.hasforward) and
                    (aktprocsym^.definition^.forwarddef=hd^.forwarddef) and
                    not((po_overload in aktprocsym^.definition^.procoptions) and
                        ((po_overload in hd^.procoptions))) then
                  begin
                    MessagePos1(aktprocsym^.definition^.fileinfo,parser_e_no_overload_for_all_procs,aktprocsym^.name);
                    break;
                  end;
               end
              else
               begin
                 if not(hd^.forwarddef) then
                  begin
                    MessagePos(aktprocsym^.definition^.fileinfo,parser_e_procedure_overloading_is_off);
                    break;
                  end;
               end;
            end;

           { try next overloaded }
           pd:=pd^.nextoverloaded;
         end;
      end
     else
      begin
      { there is no overloaded, so its always identical with itself }
        check_identical_proc:=true;
      end;
   end;
{ insert opsym only in the right symtable }
  if ((procinfo^.flags and pi_operator)<>0) and assigned(opsym)
     and not parse_only then
    begin
      if ret_in_param(aktprocsym^.definition^.rettype.def) then
        begin
          pprocdef(aktprocsym^.definition)^.parast^.insert(opsym);
        { this increases the data size }
        { correct this to get the right ret $value }
          dec(pprocdef(aktprocsym^.definition)^.parast^.datasize,opsym^.getpushsize);
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
   oldexitlabel,oldexit2label : pasmlabel;
   oldfaillabel,oldquickexitlabel:Pasmlabel;
   _class,hp:Pobjectdef;
   { switches can change inside the procedure }
   entryswitches, exitswitches : tlocalswitches;
   oldaktmaxfpuregisters,localmaxfpuregisters : longint;
   { code for the subroutine as tree }
{$ifdef newcg}
   code:ptree;
{$else newcg}
   code:ptree;
{$endif newcg}
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

   { static is also important for local procedures !! }
   if (po_staticmethod in aktprocsym^.definition^.procoptions) then
     allow_only_static:=true
   else if (lexlevel=normal_function_level) then
     allow_only_static:=false;

   { save old labels }
   oldexitlabel:=aktexitlabel;
   oldexit2label:=aktexit2label;
   oldquickexitlabel:=quickexitlabel;
   oldfaillabel:=faillabel;
   { get new labels }
   getlabel(aktexitlabel);
   getlabel(aktexit2label);
   { exit for fail in constructors }
   if (aktprocsym^.definition^.proctypeoption=potype_constructor) then
     begin
       getlabel(faillabel);
       getlabel(quickexitlabel);
     end;
   { reset break and continue labels }
   block_type:=bt_general;
   aktbreaklabel:=nil;
   aktcontinuelabel:=nil;

   { insert symtables for the class, by only if it is no nested function }
   if assigned(procinfo^._class) and not(parent_has_class) then
     begin
       { insert them in the reverse order ! }
       hp:=nil;
       repeat
         _class:=procinfo^._class;
         while _class^.childof<>hp do
           _class:=_class^.childof;
         hp:=_class;
         _class^.symtable^.next:=symtablestack;
         symtablestack:=_class^.symtable;
       until hp=procinfo^._class;
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

{$ifdef newcg}
   tg.usedinproc:=[];
{$else newcg}
   { no registers are used }
   usedinproc:=0;
{$endif newcg}
   { save entry info }
   entrypos:=aktfilepos;
   entryswitches:=aktlocalswitches;
   localmaxfpuregisters:=aktmaxfpuregisters;
{$ifdef newcg}
{$ifdef dummy}
   { parse the code ... }
   if (po_assembler in aktprocsym^.definition^.procoptions) then
     code:=convtree2node(assembler_block)
   else
     code:=convtree2node(block(current_module^.islibrary));
{$endif dummy}
   { parse the code ... }
   if (po_assembler in aktprocsym^.definition^.procoptions) then
     code:=assembler_block
   else
     code:=block(current_module^.islibrary);
{$else newcg}
   { parse the code ... }
   if (po_assembler in aktprocsym^.definition^.procoptions) then
     code:=assembler_block
   else
     code:=block(current_module^.islibrary);
{$endif newcg}

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
   { but only if the are no local variables           }
   { already done in assembler_block }
{$ifdef newcg}
   tg.setfirsttemp(procinfo^.firsttemp_offset);
{$else newcg}
   setfirsttemp(procinfo^.firsttemp_offset);
{$endif newcg}

   { ... and generate assembler }
   { but set the right switches for entry !! }
   aktlocalswitches:=entryswitches;
   oldaktmaxfpuregisters:=aktmaxfpuregisters;
   aktmaxfpuregisters:=localmaxfpuregisters;
{$ifndef NOPASS2}
{$ifdef newcg}
   if assigned(code) then
     generatecode(code);
{$else newcg}
   if assigned(code) then
     generatecode(code);
{$endif newcg}
   { set switches to status at end of procedure }
   aktlocalswitches:=exitswitches;

   if assigned(code) then
     begin
        aktprocsym^.definition^.code:=code;

        { the procedure is now defined }
        aktprocsym^.definition^.forwarddef:=false;
{$ifdef newcg}
        aktprocsym^.definition^.usedregisters:=tg.usedinproc;
{$else newcg}
        aktprocsym^.definition^.usedregisters:=usedinproc;
{$endif newcg}
     end;

{$ifdef newcg}
   stackframe:=tg.gettempsize;
{$else newcg}
   stackframe:=gettempsize;
{$endif newcg}

   { first generate entry code with the correct position and switches }
   aktfilepos:=entrypos;
   aktlocalswitches:=entryswitches;
{$ifdef newcg}
   if assigned(code) then
     cg^.g_entrycode(procinfo^.aktentrycode,proc_names,make_global,stackframe,parasize,nostackframe,false);
{$else newcg}
   if assigned(code) then
     genentrycode(procinfo^.aktentrycode,proc_names,make_global,stackframe,parasize,nostackframe,false);
{$endif newcg}

   { now generate exit code with the correct position and switches }
   aktfilepos:=exitpos;
   aktlocalswitches:=exitswitches;
   if assigned(code) then
     begin
{$ifdef newcg}
       cg^.g_exitcode(procinfo^.aktexitcode,parasize,nostackframe,false);
{$else newcg}
       genexitcode(procinfo^.aktexitcode,parasize,nostackframe,false);
{$endif newcg}
       procinfo^.aktproccode^.insertlist(procinfo^.aktentrycode);
       procinfo^.aktproccode^.concatlist(procinfo^.aktexitcode);
{$ifdef i386}
 {$ifndef NoOpt}
       if (cs_optimize in aktglobalswitches) and
       { do not optimize pure assembler procedures }
         ((procinfo^.flags and pi_is_assembler)=0)  then
           Optimize(procinfo^.aktproccode);
 {$endif NoOpt}
{$endif}
       { save local data (casetable) also in the same file }
       if assigned(procinfo^.aktlocaldata) and
          (not procinfo^.aktlocaldata^.empty) then
         begin
            procinfo^.aktproccode^.concat(new(pai_section,init(sec_data)));
            procinfo^.aktproccode^.concatlist(procinfo^.aktlocaldata);
            procinfo^.aktproccode^.concat(new(pai_section,init(sec_code)));
         end;
       { now we can insert a cut }
       if (cs_create_smart in aktmoduleswitches) then
         codesegment^.concat(new(pai_cut,init));

       { add the procedure to the codesegment }
       codesegment^.concatlist(procinfo^.aktproccode);
     end;
{$else}
   if assigned(code) then
    firstpass(code);
{$endif NOPASS2}

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
       if (Errorcount=0) then
         begin
           aktprocsym^.definition^.localst^.check_forwards;
           aktprocsym^.definition^.localst^.checklabels;
         end;
       if (procinfo^.flags and pi_uses_asm)=0 then
         begin
            { not for unit init, becuase the var can be used in finalize,
              it will be done in proc_unit }
            if not(aktprocsym^.definition^.proctypeoption
               in [potype_proginit,potype_unitinit,potype_unitfinalize]) then
               aktprocsym^.definition^.localst^.allsymbolsused;
            aktprocsym^.definition^.parast^.allsymbolsused;
         end;
     end;

   { the local symtables can be deleted, but the parast   }
   { doesn't, (checking definitons when calling a        }
   { function                                        }
   { not for a inline procedure !!               (PM)   }
   { at lexlevel = 1 localst is the staticsymtable itself }
   { so no dispose here !!                              }
   if assigned(code) and
      not(cs_browser in aktmoduleswitches) and
      not(pocall_inline in aktprocsym^.definition^.proccalloptions) then
     begin
       if lexlevel>=normal_function_level then
         dispose(aktprocsym^.definition^.localst,done);
       aktprocsym^.definition^.localst:=nil;
     end;

{$ifdef newcg}
   { all registers can be used again }
   tg.resetusableregisters;
   { only now we can remove the temps }
   tg.resettempgen;
{$else newcg}
   { all registers can be used again }
   resetusableregisters;
   { only now we can remove the temps }
   resettempgen;
{$endif newcg}

   { remove code tree, if not inline procedure }
   if assigned(code) and not(pocall_inline in aktprocsym^.definition^.proccalloptions) then
{$ifdef newcg}
     {!!!!!!! dispose(code,done); }
     disposetree(code);
{$else newcg}
     disposetree(code);
{$endif newcg}

   { remove class member symbol tables }
   while symtablestack^.symtabletype=objectsymtable do
     symtablestack:=symtablestack^.next;

   aktmaxfpuregisters:=oldaktmaxfpuregisters;

   { restore filepos, the switches are already set }
   aktfilepos:=savepos;
   { restore labels }
   aktexitlabel:=oldexitlabel;
   aktexit2label:=oldexit2label;
   quickexitlabel:=oldquickexitlabel;
   faillabel:=oldfaillabel;

   { reset to normal non static function }
   if (lexlevel=normal_function_level) then
     allow_only_static:=false;
   { previous lexlevel }
   dec(lexlevel);
end;


procedure parse_proc_directives(Anames:Pstringcontainer;var pdflags:word);
{
  Parse the procedure directives. It does not matter if procedure directives
  are written using ;procdir; or ['procdir'] syntax.
}
var
  res : boolean;
begin
  while token in [_ID,_LECKKLAMMER] do
   begin
     if try_to_consume(_LECKKLAMMER) then
      begin
        repeat
          parse_proc_direc(Anames^,pdflags);
        until not try_to_consume(_COMMA);
        consume(_RECKKLAMMER);
        { we always expect at least '[];' }
        res:=true;
      end
     else
      res:=parse_proc_direc(Anames^,pdflags);
   { A procedure directive normally followed by a semicolon, but in
     a const section we should stop when _EQUAL is found }
     if res then
      begin
        if (block_type=bt_const) and
           (token=_EQUAL) then
         break;
        { support procedure proc;stdcall export; in Delphi mode only }
        if not((m_delphi in aktmodeswitches) and
               is_proc_directive(token)) then
         consume(_SEMICOLON);
      end
     else
      break;
   end;
end;

procedure parse_var_proc_directives(var sym : psym);
var
  anames  : pstringcontainer;
  pdflags : word;
  oldsym  : pprocsym;
  pd      : pabstractprocdef;
begin
  oldsym:=aktprocsym;
  anames:=new(pstringcontainer,init);
  pdflags:=pd_procvar;
  { we create a temporary aktprocsym to read the directives }
  aktprocsym:=new(pprocsym,init(sym^.name));
  case sym^.typ of
    varsym :
      pd:=pabstractprocdef(pvarsym(sym)^.vartype.def);
    typedconstsym :
      pd:=pabstractprocdef(ptypedconstsym(sym)^.typedconsttype.def);
    typesym :
      pd:=pabstractprocdef(ptypesym(sym)^.restype.def);
    else
      internalerror(994932432);
  end;
  if pd^.deftype<>procvardef then
   internalerror(994932433);
  pabstractprocdef(aktprocsym^.definition):=pd;
  { names should never be used anyway }
  inc(lexlevel);
  parse_proc_directives(anames,pdflags);
  dec(lexlevel);
  aktprocsym^.definition:=nil;
  dispose(aktprocsym,done);
  dispose(anames,done);
  aktprocsym:=oldsym;
end;

procedure parse_object_proc_directives(var sym : pprocsym);
var
  anames : pstringcontainer;
  pdflags : word;
begin
  pdflags:=pd_object;
  anames:=new(pstringcontainer,init);
  inc(lexlevel);
  parse_proc_directives(anames,pdflags);
  dec(lexlevel);
  dispose(anames,done);
  if (po_containsself in aktprocsym^.definition^.procoptions) and
     (([po_msgstr,po_msgint]*aktprocsym^.definition^.procoptions)=[]) then
    Message(parser_e_self_in_non_message_handler);
end;

procedure checkvaluepara(p:pnamedindexobject);
var
  vs : pvarsym;
  s  : string;
begin
  with pvarsym(p)^ do
   begin
     if copy(name,1,3)='val' then
      begin
        s:=Copy(name,4,255);
        if not(po_assembler in aktprocsym^.definition^.procoptions) then
         begin
           vs:=new(Pvarsym,initdef(s,vartype.def));
           vs^.fileinfo:=fileinfo;
           vs^.varspez:=varspez;
           aktprocsym^.definition^.localst^.insert(vs);
           include(vs^.varoptions,vo_is_local_copy);
           vs^.varstate:=vs_assigned;
           localvarsym:=vs;
           inc(refs); { the para was used to set the local copy ! }
           { warnings only on local copy ! }
           varstate:=vs_used;
         end
        else
         begin
           aktprocsym^.definition^.parast^.rename(name,s);
         end;
      end;
   end;
end;


procedure read_proc;
{
  Parses the procedure directives, then parses the procedure body, then
  generates the code for it
}
var
  oldprefix     : string;
  oldprocsym       : Pprocsym;
  oldprocinfo      : pprocinfo;
  oldconstsymtable : Psymtable;
  oldfilepos       : tfileposinfo;
  names           : Pstringcontainer;
  pdflags         : word;
  prevdef,stdef   : pprocdef;
begin
{ save old state }
   oldprocsym:=aktprocsym;
   oldprefix:=procprefix;
   oldconstsymtable:=constsymtable;
   oldprocinfo:=procinfo;
{ create a new procedure }
   new(names,init);
{$ifdef fixLeaksOnError}
   strContStack.push(names);
{$endif fixLeaksOnError}
   codegen_newprocedure;
   with procinfo^ do
    begin
      parent:=oldprocinfo;
    { clear flags }
      flags:=0;
    { standard frame pointer }
      framepointer:=frame_pointer;
      { funcret_is_valid:=false; }
      funcret_state:=vs_declared;
    { is this a nested function of a method ? }
      if assigned(oldprocinfo) then
        _class:=oldprocinfo^._class;
    end;

   parse_proc_dec;

   procinfo^.sym:=aktprocsym;
   procinfo^.def:=aktprocsym^.definition;

{ set the default function options }
   if parse_only then
    begin
      aktprocsym^.definition^.forwarddef:=true;
      { set also the interface flag, for better error message when the
        implementation doesn't much this header }
      aktprocsym^.definition^.interfacedef:=true;
      pdflags:=pd_interface;
    end
   else
    begin
      pdflags:=pd_body;
      if current_module^.in_implementation then
       pdflags:=pdflags or pd_implemen;
      if (not current_module^.is_unit) or (cs_create_smart in aktmoduleswitches) then
       pdflags:=pdflags or pd_global;
      procinfo^.exported:=false;
      aktprocsym^.definition^.forwarddef:=false;
    end;

{ parse the directives that may follow }
   inc(lexlevel);
   parse_proc_directives(names,pdflags);
   dec(lexlevel);

{ set aktfilepos to the beginning of the function declaration }
   oldfilepos:=aktfilepos;
   aktfilepos:=aktprocsym^.definition^.fileinfo;

{ search for forward declarations }
   if not check_identical_proc(prevdef) then
     begin
     { A method must be forward defined (in the object declaration) }
       if assigned(procinfo^._class) and (not assigned(oldprocinfo^._class)) then
        begin
          Message1(parser_e_header_dont_match_any_member,aktprocsym^.demangledName);
          aktprocsym^.write_parameter_lists(aktprocsym^.definition);
        end
       else
        begin
          { Give a better error if there is a forward def in the interface and only
            a single implementation }
          if (not aktprocsym^.definition^.forwarddef) and
             assigned(aktprocsym^.definition^.nextoverloaded) and
             aktprocsym^.definition^.nextoverloaded^.forwarddef and
             aktprocsym^.definition^.nextoverloaded^.interfacedef and
             not(assigned(aktprocsym^.definition^.nextoverloaded^.nextoverloaded)) then
           begin
             Message1(parser_e_header_dont_match_forward,aktprocsym^.demangledName);
             aktprocsym^.write_parameter_lists(aktprocsym^.definition);
           end
          else
           begin
           { check the global flag }
             if (procinfo^.flags and pi_is_global)<>0 then
               Message(parser_e_overloaded_must_be_all_global);
           end;
        end;
     end;

   { set return type here, becuase the aktprocsym^.definition can be
     changed by check_identical_proc (PFV) }
   procinfo^.returntype.def:=aktprocsym^.definition^.rettype.def;

{$ifdef i386}
   if (po_interrupt in aktprocsym^.definition^.procoptions) then
     begin
       { we push Flags and CS as long
         to cope with the IRETD
         and we save 6 register + 4 selectors }
       inc(procinfo^.para_offset,8+6*4+4*2);
     end;
{$endif i386}

   { pointer to the return value ? }
   if ret_in_param(procinfo^.returntype.def) then
    begin
      procinfo^.return_offset:=procinfo^.para_offset;
      inc(procinfo^.para_offset,target_os.size_of_pointer);
    end;
   { allows to access the parameters of main functions in nested functions }
   aktprocsym^.definition^.parast^.address_fixup:=procinfo^.para_offset;

   { when it is a value para and it needs a local copy then rename
     the parameter and insert a copy in the localst. This is not done
     for assembler procedures }
   if (not parse_only) and (not aktprocsym^.definition^.forwarddef) then
     aktprocsym^.definition^.parast^.foreach({$ifdef FPCPROCVAR}@{$endif}checkvaluepara);

{ restore file pos }
   aktfilepos:=oldfilepos;

{ compile procedure when a body is needed }
   if (pdflags and pd_body)<>0 then
     begin
       Message1(parser_p_procedure_start,aktprocsym^.demangledname);
       names^.insert(aktprocsym^.definition^.mangledname);
      { set _FAIL as keyword if constructor }
      if (aktprocsym^.definition^.proctypeoption=potype_constructor) then
        tokeninfo^[_FAIL].keyword:=m_all;
      if assigned(aktprocsym^.definition^._class) then
        tokeninfo^[_SELF].keyword:=m_all;

       compile_proc_body(names^,((pdflags and pd_global)<>0),assigned(oldprocinfo^._class));

      { reset _FAIL as normal }
      if (aktprocsym^.definition^.proctypeoption=potype_constructor) then
        tokeninfo^[_FAIL].keyword:=m_none;
      if assigned(aktprocsym^.definition^._class) and (lexlevel=main_program_level) then
        tokeninfo^[_SELF].keyword:=m_none;
       consume(_SEMICOLON);
     end;
{ close }
{$ifdef fixLeaksOnError}
   if names <> strContStack.pop then
     writeln('problem with strContStack in psub!');
{$endif fixLeaksOnError}
   dispose(names,done);
   codegen_doneprocedure;
{ Restore old state }
   constsymtable:=oldconstsymtable;
   { from now on all refernece to mangledname means
     that the function is already used }
   aktprocsym^.definition^.count:=true;
   { restore the interface order to maintain CRC values PM }
   if assigned(prevdef) and assigned(aktprocsym^.definition^.nextoverloaded) then
     begin
       stdef:=aktprocsym^.definition;
       aktprocsym^.definition:=stdef^.nextoverloaded;
       stdef^.nextoverloaded:=prevdef^.nextoverloaded;
       prevdef^.nextoverloaded:=stdef;
     end;
   aktprocsym:=oldprocsym;
   procprefix:=oldprefix;
   procinfo:=oldprocinfo;
   opsym:=nil;
end;

end.
{
  $Log$
  Revision 1.14  2000-09-24 21:19:51  peter
    * delphi compile fixes

  Revision 1.13  2000/09/24 15:06:24  peter
    * use defines.inc

  Revision 1.12  2000/09/10 20:11:07  peter
    * overload checking in implementation removed (merged)

  Revision 1.11  2000/09/04 20:15:19  peter
    * fixed operator overloading

  Revision 1.10  2000/08/27 16:11:52  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.9  2000/08/16 18:33:54  peter
    * splitted namedobjectitem.next into indexnext and listnext so it
      can be used in both lists
    * don't allow "word = word" type definitions (merged)

  Revision 1.8  2000/08/13 12:54:56  peter
    * class member decl wrong then no other error after it
    * -vb has now also line numbering
    * -vb is also used for interface/implementation different decls and
      doesn't list the current function (merged)

  Revision 1.7  2000/08/08 19:28:57  peter
    * memdebug/memory patches (merged)
    * only once illegal directive (merged)

  Revision 1.6  2000/08/06 19:39:28  peter
    * default parameters working !

  Revision 1.5  2000/08/06 14:17:15  peter
    * overload fixes (merged)

  Revision 1.4  2000/07/30 17:04:43  peter
    * merged fixes

  Revision 1.3  2000/07/13 12:08:27  michael
  + patched to 1.1.0 with former 1.09patch from peter

  Revision 1.2  2000/07/13 11:32:46  michael
  + removed logs
}
