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

uses
  cobjects,
  symconst,symtable;

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
procedure parse_var_proc_directives(var sym : ptypesym);
procedure parse_object_proc_directives(var sym : pprocsym);
procedure read_proc;


implementation

uses
  globtype,systems,tokens,
  strings,globals,verbose,files,
  scanner,aasm,tree,types,
  import,gendef,
  hcodegen,temp_gen,pass_1,cpubase,cpuasm
{$ifndef NOPASS2}
  ,pass_2
{$endif}
{$ifdef GDB}
  ,gdb
{$endif GDB}
{$ifdef i386}
{$ifndef newcg}
  ,tgeni386
  ,cgai386
{$endif newcg}
  {$ifndef NoOpt}
  ,aopt386
  {$endif}
{$endif}
{$ifdef m68k}
  ,tgen68k,cga68k
{$endif}
  { parser specific stuff }
  ,pbase,pdecl,pexpr,pstatmnt
{$ifdef newcg}
  ,tgcpu,convtree,cgobj,tgeni386  { for the new code generator tgeni386 is only a dummy }
{$endif newcg}
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
  storetokenpos : tfileposinfo;
  p       : Pdef;
  hsym    : psym;
  hvs,
  vs      : Pvarsym;
  hs1,hs2 : string;
  varspez : Tvarspez;
  inserthigh : boolean;
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
    inserthigh:=false;
    readtypesym:=nil;
    if idtoken=_SELF then
      begin
         { we parse the defintion in the class definition }
         if assigned(procinfo._class) and procinfo._class^.is_class then
           begin
{$ifndef UseNiceNames}
            hs2:=hs2+'$'+'self';
{$else UseNiceNames}
            hs2:=hs2+tostr(length('self'))+'self';
{$endif UseNiceNames}
            vs:=new(Pvarsym,init('@',procinfo._class));
            vs^.varspez:=vs_var;
          { insert the sym in the parasymtable }
            aktprocsym^.definition^.parast^.insert(vs);
{$ifdef INCLUDEOK}
            include(aktprocsym^.definition^.procoptions,po_containsself);
{$else}
            aktprocsym^.definition^.procoptions:=aktprocsym^.definition^.procoptions+[po_containsself];
{$endif}
            inc(procinfo.ESI_offset,vs^.address);
            consume(idtoken);
            consume(_COLON);
            p:=single_type(hs1);
            if assigned(readtypesym) then
             aktprocsym^.definition^.concattypesym(readtypesym,vs_value)
            else
             aktprocsym^.definition^.concatdef(p,vs_value);
            CheckTypes(p,procinfo._class);
           end
         else
           consume(_ID);
      end
    else
      begin
       { read identifiers }
         sc:=idlist;
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
                  { we don't need the typesym anymore }
                  readtypesym:=nil;
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
               p:=openshortstringdef;
               hs1:='openstring';
               inserthigh:=true;
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
            p:=cformaldef;
            { }
          end;
         hs2:=aktprocsym^.definition^.mangledname;
         storetokenpos:=tokenpos;
         while not sc^.empty do
          begin
{$ifndef UseNiceNames}
            hs2:=hs2+'$'+hs1;
{$else UseNiceNames}
            hs2:=hs2+tostr(length(hs1))+hs1;
{$endif UseNiceNames}
            s:=sc^.get_with_tokeninfo(tokenpos);
            if assigned(readtypesym) then
             begin
               aktprocsym^.definition^.concattypesym(readtypesym,varspez);
               vs:=new(Pvarsym,initsym(s,readtypesym))
             end
            else
             begin
               aktprocsym^.definition^.concatdef(p,varspez);
               vs:=new(Pvarsym,init(s,p));
             end;
            vs^.varspez:=varspez;
          { we have to add this to avoid var param to be in registers !!!}
            if (varspez in [vs_var,vs_const]) and push_addr_param(p) then
{$ifdef INCLUDEOK}
              include(vs^.varoptions,vo_regable);
{$else}
              vs^.varoptions:=vs^.varoptions+[vo_regable];
{$endif}

            { search for duplicate ids in object members/methods    }
            { but only the current class, I don't know why ...      }
            { at least TP and Delphi do it in that way   (FK) }
            if assigned(procinfo._class) and
               (lexlevel=normal_function_level) then
             begin
               hsym:=procinfo._class^.symtable^.search(vs^.name);
               if assigned(hsym) then
                DuplicateSym(hsym);
             end;

          { do we need a local copy }
            if (varspez=vs_value) and push_addr_param(p) and
               not(is_open_array(p) or is_array_of_const(p)) then
              vs^.setname('val'+vs^.name);

          { insert the sym in the parasymtable }
            aktprocsym^.definition^.parast^.insert(vs);

          { also need to push a high value? }
            if inserthigh then
             begin
               hvs:=new(Pvarsym,init('high'+s,s32bitdef));
               hvs^.varspez:=vs_const;
               aktprocsym^.definition^.parast^.insert(hvs);
             end;

          end;
         dispose(sc,done);
         tokenpos:=storetokenpos;
      end;
    aktprocsym^.definition^.setmangledname(hs2);
  until not try_to_consume(_SEMICOLON);
  dec(testcurobject);
  consume(_RKLAMMER);
end;



procedure parse_proc_head(options:tproctypeoption);
var sp:stringid;
    pd:Pprocdef;
    paramoffset:longint;
    sym:Psym;
    hs:string;
    st : psymtable;
    overloaded_level:word;
    procstartfilepos : tfileposinfo;
begin
{ Save the position where this procedure really starts and set col to 1 which
  looks nicer }
  procstartfilepos:=aktfilepos;
  procstartfilepos.column:=1;

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
  if not(parse_only) and try_to_consume(_POINT) then
   begin
     getsym(sp,true);
     sym:=srsym;
     { qualifier is class name ? }
     if (sym^.typ<>typesym) or
        (ptypesym(sym)^.definition^.deftype<>objectdef) then
       begin
          Message(parser_e_class_id_expected);
          aktprocsym:=nil;
          consume(_ID);
       end
     else
       begin
          { used to allow private syms to be seen }
          aktobjectdef:=pobjectdef(ptypesym(sym)^.definition);
          sp:=pattern;
          realname:=orgpattern;
          consume(_ID);
          procinfo._class:=pobjectdef(ptypesym(sym)^.definition);
          aktprocsym:=pprocsym(procinfo._class^.symtable^.search(sp));
          {The procedure has been found. So it is
           a global one. Set the flags to mark this.}
          procinfo.flags:=procinfo.flags or pi_is_global;
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
               procinfo.flags:=procinfo.flags or pi_is_global;
             end;
          end;
       end;
   end;
  { problem with procedures inside methods }
{$ifndef UseNiceNames}
  if assigned(procinfo._class) then
    if (pos('_$$_',procprefix)=0) then
      hs:=procprefix+'_$$_'+procinfo._class^.objname^+'_$$_'+sp
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
           DuplicateSym(aktprocsym);
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
     { for operator we have only one definition for each overloaded
       operation }
     if (options=potype_operator) then
       begin
          { the only problem is that nextoverloaded might not be in a unit
            known for the unit itself }
          if assigned(overloaded_operators[optoken]) then
            aktprocsym^.definition:=overloaded_operators[optoken]^.definition;
       end;
     symtablestack^.insert(aktprocsym);
   end;

  st:=symtablestack;
  pd:=new(pprocdef,init);
  pd^.symtablelevel:=symtablestack^.symtablelevel;

  if assigned(procinfo._class) then
    pd^._class := procinfo._class;

  { set the options from the caller (podestructor or poconstructor) }
  pd^.proctypeoption:=options;

  { calculate the offset of the parameters }
  paramoffset:=8;

  { calculate frame pointer offset }
  if lexlevel>normal_function_level then
    begin
      procinfo.framepointer_offset:=paramoffset;
      inc(paramoffset,target_os.size_of_pointer);
      { this is needed to get correct framepointer push for local
        forward functions !! }
      pd^.parast^.symtablelevel:=lexlevel;
    end;

  if assigned (Procinfo._Class)  and
     not(Procinfo._Class^.is_class) and
     (pd^.proctypeoption in [potype_constructor,potype_destructor]) then
    inc(paramoffset,target_os.size_of_pointer);

  { self pointer offset                       }
  { self isn't pushed in nested procedure of methods }
  if assigned(procinfo._class) and (lexlevel=normal_function_level) then
    begin
      procinfo.ESI_offset:=paramoffset;
      if assigned(aktprocsym^.definition) and
         not(po_containsself in aktprocsym^.definition^.procoptions) then
        inc(paramoffset,target_os.size_of_pointer);
    end;

  { destructor flag ? }
  if assigned (Procinfo._Class) and
     procinfo._class^.is_class and
     (pd^.proctypeoption=potype_destructor) then
    inc(paramoffset,target_os.size_of_pointer);

  procinfo.call_offset:=paramoffset;

  pd^.parast^.datasize:=0;

  pd^.nextoverloaded:=aktprocsym^.definition;
  aktprocsym^.definition:=pd;
  aktprocsym^.definition^.fileinfo:=procstartfilepos;
  aktprocsym^.definition^.setmangledname(hs);
  aktprocsym^.definition^.procsym:=aktprocsym;

  { update also the current filepos for aktprocsym }
  aktprocsym^.fileinfo:=procstartfilepos;

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
    formal_parameter_list;
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
                      aktprocsym^.definition^.retdef:=single_type(hs);
                      aktprocsym^.definition^.test_if_fpu_result;
                      dec(testcurobject);
                    end;
                 end;
    _PROCEDURE : begin
                   consume(_PROCEDURE);
                   parse_proc_head(potype_none);
                   aktprocsym^.definition^.retdef:=voiddef;
                 end;
  _CONSTRUCTOR : begin
                   consume(_CONSTRUCTOR);
                   parse_proc_head(potype_constructor);
                   if assigned(procinfo._class) and
                      procinfo._class^.is_class then
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
{$else GDB}
                      aktprocsym^.definition^.retdef:=new(porddef,init(bool8bit,0,1));
{$Endif GDB}
                    end;
                 end;
   _DESTRUCTOR : begin
                   consume(_DESTRUCTOR);
                   parse_proc_head(potype_destructor);
                   aktprocsym^.definition^.retdef:=voiddef;
                 end;
     _OPERATOR : begin
                   if lexlevel>normal_function_level then
                     Message(parser_e_no_local_operator);
                   consume(_OPERATOR);
                   if not(token in [_PLUS..last_overloaded]) then
                     Message(parser_e_overload_operator_failed);
                   optoken:=token;
                   consume(Token);
                   procinfo.flags:=procinfo.flags or pi_operator;
                   parse_proc_head(potype_operator);
                   if token<>_ID then
                     begin
                        opsym:=nil;
                        if not(m_result in aktmodeswitches) then
                          consume(_ID);
                     end
                   else
                     begin
                       opsym:=new(pvarsym,init(pattern,voiddef));
                       consume(_ID);
                     end;
                   if not try_to_consume(_COLON) then
                     begin
                       consume(_COLON);
                       aktprocsym^.definition^.retdef:=generrordef;
                       consume_all_until(_SEMICOLON);
                     end
                   else
                    begin
                      aktprocsym^.definition^.retdef:=
                       single_type(hs);
                      aktprocsym^.definition^.test_if_fpu_result;
                      if (optoken in [_EQUAL,_GT,_LT,_GTE,_LTE]) and
                         ((aktprocsym^.definition^.retdef^.deftype<>
                         orddef) or (porddef(aktprocsym^.definition^.
                         retdef)^.typ<>bool8bit)) then
                        Message(parser_e_comparative_operator_return_boolean);
                       if assigned(opsym) then
                         opsym^.definition:=aktprocsym^.definition^.retdef;
                       { We need to add the retrun type in the mangledname
                         to allow overloading with just different results !! (PM) }
                       aktprocsym^.definition^.setmangledname(
                         aktprocsym^.definition^.mangledname+'$$'+hs);
                     end;
                 end;
  end;
  if isclassmethod and
     assigned(aktprocsym) then
{$ifdef INCLUDEOK}
    include(aktprocsym^.definition^.procoptions,po_classmethod);
{$else}
    aktprocsym^.definition^.procoptions:=aktprocsym^.definition^.procoptions+[po_classmethod];
{$endif}
  consume(_SEMICOLON);
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
  if assigned(procinfo._class) then
    Message(parser_e_methods_dont_be_export);
  if lexlevel<>normal_function_level then
    Message(parser_e_dont_nest_export);
  { only os/2 needs this }
  if target_info.target=target_i386_os2 then
   begin
     procnames.insert(realname);
     procinfo.exported:=true;
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
{$ifdef INCLUDEOK}
  include(aktprocsym^.symoptions,sp_forwarddef);
{$else}
  aktprocsym^.symoptions:=aktprocsym^.symoptions+[sp_forwarddef];
{$endif}
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

procedure pd_system(const procnames:Tstringcontainer);
begin
  aktprocsym^.definition^.setmangledname(realname);
end;

procedure pd_abstract(const procnames:Tstringcontainer);
begin
  if (po_virtualmethod in aktprocsym^.definition^.procoptions) then
{$ifdef INCLUDEOK}
    include(aktprocsym^.definition^.procoptions,po_abstractmethod)
{$else}
    aktprocsym^.definition^.procoptions:=aktprocsym^.definition^.procoptions+[po_abstractmethod]
{$endif}
  else
    Message(parser_e_only_virtual_methods_abstract);
  { the method is defined }
  aktprocsym^.definition^.forwarddef:=false;
end;

procedure pd_virtual(const procnames:Tstringcontainer);
begin
  if (aktprocsym^.definition^.proctypeoption=potype_constructor) and
     not(aktprocsym^.definition^._class^.is_class) then
    Message(parser_e_constructor_cannot_be_not_virtual);
end;

procedure pd_static(const procnames:Tstringcontainer);
begin
  if (cs_static_keyword in aktmoduleswitches) then
    begin
{$ifdef INCLUDEOK}
      include(aktprocsym^.symoptions,sp_static);
      include(aktprocsym^.definition^.procoptions,po_staticmethod);
{$else}
      aktprocsym^.symoptions:=aktprocsym^.symoptions+[sp_static];
      aktprocsym^.definition^.procoptions:=aktprocsym^.definition^.procoptions+[po_staticmethod];
{$endif}
    end;
end;

procedure pd_override(const procnames:Tstringcontainer);
begin
  if not(aktprocsym^.definition^._class^.is_class) then
    Message(parser_e_no_object_override);
end;

procedure pd_message(const procnames:Tstringcontainer);
var
  pt : ptree;
begin
  { check parameter type }
  if not(po_containsself in aktprocsym^.definition^.procoptions) and
     (assigned(aktprocsym^.definition^.para1^.next) or
      (aktprocsym^.definition^.para1^.paratyp<>vs_var)) then
   Message(parser_e_ill_msg_param);
  pt:=comp_expr(true);
  do_firstpass(pt);
  if pt^.treetype=stringconstn then
    begin
{$ifdef INCLUDEOK}
      include(aktprocsym^.definition^.procoptions,po_msgstr);
{$else}
      aktprocsym^.definition^.procoptions:=aktprocsym^.definition^.procoptions+[po_msgstr];
{$endif}
      aktprocsym^.definition^.messageinf.str:=strnew(pt^.value_str);
    end
  else
   if is_constintnode(pt) then
    begin
{$ifdef INCLUDEOK}
      include(aktprocsym^.definition^.procoptions,po_msgint);
{$else}
      aktprocsym^.definition^.procoptions:=aktprocsym^.definition^.procoptions+[po_msgint];
{$endif}
      aktprocsym^.definition^.messageinf.i:=pt^.value;
    end
  else
    Message(parser_e_ill_msg_expr);
  disposetree(pt);
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

{$ifdef TP}
  {$F-}
{$endif}

function parse_proc_direc(const proc_names:Tstringcontainer;var pdflags:word):boolean;
{
  Parse the procedure directive, returns true if a correct directive is found
}
const
   namelength=15;
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
  num_proc_directives=29;
  proc_direcdata:array[1..num_proc_directives] of proc_dir_rec=
   (
    (
      idtok:_ABSTRACT;
      pd_flags : pd_interface+pd_object;
      handler  : {$ifndef TP}@{$endif}pd_abstract;
      pocall   : [];
      pooption : [po_abstractmethod];
      mutexclpocall : [pocall_internproc,pocall_inline];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_exports,po_interrupt,po_external]
    ),(
      idtok:_ALIAS;
      pd_flags : pd_implemen+pd_body;
      handler  : {$ifndef TP}@{$endif}pd_alias;
      pocall   : [];
      pooption : [];
      mutexclpocall : [pocall_inline];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_ASMNAME;
      pd_flags : pd_interface+pd_implemen;
      handler  : {$ifndef TP}@{$endif}pd_asmname;
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
      handler  : {$ifndef TP}@{$endif}pd_cdecl;
      pocall   : [pocall_cdecl,pocall_clearstack];
      pooption : [po_savestdregs];
      mutexclpocall : [pocall_internproc,pocall_leftright,pocall_inline];
      mutexclpotype : [];
      mutexclpo     : [po_assembler,po_external]
    ),(
      idtok:_DYNAMIC;
      pd_flags : pd_interface+pd_object;
      handler  : {$ifndef TP}@{$endif}pd_virtual;
      pocall   : [];
      pooption : [po_virtualmethod];
      mutexclpocall : [pocall_internproc,pocall_inline];
      mutexclpotype : [];
      mutexclpo     : [po_exports,po_interrupt,po_external]
    ),(
      idtok:_EXPORT;
      pd_flags : pd_body+pd_global+pd_interface+pd_implemen{??};
      handler  : {$ifndef TP}@{$endif}pd_export;
      pocall   : [];
      pooption : [po_exports];
      mutexclpocall : [pocall_internproc,pocall_inline];
      mutexclpotype : [];
      mutexclpo     : [po_external,po_interrupt]
    ),(
      idtok:_EXTERNAL;
      pd_flags : pd_implemen+pd_interface;
      handler  : {$ifndef TP}@{$endif}pd_external;
      pocall   : [];
      pooption : [po_external];
      mutexclpocall : [pocall_internproc,pocall_inline,pocall_palmossyscall];
      mutexclpotype : [];
      mutexclpo     : [po_exports,po_interrupt,po_assembler]
    ),(
      idtok:_FAR;
      pd_flags : pd_implemen+pd_body+pd_interface+pd_procvar;
      handler  : {$ifndef TP}@{$endif}pd_far;
      pocall   : [];
      pooption : [];
      mutexclpocall : [pocall_internproc,pocall_inline];
      mutexclpotype : [];
      mutexclpo     : []
    ),(
      idtok:_FORWARD;
      pd_flags : pd_implemen;
      handler  : {$ifndef TP}@{$endif}pd_forward;
      pocall   : [];
      pooption : [];
      mutexclpocall : [pocall_internproc,pocall_inline];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_INLINE;
      pd_flags : pd_implemen+pd_body;
      handler  : {$ifndef TP}@{$endif}pd_inline;
      pocall   : [pocall_inline];
      pooption : [];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_exports,po_external,po_interrupt]
    ),(
      idtok:_INTERNCONST;
      pd_flags : pd_implemen+pd_body;
      handler  : {$ifndef TP}@{$endif}pd_intern;
      pocall   : [pocall_internconst];
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [potype_operator];
      mutexclpo     : []
    ),(
      idtok:_INTERNPROC;
      pd_flags : pd_implemen;
      handler  : {$ifndef TP}@{$endif}pd_intern;
      pocall   : [pocall_internproc];
      pooption : [];
      mutexclpocall : [pocall_inline,pocall_clearstack,pocall_leftright,pocall_cdecl];
      mutexclpotype : [potype_constructor,potype_destructor,potype_operator];
      mutexclpo     : [po_exports,po_external,po_interrupt,po_assembler,po_iocheck]
    ),(
      idtok:_INTERRUPT;
      pd_flags : pd_implemen+pd_body;
      handler  : nil;
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
      handler  : {$ifndef TP}@{$endif}pd_message;
      pocall   : [];
      pooption : []; { can be po_msgstr or po_msgint }
      mutexclpocall : [pocall_inline,pocall_internproc];
      mutexclpotype : [potype_constructor,potype_destructor,potype_operator];
      mutexclpo     : [po_interrupt,po_external]
    ),(
      idtok:_NEAR;
      pd_flags : pd_implemen+pd_body+pd_procvar;
      handler  : {$ifndef TP}@{$endif}pd_near;
      pocall   : [];
      pooption : [];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : []
    ),(
      idtok:_OVERRIDE;
      pd_flags : pd_interface+pd_object;
      handler  : {$ifndef TP}@{$endif}pd_override;
      pocall   : [];
      pooption : [po_overridingmethod,po_virtualmethod];
      mutexclpocall : [pocall_inline,pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_exports,po_external,po_interrupt]
    ),(
      idtok:_PASCAL;
      pd_flags : pd_implemen+pd_body+pd_procvar;
      handler  : nil;
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
      handler  : {$ifndef TP}@{$endif}pd_register;
      pocall   : [pocall_register];
      pooption : [];
      mutexclpocall : [pocall_leftright,pocall_cdecl,pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_SAFECALL;
      pd_flags : pd_interface+pd_implemen+pd_body+pd_procvar;
      handler  : {$ifndef TP}@{$endif}pd_safecall;
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
      handler  : {$ifndef TP}@{$endif}pd_static;
      pocall   : [];
      pooption : [po_staticmethod];
      mutexclpocall : [pocall_inline,pocall_internproc];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_external,po_interrupt,po_exports]
    ),(
      idtok:_STDCALL;
      pd_flags : pd_interface+pd_implemen+pd_body+pd_procvar;
      handler  : {$ifndef TP}@{$endif}pd_stdcall;
      pocall   : [pocall_stdcall];
      pooption : [po_savestdregs];
      mutexclpocall : [pocall_leftright,pocall_cdecl,pocall_inline,pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_SYSCALL;
      pd_flags : pd_interface;
      handler  : {$ifndef TP}@{$endif}pd_syscall;
      pocall   : [pocall_palmossyscall];
      pooption : [];
      mutexclpocall : [pocall_cdecl,pocall_inline,pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_external,po_assembler,po_interrupt,po_exports]
    ),(
      idtok:_SYSTEM;
      pd_flags : pd_implemen;
      handler  : {$ifndef TP}@{$endif}pd_system;
      pocall   : [pocall_clearstack];
      pooption : [];
      mutexclpocall : [pocall_leftright,pocall_inline,pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_external,po_assembler,po_interrupt]
    ),(
      idtok:_VIRTUAL;
      pd_flags : pd_interface+pd_object;
      handler  : {$ifndef TP}@{$endif}pd_virtual;
      pocall   : [];
      pooption : [po_virtualmethod];
      mutexclpocall : [pocall_inline,pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_external,po_interrupt,po_exports]
    )
   );

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

function check_identical(var p : pprocdef) : boolean;
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
  check_identical:=false;
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
           if not(m_repeat_forward in aktmodeswitches) or
              (equal_paras(aktprocsym^.definition^.para1,pd^.nextoverloaded^.para1,false) and
              { for operators equal_paras is not enough !! }
              ((aktprocsym^.definition^.proctypeoption<>potype_operator) or (optoken<>_ASSIGNMENT) or
               is_equal(pd^.nextoverloaded^.retdef,aktprocsym^.definition^.retdef))) then
             begin
               if pd^.nextoverloaded^.forwarddef then
               { remove the forward definition  but don't delete it,      }
               { the symtable is the owner !!  }
                 begin
                   hd:=pd^.nextoverloaded;
                 { Check if the procedure type and return type are correct }
                   if (hd^.proctypeoption<>aktprocsym^.definition^.proctypeoption) or
                      (not(is_equal(hd^.retdef,aktprocsym^.definition^.retdef)) and
                      (m_repeat_forward in aktmodeswitches)) then
                     begin
                       Message1(parser_e_header_dont_match_forward,aktprocsym^.demangledName);
                       exit;
                     end;
                 { Check calling convention, no check for internconst,internproc which
                   are only defined in interface or implementation }
                   if (hd^.proccalloptions-[pocall_internconst,pocall_internproc]<>
                       aktprocsym^.definition^.proccalloptions-[pocall_internconst,pocall_internproc]) then
                    begin
                      { only trigger an error, becuase it doesn't hurt }
                      Message(parser_e_call_convention_dont_match_forward);
                    end;
                 { manglednames are equal? }
                   hd^.count:=false;
                   if (m_repeat_forward in aktmodeswitches) or
                      aktprocsym^.definition^.haspara then
                    begin
                      if (hd^.mangledname<>aktprocsym^.definition^.mangledname) then
                       begin
                         { When overloading is not possible then we issue an error }
                         if not(m_repeat_forward in aktmodeswitches) then
                          begin
                            Message1(parser_e_header_dont_match_forward,aktprocsym^.demangledName);
                            exit;
                          end;

                         if not(po_external in aktprocsym^.definition^.procoptions) then
                           Message2(parser_n_interface_name_diff_implementation_name,hd^.mangledname,
                             aktprocsym^.definition^.mangledname);
                       { reset the mangledname of the interface part to be sure }
                       { this is wrong because the mangled name might have been used already !! }
                          if hd^.is_used then
                            renameasmsymbol(hd^.mangledname,aktprocsym^.definition^.mangledname);
                          hd^.setmangledname(aktprocsym^.definition^.mangledname);
                       { so we need to keep the name of interface !!
                         No!!!! The procedure directives can change the mangledname.
                         I fixed this by first calling check_identical and then doing
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
                             Message1(parser_e_function_already_declared_public_forward,aktprocsym^.demangledName);
                             Check_identical:=true;
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
                                     Message3(parser_e_header_different_var_names,
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
                 { also the call_offset }
                   hd^.parast^.address_fixup:=aktprocsym^.definition^.parast^.address_fixup;
                   hd^.count:=true;

                 { remove pd^.nextoverloaded from the list }
                 { and add aktprocsym^.definition }
                   pd^.nextoverloaded:=pd^.nextoverloaded^.nextoverloaded;
                   hd^.nextoverloaded:=aktprocsym^.definition^.nextoverloaded;
                 { Alert! All fields of aktprocsym^.definition that are modified
                   by the procdir handlers must be copied here!.}
                   hd^.forwarddef:=false;
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
                   check_identical:=true;
                 end
               else
               { abstract methods aren't forward defined, but this }
               { needs another error message                   }
                 if not(po_abstractmethod in pd^.nextoverloaded^.procoptions) then
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
  if ((procinfo.flags and pi_operator)<>0) and assigned(opsym)
     and not parse_only then
    begin
      if ret_in_param(aktprocsym^.definition^.retdef) then
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
   { code for the subroutine as tree }
{$ifdef newcg}
   code:pnode;
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
         _class^.symtable^.next:=symtablestack;
         symtablestack:=_class^.symtable;
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

{$ifdef newcg}
   tg.usedinproc:=[];
{$else newcg}
   { no registers are used }
   usedinproc:=0;
{$endif newcg}
   { save entry info }
   entrypos:=aktfilepos;
   entryswitches:=aktlocalswitches;
{$ifdef newcg}
   { parse the code ... }
   if (po_assembler in aktprocsym^.definition^.procoptions) then
     code:=convtree2node(assembler_block)
   else
     code:=convtree2node(block(current_module^.islibrary));
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
   tg.setfirsttemp(procinfo.firsttemp);
{$else newcg}
   setfirsttemp(procinfo.firsttemp);
{$endif newcg}

   { ... and generate assembler }
   { but set the right switches for entry !! }
   aktlocalswitches:=entryswitches;
{$ifndef NOPASS2}
{$ifdef newcg}
   tg.setfirsttemp(procinfo.firsttemp);
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
     cg^.g_entrycode(procinfo.aktentrycode,proc_names,make_global,stackframe,parasize,nostackframe,false);
{$else newcg}
   if assigned(code) then
     genentrycode(procinfo.aktentrycode,proc_names,make_global,stackframe,parasize,nostackframe,false);
{$endif newcg}

   { now generate exit code with the correct position and switches }
   aktfilepos:=exitpos;
   aktlocalswitches:=exitswitches;
   if assigned(code) then
     begin
{$ifdef newcg}
       cg^.g_exitcode(procinfo.aktexitcode,parasize,nostackframe,false);
{$else newcg}
       genexitcode(procinfo.aktexitcode,parasize,nostackframe,false);
{$endif newcg}
       procinfo.aktproccode^.insertlist(procinfo.aktentrycode);
       procinfo.aktproccode^.concatlist(procinfo.aktexitcode);
{$ifdef i386}
 {$ifndef NoOpt}
       if (cs_optimize in aktglobalswitches) and
       { do not optimize pure assembler procedures }
         ((procinfo.flags and pi_is_assembler)=0)  then
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
       if (procinfo.flags and pi_uses_asm)=0 then
         begin
            { not for unit init, becuase the var can be used in finalize,
              it will be done in proc_unit }
            if not(aktprocsym^.definition^.proctypeoption in [potype_unitinit,potype_unitfinalize]) then
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

   { only now we can remove the temps }
   resettempgen;

   { remove code tree, if not inline procedure }
   if assigned(code) and not(pocall_inline in aktprocsym^.definition^.proccalloptions) then
{$ifdef newcg}
     dispose(code,done);
{$else newcg}
     disposetree(code);
{$endif newcg}

   { remove class member symbol tables }
   while symtablestack^.symtabletype=objectsymtable do
     symtablestack:=symtablestack^.next;

   { restore filepos, the switches are already set }
   aktfilepos:=savepos;
   { free labels }
   freelabel(aktexitlabel);
   freelabel(aktexit2label);
   if (aktprocsym^.definition^.proctypeoption=potype_constructor) then
     begin
       freelabel(faillabel);
       freelabel(quickexitlabel);
     end;
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
   { A procedure directive is always followed by a semicolon }
     if res then
      consume(_SEMICOLON)
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
  { aktprocsym^.definition:=pprocdef(sym^.definition);
    this breaks the rule for TESTOBJEXT !! }
  pabstractprocdef(aktprocsym^.definition):=pabstractprocdef(sym^.definition);
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

procedure checkvaluepara(p:pnamedindexobject);{$ifndef FPC}far;{$endif}
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
           vs:=new(Pvarsym,init(s,definition));
           vs^.fileinfo:=fileinfo;
           vs^.varspez:=varspez;
           aktprocsym^.definition^.localst^.insert(vs);
           vs^.islocalcopy:=true;
           vs^.varstate:=vs_used;
           localvarsym:=vs;
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
  oldprocinfo      : tprocinfo;
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
{$ifdef INCLUDEOK}
      include(aktprocsym^.symoptions,sp_forwarddef);
{$else}
      aktprocsym^.symoptions:=aktprocsym^.symoptions+[sp_forwarddef];
{$endif}
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
      if (not current_module^.is_unit) or (cs_smartlink in aktmoduleswitches) then
       pdflags:=pdflags or pd_global;
      procinfo.exported:=false;
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
   if not check_identical(prevdef) then
     begin
     { A method must be forward defined (in the object declaration) }
       if assigned(procinfo._class) and (not assigned(oldprocinfo._class)) then
         Message(parser_e_header_dont_match_any_member);
     { Give a better error if there is a forward def in the interface and only
       a single implementation }
       if (not aktprocsym^.definition^.forwarddef) and
          assigned(aktprocsym^.definition^.nextoverloaded) and
          aktprocsym^.definition^.nextoverloaded^.forwarddef and
          aktprocsym^.definition^.nextoverloaded^.interfacedef and
          not(assigned(aktprocsym^.definition^.nextoverloaded^.nextoverloaded)) then
         Message1(parser_e_header_dont_match_forward,aktprocsym^.demangledName)
       else
        begin
        { check the global flag }
          if (procinfo.flags and pi_is_global)<>0 then
            Message(parser_e_overloaded_must_be_all_global);
        end
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
   aktprocsym^.definition^.parast^.address_fixup:=procinfo.call_offset;

   { when it is a value para and it needs a local copy then rename
     the parameter and insert a copy in the localst. This is not done
     for assembler procedures }
   if (not parse_only) and (not aktprocsym^.definition^.forwarddef) then
     aktprocsym^.definition^.parast^.foreach({$ifndef TP}@{$endif}checkvaluepara);

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

       compile_proc_body(names^,((pdflags and pd_global)<>0),assigned(oldprocinfo._class));

      { reset _FAIL as normal }
      if (aktprocsym^.definition^.proctypeoption=potype_constructor) then
        tokeninfo^[_FAIL].keyword:=m_none;
      if assigned(aktprocsym^.definition^._class) and (lexlevel=main_program_level) then
        tokeninfo^[_SELF].keyword:=m_none;
       consume(_SEMICOLON);
     end;
{ close }
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
  Revision 1.21  1999-09-15 20:35:42  florian
    * small fix to operator overloading when in MMX mode
    + the compiler uses now fldz and fld1 if possible
    + some fixes to floating point registers
    + some math. functions (arctan, ln, sin, cos, sqrt, sqr, pi) are now inlined
    * .... ???

  Revision 1.20  1999/09/10 18:48:09  florian
    * some bug fixes (e.g. must_be_valid and procinfo.funcret_is_valid)
    * most things for stored properties fixed

  Revision 1.19  1999/09/07 14:59:40  pierre
   * bugfix for 237 and 244

  Revision 1.18  1999/09/02 18:47:45  daniel
    * Could not compile with TP, some arrays moved to heap
    * NOAG386BIN default for TP
    * AG386* files were not compatible with TP, fixed.

  Revision 1.17  1999/08/30 10:17:57  peter
    * fixed crash in psub
    * ansistringcompare fixed
    * support for #$0b8

  Revision 1.16  1999/08/27 10:41:46  pierre
    * pi_is_global was missing for global methods
    + code to restore overloaded list order
      (necessary to get same CRC values after interface and after
      implementation)

  Revision 1.15  1999/08/19 13:02:11  pierre
    + label faillabel added for _FAIL support

  Revision 1.14  1999/08/10 16:24:44  pierre
   * linking to C code with cdecl;external; was broken

  Revision 1.13  1999/08/10 12:37:44  pierre
   + procsym field of tprocdef set

  Revision 1.12  1999/08/05 16:53:06  peter
    * V_Fatal=1, all other V_ are also increased
    * Check for local procedure when assigning procvar
    * fixed comment parsing because directives
    * oldtp mode directives better supported
    * added some messages to errore.msg

  Revision 1.11  1999/08/04 13:03:01  jonas
    * all tokens now start with an underscore
    * PowerPC compiles!!

  Revision 1.10  1999/08/04 00:23:20  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.9  1999/08/03 22:03:05  peter
    * moved bitmask constants to sets
    * some other type/const renamings

  Revision 1.8  1999/08/03 17:09:42  florian
    * the alpha compiler can be compiled now

  Revision 1.7  1999/08/02 21:29:01  florian
    * the main branch psub.pas is now used for
      newcg compiler

  Revision 1.6  1999/07/27 23:42:16  peter
    * indirect type referencing is now allowed

  Revision 1.5  1999/07/26 09:42:15  florian
    * bugs 494-496 fixed

  Revision 1.4  1999/07/11 20:10:24  peter
    * merged

  Revision 1.3  1999/07/02 13:02:24  peter
    * merged

  Revision 1.2  1999/06/17 13:19:56  pierre
   * merged from 0_99_12 branch

  Revision 1.1.2.4  1999/07/11 20:07:39  peter
    * message crash fixed
    * no error if self is used with non-string message

  Revision 1.1.2.3  1999/07/11 20:04:05  pierre
   * fix for problem with external without parameters in Delphi mode

  Revision 1.1.2.2  1999/07/02 12:59:52  peter
    * fixed parsing of message directive

  Revision 1.1.2.1  1999/06/17 12:44:47  pierre
    * solve problems related to assignment overloading
    * support Delphi syntax for operator
    * avoid problems if local procedure in operator

  Revision 1.1  1999/06/11 13:21:37  peter
    * reinserted

  Revision 1.153  1999/06/02 22:44:14  pierre
   * previous wrong log corrected

  Revision 1.152  1999/06/02 22:25:46  pierre
  * changed $ifdef FPC @ into $ifndef TP

  Revision 1.151  1999/06/01 22:47:06  pierre
   * problem with static keyword solved

  Revision 1.150  1999/06/01 14:45:53  peter
    * @procvar is now always needed for FPC

  Revision 1.149  1999/05/31 16:42:31  peter
    * interfacedef flag for procdef if it's defined in the interface, to
      make a difference with 'forward;' directive forwarddef. Fixes 253

  Revision 1.148  1999/05/27 19:44:52  peter
    * removed oldasm
    * plabel -> pasmlabel
    * -a switches to source writing automaticly
    * assembler readers OOPed
    * asmsymbol automaticly external
    * jumptables and other label fixes for asm readers

  Revision 1.147  1999/05/24 08:55:27  florian
    * non working safecall directiv implemented, I don't know if we
      need it

  Revision 1.146  1999/05/23 18:42:11  florian
    * better error recovering in typed constants
    * some problems with arrays of const fixed, some problems
      due my previous
       - the location type of array constructor is now LOC_MEM
       - the pushing of high fixed
       - parameter copying fixed
       - zero temp. allocation removed
    * small problem in the assembler writers fixed:
      ref to nil wasn't written correctly

  Revision 1.145  1999/05/21 13:55:09  peter
    * NEWLAB for label as symbol

  Revision 1.144  1999/05/18 14:15:55  peter
    * containsself fixes
    * checktypes()

  Revision 1.143  1999/05/17 21:57:13  florian
    * new temporary ansistring handling

  Revision 1.142  1999/05/17 15:06:38  pierre
   * fixes for object type check

  Revision 1.141  1999/05/13 21:59:39  peter
    * removed oldppu code
    * warning if objpas is loaded from uses
    * first things for new deref writing

  Revision 1.140  1999/05/12 22:36:12  florian
    * override isn't allowed in objects!

  Revision 1.139  1999/05/10 09:01:41  peter
    * small message fixes

  Revision 1.138  1999/05/09 12:46:24  peter
    + hint where a duplicate sym is already defined

  Revision 1.137  1999/05/08 19:48:45  peter
    * better error message if declaration doesn't match forward

  Revision 1.136  1999/05/08 15:26:15  peter
    * print also manglednames when changed

  Revision 1.135  1999/05/06 10:12:10  peter
    * fixed operator result offset which destroyed parast^.datasize

  Revision 1.134  1999/05/01 13:24:36  peter
    * merged nasm compiler
    * old asm moved to oldasm/

  Revision 1.133  1999/04/28 11:12:03  peter
    * fixed crash with self pointer

  Revision 1.132  1999/04/28 06:02:09  florian
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

  Revision 1.131  1999/04/26 13:31:44  peter
    * release storenumber,double_checksum

  Revision 1.130  1999/04/21 09:43:49  peter
    * storenumber works
    * fixed some typos in double_checksum
    + incompatible types type1 and type2 message (with storenumber)

  Revision 1.129  1999/04/20 14:39:07  daniel
  *** empty log message ***

  Revision 1.125  1999/04/14 09:14:55  peter
    * first things to store the symbol/def number in the ppu

  Revision 1.124  1999/04/07 15:31:13  pierre
    * all formaldefs are now a sinlge definition
      cformaldef (this was necessary for double_checksum)
    + small part of double_checksum code

  Revision 1.123  1999/04/06 11:21:58  peter
    * more use of ttoken

  Revision 1.122  1999/03/31 13:55:16  peter
    * assembler inlining working for ag386bin

  Revision 1.121  1999/03/26 00:05:39  peter
    * released valintern
    + deffile is now removed when compiling is finished
    * ^( compiles now correct
    + static directive
    * shrd fixed

  Revision 1.120  1999/03/24 23:17:18  peter
    * fixed bugs 212,222,225,227,229,231,233

  Revision 1.119  1999/03/05 09:46:18  pierre
   * public problem for methods

  Revision 1.118  1999/03/05 01:14:24  pierre
    * bug0198 : call conventions for methods
      not yet implemented is the control of same calling convention
      for virtual and child's virtual
    * msgstr and msgint only created if message was found
      who implemented this by the way ?
      it leaks lots of plabels !!!! (check with heaptrc !)

  Revision 1.117  1999/03/04 13:55:47  pierre
    * some m68k fixes (still not compilable !)
    * new(tobj) does not give warning if tobj has no VMT !

  Revision 1.116  1999/03/01 15:40:52  peter
    * external name <str> didn't concatexternal()

  Revision 1.115  1999/03/01 13:31:58  pierre
   * external used before implemented problem fixed

  Revision 1.114  1999/02/24 00:59:15  peter
    * small updates for ag386bin

  Revision 1.113  1999/02/23 18:29:21  pierre
    * win32 compilation error fix
    + some work for local browser (not cl=omplete yet)

  Revision 1.112  1999/02/22 13:07:03  pierre
    + -b and -bl options work !
    + cs_local_browser ($L+) is disabled if cs_browser ($Y+)
      is not enabled when quitting global section
    * local vars and procedures are not yet stored into PPU

  Revision 1.111  1999/02/22 02:15:33  peter
    * updates for ag386bin

  Revision 1.110  1999/02/16 12:23:19  pierre
   * nested forward procedure bug solved

  Revision 1.109  1999/02/15 10:07:06  pierre
   * memory leaks due to last commit solved

  Revision 1.108  1999/02/11 09:46:27  pierre
    * fix for normal method calls inside static methods :
      WARNING there were both parser and codegen errors !!
      added static_call boolean to calln tree

  Revision 1.107  1999/02/10 11:27:39  pierre
   * overloaded function locals problem bug0213

  Revision 1.106  1999/02/08 11:29:05  pierre
   * fix for bug0214
     several problems where combined
     search_class_member did not set srsymtable
     => in do_member_read the call node got a wrong symtable
     in cg386cal the vmt was pushed twice without chacking if it exists
     now %esi is set to zero and pushed if not vmt
     (not very efficient but should work !)

  Revision 1.105  1999/02/05 12:51:20  florian
    + openstring id is now supported

  Revision 1.104  1999/02/03 09:26:44  pierre
   + better reference for args of procs

  Revision 1.103  1999/02/02 11:04:37  florian
    * class destructors fixed, class instances weren't disposed correctly

  Revision 1.102  1999/01/21 22:10:46  peter
    * fixed array of const
    * generic platform independent high() support

  Revision 1.101  1999/01/20 14:18:38  pierre
    * bugs related to mangledname solved
      - linux external without name
      -external procs already used
      (added count and is_used boolean fiels in tprocvar)

  Revision 1.100  1999/01/20 10:20:19  peter
    * don't make localvar copies for assembler procedures

  Revision 1.99  1999/01/19 15:59:40  pierre
   * fix for function a;

  Revision 1.98  1999/01/19 12:16:07  peter
    * NOPASS2 now calls firstpass

  Revision 1.97  1999/01/14 11:35:30  daniel
  * Fixed manglednames

  Revision 1.96  1998/12/30 13:41:10  peter
    * released valuepara

  Revision 1.95  1998/12/30 10:36:39  michael
  + Delphi also allows external in interface section

  Revision 1.94  1998/12/29 18:48:26  jonas
    + optimize pascal code surrounding assembler blocks

  Revision 1.93  1998/12/28 15:44:49  peter
    + NOPASS2 define

  Revision 1.92  1998/12/11 00:03:39  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.91  1998/11/27 14:50:42  peter
    + open strings, $P switch support

  Revision 1.90  1998/11/18 17:45:27  peter
    * fixes for VALUEPARA

  Revision 1.89  1998/11/18 15:44:15  peter
    * VALUEPARA for tp7 compatible value parameters

  Revision 1.88  1998/11/16 15:40:30  pierre
   * mangling name and -So bugs solved

  Revision 1.87  1998/11/16 11:29:02  pierre
    * stackcheck removed for i386_win32
    * exportlist does not crash at least !!
      (was need for tests dir !)z

  Revision 1.86  1998/11/16 10:13:54  peter
    * label defines are checked at the end of the proc

  Revision 1.85  1998/11/13 15:40:26  pierre
    + added -Se in Makefile cvstest target
    + lexlevel cleanup
      normal_function_level main_program_level and unit_init_level defined
    * tins_cache grown to A_EMMS (gave range check error in asm readers)
      (test added in code !)
    * -Un option was wrong
    * _FAIL and _SELF only keyword inside
      constructors and methods respectively

  Revision 1.84  1998/11/10 10:09:13  peter
    * va_list -> array of const

  Revision 1.83  1998/11/09 11:44:34  peter
    + va_list for printf support

  Revision 1.82  1998/10/29 11:35:53  florian
    * some dll support for win32
    * fixed assembler writing for PalmOS

  Revision 1.81  1998/10/28 18:26:16  pierre
   * removed some erros after other errors (introduced by useexcept)
   * stabs works again correctly (for how long !)

  Revision 1.80  1998/10/27 13:45:37  pierre
    * classes get a vmt allways
    * better error info (tried to remove
      several error strings introduced by the tpexcept handling)

  Revision 1.79  1998/10/23 00:09:43  peter
    * fixed message for forward declaration

  Revision 1.78  1998/10/20 13:10:37  peter
    * fixed crash when aktprocsym<>procsym

  Revision 1.77  1998/10/20 08:06:55  pierre
    * several memory corruptions due to double freemem solved
      => never use p^.loc.location:=p^.left^.loc.location;
    + finally I added now by default
      that ra386dir translates global and unit symbols
    + added a first field in tsymtable and
      a nextsym field in tsym
      (this allows to obtain ordered type info for
      records and objects in gdb !)

  Revision 1.76  1998/10/19 08:55:02  pierre
    * wrong stabs info corrected once again !!
    + variable vmt offset with vmt field only if required
      implemented now !!!

  Revision 1.75  1998/10/16 08:51:48  peter
    + target_os.stackalignment
    + stack can be aligned at 2 or 4 byte boundaries

  Revision 1.74  1998/10/14 20:39:21  florian
    * syscall for PalmOs fixed

  Revision 1.73  1998/10/12 12:20:56  pierre
    + added tai_const_symbol_offset
      for r : pointer = @var.field;
    * better message for different arg names on implementation
      of function

  Revision 1.72  1998/10/08 23:29:03  peter
    * -vu shows unit info, -vt shows tried/used files

  Revision 1.71  1998/10/08 17:17:28  pierre
    * current_module old scanner tagged as invalid if unit is recompiled
    + added ppheap for better info on tracegetmem of heaptrc
      (adds line column and file index)
    * several memory leaks removed ith help of heaptrc !!

  Revision 1.70  1998/10/08 13:48:49  peter
    * fixed memory leaks for do nothing source
    * fixed unit interdependency

  Revision 1.69  1998/10/05 21:33:27  peter
    * fixed 161,165,166,167,168

  Revision 1.68  1998/09/29 11:31:30  florian
    * better error recovering when the object type of procedure tobject.method
      isn't found

  Revision 1.67  1998/09/26 17:45:39  peter
    + idtoken and only one token table

  Revision 1.66  1998/09/24 23:49:16  peter
    + aktmodeswitches

  Revision 1.65  1998/09/24 11:08:14  florian
    * small problem in _proc_header with array of const fixed:
      getsymonlyin doesn't set srsym to nil

  Revision 1.64  1998/09/23 15:39:12  pierre
    * browser bugfixes
      was adding a reference when looking for the symbol
      if -bSYM_NAME was used

  Revision 1.63  1998/09/22 17:13:50  pierre
    + browsing updated and developed
      records and objects fields are also stored

  Revision 1.62  1998/09/22 15:37:21  peter
    + array of const start

  Revision 1.61  1998/09/21 08:45:20  pierre
    + added vmt_offset in tobjectdef.write for fututre use
      (first steps to have objects without vmt if no virtual !!)
    + added fpu_used field for tabstractprocdef  :
      sets this level to 2 if the functions return with value in FPU
      (is then set to correct value at parsing of implementation)
      THIS MIGHT refuse some code with FPU expression too complex
      that were accepted before and even in some cases
      that don't overflow in fact
      ( like if f : float; is a forward that finally in implementation
       only uses one fpu register !!)
      Nevertheless I think that it will improve security on
      FPU operations !!
    * most other changes only for UseBrowser code
      (added symtable references for record and objects)
      local switch for refs to args and local of each function
      (static symtable still missing)
      UseBrowser still not stable and probably broken by
      the definition hash array !!

  Revision 1.60  1998/09/17 09:42:42  peter
    + pass_2 for cg386
    * Message() -> CGMessage() for pass_1/pass_2

  Revision 1.59  1998/09/15 14:05:25  jonas
    * fixed optimizer incompatibilities with freelabel code in psub

  Revision 1.58  1998/09/14 21:27:41  peter
    - freelabel calls, becuase they are instable with -O2

  Revision 1.57  1998/09/14 10:38:27  peter
    * pd_alias now uses get_stringconst

  Revision 1.56  1998/09/14 10:29:38  daniel
  * Fixed memory leaks.

  Revision 1.55  1998/09/09 11:50:56  pierre
    * forward def are not put in record or objects
    + added check for forwards also in record and objects
    * dummy parasymtable for unit initialization removed from
    symtable stack

  Revision 1.54  1998/09/04 08:42:05  peter
    * updated some error messages

  Revision 1.53  1998/09/01 17:39:51  peter
    + internal constant functions

  Revision 1.52  1998/09/01 09:07:12  peter
    * m68k fixes, splitted cg68k like cgi386

  Revision 1.51  1998/09/01 07:54:21  pierre
    * UseBrowser a little updated (might still be buggy !!)
    * bug in psub.pas in function specifier removed
    * stdcall allowed in interface and in implementation
      (FPC will not yet complain if it is missing in either part
      because stdcall is only a dummy !!)

  Revision 1.50  1998/08/31 12:26:31  peter
    * m68k and palmos updates from surebugfixes

  Revision 1.49  1998/08/25 12:42:43  pierre
    * CDECL changed to CVAR for variables
      specifications are read in structures also
    + started adding GPC compatibility mode ( option  -Sp)
    * names changed to lowercase

  Revision 1.48  1998/08/21 08:43:30  pierre
    * pocdecl and poclearstack are now different
      external must but written as last specification

  Revision 1.47  1998/08/20 09:26:44  pierre
    + funcret setting in underproc testing
      compile with _dTEST_FUNCRET

  Revision 1.46  1998/08/19 18:04:55  peter
    * fixed current_module^.in_implementation flag

  Revision 1.45  1998/08/13 10:58:38  peter
    * fixed function reading for -So which was not correct after my previous
      fix for bug 147

  Revision 1.44  1998/08/10 14:50:18  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.43  1998/08/10 09:58:33  peter
    * Fixed function b; in -So mode

  Revision 1.42  1998/07/30 16:07:11  florian
    * try ... expect <statement> end; works now

  Revision 1.41  1998/07/23 19:31:19  jonas
    * split the optimizer

  Revision 1.40  1998/07/21 11:16:24  florian
    * bug0147 fixed

  Revision 1.39  1998/07/14 21:46:54  peter
    * updated messages file

  Revision 1.38  1998/07/14 14:46:57  peter
    * released NEWINPUT

  Revision 1.37  1998/07/10 13:12:53  peter
    * carls patch

  Revision 1.36  1998/07/10 13:06:53  michael
  + Carls patch. Checked make cycle.

  Revision 1.35  1998/07/10 00:00:01  peter
    * fixed ttypesym bug finally
    * fileinfo in the symtable and better using for unused vars

  Revision 1.34  1998/07/07 11:20:05  peter
    + NEWINPUT for a better inputfile and scanner object

  Revision 1.33  1998/06/15 15:38:08  pierre
    * small bug in systems.pas corrected
    + operators in different units better hanlded

  Revision 1.32  1998/06/13 00:10:13  peter
    * working browser and newppu
    * some small fixes against crashes which occured in bp7 (but not in
      fpc?!)

  Revision 1.31  1998/06/10 17:04:05  michael
  + Fix for reading untyped const parameters

  Revision 1.30  1998/06/09 16:01:50  pierre
    + added procedure directive parsing for procvars
      (accepted are popstack cdecl and pascal)
    + added C vars with the following syntax
      var C calias 'true_c_name';(can be followed by external)
      reason is that you must add the Cprefix

      which is target dependent

  Revision 1.29  1998/06/08 22:59:51  peter
    * smartlinking works for win32
    * some defines to exclude some compiler parts

  Revision 1.28  1998/06/08 13:13:45  pierre
    + temporary variables now in temp_gen.pas unit
      because it is processor independent
    * mppc68k.bat modified to undefine i386 and support_mmx
      (which are defaults for i386)

  Revision 1.27  1998/06/05 17:47:30  peter
    * some better uses clauses

  Revision 1.26  1998/06/05 14:37:36  pierre
    * fixes for inline for operators
    * inline procedure more correctly restricted

  Revision 1.25  1998/06/04 23:51:54  peter
    * m68k compiles
    + .def file creation moved to gendef.pas so it could also be used
      for win32

  Revision 1.24  1998/06/04 09:55:44  pierre
    * demangled name of procsym reworked to become independant of the mangling scheme

  Revision 1.23  1998/05/28 17:26:51  peter
    * fixed -R switch, it didn't work after my previous akt/init patch
    * fixed bugs 110,130,136

  Revision 1.22  1998/05/28 14:40:27  peter
    * fixes for newppu, remake3 works now with it

  Revision 1.21  1998/05/23 01:21:25  peter
    + aktasmmode, aktoptprocessor, aktoutputformat
    + smartlink per module $SMARTLINK-/+ (like MMX) and moved to aktswitches
    + $LIBNAME to set the library name where the unit will be put in
    * splitted cgi386 a bit (codeseg to large for bp7)
    * nasm, tasm works again. nasm moved to ag386nsm.pas

  Revision 1.20  1998/05/21 19:33:34  peter
    + better procedure directive handling and only one table

  Revision 1.19  1998/05/20 09:42:36  pierre
    + UseTokenInfo now default
    * unit in interface uses and implementation uses gives error now
    * only one error for unknown symbol (uses lastsymknown boolean)
      the problem came from the label code !
    + first inlined procedures and function work
      (warning there might be allowed cases were the result is still wrong !!)
    * UseBrower updated gives a global list of all position of all used symbols
      with switch -gb

  Revision 1.18  1998/05/11 13:07:56  peter
    + $ifdef NEWPPU for the new ppuformat
    + $define GDB not longer required
    * removed all warnings and stripped some log comments
    * no findfirst/findnext anymore to remove smartlink *.o files

  Revision 1.17  1998/05/06 18:36:54  peter
    * tai_section extended with code,data,bss sections and enumerated type
    * ident 'compiled by FPC' moved to pmodules
    * small fix for smartlink

  Revision 1.16  1998/05/06 08:38:47  pierre
    * better position info with UseTokenInfo
      UseTokenInfo greatly simplified
    + added check for changed tree after first time firstpass
      (if we could remove all the cases were it happen
      we could skip all firstpass if firstpasscount > 1)
      Only with ExtDebug

  Revision 1.15  1998/05/04 17:54:28  peter
    + smartlinking works (only case jumptable left todo)
    * redesign of systems.pas to support assemblers and linkers
    + Unitname is now also in the PPU-file, increased version to 14

  Revision 1.14  1998/05/01 09:01:24  florian
    + correct semantics of private and protected
    * small fix in variable scope:
       a id can be used in a parameter list of a method, even it is used in
       an anchestor class as field id

  Revision 1.13  1998/04/30 15:59:42  pierre
    * GDB works again better :
      correct type info in one pass
    + UseTokenInfo for better source position
    * fixed one remaining bug in scanner for line counts
    * several little fixes

  Revision 1.12  1998/04/29 10:34:00  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.11  1998/04/27 23:10:28  peter
    + new scanner
    * $makelib -> if smartlink
    * small filename fixes pmodule.setfilename
    * moved import from files.pas -> import.pas

  Revision 1.10  1998/04/21 10:16:48  peter
    * patches from strasbourg
    * objects is not used anymore in the fpc compiled version

  Revision 1.9  1998/04/13 22:20:36  florian
    + stricter checking for duplicate id, solves also bug0097

  Revision 1.8  1998/04/13 21:15:42  florian
    * error handling of pass_1 and cgi386 fixed
    * the following bugs fixed: 0117, 0118, 0119 and 0129, 0122 was already
      fixed, verified

  Revision 1.7  1998/04/13 08:42:52  florian
    * call by reference and call by value open arrays fixed

  Revision 1.6  1998/04/10 15:39:48  florian
    * more fixes to get classes.pas compiled

  Revision 1.5  1998/04/10 14:41:43  peter
    * removed some Hints
    * small speed optimization for AsmLn

  Revision 1.4  1998/04/08 16:58:05  pierre
    * several bugfixes
      ADD ADC and AND are also sign extended
      nasm output OK (program still crashes at end
      and creates wrong assembler files !!)
      procsym types sym in tdef removed !!
}
