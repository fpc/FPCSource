{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

    This unit is the interface of the compiler which can be used by
     external programs to link in the compiler

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

 ****************************************************************************}

{
  possible compiler switches:
  -----------------------------------------------------------------
  TP                  to compile the compiler with Turbo or Borland Pascal
  I386                generate a compiler for the Intel i386+
  M68K                generate a compiler for the M68000
  GDB                 support of the GNU Debugger
  EXTDEBUG            some extra debug code is executed
  SUPPORT_MMX         only i386: releases the compiler switch
                      MMX which allows the compiler to generate
                      MMX instructions
  EXTERN_MSG          Don't compile the msgfiles in the compiler, always
                      use external messagefiles
  NOAG386INT          no Intel Assembler output
  NOAG386NSM          no NASM output
  -----------------------------------------------------------------
}

{$ifdef FPC}
   { but I386 or M68K must be defined }
   { and only one of the two }
   {$ifndef I386}
      {$ifndef M68K}
        {$fatal One of the switches I386 or M68K must be defined}
      {$endif M68K}
   {$endif I386}
   {$ifdef I386}
      {$ifdef M68K}
        {$fatal ONLY one of the switches I386 or M68K must be defined}
      {$endif M68K}
   {$endif I386}
   {$ifdef support_mmx}
     {$ifndef i386}
       {$fatal I386 switch must be on for MMX support}
     {$endif i386}
   {$endif support_mmx}
{$endif}

unit compiler;
interface

{ Use exception catching so the compiler goes futher after a Stop }
{$ifdef i386}
  {$define USEEXCEPT}
{$endif}


uses
{$ifdef fpc}
  {$ifdef GO32V2}
    emu387,
    dpmiexcp,
  {$endif GO32V2}
  {$ifdef LINUX}
{    catch, }
  {$endif LINUX}
{$endif}
{$ifdef USEEXCEPT}
  tpexcept,
{$endif USEEXCEPT}
  dos,verbose,comphook,systems,
  globals,options,parser,symtable,link,import;

function Compile(const cmd:string):longint;


implementation


var
  CompilerInited : boolean;
  recoverpos : jmp_buf;

{$ifdef USEEXCEPT}
procedure RecoverStop;{$ifndef FPC}far;{$endif}
begin
  LongJmp(recoverpos,1);
end;
{$endif USEEXCEPT}


procedure DoneCompiler;
begin
  if not CompilerInited then
   exit;
{ Free memory }
  DoneSymtable;
  CompilerInited:=false;
end;


procedure InitCompiler(const cmd:string);
begin
  if CompilerInited then
   DoneCompiler;
{ inits which need to be done before the arguments are parsed }
  get_exepath;
  InitVerbose;
  InitGlobals;
  InitSymtable;
  linker.init;
{ read the arguments }
  read_arguments(cmd);
{ inits which depend on arguments }
  initparser;
  initimport;
  CompilerInited:=true;
end;


function Compile(const cmd:string):longint;

  function getrealtime : real;
  var
    h,m,s,s100 : word;
  begin
    gettime(h,m,s,s100);
    getrealtime:=h*3600.0+m*60.0+s+s100/100.0;
  end;

var
  starttime  : real;
{$ifdef USEEXCEPT}  
  olddo_stop : tstopprocedure;
{$endif}  
{$ifdef TP}
  oldfreelist,
  oldheapptr,
  oldheaporg : pointer;
{$endif}
{$IfDef Extdebug}
  EntryMemAvail : longint;
{$EndIf}
begin
{$Ifdef TP}
{ Save old heap }
  oldfreelist:=freelist;
  oldheapptr:=heapptr;
  oldheaporg:=heaporg;
{ Create a new heap }
  heaporg:=oldheapptr;
  heapptr:=heaporg;
  freelist:=heaporg;
{$endif}
{$ifdef EXTDEBUG}
  EntryMemAvail:=MemAvail;
{$endif}

{ Initialize the compiler }
  InitCompiler(cmd);

{ show some info }
  Message1(general_i_compilername,FixFileName(paramstr(0)));
  Message1(general_d_sourceos,source_os.name);
  Message1(general_i_targetos,target_os.name);
  Message1(general_u_exepath,exepath);
  Message1(general_u_unitpath,unitsearchpath);
  Message1(general_u_includepath,includesearchpath);
  Message1(general_u_librarypath,Linker.librarysearchpath);
  Message1(general_u_objectpath,objectsearchpath);
{$ifdef TP}
  Comment(V_Info,'Memory: '+tostr(MemAvail)+' Bytes Free');
{$endif}

{$ifdef USEEXCEPT}
  olddo_stop:=do_stop;
  do_stop:=recoverstop;
  if setjmp(recoverpos)=0 then
   begin
{$endif USEEXCEPT}   
     starttime:=getrealtime;
     parser.compile(inputdir+inputfile+inputextension,false);
     if status.errorcount=0 then
      begin
        starttime:=getrealtime-starttime;
        Message2(general_i_abslines_compiled,tostr(status.compiledlines),tostr(trunc(starttime))+
          '.'+tostr(trunc(frac(starttime)*10)));
      end;
   { Stop the compiler, frees also memory }
     DoneCompiler;
{$ifdef USEEXCEPT}
   end;
{ Stop is always called, so we come here when a program is compiled or not }
  do_stop:=olddo_stop;
{$endif USEEXCEPT}
{$ifdef EXTDEBUG}
  Comment(V_Info,'Memory Lost = '+tostr(EntryMemAvail-MemAvail));
{$endif EXTDEBUG}
{$Ifdef TP}
{ Restore old heap }
  freelist:=oldfreelist;
  heapptr:=oldheapptr;
  heaporg:=oldheaporg;
{$endIf TP}
{ Set the return value if an error has occurred }
  if status.errorcount=0 then
   Compile:=0
  else
   Compile:=1;
end;


end.
{
  $Log$
  Revision 1.3  1998-08-11 00:01:20  peter
    * -vu displays now all searchpaths

  Revision 1.2  1998/08/10 14:49:56  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.1  1998/08/10 10:18:24  peter
    + Compiler,Comphook unit which are the new interface units to the
      compiler

}
