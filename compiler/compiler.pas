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

{$ifdef TP}
  {$ifdef DPMI}
    {$undef USEEXCEPT}
  {$endif}
{$endif}

uses
{$ifdef fpc}
  {$ifdef GO32V2}
    emu387,
    dpmiexcp,
  {$endif GO32V2}
  {$ifdef LINUX}
    catch,
  {$endif LINUX}
{$endif}
{$ifdef USEEXCEPT}
  tpexcept,
{$endif USEEXCEPT}
{$ifdef BrowserLog}
  browlog,
{$endif BrowserLog}
{$ifdef BrowserCol}
  browcol,
{$endif BrowserCol}
  dos,verbose,comphook,systems,
  globals,options,parser,symtable,link,import,export;

function Compile(const cmd:string):longint;

implementation


var
  CompilerInitedAfterArgs,
  CompilerInited : boolean;

{$ifdef USEEXCEPT}

procedure RecoverStop;{$ifndef FPC}far;{$endif}
begin
  if recoverpospointer<>nil then
    LongJmp(recoverpospointer^,1)
  else
    Halt(1);
end;
{$endif USEEXCEPT}


{****************************************************************************
                                Compiler
****************************************************************************}

procedure DoneCompiler;
begin
  if not CompilerInited then
   exit;
{ Free compiler if args are read }
  if CompilerInitedAfterArgs then
   begin
     CompilerInitedAfterArgs:=false;
     doneparser;
     DoneImport;
     DoneExport;
   end;
{ Free memory for the others }
  CompilerInited:=false;
  DoneSymtable;
  DoneGlobals;
  linker.done;
{$ifdef BrowserLog}
  DoneBrowserLog;
{$endif BrowserLog}
{$ifdef BrowserCol}
  DoneBrowserCol;
{$endif BrowserCol}
{$ifdef USEEXCEPT}
  recoverpospointer:=nil;
  longjump_used:=false;
{$endif USEEXCEPT}
end;


procedure InitCompiler(const cmd:string);
begin
  if CompilerInited then
   DoneCompiler;
{ inits which need to be done before the arguments are parsed }
  InitSystems;
  InitVerbose;
{$ifdef BrowserLog}
  InitBrowserLog;
{$endif BrowserLog}
{$ifdef BrowserCol}
  InitBrowserCol;
{$endif BrowserCol}
  InitGlobals;
  InitSymtable;
  linker.init;
  CompilerInited:=true;
{ read the arguments }
  read_arguments(cmd);
{ inits which depend on arguments }
  initparser;
  InitImport;
  InitExport;
  CompilerInitedAfterArgs:=true;
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
  recoverpos : jmp_buf;
  olddo_stop : tstopprocedure;
{$endif}
{$IfDef Extdebug}
{$ifdef FPC}
  EntryMemUsed : longint;
{$endif FPC}
{$EndIf}
begin
{$ifdef EXTDEBUG}
{$ifdef FPC}
  EntryMemUsed:=system.HeapSize-MemAvail;
{$endif FPC}
{$endif}

{ Initialize the compiler }
  InitCompiler(cmd);

{ show some info }
  Message1(general_t_compilername,FixFileName(paramstr(0)));
  Message1(general_d_sourceos,source_os.name);
  Message1(general_i_targetos,target_os.name);
  Message1(general_t_exepath,exepath);
  Message1(general_t_unitpath,unitsearchpath);
  Message1(general_t_includepath,includesearchpath);
  Message1(general_t_librarypath,Linker.librarysearchpath);
  Message1(general_t_objectpath,objectsearchpath);
{$ifdef TP}
  Comment(V_Info,'Memory: '+tostr(MemAvail)+' Bytes Free');
{$endif}

{$ifdef USEEXCEPT}
  if setjmp(recoverpos)=0 then
   begin
     olddo_stop:=do_stop;
     recoverpospointer:=@recoverpos;
     do_stop:=recoverstop;
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
   end
  else
    DoneCompiler;
{ Stop is always called, so we come here when a program is compiled or not }
  do_stop:=olddo_stop;
{$endif USEEXCEPT}
{$ifdef EXTDEBUG}
{$ifdef FPC}
  Comment(V_Info,'Memory Lost = '+tostr(system.HeapSize-MemAvail+EntryMemUsed));
{$endif FPC}
  Comment(V_Info,'Repetitive firstpass = '+tostr(firstpass_several)+'/'+tostr(total_of_firstpass));
{$endif EXTDEBUG}

{ Set the return value if an error has occurred }
  if status.errorcount=0 then
   Compile:=0
  else
   Compile:=1;

{ no message possible after this !! }
  DoneVerbose;
end;


end.
{
  $Log$
  Revision 1.19  1999-03-09 11:52:06  pierre
   * compilation after a failure longjumped directly to end

  Revision 1.18  1999/02/26 00:48:16  peter
    * assembler writers fixed for ag386bin

  Revision 1.17  1999/01/12 14:25:25  peter
    + BrowserLog for browser.log generation
    + BrowserCol for browser info in TCollections
    * released all other UseBrowser

  Revision 1.16  1998/12/15 10:23:23  peter
    + -iSO, -iSP, -iTO, -iTP

  Revision 1.15  1998/10/29 11:35:40  florian
    * some dll support for win32
    * fixed assembler writing for PalmOS

  Revision 1.14  1998/10/26 22:58:17  florian
    * new introduded problem with classes fix, the parent class wasn't set
      correct, if the class was defined forward before

  Revision 1.13  1998/10/26 17:15:17  pierre
    + added two level of longjump to
      allow clean freeing of used memory on errors

  Revision 1.12  1998/10/09 16:36:02  pierre
    * some memory leaks specific to usebrowser define fixed
    * removed tmodule.implsymtable (was like tmodule.localsymtable)

  Revision 1.11  1998/10/08 23:28:51  peter
    * -vu shows unit info, -vt shows tried/used files

  Revision 1.10  1998/10/08 17:17:18  pierre
    * current_module old scanner tagged as invalid if unit is recompiled
    + added ppheap for better info on tracegetmem of heaptrc
      (adds line column and file index)
    * several memory leaks removed ith help of heaptrc !!

  Revision 1.9  1998/10/06 17:16:46  pierre
    * some memory leaks fixed (thanks to Peter for heaptrc !)

  Revision 1.8  1998/09/01 09:00:27  peter
    - removed tempheap creation/restore

  Revision 1.7  1998/09/01 07:54:17  pierre
    * UseBrowser a little updated (might still be buggy !!)
    * bug in psub.pas in function specifier removed
    * stdcall allowed in interface and in implementation
      (FPC will not yet complain if it is missing in either part
      because stdcall is only a dummy !!)

  Revision 1.6  1998/08/29 13:51:10  peter
    * moved get_exepath to globals
    + date_string const with the current date for 0.99.7+

  Revision 1.5  1998/08/11 21:38:24  peter
    + createheap/restoreheap procedures (only tp7 rm currently) and support
      for tp7 dpmi

  Revision 1.4  1998/08/11 14:09:06  peter
    * fixed some messages and smaller msgtxt.inc

  Revision 1.3  1998/08/11 00:01:20  peter
    * -vu displays now all searchpaths

  Revision 1.2  1998/08/10 14:49:56  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.1  1998/08/10 10:18:24  peter
    + Compiler,Comphook unit which are the new interface units to the
      compiler

}
