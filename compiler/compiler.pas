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
   { One of Alpha, I386 or M68K must be defined }
   {$UNDEF CPUOK}

   {$ifdef I386}
   {$define CPUOK}
   {$endif}

   {$ifdef M68K}
   {$ifndef CPUOK}
   {$DEFINE CPUOK}
   {$else}
     {$fatal cannot define two CPU switches}
   {$endif}
   {$endif}

   {$ifdef alpha}
   {$ifndef CPUOK}
   {$DEFINE CPUOK}
   {$else}
     {$fatal cannot define two CPU switches}
   {$endif}
   {$endif}

   {$ifdef powerpc}
   {$ifndef CPUOK}
   {$DEFINE CPUOK}
   {$else}
     {$fatal cannot define two CPU switches}
   {$endif}
   {$endif}

   {$ifndef CPUOK}
   {$fatal One of the switches I386, Alpha, PowerPC or M68K must be defined}
   {$endif}

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
{    dpmiexcp, }
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
{$ifdef Delphi}
  dmisc,
{$else Delphi}
  dos,
{$endif Delphi}
  verbose,comphook,systems,
  globals,options,parser,symtable,link,import,export,tokens;

function Compile(const cmd:string):longint;

implementation

uses
  cpubase;

var
  CompilerInitedAfterArgs,
  CompilerInited : boolean;

{$ifdef USEEXCEPT}

procedure RecoverStop;{$ifndef FPC}far;{$endif}
begin
  if recoverpospointer<>nil then
    LongJmp(recoverpospointer^,1)
  else
    Do_Halt(1);
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
{$ifdef BrowserLog}
  DoneBrowserLog;
{$endif BrowserLog}
{$ifdef BrowserCol}
  DoneBrowserCol;
{$endif BrowserCol}
  if CompilerInitedAfterArgs then
   begin
     CompilerInitedAfterArgs:=false;
     doneparser;
     DoneImport;
     DoneExport;
     DoneLinker;
     DoneCpu;
   end;
{ Free memory for the others }
  CompilerInited:=false;
  DoneSymtable;
  DoneGlobals;
  donetokens;
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
  inittokens;
  InitSymtable;
  CompilerInited:=true;
{ read the arguments }
  read_arguments(cmd);
{ inits which depend on arguments }
  initparser;
  InitImport;
  InitExport;
  InitLinker;
  InitCpu;
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
begin

{ Initialize the compiler }
  InitCompiler(cmd);

{ show some info }
  Message1(general_t_compilername,FixFileName(paramstr(0)));
  Message1(general_d_sourceos,source_os.name);
  Message1(general_i_targetos,target_os.name);
  Message1(general_t_exepath,exepath);
  Message1(general_t_unitpath,unitsearchpath);
  Message1(general_t_includepath,includesearchpath);
  Message1(general_t_librarypath,librarysearchpath);
  Message1(general_t_objectpath,objectsearchpath);
{$ifdef TP}
{$ifndef Delphi}
  Comment(V_Info,'Memory: '+tostr(MemAvail)+' Bytes Free');
{$endif Delphi}
{$endif}

{$ifdef USEEXCEPT}
  if setjmp(recoverpos)=0 then
   begin
     olddo_stop:=do_stop;
     recoverpospointer:=@recoverpos;
{$ifdef TP}
     do_stop:=recoverstop;
{$else TP}
     do_stop:=@recoverstop;
{$endif TP}
{$endif USEEXCEPT}
     starttime:=getrealtime;
     parser.compile(inputdir+inputfile+inputextension,false);
     if status.errorcount=0 then
      begin
        starttime:=getrealtime-starttime;
        if starttime<0 then
          starttime:=starttime+3600.0*24.0;
        Message2(general_i_abslines_compiled,tostr(status.compiledlines),tostr(trunc(starttime))+
          '.'+tostr(trunc(frac(starttime)*10)));
      end;
{$ifdef USEEXCEPT}
    end;
{ Stop is always called, so we come here when a program is compiled or not }
  do_stop:=olddo_stop;
{$endif USEEXCEPT}

{ Stop the compiler, frees also memory }
{ no message possible after this !!    }
  DoneCompiler;

{ Set the return value if an error has occurred }
  if status.errorcount=0 then
   Compile:=0
  else
   Compile:=1;

  DoneVerbose;
{$ifdef EXTDEBUG}
{$ifdef FPC}
  Writeln('Memory Lost = '+tostr(system.HeapSize-MemAvail-EntryMemUsed));
{$endif FPC}
{$ifndef newcg}
  Writeln('Repetitive firstpass = '+tostr(firstpass_several)+'/'+tostr(total_of_firstpass));
{$endif newcg}
{$endif EXTDEBUG}

end;


end.
{
  $Log$
  Revision 1.36  1999-10-12 21:20:41  florian
    * new codegenerator compiles again

  Revision 1.35  1999/09/28 19:48:45  florian
    * bug 617 fixed

  Revision 1.34  1999/09/16 23:05:52  florian
    * m68k compiler is again compilable (only gas writer, no assembler reader)

  Revision 1.33  1999/09/07 15:10:04  pierre
   * use do_halt instead of halt

  Revision 1.32  1999/09/02 18:47:44  daniel
    * Could not compile with TP, some arrays moved to heap
    * NOAG386BIN default for TP
    * AG386* files were not compatible with TP, fixed.

  Revision 1.31  1999/08/20 10:17:01  michael
  + Patch from pierre

  Revision 1.30  1999/08/11 17:26:31  peter
    * tlinker object is now inherited for win32 and dos
    * postprocessexecutable is now a method of tlinker

  Revision 1.29  1999/08/09 22:13:43  peter
    * fixed writing of lost memory which should be after donecompiler

  Revision 1.28  1999/08/04 13:02:40  jonas
    * all tokens now start with an underscore
    * PowerPC compiles!!

  Revision 1.27  1999/08/02 21:28:56  florian
    * the main branch psub.pas is now used for
      newcg compiler

  Revision 1.26  1999/08/02 20:46:57  michael
  * Alpha aware switch detection

  Revision 1.25  1999/07/18 14:47:22  florian
    * bug 487 fixed, (inc(<property>) isn't allowed)
    * more fixes to compile with Delphi

  Revision 1.24  1999/07/18 10:19:48  florian
    * made it compilable with Dlephi 4 again
    + fixed problem with large stack allocations on win32

  Revision 1.23  1999/06/22 16:24:41  pierre
   * local browser stuff corrected

  Revision 1.22  1999/05/17 14:24:32  pierre
   * DoneCompiler called later to prevent accessing invalid data

  Revision 1.21  1999/05/04 21:44:39  florian
    * changes to compile it with Delphi 4.0

  Revision 1.20  1999/04/21 09:43:33  peter
    * storenumber works
    * fixed some typos in double_checksum
    + incompatible types type1 and type2 message (with storenumber)

  Revision 1.19  1999/03/09 11:52:06  pierre
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
