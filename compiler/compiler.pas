{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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
  cobjects,globals,options,parser,symtable,link,import,export,tokens;

function Compile(const cmd:string):longint;

Const
       { do we need to link }
       IsExe : boolean = false;

implementation

uses
  cpubase;

var
  CompilerInitedAfterArgs,
  CompilerInited : boolean;
  olddo_stop : tstopprocedure;

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

procedure minimal_stop;{$ifndef fpc}far;{$endif}
begin
  DoneCompiler;
  olddo_stop;
end;


function Compile(const cmd:string):longint;

{$ifdef fpc}
{$maxfpuregisters 0}
{$endif fpc}

  procedure writepathlist(w:tmsgconst;l:TSearchPathList);
  var
    hp : pstringqueueitem;
  begin
    hp:=l.first;
    while assigned(hp) do
     begin
       Message1(w,hp^.data^);
       hp:=hp^.next;
     end;
  end;

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
{$endif}
begin

  olddo_stop:=do_stop;
{$ifdef TP}
  do_stop:=minimal_stop;
{$else TP}
  do_stop:=@minimal_stop;
{$endif TP}
{ Initialize the compiler }
  InitCompiler(cmd);

{ show some info }
  Message1(general_t_compilername,FixFileName(paramstr(0)));
  Message1(general_d_sourceos,source_os.name);
  Message1(general_i_targetos,target_os.name);
  Message1(general_t_exepath,exepath);
  WritePathList(general_t_unitpath,unitsearchpath);
  WritePathList(general_t_includepath,includesearchpath);
  WritePathList(general_t_librarypath,librarysearchpath);
  WritePathList(general_t_objectpath,objectsearchpath);
{$ifdef TP}
{$ifndef Delphi}
  Comment(V_Info,'Memory: '+tostr(MemAvail)+' Bytes Free');
{$endif Delphi}
{$endif}

{$ifdef USEEXCEPT}
  if setjmp(recoverpos)=0 then
   begin
     recoverpospointer:=@recoverpos;
{$ifdef TP}
     do_stop:=recoverstop;
{$else TP}
     do_stop:=@recoverstop;
{$endif TP}
{$endif USEEXCEPT}
     starttime:=getrealtime;
     if parapreprocess then
      parser.preprocess(inputdir+inputfile+inputextension)
     else
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
{$endif USEEXCEPT}

{ Stop is always called, so we come here when a program is compiled or not }
  do_stop:=olddo_stop;
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
{$ifdef fixLeaksOnError}
 {$ifdef tp}
  do_stop;
 {$else tp}
  do_stop();
 {$endif tp}
{$endif fixLeaksOnError}
end;


end.
{
  $Log$
  Revision 1.47  2000-03-18 15:05:33  jonas
    + added $maxfpuregisters 0 for compile() procedure

  Revision 1.46  2000/02/09 13:22:50  peter
    * log truncated

  Revision 1.45  2000/01/11 17:16:04  jonas
    * removed a lot of memory leaks when an error is encountered (caused by
      procinfo and pstringcontainers). There are still plenty left though :)

  Revision 1.44  2000/01/11 16:56:22  jonas
    - removed call to do_stop at the end of compile() since it obviously breaks the
      automatic compiling of units. Make cycle worked though! 8)

  Revision 1.43  2000/01/11 16:53:24  jonas
    + call do_stop at the end of compile()

  Revision 1.42  2000/01/07 01:14:23  peter
    * updated copyright to 2000

  Revision 1.41  1999/12/02 17:34:34  peter
    * preprocessor support. But it fails on the caret in type blocks

  Revision 1.40  1999/11/18 13:43:48  pierre
   + IsExe global var needed for IDE

  Revision 1.39  1999/11/12 11:03:50  peter
    * searchpaths changed to stringqueue object

  Revision 1.38  1999/11/09 23:47:53  pierre
   + minimal_stop to avoid memory loss with -iTO switch

  Revision 1.37  1999/11/06 14:34:20  peter
    * truncated log to 20 revs

  Revision 1.36  1999/10/12 21:20:41  florian
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

}
