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
{$ifndef NOUSEEXCEPT}
{$ifdef i386}
  {$define USEEXCEPT}
{$endif}

{$ifdef TP}
  {$ifdef DPMI}
    {$undef USEEXCEPT}
  {$endif}
{$endif}
{$endif ndef NOUSEEXCEPT}

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

{$ifdef EXTDEBUG}
{$ifdef FPC}
  Var
    LostMemory : longint;
  Procedure CheckMemory(LostMemory : longint);
  begin
    if LostMemory<>0 then
      begin
        Writeln('Memory Lost = '+tostr(LostMemory));
{$ifdef DEBUG}
        def_gdb_stop(V_Warning);
{$endif DEBUG}
      end;
  end;
{$endif FPC}
{$endif EXTDEBUG}
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
  do_doneSymbolInfo;
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
  do_initSymbolInfo;
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

  procedure writepathlist(w:longint;l:TSearchPathList);
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
  LostMemory:=system.HeapSize-MemAvail-EntryMemUsed;
  CheckMemory(LostMemory);
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
  Revision 1.2  2000-07-13 11:32:38  michael
  + removed logs

}
