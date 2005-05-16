{
    $Id: compiler.pas,v 1.61 2005/05/06 18:54:26 florian Exp $
    Copyright (c) 1998-2002 by Florian Klaempfl

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

unit compiler;

{$i fpcdefs.inc}

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

   {$ifdef vis}
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

   {$ifdef ia64}
   {$ifndef CPUOK}
   {$DEFINE CPUOK}
   {$else}
     {$fatal cannot define two CPU switches}
   {$endif}
   {$endif}

   {$ifdef SPARC}
   {$ifndef CPUOK}
   {$DEFINE CPUOK}
   {$else}
     {$fatal cannot define two CPU switches}
   {$endif}
   {$endif}

   {$ifdef x86_64}
   {$ifndef CPUOK}
   {$DEFINE CPUOK}
   {$else}
     {$fatal cannot define two CPU switches}
   {$endif}
   {$endif}

   {$ifdef ARM}
   {$ifndef CPUOK}
   {$DEFINE CPUOK}
   {$else}
     {$fatal cannot define two CPU switches}
   {$endif ARM}
   {$endif ARM}


   {$ifdef MIPS}
   {$ifndef CPUOK}
   {$DEFINE CPUOK}
   {$else}
     {$fatal cannot define two CPU switches}
   {$endif MIPS}
   {$endif MIPS}

   {$ifndef CPUOK}
   {$fatal One of the switches I386, iA64, Alpha, PowerPC or M68K must be defined}
   {$endif}

   {$ifdef support_mmx}
     {$ifndef i386}
       {$fatal I386 switch must be on for MMX support}
     {$endif i386}
   {$endif support_mmx}
{$endif}

interface

uses
{$ifdef fpc}
  {$ifdef GO32V2}
    emu387,
  {$endif GO32V2}
  {$ifdef WATCOM} // wiktor: pewnie nie potrzeba
    emu387,
{    dpmiexcp, }
  {$endif WATCOM}
{$endif}
{$ifdef BrowserLog}
  browlog,
{$endif BrowserLog}
{$IFDEF USE_SYSUTILS}
{$ELSE USE_SYSUTILS}
  dos,
{$ENDIF USE_SYSUTILS}
{$IFNDEF MACOS_USE_FAKE_SYSUTILS}
  sysutils,
{$ENDIF MACOS_USE_FAKE_SYSUTILS}
  verbose,comphook,systems,
  cutils,cclasses,globals,options,fmodule,parser,symtable,
  assemble,link,import,export,tokens,pass_1
  { cpu overrides }
  ,cpuswtch
  { cpu codegenerator }
  ,cgcpu
{$ifndef NOPASS2}
  ,cpunode
{$endif}
  { cpu targets }
  ,cputarg
  { cpu parameter handling }
  ,cpupara
  { procinfo stuff }
  ,cpupi
  { system information for source system }
  { the information about the target os  }
  { are pulled in by the t_* units       }
{$ifdef amiga}
  ,i_amiga
{$endif amiga}
{$ifdef atari}
  ,i_atari
{$endif atari}
{$ifdef beos}
  ,i_beos
{$endif beos}
{$ifdef fbds}
  ,i_fbsd
{$endif fbds}
{$ifdef go32v2}
  ,i_go32v2
{$endif go32v2}
{$ifdef linux}
  ,i_linux
{$endif linux}
{$ifdef macos}
  ,i_macos
{$endif macos}
{$ifdef nwm}
  ,i_nwm
{$endif nwm}
{$ifdef nwl}
  ,i_nwl
{$endif nwm}
{$ifdef os2}
 {$ifdef emx}
  ,i_emx
 {$else emx}
  ,i_os2
 {$endif emx}
{$endif os2}
{$ifdef palmos}
  ,i_palmos
{$endif palmos}
{$ifdef solaris}
  ,i_sunos
{$endif solaris}
{$ifdef wdosx}
  ,i_wdosx
{$endif wdosx}
{$ifdef win32}
  ,i_win32
{$endif win32}
  { assembler readers }
{$ifdef i386}
  {$ifndef NoRa386Int}
       ,ra386int
  {$endif NoRa386Int}
  {$ifndef NoRa386Att}
       ,ra386att
  {$endif NoRa386Att}
{$else}
  ,rasm
{$endif i386}
{$ifdef powerpc}
  ,rappcgas
{$endif powerpc}
{$ifdef x86_64}
  ,rax64att
{$endif x86_64}
{$ifdef arm}
  ,raarmgas
{$endif arm}
{$ifdef SPARC}
  ,racpugas
{$endif SPARC}
  ;

function Compile(const cmd:string):longint;


implementation

uses
{$IFDEF USE_SYSUTILS}
  SysUtils,
{$ENDIF USE_SYSUTILS}
  aasmcpu;

{$ifdef EXTDEBUG}
  {$define SHOWUSEDMEM}
{$endif}
{$ifdef MEMDEBUG}
  {$define SHOWUSEDMEM}
{$endif}

var
  CompilerInitedAfterArgs,
  CompilerInited : boolean;


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
     DoneParser;
     DoneImport;
     DoneExport;
     DoneLinker;
     DoneAssembler;
     DoneAsm;
   end;
{ Free memory for the others }
  CompilerInited:=false;
  DoneSymtable;
  DoneGlobals;
  donetokens;
end;


procedure InitCompiler(const cmd:string);
begin
  if CompilerInited then
   DoneCompiler;
{ inits which need to be done before the arguments are parsed }
  InitSystems;
  { globals depends on source_info so it must be after systems }
  InitGlobals;
  { verbose depends on exe_path and must be after globals }
  InitVerbose;
{$ifdef BrowserLog}
  InitBrowserLog;
{$endif BrowserLog}
{$ifdef BrowserCol}
  do_initSymbolInfo;
{$endif BrowserCol}
  inittokens;
  InitSymtable; {Must come before read_arguments, to enable macrosymstack}
  CompilerInited:=true;
{ this is needed here for the IDE
  in case of compilation failure
  at the previous compile }
  current_module:=nil;
{ read the arguments }
  read_arguments(cmd);
{ inits which depend on arguments }
  InitParser;
  InitImport;
  InitExport;
  InitLinker;
  InitAssembler;
  InitAsm;
  CompilerInitedAfterArgs:=true;
end;


function Compile(const cmd:string):longint;

{$ifdef fpc}
{$maxfpuregisters 0}
{$endif fpc}

  procedure writepathlist(w:longint;l:TSearchPathList);
  var
    hp : tstringlistitem;
  begin
    hp:=tstringlistitem(l.first);
    while assigned(hp) do
     begin
       Message1(w,hp.str);
       hp:=tstringlistitem(hp.next);
     end;
  end;

  function getrealtime : real;
  var
{$IFDEF USE_SYSUTILS}
    h,m,s,s1000 : word;
{$ELSE USE_SYSUTILS}
    h,m,s,s100 : word;
{$ENDIF USE_SYSUTILS}
  begin
{$IFDEF USE_SYSUTILS}
    DecodeTime(Time,h,m,s,s1000);
    getrealtime:=h*3600.0+m*60.0+s+s1000/1000.0;
{$ELSE USE_SYSUTILS}
    gettime(h,m,s,s100);
    getrealtime:=h*3600.0+m*60.0+s+s100/100.0;
{$ENDIF USE_SYSUTILS}
  end;

var
  starttime  : real;
{$ifdef SHOWUSEDMEM}
{$ifdef HASGETHEAPSTATUS}
  hstatus : TFPCHeapStatus;
{$endif HASGETHEAPSTATUS}
{$endif SHOWUSEDMEM}
begin
  try
    try
       { Initialize the compiler }
       InitCompiler(cmd);

       { show some info }
       Message1(general_t_compilername,FixFileName(system.paramstr(0)));
       Message1(general_d_sourceos,source_info.name);
       Message1(general_i_targetos,target_info.name);
       Message1(general_t_exepath,exepath);
       WritePathList(general_t_unitpath,unitsearchpath);
       WritePathList(general_t_includepath,includesearchpath);
       WritePathList(general_t_librarypath,librarysearchpath);
       WritePathList(general_t_objectpath,objectsearchpath);

       starttime:=getrealtime;

       { Compile the program }
  {$ifdef PREPROCWRITE}
       if parapreprocess then
        parser.preprocess(inputdir+inputfile+inputextension)
       else
  {$endif PREPROCWRITE}
        parser.compile(inputdir+inputfile+inputextension);

       { Show statistics }
       if status.errorcount=0 then
        begin
          starttime:=getrealtime-starttime;
          if starttime<0 then
            starttime:=starttime+3600.0*24.0;
          Message2(general_i_abslines_compiled,tostr(status.compiledlines),tostr(trunc(starttime))+
            '.'+tostr(trunc(frac(starttime)*10)));
        end;
     finally
       { no message possible after this !!    }
       DoneCompiler;
     end;
  except

    on EControlCAbort do
      begin
        try
          { in case of 50 errors, this could cause another exception,
            suppress this exception
          }
          Message(general_e_compilation_aborted);
        except
          on ECompilerAbort do
            ;
        end;
        DoneVerbose;
      end;
    on ECompilerAbort do
      begin
        try
          { in case of 50 errors, this could cause another exception,
            suppress this exception
          }
          Message(general_e_compilation_aborted);
        except
          on ECompilerAbort do
            ;
        end;
        DoneVerbose;
      end;
    on ECompilerAbortSilent do
      begin
        DoneVerbose;
      end;
    on Exception do
      begin
        { General catchall, normally not used }
        try
          { in case of 50 errors, this could cause another exception,
            suppress this exception
          }
          Message(general_e_compilation_aborted);
        except
          on ECompilerAbort do
            ;
        end;
        DoneVerbose;
        Raise;
      end;
  end;
{$ifdef SHOWUSEDMEM}
  {$ifdef HASGETHEAPSTATUS}
      hstatus:=GetFPCHeapStatus;
      Writeln('Max Memory used/heapsize: ',DStr(hstatus.MaxHeapUsed shr 10),'/',DStr(hstatus.MaxHeapSize shr 10),' Kb');
  {$else HASGETHEAPSTATUS}
      Writeln('Memory used (heapsize): ',DStr(system.Heapsize shr 10),' Kb');
  {$endif HASGETHEAPSTATUS}
{$endif SHOWUSEDMEM}

  { Set the return value if an error has occurred }
  if status.errorcount=0 then
    result:=0
  else
    result:=1;
end;

end.
{
  $Log: compiler.pas,v $
  Revision 1.61  2005/05/06 18:54:26  florian
    * better exception catching

  Revision 1.60  2005/04/24 21:01:37  peter
    * always use exceptions to stop the compiler
    - remove stop, do_stop

  Revision 1.59  2005/03/25 21:55:43  jonas
    * removed some unused variables

  Revision 1.58  2005/02/28 15:38:38  marco
   * getFPCheapstatus  (no, FPC HEAP, not FP CHEAP!)

  Revision 1.57  2005/02/15 19:15:45  peter
    * Handle Control-C exception more cleanly

  Revision 1.56  2005/02/14 17:13:06  peter
    * truncate log

  Revision 1.55  2005/02/13 20:11:16  peter
    * sunos to solaris

  Revision 1.54  2005/02/13 18:55:19  florian
    + overflow checking for the arm

  Revision 1.53  2005/01/31 21:30:56  olle
    + Added fake Exception classes, only for MACOS.

  Revision 1.52  2005/01/26 16:23:28  peter
    * detect arithmetic overflows for constants at compile time
    * use try..except instead of setjmp

  Revision 1.51  2005/01/09 20:24:43  olle
    * rework of macro subsystem
    + exportable macros for mode macpas

}
