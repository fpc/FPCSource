{
    $Id$
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
{$ifdef sunos}
  ,i_sunos
{$endif sunos}
{$ifdef wdosx}
  ,i_wdosx
{$endif wdosx}
{$ifdef win32}
  ,i_win32
{$endif win32}
  ;

function Compile(const cmd:string):longint;


implementation

uses
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
  olddo_stop : tstopprocedure;

{$ifdef USEEXCEPT}
procedure RecoverStop;
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
  InitSymtable;
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

procedure minimal_stop;
begin
  DoneCompiler;
  olddo_stop{$ifdef FPCPROCVAR}(){$endif};
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
  do_stop:={$ifdef FPCPROCVAR}@{$endif}minimal_stop;
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

{$ifdef USEEXCEPT}
  if setjmp(recoverpos)=0 then
   begin
     recoverpospointer:=@recoverpos;
     do_stop:={$ifdef FPCPROCVAR}@{$endif}recoverstop;
{$endif USEEXCEPT}
     starttime:=getrealtime;
{$ifdef PREPROCWRITE}
     if parapreprocess then
      parser.preprocess(inputdir+inputfile+inputextension)
     else
{$endif PREPROCWRITE}
      parser.compile(inputdir+inputfile+inputextension);
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
{$ifdef SHOWUSEDMEM}
  Writeln('Memory used (heapsize): ',DStr(system.Heapsize shr 10),' Kb');
{$endif SHOWUSEDMEM}
{$ifdef fixLeaksOnError}
  do_stop{$ifdef FPCPROCVAR}(){$endif};
{$endif fixLeaksOnError}
end;

end.
{
  $Log$
  Revision 1.40  2003-09-05 17:41:12  florian
    * merged Wiktor's Watcom patches in 1.1

  Revision 1.39  2003/09/03 11:18:36  florian
    * fixed arm concatcopy
    + arm support in the common compiler sources added
    * moved some generic cg code around
    + tfputype added
    * ...

  Revision 1.38  2003/05/22 21:39:51  peter
    * add cgcpu

  Revision 1.37  2003/03/23 23:20:38  hajny
    + emx target added

  Revision 1.36  2003/02/02 19:25:54  carl
    * Several bugfixes for m68k target (register alloc., opcode emission)
    + VIS target
    + Generic add more complete (still not verified)

  Revision 1.35  2002/09/05 19:28:31  peter
    * removed repetitive pass counting
    * display heapsize also for extdebug

  Revision 1.34  2002/08/17 09:23:34  florian
    * first part of procinfo rewrite

  Revision 1.33  2002/07/26 21:15:37  florian
    * rewrote the system handling

  Revision 1.32  2002/07/11 14:41:27  florian
    * start of the new generic parameter handling

  Revision 1.31  2002/07/04 19:00:23  florian
    + x86_64 define added

  Revision 1.30  2002/07/01 18:46:22  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.29  2002/05/18 13:34:06  peter
    * readded missing revisions

  Revision 1.28  2002/05/16 19:46:35  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.26  2002/05/12 16:53:05  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.25  2002/04/15 19:53:54  peter
    * fixed conflicts between the last 2 commits

  Revision 1.24  2002/04/15 18:56:42  carl
  + InitAsm

  Revision 1.23  2002/03/24 19:05:31  carl
  + patch for SPARC from Mazen NEIFER

}
