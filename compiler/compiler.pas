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

unit compiler;

{$i defines.inc}

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
{$ifndef NOPASS2}
  ,cpunode
{$endif}
  { cpu targets }
  ,cputarg
  ;

function Compile(const cmd:string):longint;


implementation

uses
  cpubase,cpuasm;

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
{$ifdef EXTDEBUG}
  {$ifdef FPC}
    LostMemory:=system.HeapSize-MemAvail-EntryMemUsed;
    CheckMemory(LostMemory);
  {$endif FPC}
  Writeln('Repetitive firstpass = '+tostr(firstpass_several)+'/'+tostr(total_of_firstpass));
  Writeln('Repetitive resulttypepass = ',multiresulttypepasscnt,'/',resulttypepasscnt);
{$endif EXTDEBUG}
{$ifdef MEMDEBUG}
  Writeln('Memory used: ',system.Heapsize);
{$endif}
{$ifdef fixLeaksOnError}
  do_stop{$ifdef FPCPROCVAR}(){$endif};
{$endif fixLeaksOnError}
end;

end.
{
  $Log$
  Revision 1.25  2002-04-15 19:53:54  peter
    * fixed conflicts between the last 2 commits

  Revision 1.24  2002/04/15 18:56:42  carl
  + InitAsm

  Revision 1.23  2002/03/24 19:05:31  carl
  + patch for SPARC from Mazen NEIFER

  Revision 1.22  2001/09/18 11:30:47  michael
  * Fixes win32 linking problems with import libraries
  * LINKLIB Libraries are now looked for using C file extensions
  * get_exepath fix

  Revision 1.21  2001/05/06 14:49:16  peter
    * ppu object to class rewrite
    * move ppu read and write stuff to fppu

  Revision 1.20  2001/04/21 13:37:16  peter
    * made tclassheader using class of to implement cpu dependent code

  Revision 1.19  2001/04/18 22:01:53  peter
    * registration of targets and assemblers

  Revision 1.18  2001/04/13 18:08:36  peter
    * scanner object to class

  Revision 1.17  2001/04/13 01:22:06  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.16  2001/04/02 21:20:29  peter
    * resulttype rewrite

  Revision 1.15  2000/12/26 15:57:25  peter
    * use system.paramstr()

  Revision 1.14  2000/12/25 00:07:25  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.13  2000/12/24 12:24:38  peter
    * moved preprocessfile into a conditional

  Revision 1.11  2000/11/29 00:30:30  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.10  2000/10/31 22:02:46  peter
    * symtable splitted, no real code changes

  Revision 1.9  2000/10/15 09:39:36  peter
    * moved cpu*.pas to i386/
    * renamed n386 to common cpunode

  Revision 1.8  2000/10/14 10:14:46  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.7  2000/10/08 10:26:33  peter
    * merged @result fix from Pierre

  Revision 1.6  2000/09/24 15:06:14  peter
    * use defines.inc

  Revision 1.5  2000/08/27 16:11:50  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.4  2000/08/21 09:14:40  jonas
    - removed catch unit from uses clause for Linux (clashed with fpcatch
     from IDE and is already in pp.pas for command line compiler) (merged
     from fixes branch)

  Revision 1.3  2000/08/04 22:00:50  peter
    * merges from fixes

  Revision 1.2  2000/07/13 11:32:38  michael
  + removed logs
}
