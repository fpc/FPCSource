{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

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
  possible compiler switches (* marks a currently required switch):
  -----------------------------------------------------------------
  USE_RHIDE           generates errors and warning in an format recognized
                      by rhide
  TP                  to compile the compiler with Turbo or Borland Pascal
  GDB*                support of the GNU Debugger
  I386                generate a compiler for the Intel i386+
  M68K                generate a compiler for the M68000
  MULLER              release special debug code of Pierre Muller
                      (needs some extra units)
  USEOVERLAY          compiles a TP version which uses overlays
  EXTDEBUG            some extra debug code is executed
  SUPPORT_MMX         only i386: releases the compiler switch
                      MMX which allows the compiler to generate
                      MMX instructions
  EXTERN_MSG          Don't compile the msgfiles in the compiler, always
                      use external messagefiles
  -----------------------------------------------------------------

  Required switches for a i386 compiler be compiled by Free Pascal Compiler:
  GDB;I386

  Required switches for a i386 compiler be compiled by Turbo Pascal:
  GDB;I386;TP

  Required switches for a 68000 compiler be compiled by Turbo Pascal:
  GDB;M68k;TP
}

{$ifdef FPC}
   {$ifndef GDB}
      { people can try to compile without GDB }
      { $error The compiler switch GDB must be defined}
   {$endif GDB}
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

{$ifdef TP}
  {$IFNDEF DPMI}
    {$M 24000,0,655360}
  {$ELSE}
    {$M 65000}
  {$ENDIF DPMI}
  {$E+,N+,F+,S-,R-}
{$endif TP}


program pp;

{$IFDEF TP}
  {$UNDEF PROFILE}
  {$IFDEF DPMI}
    {$UNDEF USEOVERLAY}
  {$ENDIF}
{$ENDIF}
{$ifdef FPC}
  {$UNDEF USEOVERLAY}
  {$UNDEF USEPMD}
{$ENDIF}

uses
{$ifdef fpc}
  {$ifdef GO32V2}
    emu387,
    dpmiexcp,
  {$endif GO32V2}
{$endif}
{$ifdef useoverlay}
  {$ifopt o+}
    Overlay,ppovin,
  {$else}
  { warn when not $O+ is used }
    - You must compile with the $O+ switch
  {$endif}
{$endif useoverlay}
{$ifdef lock}
  lock,
{$endif lock}
{$ifdef profile}
  profile,
{$endif profile}
{$ifdef muller}
  openfile,
  {$ifdef usepmd}
    usepmd,
  {$endif usepmd}
{$endif}
{$ifdef LINUX}
  catch,
{$endif LINUX}
{$IfDef PMD}
     OpenFile,
     BBError,
     ObjMemory,
     PMD, MemCheck,
{$EndIf}
{$ifdef TP}
  objects,
{$endif}

  dos,cobjects,
  globals,parser,systems,tree,symtable,options,link,import,files,
  verb_def,verbose;

{$ifdef useoverlay}
  {$O files}
  {$O globals}
  {$O hcodegen}
  {$O pass_1}
  {$O tree}
  {$O types}
  {$O objects}
  {$O options}
  {$O cobjects}
  {$O globals}
  {$O systems}
  {$O parser}
  {$O pbase}
  {$O pdecl}
  {$O pexports}
  {$O pexpr}
  {$O pmodules}
  {$O pstatmnt}
  {$O psub}
  {$O psystem}
  {$O ptconst}
  {$O script}
  {$O switches}
  {$O temp_gen}
  {$O verb_def}
  {$O dos}
  {$O scanner}
  {$O symtable}
  {$O objects}
  {$O aasm}
  {$O link}
  {$O assemble}
  {$O messages}
  {$O gendef}
  {$O import}
  {$O os2_targ}
  {$O win_targ}
  {$O asmutils}
  {$ifdef gdb}
        {$O gdb}
  {$endif gdb}
  {$ifdef i386}
        {$O opts386}
        {$O cgi386}
        {$O cg386add}
        {$O cg386cal}
        {$O cg386cnv}
        {$O cg386con}
        {$O cg386flw}
        {$O cg386ld}
        {$O cg386mat}
        {$O cg386set}
        {$O aopt386}
        {$O cgai386}
        {$O i386}
        {$O ra386dir}
        {$O ra386int}
        {$O ra386att}
        {$O tgeni386}
        {$O ag386int}
        {$O ag386att}
        {$O ag386nsm}
  {$endif}
  {$ifdef m68k}
        {$O opts68k}
        {$O cg68k}
        {$O ra68kmot}
        {$O ag68kgas}
        {$O ag68kmot}
        {$O ag68kmit}
  {$endif}
{$endif useoverlay}


function getrealtime : real;
var
  h,m,s,s100 : word;
begin
  dos.gettime(h,m,s,s100);
  getrealtime:=h*3600.0+m*60.0+s+s100/100.0;
end;



var
  oldexit : pointer;
procedure myexit;{$ifndef FPC}far;{$endif}
begin
  exitproc:=oldexit;
{$ifdef tp}
  if use_big then
   symbolstream.done;
{$endif}
  if (erroraddr<>nil) then
   begin
     case exitcode of
      202 : begin
              erroraddr:=nil;
              Writeln('Error: Stack Overflow');
            end;
      203 : begin
              erroraddr:=nil;
              Writeln('Error: Out of memory');
            end;
     end;
   {when the module is assigned, then the messagefile is also loaded}
{$ifdef NEWINPUT}
     Writeln('Compilation aborted at line ',aktfilepos.line);
{$else}
     if assigned(current_module) and assigned(current_module^.current_inputfile) then
      Writeln('Compilation aborted at line ',current_module^.current_inputfile^.line_no);
{$endif}
   end;
end;


{$ifdef tp}
  procedure do_streamerror;
  begin
    if symbolstream.status=-2 then
     WriteLn('Error: Not enough EMS memory')
    else
     WriteLn('Error: EMS Error ',symbolstream.status);
  {$ifndef MULLER}
    halt(1);
  {$else MULLER}
    runerror(190);
  {$endif MULLER}
  end;

  {$ifdef USEOVERLAY}
    function _heaperror(size:word):integer;far;
    type
      heaprecord=record
        next:pointer;
        values:longint;
      end;
    var
      l,m:longint;
    begin
      l:=ovrgetbuf-ovrminsize;
      if (size>maxavail) and (l>=size) then
       begin
         m:=((longint(size)+$3fff) and $ffffc000);
         {Clear the overlay buffer.}
         ovrclearbuf;
         {Shrink it.}
         ovrheapend:=ovrheapend-m shr 4;
         heaprecord(ptr(ovrheapend,0)^).next:=freelist;
         heaprecord(ptr(ovrheapend,0)^).values:=m shl 12;
         heaporg:=ptr(ovrheapend,0);
         freelist:=heaporg;
         Writeln('Warning: Overlay buffer shrinked, because of memory shortage');
         _heaperror:=2;
       end
      else
       _heaperror:=0;
    end;
  {$endif USEOVERLAY}
{$endif TP}



var
  start : real;
{$IfDef Extdebug}
  EntryMemAvail : longint;
{$EndIf}
begin
  oldexit:=exitproc;
  exitproc:=@myexit;
{$ifdef linux}
  heapblocks:=true;
{$else}
  {$ifdef go32v2}
    heapblocks:=true;
  {$endif}
{$endif}
{$ifdef EXTDEBUG}
   EntryMemAvail:=MemAvail;
{$endif}
{$ifdef MULLER}
  {$ifdef DPMI}
     HeapBlock:=$ff00;
  {$endif DPMI}
{$endif MULLER}
{$ifdef TP}
  {$IFDEF USEOVERLAY}
    heaperror:=@_heaperror;
  {$ENDIF USEOVERLAY}
   if use_big then
    begin
      streamerror:=@do_streamerror;
    { symbolstream.init('TMPFILE',stcreate,16000); }
    {$ifndef dpmi}
      symbolstream.init(10000,4000000); {using ems streams}
    {$else}
      symbolstream.init(1000000,16000); {using memory streams}
    {$endif}
      if symbolstream.errorinfo=stiniterror then
       do_streamerror;
    { write something, because pos 0 means nil pointer }
      symbolstream.writestr(@inputfile);
    end;
{$endif tp}

   { inits which need to be done  before the arguments are parsed }
   get_exepath;
   init_tree;
   globalsinit;
   init_symtable;
   linker.init;

   { read the arguments }
   read_arguments;

   { inits which depend on arguments }
   initparser;
   initimport;

   {show some info}
   Message1(general_i_compilername,FixFileName(paramstr(0)));
   Message1(general_i_unitsearchpath,unitsearchpath);
   Message1(general_d_sourceos,source_os.name);
   Message1(general_i_targetos,target_os.name);
   Message1(general_u_exepath,exepath);
{$ifdef linux}
   Message1(general_u_gcclibpath,Linker.librarysearchpath);
{$endif}
{$ifdef TP}
   Comment(V_Info,'Memory: '+tostr(MemAvail)+' Bytes Free');
{$endif}

   start:=getrealtime;
   compile(inputdir+inputfile+inputextension,false);
   if status.errorcount=0 then
    begin
      start:=getrealtime-start;
      Message2(general_i_abslines_compiled,tostr(status.compiledlines),tostr(trunc(start))+'.'+tostr(trunc(frac(start)*10)));
    end;
{***Obsolete
   clearnodes;
***}
   done_symtable;
{$ifdef TP}
   Comment(V_Info,'Memory: '+tostr(MemAvail)+' Bytes Free');
{$endif}
{$ifdef EXTDEBUG}
   Comment(V_Info,'Memory lost = '+tostr(EntryMemAvail-MemAvail));
{$endif EXTDEBUG}
{ exits with error 1 if no codegeneration }
   if status.errorcount=0 then
    halt(0)
   else
    halt(1);
end.
{
  $Log$
  Revision 1.19  1998-07-07 11:20:04  peter
    + NEWINPUT for a better inputfile and scanner object

  Revision 1.18  1998/06/24 14:06:33  peter
    * fixed the name changes

  Revision 1.17  1998/06/23 08:59:22  daniel
  * Recommitted.

  Revision 1.16  1998/06/17 14:10:17  peter
    * small os2 fixes
    * fixed interdependent units with newppu (remake3 under linux works now)

  Revision 1.15  1998/06/16 11:32:18  peter
    * small cosmetic fixes

  Revision 1.14  1998/06/15 13:43:45  daniel


  * Updated overlays.

  Revision 1.12  1998/05/23 01:21:23  peter
    + aktasmmode, aktoptprocessor, aktoutputformat
    + smartlink per module $SMARTLINK-/+ (like MMX) and moved to aktswitches
    + $LIBNAME to set the library name where the unit will be put in
    * splitted cgi386 a bit (codeseg to large for bp7)
    * nasm, tasm works again. nasm moved to ag386nsm.pas

  Revision 1.11  1998/05/20 09:42:35  pierre
    + UseTokenInfo now default
    * unit in interface uses and implementation uses gives error now
    * only one error for unknown symbol (uses lastsymknown boolean)
      the problem came from the label code !
    + first inlined procedures and function work
      (warning there might be allowed cases were the result is still wrong !!)
    * UseBrower updated gives a global list of all position of all used symbols
      with switch -gb

  Revision 1.10  1998/05/12 10:47:00  peter
    * moved printstatus to verb_def
    + V_Normal which is between V_Error and V_Warning and doesn't have a
      prefix like error: warning: and is included in V_Default
    * fixed some messages
    * first time parameter scan is only for -v and -T
    - removed old style messages

  Revision 1.9  1998/05/11 13:07:56  peter
    + $ifdef NEWPPU for the new ppuformat
    + $define GDB not longer required
    * removed all warnings and stripped some log comments
    * no findfirst/findnext anymore to remove smartlink *.o files

  Revision 1.8  1998/05/08 09:21:57  michael
  + Librarysearchpath is now a linker object field;

  Revision 1.7  1998/05/04 17:54:28  peter
    + smartlinking works (only case jumptable left todo)
    * redesign of systems.pas to support assemblers and linkers
    + Unitname is now also in the PPU-file, increased version to 14

  Revision 1.6  1998/04/29 13:40:23  peter
    + heapblocks:=true

  Revision 1.5  1998/04/29 10:33:59  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.3  1998/04/21 10:16:48  peter
    * patches from strasbourg
    * objects is not used anymore in the fpc compiled version

  Revision 1.2  1998/04/07 13:19:47  pierre
    * bugfixes for reset_gdb_info
      in MEM parsing for go32v2
      better external symbol creation
      support for rhgdb.exe (lowercase file names)
}
