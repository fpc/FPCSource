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
      {$error The compiler switch GDB must be defined}
   {$endif GDB}
   { but I386 or M68K must be defined }
   { and only one of the two }
   {$ifndef I386}
      {$ifndef M68K}
        {$fatalerror One of the switches I386 or M68K must be defined}
      {$endif M68K}
   {$endif I386}
   {$ifdef I386}
      {$ifdef M68K}
        {$fatalerror ONLY one of the switches I386 or M68K must be defined}
      {$endif M68K}
   {$endif I386}
   {$ifdef support_mmx}
     {$ifndef i386}
       {$fatalerror I386 switch must be on for MMX support}
     {$endif i386}
   {$endif support_mmx}
{$endif}

{$ifdef TP}
  {$IFNDEF DPMI}
    {$M 24576,0,655360}
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
     OpenFiles,
     BBError,
     ObjMemory,
     PMD, MemCheck,
{$EndIf}

  dos,objects,cobjects,
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
  {$O dos}
  {$O scanner}
  {$O symtable}
  {$O objects}
  {$O aasm}
  {$ifdef gdb}
    {$O gdb}
  {$endif gdb}
  {$ifdef i386}
    {$O opts386}
    {$O cgi386}
    {$O aopt386}
    {$O cgai386}
    {$O i386}
    {$O radi386}
    {$O rai386}
    {$O ratti386}
    {$O tgeni386}
  {$endif}
  {$ifdef m68k}
    {$O opts68k}
    {$O cg68k}
    {$O ra68k}
    {$O ag68kgas}
  {$endif}
{$endif useoverlay}


function print_status(const status : tcompilestatus) : boolean;
begin
  print_status:=false;
  if (abslines=1) then
   Message1(general_i_kb_free,tostr(memavail shr 10));
  if (status.currentline mod 100=0) then
   Message2(general_l_lines_and_free,tostr(status.currentline),tostr(memavail shr 10));
{$ifdef tp}
  if (use_big) then
   begin
   {$ifdef dpmi}
     Message1(general_i_stream_kb_free,tostr(symbolstream.getsize shr 10));
   {$else}
     Message1(general_i_ems_kb_free,tostr(symbolstream.getsize shr 10));
   {$endif}
   end;
{$endif}
end;


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
     if assigned(current_module) and assigned(current_module^.current_inputfile) then
      Writeln('Compilation aborted at line ',current_module^.current_inputfile^.line_no);
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

  start:=getrealtime;
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

{$ifndef TP}
   compilestatusproc:=@print_status;
{$else}
   compilestatusproc:=print_status;
{$endif}

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
   Message1(general_d_sourceos,source_info.source_name);
   Message1(general_i_targetos,target_info.target_name);
   Message1(general_u_exepath,exepath);
{$ifdef linux}
   Message1(general_u_gcclibpath,Linker.gcclibrarypath);
{$endif}

   compile(inputdir+inputfile+inputextension,false);

   if errorcount=0 then
    begin
      start:=getrealtime-start;
      Message2(general_i_abslines_compiled,tostr(abslines),tostr(trunc(start))+'.'+tostr(trunc(frac(start)*10)));
    end;

   clearnodes;
   done_symtable;
{$ifdef EXTDEBUG}
   Comment(V_Info,'Memory lost = '+tostr(EntryMemAvail-MemAvail));
{$endif EXTDEBUG}
{ exits with error 1 if no codegeneration }
   if errorcount=0 then
    halt(0)
   else
    halt(1);
end.
{
  $Log$
  Revision 1.2  1998-04-07 13:19:47  pierre
    * bugfixes for reset_gdb_info
      in MEM parsing for go32v2
      better external symbol creation
      support for rhgdb.exe (lowercase file names)

  Revision 1.1.1.1  1998/03/25 11:18:14  root
  * Restored version

  Revision 1.40  1998/03/16 22:42:21  florian
    * some fixes of Peter applied:
      ofs problem, profiler support

  Revision 1.39  1998/03/10 15:20:30  carl
    * bugfix of spelling mistake
     * make it compile under TP with overlays

  Revision 1.38  1998/03/10 13:23:00  florian
    * small win32 problems fixed

  Revision 1.37  1998/03/10 01:17:24  peter
    * all files have the same header
    * messages are fully implemented, EXTDEBUG uses Comment()
    + AG... files for the Assembler generation

  Revision 1.36  1998/03/06 00:52:46  peter
    * replaced all old messages from errore.msg, only ExtDebug and some
      Comment() calls are left
    * fixed options.pas

  Revision 1.35  1998/03/05 02:44:16  peter
    * options cleanup and use of .msg file

  Revision 1.34  1998/03/04 17:33:52  michael
  + Changed ifdef FPK to ifdef FPC

  Revision 1.33  1998/03/02 23:08:42  florian
    * the concatcopy bug removed (solves problems when compilg sysatari!)

  Revision 1.32  1998/03/02 16:02:04  peter
    * new style messages for pp.pas
    * cleanup of pp.pas

  Revision 1.31  1998/03/02 13:38:49  peter
    + importlib object
    * doesn't crash on a systemunit anymore
    * updated makefile and depend

  Revision 1.30  1998/03/02 01:49:05  peter
    * renamed target_DOS to target_GO32V1
    + new verbose system, merged old errors and verbose units into one new
      verbose.pas, so errors.pas is obsolete

  Revision 1.29  1998/02/25 14:31:28  jonas
    * added $d- for TP compiling (disable strict var checking) and removed a duplicate $M statement

  Revision 1.28  1998/02/22 23:03:29  peter
    * renamed msource->mainsource and name->unitname
    * optimized filename handling, filename is not seperate anymore with
      path+name+ext, this saves stackspace and a lot of fsplit()'s
    * recompiling of some units in libraries fixed
    * shared libraries are working again
    + $LINKLIB <lib> to support automatic linking to libraries
    + libraries are saved/read from the ppufile, also allows more libraries
      per ppufile

  Revision 1.27  1998/02/16 14:19:15  florian
  *** empty log message ***

  Revision 1.26  1998/02/16 13:46:43  michael
  + Further integration of linker object:
    - all options pertaining to linking go directly to linker object
    - removed redundant variables/procedures, especially in OS_TARG...

  Revision 1.25  1998/02/16 12:51:40  michael
  + Implemented linker object

  Revision 1.24  1998/02/14 05:04:49  carl
    + more overlay stuff for m68k target

  Revision 1.23  1998/02/14 01:45:30  peter
    * more fixes
    - pmode target is removed
    - search_as_ld is removed, this is done in the link.pas/assemble.pas
    + findexe() to search for an executable (linker,assembler,binder)

  Revision 1.22  1998/02/13 22:26:39  peter
    * fixed a few SigSegv's
    * INIT$$ was not written for linux!
    * assembling and linking works again for linux and dos
    + assembler object, only attasmi3 supported yet
    * restore pp.pas with AddPath etc.

  Revision 1.18  1998/02/03 22:13:34  florian
    * clean up

  Revision 1.17  1998/02/02 00:55:33  peter
    * defdatei -> deffile and some german comments to english
    * search() accepts : as seperater under linux
    * search for ppc.cfg doesn't open a file (and let it open)
    * reorganize the reading of parameters/file a bit
    * all the PPC_ environments are now for all platforms

  Revision 1.16  1998/01/27 10:48:19  florian
    * dpmiexcp is now always used by a go32v2 compiler executable

  Revision 1.15  1998/01/25 18:45:50  peter
    + Search for as and ld at startup
    + source_info works the same as target_info
    + externlink allows only external linking

  Revision 1.14  1998/01/23 10:46:42  florian
    * small problems with FCL object model fixed, objpas?.inc is compilable

  Revision 1.13  1998/01/18 21:34:29  florian
  *** empty log message ***

  Revision 1.12  1998/01/16 12:52:10  michael
  + Path treatment and file searching should now be more or less in their
    definite form:
    - Using now modified AddPathToList everywhere.
    - File Searching mechanism is uniform for all files.
    - Include path is working now !!
    All fixes by Peter Vreman. Tested with remake3 target.

  Revision 1.11  1998/01/07 00:17:04  michael
  Restored released version (plus fixes) as current

  Revision 1.10  1997/12/12 13:28:39  florian
  + version 0.99.0
  * all WASM options changed into MASM
  + -O2 for Pentium II optimizations

  Revision 1.9  1997/12/09 13:57:21  carl
  * bugfix when compiling using overlays

  Revision 1.8  1997/12/05 14:38:39  carl
  * equivalent to version 1.5 (otherwise would not compile)

  Revision 1.5  1997/12/03 14:36:14  carl
  * bugfix of my bug with $ifdef support_mxx

  Revision 1.4  1997/12/03 13:41:37  carl
   + checks that i386 is defined if with mmx_support switch.

  Revision 1.3  1997/11/29 15:40:10  florian
  + myexit is now executed

  Revision 1.2  1997/11/28 18:14:43  pierre
   working version with several bug fixes

  Revision 1.1.1.1  1997/11/27 08:33:00  michael
  FPC Compiler CVS start


  Pre-CVS log:

  FK     Florian Klaempfl
  +      feature added
  -      removed
  *      bug fixed or changed

  History (started at 19th september 1997):
      19th september 1997:
       + informations about ccompiler switches added (FK)
       2nd october 1997:
         *- removed ifndef dpmi for stream init, tmemorystream is used if
          in dpmi everywhere else if use_big on. (CEC)
       6th november 1997:
         - crt unit to allow output redirection (FK)
}
