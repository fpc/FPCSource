{$MINSTACKSIZE $00004000}
{$MAXSTACKSIZE $00100000}
{$IMAGEBASE $00400000}
{$APPTYPE CONSOLE}
{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Commandline compiler for Free Pascal

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
  USEOVERLAY          compiles a TP version which uses overlays
  EXTDEBUG            some extra debug code is executed
  SUPPORT_MMX         only i386: releases the compiler switch
                      MMX which allows the compiler to generate
                      MMX instructions
  EXTERN_MSG          Don't compile the msgfiles in the compiler, always
                      use external messagefiles, default for TP
  NOAG386INT          no Intel Assembler output
  NOAG386NSM          no NASM output
  -----------------------------------------------------------------

  Required switches for a i386 compiler be compiled by Free Pascal Compiler:
  GDB;I386

  Required switches for a i386 compiler be compiled by Turbo Pascal:
  GDB;I386;TP

  Required switches for a 68000 compiler be compiled by Turbo Pascal:
  GDB;M68k;TP

  To compile the compiler with Delphi do the following:

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

{$ifndef DELPHI}
{$ifdef TP}
  {$IFNDEF DPMI}
    {$M 24000,0,655360}
  {$ELSE}
    {$M 65000}
  {$ENDIF DPMI}
  {$E+,N+,F+,S-,R-}
{$endif TP}
{$endif DELPHI}


program pp;

{$IFDEF TP}
  {$UNDEF PROFILE}
  {$IFDEF DPMI}
    {$UNDEF USEOVERLAY}
  {$ENDIF}
{$ENDIF}
{$ifdef FPC}
  {$UNDEF USEOVERLAY}
{$ENDIF}

uses
{$ifdef useoverlay}
  {$ifopt o+}
    Overlay,ppovin,
  {$else}
    {$error You must compile with the $O+ switch}
  {$endif}
{$endif useoverlay}
{$ifdef profile}
  profile,
{$endif profile}
  globals,compiler;

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
  {$O comphook}
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
{$ifndef NOOPT}
        {$O aopt386}
{$endif NOOPT}
        {$O cgai386}
        {$O i386}
{$IfNDef Nora386dir}
        {$O ra386dir}
{$endif Nora386dir}
{$IfNDef Nora386int}
        {$O ra386int}
{$endif Nora386int}
{$IfNDef Nora386att}
        {$O ra386att}
{$endif Nora386att}
        {$O tgeni386}
{$ifndef NoAg386Int}
        {$O ag386int}
{$endif NoAg386Int}
        {$O ag386att}
{$ifndef NoAg386Nsm}
        {$O ag386nsm}
{$endif}
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

var
  oldexit : pointer;
procedure myexit;{$ifndef FPC}far;{$endif}
begin
  exitproc:=oldexit;
{ Show Runtime error if there was an error }
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
     Writeln('Compilation aborted at line ',aktfilepos.line);
   end;
end;

begin
  oldexit:=exitproc;
  exitproc:=@myexit;
{$ifndef VER0_99_5}
  {$ifndef TP}
  {$ifndef Delphi}
    heapblocks:=true;
  {$endif Delphi}
  {$endif}
{$endif}
{$ifdef UseOverlay}
  InitOverlay;
{$endif}

{ Call the compiler with empty command, so it will take the parameters }
  Halt(Compile(''));
end.
{
  $Log$
  Revision 1.1  2000-07-13 06:29:54  michael
  + Initial import

  Revision 1.4  2000/01/07 01:14:30  peter
    * updated copyright to 2000

  Revision 1.3  1999/07/18 10:20:00  florian
    * made it compilable with Dlephi 4 again
    + fixed problem with large stack allocations on win32

  Revision 1.2  1999/05/04 21:44:58  florian
    * changes to compile it with Delphi 4.0

  Revision 1.1  1998/09/18 16:03:44  florian
    * some changes to compile with Delphi

  Revision 1.28  1998/08/26 15:31:17  peter
    * heapblocks for >0.99.5

  Revision 1.27  1998/08/11 00:00:00  peter
    * fixed dup log

  Revision 1.26  1998/08/10 15:49:40  peter
    * small fixes for 0.99.5

  Revision 1.25  1998/08/10 14:50:16  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.24  1998/08/10 10:18:32  peter
    + Compiler,Comphook unit which are the new interface units to the
      compiler

  Revision 1.23  1998/08/05 16:00:16  florian
    * some fixes for ansi strings

  Revision 1.22  1998/08/04 16:28:40  jonas
  * added support for NoRa386* in the $O ... section

  Revision 1.21  1998/07/18 17:11:12  florian
    + ansi string constants fixed
    + switch $H partial implemented

  Revision 1.20  1998/07/14 14:46:55  peter
    * released NEWINPUT

  Revision 1.19  1998/07/07 11:20:04  peter
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
