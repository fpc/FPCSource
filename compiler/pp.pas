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
  NOAG386BIN          leaves out the binary writer, default for TP
  LOGMEMBLOCKS        adds memory manager which logs the size of
                      each allocated memory block, the information
                      is written to memuse.log after compiling
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
  {$DEFINE NOAG386BIN}
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
{$ifdef FPC}
{$ifdef heaptrc}
  ppheap,
{$endif heaptrc}
{$ifdef linux}
  catch,
{$endif}
{$ifdef go32v2}
  {$ifdef DEBUG}
    {$define NOCATCH}
  {$endif DEBUG}
  catch,
  {$ifdef nocatch}
  lineinfo,
  {$endif nocatch}
{$endif}
{$ifdef win32}
  {$ifdef DEBUG}
  lineinfo,
  {$endif DEBUG}
{$endif}
{$endif FPC}
  globals,compiler
{$ifdef logmemblocks}
{$ifdef fpc}
  ,memlog
{$endif fpc}
{$endif logmemblocks}
  ;

{$ifdef useoverlay}
  {$O files}
  {$O globals}
  {$O hcodegen}
  {$O pass_1}
  {$O pass_2}
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
  {$ifdef gdb}
        {$O gdb}
  {$endif gdb}
  {$ifdef i386}
        {$O cpubase}
        {$O cgai386}
        {$O tgeni386}
        {$O cg386add}
        {$O cg386cal}
        {$O cg386cnv}
        {$O cg386con}
        {$O cg386flw}
        {$O cg386ld}
        {$O cg386inl}
        {$O cg386mat}
        {$O cg386set}
        {$ifndef NOOPT}
          {$O aopt386}
          {$O opts386}
        {$endif}
        {$IfNDef Nora386dir}
          {$O ra386dir}
        {$endif}
        {$IfNDef Nora386int}
          {$O ra386int}
        {$endif}
        {$IfNDef Nora386att}
          {$O ra386att}
        {$endif}
        {$ifndef NoAg386Int}
          {$O ag386int}
        {$endif}
        {$ifndef NoAg386Att}
          {$O ag386att}
        {$endif}
        {$ifndef NoAg386Nsm}
          {$O ag386nsm}
        {$endif}
  {$endif}
  {$ifdef m68k}
        {$O opts68k}
        {$O cpubase}
        {$O cga68k}
        {$O tgen68k}
        {$O cg68kadd}
        {$O cg68kcal}
        {$O cg68kcnv}
        {$O cg68kcon}
        {$O cg68kflw}
        {$O cg68kld}
        {$O cg68kinl}
        {$O cg68kmat}
        {$O cg68kset}
        {$IfNDef Nora68kMot}
          {$O ra68kmot}
        {$endif}
        {$IfNDef Noag68kGas}
          {$O ag68kgas}
        {$endif}
        {$IfNDef Noag68kMot}
          {$O ag68kmot}
        {$endif}
        {$IfNDef Noag68kMit}
          {$O ag68kmit}
        {$endif}
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
     { we cannot use aktfilepos.file because all memory might have been
       freed already !
       But we can use global parser_current_file var }
     Writeln('Compilation aborted ',parser_current_file,':',aktfilepos.line);
   end;
end;

begin
  oldexit:=exitproc;
  exitproc:=@myexit;
{$ifdef UseOverlay}
  InitOverlay;
{$endif}

{ Call the compiler with empty command, so it will take the parameters }
  Halt(compiler.Compile(''));
end.
{
  $Log$
  Revision 1.57  2000-03-14 16:30:14  pierre
   + lineinfo for win32 with debug

  Revision 1.56  2000/02/18 12:34:43  pierre
   DEBUG implies NOCATCH for go32v2

  Revision 1.55  2000/02/10 23:44:43  florian
    * big update for exception handling code generation: possible mem holes
      fixed, break/continue/exit should work always now as expected

  Revision 1.54  2000/02/09 13:22:59  peter
    * log truncated

  Revision 1.53  2000/01/07 01:14:30  peter
    * updated copyright to 2000

  Revision 1.52  1999/11/06 14:34:23  peter
    * truncated log to 20 revs

  Revision 1.51  1999/11/05 13:15:00  florian
    * some fixes to get the new cg compiling again

  Revision 1.50  1999/09/17 17:14:10  peter
    * @procvar fixes for tp mode
    * @<id>:= gives now an error

  Revision 1.49  1999/09/16 23:05:54  florian
    * m68k compiler is again compilable (only gas writer, no assembler reader)

  Revision 1.48  1999/09/10 18:48:08  florian
    * some bug fixes (e.g. must_be_valid and procinfo.funcret_is_valid)
    * most things for stored properties fixed

  Revision 1.47  1999/09/02 18:47:45  daniel
    * Could not compile with TP, some arrays moved to heap
    * NOAG386BIN default for TP
    * AG386* files were not compatible with TP, fixed.

  Revision 1.46  1999/08/28 15:34:20  florian
    * bug 519 fixed

  Revision 1.45  1999/08/04 00:23:18  florian
    * renamed i386asm and i386base to cpuasm and cpubase
}