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
}

{$ifdef FPC}
   {$ifndef GDB}
      { people can try to compile without GDB }
      { $error The compiler switch GDB must be defined}
   {$endif GDB}

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
   {$fatal One of the switches I386,Alpha, PowerPC or M68K must be defined}
   {$endif}

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
{$ifdef Unix}
  catch,
{$endif}
{$endif FPC}
  globals,compiler
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
{$ifdef i386}
  {$O os2_targ}
  {$O win_targ}
{$endif i386}
  {$ifdef gdb}
        {$O gdb}
  {$endif gdb}
  {$ifdef i386}
        {$O opts386}
        {$O i386base}
        {$O i386asm}
        {$O tgeni386}
        {$ifndef NOOPT}
          {$O aopt386}
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
        {$O m68k}
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
     Writeln('Compilation aborted at line ',aktfilepos.line);
   end;
end;

begin
  oldexit:=exitproc;
  exitproc:=@myexit;
{$ifdef UseOverlay}
  InitOverlay;
{$endif}

{ Call the compiler with empty command, so it will take the parameters }
  Halt(Compile(''));
end.
{
  $Log$
  Revision 1.2  2002-06-02 08:41:22  marco
   * renamefest

  Revision 1.1  2000/07/13 06:30:08  michael
  + Initial import

  Revision 1.8  2000/01/07 01:14:54  peter
    * updated copyright to 2000

  Revision 1.7  1999/10/12 21:20:47  florian
    * new codegenerator compiles again

  Revision 1.6  1999/08/04 12:59:22  jonas
    * all tokes now start with an underscore
    * PowerPC compiles!!

  Revision 1.5  1999/08/02 21:29:06  florian
    * the main branch psub.pas is now used for
      newcg compiler

  Revision 1.4  1999/08/02 17:15:03  michael
  + CPU check better

  Revision 1.3  1999/08/02 17:14:10  florian
    + changed the temp. generator to an object

  Revision 1.2  1999/08/01 18:22:37  florian
   * made it again compilable

  Revision 1.1  1998/12/26 15:20:31  florian
    + more changes for the new version

}
