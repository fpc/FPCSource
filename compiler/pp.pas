{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

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

 ****************************************************************************
}
program pp;

{
  possible compiler switches (* marks a currently required switch):
  -----------------------------------------------------------------
  GDB*                support of the GNU Debugger
  I386                generate a compiler for the Intel i386+
  x86_64              generate a compiler for the AMD x86-64 architecture
  M68K                generate a compiler for the M68000
  SPARC               generate a compiler for SPARC
  POWERPC             generate a compiler for the PowerPC
  USEOVERLAY          compiles a TP version which uses overlays
  DEBUG               version with debug code is generated
  EXTDEBUG            some extra debug code is executed
  SUPPORT_MMX         only i386: releases the compiler switch
                      MMX which allows the compiler to generate
                      MMX instructions
  EXTERN_MSG          Don't compile the msgfiles in the compiler, always
                      use external messagefiles, default for TP
  NOAG386INT          no Intel Assembler output
  NOAG386NSM          no NASM output
  NOAG386BIN          leaves out the binary writer, default for TP
  NORA386DIR          No direct i386 assembler reader
  TEST_GENERIC        Test Generic version of code generator
                      (uses generic RTL calls)
  -----------------------------------------------------------------

  Required switches for a i386 compiler be compiled by Free Pascal Compiler:
  GDB;I386
}

{$i fpcdefs.inc}

{$ifdef FPC}
   {$ifndef GDB}
      { people can try to compile without GDB }
      { $error The compiler switch GDB must be defined}
   {$endif GDB}
   { exactly one target CPU must be defined }
   {$ifdef I386}
     {$ifdef CPUDEFINED}
        {$fatal ONLY one of the switches for the CPU type must be defined}
     {$endif CPUDEFINED}
     {$define CPUDEFINED}
   {$endif I386}
   {$ifdef x86_64}
     {$ifdef CPUDEFINED}
        {$fatal ONLY one of the switches for the CPU type must be defined}
     {$endif CPUDEFINED}
     {$define CPUDEFINED}
   {$endif x86_64}
   {$ifdef M68K}
     {$ifdef CPUDEFINED}
        {$fatal ONLY one of the switches for the CPU type must be defined}
     {$endif CPUDEFINED}
     {$define CPUDEFINED}
   {$endif M68K}
   {$ifdef iA64}
     {$ifdef CPUDEFINED}
        {$fatal ONLY one of the switches for the CPU type must be defined}
     {$endif CPUDEFINED}
     {$define CPUDEFINED}
   {$endif iA64}
   {$ifdef POWERPC}
     {$ifdef CPUDEFINED}
        {$fatal ONLY one of the switches for the CPU type must be defined}
     {$endif CPUDEFINED}
     {$define CPUDEFINED}
   {$endif POWERPC}
   {$ifdef ALPHA}
     {$ifdef CPUDEFINED}
        {$fatal ONLY one of the switches for the CPU type must be defined}
     {$endif CPUDEFINED}
     {$define CPUDEFINED}
   {$endif ALPHA}
   {$ifdef SPARC}
     {$ifdef CPUDEFINED}
        {$fatal ONLY one of the switches for the CPU type must be defined}
     {$endif CPUDEFINED}
     {$define CPUDEFINED}
   {$endif SPARC}
   {$ifndef CPUDEFINED}
     {$fatal A CPU type switch must be defined}
   {$endif CPUDEFINED}
   {$ifdef support_mmx}
     {$ifndef i386}
       {$fatal I386 switch must be on for MMX support}
     {$endif i386}
   {$endif support_mmx}
{$endif}

uses
{$ifdef FPC}
  {$ifdef profile}
    profile,
  {$endif profile}
  {$ifdef heaptrc}
    ppheap,
  {$endif heaptrc}
  {$ifdef EXTDEBUG}
    checkmem,
  {$endif EXTDEBUG}
  {$ifndef NOCATCH}
    {$ifdef Unix}
      catch,
    {$endif}
    {$ifdef go32v2}
      catch,
    {$endif}
  {$endif NOCATCH}
{$endif FPC}
  globals,compiler;

var
  oldexit : pointer;
procedure myexit;
begin
  exitproc:=oldexit;
{ Show Runtime error if there was an error }
  if (erroraddr<>nil) then
   begin
     case exitcode of
      100:
        begin
           erroraddr:=nil;
           writeln('Error while reading file');
        end;
      101:
        begin
           erroraddr:=nil;
           writeln('Error while writing file');
        end;
      202:
        begin
           erroraddr:=nil;
           writeln('Error: Stack Overflow');
        end;
      203:
        begin
           erroraddr:=nil;
           writeln('Error: Out of memory');
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

{ Call the compiler with empty command, so it will take the parameters }
  Halt(compiler.Compile(''));
end.
{
  $Log$
  Revision 1.19  2002-11-15 01:58:53  peter
    * merged changes from 1.0.7 up to 04-11
      - -V option for generating bug report tracing
      - more tracing for option parsing
      - errors for cdecl and high()
      - win32 import stabs
      - win32 records<=8 are returned in eax:edx (turned off by default)
      - heaptrc update
      - more info for temp management in .s file with EXTDEBUG

  Revision 1.18  2002/10/30 21:45:02  peter
    * do not include catch unit when compiling with NOCATCH

  Revision 1.17  2002/10/15 18:16:44  peter
    * GDB switch is not required

  Revision 1.16  2002/08/23 13:17:59  mazen
  *** empty log message ***

  Revision 1.15  2002/07/04 20:43:01  florian
    * first x86-64 patches

  Revision 1.14  2002/05/22 19:02:16  carl
  + generic FPC_HELP_FAIL
  + generic FPC_HELP_DESTRUCTOR instated (original from Pierre)
  + generic FPC_DISPOSE_CLASS
  + TEST_GENERIC define

  Revision 1.13  2002/05/18 13:34:13  peter
    * readded missing revisions

  Revision 1.12  2002/05/16 19:46:43  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.10  2002/03/24 19:06:29  carl
  + patch for SPARC from Mazen NEIFER

}
