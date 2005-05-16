{
    $Id: pp.pas,v 1.35 2005/02/14 17:13:07 peter Exp $
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
  CMEM                use cmem unit for better memory debugging
  I386                generate a compiler for the Intel i386+
  x86_64              generate a compiler for the AMD x86-64 architecture
  M68K                generate a compiler for the M68000
  SPARC               generate a compiler for SPARC
  POWERPC             generate a compiler for the PowerPC
  VIS                 generate a compile for the VIS
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
  cpuflags            The target processor has status flags (on by default)
  cpufpemu            The target compiler will also support emitting software
                       floating point operations
  cpu64bit            The target is a 64-bit processor
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
   {$ifdef vis}
     {$ifdef CPUDEFINED}
        {$fatal ONLY one of the switches for the CPU type must be defined}
     {$endif CPUDEFINED}
     {$define CPUDEFINED}
   {$endif}
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
   {$ifdef ARM}
     {$ifdef CPUDEFINED}
        {$fatal ONLY one of the switches for the CPU type must be defined}
     {$endif CPUDEFINED}
     {$define CPUDEFINED}
   {$endif ARM}
   {$ifdef MIPS}
     {$ifdef CPUDEFINED}
        {$fatal ONLY one of the switches for the CPU type must be defined}
     {$endif CPUDEFINED}
     {$define CPUDEFINED}
   {$endif MIPS}
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
{$ifdef cmem}
  cmem,
{$endif cmem}
{$ifdef FPC}
  {$ifdef profile}
    profile,
  {$endif profile}
  {$ifndef NOCATCH}
    {$ifdef Unix}
      catch,
    {$endif}
    {$ifdef go32v2}
      catch,
    {$endif}
    {$ifdef WATCOM}
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
{$ifdef nocatch}
  exit;
{$endif nocatch}
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
{$ifdef extheaptrc}
  keepreleased:=true;
{$endif extheaptrc}
  SetFPUExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,
                        exOverflow, exUnderflow, exPrecision]);
{ Call the compiler with empty command, so it will take the parameters }
  Halt(compiler.Compile(''));
end.
{
  $Log: pp.pas,v $
  Revision 1.35  2005/02/14 17:13:07  peter
    * truncate log

  Revision 1.34  2005/02/13 18:55:19  florian
    + overflow checking for the arm

}
