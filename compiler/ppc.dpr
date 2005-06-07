{
    $Id: ppc.dpr,v 1.6 2002/10/05 12:43:27 carl Exp $
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

 ****************************************************************************
}
program ppc;

{$MINSTACKSIZE $00004000}
{$MAXSTACKSIZE $00100000}
{$IMAGEBASE $00400000}
{$APPTYPE CONSOLE}

{
  possible compiler switches (* marks a currently required switch):
  -----------------------------------------------------------------
  USE_RHIDE           generates errors and warning in an format recognized
                      by rhide
  GDB*                support of the GNU Debugger
  I386                generate a compiler for the Intel i386+
  M68K                generate a compiler for the M68000
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

{$i fpcdefs.inc}

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

uses
{$ifdef FPC}
  {$ifdef profile}
    profile,
  {$endif profile}
  {$ifdef heaptrc}
    ppheap,
  {$endif heaptrc}
  {$ifdef Unix}
    catch,
  {$endif}
  {$ifdef go32v2}
    {$ifdef DEBUG}
      {$define NOCATCH}
    {$endif DEBUG}
    catch,
  {$endif}
  { we've now a lineinfo unit for all OSes }
  {$ifdef DEBUG}
    lineinfo,
  {$endif DEBUG}
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
