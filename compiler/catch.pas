{
    $Id$
    Copyright (c) 1997-98 by Michael Van Canneyt

    Unit to catch segmentation faults and Ctrl-C and exit gracefully
    under linux and go32v2

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

  *********************************************************************
}
Unit catch;
interface
uses
{$ifdef linux}
  linux,
{$endif}
{$ifdef go32v2}
  dpmiexcp,
{$endif}
  verbose;

Var
  NewSignal,OldSigSegm,OldSigInt : SignalHandler;


Implementation

{$ifdef linux}
Procedure CatchSignal(Sig : Integer);cdecl;
{$else}
Function CatchSignal(Sig : longint):longint;
{$endif}
begin
  case Sig of
   SIGSEGV : begin
             { Temporary message - until we get an error number... }
               writeln ('Panic : Internal compiler error, exiting.');
               internalerror(9999);
             end;
    SIGINT : begin
               WriteLn('Ctrl-C Signaled!');
               Stop;
             end;
  end;
{$ifndef linux}
  CatchSignal:=0;
{$endif}
end;


begin
{$ifdef linux}
  NewSignal:=@CatchSignal;
  OldSigSegm:=Signal (SIGSEGV,NewSignal);
  OldSigInt:=Signal (SIGINT,NewSignal);
{$else}
  NewSignal:=SignalHandler(@CatchSignal);
  Signal (SIGSEGV,NewSignal);
  Signal (SIGINT,NewSignal);
{$endif}
end.

{
  $Log$
  Revision 1.2  1998-09-08 13:01:09  michael
  Adapted to changed Signal call

  Revision 1.1.1.1  1998/03/25 11:18:12  root
  * Restored version

  Revision 1.5  1998/03/10 01:17:15  peter
    * all files have the same header
    * messages are fully implemented, EXTDEBUG uses Comment()
    + AG... files for the Assembler generation

  Revision 1.4  1998/03/02 01:48:07  peter
    * renamed target_DOS to target_GO32V1
    + new verbose system, merged old errors and verbose units into one new
      verbose.pas, so errors.pas is obsolete

  Revision 1.3  1998/02/13 10:34:37  daniel
  * Made Motorola version compilable.
  * Fixed optimizer

  Revision 1.2  1998/01/27 23:34:35  peter
    + SIGINT capture with exit. It works for linux and go32v2 (last one
      not 100% yet)

  Revision 1.1.1.1  1997/11/27 08:32:51  michael
  FPC Compiler CVS start
}
