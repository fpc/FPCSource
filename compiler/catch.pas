{
    $Id$
    Copyright (c) 1998-2000 by Michael Van Canneyt

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

{$ifdef go32v2}
  { go32v2 stack check goes nuts if ss is not the data selector (PM) }
  {$S-}
{$endif}

interface
uses
{$ifdef linux}
{$define has_signal}
  linux,
{$endif}
{$ifdef go32v2}
{$define has_signal}
  dpmiexcp,
{$endif}
  verbose;


{$ifdef has_signal}
Var
  NewSignal,OldSigSegm,
  OldSigInt,OldSigFPE : SignalHandler;
{$endif}

Const in_const_evaluation : boolean = false;

Implementation

{$ifdef has_signal}
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
    SIGFPE : begin
               If in_const_evaluation then
                 Writeln('FPE error computing constant expression')
               else
                 Writeln('FPE error inside compiler');
               Stop;
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
{$endif def has_signal}


begin
{$ifndef nocatch}
{$ifdef has_signal}
{$ifndef TP}
  NewSignal:=SignalHandler(@CatchSignal);
{$else TP}
  NewSignal:=SignalHandler(CatchSignal);
{$endif TP}
  OldSigSegm:=Signal (SIGSEGV,NewSignal);
  OldSigInt:=Signal (SIGINT,NewSignal);
  OldSigFPE:=Signal (SIGFPE,NewSignal);
{$endif}
{$endif nocatch}
end.

{
  $Log$
  Revision 1.10  2000-01-07 01:14:20  peter
    * updated copyright to 2000

  Revision 1.9  1999/08/25 16:41:04  peter
    * resources are working again

  Revision 1.8  1999/08/10 12:27:15  pierre
   * not stack check inside catch !!

  Revision 1.7  1999/07/05 12:13:22  florian
    * property reading from PPU fixed (new PPU format), it uses now writesym...

  Revision 1.6  1999/06/02 22:44:05  pierre
   * previous wrong log corrected

  Revision 1.5  1999/06/02 22:25:28  pierre
  * added SIGFPE

  Revision 1.4  1999/01/28 19:42:03  peter
    * mssing endif added

  Revision 1.3  1999/01/27 13:20:37  pierre
   * slightly rewritten code

  Revision 1.2  1998/09/08 13:01:09  michael
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
