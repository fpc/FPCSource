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

{$ifdef BSD}
  {$define NOCATCH}
{$endif}

{$ifdef DEBUG}
  {$define NOCATCH}
{$endif DEBUG}


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
  Revision 1.14  2000-04-07 20:52:24  marco
   * For BSD signals are off for now

  Revision 1.13  2000/03/20 09:37:51  florian
    * catching of exceptions is switched off on all targets if the define
      DEBUG is used

  Revision 1.12  2000/02/18 12:34:43  pierre
   DEBUG implies NOCATCH for go32v2

  Revision 1.11  2000/02/09 13:22:45  peter
    * log truncated

  Revision 1.10  2000/01/07 01:14:20  peter
    * updated copyright to 2000

  Revision 1.9  1999/08/25 16:41:04  peter
    * resources are working again

  Revision 1.8  1999/08/10 12:27:15  pierre
   * not stack check inside catch !!

}
