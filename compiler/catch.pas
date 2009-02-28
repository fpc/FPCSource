{
    Copyright (c) 1998-2002 by Michael Van Canneyt

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

{$i fpcdefs.inc}

{$ifdef DEBUG}
  {$define NOCATCH}
{$endif DEBUG}

interface
uses
{ you cannot safely raise an exception inside a signal handler on any OS,
  and on darwin this even often crashes
}
{$if defined(unix) and not defined(darwin) }
 {$ifndef darwin}
  {$define has_signal}
  BaseUnix,Unix,
 {$endif}
{$endif}
{$ifdef go32v2}
{$define has_signal}
  dpmiexcp,
{$endif}
{$ifdef watcom}
  {$define has_signal}
  dpmiexcp,
{$endif}
  verbose;

{$ifdef has_signal}
Var
  NewSignal,
  OldSigInt : SignalHandler;
{$endif}

Implementation

uses
  comphook;

{$ifdef has_signal}
{$ifdef unix}
Procedure CatchSignal(Sig : Longint);cdecl;
{$else}
Function CatchSignal(Sig : longint):longint; cdecl;
{$endif}
begin
  case Sig of
    SIGINT :
      raise EControlCAbort.Create;
  end;
{$ifndef unix}
  CatchSignal:=0;
{$endif}
end;
{$endif def has_signal}

begin
{$ifndef nocatch}
  {$ifdef has_signal}
    NewSignal:=SignalHandler(@CatchSignal);
    OldSigInt:={$ifdef Unix}fpSignal{$else}Signal{$endif}(SIGINT,NewSignal);
  {$endif}
{$endif nocatch}
end.
