{
    $Id: catch.pas,v 1.25 2005/04/24 21:21:10 peter Exp $
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
{$ifdef unix}
 {$ifndef beos}
  {$define has_signal}
  {$ifdef havelinuxrtl10}
    Linux,
  {$else}
    BaseUnix,Unix,
  {$endif}
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

Const in_const_evaluation : boolean = false;

Implementation

{$IFNDEF MACOS_USE_FAKE_SYSUTILS}
uses
  comphook;
{$ENDIF MACOS_USE_FAKE_SYSUTILS}

{$ifdef has_signal}
{$ifdef unix}
Procedure CatchSignal(Sig : Longint);cdecl;
{$else}
Function CatchSignal(Sig : longint):longint;
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
    OldSigInt:={$ifdef havelinuxrtl10}Signal{$else}{$ifdef Unix}fpSignal{$else}Signal{$endif}{$endif}  (SIGINT,NewSignal);
  {$endif}
{$endif nocatch}
end.

{
  $Log: catch.pas,v $
  Revision 1.25  2005/04/24 21:21:10  peter
    * use comphook for fpc exceptions

  Revision 1.24  2005/02/15 19:15:45  peter
    * Handle Control-C exception more cleanly

  Revision 1.23  2005/02/14 17:13:06  peter
    * truncate log

  Revision 1.22  2005/01/31 21:30:56  olle
    + Added fake Exception classes, only for MACOS.

  Revision 1.21  2005/01/26 16:23:28  peter
    * detect arithmetic overflows for constants at compile time
    * use try..except instead of setjmp

}
