{
    $Id$
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

uses
  sysutils;

{$ifdef has_signal}
{$ifdef unix}
Procedure CatchSignal(Sig : Longint);cdecl;
{$else}
Function CatchSignal(Sig : longint):longint;
{$endif}
begin
  case Sig of
    SIGINT :
      raise Exception.Create('Ctrl-C Signaled!');
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
  $Log$
  Revision 1.21  2005-01-26 16:23:28  peter
    * detect arithmetic overflows for constants at compile time
    * use try..except instead of setjmp

  Revision 1.20  2004/10/15 09:14:16  mazen
  - remove $IFDEF DELPHI and related code
  - remove $IFDEF FPCPROCVAR and related code

  Revision 1.19  2004/09/09 08:19:47  olle
    + Added argument to Stop

  Revision 1.18  2004/06/20 08:55:28  florian
    * logs truncated

}
