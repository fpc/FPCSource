{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


Uses
{$ifdef ver1_0}
  Linux
{$else}
  baseUnix
{$endif}
  ;

{ Declared EXPLICITLY with Ansistring, so NO mistaking is possible }

Function Getenv (Var EnvVar  : AnsiString): AnsiString;

Var P : Pchar;

begin
   // Linux version returns pchar.
   p:={$ifdef ver1_0}Linux.getenv{$else}BaseUnix.fpgetenv{$endif}(EnvVar);
   if P<>nil then
     getenv:=strpas(p)
   else
     getenv:='';
end;

{
  $Log$
  Revision 1.7  2003-09-20 20:23:08  marco
   * fix

  Revision 1.6  2003/09/20 12:38:29  marco
   * FCL now compiles for FreeBSD with new 1.1. Now Linux.

  Revision 1.5  2002/09/07 15:15:29  peter
    * old logs removed and tabs fixed

}
