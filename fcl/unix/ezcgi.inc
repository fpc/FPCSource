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


Uses Unix;

{ Declared EXPLICITLY with Ansistring, so NO mistaking is possible }

Function Getenv (Var EnvVar  : AnsiString): AnsiString;

Var P : Pchar;

begin
   // Linux version returns pchar.
   p:=Unix.getenv(EnvVar);
   if P<>nil then
     getenv:=strpas(p)
   else
     getenv:='';
end;

{
  $Log$
  Revision 1.3  2001-01-21 20:45:09  marco
   * Rename fest II  FCL version.

  Revision 1.2  2000/10/26 22:30:21  peter
    * freebsd update
    * classes.rst

  Revision 1.1.2.1  2000/10/25 12:36:24  marco
   * Split up Freebsd/Unix/Linux for Fixes FCL

  Revision 1.1  2000/07/13 06:31:32  michael
  + Initial import

  Revision 1.7  2000/02/15 22:03:38  sg
  * Inserted wrong copyright notice ;)  Fixed.

  Revision 1.6  2000/02/15 21:57:51  sg
  * Added copyright notice and CVS log tags where necessary

}
