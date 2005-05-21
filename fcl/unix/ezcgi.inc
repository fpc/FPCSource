{
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
  $Log: ezcgi.inc,v $
  Revision 1.8  2005/02/14 17:13:18  peter
    * truncate log

}
