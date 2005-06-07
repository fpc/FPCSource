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
  baseUnix;

{ Declared EXPLICITLY with Ansistring, so NO mistaking is possible }

Function Getenv (Var EnvVar  : AnsiString): AnsiString;

Var P : Pchar;

begin
   // Linux version returns pchar.
   p:=BaseUnix.fpgetenv(EnvVar);
   if P<>nil then
     getenv:=strpas(p)
   else
     getenv:='';
end;

