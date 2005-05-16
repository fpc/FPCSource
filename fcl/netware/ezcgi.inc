{
    $Id: ezcgi.inc,v 1.2 2005/02/14 17:13:15 peter Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}



{ Declared EXPLICITLY with Ansistring, so NO mistaking is possible }

FUNCTION _getenv (name : pchar) : pchar; cdecl; external 'clib' name 'getenv';

Function Getenv (Var EnvVar  : AnsiString): AnsiString;

Var P : Pchar;

begin
  P := _getenv (pchar(EnvVar));
  if p = nil then
    GetEnv := ''
  else
    GetEnv := strpas (P);
end;

{
  $Log: ezcgi.inc,v $
  Revision 1.2  2005/02/14 17:13:15  peter
    * truncate log

}
