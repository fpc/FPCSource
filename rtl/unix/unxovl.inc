{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Marco van de Voort

    Some generic overloads for stringfunctions in the unix unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Function fpStatFS(Path:ansistring;Info:PStatfs):cint;

begin
  fpstatfs:=fpstatfs(pchar(path),info);
end;

