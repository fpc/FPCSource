{
    This file is part of the Free Pascal run time library.
    This unit implements cygwin initialization
    Copyright (c) 1999-2006 by the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit initc;

interface

type
 libcint   = longint;
 plibcint = ^libcint;

function fpgetCerrno:libcint;
procedure fpsetCerrno(err:libcint);

property cerrno:libcint read fpgetCerrno write fpsetcerrno;


implementation

function geterrnolocation: Plibcint; cdecl;external 'cygwin1.dll' name '__errno';

function fpgetCerrno:libcint;
begin
  fpgetCerrno:=geterrnolocation^;
end;

procedure fpsetCerrno(err:libcint);
begin
  geterrnolocation^:=err;
end;

end.
