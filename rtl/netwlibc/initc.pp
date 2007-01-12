{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2004 by the Free Pascal development team.

    This file handles the libc errno abstraction.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit initc;

interface
uses
  ctypes;

function fpgetCerrno:cint;
procedure fpsetCerrno(err:cint);

property cerrno:cint read fpgetCerrno write fpsetcerrno;

implementation

const clib = 'libc';

function geterrnolocation: pcint; cdecl;external clib name '___errno';

function fpgetCerrno:cint;

begin
  fpgetCerrno:=geterrnolocation^;
end;

procedure fpsetCerrno(err:cint);
begin
  geterrnolocation^:=err;
end;


end.
