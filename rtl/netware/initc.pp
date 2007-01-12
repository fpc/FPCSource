{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004 the Free Pascal development team

    This file handles the clib errno abstraction for netware.

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
{$i nwsys.inc}

function fpgetCerrno:cint;
procedure fpsetCerrno(err:cint);

property cerrno:cint read fpgetCerrno write fpsetcerrno;

implementation

function fpgetCerrno:cint;
begin
  fpgetCerrno:=__get_errno_ptr^;
end;

procedure fpsetCerrno(err:cint);
begin
  __get_errno_ptr^:=err;
end;


end.
