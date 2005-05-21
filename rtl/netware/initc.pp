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
{$i nwsys.inc}

type libcint   = longint;
     plibcint = ^libcint;

function fpgetCerrno:libcint;
procedure fpsetCerrno(err:libcint);

{$ifdef HASGLOBALPROPERTY}
property cerrno:libcint read fpgetCerrno write fpsetcerrno;
{$endif HASGLOBALPROPERTY}

implementation

function fpgetCerrno:libcint;
begin
  fpgetCerrno:=__get_errno_ptr^;
end;

procedure fpsetCerrno(err:libcint);
begin
  __get_errno_ptr^:=err;
end;


end.
