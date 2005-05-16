{
    $Id: initc.pp,v 1.2 2005/02/14 17:13:30 peter Exp $
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

type libcint   = longint;
     plibcint = ^libcint;

function fpgetCerrno:libcint;
procedure fpsetCerrno(err:libcint);

{$ifdef HASGLOBALPROPERTY}
property cerrno:libcint read fpgetCerrno write fpsetcerrno;
{$endif HASGLOBALPROPERTY}

implementation

const clib = 'libc';

function geterrnolocation: Plibcint; cdecl;external clib name '___errno';

function fpgetCerrno:libcint;

begin
  fpgetCerrno:=geterrnolocation^;
end;

procedure fpsetCerrno(err:libcint);
begin
  geterrnolocation^:=err;
end;


end.
{
  $Log: initc.pp,v $
  Revision 1.2  2005/02/14 17:13:30  peter
    * truncate log

}
