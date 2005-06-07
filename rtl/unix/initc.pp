{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt and Peter Vreman,
    members of the Free Pascal development team

    This file links to libc, and handles the libc errno abstraction.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit initc;
interface
{$linklib c}

type libcint   = longint;
     plibcint = ^libcint;

function fpgetCerrno:libcint;
procedure fpsetCerrno(err:libcint);

property cerrno:libcint read fpgetCerrno write fpsetcerrno;

const clib = 'c';

implementation
// hasn't been divided up in .inc's, because I first want to see hoe
// this idea works out.

{$ifdef OpenBSD}
{$define UseOldErrnoDirectLink}
{$endif}

{$ifdef UseOldErrnoDirectLink}
Var
  interrno : libcint;external name {$ifdef OpenBSD} '_errno' {$else} 'h_errno'{$endif};

function fpgetCerrno:libcint;

begin
  fpgetCerrno:=interrno;
end;

procedure fpsetCerrno(err:libcint);
begin
  interrno:=err;
end;
{$else}


{$ifdef Linux}
function geterrnolocation: Plibcint; cdecl;external clib name '__errno_location';
{$else}
{$ifdef FreeBSD} // tested on x86
function geterrnolocation: Plibcint; cdecl;external clib name '__error';
{$else}
{$ifdef NetBSD} // from a sparc dump.
function geterrnolocation: Plibcint; cdecl;external clib name '__errno';
{$else}
{$ifdef Darwin}
function geterrnolocation: Plibcint; cdecl;external clib name '__error';
{$endif}
{$endif}
{$endif}
{$endif}

function fpgetCerrno:libcint;

begin
  fpgetCerrno:=geterrnolocation^;
end;

procedure fpsetCerrno(err:libcint);
begin
  geterrnolocation^:=err;
end;

{$endif}

end.
