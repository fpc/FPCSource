{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    This file implements all the base types and limits required
    for a minimal POSIX compliant subset required to port the compiler
    to a new OS.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifdef FPC_USE_LIBC}

const clib = 'c';

type libcint=longint;
     plibcint=^libcint;

{$ifdef FreeBSD} // tested on x86
function geterrnolocation: Plibcint; cdecl;external clib name '__error';
{$else}
{$ifdef NetBSD} // from a sparc dump.
function geterrnolocation: Plibcint; cdecl;external clib name '__errno';
{$else}
{$ifdef Darwin}
function geterrnolocation: Plibcint; cdecl;external clib name '__error';
{$else}
{$ifdef OpenBSD}

var libcerrno : libcint; cvar;

function geterrnolocation: Plibcint; cdecl;

begin
 geterrnolocation:=@libcerrno;
end;

{$else}
{$endif}
{$endif}
{$endif}
{$endif}

function geterrno:libcint; [public, alias: 'FPC_SYS_GETERRNO'];

begin
 geterrno:=geterrnolocation^;
end;

procedure seterrno(err:libcint); [public, alias: 'FPC_SYS_SETERRNO'];
begin
  geterrnolocation^:=err;
end;

{$else}
{$ifdef ver1_0}
Var
{$else}
threadvar
{$endif}
      Errno : longint;

function geterrno:longint; [public, alias: 'FPC_SYS_GETERRNO'];

begin
 GetErrno:=Errno;
end;

procedure seterrno(err:longint); [public, alias: 'FPC_SYS_SETERRNO'];

begin
 Errno:=err;
end;
{$endif}

{ OS dependant parts  }

{$I errno.inc}
{$I bunxtype.inc}
{$I ossysc.inc}
{$I osmain.inc}



{
   $Log$
   Revision 1.1  2005-02-06 12:16:52  peter
     * bsd thread updates

}

