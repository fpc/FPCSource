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

const clib = 'c';

type libcint=longint;
     plibcint=^libcint;

function geterrnolocation: Plibcint; cdecl;external clib name'__errno_location';

function geterrno:libcint; [public, alias: 'FPC_SYS_GETERRNO'];

begin
 geterrno:=geterrnolocation^;
end;

procedure seterrno(err:libcint); [public, alias: 'FPC_SYS_SETERRNO'];
begin
  geterrnolocation^:=err;
end;

{ OS dependant parts  }

{$I errno.inc}                          // error numbers
{$I bunxtype.inc}                       // c-types, unix base types, unix
                                        //    base structures

{*****************************************************************************
                 Extra cdecl declarations for FPC_USE_LIBC for this os
*****************************************************************************}

Function fpReadLink(name,linkname:pchar;maxlen:cint):cint;  cdecl; external name 'readlink';
function fpgetcwd(buf:pchar;_size:size_t):pchar; cdecl; external name 'getcwd';

{$I ossysc.inc}                         // base syscalls
{$I osmain.inc}                         // base wrappers *nix RTL (derivatives)

function do_isdevice(handle:longint):boolean;
begin
  do_isdevice:= (handle=StdInputHandle) or
                (handle=StdOutputHandle) or
                (handle=StdErrorHandle);
end;

{
   $Log$
   Revision 1.1  2005-02-07 22:17:26  peter
     * updated for 1.9.x unix rtl

   Revision 1.2  2005/02/06 13:06:20  peter
     * moved file and dir functions to sysfile/sysdir
     * win32 thread in systemunit

   Revision 1.1  2005/02/06 11:20:52  peter
     * threading in system unit
     * removed systhrds unit

}

