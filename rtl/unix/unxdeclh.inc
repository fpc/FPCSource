{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    cdecl; definitions needed for unix unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ***********************************************************************}

type filedesarray=array[0..1] of cint;

function fpMUnMap (P : Pointer; Size : size_t) : cint; cdecl; external name 'munmap';
Function fStatFS(Fd:Longint;Var Info:tstatfs):cint; cdecl; external name 'fstatfs';
Function Flock (fd,mode : longint) : cint; cdecl; external name 'flock';
Function fsync (fd : cint) : cint; cdecl; external name 'fsync';
Function StatFS  (Path:pchar;Var Info:tstatfs):cint; cdecl; external name 'statfs';
function pipe (var fildes: filedesarray):cint;  cdecl; external name 'pipe';
function fpNice(N:cint):cint; cdecl; external name 'nice';
Function fpGetPriority(Which,Who:cint):cint;      cdecl; external name 'getpriority';
Function fpSetPriority(Which,Who,What:cint):cint; cdecl; external name 'setpriority';
Function fpLstat(path:pchar;Info:pstat):cint;     cdecl; external name 'lstat';
Function fpSymlink(oldname,newname:pchar):cint;   cdecl; external name 'symlink';
Function fpReadLink(name,linkname:pchar;maxlen:cint):cint;  cdecl; external name 'readlink';
function fpgettimeofday(tp: ptimeval;tzp:ptimezone):cint;   cdecl; external name 'gettimeofday';

{
   $Log$
   Revision 1.2  2004-01-01 14:09:14  marco
    * FPC_USE_LIBC fixes

   Revision 1.1  2003/12/30 12:29:16  marco
    * FPC_USE_LIBC

   Revision 1.1  2003/06/01 15:23:46  marco
    * Shared for now

   Revision 1.1  2002/12/18 16:43:26  marco
    * new unix rtl, linux part.....


}