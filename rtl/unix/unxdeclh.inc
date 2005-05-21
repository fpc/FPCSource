{
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

Function fStatFS(Fd:Longint;Var Info:tstatfs):cint; cdecl; external clib name 'fstatfs';
Function fpFlock (fd,mode : longint) : cint; cdecl; external clib name 'flock';
Function fsync (fd : cint) : cint; cdecl; external clib name 'fsync';
Function StatFS  (Path:pchar;Var Info:tstatfs):cint; cdecl; external clib name 'statfs';
function pipe (var fildes: filedesarray):cint;  cdecl; external clib name 'pipe';
function fpgettimeofday(tp: ptimeval;tzp:ptimezone):cint;   cdecl; external clib name 'gettimeofday';
{
   $Log: unxdeclh.inc,v $
   Revision 1.7  2005/02/14 17:13:31  peter
     * truncate log

}
