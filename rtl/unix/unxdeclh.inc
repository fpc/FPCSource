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

Function fStatFS(Fd:Longint;Var Info:tstatfs):cint; cdecl; external clib name 'fstatfs';
Function fpFlock (fd,mode : longint) : cint; cdecl; external clib name 'flock';
Function fsync (fd : cint) : cint; cdecl; external clib name 'fsync';
Function StatFS  (Path:pchar;Var Info:tstatfs):cint; cdecl; external clib name 'statfs';
function pipe (var fildes: filedesarray):cint;  cdecl; external clib name 'pipe';

{
   $Log$
   Revision 1.5  2004-11-14 12:21:08  marco
    * moved some calls from unix to baseunix. Darwin untested.

   Revision 1.4  2004/04/23 19:16:25  marco
    * flock -> fpflock because of conflicting structure name

   Revision 1.3  2004/01/04 21:05:01  jonas
     * declare C-library routines as external in libc so we generate proper
       import entries for Darwin

   Revision 1.2  2004/01/01 14:09:14  marco
    * FPC_USE_LIBC fixes

   Revision 1.1  2003/12/30 12:29:16  marco
    * FPC_USE_LIBC

   Revision 1.1  2003/06/01 15:23:46  marco
    * Shared for now

   Revision 1.1  2002/12/18 16:43:26  marco
    * new unix rtl, linux part.....


}