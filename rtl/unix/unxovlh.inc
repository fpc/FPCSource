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

Function PClose(Var F:file) : cint;
Function PClose      (Var F:text) : cint;
Function StatFS(Path:ansistring;Var Info:Tstatfs):cint;
//Function fpReadLink(Name:ansistring):ansistring;

{
   $Log$
   Revision 1.4  2004-11-14 12:21:08  marco
    * moved some calls from unix to baseunix. Darwin untested.

   Revision 1.3  2004/11/03 15:00:43  marco
    * Pathstr eliminated

   Revision 1.2  2004/01/01 16:10:23  marco
    * fpreadlink(pathstr) moved to unxovl (since not platform specific),
   	small fixes for "make all OPT='-dFPC_USE_LIBC'

   Revision 1.1  2003/12/30 12:27:44  marco
    * FPC_USE_LIBC

   Revision 1.1  2003/06/01 15:23:46  marco
    * Shared for now

   Revision 1.1  2002/12/18 16:43:26  marco
    * new unix rtl, linux part.....


}