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
Function StatFS(Path:Pathstr;Var Info:Tstatfs):cint;


{
   $Log$
   Revision 1.1  2003-12-30 12:27:44  marco
    * FPC_USE_LIBC

   Revision 1.1  2003/06/01 15:23:46  marco
    * Shared for now

   Revision 1.1  2002/12/18 16:43:26  marco
    * new unix rtl, linux part.....


}