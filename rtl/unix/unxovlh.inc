{
    $Id: unxovlh.inc,v 1.5 2005/02/14 17:13:31 peter Exp $
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
   $Log: unxovlh.inc,v $
   Revision 1.5  2005/02/14 17:13:31  peter
     * truncate log

}
