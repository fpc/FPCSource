{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    Some calls for the unix unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ***********************************************************************}

{
 $Log$
 Revision 1.7  2004-11-14 12:21:08  marco
  * moved some calls from unix to baseunix. Darwin untested.

 Revision 1.6  2004/11/03 15:00:43  marco
  * Pathstr eliminated

 Revision 1.5  2004/04/22 17:17:23  peter
   * x86-64 fixes

 Revision 1.4  2004/01/01 16:10:23  marco
  * fpreadlink(pathstr) moved to unxovl (since not platform specific),
 	small fixes for "make all OPT='-dFPC_USE_LIBC'

 Revision 1.3  2003/11/13 13:11:55  marco
  * Linuxerror remove + hdr+log added


}