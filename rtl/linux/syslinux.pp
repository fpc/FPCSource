{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ These things are set in the makefile, }
{ But you can override them here.}

{ If you use an aout system, set the conditional AOUT}
{ $Define AOUT}

Unit SysLinux;
Interface

{$I sysunixh.inc}

Implementation

{$I sysunix.inc}

{
  $Log$
  Revision 1.7  2000-09-18 13:14:50  marco
   * Global Linux +bsd to (rtl/freebsd rtl/unix rtl/linux structure)

  Revision 1.6  2000/09/11 13:48:08  marco
   * FreeBSD support and removal of old sighandler

  Revision 1.5  2000/08/13 08:43:45  peter
    * don't check for directory in do_open (merged)

  Revision 1.4  2000/08/05 18:33:51  peter
    * paramstr(0) fix for linux 2.0 kernels (merged)

  Revision 1.3  2000/07/14 10:33:10  michael
  + Conditionals fixed

  Revision 1.2  2000/07/13 11:33:49  michael
  + removed logs

}
