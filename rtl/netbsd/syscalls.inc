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



{
  $Log$
  Revision 1.8  2004-01-04 01:11:28  marco
   * a new qod port of the freebsd rtl. To be refined in the coming days.

  Revision 1.7  2003/10/17 20:57:10  olle
    * Changed m68k to cpum68k, i386 to cpui386

  Revision 1.6  2003/08/21 22:22:11  olle
    - removed parameter from fpc_iocheck

  Revision 1.5  2003/05/30 19:58:40  marco
   * Getting NetBSD/i386 to compile.

  Revision 1.3  2003/01/21 15:39:45  marco
   * NetBSD first rtl. Still not 100%, but close

  Revision 1.2  2003/01/17 22:13:47  marco
   * some updates

  Revision 1.1.2.4  2002/09/20 07:04:44  pierre
  * avoid compiler warning

  Revision 1.1.2.3  2002/01/23 09:10:05  marco
   * Similar truncate test. truncate tests from testsuite now seem to pass

  Revision 1.1.2.2  2001/08/29 09:43:07  marco
   * first lseek fix. Still not 100%, but don't know exact problem yet.

  Revision 1.1.2.1  2001/08/14 20:22:53  pierre
   New file

  Revision 1.1.2.7  2001/04/04 10:38:36  marco
   * Small fix to readdir. Is this *the* bug?

  Revision 1.1.2.6  2001/03/14 17:19:10  marco
   * Readdir compiles (some conflicts in Linux). Untested. Use NewReaddir to enable

  Revision 1.1.2.5  2001/03/12 20:37:50  marco
   * [Solaris] Now cycles for FreeBSD (wrong version Linux unit commited)

  Revision 1.1.2.4  2001/03/12 14:57:38  marco
   * [solaris] added some sys functions to decrease amount of ifdefs needed

  Revision 1.1.2.3  2000/09/19 09:58:49  marco
   * Mkdir fix

  Revision 1.1.2.2  2000/09/18 12:14:41  marco
   * An addw in the do_syscall(integer) caused warnings. Fixed

  Revision 1.1.2.1  2000/09/16 11:19:08  marco
   * Moved files from BSD to FreeBSD directory, with some small changes

  Revision 1.1.2.1  2000/09/10 16:12:14  marco
  Initial signals, sockets and clone

  Revision 1.1  2000/07/13 06:30:32  michael
  + Initial import

  Revision 1.15  2000/04/16 16:08:53  marco
   * Fixes (mainly opendir/Readdir/closedir)

  Revision 1.14  2000/04/14 17:04:13  marco
   * Working!

  Revision 1.13  2000/04/10 15:46:52  marco
   * worked all day. probably a lot changed

  Revision 1.11  2000/04/05 13:58:40  marco
   * syscall variablenames reintroduced.

  Revision 1.10  2000/03/16 16:18:12  marco
   * Last changes before next test. ppc386 -h works with these srcs.

  Revision 1.9  2000/03/02 15:34:07  marco
   * added a syscall for 5 longints

  Revision 1.8  2000/03/01 20:03:57  marco
   * small fixes for syslinux

  Revision 1.7  2000/03/01 17:28:40  marco
   * some changes due to updating linux.pp to new syscall

  Revision 1.6  2000/02/27 23:45:39  marco
   * Redone the syscalls

  Revision 1.5  2000/02/04 16:53:26  marco
   * Finished Linux (and rest syscalls) roughly. Some things still need to be
    tested, and checked (off_t calls specially)

  Revision 1.4  2000/02/03 17:04:47  marco
   * additions fixes due to port linux

  Revision 1.3  2000/02/02 18:07:27  marco
   * Ported except for readdir which is 200 lines C code in FBSD linux
        emulator

  Revision 1.2  2000/02/02 16:35:10  marco
   * Ported more functions. Half done now.

  Revision 1.1  2000/02/02 15:41:56  marco
   * Initial BSD version. Still needs a lot of work.

}
