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
type

  TStatfs = packed record
    spare2,	       { place holder}
    bsize,	       { fundamental block size}
    iosize,            { optimal block size }
    blocks,            {  total blocks}
    bfree,             { blocks free}
    bavail,            { block available for mortal users}
    files,	       { Total file nodes}
    ffree	   : longint;    { file nodes free}
    fsid	   : array[0..1] of longint;
    fowner	   : longint; {mounter uid}
    ftype	   : longint;
    fflags         : longint; {copy of mount flags}
    spare          : array [0..1] of longint; { For later use }
    fstypename     : array[0..15] of char;
    mountpoint     : array[0..89] of char;
    mnfromname     : array[0..89] of char;
  end;
  PStatFS=^TStatFS;


{
  $Log$
  Revision 1.4  2004-01-04 01:11:28  marco
   * a new qod port of the freebsd rtl. To be refined in the coming days.

  Revision 1.3  2003/01/21 15:39:45  marco
   * NetBSD first rtl. Still not 100%, but close

  Revision 1.2  2003/01/17 22:13:47  marco
   * some updates

  Revision 1.1.2.2  2001/08/30 23:00:18  marco
   * Fix for stat record.

  Revision 1.1.2.1  2001/08/10 11:07:17  pierre
   New NetBSD files taken and adapted from FreeBSD

  Revision 1.1.2.5  2001/03/12 15:01:39  marco
   * [Solaris] introduced dev_t

  Revision 1.1.2.4  2001/01/20 16:54:51  marco
   * Fixed timeval. Must have thought that long=int64 when I did initial port?

  Revision 1.1.2.3  2000/10/30 14:36:07  marco
   * timespec from int64 to longint.

  Revision 1.1.2.2  2000/10/24 12:14:37  pierre
   + timespec type for syscall_nr_nanosleep

  Revision 1.1.2.1  2000/09/16 11:19:08  marco
   * Moved files from BSD to FreeBSD directory, with some small changes

  Revision 1.1.2.1  2000/09/10 16:12:14  marco
  Initial signals, sockets and clone

  Revision 1.1  2000/07/13 06:30:33  michael
  + Initial import

  Revision 1.8  2000/04/16 16:08:30  marco
   * Updated PDir to BSD libc layout. (which is totally different from Linux)

  Revision 1.7  2000/04/10 15:46:52  marco
   * worked all day. probably a lot changed

  Revision 1.5  2000/03/17 12:58:57  marco
   * some changes to ftruncate based procs. Added a "0" as extra parameter

  Revision 1.4  2000/02/04 16:55:47  marco
   * Fixed tdir, some params need to be 32-bit

  Revision 1.3  2000/02/03 17:05:55  marco

   * Some types fixed/ported.

  Revision 1.2  2000/02/02 16:45:38  marco
   * Typo in STAT record

  Revision 1.1  2000/02/02 16:36:09  marco
   * Initial version. Copy of linux version, with BSD stat.

}
