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

{BSD version, only the blocks with BSD in the comment are updated}

Const
  { Things for LSEEK call, same in linux and BSD }
  Seek_set = 0;
  Seek_Cur = 1;
  Seek_End = 2;
  { Things for OPEN call - after include/sys/fcntl.h, BSD updated.
   BSD specifies these constants in hex }
  Open_Accmode  = 3;
  Open_RdOnly   = 0;
  Open_WrOnly   = 1;
  Open_RdWr     = 2;
  Open_NonBlock = 4;
  Open_Append   = 8;
  Open_ShLock   = $10;
  Open_ExLock   = $20;
  Open_ASync    = $40;
  Open_FSync    = $80;
  Open_NoFollow = $100;
  Open_Create   = $200;       {BSD convention}
  Open_Creat    = $200;       {Linux convention}
  Open_Trunc    = $400;
  Open_Excl     = $800;
  Open_NOCTTY   = $8000;

   { The waitpid uses the following options:}
  Wait_NoHang   = 1;
  Wait_UnTraced = 2;
  Wait_Any      = -1; 
  Wait_MyPGRP   = 0;  
  { Constants to check stat.mode -  checked all STAT constants with BSD}
  STAT_IFMT   = $f000; {00170000 }
  STAT_IFSOCK = $c000; {0140000 }
  STAT_IFLNK  = $a000; {0120000 }
  STAT_IFREG  = $8000; {0100000 }
  STAT_IFBLK  = $6000; {0060000 }
  STAT_IFDIR  = $4000; {0040000 }
  STAT_IFCHR  = $2000; {0020000 }
  STAT_IFIFO  = $1000; {0010000 }
  STAT_ISUID  = $0800; {0004000 }
  STAT_ISGID  = $0400; {0002000 }
  STAT_ISVTX  = $0200; {0001000} 
  { Constants to check permissions all }
  STAT_IRWXO = $7;         
  STAT_IROTH = $4;         
  STAT_IWOTH = $2;         
  STAT_IXOTH = $1;         

  STAT_IRWXG = STAT_IRWXO shl 3;
  STAT_IRGRP = STAT_IROTH shl 3;
  STAT_IWGRP = STAT_IWOTH shl 3;
  STAT_IXGRP = STAT_IXOTH shl 3;

  STAT_IRWXU = STAT_IRWXO shl 6;
  STAT_IRUSR = STAT_IROTH shl 6;
  STAT_IWUSR = STAT_IWOTH shl 6;
  STAT_IXUSR = STAT_IXOTH shl 6;

  { Constants to test the type of filesystem }
  fs_old_ext2 = $ef51;
  fs_ext2     = $ef53;
  fs_ext      = $137d;
  fs_iso      = $9660;
  fs_minix    = $137f;
  fs_minix_30 = $138f;
  fs_minux_V2 = $2468;
  fs_msdos    = $4d44;
  fs_nfs      = $6969;
  fs_proc     = $9fa0;
  fs_xia      = $012FD16D;

  { Constansts for MMAP }
  MAP_PRIVATE   =2;
  MAP_ANONYMOUS =$1000;

  {Constansts Termios/Ioctl (used in Do_IsDevice) }
  IOCtl_TCGETS=$5401; // TCGETS is also in termios.inc, but the sysunix needs only this

{
  $Log$
  Revision 1.2  2003-01-17 22:13:47  marco
   * some updates

  Revision 1.1.2.1  2001/08/10 11:07:17  pierre
   New NetBSD files taken and adapted from FreeBSD

  Revision 1.1.2.3  2001/03/12 14:58:37  marco
   * [Solaris] some constants moved from sysunix.inc

  Revision 1.1.2.2  2000/11/03 14:58:52  marco
   * Open_NoCTTY added. Needed for serial.pp

  Revision 1.1.2.1  2000/09/16 11:19:08  marco
   * Moved files from BSD to FreeBSD directory, with some small changes

  Revision 1.1  2000/07/13 06:30:33  michael
  + Initial import

  Revision 1.5  2000/04/10 15:46:52  marco
   * worked all day. probably a lot changed

  Revision 1.3  2000/02/03 17:05:13  marco
   * fixes and updates due to linux unit porting

  Revision 1.2  2000/02/02 16:35:29  marco
   * Checked stat constants

  Revision 1.1  2000/02/02 15:41:28  marco
   * initial BSD version.

  Revision 1.4  2000/01/07 16:41:41  daniel
    * copyright 2000

  Revision 1.3  2000/01/07 16:32:28  daniel
    * copyright 2000 added

  Revision 1.2  1998/05/06 12:38:22  michael
  + Removed log from before restored version.

  Revision 1.1.1.1  1998/03/25 11:18:43  root
  * Restored version
}
