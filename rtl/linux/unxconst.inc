{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Const
  { Things for OPEN call - after linux/fcntl.h }
  Open_Accmode   = 3;
  Open_RdOnly    = 0;
  Open_WrOnly    = 1;
  Open_RdWr      = 2;
  Open_Creat     = 1 shl 6;             // %100 etc
  Open_Excl      = 2 shl 6;
  Open_NoCtty    = 4 shl 6;
  Open_Trunc     = 1 shl 9;
  Open_Append    = 2 shl 9;
  Open_NonBlock  = 4 shl 9;
  Open_NDelay    = Open_NonBlock;
  Open_Sync      = 1 shl 12;
  Open_Direct    = 4 shl 12;
  Open_LargeFile = 1 shl 15;
  Open_Directory = 2 shl 15;
  Open_NoFollow  = 4 shl 15;

  { The waitpid uses the following options:}
  Wait_NoHang   = 1;
  Wait_UnTraced = 2;
  Wait_Any      = -1;
  Wait_MyPGRP   = 0;
  Wait_Clone    = $80000000;

  { Constants to check stat.mode }
  STAT_IFMT   = $f000; {00170000}
  STAT_IFSOCK = $c000; {0140000}
  STAT_IFLNK  = $a000; {0120000}
  STAT_IFREG  = $8000; {0100000}
  STAT_IFBLK  = $6000; {0060000}
  STAT_IFDIR  = $4000; {0040000}
  STAT_IFCHR  = $2000; {0020000}
  STAT_IFIFO  = $1000; {0010000}
  STAT_ISUID  = $0800; {0004000}
  STAT_ISGID  = $0400; {0002000}
  STAT_ISVTX  = $0200; {0001000}
  { Constants to check permissions }
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

  {Constansts Termios/Ioctl (used in Do_IsDevice) }
  {$if defined(PowerPC) or defined(Powerpc64)}
  IOCtl_TCGETS=$402c7413;
  {$else}
  IOCtl_TCGETS=$5401; // TCGETS is also in termios.inc, but the sysunix needs only this
  {$endif}

