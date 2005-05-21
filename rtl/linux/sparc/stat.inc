{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2003 by Jonas Maebe,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

  Stat = packed Record  // No unix typing because of differences
    case byte of
      0: (st_dev    : word;
          __pad1    : word;
          st_ino    : cardinal;
          st_mode   : word;
          nlink     : smallint;
          uid       : word;
          gid       : word;
          rdev      : word;
          __pad2    : word;
          st_size   : longint;
          st_atime  : longint;
          st_atime_nsecs : cardinal;
          st_mtime  : longint;
          st_mtime_nsecs : cardinal;
          st_ctime  : longint;
          st_ctime_nsecs : cardinal;
          st_blksize : longint;
          st_blocks  : longint;
          __unused1,
          __unused2  : cardinal;
         );
      1: (dev    : word;
          __pad3 : word;
          ino    : word;
          mode   : cardinal;
          nlink_dummy  : smallint;
          uid_dummy,
          gid_dummy,
          rdev_dummy      : word;
          __pad4 : word;
          size   : longint;
          atime,
          __unused1_dummy,
          mtime,
          __unused2_dummy,
          ctime,
          __unused3_dummy,
          blksize,
          blocks            : longint;
          __unused4_dummy,
          __unused5_dummy  : cardinal;
         );
  end;

{
  $Log: stat.inc,v $
  Revision 1.4  2005/02/14 17:13:30  peter
    * truncate log

}
