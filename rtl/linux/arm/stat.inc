                                                              {
    $Id: stat.inc,v 1.2 2005/02/14 17:13:30 peter Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by Florian Klaempfl,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

  Stat = packed Record  // No unix typing because of differences
    case byte of
      0: (st_dev,
          __pad1    : word;
          st_ino    : cardinal;
          st_mode,
          nlink,
          uid,
          gid,
          rdev,
          __pad2    : word;
          st_size,
          st_blksize,
          st_blocks,
          st_atime,
          __unused1,
          st_mtime,
          __unused2,
          st_ctime,
          __unused3,
          __unused4,
          __unused5  : cardinal;);
      1: (dev,
          __pad3    : word;
          ino       : cardinal;
          mode,
          nlink_dummy,
          uid_dummy,
          gid_dummy,
          rdev_dummy,
          __pad4     : word;
          size,
          blksize,
          blocks,
          atime,
          __unused1_dummy,
          mtime,
          __unused2_dummy,
          ctime,
          __unused3_dummy,
          __unused4_dummy,
          __unused5_dummy  : cardinal;);
  end;

{
  $Log: stat.inc,v $
  Revision 1.2  2005/02/14 17:13:30  peter
    * truncate log

}
