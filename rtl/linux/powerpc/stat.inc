{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Jonas Maebe,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

  Stat = packed Record  // No unix typing because of differences
    case byte of
      0: (st_dev    : cardinal;
          st_ino,
          st_mode   : cardinal;
          nlink     : word;
          __pad1    : word;
          uid,
          gid,
          rdev      : cardinal;
          st_size   : longint;
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
      1: (dev    : cardinal;
          ino,
          mode   : cardinal;
          nlink_dummy     : word;
          __pad2    : word;
          uid_dummy,
          gid_dummy,
          rdev_dummy      : cardinal;
          size   : longint;
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

