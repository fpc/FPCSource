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

  Stat = Packed Record  // No unix typing because of differences
                              // kernel <->libc
    case byte of
      0: (st_dev     : word;
          pad1       : word;
          st_ino     : longint;
          st_mode,
          nlink,
          uid,
          gid        : word;
          rdev       : word;
          pad2       : word;
          st_size,
          st_blksze,
          st_blocks,
          st_atime,
          unused1,
          st_mtime,
          unused2,
          st_ctime,
          unused3,
          unused4,
          unused5   : longint;);
      1: (
          dev     : word;
          pad1_dummy    : word;
          ino     : longint;
          mode,
          nlink_dummy,
          uid_dummy,
          gid_dummy     : word;
          rdev_dummy    : word;
          pad2_dummy    : word;
          size,
          blksze,
          blocks,
          atime,
          unused1_dummy,
          mtime,
          unused2_dummy,
          ctime,
          unused3_dummy,
          unused4_dummy,
          unused5_dummy : longint;
         );
  end;

