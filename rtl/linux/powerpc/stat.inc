{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Jonas Maebe,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

  { note: NO packed record, C also inserts padding after the nlink field! }
  Stat = Record  // No unix typing because of differences
    case byte of   
      0: (st_dev    : cardinal;
          st_ino,
          st_mode   : cardinal;
          nlink     : word;
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
      1: (
          dev     : cardinal;
          pad1    : word;
          ino     : longint;
          mode,
          nlink_dummy,
          uid_dummy,
          gid_dummy     : word;
          rdev_dummy    : dev_t;
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

{
  $Log$
  Revision 1.1  2003-05-15 22:50:51  jonas
    * the stat type is processor-dependent
    * the dev_t tpye is processor dependent. Don't use it in the stat type
      however, as that one is also used at a time where dev_t is already
      defined as qword

}
