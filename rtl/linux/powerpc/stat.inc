{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Jonas Maebe, (c) 2005 Thomas Schatzl,
    members of the Free Pascal development team.

    Contains the definition of the stat type for the PowerPC platform.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ This structure was ported from 

    /usr/include/asm-ppc/stat.h

  on a 2.6.11 kernel.
}

{$PACKRECORDS C}

{$IFNDEF FPC_USE_LIBC}
  stat = record
    case byte of
      0: (
        st_dev     : cULong;   { actually dev_t, but this one is defined wrongly }
        st_ino     : ino_t;
        st_mode    : mode_t;
        st_nlink   : nlink_t;
        st_uid     : uid_t;
        st_gid     : gid_t;
        st_rdev    : cULong;   { actually dev_t, but this one is defined wrongly }
        st_size    : off_t;
        st_blksize : cULong;
        st_blocks  : cULong;
        st_atime,
        __unused1,
        st_mtime,
        __unused2,
        st_ctime,
        __unused3,
        __unused4,
        __unused5  : cULong);
      1: (
        dev    : cULong;
        ino    : ino_t;
        mode   : mode_t;
        nlink  : nlink_t;
        uid    : uid_t;
        gid    : gid_t;
        rdev   : cULong;
        size   : off_t;
        blksize,
        blocks,
        atime,
        __unused1_dummy,
        mtime,
        __unused2_dummy,
        ctime,
        __unused3_dummy,
        __unused4_dummy,
        __unused5_dummy  : cULong);
  end;

{$ELSE FPC_USE_LIBC}

{ when linking to libc, we need to use some other, 64 bit enhanced stat type.
 Found out by having too much time on hand and some sophisticated guessing. }

  stat = record
    case byte of
      0: (
        st_dev     : cULongLong;
        st_ino     : cULongLong;
        st_mode    : mode_t;
        st_nlink   : nlink_t;
        st_uid     : uid_t;
        st_gid     : gid_t;
        st_rdev    : cULongLong;
        st_size    : cLongLong;
        st_blksize : cULong;
        st_blocks  : cULong;
        st_atime,
        __unused1,
        st_mtime,
        __unused2,
        st_ctime,
        __unused3,
        __unused4,
        __unused5  : cULong);
      1: (
        dev    : cULongLong;
        ino    : cULongLong;
        mode   : mode_t;
        nlink  : nlink_t;
        uid    : uid_t;
        gid    : gid_t;
        rdev   : cULongLong;
        size   : cLongLong;
        blksize,
        blocks,
        atime,
        __unused1_dummy,
        mtime,
        __unused2_dummy,
        ctime,
        __unused3_dummy,
        __unused4_dummy,
        __unused5_dummy  : cULong);
  end;

{$ENDIF FPC_USE_LIBC}

