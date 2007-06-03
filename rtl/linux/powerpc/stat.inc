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

  Stat = record  // No unix typing because of differences
    case byte of
      0:
        (dev    : culonglong deprecated;
        ino    : culonglong deprecated;
        mode   : cuint deprecated;
        nlink  : cuint deprecated;
        uid    : cuint deprecated;
        gid    : cuint deprecated;
        rdev   : culonglong deprecated;
        __pad2 : cushort deprecated;
        size   : clonglong deprecated;
        blksize: clong deprecated;

        blocks : clonglong deprecated;
        atime  : clong deprecated;
        atime_nsec : culong deprecated;
        mtime  : clong deprecated;
        mtime_nsec : culong deprecated;
        ctime  : clong deprecated;
        __unused3 : culong deprecated;
        __unused4 : culong deprecated;
        __unused5 : culong deprecated;);
      1:
        (st_dev    : culonglong;
        st_ino    : culonglong;
        st_mode   : cuint;
        st_nlink  : cuint;
        st_uid    : cuint;
        st_gid    : cuint;
        st_rdev   : culonglong;
        __pad2_    : cushort;
        st_size   : clonglong;
        st_blksize: clong;

        st_blocks : clonglong;
        st_atime  : clong;
        st_atime_nsec : culong;
        st_mtime  : clong;
        st_mtime_nsec : culong;
        st_ctime  : clong;
        __unused3_ : culong;
        __unused4_ : culong;
        __unused5_ : culong;);
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
        __pad2     : cushort;
        st_size    : cLongLong;
        st_blksize : cULong;
        st_blocks  : cULongLong;
        st_atime,
        st_atime_nsec,
        st_mtime,
        st_mtime_nsec,
        st_ctime,
        st_ctime_nsec,
        __unused4,
        __unused5  : cULong);
      1: (
        dev    : cULongLong deprecated;
        ino    : cULongLong deprecated;
        mode   : mode_t deprecated;
        nlink  : nlink_t deprecated;
        uid    : uid_t deprecated;
        gid    : gid_t deprecated;
        rdev   : cULongLong deprecated;
        __pad2_: cushort deprecated;
        size   : cLongLong deprecated;
        blksize: cULong deprecated;
        blocks : cULongLong deprecated;
        atime,
        __unused1_dummy,
        mtime,
        __unused2_dummy,
        ctime,
        __unused3_dummy,
        __unused4_dummy,
        __unused5_dummy  : cULong deprecated);
  end;

{$ENDIF FPC_USE_LIBC}

