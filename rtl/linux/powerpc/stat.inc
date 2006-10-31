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

  Stat = packed record  // No unix typing because of differences
    st_dev    : culonglong;
    st_ino    : culonglong;
    st_mode   : cuint;
    st_nlink  : cuint;
    st_uid    : cuint;
    st_gid    : cuint;
    st_rdev   : culonglong;
    __pad2    : cushort;
    st_size   : clonglong;
    st_blksize: cint;
    st_blocks : clonglong;
    st_atime  : cint;
    st_atime_nsec : cuint;
    st_mtime  : cint;
    st_mtime_nsec : cuint;
    st_ctime  : cint;
    st_ctime_nsec : cuint;
    __unused4 : cuint;
    __unused5 : cuint;
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

