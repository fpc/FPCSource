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

{$PACKRECORDS C}

{$IFNDEF FPC_USE_LIBC}

  { do NOT declare this record as packed, because it needs to be aligned }
  { to its largest member (i.e., 8 bytes), and declaring it as packed    }
  { disables that                                                        }
  Stat = record  // No unix typing because of differences
    st_dev    : culong;
    st_ino    : culong;
    st_nlink  : culong;

    st_mode   : cuint;
    st_uid    : cuint;
    st_gid    : cuint;
    __pad0    : cuint;

    st_rdev   : culong;
    st_size   : clong;
    st_blksize: clong;
    st_blocks : clong;

    st_atime  : culong;
    st_atime_nsec : culong;
    st_mtime  : culong;
    st_mtime_nsec : culong;
    st_ctime  : culong;
    st_ctime_nsec : culong;
    __unused  : array[0..2] of clong;
  end;

{$ELSE FPC_USE_LIBC}

  Stat = record
    st_dev    : cULongLong;
    st_ino     : cULongLong;
    st_mode    : mode_t;
    st_nlink   : nlink_t;
    st_uid     : uid_t;
    st_gid     : gid_t;
    st_rdev    : cULongLong;
    __pad2_    : cushort;
    st_size    : cLongLong;
    st_blksize : cULong;
    st_blocks  : cULongLong;
    st_atime,
    st_atime_nsec,
    st_mtime,
    st_mtime_nsec,
    st_ctime,
    st_ctime_nsec,
    __unused4_,
    __unused5_ : cULong;
  end;

{$ENDIF FPC_USE_LIBC}

