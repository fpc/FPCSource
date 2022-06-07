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

{$IFNDEF FPC_USE_LIBC}

  { do NOT declare this record as packed, because it needs to be aligned }
  { to its largest member (i.e., 8 bytes), and declaring it as packed    }
  { disables that                                                        }
  Stat = record  // No unix typing because of differences
    case byte of
    0:
      (dev   : culonglong deprecated;
      ino    : culonglong deprecated;
      mode   : cuint deprecated;
      nlink  : cuint deprecated;
      uid    : cuint deprecated;
      gid    : cuint deprecated;
      rdev   : culonglong deprecated;
      __pad3 : array[0..7] of cuchar deprecated;
      size   : clonglong deprecated;
      blksize: cuint deprecated;
      __pad4 : array[0..7] of cuchar deprecated;
      blocks : cuint deprecated;
      atime  : cuint deprecated;
      atime_nsec : cuint deprecated;
      mtime  : cuint deprecated;
      mtime_nsec : cuint deprecated;
      ctime  : cuint deprecated;
      ctime_nsec : cuint deprecated;
      __unused4 : cuint deprecated;
      __unused5 : cuint deprecated);
    1:
      (st_dev    : culonglong;
      st_ino    : culonglong;
      st_mode   : cuint;
      st_nlink  : cuint;
      st_uid    : cuint;
      st_gid    : cuint;
      st_rdev   : culonglong;
      __pad3_   : array[0..7] of cuchar;
      st_size   : clonglong;
      st_blksize: cuint;
      __pad4_   : array[0..7] of cuchar;
      st_blocks : cuint;
      st_atime  : cuint;
      st_atime_nsec : cuint;
      st_mtime  : cuint;
      st_mtime_nsec : cuint;
      st_ctime  : cuint;
      st_ctime_nsec : cuint;
      __unused4_ : cuint;
      __unused5_ : cuint);
  end;

{$ELSE FPC_USE_LIBC}

{$PACKRECORDS C}

  Stat = record
    case byte of
     0:
      (dev     : cULongLong deprecated;
      ino     : cULongLong deprecated;
      mode    : mode_t deprecated;
      nlink   : nlink_t deprecated;
      uid     : uid_t deprecated;
      gid     : gid_t deprecated;
      rdev    : cULongLong deprecated;
      __pad2  : cushort deprecated;
      size    : cLongLong deprecated;
      blksize : cULong deprecated;
      blocks  : cULongLong deprecated;
      atime,
      atime_nsec,
      mtime,
      mtime_nsec,
      ctime,
      ctime_nsec,
      __unused4,
      __unused5  : cULong);
     1:
      (st_dev    : cULongLong;
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
      __unused5_ : cULong);
  end;

{$ENDIF FPC_USE_LIBC}

