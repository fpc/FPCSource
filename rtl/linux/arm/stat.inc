                                                              {
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by Florian Klaempfl,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

  Stat = packed record  // No unix typing because of differences
    case byte of
    0:
      (dev    : culonglong;
      __pad0    : array[0..3] of cuchar;
      __st_ino  : culong;
      mode   : cuint;
      nlink  : cuint;
      uid    : culong;
      gid    : culong;
      rdev   : culonglong;
      __pad3 : array[0..3] of cuchar;
      size   : culonglong;
      blksize: culong;
      blocks : culonglong;
      atime  : culong;
      atime_nsec : culong;
      mtime  : culong;
      mtime_nsec : culong;
      ctime  : culong;
      ctime_nsec : culong;
      ino    : culonglong;);
    1:
      (st_dev    : culonglong;
      __pad0_    : array[0..3] of cuchar;
      __st_ino_  : culong;
      st_mode   : cuint;
      st_nlink  : cuint;
      st_uid    : culong;
      st_gid    : culong;
      st_rdev   : culonglong;
      __pad3_   : array[0..3] of cuchar;
      st_size   : culonglong;
      st_blksize: culong;
      st_blocks : culonglong;
      st_atime  : culong;
      st_atime_nsec : culong;
      st_mtime  : culong;
      st_mtime_nsec : culong;
      st_ctime  : culong;
      st_ctime_nsec : culong;
      st_ino    : culonglong);
  end;
