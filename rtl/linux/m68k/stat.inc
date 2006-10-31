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

  Stat = packed record  // No unix typing because of differences
    st_dev    : culonglong;
    __pad0    : array[0..1] of cuchar;
    __st_ino  : culong;
    st_mode   : cuint;
    st_nlink  : cuint;
    st_uid    : culong;
    st_gid    : culong;
    st_rdev   : culonglong;
    __pad3    : array[0..1] of cuchar;
    st_size   : clonglong;
    st_blksize: culong;
    st_blocks : culonglong;
    st_atime  : culong;
    st_atime_nsec : culong;
    st_mtime  : culong;
    st_mtime_nsec : culong;
    st_ctime  : culong;
    st_ctime_nsec : culong;
    st_ino    : culonglong;
  end;

