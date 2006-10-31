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

  Stat = packed record  // No unix typing because of differences
    st_dev    : culonglong;
    st_ino    : culonglong;
    st_mode   : cuint;
    st_nlink  : cuint;
    st_uid    : cuint;
    st_gid    : cuint;
    st_rdev   : culonglong;
    __pad3    : array[0..7] of cuchar;
    st_size   : clonglong;
    st_blksize: cuint;
    __pad4    : array[0..7] of cuchar;
    st_blocks : cuint;
    st_atime  : cuint;
    st_atime_nsec : cuint;
    st_mtime  : cuint;
    st_mtime_nsec : cuint;
    st_ctime  : cuint;
    st_ctime_nsec : cuint;
    __unused4 : cuint;
    __unused5 : cuint;
  end;

