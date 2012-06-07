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

  Stat = Record
    st_dev     : culong;
    st_pad0    : array[0..2] of culong;
    st_ino     : culonglong;
    st_mode    : mode_t;
    st_nlink   : nlink_t;
    st_uid     : uid_t;
    st_gid     : gid_t;
    st_rdev    : culong;
    st_pad1    : array[0..2] of culong;
    st_size    : clonglong;
{$ifdef __USE_MISC}
    st_atim: timespec;
    st_mtim: timespec;
    st_ctim: timespec;
{$else not __USE_MISC}
    st_atime: time_t;
    st_atime_nsec: cint;

    st_mtime: time_t;
    st_mtime_nsec: cint;

    st_ctime: time_t;
    st_ctime_nsec: cint;
{$endif not __USE_MISC}
    st_blksize : blksize_t;
    __pad4     : cuint;
{$ifndef __USE_FILE_OFFSET64}
    st_blocks  : blkcnt_t;
{$else __USE_FILE_OFFSET64}
    st_blocks  : blkcnt64_t;
{$endif __USE_FILE_OFFSET64}
    st_pad5: array[0..13] of cint;
  end;
