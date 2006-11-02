{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2004 by Jonas Maebe,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifndef FPC_USE_LIBC} // kernel record

  stat = packed record  // No unix typing because of differences
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

{$else}

(* get it from glibc/sysdeps/unix/sysv/linux/x86_64/bits/stat.h and check defines with gcc *)

{$endif}

