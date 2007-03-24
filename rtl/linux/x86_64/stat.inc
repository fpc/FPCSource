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
    case byte of
      0:
        (dev,
        ino,
        nlink  : qword;

        mode,
        uid,
        gid,
        __pad0 : cardinal;
        rdev   : qword;
        size,
        blksize,
        blocks : int64;

        atime,
        atime_nsec,
        mtime,
        mtime_nsec,
        ctime,
        ctime_nsec : qword;
        __unused  : array[0..2] of qword);
      1:
        (st_dev,
        st_ino,
        st_nlink  : qword;

        st_mode,
        st_uid,
        st_gid,
        __pad0    : cardinal;
        st_rdev   : qword;
        st_size,
        st_blksize,
        st_blocks : int64;

        st_atime,
        st_atime_nsec,
        st_mtime,
        st_mtime_nsec,
        st_ctime,
        st_ctime_nsec : qword;
        __unused  : array[0..2] of qword;);
  end;

{$else}

(* get it from glibc/sysdeps/unix/sysv/linux/x86_64/bits/stat.h and check defines with gcc *)

{$endif}

