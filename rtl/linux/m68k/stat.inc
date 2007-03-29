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
    case byte of
      0:
        (dev    : qword;
        __pad0  : array[0..1] of byte;
        __ino,
        mode,
        nlink,
        uid,
        gid     : cardinal;
        rdev    : qword;
        __pad3  : array[0..1] of byte;
        size    : qword;
        blksize : cardinal;
        blocks  : qword;
        atime,
        atime_nsec,
        mtime,
        mtime_nsec,
        ctime,
        ctime_nsec : cardinal;
        ino    : qword);
      1:
        (st_dev   : qword;
        __pad0_   : array[0..1] of byte;
        __st_ino_,
        st_mode,
        st_nlink,
        st_uid,
        st_gid    : cardinal;
        st_rdev   : qword;
        __pad3_   : array[0..1] of byte;
        st_size   : qword;
        st_blksize: cardinal;
        st_blocks : qword;
        st_atime,
        st_atime_nsec,
        st_mtime,
        st_mtime_nsec,
        st_ctime,
        st_ctime_nsec : cardinal;
        st_ino    : qword);
  end;
