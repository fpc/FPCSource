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
        nlink  : qword deprecated;

        mode,
        uid,
        gid,
        __pad0 : cardinal deprecated;
        rdev   : qword deprecated;
        size,
        blksize,
        blocks : int64 deprecated;

        atime,
        atime_nsec,
        mtime,
        mtime_nsec,
        ctime,
        ctime_nsec : qword deprecated;
        __unused  : array[0..2] of qword deprecated);
      1:
        (st_dev,
        st_ino,
        st_nlink  : qword;

        st_mode,
        st_uid,
        st_gid,
        __pad1    : cardinal;
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
        __unused2  : array[0..2] of qword;);
  end;

{$else}

{$packrecords C}
// god from /usr/include/asm-x86_64 some -dev of linux-headers must be installed

  Stat = record
  case Byte of
    0:(dev: cuLong deprecated;
    ino: cuLong deprecated;
    nlink: cuLong deprecated;

    mode: cuInt deprecated;
    uid: cuInt deprecated;
    gid: cuInt deprecated;
    __pad0: cuInt deprecated;
    rdev: cuLong deprecated;
    size: cLong deprecated;
    blksize: cLong deprecated;
    blocks: cLong	deprecated; //* Number 512-byte blocks allocated. */

    atime: cuLong deprecated;
    atime_nsec: cuLong deprecated;
    mtime: cuLong deprecated;
    mtime_nsec: cuLong deprecated;
    ctime: cuLong deprecated;
    ctime_nsec: cuLong deprecated;
    __unused: array[0..2] of cLong deprecated;);

    1:(st_dev: cuLong;
    st_ino: cuLong;
    st_nlink: cuLong;

    st_mode: cuInt;
    st_uid: cuInt;
    st_gid: cuInt;
    __pad1: cuInt;
    st_rdev: cuLong;
    st_size: cLong;
    st_blksize: cLong;
    st_blocks: cLong;	//* Number 512-byte blocks allocated. */

    st_atime: cuLong;
    st_atime_nsec: cuLong; 
    st_mtime: cuLong;
    st_mtime_nsec: cuLong;
    st_ctime: cuLong;
    st_ctime_nsec: cuLong;
    __unused2: array[0..2] of cLong);
  end;

{$endif}

