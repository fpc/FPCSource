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
// (* get it from glibc/sysdeps/unix/sysv/linux/x86_64/bits/stat.h and check defines with gcc *)
// kill the one who made the st_less idea!

  Stat = record
  case Byte of
    0:(dev: cuLong deprecated;
    st_ino: cuLong deprecated;
    st_nlink: cuLong deprecated;

    st_mode: cuInt deprecated;
    st_uid: cuInt deprecated;
    st_gid: cuInt deprecated;
    __pad0: cuInt deprecated;
    st_rdev: cuLong deprecated;
    st_size: cLong deprecated;
    st_blksize: cLong deprecated;
    st_blocks: cLong	deprecated; //* Number 512-byte blocks allocated. */

    st_atime: cuLong deprecated;
    st_atime_nsec: cuLong deprecated;
    st_mtime: cuLong deprecated;
    st_mtime_nsec: cuLong deprecated;
	  st_ctime: cuLong deprecated;
    st_ctime_nsec: cuLong deprecated;
    __unused: array[0..2] of cLong deprecated;);

    1:(st_dev: cuLong;
    st_ino: cuLong;
    st_nlink: cuLong;

    st_mode: cuInt;
    st_uid: cuInt;
    st_gid: cuInt;
    __pad0: cuInt;
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
    __unused: array[0..2] of cLong);
  end;

{$endif}

