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

{$ifndef FPC_USE_LIBC} // kernel record

  Stat = packed record  
        st_dev   : qword;
        __pad0_   : array[0..3] of byte;
        __st_ino_,
        st_mode,
        st_nlink,
        st_uid,
        st_gid    : cardinal;
        st_rdev   : qword;
        __pad3_   : array[0..3] of byte;
        st_size   : qword;
        st_blksize: cardinal;
        st_blocks : qword;
        st_atime,
        st_atime_nsec,
        st_mtime,
        st_mtime_nsec,
        st_ctime,
        st_ctime_nsec : cardinal;
        st_ino    : qword;
  end;

{$else}

// info taken from /usr/include/asm-i386/stat.h you must have some -dev installed for this I think.. or linux-headers

{$packrecords C}

{$ifdef fs32bit}
  Stat =  Record
         st_dev: cuLong;
         st_ino: cuLong;
         st_mode: cuShort;
         st_nlink: cuShort; 
         st_uid: cuShort;
         st_gid: cuShort;
         st_rdev: cuLong;
         st_size: cuLong;
         st_blksize: cuLong;
         st_blocks: cuLong;
         st_atime: cuLong;
         st_atime_nsec: cuLong;
         st_mtime: cuLong;
         st_mtime_nsec: cuLong;
         st_ctime: cuLong;
         st_ctime_nsec: cuLong;
         __unused4_: cuLong;
         __unused5_: cuLong;
  end;

{$else} // 64 bit stat

  Stat = record
    st_dev: cuLongLong;
    __pad0_: array[0..3] of cuChar;

    __st_ino: cuLong;

    st_mode: cuInt;
    st_nlink: cuInt;

    st_uid: cuLong;
    st_gid: cuLong;

    st_rdev: cuLongLong;
    __pad3_: array[0..3] of cuChar;

    st_size: cLongLong;
    st_blksize: cuLong;

    st_blocks: cuLongLong;	//* Number 512-byte blocks allocated. */

    st_atime: cuLong;
    st_atime_nsec: cuLong;

    st_mtime: cuLong;
    st_mtime_nsec: cuInt;

    st_ctime: cuLong;
    st_ctime_nsec: cuLong;

    st_ino: cuLongLong;
  end;

{$endif}


{$endif}

