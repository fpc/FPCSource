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

  Stat = packed record  // No unix typing because of differences
    case byte of
      0:
        (dev    : qword;
        __pad0  : array[0..3] of byte;
        __ino,
        mode,
        nlink,
        uid,
        gid     : cardinal;
        rdev    : qword;
        __pad3  : array[0..3] of byte;
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
        st_ino    : qword);
  end;

{$else}

{$packrecords C}
  Stat =  Record
    case byte of
      0:
                              // kernel <->libc
        (dev     : dev_t;
        pad1       : word;
{$ifdef fs32bit}
        ino     : ino_t;
{$else}
        __ino   : ino_t;
{$endif}    
        mode    : mode_t;
        nlink   : nlink_t;
        uid     : uid_t;
        gid     : gid_t;
        rdev    : dev_t;
        pad2       : word;
        size    : off_t;
        blksize : blksize_t;
{$ifdef fs32bit}
        blocks  : blkcnt_t;
{$else}    
        blocks  : blkcnt64_t;
{$endif}    
        atime,
        atime_nsec,
        mtime,
        mtime_nsec,
        ctime,
        ctime_nsec : cardinal;
{$ifdef fs32bit}
        unused4, unused5: cardinal;
{$else}    
        ino     : ino64_t;
{$endif});
      1:
                              // kernel <->libc
        (st_dev    : dev_t;
        pad1_      : word;
{$ifdef fs32bit}
        st_ino     : ino_t;
{$else}
        __st_ino   : ino_t;
{$endif}    
        st_mode    : mode_t;
        st_nlink   : nlink_t;
        st_uid     : uid_t;
        st_gid     : gid_t;
        st_rdev    : dev_t;
        pad2_       : word;
        st_size    : off_t;
        st_blksize : blksize_t;
{$ifdef fs32bit}
        st_blocks  : blkcnt_t;
{$else}    
        st_blocks  : blkcnt64_t;
{$endif}    
        st_atime,
        st_atime_nsec,
        st_mtime,
        st_mtime_nsec,
        st_ctime,
        st_ctime_nsec : culong;
{$ifdef fs32bit}
        unused4_, unused5_: culong;
{$else}    
        st_ino     : ino64_t;
{$endif});
  end;


{$endif}

