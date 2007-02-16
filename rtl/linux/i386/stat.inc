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
    st_dev    : culonglong;
    __pad0    : array[0..3] of cuchar;
    __st_ino  : culong;
    st_mode   : cuint;
    st_nlink  : cuint;
    st_uid    : culong;
    st_gid    : culong;
    st_rdev   : culonglong;
    __pad3    : array[0..3] of cuchar;
    st_size   : culonglong;
    st_blksize: culong;
    st_blocks : culonglong;
    st_atime  : culong;
    st_atime_nsec : culong;
    st_mtime  : culong;
    st_mtime_nsec : cuint;
    st_ctime  : culong;
    st_ctime_nsec : culong;
    st_ino    : culonglong;
  end;

{$else}

{$packrecords C}
  Stat =  Record  // No unix typing because of differences
                              // kernel <->libc
    st_dev     : dev_t;
    pad1       : word;
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
    pad2       : word;
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
    unused4, unused5: culong;
{$else}    
    st_ino     : ino64_t;
{$endif}    
  end;


{$endif}

