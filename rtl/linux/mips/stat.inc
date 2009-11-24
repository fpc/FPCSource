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

  Stat = packed Record  // No unix typing because of differences
      st_dev    : dword;
          __pad1    : dword;
          __pad11    : dword;
          __pad12    : dword;
          st_ino    : cardinal;
          st_mode   : dword;
          nlink     :longint; //: smallint;
          uid       : dword;
          gid       : dword;
          rdev      : dword;
          __pad2    : dword;
          __pad21    : dword;
          st_size   : longint;
     st_pad3   : longint;

          st_atime  : longint;
          st_atime_nsecs : cardinal;
          st_mtime  : longint;
          st_mtime_nsecs : cardinal;
          st_ctime  : longint;
          st_ctime_nsecs : cardinal;
          st_blksize : longint;
          st_blocks  : longint;
          st_pad5: array[0..13] of longint;

  end;
