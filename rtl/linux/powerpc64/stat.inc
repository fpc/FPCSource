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
    st_dev : qword;
    st_ino : qword;
    st_nlink : qword;

    st_mode : dword;
    st_uid : dword;
    st_gid : dword;
    __pad0 : dword;
    st_rdev : qword;
    st_size : int64;
    st_blksize : int64;
    st_blocks : int64;      { Number 512-byte blocks allocated. }

    st_atime : qword;
    __reserved0 : qword;    { reserved for atime.nanoseconds }
    st_mtime : qword;
    __reserved1 : qword;    { reserved for atime.nanoseconds }
    st_ctime : qword;
    __reserved2 : qword;    { reserved for atime.nanoseconds }
    __unused : array[0..2] of int64;
  end;

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
    st_dev : qword;
    st_ino : qword;
    st_nlink : qword;

    st_mode : dword;
    st_uid : dword;
    st_gid : dword;
    __pad0 : dword;
    st_rdev : qword;
    st_size : int64;
    st_blksize : int64;
    st_blocks : int64;      { Number 512-byte blocks allocated. }

    st_atime : qword;
    __reserved0 : qword;    { reserved for atime.nanoseconds }
    st_mtime : qword;
    __reserved1 : qword;    { reserved for atime.nanoseconds }
    st_ctime : qword;
    __reserved2 : qword;    { reserved for atime.nanoseconds }
    __unused : array[0..2] of int64;
  end;

