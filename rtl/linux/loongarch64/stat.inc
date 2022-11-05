{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Jonas Maebe, (c) 2005 Thomas Schatzl,
    members of the Free Pascal development team.

    Contains the definition of the stat type for the PowerPC64 platform.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ This structure was adapted from

    include/uapi/asm-generic/stat.h

  in Linux 4.0. Note that the stat record is the same for direct
  syscalls as for when linking to libc.
}

{$PACKRECORDS C}
  stat = record
      st_dev        : cULong;
      st_ino        : cULong;
      st_mode       : cUInt;
      st_nlink      : cUInt;
      st_uid        : cUInt;
      st_gid        : cUInt;
      st_rdev       : cULong;
      __pad1a       : cULong;
      st_size       : cLong;
      st_blksize    : cInt;
      __pad2a       : cInt;
      st_blocks     : cLong;
      st_atime      : cLong;
      st_atime_nsec : cULong;
      st_mtime      : cLong;
      st_mtime_nsec : cULong;
      st_ctime      : cLong;
      st_ctime_nsec : cULong;
      __unused4a    : cUInt;
      __unused5a    : cUInt;
  end;
