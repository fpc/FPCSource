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

{ This structure was ported from 

    /usr/include/asm-ppc64/stat.h

  on a 2.6.11 kernel. Note that the stat record is the same for direct
  syscalls as for when linking to libc.
}

{$PACKRECORDS C}
  stat = record 
    case integer of
    0 : (
      st_dev     : cULong;
      st_ino     : cULong;     { wrongly defined in RTL? }
      st_nlink   : cULong;     { wrongly defined in RTL? }

      st_mode    : mode_t;
      st_uid     : uid_t;
      st_gid     : gid_t;

      st_rdev    : cULong;
      st_size    : off_t;
      st_blksize : cULong;
      st_blocks  : cULong;     { Number 512-byte blocks allocated. }

      st_atime : cULong;
      st_atime_nsec : cULong;
      st_mtime : cULong;
      st_mtime_nsec : cULong;
      st_ctime : cULong;
      st_ctime_nsec : cULong;
      __unused : array[0..2] of cULong
      );
    1 : (
      dev        : cULong deprecated;
      ino        : cULong deprecated;
      nlink      : cULong deprecated;
      mode       : mode_t deprecated;
      uid        : uid_t deprecated;
      gid        : gid_t deprecated;
      rdev       : cULong deprecated;
      size       : off_t deprecated;
      blksize,
      blocks,
      atime,
      atime_nsec,
      mtime,
      mtime_nsec,
      ctime,
      ctime_nsec : cULong deprecated;
      __unused_dummy : array[0..2] of cULong deprecated;
      );
  end;
