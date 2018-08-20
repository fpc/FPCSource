{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Jonas Maebe, (c) 2005 Thomas Schatzl,
    members of the Free Pascal development team.

    Contains the definition of the stat type for the PowerPC platform.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ This structure was ported from 

    /usr/include/asm-ppc/stat.h

  on a 2.6.11 kernel.
}

{$PACKRECORDS C}

{$IFNDEF FPC_USE_LIBC}

  Stat = record  // No unix typing because of differences
        st_dev    : culonglong;
        st_ino    : culonglong;
        st_mode   : cuint;
        st_nlink  : cuint;
        st_uid    : cuint;
        st_gid    : cuint;
        st_rdev   : culonglong;
        __pad2_    : cushort;
        st_size   : clonglong;
        st_blksize: clong;

        st_blocks : clonglong;
        st_atime  : clong;
        st_atime_nsec : culong;
        st_mtime  : clong;
        st_mtime_nsec : culong;
        st_ctime  : clong;
        __unused3_ : culong;
        __unused4_ : culong;
        __unused5_ : culong;
  end;

{$ELSE FPC_USE_LIBC}

{ when linking to libc, we need to use some other, 64 bit enhanced stat type }

{ i.e. powerpc kernel sources (2.6.20-15) /include/asm-powerpc/stat.h, stat64 struct }

  stat = record
        st_dev     : cULongLong;
        st_ino     : cULongLong;
        st_mode    : cUInt;
        st_nlink   : cUInt;
        st_uid     : cUInt;
        st_gid     : cUInt;
        st_rdev    : cULongLong;
        __pad2     : cUShort;
        st_size    : cLongLong;
        st_blksize : cInt;
        st_blocks  : cULongLong;
        st_atime,
        st_atime_nsec,
        st_mtime,
        st_mtime_nsec,
        st_ctime,
        st_ctime_nsec,
        __unused4,
        __unused5  : cULong;
  end;

{$ENDIF FPC_USE_LIBC}

