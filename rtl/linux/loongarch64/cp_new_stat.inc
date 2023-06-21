{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Jonas Maebe, (c) 2005 Thomas Schatzl,
    members of the Free Pascal development team.

    Contains the transformation from struct stat to struct statx.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

  procedure cp_new_stat(nbuf: pstatx64; buf: pstat);

  begin
    buf^.st_dev:=makedev(nbuf^.stx_dev_major,nbuf^.stx_dev_minor);
    buf^.st_rdev:=makedev(nbuf^.stx_rdev_major,nbuf^.stx_rdev_minor);
    buf^.st_ino:=nbuf^.stx_ino;
    buf^.st_mode:=nbuf^.stx_mode;
    buf^.st_nlink:=nbuf^.stx_nlink;
    buf^.st_uid:=nbuf^.stx_uid;
    buf^.st_gid:=nbuf^.stx_gid;
    buf^.st_size:=nbuf^.stx_size;
    buf^.st_blksize:=nbuf^.stx_blksize;
    buf^.st_blocks:=nbuf^.stx_blocks;
    buf^.st_atime:=nbuf^.stx_atime.tv_sec;
    buf^.st_atime_nsec:=nbuf^.stx_atime.tv_nsec;
    buf^.st_mtime:=nbuf^.stx_mtime.tv_sec;
    buf^.st_mtime_nsec:=nbuf^.stx_mtime.tv_nsec;
    buf^.st_ctime:=nbuf^.stx_ctime.tv_sec;
    buf^.st_ctime_nsec:=nbuf^.stx_ctime.tv_nsec;
  end;
