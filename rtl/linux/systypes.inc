{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
type

{
 Linux system calls take arguments as follows :

   i386    m68k    ppc
   %eax    %d0     r0      System call number
   %ebx    %d1     r3      first argument
   %ecx    %d2     r4      second argument
   %edx    %d3     r5      third argumens
   %esi    %d3     r6      fourth argument
   %edi    %d4     r7      fifth argument

  That is why we define a special type, with only these arguments
  To make it processor independent, we don't give any system dependent
  names, but the rather abstract reg1,reg2 etc;
}
  SysCallRegs=record
    reg1,reg2,reg3,reg4,reg5,reg6 : longint;
  end;
  PSysCallRegs=^SysCallRegs;
  TSysCallRegs=SysCallRegs;

{ The following are records for system calls }
  dirent = packed record
    ino,
    off    : longint;
    reclen : word;
    name   : array [0..255] of char;
  end;
  pdirent =^dirent;
  TDirEnt = dirent;

  TDir = packed record
    fd     : integer;
    loc    : longint;
    size   : integer;
    buf    : pdirent;
  {The following are used in libc, but NOT in the linux kernel sources ??}
    nextoff: longint;
    dd_max : integer; {size of buf. Irrelevant, as buf is of type dirent}
    lock   : pointer;
  end;
  PDir =^TDir;

  dev_t = word;

  Stat = packed record
    dev     : dev_t;
    pad1    : word;
    ino     : longint;
    mode,
    nlink,
    uid,
    gid     : word;
    rdev    : dev_t;
    pad2    : word;
    size,
    blksze,
    blocks,
    atime,
    unused1,
    mtime,
    unused2,
    ctime,
    unused3,
    unused4,
    unused5 : longint;
  end;
  PStat=^Stat;
  TStat=Stat;

  TStatfs = packed record
    fstype,            { File system type }
    bsize,             { Optimal block trensfer size }
    blocks,            { Data blocks in system }
    bfree,             { free blocks in system }
    bavail,            { Available free blocks to non-root users }
    files,             { File nodes in system }
    ffree,             { Free file nodes in system }
    fsid,              { File system ID }
    namelen : longint; { Maximum name length in system }
    spare   : array [0..6] of longint; { For later use }
  end;
  PStatFS=^TStatFS;

  fdSet=array[0..7] of longint;{=256 bits}
  pfdset=^fdset;
  TFDSet=fdset;

  timeval = packed record
    sec,usec:longint
  end;
  ptimeval=^timeval;
  TTimeVal=timeval;

  timespec = packed record
    tv_sec,tv_nsec:longint;
  end;

  timezone = packed record
    minuteswest,dsttime:longint;
  end;
  ptimezone =^timezone;
  TTimeZone = timezone;

  utsname = packed record
    sysname,
    nodename,
    release,
    version,
    machine,
    domainname : Array[0..64] of char;
  end;
  PUTSName=^UTSName;
  TUTSName=UTSName;

{
  $Log$
  Revision 1.6  2002-07-29 17:50:02  florian
    + added register location description for ppc

  Revision 1.5  2001/06/03 20:19:09  peter
    * FSStat to StatFS
    * StatFS structure to TStatFS

  Revision 1.4  2001/06/02 00:31:30  peter
    * merge unix updates from the 1.0 branch, mostly related to the
      solaris target

}
