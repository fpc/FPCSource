{
    $Id$
    Copyright (c) 2002 by Marco van de Voort

    Header for functions/syscalls included in system, but not in POSIX.  To
    implement unit UNIX, and/or other lowlevel unix routines.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ****************************************************************************
}

Function Fpmmap(adr,len,prot,flags,fdes,off:longint):longint;  external name  'FPC_SYSC_MMAP';
Function Fpmunmap(adr:longint;len:size_t):longint;  external name 'FPC_SYSC_MUNMAP';
Function FpIOCtl(Handle,Ndx: Longint;Data: Pointer):LongInt;  external name  'FPC_SYSC_IOCTL';
Function FpGetPid:LongInt;   external name  'FPC_SYSC_GETPID';
Function FpReadLink(name,linkname:pchar;maxlen:longint):longint;  external name  'FPC_SYSC_READLINK';

{ Needed in both POSIX (for implementation of sleep()) as POSIX realtime extensions or  Unix/freebsd}
Function FpNanoSleep (const req : timespec;var rem : timespec) : longint; external name 'FPC_SYSC_NANOSLEEP';

{ can be used for getdir?}
Function Fpgetcwd (path:pchar; siz:size_t):pchar; external name 'FPC_SYSC_GETCWD';

{
  $Log$
  Revision 1.1  2003-01-05 19:01:28  marco
   * FreeBSD compiles now with baseunix mods.

  Revision 1.4  2002/10/27 11:58:29  marco
   * Modifications from Saturday.



}