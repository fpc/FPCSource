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

Function Sys_mmap(adr,len,prot,flags,fdes,off:longint):longint;  external name  'FPC_SYSC_MMAP';
Function Sys_munmap(adr:longint;len:size_t):longint;  external name 'FPC_SYSC_MUNMAP';
Function Sys_IOCtl(Handle,Ndx: Longint;Data: Pointer):LongInt;  external name  'FPC_SYSC_IOCTL';
Function sys_GetPid:LongInt;   external name  'FPC_SYSC_GETPID';
Function Sys_ReadLink(name,linkname:pchar;maxlen:longint):longint;  external name  'FPC_SYSC_READLINK';

{ Needed in both POSIX (for implementation of sleep()) as POSIX realtime extensions or  Unix/freebsd}
Function sys_NanoSleep (const req : timespec;var rem : timespec) : longint; external name 'FPC_SYSC_NANOSLEEP';

{ can be used for getdir?}
Function sys_getcwd (pt:pchar; _size:size_t):pchar; external name 'FPC_SYSC_GETCWD';

{
  $Log$
  Revision 1.4  2002-10-27 11:58:29  marco
   * Modifications from Saturday.



}