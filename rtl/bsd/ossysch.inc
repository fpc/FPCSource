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

//Function Fpmmap(adr,len,prot,flags,fdes,off:longint):longint;  external name  'FPC_SYSC_MMAP';
//Function Fpmmap(start:pointer;len:size_t;prot:cint;flags:cint;fd:cint;offst:off_t):pointer; external name  'FPC_SYSC_MMAP';
//Function Fpmunmap(start:pointer;len:size_t):cint;  external name 'FPC_SYSC_MUNMAP';
//function fpgettimeofday(tp: ptimeval;tzp:ptimezone):cint; external name 'FPC_SYSC_GETTIMEOFDAY';

Function FpIOCtl(Handle:cint;Ndx: culong;Data: Pointer):cint;  external name  'FPC_SYSC_IOCTL';
Function FpGetPid:LongInt;   external name  'FPC_SYSC_GETPID';
//Function FpReadLink(name,linkname:pchar;maxlen:longint):longint;  external name  'FPC_SYSC_READLINK';
{ Needed in both POSIX (for implementation of sleep()) as POSIX realtime extensions or  Unix/freebsd}
Function FpNanoSleep (req : ptimespec;rem : ptimespec) : cint; external name 'FPC_SYSC_NANOSLEEP';

{ can be used for getdir?}
Function Fpgetcwd (path:pchar; siz:size_t):pchar; external name 'FPC_SYSC_GETCWD';

{
  $Log$
  Revision 1.6  2003-09-27 13:45:58  peter
    * fpnanosleep exported in baseunix
    * fpnanosleep has pointer arguments to be C compliant

  Revision 1.5  2003/09/20 12:38:29  marco
   * FCL now compiles for FreeBSD with new 1.1. Now Linux.

  Revision 1.4  2003/09/16 12:45:49  marco
   * mmap typing fixes

  Revision 1.3  2003/09/15 20:08:49  marco
   * small fixes. FreeBSD now cycles

  Revision 1.2  2003/09/14 20:15:01  marco
   * Unix reform stage two. Remove all calls from Unix that exist in Baseunix.

  Revision 1.1  2003/01/05 19:01:28  marco
   * FreeBSD compiles now with baseunix mods.

  Revision 1.4  2002/10/27 11:58:29  marco
   * Modifications from Saturday.



}