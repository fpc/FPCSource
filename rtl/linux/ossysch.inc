{
    $Id$
    Copyright (c) 2002 by Marco van de Voort

    Header file for calls used in System unit, but not exposed
    in BaseUnix.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ****************************************************************************
}

Function Fpmmap(adr:pointer;len:size_t;prot:cint;flags:cint;fd:cint;off:off_t):pointer;  external name 'FPC_SYSC_MMAP';
Function Fpmunmap(adr:pointer;len:size_t):cint; external name 'FPC_SYSC_MUNMAP';
Function FpIOCtl(handle:cint;ndx:culong;Data: Pointer):cint;  external name 'FPC_SYSC_IOCTL';
Function FpGetPid:pid_t;   external name 'FPC_SYSC_GETPID';
Function FpReadLink(name,linkname:pchar;maxlen:size_t):cint;  external name 'FPC_SYSC_READLINK';
Function FpNanoSleep(const req : timespec;rem : ptimespec) : longint; external name 'FPC_SYSC_NANOSLEEP';
function gettimeofday(tp: ptimeval;tzp:ptimezone):cint; external name 'FPC_SYSC_GETTIMEOFDAY';

{
 $Log$
 Revision 1.2  2003-09-14 20:15:01  marco
  * Unix reform stage two. Remove all calls from Unix that exist in Baseunix.

 Revision 1.1  2002/12/18 16:43:26  marco
  * new unix rtl, linux part.....


}
