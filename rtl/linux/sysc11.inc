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

{ Include syscall itself }
{i syscallo.inc}
Function Sys_mmap(adr,len,prot,flags,fdes,off:longint):longint; // moved from sysunix.inc, used in sbrk

begin
  fpmmap(pointer(adr),size_t(len),cint(prot),cint(flags),cint(fdes),off_t(off));
end;

Function Sys_munmap(adr,len:longint):longint; // moved from sysunix.inc, used in sbrk
begin
 fpmunmap(pointer(adr),cint(len));
end;

{
  Interface to Unix ioctl call.
  Performs various operations on the filedescriptor Handle.
  Ndx describes the operation to perform.
  Data points to data needed for the Ndx function. The structure of this
  data is function-dependent.
}
Function Sys_IOCtl(Handle,Ndx: Longint;Data: Pointer):LongInt;  // This was missing here, instead hardcode in Do_IsDevice

begin
  sys_ioctl:=fpioctl(handle,ndx,data);
end;


{
  $Log$
  Revision 1.3  2003-11-17 11:28:08  marco
   * Clone moved to linux, + few small unit unix changes

  Revision 1.2  2003/10/31 08:55:11  mazen
  + assembler mode forced to ATT style for x86 cpu

  Revision 1.1  2003/10/30 16:42:25  marco
   * Killing off old syscall convention anywhere except for oldlinux

  Revision 1.19  2003/10/17 20:56:24  olle
    * Changed m68k to cpum68k, i386 to cpui386

  Revision 1.18  2003/09/14 20:15:01  marco
   * Unix reform stage two. Remove all calls from Unix that exist in Baseunix.

  Revision 1.17  2002/12/18 16:43:26  marco
   * new unix rtl, linux part.....

  Revision 1.16  2002/11/11 21:40:26  marco
   * rename syscall.inc -> syscallo.inc

  Revision 1.15  2002/10/14 19:39:17  peter
    * threads unit added for thread support

  Revision 1.14  2002/09/10 21:32:14  jonas
    + added "nop" after sc instruction, since normally in case of success,
      sc returns to the second instruction after itself

  Revision 1.13  2002/09/07 16:01:19  peter
    * old logs removed and tabs fixed

  Revision 1.12  2002/09/07 13:14:04  florian
    * hopefully final fix for ppc syscall BTW: The regX numbering is somehow messy

  Revision 1.11  2002/09/03 21:37:54  florian
    * hopefully final fix for ppc syscall

  Revision 1.10  2002/09/02 20:42:22  florian
    * another ppc syscall fix

  Revision 1.9  2002/09/02 20:03:20  florian
    * ppc syscall code fixed

  Revision 1.8  2002/08/19 18:24:05  jonas
    + ppc support for do_syscall

  Revision 1.7  2002/07/29 21:28:17  florian
    * several fixes to get further with linux/ppc system unit compilation

  Revision 1.6  2002/07/28 20:43:48  florian
    * several fixes for linux/powerpc
    * several fixes to MT

}
