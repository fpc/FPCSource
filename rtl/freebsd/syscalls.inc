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

{BSD version of the syscalls required to implement SysLinux.}

{No debugging for syslinux include !}
{$IFDEF SYS_LINUX}
  {$UNDEF SYSCALL_DEBUG}
{$ENDIF SYS_LINUX}

{*****************************************************************************
                     --- Main:The System Call Self ---
*****************************************************************************}

{
Function fpmmap(adr:pointer;len:TSize;prot:cint;flags:cint;fdes:cint;off:TOff):cint;  // moved from sysunix.inc, used in sbrk
begin
 {$ifdef LITTLE_ENDIAN}
  fpmmap:=do_syscall(syscall_nr_mmap,Adr,Len,Prot,Flags,fdes,lo(off),0);
 {$else}
  fpmmap:=do_syscall(syscall_nr_mmap,TSysParam(Adr),TSysParam(Len),Prot,Flags,fdes,0,lo(off));
 {$endif}
end;
}

{
  $Log$
  Revision 1.13  2003-09-16 12:58:35  marco
   * fixje voor mmap parameter typering veranderingen

  Revision 1.12  2003/09/14 20:15:01  marco
   * Unix reform stage two. Remove all calls from Unix that exist in Baseunix.

  Revision 1.11  2003/06/01 16:35:28  marco
   * Several small fixes to harmonize the *BSD rtls and Linux.

  Revision 1.10  2003/01/05 19:02:29  marco
   * Should now work with baseunx. (gmake all works)

  Revision 1.9  2002/09/07 16:01:17  peter
    * old logs removed and tabs fixed

  Revision 1.8  2002/05/06 07:27:39  marco
   * Fixed a readdir bug, already fixed in januari in 1.0.x

}
