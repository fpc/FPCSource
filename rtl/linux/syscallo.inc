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

{No debugging for syslinux include !}
{$IFDEF SYS_LINUX}
  {$UNDEF SYSCALL_DEBUG}
{$ENDIF SYS_LINUX}


{*****************************************************************************
                     --- Main:The System Call Self ---
*****************************************************************************}


Procedure Do_SysCallspec( callnr:longint;var regs : SysCallregs );assembler;
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
{$ifdef cpui386}
{$define fpc_syscall_ok}
asm
{ load the registers... }
  movl 12(%ebp),%eax
  movl 4(%eax),%ebx
  movl 8(%eax),%ecx
  movl 12(%eax),%edx
  movl 16(%eax),%esi
  movl 20(%eax),%edi
{ set the call number }
  movl 8(%ebp),%eax
{ Go ! }
  int $0x80
{ Put back the registers... }
  pushl %eax
  movl 12(%ebp),%eax
  movl %edi,20(%eax)
  movl %esi,16(%eax)
  movl %edx,12(%eax)
  movl %ecx,8(%eax)
  movl %ebx,4(%eax)
  popl %ebx
  movl %ebx,(%eax)
end;
{$endif cpui386}
{$ifdef cpum68k}
{$define fpc_syscall_ok}
asm
{ load the registers... }
  move.l 12(a6),a0
  move.l 4(a0),d1
  move.l 8(a0),d2
  move.l 12(a0),d3
  move.l 16(a0),d4
  move.l 20(a0),d5
{ set the call number }
  move.l 8(a6),d0
{ Go ! }
  trap #0
{ Put back the registers... }
  move.l d0,-(sp)
  move.l 12(a6),a0
  move.l d5,20(a0)
  move.l d4,16(a0)
  move.l d3,12(a0)
  move.l d2,8(a0)
  move.l d1,4(a0)
  move.l (sp)+,d1
  move.l d1,(a0)
end;
{$endif cpum68k}
{$ifdef cpupowerpc}
{$define fpc_syscall_ok}
asm
{ load the registers... }
  lwz  r5, 12(r4)
  lwz  r6, 16(r4)
  lwz  r7, 20(r4)
  mr   r0, r3
  lwz  r3, 4(r4)
  stw  r4, regs
  lwz  r4, 8(r4)
{ Go ! }
  sc
  bns  Lsyscallo_ok
  neg  r3,r3
Lsyscallo_ok:
{ Put back the registers... }
  lwz    r8, regs
  stw    r3, 0(r8)
  stw    r4, 4(r8)
  stw    r5, 8(r8)
  stw    r6, 12(r8)
  stw    r7, 16(r8)
end;
{$endif cpupowerpc}
{$ifdef cpusparc}
{$define fpc_syscall_ok}
asm
{ we are using the callers register window }
  or   %i0,%g0,%g1
  ld   [%i1],%o0
  ld   [%i1+4],%o1
  ld   [%i1+8],%o2
  ld   [%i1+12],%o3
  ld   [%i1+16],%o4
{ Go ! }
  ta   0x10
{ Put back the registers... }
  st   %o0,[%i1]
  st   %o1,[%i1+4]
  st   %o2,[%i1+8]
  st   %o3,[%i1+12]
  st   %o4,[%i1+16]
end;
{$endif cpupowerpc}
{$ifndef fpc_syscall_ok}
{$error Cannot decide which processor you have!}
asm
end;
{$endif not fpc_syscall_ok}

{$IFDEF SYSCALL_DEBUG}
Const
  DoSysCallDebug : Boolean = False;

var
  LastCnt,
  LastEax,
  LastCall : longint;
  DebugTxt : string[20];
{$ENDIF}

Function SysCall( callnr:longint;var regs : SysCallregs ):longint;
{
  This function serves as an interface to do_SysCall.
  If the SysCall returned a negative number, it returns -1, and puts the
  SysCall result in errno. Otherwise, it returns the SysCall return value
}
begin
  do_SysCallspec(callnr,regs);
  if regs.reg1<0 then
   begin
{$IFDEF SYSCALL_DEBUG}
     If DoSysCallDebug then
       debugtxt:=' syscall error: ';
{$endif}
     fpseterrno(-regs.reg1);
     SysCall:=-1;
   end
  else
   begin
{$IFDEF SYSCALL_DEBUG}
  if DoSysCallDebug then
       debugtxt:=' syscall returned: ';
{$endif}
     SysCall:=regs.reg1;
     fpseterrno(0);
   end;
{$IFDEF SYSCALL_DEBUG}
  if DoSysCallDebug then
    begin
    inc(lastcnt);
    if (callnr<>lastcall) or (regs.reg1<>lasteax) then
      begin
      if lastcnt>1 then
        writeln(sys_nr_txt[lastcall],debugtxt,lasteax,' (',lastcnt,'x)');
      lastcall:=callnr;
      lasteax:=regs.reg1;
      lastcnt:=0;
      writeln(sys_nr_txt[lastcall],debugtxt,lasteax);
      end;
    end;
{$endif}
end;

{
  $Log$
  Revision 1.6  2003-09-14 20:15:01  marco
   * Unix reform stage two. Remove all calls from Unix that exist in Baseunix.

  Revision 1.5  2003/08/21 22:24:52  olle
    - removed parameter from fpc_iocheck

  Revision 1.4  2003/06/17 16:39:58  jonas
    * fixed old syscall handling for ppc

  Revision 1.3  2003/06/04 15:18:14  peter
    * compile fix for systhrds

  Revision 1.2  2003/04/22 17:07:55  florian
    * there where two SYSCALL1 procedures for the powerpc, fixed

  Revision 1.1  2002/11/11 21:40:26  marco
   * rename syscall.inc -> syscallo.inc

  Revision 1.1  2002/10/14 19:39:44  peter
    * syscall moved into seperate include
}

