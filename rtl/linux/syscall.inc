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

Procedure Do_SysCall( callnr:longint;var regs : SysCallregs );assembler;
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
{$ifdef i386}
{$ASMMODE ATT}
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
{$ASMMODE DEFAULT}
{$endif i386}
{$ifdef m68k}
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
{$endif m68k}
{$ifdef powerpc}
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
  nop
{ Put back the registers... }
  lwz    r8, regs
  stw    r3, 0(r8)
  stw    r4, 4(r8)
  stw    r5, 8(r8)
  stw    r6, 12(r8)
  stw    r7, 16(r8)
end;
{$endif powerpc}
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
  do_SysCall(callnr,regs);
  if regs.reg1<0 then
   begin
{$IFDEF SYSCALL_DEBUG}
     If DoSysCallDebug then
       debugtxt:=' syscall error: ';
{$endif}
     ErrNo:=-regs.reg1;
     SysCall:=-1;
   end
  else
   begin
{$IFDEF SYSCALL_DEBUG}
  if DoSysCallDebug then
       debugtxt:=' syscall returned: ';
{$endif}
     SysCall:=regs.reg1;
     errno:=0
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
  Revision 1.1  2002-10-14 19:39:44  peter
    * syscall moved into seperate include

}
 