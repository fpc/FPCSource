{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt,
    member of the Free Pascal development team.

    The syscalls for the new RTL, moved to platform dependant dir.
    Old linux calling convention is stil kept.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{$ASMMODE ATT}

function FpSysCall(sysnr:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL0'];

asm
{ load the registers... }
  movl  sysnr,%eax
  int   $0x80
  movl  %eax,%ecx
  xorl  %edx,%edx
  testl %eax,%eax
  jns   .LSyscOK
  negl  %eax
  movl  $-1,%ecx
  movl  %eax,%edx
.LSyscOK:
{$ifdef VER1_0}
  movl  %edx,Errno
  movl  %ecx,%eax
{$else}
  movl  FPC_THREADVAR_RELOCATE,%eax
  testl %eax,%eax
  jne   .LThread
  movl  %edx,Errno+4
  movl  %ecx,%eax
  jmp   .LNoThread
.LThread:
  pushl %ecx
  pushl Errno
  call  *%eax
  movl  %edx,(%eax)
  popl  %eax
.LNoThread:
{$endif}
end;

function FpSysCall(sysnr,param1 : TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL1'];

asm
{ load the registers... }
  movl sysnr,%eax
  movl param1,%ebx
  int $0x80
  movl  %eax,%ecx
  xorl  %edx,%edx
  testl %eax,%eax
  jns   .LSyscOK
  negl  %eax
  movl  $-1,%ecx
  movl  %eax,%edx
.LSyscOK:
{$ifdef VER1_0}
  movl  %edx,Errno
  movl  %ecx,%eax
{$else}
  movl  FPC_THREADVAR_RELOCATE,%eax
  testl %eax,%eax
  jne   .LThread
  movl  %edx,Errno+4
  movl  %ecx,%eax
  jmp   .LNoThread
.LThread:
  pushl %ecx
  pushl Errno
  call  *%eax
  movl  %edx,(%eax)
  popl  %eax
.LNoThread:
{$endif}
end;

function FpSysCall(sysnr,param1,param2 : TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL2'];

asm
{ load the registers... }
  movl sysnr,%eax
  movl param1,%ebx
  movl param2,%ecx
  int $0x80
  movl  %eax,%ecx
  xorl  %edx,%edx
  testl %eax,%eax
  jns   .LSyscOK
  negl  %eax
  movl  $-1,%ecx
  movl  %eax,%edx
.LSyscOK:
{$ifdef VER1_0}
  movl  %edx,Errno
  movl  %ecx,%eax
{$else}
  movl  FPC_THREADVAR_RELOCATE,%eax
  testl %eax,%eax
  jne   .LThread
  movl  %edx,Errno+4
  movl  %ecx,%eax
  jmp   .LNoThread
.LThread:
  pushl %ecx
  pushl Errno
  call  *%eax
  movl  %edx,(%eax)
  popl  %eax
.LNoThread:
{$endif}
end;

function FpSysCall(sysnr,param1,param2,param3:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL3'];

asm
{ load the registers... }
  movl sysnr,%eax
  movl param1,%ebx
  movl param2,%ecx
  movl param3,%edx
  int $0x80
  movl  %eax,%ecx
  xorl  %edx,%edx
  testl %eax,%eax
  jns   .LSyscOK
  negl  %eax
  movl  $-1,%ecx
  movl  %eax,%edx
.LSyscOK:
{$ifdef VER1_0}
  movl  %edx,Errno
  movl  %ecx,%eax
{$else}
  movl  FPC_THREADVAR_RELOCATE,%eax
  testl %eax,%eax
  jne   .LThread
  movl  %edx,Errno+4
  movl  %ecx,%eax
  jmp   .LNoThread
.LThread:
  pushl %ecx
  pushl Errno
  call  *%eax
  movl  %edx,(%eax)
  popl  %eax
.LNoThread:
{$endif}
end;

function FpSysCall(sysnr,param1,param2,param3,param4:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL4'];

asm
{ load the registers... }
  movl sysnr,%eax
  movl param1,%ebx
  movl param2,%ecx
  movl param3,%edx
  movl param4,%esi
  int $0x80
  movl  %eax,%ecx
  xorl  %edx,%edx
  testl %eax,%eax
  jns   .LSyscOK
  negl  %eax
  movl  $-1,%ecx
  movl  %eax,%edx
.LSyscOK:
{$ifdef VER1_0}
  movl  %edx,Errno
  movl  %ecx,%eax
{$else}
  movl  FPC_THREADVAR_RELOCATE,%eax
  testl %eax,%eax
  jne   .LThread
  movl  %edx,Errno+4
  movl  %ecx,%eax
  jmp   .LNoThread
.LThread:
  pushl %ecx
  pushl Errno
  call  *%eax
  movl  %edx,(%eax)
  popl  %eax
.LNoThread:
{$endif}
end;

function FpSysCall(sysnr,param1,param2,param3,param4,param5 : TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL5'];

asm
{ load the registers... }
  movl sysnr,%eax
  movl param1,%ebx
  movl param2,%ecx
  movl param3,%edx
  movl param4,%esi
  movl param5,%edi
  int $0x80
  movl  %eax,%ecx
  xorl  %edx,%edx
  testl %eax,%eax
  jns   .LSyscOK
  negl  %eax
  movl  $-1,%ecx
  movl  %eax,%edx
.LSyscOK:
{$ifdef VER1_0}
  movl  %edx,Errno
  movl  %ecx,%eax
{$else}
  movl  FPC_THREADVAR_RELOCATE,%eax
  testl %eax,%eax
  jne   .LThread
  movl  %edx,Errno+4
  movl  %ecx,%eax
  jmp   .LNoThread
.LThread:
  pushl %ecx
  pushl Errno
  call  *%eax
  movl  %edx,(%eax)
  popl  %eax
.LNoThread:
{$endif}
end;

{$ifdef notsupported}
{ Only 5 params are pushed, so it'll not work as expected (PFV) }
function FpSysCall(sysnr,param1,param2,param3,param4,param5,param6 : TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL6'];

asm
{ load the registers... }
  movl sysnr,%eax
  movl param1,%ebx
  movl param2,%ecx
  movl param3,%edx
  movl param4,%esi
  movl param5,%edi
  int $0x80
  movl  %eax,%ecx
  xorl  %edx,%edx
  testl %eax,%eax
  jns   .LSyscOK
  negl  %eax
  movl  $-1,%ecx
  movl  %eax,%edx
.LSyscOK:
{$ifdef VER1_0}
  movl  %edx,Errno
  movl  %ecx,%eax
{$else}
  movl  FPC_THREADVAR_RELOCATE,%eax
  testl %eax,%eax
  jne   .LThread
  movl  %edx,Errno+4
  movl  %ecx,%eax
  jmp   .LNoThread
.LThread:
  pushl %ecx
  pushl Errno
  call  *%eax
  movl  %edx,(%eax)
  popl  %eax
.LNoThread:
{$endif}
end;
{$endif notsupported}

{No debugging for syslinux include !}
{$IFDEF SYS_LINUX}
  {$UNDEF SYSCALL_DEBUG}
{$ENDIF SYS_LINUX}

{*****************************************************************************
                     --- Main:The System Call Self ---
*****************************************************************************}

Procedure FpSysCall( callnr:TSysParam;var regs : SysCallregs );assembler;
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
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

Function SysCall( callnr:longint;var regs : SysCallregs ):longint;
{
  This function serves as an interface to do_SysCall.
  If the SysCall returned a negative number, it returns -1, and puts the
  SysCall result in errno. Otherwise, it returns the SysCall return value
}
begin
  FpSysCall(callnr,regs);
  if regs.reg1<0 then
   begin
{$IFDEF SYSCALL_DEBUG}
     If DoSysCallDebug then
       debugtxt:=' syscall error: ';
{$endif}
     setErrNo(-regs.reg1);
     SysCall:=-1;
   end
  else
   begin
{$IFDEF SYSCALL_DEBUG}
  if DoSysCallDebug then
       debugtxt:=' syscall returned: ';
{$endif}
     SysCall:=regs.reg1;
     seterrno(0);
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
  Revision 1.6  2002-12-23 21:17:53  peter
    * MT fix

  Revision 1.5  2002/12/23 20:56:32  peter
    * Reset Errno to 0 if call is successfull
    * Fix broken 1.0.x Errno which does not have threadvar

  Revision 1.4  2002/12/18 20:41:33  peter
    * Threadvar support for Errno
    * Fixed syscall error return check
    * Uncommented Syscall with 6 parameters, only 5 were really set

  Revision 1.3  2002/12/18 16:46:37  marco
   * Some mods.

  Revision 1.2  2002/11/16 15:37:47  marco
   * TSysParam + result moved to -h

  Revision 1.1  2002/11/12 13:31:32  marco
   * New syscall.inc


}
 