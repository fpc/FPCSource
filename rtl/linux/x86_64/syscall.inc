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
  testl %eax,%eax
  jns   .LSyscOK
  negl  %eax
{$ifdef VER1_0}
  movl  %eax,Errno
{$else}
  movl  %eax,%edx
  movl  FPC_THREADVAR_RELOCATE,%eax
  testl %eax,%eax
  jne   .LThread
  movl  %edx,Errno+4
  jmp   .LNoThread
.LThread:
  pushl %edx
  pushl Errno
  call  *%eax
  popl  %edx
  movl  %edx,(%eax)
.LNoThread:
  movl  $-1,%eax
{$endif}
.LSyscOK:
end;

function FpSysCall(sysnr,param1 : TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL1'];

asm
{ load the registers... }
  movl sysnr,%eax
  movl param1,%ebx
  int $0x80
  testl %eax,%eax
  jns   .LSyscOK
  negl  %eax
{$ifdef VER1_0}
  movl  %eax,Errno
{$else}
  movl  %eax,%edx
  movl  FPC_THREADVAR_RELOCATE,%eax
  testl %eax,%eax
  jne   .LThread
  movl  %edx,Errno+4
  jmp   .LNoThread
.LThread:
  pushl %edx
  pushl Errno
  call  *%eax
  popl  %edx
  movl  %edx,(%eax)
.LNoThread:
  movl  $-1,%eax
{$endif}
.LSyscOK:
end;

function FpSysCall(sysnr,param1,param2 : TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL2'];

asm
{ load the registers... }
  movl sysnr,%eax
  movl param1,%ebx
  movl param2,%ecx
  int $0x80
  testl %eax,%eax
  jns   .LSyscOK
  negl  %eax
{$ifdef VER1_0}
  movl  %eax,Errno
{$else}
  movl  %eax,%edx
  movl  FPC_THREADVAR_RELOCATE,%eax
  testl %eax,%eax
  jne   .LThread
  movl  %edx,Errno+4
  jmp   .LNoThread
.LThread:
  pushl %edx
  pushl Errno
  call  *%eax
  popl  %edx
  movl  %edx,(%eax)
.LNoThread:
  movl  $-1,%eax
{$endif}
.LSyscOK:
end;

function FpSysCall(sysnr,param1,param2,param3:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL3'];

asm
{ load the registers... }
  movl sysnr,%eax
  movl param1,%ebx
  movl param2,%ecx
  movl param3,%edx
  int $0x80
  testl %eax,%eax
  jns   .LSyscOK
  negl  %eax
{$ifdef VER1_0}
  movl  %eax,Errno
{$else}
  movl  %eax,%edx
  movl  FPC_THREADVAR_RELOCATE,%eax
  testl %eax,%eax
  jne   .LThread
  movl  %edx,Errno+4
  jmp   .LNoThread
.LThread:
  pushl %edx
  pushl Errno
  call  *%eax
  popl  %edx
  movl  %edx,(%eax)
.LNoThread:
  movl  $-1,%eax
{$endif}
.LSyscOK:
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
  testl %eax,%eax
  jns   .LSyscOK
  negl  %eax
{$ifdef VER1_0}
  movl  %eax,Errno
{$else}
  movl  %eax,%edx
  movl  FPC_THREADVAR_RELOCATE,%eax
  testl %eax,%eax
  jne   .LThread
  movl  %edx,Errno+4
  jmp   .LNoThread
.LThread:
  pushl %edx
  pushl Errno
  call  *%eax
  popl  %edx
  movl  %edx,(%eax)
.LNoThread:
  movl  $-1,%eax
{$endif}
.LSyscOK:
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
  testl %eax,%eax
  jns   .LSyscOK
  negl  %eax
{$ifdef VER1_0}
  movl  %eax,Errno
{$else}
  movl  %eax,%edx
  movl  FPC_THREADVAR_RELOCATE,%eax
  testl %eax,%eax
  jne   .LThread
  movl  %edx,Errno+4
  jmp   .LNoThread
.LThread:
  pushl %edx
  pushl Errno
  call  *%eax
  popl  %edx
  movl  %edx,(%eax)
.LNoThread:
  movl  $-1,%eax
{$endif}
.LSyscOK:
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
  testl %eax,%eax
  jns   .LSyscOK
  negl  %eax
{$ifdef VER1_0}
  movl  %eax,Errno
{$else}
  movl  %eax,%edx
  movl  FPC_THREADVAR_RELOCATE,%eax
  testl %eax,%eax
  jne   .LThread
  movl  %edx,Errno+4
  jmp   .LNoThread
.LThread:
  pushl %edx
  pushl Errno
  call  *%eax
  popl  %edx
  movl  %edx,(%eax)
.LNoThread:
  movl  $-1,%eax
{$endif}
.LSyscOK:
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
  Revision 1.1  2003-04-30 22:11:06  florian
    + for a lot of x86-64 dependend files mostly dummies added
}

