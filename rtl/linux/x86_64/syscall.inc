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


{$ASMMODE GAS}

function FpSysCall(sysnr:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL0'];

asm
  movq sysnr, %rax        { Syscall number -> rax.  }
  movq %rsi, %rdi         { shift arg1 - arg5. }
  movq %rdx, %rsi
  movq %rcx, %rdx
  movq %r8, %r10
  movq %r9, %r8
  syscall                 { Do the system call. }
  cmpq $-4095, %rax       { Check %rax for error.  }
  jnae .LSyscOK           { Jump to error handler if error.  }  
  negq  %rax
  movq  %rax,%rdx
  movq  FPC_THREADVAR_RELOCATE,%rax
  testq %rax,%rax
  jne   .LThread
  movq  %rdx,Errno+4
  jmp   .LNoThread
.LThread:
  pushq %rdx
  pushq Errno
  call  *%rax
  popq  %rdx
  movq  %rdx,(%rax)
.LNoThread:
  movq  $-1,%rax
.LSyscOK:
end;

function FpSysCall(sysnr,param1 : TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL1'];

asm
  movq sysnr, %rax        { Syscall number -> rax.  }
  movq param1, %rdi         { shift arg1 - arg5. }
  syscall                 { Do the system call. }
  cmpq $-4095, %rax       { Check %rax for error.  }
  jnae .LSyscOK           { Jump to error handler if error.  }  
  negq  %rax
  movq  %rax,%rdx
  movq  FPC_THREADVAR_RELOCATE,%rax
  testq %rax,%rax
  jne   .LThread
  movq  %rdx,Errno+4
  jmp   .LNoThread
.LThread:
  pushq %rdx
  pushq Errno
  call  *%rax
  popq  %rdx
  movq  %rdx,(%rax)
.LNoThread:
  movq  $-1,%rax
.LSyscOK:
end;

function FpSysCall(sysnr,param1,param2 : TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL2'];

asm
  movq sysnr, %rax        { Syscall number -> rax.  }
  movq param1, %rdi         { shift arg1 - arg5. }
  movq param2, %rsi
  syscall                 { Do the system call. }
  cmpq $-4095, %rax       { Check %rax for error.  }
  jnae .LSyscOK           { Jump to error handler if error.  }  
  negq  %rax
  movq  %rax,%rdx
  movq  FPC_THREADVAR_RELOCATE,%rax
  testq %rax,%rax
  jne   .LThread
  movq  %rdx,Errno+4
  jmp   .LNoThread
.LThread:
  pushq %rdx
  pushq Errno
  call  *%rax
  popq  %rdx
  movq  %rdx,(%rax)
.LNoThread:
  movq  $-1,%rax
.LSyscOK:
end;

function FpSysCall(sysnr,param1,param2,param3:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL3'];

asm
  movq sysnr, %rax        { Syscall number -> rax.  }
  movq param1, %rdi         { shift arg1 - arg5. }
  movq param2, %rsi
  movq param3, %rdx
  syscall                 { Do the system call. }
  cmpq $-4095, %rax       { Check %rax for error.  }
  jnae .LSyscOK           { Jump to error handler if error.  }  
  negq  %rax
  movq  %rax,%rdx
  movq  FPC_THREADVAR_RELOCATE,%rax
  testq %rax,%rax
  jne   .LThread
  movq  %rdx,Errno+4
  jmp   .LNoThread
.LThread:
  pushq %rdx
  pushq Errno
  call  *%rax
  popq  %rdx
  movq  %rdx,(%rax)
.LNoThread:
  movq  $-1,%rax
.LSyscOK:
end;

function FpSysCall(sysnr,param1,param2,param3,param4:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL4'];

asm
  movq sysnr, %rax        { Syscall number -> rax.  }
  movq param1, %rdi         { shift arg1 - arg5. }
  movq param2, %rsi
  movq param3, %rdx
  movq param4, %r10
  syscall                 { Do the system call. }
  cmpq $-4095, %rax       { Check %rax for error.  }
  jnae .LSyscOK           { Jump to error handler if error.  }  
  negq  %rax
  movq  %rax,%rdx
  movq  FPC_THREADVAR_RELOCATE,%rax
  testq %rax,%rax
  jne   .LThread
  movq  %rdx,Errno+4
  jmp   .LNoThread
.LThread:
  pushq %rdx
  pushq Errno
  call  *%rax
  popq  %rdx
  movq  %rdx,(%rax)
.LNoThread:
  movq  $-1,%rax
.LSyscOK:
end;

function FpSysCall(sysnr,param1,param2,param3,param4,param5 : TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL5'];

asm
  movq sysnr, %rax        { Syscall number -> rax.  }
  movq param1, %rdi         { shift arg1 - arg5. }
  movq param2, %rsi
  movq param3, %rdx
  movq param4, %r10
  movq param5, %r8
  syscall                 { Do the system call. }
  cmpq $-4095, %rax       { Check %rax for error.  }
  jnae .LSyscOK           { Jump to error handler if error.  }  
  negq  %rax
  movq  %rax,%rdx
  movq  FPC_THREADVAR_RELOCATE,%rax
  testq %rax,%rax
  jne   .LThread
  movq  %rdx,Errno+4
  jmp   .LNoThread
.LThread:
  pushq %rdx
  pushq Errno
  call  *%rax
  popq  %rdx
  movq  %rdx,(%rax)
.LNoThread:
  movq  $-1,%rax
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
{$define fpc_syscall_ok}
asm
  pushq %rdi
  movq sysnr, %rax        { Syscall number -> rax.  }
  movq 8(%rdi),%rsi       { load paras }
  movq 16(%rdi),%rdx
  movq 24(%rdi),%r10
  movq 32(%rdi),%r8
  movq (%rdi),%rdi
  syscall                 { Do the system call. }
  popq %rdi
  movq %rax,(%rdi)
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
  if regs.reg1>=$fffffffffffff001 then
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
  Revision 1.3  2004-02-06 15:58:21  florian
    * fixed x86-64 assembler problems

  Revision 1.2  2004/02/05 01:16:12  florian
    + completed x86-64/linux system unit

  Revision 1.1  2003/04/30 22:11:06  florian
    + for a lot of x86-64 dependend files mostly dummies added
}
