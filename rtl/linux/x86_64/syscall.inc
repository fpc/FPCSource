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
  syscall                 { Do the system call. }
  cmpq $-4095, %rax       { Check %rax for error.  }
  jnae .LSyscOK           { Jump to error handler if error.  }
  negq  %rax
  movq  %rax,%rdx
  movq  fpc_threadvar_relocate_proc,%rax
  testq %rax,%rax
  jne   .LThread
  movq  %rdx,Errno+8
  jmp   .LNoThread
.LThread:
  pushq %rdx
  movq  Errno,%rdi
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
  movq  fpc_threadvar_relocate_proc,%rax
  testq %rax,%rax
  jne   .LThread
  movq  %rdx,Errno+8
  jmp   .LNoThread
.LThread:
  pushq %rdx
  movq  Errno,%rdi
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
  movq  fpc_threadvar_relocate_proc,%rax
  testq %rax,%rax
  jne   .LThread
  movq  %rdx,Errno+8
  jmp   .LNoThread
.LThread:
  pushq %rdx
  movq  Errno,%rdi
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
  movq  fpc_threadvar_relocate_proc,%rax
  testq %rax,%rax
  jne   .LThread
  movq  %rdx,Errno+8
  jmp   .LNoThread
.LThread:
  pushq %rdx
  movq  Errno,%rdi
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
  movq  fpc_threadvar_relocate_proc,%rax
  testq %rax,%rax
  jne   .LThread
  movq  %rdx,Errno+8
  jmp   .LNoThread
.LThread:
  pushq %rdx
  movq  Errno,%rdi
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
  movq  fpc_threadvar_relocate_proc,%rax
  testq %rax,%rax
  jne   .LThread
  movq  %rdx,Errno+8
  jmp   .LNoThread
.LThread:
  pushq %rdx
  movq  Errno,%rdi
  call  *%rax
  popq  %rdx
  movq  %rdx,(%rax)
.LNoThread:
  movq  $-1,%rax
.LSyscOK:
end;


function FpSysCall(sysnr,param1,param2,param3,param4,param5,param6 : TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL6'];

asm
  movq sysnr, %rax        { Syscall number -> rax.  }
  movq param1, %rdi         { shift arg1 - arg5. }
  movq param2, %rsi
  movq param3, %rdx
  movq param4, %r10
  movq param5, %r8
  movq param6, %r9
  syscall                 { Do the system call. }
  cmpq $-4095, %rax       { Check %rax for error.  }
  jnae .LSyscOK           { Jump to error handler if error.  }
  negq  %rax
  movq  %rax,%rdx
  movq  fpc_threadvar_relocate_proc,%rax
  testq %rax,%rax
  jne   .LThread
  movq  %rdx,Errno+8
  jmp   .LNoThread
.LThread:
  pushq %rdx
  movq  Errno,%rdi
  call  *%rax
  popq  %rdx
  movq  %rdx,(%rax)
.LNoThread:
  movq  $-1,%rax
.LSyscOK:
end;


{No debugging for syslinux include !}
{$IFDEF SYS_LINUX}
  {$UNDEF SYSCALL_DEBUG}
{$ENDIF SYS_LINUX}


{
  $Log$
  Revision 1.9  2004-12-12 14:30:27  peter
    * x86_64 updates

  Revision 1.8  2004/05/28 21:37:34  peter
    * fix errno for multithread

  Revision 1.7  2004/05/18 20:13:12  peter
    * fix offset of errno

  Revision 1.6  2004/05/16 11:04:27  marco
   * comment removed and four dud instrcutions

  Revision 1.5  2004/04/22 17:17:23  peter
    * x86-64 fixes

  Revision 1.4  2004/02/06 23:06:16  florian
    - killed tsyscallregs

  Revision 1.3  2004/02/06 15:58:21  florian
    * fixed x86-64 assembler problems

  Revision 1.2  2004/02/05 01:16:12  florian
    + completed x86-64/linux system unit

  Revision 1.1  2003/04/30 22:11:06  florian
    + for a lot of x86-64 dependend files mostly dummies added
}
