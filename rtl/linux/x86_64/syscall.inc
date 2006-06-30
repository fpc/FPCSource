{
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
{$ifdef FPC_PIC}
  movq  fpc_threadvar_relocate_proc@GOTPCREL(%rip),%rax
  movq  (%rax),%rax
  movq  Errno@GOTPCREL(%rip),%r11
{$else FPC_PIC}
  movq  fpc_threadvar_relocate_proc,%rax
  leaq  Errno,%r11
{$endif FPC_PIC}
  testq %rax,%rax
  jne   .LThread
  movl  %edx,8(%r11)
  jmp   .LNoThread
.LThread:
  pushq %rdx
  movq  (%r11),%rdi
  call  *%rax
  popq  %rdx
  movl  %edx,(%rax)
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
{$ifdef FPC_PIC}
  movq  fpc_threadvar_relocate_proc@GOTPCREL(%rip),%rax
  movq  (%rax),%rax
  movq  Errno@GOTPCREL(%rip),%r11
{$else FPC_PIC}
  movq  fpc_threadvar_relocate_proc,%rax
  leaq  Errno,%r11
{$endif FPC_PIC}
  testq %rax,%rax
  jne   .LThread
  movl  %edx,8(%r11)
  jmp   .LNoThread
.LThread:
  pushq %rdx
  movq  (%r11),%rdi
  call  *%rax
  popq  %rdx
  movl  %edx,(%rax)
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
{$ifdef FPC_PIC}
  movq  fpc_threadvar_relocate_proc@GOTPCREL(%rip),%rax
  movq  (%rax),%rax
  movq  Errno@GOTPCREL(%rip),%r11
{$else FPC_PIC}
  movq  fpc_threadvar_relocate_proc,%rax
  leaq  Errno,%r11
{$endif FPC_PIC}
  testq %rax,%rax
  jne   .LThread
  movl  %edx,8(%r11)
  jmp   .LNoThread
.LThread:
  pushq %rdx
  movq  (%r11),%rdi
  call  *%rax
  popq  %rdx
  movl  %edx,(%rax)
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
{$ifdef FPC_PIC}
  movq  fpc_threadvar_relocate_proc@GOTPCREL(%rip),%rax
  movq  (%rax),%rax
  movq  Errno@GOTPCREL(%rip),%r11
{$else FPC_PIC}
  movq  fpc_threadvar_relocate_proc,%rax
  leaq  Errno,%r11
{$endif FPC_PIC}
  testq %rax,%rax
  jne   .LThread
  movl  %edx,8(%r11)
  jmp   .LNoThread
.LThread:
  pushq %rdx
  movq  (%r11),%rdi
  call  *%rax
  popq  %rdx
  movl  %edx,(%rax)
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
{$ifdef FPC_PIC}
  movq  fpc_threadvar_relocate_proc@GOTPCREL(%rip),%rax
  movq  (%rax),%rax
  movq  Errno@GOTPCREL(%rip),%r11
{$else FPC_PIC}
  movq  fpc_threadvar_relocate_proc,%rax
  leaq  Errno,%r11
{$endif FPC_PIC}
  testq %rax,%rax
  jne   .LThread
  movl  %edx,8(%r11)
  jmp   .LNoThread
.LThread:
  pushq %rdx
  movq  (%r11),%rdi
  call  *%rax
  popq  %rdx
  movl  %edx,(%rax)
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
{$ifdef FPC_PIC}
  movq  fpc_threadvar_relocate_proc@GOTPCREL(%rip),%rax
  movq  (%rax),%rax
  movq  Errno@GOTPCREL(%rip),%r11
{$else FPC_PIC}
  movq  fpc_threadvar_relocate_proc,%rax
  leaq  Errno,%r11
{$endif FPC_PIC}
  testq %rax,%rax
  jne   .LThread
  movl  %edx,8(%r11)
  jmp   .LNoThread
.LThread:
  pushq %rdx
  movq  (%r11),%rdi
  call  *%rax
  popq  %rdx
  movl  %edx,(%rax)
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
{$ifdef FPC_PIC}
  movq  fpc_threadvar_relocate_proc@GOTPCREL(%rip),%rax
  movq  (%rax),%rax
  movq  Errno@GOTPCREL(%rip),%r11
{$else FPC_PIC}
  movq  fpc_threadvar_relocate_proc,%rax
  leaq  Errno,%r11
{$endif FPC_PIC}
  testq %rax,%rax
  jne   .LThread
  movl  %edx,8(%r11)
  jmp   .LNoThread
.LThread:
  pushq %rdx
  movq  (%r11),%rdi
  call  *%rax
  popq  %rdx
  movl  %edx,(%rax)
.LNoThread:
  movq  $-1,%rax
.LSyscOK:
end;


{No debugging for syslinux include !}
{$IFDEF SYS_LINUX}
  {$UNDEF SYSCALL_DEBUG}
{$ENDIF SYS_LINUX}
