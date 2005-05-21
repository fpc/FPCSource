{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2004 Marco van de Voort
    member of the Free Pascal development team.

    The syscalls for the *BSD AMD64 rtl

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
 Origin of this file: copied from linux/x86_64 dir, blended with the
        freebsd x86 changes and checked against objdump of a
        x86_64/freebsdprog
- jge directly behind the syscall to branch on non-error
- rcx is used as scratch reg (fpc/Linux-x86_64 uses edx)
- More 6 and 7 param dosyscall because of the __syscall problem
}

{$ASMMODE GAS}

function do_sysCall(sysnr:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL0'];

asm
  movq sysnr, %rax        { Syscall number -> rax.  }
  syscall                 { Do the system call. }
  jge .LSyscOK            { branch to exit if ok, errorhandler otherwise}
  movq  %rax,%rcx
  movq  FPC_THREADVAR_RELOCATE,%rax
  testq %rax,%rax
  jne   .LThread
  movq  %rcx,Errno+8
  jmp   .LNoThread
.LThread:
  pushq %rcx
  pushq Errno
  call  *%rax
  popq  %rcx
  movq  %rcx,(%rax)
.LNoThread:
  movq  $-1,%rax
  movq  rax,%rdx
.LSyscOK:
end;

function do_sysCall(sysnr,param1 : TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL1'];

asm
  movq sysnr, %rax        { Syscall number -> rax.  }
  movq param1, %rdi         { shift arg1 - arg1. }
  syscall                 { Do the system call. }
  jge .LSyscOK            { branch to exit if ok, errorhandler otherwise}
  movq  %rax,%rcx
  movq  FPC_THREADVAR_RELOCATE,%rax
  testq %rax,%rax
  jne   .LThread
  movq  %rcx,Errno+8
  jmp   .LNoThread
.LThread:
  pushq %rcx
  pushq Errno
  call  *%rax
  popq  %rcx
  movq  %rcx,(%rax)
.LNoThread:
  movq  $-1,%rax
  movq  rax,%rdx
.LSyscOK:
end;

function do_sysCall(sysnr,param1,param2 : TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL2'];

asm
  movq sysnr, %rax        { Syscall number -> rax.  }
  movq param1, %rdi         { shift arg1 - arg2. }
  movq param2, %rsi
  mov %rcx,%r10
  syscall                 { Do the system call. }
  jge .LSyscOK            { branch to exit if ok, errorhandler otherwise}
  movq  %rax,%rcx
  movq  FPC_THREADVAR_RELOCATE,%rax
  testq %rax,%rax
  jne   .LThread
  movq  %rcx,Errno+8
  jmp   .LNoThread
.LThread:
  pushq %rcx
  pushq Errno
  call  *%rax
  popq  %rcx
  movq  %rcx,(%rax)
.LNoThread:
  movq  $-1,%rax
  movq  rax,%rdx
.LSyscOK:
end;

function do_sysCall(sysnr,param1,param2,param3:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL3'];

asm
  movq sysnr, %rax        { Syscall number -> rax.  }
  movq param1, %rdi         { shift arg1 - arg3. }
  movq param2, %rsi
  movq param3, %rdx
  mov %rcx,%r10
  syscall                 { Do the system call. }
  jge .LSyscOK            { branch to exit if ok, errorhandler otherwise}
  movq  %rax,%rcx
  movq  FPC_THREADVAR_RELOCATE,%rax
  testq %rax,%rax
  jne   .LThread
  movq  %rcx,Errno+8
  jmp   .LNoThread
.LThread:
  pushq %rcx
  pushq Errno
  call  *%rax
  popq  %rcx
  movq  %rcx,(%rax)
.LNoThread:
  movq  $-1,%rax
  movq  rax,%rdx
.LSyscOK:
end;

function do_sysCall(sysnr,param1,param2,param3,param4:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL4'];

asm
  movq sysnr, %rax        { Syscall number -> rax.  }
  movq param1, %rdi         { shift arg1 - arg5. }
  movq param2, %rsi
  movq param3, %rdx
  movq param4, %r10
  mov %rcx,%r10
  syscall                 { Do the system call. }
  jge .LSyscOK            { branch to exit if ok, errorhandler otherwise}
  movq  %rax,%rcx
  movq  FPC_THREADVAR_RELOCATE,%rax
  testq %rax,%rax
  jne   .LThread
  movq  %rcx,Errno+8
  jmp   .LNoThread
.LThread:
  pushq %rcx
  pushq Errno
  call  *%rax
  popq  %rcx
  movq  %rcx,(%rax)
.LNoThread:
  movq  $-1,%rax
  movq  rax,%rdx
.LSyscOK:
end;

function do_sysCall(sysnr,param1,param2,param3,param4,param5 : TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL5'];

asm
  movq sysnr, %rax        { Syscall number -> rax.  }
  movq param1, %rdi         { shift arg1 - arg5. }
  movq param2, %rsi
  movq param3, %rdx
  movq param4, %r10
  movq param5, %r8
  mov %rcx,%r10
  syscall                 { Do the system call. }
  jge .LSyscOK            { branch to exit if ok, errorhandler otherwise}
  movq  %rax,%rcx
  movq  FPC_THREADVAR_RELOCATE,%rax
  testq %rax,%rax
  jne   .LThread
  movq  %rcx,Errno+8
  jmp   .LNoThread
.LThread:
  pushq %rcx
  pushq Errno
  call  *%rax
  popq  %rcx
  movq  %rcx,(%rax)
.LNoThread:
  movq  $-1,%rax
  movq  rax,%rdx
.LSyscOK:
end;


function do_sysCall(sysnr,param1,param2,param3,param4,param5,param6 : TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL6'];

asm
  movq sysnr, %rax        { Syscall number -> rax.  }
  movq param1, %rdi         { shift arg1 - arg6. }
  movq param2, %rsi
  movq param3, %rdx
  movq param4, %r10
  movq param5, %r8
  movq param6, %r9
  mov %rcx,%r10
  syscall                 { Do the system call. }
  jge .LSyscOK            { branch to exit if ok, errorhandler otherwise}
  movq  %rax,%rcx
  movq  FPC_THREADVAR_RELOCATE,%rax
  testq %rax,%rax
  jne   .LThread
  movq  %rcx,Errno+8
  jmp   .LNoThread
.LThread:
  pushq %rcx
  pushq Errno
  call  *%rax
  popq  %rcx
  movq  %rcx,(%rax)
.LNoThread:
  movq  $-1,%rax
  movq  rax,%rdx
.LSyscOK:
end;

procedure actualsyscall; assembler; {inline requires a dummy push IIRC}
    asm
     syscall
     jge .LSyscOK            { branch to exit if ok, errorhandler otherwise}
     movq  %rax,%rcx
     movq  FPC_THREADVAR_RELOCATE,%rax
     testq %rax,%rax
     jne   .LThread
     movq  %rcx,Errno+8
     jmp   .LNoThread
   .LThread:
     pushq %rcx
     pushq Errno
     call  *%rax
     popq  %rcx
     movq  %rcx,(%rax)
   .LNoThread:
     movq  $-1,%rax
     movq  rax,%rdx
   .LSyscOK:
end;

function do__sysCall(sysnr,param1,param2,param3,param4,param5,param6,param7,Param8:TSysParam):TSysResult; oldfpccall; external name 'FPC_DOSYS8';
// Hmm, we have to do something different :)

asm
  movq param8,%rax
  push %rax
  movq param7,%rax
  push %rax
  movq $syscall_nr__syscall,  %rax
  mov %rcx,%r10
  call actualsyscall
  add $16,%rsp
end;


{
  $Log: syscall.inc,v $
  Revision 1.4  2005/02/14 17:13:21  peter
    * truncate log

}
