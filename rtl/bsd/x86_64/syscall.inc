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

function fpsysCall(sysnr:TSysParam):TSysResult; assembler;[public,alias:'FPC_DOSYS0'];

asm
  movq sysnr,%rax         { Syscall number -> rax.  }
  syscall                 { Do the system call. }
  jnb .LSyscOK            { branch to exit if ok, errorhandler otherwise}
  movq  %rax,%rdi
  call  SetErrno
  movq  $-1,%rax
.LSyscOK:
end;

function fpsysCall(sysnr,param1 : TSysParam):TSysResult; assembler;[public,alias:'FPC_DOSYS1'];

asm
  movq sysnr,%rax         { Syscall number -> rax.  }
  movq param1,%rdi        { shift arg1 - arg1. }
  syscall                 { Do the system call. }
  jnb .LSyscOK            { branch to exit if ok, errorhandler otherwise}
  movq  %rax,%rdi
  call  SetErrno
  movq  $-1,%rax
.LSyscOK:
end;

function fpsysCall(sysnr,param1,param2 : TSysParam):TSysResult; assembler;[public,alias:'FPC_DOSYS2'];

asm
  movq sysnr,%rax         { Syscall number -> rax.  }
  movq param1,%rdi        { shift arg1 - arg2. }
  movq param2,%rsi
  syscall                 { Do the system call. }
  jnb .LSyscOK            { branch to exit if ok, errorhandler otherwise}
  movq  %rax,%rdi
  call  SetErrno
  movq  $-1,%rax
.LSyscOK:
end;

function fpsysCall(sysnr,param1,param2,param3:TSysParam):TSysResult; assembler;[public,alias:'FPC_DOSYS3'];

asm
  movq sysnr,%rax         { Syscall number -> rax.  }
  movq param1,%rdi        { shift arg1 - arg3. }
  movq param2,%rsi
  movq param3,%rdx
  syscall                 { Do the system call. }
  jnb .LSyscOK            { branch to exit if ok, errorhandler otherwise}
  movq  %rax,%rdi
  call  SetErrno
  movq  $-1,%rax
.LSyscOK:
end;

function fpsysCall(sysnr,param1,param2,param3,param4:TSysParam):TSysResult; assembler;[public,alias:'FPC_DOSYS4'];

asm
  movq sysnr,%rax         { Syscall number -> rax.  }
  movq param1,%rdi        { shift arg1 - arg5. }
  movq param2,%rsi
  movq param3,%rdx
  movq param4,%r10
  syscall                 { Do the system call. }
  jnb .LSyscOK            { branch to exit if ok, errorhandler otherwise}
  movq  %rax,%rdi
  call  SetErrno
  movq  $-1,%rax
.LSyscOK:
end;

function fpsysCall(sysnr,param1,param2,param3,param4,param5 : TSysParam):TSysResult; assembler;[public,alias:'FPC_DOSYS5'];

asm
  movq sysnr,%rax         { Syscall number -> rax.  }
  movq param1,%rdi        { shift arg1 - arg5. }
  movq param2,%rsi
  movq param3,%rdx
  movq param4,%r10
  movq param5,%r8
  syscall                 { Do the system call. }
  jnb .LSyscOK            { branch to exit if ok, errorhandler otherwise}
  movq  %rax,%rdi
  call  SetErrno
  movq  $-1,%rax
.LSyscOK:
end;

function fpsysCall(sysnr,param1,param2,param3,param4,param5,param6 : TSysParam):TSysResult; assembler;[public,alias:'FPC_DOSYS6'];

asm
  movq sysnr,%rax         { Syscall number -> rax.  }
  movq param1,%rdi        { shift arg1 - arg6. }
  movq param2,%rsi
  movq param3,%rdx
  movq param4,%r10
  movq param5,%r8
  movq param6,%r9
  syscall                 { Do the system call. }
  jnb .LSyscOK            { branch to exit if ok, errorhandler otherwise}
  movq  %rax,%rdi
  call  SetErrno
  movq  $-1,%rax
.LSyscOK:

end;

function fp_sysCall(sysnr,param1,param2,param3,param4,param5,param6,param7 : TSysParam):TSysResult; assembler;[public,alias:'FPC_DOSYS7'];

asm
  subq   $0x10,%rsp
  movq   sysnr,%rax       { Syscall number -> rax.  }
  movq   param1,%rdi      { shift arg1 - arg6. }
  movq   param2,%rsi
  movq   param3,%rdx
  movq   param4,%r10
  movq   param5,%r8
  movq   param6,%r9
  movq   param7,%r11
  movq   %r11,8(%rsp)
  syscall                 { Do the system call. }
  jnb .LSyscOK            { branch to exit if ok, errorhandler otherwise}
  movq  %rax,%rdi
  call  SetErrno
  movq  $-1,%rax
.LSyscOK:
  addq   $0x10,%rsp
end;

