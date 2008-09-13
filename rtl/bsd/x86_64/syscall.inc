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

function fpsysCall(sysnr,param1 : TSysParam):TSysResult; assembler;[public,alias:'FPC_DOSYS1'];

asm
  movq sysnr,%rax         { Syscall number -> rax.  }
  movq param1,%rdi        { shift arg1 - arg1. }
  syscall                 { Do the system call. }
  jb .LSyscOK             { branch to exit if ok, errorhandler otherwise}
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

function fpsysCall(sysnr,param1,param2 : TSysParam):TSysResult; assembler;[public,alias:'FPC_DOSYS2'];

asm
  movq sysnr,%rax         { Syscall number -> rax.  }
  movq param1,%rdi        { shift arg1 - arg2. }
  movq param2,%rsi
  syscall                 { Do the system call. }
  jnb .LSyscOK            { branch to exit if ok, errorhandler otherwise}
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

function fpsysCall(sysnr,param1,param2,param3:TSysParam):TSysResult; assembler;[public,alias:'FPC_DOSYS3'];

asm
  movq sysnr,%rax         { Syscall number -> rax.  }
  movq param1,%rdi        { shift arg1 - arg3. }
  movq param2,%rsi
  movq param3,%rdx
  syscall                 { Do the system call. }
  jnb .LSyscOK            { branch to exit if ok, errorhandler otherwise}
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

function fpsysCall(sysnr,param1,param2,param3,param4:TSysParam):TSysResult; assembler;[public,alias:'FPC_DOSYS4'];

asm
  movq sysnr,%rax         { Syscall number -> rax.  }
  movq param1,%rdi        { shift arg1 - arg5. }
  movq param2,%rsi
  movq param3,%rdx
  movq param4,%r10
  syscall                 { Do the system call. }
  jnb .LSyscOK            { branch to exit if ok, errorhandler otherwise}
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
//                    edi   esi   edx 	 ecx    r8      r9   stack
function fp_sysCall(sysnr,param1,param2,param3,param4,param5,param6 : TSysParam):TSysResult; assembler;[public,alias:'FPC__DOSYS'];

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
  add    $0x18,%rsp
end;


//                    edi   esi   edx 	 ecx    r8      r9   stack  stack
function fp_sysCall(sysnr,param1,param2,param3,param4,param5,param6,param7 : TSysParam):TSysResult; assembler;[public,alias:'FPC__DOSYS7'];

asm
  sub    $0x10,%rsp
  movq   sysnr,%rax       { Syscall number -> rax.  }
  movq   param1,%rdi      { shift arg1 - arg6. }
  movq   param2,%rsi
  movq   param3,%rdx
  movq   param4,%r10
  movq   param5,%r8
  movq   param6,%r9
  movq   param7,%r11
  movq   %r11,(%rsp)
  syscall                 { Do the system call. }
  jnb .LSyscOK            { branch to exit if ok, errorhandler otherwise}
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
  add    $0x10,%rsp
end;

