{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by Florian Klaempfl and other members of the
    Free Pascal development team

    SetJmp and LongJmp implementation for exception handling

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

function setjmp(var S : jmp_buf) : longint;assembler;[Public, alias : 'FPC_SETJMP'];nostackframe;
  asm
{$ifdef win64}
    // Save registers.
    movq %rbx,(%rcx)
    movq %rbp,8(%rcx)
    movq %r12,16(%rcx)
    movq %r13,24(%rcx)
    movq %r14,32(%rcx)
    movq %r15,40(%rcx)
    movq %rsi,64(%rcx)
    movq %rdi,72(%rcx)
    leaq 8(%rsp),%rdx       // Save SP as it will be after we return.
    movq %rdx,48(%rcx)
    movq 0(%rsp),%r8        // Save PC we are returning to now.
    movq %r8,56(%rcx)
    xorq %rax,%rax
{$else win64}
    // Save registers.
    movq %rbx,(%rdi)
    movq %rbp,8(%rdi)
    movq %r12,16(%rdi)
    movq %r13,24(%rdi)
    movq %r14,32(%rdi)
    movq %r15,40(%rdi)
    leaq 8(%rsp),%rdx       // Save SP as it will be after we return.
    movq %rdx,48(%rdi)
    movq 0(%rsp),%rsi       // Save PC we are returning to now.
    movq %rsi,56(%rdi)
    xorq %rax,%rax
{$endif win64}
  end;


procedure longjmp(var S : jmp_buf;value : longint);assembler;[Public, alias : 'FPC_LONGJMP'];
  asm
{$ifdef win64}
    // Restore registers.
    movq (%rcx),%rbx
    movq 8(%rcx),%rbp
    movq 16(%rcx),%r12
    movq 24(%rcx),%r13
    movq 32(%rcx),%r14
    movq 40(%rcx),%r15
    // Set return value for setjmp.
    test %edx,%edx
    mov $01,%eax
    cmove %eax,%edx
    mov %edx,%eax
    movq 48(%rcx),%rsp
    movq 56(%rcx),%rdx
    movq 64(%rcx),%rsi
    movq 72(%rcx),%rdi
    jmpq *%rdx
{$else win64}
    // Restore registers.
    movq (%rdi),%rbx
    movq 8(%rdi),%rbp
    movq 16(%rdi),%r12
    movq 24(%rdi),%r13
    movq 32(%rdi),%r14
    movq 40(%rdi),%r15
    // Set return value for setjmp.
    test %esi,%esi
    mov $01,%eax
    cmove %eax,%esi
    mov %esi,%eax
    movq 56(%rdi),%rdx
    movq 48(%rdi),%rsp
    jmpq *%rdx
{$endif win64}
  end;

