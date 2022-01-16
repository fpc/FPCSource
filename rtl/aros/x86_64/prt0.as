#
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 2015-2016 by Marcus Sackrow
#
#   AROS x86_64 startup code
#
#   See the file COPYING.FPC, included in this distribution,
#   for details about the copyright.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY;without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
#**********************************************************************}

# AROS startup code
  .text
  .align 8
  .section .aros.startup, "ax"
  .globl start
  .globl _start
  .type _start,@function
_start:
start:
# save registers, suggested by
# System V Application Binary Interface
# AMD64 Architecture Processor Support
# 0.99.6 page 21 Fig. 3.4
  push %rbp
  push %rbx
  push %r12
  push %r13
  push %r14
  push %r15
  movq %rdx, _ExecBase
  movq %rsp, STKPTR
# todo: stack change (see i386)

# FindTask(nil)
  movq _ExecBase, %rsi  /* Execbase to rsi */
  movq $0, %rdi         /* 1. Argument nil */
  movq -392(%rsi), %rax /* calculate jump address */
  call *%rax            /* do the call */
# PTask in %rax -> get data from PTask splower rbx, spupper rcx
  movq 112(%rax), %rcx
  movq 104(%rax), %rbx
  subq %rbx, %rcx
# check if Stack is big enough
  cmpl  __stklen, %ecx
  jl _AllocStack             /* we need a stack */
  # no stack needed, clear StackAreaPtr jump to Pascal routine
  xorq  %rax, %rax
  movq %rax, StackAreaPtr
  jmp _NoAllocStack

# Alloc a new Stack and swap to it
_AllocStack:
  # AllocVec(__stklen, MEMF_ANY)
  movq _ExecBase, %rdx      /* execbase Execbase is already in %rdx */
  movq $0, %rsi             /* 2. Argument MEMF_ANY to %rsi */
  movl __stklen, %edi       /* 1. Argument stklen to %edi */
  call *-912(%rdx)          /* Do the call */
  # save the Stack Area
  movq %rax, StackAreaPtr
  # Check if we got some memory
  cmpq $0, %rax
  je  _exit

  # Setup StackSwapStruct
  lea StackSwapStruct, %rcx   /* StackSwapStruct in RCX */
  movq StackAreaPtr, %rax
  movq %rax,(%rcx)            /* stk_Lower Bottom of the stack */
  xorq %rbx, %rbx
  movl __stklen, %ebx         /* get Stacklen */
  addq %rbx, %rax             /* add stacklen to lower stack pointer */
  movq %rax, 8(%rcx)          /* Top of the stack */
  movq %rax, 16(%rcx)         /* Initial stackpointer */

  # NewStackSwap(StackSwapStruct, _initProc, StackSwapArgs)
  movq _ExecBase, %rcx      /* Execbase to rcx */
  lea StackSwapArgs, %rdx   /* 3. Argument StackSwapArgument -> rdx */
  lea _initProc, %rsi       /* 2. Argument FunktionPtr -> rsi */
  lea StackSwapStruct, %rdi /* 1. Argument StackSwapStruct -> rdi */
  call *-1072(%rcx)         /* Do the call */
  jmp _afterMain

_NoAllocStack:
  call _initProc

_afterMain:
  # test it StackArea Ptr assigned
  movq StackAreaPtr,%rax
  cmpq $0, StackAreaPtr
  je _exit                 /* its nil -> direkt exit */

  # FreeVec(StackAreaPtr)
  movq _ExecBase, %rsi     /* ExecBase as last argument %rsi */
  movq StackAreaPtr, %rdi  /* StackAreaPtr 1 Argument %rdi */
  call *-920(%rsi)         /* Do the call */

_exit:
  # set returncode
  movslq  operatingsystem_result,%rax
  # get back all registers
  mov STKPTR, %rsp
  pop %r15
  pop %r14
  pop %r13
  pop %r12
  pop %rbx
  pop %rbp
  # bye bye
  ret

_initProc:
  push %rbp
  push %rbx
  push %r12
  push %r13
  push %r14
  push %r15
  # Save stack pointer
  movq %rsp,SSTKPTR
  # call the pascal main function
  callq PASCALMAIN

  # entry to stop the program
  .globl  _haltproc
  .type   _haltproc,@function
_haltproc:
  # restore the old stackPtr and return
  movq SSTKPTR,%rsp
  pop %r15
  pop %r14
  pop %r13
  pop %r12
  pop %rbx
  pop %rbp
  ret

/*----------------------------------------------------*/
  .data
  .globl __data_start
__data_start:
  .long 0
  .weak data_start
        data_start = __data_start

.bss
.type STKPTR,@object
.size STKPTR,8
STKPTR: .skip 8

.type SSTKPTR,@object
.size SSTKPTR,8
SSTKPTR: .skip 8

.type _ExecBase,@object
.size _ExecBase,8
.global _ExecBase
_ExecBase:  .skip 8

.type StackAreaPtr,@object
.size StackAreaPtr,8
StackAreaPtr:  .skip 8

.type StackSwapStruct,@object
.size StackSwapStruct,24
StackSwapStruct: .skip 24

.type StackSwapStruct,@object
.size StackSwapStruct,64
StackSwapArgs: .skip 64

