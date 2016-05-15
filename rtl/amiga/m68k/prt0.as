|
|  This file is part of the Free Pascal run time library.
|  Copyright (c) 2005-2014 by Karoly Balogh
|
|  Startup code for Amiga/m68k RTL
|
|  See the file COPYING.FPC, included in this distribution,
|  for details about the copyright.
|
|  This program is distributed in the hope that it will be useful,
|  but WITHOUT ANY WARRANTY; without even the implied warranty of
|  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
|

   .text
   .align 4

| This symbol is required for lineinfo support!
.globl __EXESTART
__EXESTART:

   .globl _start
   .globl start
_start:
start:
   movem.l  d0-d7/a0-a6,-(sp)

   | Get ExecBase
   move.l   0x4,a6
   move.l   a6,_ExecBase

   | Get current stacksize
   move     #0,a1            | nil
   jsr      -294(a6)         | FindTask()
   move.l   d0,a0
   move.l   62(a0),d1        | SPUpper
   sub.l    58(a0),d1        | SPLower

   | Check if we need a new stack
   | Only allocate a new stack if the system-provided
   | stack is smaller than the one set compile time
   move.l   __stklen,d0      | Also an argument for AllocVec() below
   cmp.l    d0,d1
   blt      _allocStack

   move.l   d1,__stklen      | Store the new stack size
   moveq.l  #0,d0
   move.l   d0,stackArea     | Clear the stackArea pointer for exit test
   move.l   sp,stackPtr      | Store the stackPointer for restoration
   bra      _noAllocStack

_allocStack:
   | Allocating new stack
   moveq.l  #0,d1            | MEMF_ANY
   jsr      -684(a6)         | AllocVec()
   tst.l    d0
   beq      __exit
   move.l   d0,stackArea

   | Setting up StackSwap structure, and do the StackSwap
   lea      stackSwap,a0
   move.l   d0,(a0)          | Bottom of the stack
   add.l    __stklen,d0
   move.l   d0,4(a0)         | Top of the stack
   move.l   d0,8(a0)         | Initial stackpointer
   jsr      -732(a6)         | StackSwap()

_noAllocStack:
   jsr PASCALMAIN

   .globl _haltproc 
_haltproc:
   | Check if we need to release a stack
   tst.l    stackArea
   bne      _freeStack

   move.l   stackPtr,sp
   bra      __exit

_freeStack:
   | Swapping the stack back
   move.l   _ExecBase,a6
   lea      stackSwap,a0
   jsr      -732(a6)         | StackSwap()

   | Freeing up stack area
   move.l   stackArea,a1
   jsr      -690(a6)         | FreeVec()

__exit:
   movem.l  (sp)+,d0-d7/a0-a6
   move.l   operatingsystem_result,d0
   rts

   .bss

   .globl _ExecBase
   .globl SysBase
   .balign 4
SysBase:
_ExecBase:
   .skip 4

   .globl stackArea
   .balign 4
stackArea:
   .skip 4

   .globl stackPtr
   .balign 4
stackPtr:
   .skip 4

   .globl stackSwap
   .balign 4
stackSwap:
   .skip 12
