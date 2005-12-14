|
|  This file is part of the Free Pascal run time library.
|  Copyright (c) 2005 by Karoly Balogh
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
   
   | Allocating new stack
   move.l   __stklen,d0
   moveq.l  #0,d1            | MEMF_ANY
   jsr      -684(a6)         | AllocVec()
   tst.l    d0
   beq      __exit  
   move.l   d0,stackArea

   | Setting up StackSwap structure, and do the StackSwap
   lea.l    stackSwap,a0
   move.l   d0,(a0)          | Bottom of the stack
   add.l    __stklen,d0
   move.l   d0,4(a0)         | Top of the stack
   move.l   d0,8(a0)         | Initial stackpointer
   jsr      -732(a6)         | StackSwap()

   jsr PASCALMAIN

   .globl _haltproc 
_haltproc:
   move.l   d0,returnValue

   | Swapping the stack back
   move.l   _ExecBase,a6
   lea.l    stackSwap,a0
   jsr      -732(a6)         | StackSwap()

   | Freeing up stack area
   move.l   stackArea,a1
   jsr      -690(a6)         | FreeVec()

__exit:
   movem.l  (sp)+,d0-d7/a0-a6
   move.l   returnValue,d0
   rts

   .data

   .globl _ExecBase
   .globl SysBase
   .align 4
SysBase:
_ExecBase:
   .long 0

   .globl returnValue
   .align 4
returnValue:
   .long 0

   .globl stackArea
   .align 4
stackArea:
   .long 0

   .globl stackSwap
   .align 4
stackSwap:
   .long 0
   .long 0
   .long 0
