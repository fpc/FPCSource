/*
  $Id$
*/
/*
   This file is part of the Free Pascal run time library.
   Copyright (c) 2004 by Karoly Balogh for Genesi Sarl

   Thanks for Martin 'MarK' Kuchinka <kuchinka@volny.cz>
   for his help.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*/
        .section ".text"
        .globl _start
        .align 4
_start:
        stwu 1,-16(1)
        mflr 0
        stw 0,20(1)

        /* Get ExecBase */
        lwz 3,4(0)
        lis 4,_ExecBase@ha
        stw 3,_ExecBase@l(4)

        /* Allocating new stack */
        lis 4,__stklen@ha
        lwz 3,__stklen@l(4)
        stw 3,0(2)
        li 3,1
        stw 3,4(2)
        lwz 3,4(0)
        stw 3,56(2)
        lwz 3,100(2)
        mtlr 3
        li 3,-684              /* AllocVec */
        blrl
        /* FIXME: check for successful allocation! */

        lis 4,stackArea@ha
        stw 3,stackArea@l(4)

        /* Setting up stackSwap struct */
        lis 4,stackSwap@ha
        addi 4,4,stackSwap@l
        stw 3,0(4)
        lis 5,__stklen@ha
        lwz 6,__stklen@l(5)
        add 3,3,6
        stw 3,4(4)
        stw 3,8(4)

        /* Calling main function with the new stack */
        stw 4,32(2)
        lis 4,_initproc@ha
        addi 4,4,_initproc@l
        stw 4,36(2)
        li 3,0
        stw 3,40(2)
        lwz 4,100(2)
        mtlr 4
        li 3,-804              /* NewPPCStackSwap */
        blrl

        /* Freeing up stack area */
        lis 4,stackArea@ha
        lwz 3,stackArea@l(4)
        stw 3,36(2)
        lwz 3,4(0)
        stw 3,56(2)
        lwz 3,100(2)
        mtlr 3
        li 3,-690              /* FreeVec */
        blrl

        /* Setting return value */
        lis 4,returnValue@ha
        lwz 3,returnValue@l(4)

        lwz 4,0(1)
        lwz 0,4(4)
        mtlr 0
        mr 1,4
        blr

_initproc:
        stwu 1,-128(1)
        mflr 0
        stw 13,52(1)
        stw 14,56(1)
        stw 15,60(1)
        stw 16,64(1)
        stw 17,68(1)
        stw 18,72(1)
        stw 19,76(1)
        stw 20,80(1)
        stw 21,84(1)
        stw 22,88(1)
        stw 23,92(1)
        stw 24,96(1)
        stw 25,100(1)
        stw 26,104(1)
        stw 27,108(1)
        stw 28,112(1)
        stw 29,116(1)
        stw 30,120(1)
        stw 31,124(1)
        stw 0,132(1)

        /* Save Stackpointer */
        lis 4,OriginalStkPtr@ha
        stw 1,OriginalStkPtr@l(4)

        bl PASCALMAIN

        .globl  _haltproc
_haltproc:
        /* Restore Stackpointer */
        lis 4,OriginalStkPtr@ha
        lwz 1,OriginalStkPtr@l(4)

        /* Store return value */
        lis 4,returnValue@ha
        stw 3,returnValue@l(4)

        lwz 4,0(1)
        lwz 0,4(4)
        mtlr 0
        lwz 13,-76(4)
        lwz 14,-72(4)
        lwz 15,-68(4)
        lwz 16,-64(4)
        lwz 17,-60(4)
        lwz 18,-56(4)
        lwz 19,-52(4)
        lwz 20,-48(4)
        lwz 21,-44(4)
        lwz 22,-40(4)
        lwz 23,-36(4)
        lwz 24,-32(4)
        lwz 25,-28(4)
        lwz 26,-24(4)
        lwz 27,-20(4)
        lwz 28,-16(4)
        lwz 29,-12(4)
        lwz 30,-8(4)
        lwz 31,-4(4)
        mr 1,4
        blr

        .globl _ExecBase
        .globl SysBase
        .align 4
SysBase:
_ExecBase:
        .long 0

        .globl OriginalStkPtr
        .align 4
OriginalStkPtr:
        .long 0

        .globl OriginalLinkRegister
        .align 4
OriginalLinkRegister:
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

        /* This is needed to be a proper MOS ABox executable */
        /* This symbol _MUST NOT_ be stripped out from the executable */
        /* or else... */
        .globl __abox__
        .type __abox__,@object
        .size __abox__,4
__abox__:
        .long 1

/*
  $Log$
  Revision 1.10  2004-06-06 12:51:06  karoly
    * changelog fixed

  Revision 1.9  2004/06/06 12:47:57  karoly
    * some cleanup, comments added

  Revision 1.8  2004/06/05 19:25:12  karoly
    + reworked to support resizing of stack

  Revision 1.7  2004/05/13 01:15:42  karoly
    - removed comment about argc/argv, made it work another way

  Revision 1.6  2004/05/01 15:08:57  karoly
    + haltproc added, saving/restoring stackpointer added

  Revision 1.5  2004/04/21 03:24:55  karoly
   * rewritten to be similar to GCC startup code

  Revision 1.4  2004/04/09 04:02:43  karoly
   * abox id symbol fixed

  Revision 1.3  2004/04/09 02:58:15  karoly
   * typo fixed.

  Revision 1.2  2004/04/09 02:54:25  karoly
   * execbase loading oops fixed.

  Revision 1.1  2004/03/16 10:29:22  karoly
   * first implementation of some startup code for MOS

*/
