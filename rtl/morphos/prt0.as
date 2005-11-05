#
#  This file is part of the Free Pascal run time library.
#  Copyright (c) 2004 by Karoly Balogh for Genesi Sarl
#
#  Thanks for Martin 'MarK' Kuchinka <kuchinka@volny.cz>
#  for his help.
#
#  See the file COPYING.FPC, included in this distribution,
#  for details about the copyright.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY;without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
        .section ".text"
        .globl _start
        .align 4
_start:
        mflr 0
        stw  0,4(1)
        stwu 1,-16(1)

        # Get ExecBase
        lwz 3,4(0)
        lis 4,_ExecBase@ha
        stw 3,_ExecBase@l(4)

        # Allocating new stack
        lis 4,__stklen@ha
        lwz 3,__stklen@l(4)
        stw 3,0(2)
        lwz 3,4(0)
        stw 3,56(2)
        lwz 3,100(2)
        mtlr 3
        li 3,-858              # AllocTaskPooled
        blrl

        cmplwi cr0,3,0
        beq cr0,_exit

        lis 4,stackArea@ha
        stw 3,stackArea@l(4)

        # Setting up stackSwap struct
        lis 4,stackSwap@ha
        addi 4,4,stackSwap@l
        stw 3,0(4)
        lis 5,__stklen@ha
        lwz 6,__stklen@l(5)
        add 3,3,6
        stw 3,4(4)
        stw 3,8(4)

        # Calling main function with the new stack
        stw 4,32(2)
        lis 4,_initproc@ha
        addi 4,4,_initproc@l
        stw 4,36(2)
        li 3,0
        stw 3,40(2)
        lwz 4,100(2)
        mtlr 4
        li 3,-804              # NewPPCStackSwap
        blrl

        # Setting return value
        lis 4,returnValue@ha
        lwz 3,returnValue@l(4)

_exit:
        addi 1,1,16
        lwz  0,4(1)
        mtlr 0
        blr

_initproc:
        mflr 0
        stw  0,4(1)     
        stwu 1,-128(1)
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

        # Save Stackpointer
        lis 4,OriginalStkPtr@ha
        stw 1,OriginalStkPtr@l(4)

        bl PASCALMAIN

        .globl  _haltproc
_haltproc:
        # Restore Stackpointer
        lis 4,OriginalStkPtr@ha
        lwz 1,OriginalStkPtr@l(4)

        # Store return value
        lis 4,returnValue@ha
        stw 3,returnValue@l(4)

        lwz 13,52(1)
        lwz 14,56(1)
        lwz 15,60(1)
        lwz 16,64(1)
        lwz 17,68(1)
        lwz 18,72(1)
        lwz 19,76(1)
        lwz 20,80(1)
        lwz 21,84(1)
        lwz 22,88(1)
        lwz 23,92(1)
        lwz 24,96(1)
        lwz 25,100(1)
        lwz 26,104(1)
        lwz 27,108(1)
        lwz 28,112(1)
        lwz 29,116(1)
        lwz 30,120(1)
        lwz 31,124(1)
        addi 1,1,128
        lwz 0,4(1)
        mtlr 0
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

        # This is needed to be a proper MOS ABox executable
        # This symbol _MUST NOT_ be stripped out from the executable
        # or else...
        .globl __abox__
        .type __abox__,@object
        .size __abox__,4
__abox__:
        .long 1

