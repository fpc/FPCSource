#
#  This file is part of the Free Pascal run time library.
#  Copyright (c) 2005 by Karoly Balogh
#
#  Startup code for AmigaOS/PowerPC RTL
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
        lwz  3,4(0)
        lis  4,_ExecBase@ha
        stw  3,_ExecBase@l(4)

        # Get Exec Main Interface
        lwz  5,632(3)               # MainInterface
        lis  6,_IExec@ha
        stw  5,_IExec@l(6)

        # Make use of stack cookie, to avoid 
        # stripping the symbol in all cases
        lis  6,__stack_cookie@ha

        # Store start of stack area
        mr   6,1
        addi 6,6,16
        lis  4,stackArea@ha
        stw  6,stackArea@l(4)

        # Store regs
        stwu 1,-128(1)
        stw  13,52(1)
        stw  14,56(1)
        stw  15,60(1)
        stw  16,64(1)
        stw  17,68(1)
        stw  18,72(1)
        stw  19,76(1)
        stw  20,80(1)
        stw  21,84(1)
        stw  22,88(1)
        stw  23,92(1)
        stw  24,96(1)
        stw  25,100(1)
        stw  26,104(1)
        stw  27,108(1)
        stw  28,112(1)
        stw  29,116(1)
        stw  30,120(1)
        stw  31,124(1)

        # Save Stackpointer
        lis  4,OriginalStkPtr@ha
        stw  1,OriginalStkPtr@l(4)

        bl PASCALMAIN

        .globl  _haltproc
_haltproc:
        # Restore Stackpointer
        lis 4,OriginalStkPtr@ha
        lwz 1,OriginalStkPtr@l(4)

        lwz  13,52(1)
        lwz  14,56(1)
        lwz  15,60(1)
        lwz  16,64(1)
        lwz  17,68(1)
        lwz  18,72(1)
        lwz  19,76(1)
        lwz  20,80(1)
        lwz  21,84(1)
        lwz  22,88(1)
        lwz  23,92(1)
        lwz  24,96(1)
        lwz  25,100(1)
        lwz  26,104(1)
        lwz  27,108(1)
        lwz  28,112(1)
        lwz  29,116(1)
        lwz  30,120(1)
        lwz  31,124(1)
        addi 1,1,128
_exit:
        addi 1,1,16
        lwz  0,4(1)
        mtlr 0
        blr


        .section .data
        .globl _data_start
_data_start:

        .globl _ExecBase
        .globl SysBase
        .align 4
SysBase:
_ExecBase:
        .long 0

        .globl _IExec
        .align 4
_IExec:
        .long 0

        .globl OriginalStkPtr
        .align 4
OriginalStkPtr:
        .long 0

        .globl stackArea
        .align 4
stackArea:
        .long 0

        # This is needed to be a proper Amiga OS4 executable
        # This symbol _MUST NOT_ stripped out from the executable
        # or else...
        .globl __amigaos4__
        .type __amigaos4__,@object
        .size __amigaos4__,4
__amigaos4__:
        .long 1
