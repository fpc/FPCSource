#
#    $Id$
#    This file is part of the Free Pascal run time library.
#    Copyright (c) 1999-2000 by the Free Pascal development team.
#
#    Go32V1 Startup code
#
#    See the file COPYING.FPC, included in this distribution,
#    for details about the copyright.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
# **********************************************************************
#
#  Called as start(argc, argv, envp)
#
#  gs:edx points to prog_info structure.  All other registers are OBSOLETE
#  but included for backwards compatibility
#
.text
        .globl  _start
_start:
        .globl  start
start:
# the first instruction must be movl %eax,
# because that is the way GO32V2 makes the difference between V1 and V2 coff format
        movl    %eax,__hard_master
        movl    %esi,___pid
        movl    %edi,___transfer_buffer
        movl    %ebx,_ScreenPrimary
        movl    %ebp,_ScreenSecondary
        cmpl    $0, %edx
        je      Lcopy_none
        movw    %gs,%cx
        movw    %ds,%ax
        cmpw    %cx,%ax
        je      Lcopy_none
# set the right size
        movl  $40,U_SYSTEM_GO32_INFO_BLOCK

        movl    %gs:(%edx), %ecx
        cmpl    U_SYSTEM_GO32_INFO_BLOCK, %ecx
        jbe     Lcopy_less
        movl    U_SYSTEM_GO32_INFO_BLOCK, %ecx
Lcopy_less:
        movl    $U_SYSTEM_GO32_INFO_BLOCK, %edi
        addl    $3, %ecx
        andl    $0xfffffffc, %ecx
        movl    %ecx, (%edi)
        addl    $4, %edi
        addl    $4, %edx
        subl    $4, %ecx
Lcopy_more:
        movl    %gs:(%edx), %eax
        movl    %eax, (%edi)
        addl    $4, %edx
        addl    $4, %edi
        subl    $4, %ecx
        jnz     Lcopy_more

        movl    U_SYSTEM_GO32_INFO_BLOCK+4, %eax
        movl    %eax, _ScreenPrimary
        movl    U_SYSTEM_GO32_INFO_BLOCK+8, %eax
        movl    %eax, _ScreenSecondary
        movl    U_SYSTEM_GO32_INFO_BLOCK+12, %eax
        movl    %eax, ___transfer_buffer
        movl    U_SYSTEM_GO32_INFO_BLOCK+20, %eax
        movl    %eax, ___pid
        movl    U_SYSTEM_GO32_INFO_BLOCK+24, %eax
        movl    %eax, __hard_master

        jmp     Lcopy_done

Lcopy_none:
        movl    %ebx,U_SYSTEM_GO32_INFO_BLOCK+4
        movl    %ebp,U_SYSTEM_GO32_INFO_BLOCK+8
        movl    %edi,U_SYSTEM_GO32_INFO_BLOCK+12
        movl    $4096,U_SYSTEM_GO32_INFO_BLOCK+16
        movl    %esi,U_SYSTEM_GO32_INFO_BLOCK+20
        movl    %eax,U_SYSTEM_GO32_INFO_BLOCK+24
        movl    $28, U_SYSTEM_GO32_INFO_BLOCK
Lcopy_done:

        movw    U_SYSTEM_GO32_INFO_BLOCK+36,%ax
        movw    %ax,_run_mode
# I need a value for the stack bottom,
# According to Pierre, from the source code of go32v1
# the stack is 256Kb in length
        movl    %esp,%eax
        subl    $0x40000,%eax
        movl    %eax,__stkbottom

        movw    U_SYSTEM_GO32_INFO_BLOCK+26,%ax
        movw    %ax,_core_selector
        movl    U_SYSTEM_GO32_INFO_BLOCK+28,%eax
        movl    %eax,U_SYSTEM_STUB_INFO
        xorl    %esi,%esi
        xorl    %edi,%edi
        xorl    %ebp,%ebp
        xorl    %ebx,%ebx

        movl    %esp,%ebx
        movl    $0x0,%ebp
        movl    %esp,%ebx
        movl    8(%ebx),%eax
        movl    %eax,_environ
        movl    %eax,U_SYSTEM_ENVP
        movl    4(%ebx),%eax
        movl    %eax,_args
        movl    %eax,U_SYSTEM_ARGV
        movl    (%ebx),%eax
        movl    %eax,_argc
        movl    %eax,U_SYSTEM_ARGC

        call    PASCALMAIN

exit_again:
        movl    $0x4c00,%eax
        int     $0x21
        jmp     exit_again

        ret

.data
        .globl _argc
_argc:
        .long   0

        .globl  _args
_args:
        .long   0

        .globl  _environ
_environ:
        .long   0

        .globl  __stkbottom
__stkbottom:
        .long   0

        .globl  _run_mode
_run_mode:
        .word   0

        .globl  _core_selector
_core_selector:
        .word   0

        .globl  ___pid
___pid:
        .long   42

        .globl  ___transfer_buffer
___transfer_buffer:
        .long   0

        .globl  _ScreenPrimary
_ScreenPrimary:
        .long   0

        .globl  _ScreenSecondary
_ScreenSecondary:
        .long   0

        .globl  __hard_master
__hard_master:
        .byte   0

        .globl  __hard_slave
__hard_slave:
        .byte   0

        .globl  __core_select
__core_select:
        .short  0
#
# $Log$
# Revision 1.3  2000-01-07 16:41:30  daniel
#   * copyright 2000
#
# Revision 1.2  2000/01/07 16:32:23  daniel
#   * copyright 2000 added
#
# Revision 1.1  1998/12/21 13:07:02  peter
#   * use -FE
#
# Revision 1.4  1998/08/04 13:35:34  carl
#   * stack size default is 256Kb! not 16K! as information stated by Pierre
#
# Revision 1.3  1998/05/22 00:39:32  peter
#   * go32v1, go32v2 recompiles with the new objects
#   * remake3 works again with go32v2
#   - removed some "optimizes" from daniel which were wrong
#
#
