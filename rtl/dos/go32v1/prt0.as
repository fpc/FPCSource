#
#    $Id$
#    This file is part of the Free Pascal run time library.
#    Copyright (c) 1993,97 by the Free Pascal development team.
#
#    See the file COPYING.FPC, included in this distribution,
#    for details about the copyright.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
# **********************************************************************
#///*
#//**	Called as start(argc, argv, envp)
#//*/
#///*	gs:edx points to prog_info structure.  All other registers are OBSOLETE
#//**	but included for backwards compatibility
#//*/

	.text
	.globl	_start
_start:
	.globl	start
start:
# the first instruction must be movl %eax,
# because that is the way GO32V2 makes the difference between V1 and V2 coff format
	movl	%eax,__hard_master
	movl	%esi,___pid
	movl	%edi,___transfer_buffer
	movl	%ebx,_ScreenPrimary
	movl	%ebp,_ScreenSecondary
	cmpl	$0, %edx
	je	Lcopy_none
	movw	%gs,%cx
	movw	%ds,%ax
	cmpw	%cx,%ax
	je	Lcopy_none
#   /* set the right size */
	movl  $40,U_SYSTEM_GO32_INFO_BLOCK

	movl	%gs:(%edx), %ecx
	cmpl	U_SYSTEM_GO32_INFO_BLOCK, %ecx
	jbe	Lcopy_less
	movl	U_SYSTEM_GO32_INFO_BLOCK, %ecx
Lcopy_less:
	movl	$U_SYSTEM_GO32_INFO_BLOCK, %edi
	addl	$3, %ecx
	andl	$0xfffffffc, %ecx
	movl	%ecx, (%edi)
	addl	$4, %edi
	addl	$4, %edx
	subl	$4, %ecx
Lcopy_more:
	movl	%gs:(%edx), %eax
	movl	%eax, (%edi)
	addl	$4, %edx
	addl	$4, %edi
	subl	$4, %ecx
	jnz	Lcopy_more

	movl	U_SYSTEM_GO32_INFO_BLOCK+4, %eax
	movl	%eax, _ScreenPrimary
	movl	U_SYSTEM_GO32_INFO_BLOCK+8, %eax
	movl	%eax, _ScreenSecondary
        movl	U_SYSTEM_GO32_INFO_BLOCK+12, %eax
	movl	%eax, ___transfer_buffer
	movl	U_SYSTEM_GO32_INFO_BLOCK+20, %eax
	movl	%eax, ___pid
	movl	U_SYSTEM_GO32_INFO_BLOCK+24, %eax
	movl	%eax, __hard_master

	jmp	Lcopy_done

Lcopy_none:
	movl	%ebx,U_SYSTEM_GO32_INFO_BLOCK+4
	movl	%ebp,U_SYSTEM_GO32_INFO_BLOCK+8
	movl	%edi,U_SYSTEM_GO32_INFO_BLOCK+12
	movl	$4096,U_SYSTEM_GO32_INFO_BLOCK+16
	movl	%esi,U_SYSTEM_GO32_INFO_BLOCK+20
	movl	%eax,U_SYSTEM_GO32_INFO_BLOCK+24
	movl	$28, U_SYSTEM_GO32_INFO_BLOCK
Lcopy_done:

        movw    U_SYSTEM_GO32_INFO_BLOCK+36,%ax
        movw    %ax,_run_mode
#/* I need a value for the stack bottom,            */
#/* but I don't know how to get it from go32        */
#/* I suppose the stack is 4Ko long, is this true ? */
        movl    %esp,%eax
        subl    $0x4000,%eax
        movl    %eax,__stkbottom

        movw    U_SYSTEM_GO32_INFO_BLOCK+26,%ax
        movw    %ax,_core_selector
   	movl    U_SYSTEM_GO32_INFO_BLOCK+28,%eax
   	movl  %eax,U_SYSTEM_STUB_INFO
	xorl	%esi,%esi
	xorl	%edi,%edi
	xorl	%ebp,%ebp
	xorl	%ebx,%ebx

	movl	%esp,%ebx
        movl    $0x0,%ebp
	movl	%esp,%ebx
	movl	8(%ebx),%eax
	movl	%eax,_environ
	movl	4(%ebx),%eax
	movl	%eax,_args
	movl	(%ebx),%eax
	movl	%eax,_argc

	call	PASCALMAIN


exit_again:
	movl	$0x4c00,%eax
	int	$0x21
	jmp	exit_again

	ret

	.data
        .globl _argc
_argc:
	.long   0
	.globl  _args
_args:
	.long	0
	.globl	_run_mode
_run_mode:
	.word	0
	.globl	_core_selector
_core_selector:
	.word	0
	.globl	_environ
_environ:
	.long	0

	.globl	___pid
___pid:
	.long	42

	.globl	___transfer_buffer
___transfer_buffer:
	.long	0

	.globl	_ScreenPrimary
_ScreenPrimary:
	.long	0

	.globl	_ScreenSecondary
_ScreenSecondary:
	.long	0

	.globl	__hard_master
	.globl	__hard_slave
	.globl	__core_select
__hard_master:
	.byte	0
__hard_slave:
	.byte	0
__core_select:
	.short	0
        .globl  __stkbottom
__stkbottom:
        .long   0
#  .globl U_SYSTEM_GO32_INFO_BLOCK
# U_SYSTEM_GO32_INFO_BLOCK:
#  .long  __go32_end - U_SYSTEM_GO32_INFO_BLOCK #//* size */
#  .long  0 #//* offs 4 linear_address_of_primary_screen; */
#  .long  0 #//* offs 8 linear_address_of_secondary_screen; */
#  .long  0 #//* offs 12 linear_address_of_transfer_buffer; */
#  .long  0 #//* offs 16 size_of_transfer_buffer;  >= 4k */
#  .long  0 #//* offs 20 pid; */
#  .byte  0 #//* offs 24 u_char master_interrupt_controller_base; */
#  .byte  0 #//* offs 25 u_char slave_interrupt_controller_base; */
#  .word  0 #//* offs 26 u_short selector_for_linear_memory; */
#  .long  0 #//* offs 28 u_long linear_address_of_stub_info_structure; */
#  .long  0 #//* offs 32 u_long linear_address_of_original_psp; */
#  .word  0 #//* offs 36 u_short run_mode; */
#  .word  0 #//* offs 38 u_short run_mode_info; */
#__go32_end:
