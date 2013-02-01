/*
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2009 by Michael Van Canneyt and David Zhang

    Startup code for elf32-mipsel/elf32-mips

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*/
        .set noat

	.section ".text"

	.align 4
	.global _dynamic_start
    .global FPC_SHARED_LIB_START
	.type _dynamic_start,@function
    .type  FPC_SHARED_LIB_START,@function
FPC_SHARED_LIB_START:
_dynamic_start:
	.ent  _dynamic_start 
    .set noreorder
	.cpload $25
    /* TODO: check whether this code is correct */
    la      $v0,__dl_fini
    lw      $v0,($v0)
	la      $t9,_start
    jr	    $t9
	nop

	.end	_dynamic_start
	.size 	_dynamic_start, .-_dynamic_start

	.align 4
	.global _start
    .set    nomips16
 	.type	_start,@function
/*  This is the canonical entry point, usually the first thing in the text
    segment.  The SVR4/Mips ABI (pages 3-31, 3-32) says that when the entry
    point runs, most registers' values are unspecified, except for:

    v0 ($2)	Function pointer of a function to be executed at exit

    sp ($29)	The stack contains the arguments and environment:
 		0(%sp)			argc
 		4(%sp)			argv[0]

 		...
 		(4*argc)(%sp)		NULL
 		(4*(argc+1))(%sp)	envp[0]
 		...
 					NULL
    ra ($31)	Return address set to zero.
*/
_start:
    .ent _start
    /* load fp */
    .set noreorder
	.cpload $25

    /* Record $sp into $s8 */
	move    $s8,$sp

    /* align stack */
    li      $at,-8
    and     $sp,$sp,$at
    addiu   $sp,$sp,-32

    /* Compute and save sp offset */
    subu    $t1,$s8,$sp
    sw      $t1,24($sp)

    /* Save $ra register */
    sw      $ra,28($sp)
    /* Save $gp register */
	.cprestore 20 

    /* Set __stkptr variable */
    move    $s8,$sp
    la      $t1,__stkptr
    sw      $s8,($t1)

    /* store argc, which is in $a0 */
    lw      $a0,0($s8)
    la      $t1,operatingsystem_parameter_argc
    sw      $a0,($t1)

    /* store argv which is in $a1 */
    addiu   $a1,$s8,4
    la      $t1,operatingsystem_parameter_argv
    sw      $a1,($t1)

    /* store envp which is in $a2 */
    la      $t1,operatingsystem_parameter_envp
    sw      $a2,($t1)

    /* Set IsLibrary to one */
    la      $t1,operatingsystem_islibrary
    li      $t2,1
    sb      $t2,($t1)
    /* Jump to PASCALMAIN */
    la      $t9,PASCALMAIN
    jalr    $t9
    nop

    /* Restore $ra */
    lw      $ra,28($sp)

    /* Restore old $sp */
    lw      $t1,24($sp)
    addu    $sp,$sp,$t1
    /* Return to caller */
    jr      $ra
    nop

	.end	_start
	.size 	_start, .-_start

	.globl  _haltproc
    .globl  FPC_SHARED_LIB_EXIT
	.ent	_haltproc
	.type   _haltproc,@function
    .type  FPC_SHARED_LIB_EXIT,@function
FPC_SHARED_LIB_EXIT:
_haltproc:
        /* TODO: need to check whether __dl_fini is non-zero and call the function pointer in case */

        li      $v0,4001
        syscall
        b       _haltproc
        nop

	.end _haltproc
	.size _haltproc, .-_haltproc

        .comm __stkptr,4
        .comm __dl_fini,4

        .comm operatingsystem_parameter_envp,4
        .comm operatingsystem_parameter_argc,4
        .comm operatingsystem_parameter_argv,4

