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
	.ent	_dynamic_start
	.type _dynamic_start,@function
_dynamic_start:
        .set noreorder
	.cpload $25
        /* TODO: check whether this code is correct */
        la      $v0,__dl_fini
        lw      $v0,($v0)
	lw      $v1,%call16(_start)($gp)
	move    $t9,$v1
        jalr	$t9
	nop

	.end	_dynamic_start
	.size 	_dynamic_start, .-_dynamic_start

	.align 4
	.global _start
        .set    nomips16
        .ent    _start
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
        /* load fp */
        .set noreorder
	.cpload $25
	addiu   $sp,$sp,-32
        move    $s8,$sp
	.cprestore 16
        la      $t1,__stkptr
        sw      $s8,($t1)

        /* align stack */
        li      $at,-8
        and     $sp,$sp,$at

        addiu   $sp,$sp,-32

        lui     $s7,0x3d
        addiu   $s7,$s7,2304
        li      $at,-8
        and     $s7,$s7,$at
        addiu   $s7,$s7,-32

        /* store argc */
        lw      $a0,0($s8)
        la      $a1,operatingsystem_parameter_argc
        sw      $a0,($a1)

        /* store argv */
        addiu   $a1,$s8,4
        la      $a2,operatingsystem_parameter_argv
        sw      $a1,($a2)

        /* store envp */
        addiu   $a2,$a0,1
        sll     $a2,$a2,0x2
        addu    $a2,$a2,$a1
        la      $a3,operatingsystem_parameter_envp
        sw      $a2,($a3)
        la      $t9,PASCALMAIN
        jalr    $t9
        nop
	b       _haltproc
        nop

	.end	_start
	.size 	_start, .-_start

	.globl  _haltproc
	.ent	_haltproc
	.type   _haltproc,@function
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

