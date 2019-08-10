/*
   Start-up code for Free Pascal Compiler, not in a shared library,
   not linking with C library.

   Written by Edmund Grimley Evans in 2015 and released into the public domain.
*/

	.text
	.align 2

	.globl _dynamic_start
	.type  _dynamic_start, function
_dynamic_start:
	lui	x5,%hi(__dl_fini)
	addi	x5,x5,%lo(__dl_fini)
	sw	x10, (x5)
	jal	x0, _start

	.globl	_start
	.type	_start, function
_start:
	/* Initialise FP to zero */
	addi	x2,x0,0

	/* Get argc, argv, envp */
	lw	x5,(x2)
	addi	x6,x2,8
	addi	x7,x5,1
	slli  x7,x7,3
	add 	x7,x6,x7

	/* Save argc, argv, envp, and initial stack pointer */
	lui	x8,%hi(operatingsystem_parameter_argc)
	addi x8,x8,%lo(operatingsystem_parameter_argc)
	sw	x5,(x8)
	lui	x8,%hi(operatingsystem_parameter_argv)
	addi x8,x8,%lo(operatingsystem_parameter_argv)
	sw	x6,(x8)
	lui	x8,%hi(operatingsystem_parameter_envp)
	addi x8,x8,%lo(operatingsystem_parameter_envp)
	sw	x7,(x8)
	lui	x5,%hi(__stkptr)
	addi x5,x8,%lo(__stkptr)
	addi	x6, x2, 0
	sw	x6,(x5)

	/* Call main */
	jal x1, PASCALMAIN

	.globl	_haltproc
	.type	_haltproc,function
_haltproc:
	lui x10,%hi(__dl_fini)
	addi x10,x10,%lo(__dl_fini)
	lw	x10,(x10)
	beq	x10,x0,.Lexit
	jalr x1,x10
.Lexit:
	lui x10,%hi(operatingsystem_result)
	addi x10,x10,%lo(operatingsystem_result)
	lw	x10,(x10)
	addi	x17, x0, 94
	scall
	jal x0, _haltproc

	/* Define a symbol for the first piece of initialized data. */
	.data
	.align 3
	.globl __data_start
__data_start:
	.long 0
	.weak data_start
	data_start = __data_start

	.bss
	.align 3

	.comm __dl_fini,8
	.comm __stkptr,8

	.comm operatingsystem_parameter_envp,8
	.comm operatingsystem_parameter_argc,8
	.comm operatingsystem_parameter_argv,8

	.section .note.GNU-stack,"",%progbits
