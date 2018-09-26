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
1:
	auipc	x5,%pcrel_hi(__dl_fini)
	sd	x10, %pcrel_lo(1b)(x5)
	jal	x0, _start

	.globl	_start
	.type	_start, function
_start:
	.option push
	.option norelax
1:  auipc gp, %pcrel_hi(__bss_start+0x800)
	addi  gp, gp, %pcrel_lo(1b)
	.option pop
  
	/* Get argc, argv, envp */
	ld		x5,(x2)
	addi	x6,x2,8
	addi	x7,x5,1
	slli    x7,x7,3
	add 	x7,x6,x7

	/* Save argc, argv, envp, and initial stack pointer */
1:auipc	x8,%pcrel_hi(operatingsystem_parameter_argc)
	sw	x5,%pcrel_lo(1b)(x8)
1:auipc	x8,%pcrel_hi(operatingsystem_parameter_argv)
	sd	x6,%pcrel_lo(1b)(x8)
1:auipc	x8,%pcrel_hi(operatingsystem_parameter_envp)
	sd	x7,%pcrel_lo(1b)(x8)
1:auipc	x5,%pcrel_hi(__stkptr)
	addi	x6, x2, 0
	sd	x6,%pcrel_lo(1b)(x5)
	
	/* Initialise FP to zero */
	addi	x8,x0,0

	/* Call main */
	jal x1, PASCALMAIN

	.globl	_haltproc
	.type	_haltproc,function
_haltproc:
1:auipc x10,%pcrel_hi(__dl_fini)
	ld	x10,%pcrel_lo(1b)(x10)
	beq	x10,x0,.Lexit
	jalr x1,x10
.Lexit:
1:auipc x10,%pcrel_hi(operatingsystem_result)
	ld	x10,%pcrel_lo(1b)(x10)
	addi	x17, x0, 94
	ecall
	jal x0, _haltproc

	/* Define a symbol for the first piece of initialized data. */
	.data
	.align 4
	.globl __data_start
__data_start:
	.quad 0
	.weak data_start
	data_start = __data_start

	.bss
	.align 4

	.comm __dl_fini,8
	.comm __stkptr,8

	.comm operatingsystem_parameter_envp,8
	.comm operatingsystem_parameter_argc,4
	.comm operatingsystem_parameter_argv,8

	.section .note.GNU-stack,"",%progbits
