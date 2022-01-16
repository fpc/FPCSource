/*
   Start-up code for Free Pascal Compiler, not in a shared library,
   not linking with C library.

   Written by Edmund Grimley Evans in 2015 and released into the public domain.
*/

	.text
	.align 2

	.globl _dynamic_start
	.type  _dynamic_start,#function
_dynamic_start:
	ldr	x10,=__dl_fini
	str	x0,[x10]
	b	_start

	.globl	_start
	.type	_start,#function
_start:
	/* Initialise FP to zero */
	mov	x29,#0

	/* Get argc, argv, envp */
	ldr	x1,[sp]
	add	x2,sp,#8
	add	x11,x1,#1
	add	x11,x2,x11,lsl #3

	/* Save argc, argv, envp, and initial stack pointer */
	ldr	x10,=operatingsystem_parameter_argc
	str	x1,[x10]
	ldr	x10,=operatingsystem_parameter_argv
	str	x2,[x10]
	ldr	x10,=operatingsystem_parameter_envp
	str	x11,[x10]
	ldr	x10,=__stkptr
	mov	x6,sp
	str	x6,[x10]

	/* Call main */
	bl	PASCALMAIN

	.globl	_haltproc
	.type	_haltproc,#function
_haltproc:
	ldr	x10,=__dl_fini
	ldr	x0,[x10]
	cbz	x0,.Lexit
	blr	x0
.Lexit:
	ldr	x10,=operatingsystem_result
	ldr	w0,[x10]
	mov	w8,#94 // syscall_nr_exit_group
	svc	#0
	b	_haltproc

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
