/*
   Start-up code for Free Pascal Compiler when linking with C library.

   Written by Edmund Grimley Evans in 2015 and released into the public domain.
*/

	.text
	.align 2

	.globl __libc_csu_init
	.type __libc_csu_init,#function
	.weak __libc_csu_init
__libc_csu_init:
	ret

	.globl __libc_csu_fini
	.type __libc_csu_fini,#function
	.weak __libc_csu_fini
__libc_csu_fini:
	ret

	.globl _start
	.type  _start,#function
_start:
	/* Initialise FP to zero */
	mov	x29,#0

	/* This is rtld_fini */
	mov	x5,x0

	/* Get argc, argv, envp */
	ldr	x1,[sp]
	add	x2,sp,#8
	add	x11,x1,#1
	add	x11,x2,x11,lsl #3

	/* Save argc, argv, envp, and initial stack pointer */
	adrp	x10,:got:operatingsystem_parameter_argc
	ldr	x10,[x10,#:got_lo12:operatingsystem_parameter_argc]
	str	x1,[x10]
	adrp	x10,:got:operatingsystem_parameter_argv
	ldr	x10,[x10,#:got_lo12:operatingsystem_parameter_argv]
	str	x2,[x10]
	adrp	x10,:got:operatingsystem_parameter_envp
	ldr	x10,[x10,#:got_lo12:operatingsystem_parameter_envp]
	str	x11,[x10]
	adrp	x10,:got:__stkptr
	ldr	x10,[x10,#:got_lo12:__stkptr]
	mov	x6,sp
	str	x6,[x10]

	/* __libc_start_main(main, argc, argv,
	                     init, fini, rtld_fini, stack_end) */
	adrp	x0,:got:PASCALMAIN
	ldr	x0,[x0,#:got_lo12:PASCALMAIN]
	adrp	x3,:got:__libc_csu_init
	ldr	x3,[x3,#:got_lo12:__libc_csu_init]
	adrp	x4,:got:__libc_csu_fini
	ldr	x4,[x4,#:got_lo12:__libc_csu_fini]
	bl	__libc_start_main

	/* This should never happen */
	b	abort

	.globl	_haltproc
	.type	_haltproc,#function
_haltproc:
	adrp	x0,:got:operatingsystem_result
	ldr	x0,[x0,#:got_lo12:operatingsystem_result]
	ldr	w0,[x0]
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

	.comm __stkptr,8

	.comm operatingsystem_parameter_envp,8
	.comm operatingsystem_parameter_argc,8
	.comm operatingsystem_parameter_argv,8

	.section .note.GNU-stack,"",%progbits
