/*
   Start-up code for Free Pascal Compiler when linking with C library
   with profiling support.

   Written by Edmund Grimley Evans in 2015 and released into the public domain.
*/

	.text
	.align 2

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
	adrp	x0,:got:main_stub
	ldr	x0,[x0,#:got_lo12:main_stub]
	adrp	x3,:got:_init_dummy
	ldr	x3,[x3,#:got_lo12:_init_dummy]
	adrp	x4,:got:_fini_dummy
	ldr	x4,[x4,#:got_lo12:_fini_dummy]
	bl	__libc_start_main

	/* This should never happen */
	b	abort

	.globl	_init_dummy
	.type	_init_dummy,#function
_init_dummy:
	ret

	.globl	_fini_dummy
	.type	_fini_dummy,#function
_fini_dummy:
	ret

	.globl	main_stub
	.type	main_stub,#function
main_stub:
	stp	x29,x30,[sp,#-16]!

	/* Save initial stackpointer */
	mov	x0,sp
	adrp	x1,:got:__stkptr
	ldr	x1,[x1,#:got_lo12:__stkptr]
	str	x0,[x1]

	/* Initialize gmon */
	adrp	x0,:got:_start
	ldr	x0,[x0,#:got_lo12:_start]
	adrp	x1,:got:_etext
	ldr	x1,[x1,#:got_lo12:_etext]
	bl	__monstartup
	adrp	x0,:got:_mcleanup
	ldr	x0,[x0,#:got_lo12:_mcleanup]
	bl	atexit

	/* Start the program */
	bl	 PASCALMAIN
	b	 abort

	.globl	_haltproc
	.type	_haltproc,#function
_haltproc:
	/* Return to libc */
	adrp	x1,:got:__stkptr
	ldr	x1,[x1,#:got_lo12:__stkptr]
	ldr	x1,[x1]
	mov	sp,x1
	ldp	x29,x30,[sp],#16
	ret

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
