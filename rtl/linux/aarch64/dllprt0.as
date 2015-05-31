/*
   Start-up code for Free Pascal Compiler in a shared library,
   not linking with C library.

   Written by Edmund Grimley Evans in 2015 and released into the public domain.
*/

	.text
	.align 2

	.globl	_startlib
	.type	_startlib,#function
_startlib:
	.globl	FPC_SHARED_LIB_START
	.type	FPC_SHARED_LIB_START,#function
FPC_SHARED_LIB_START:
	stp	x29,x30,[sp,#-16]!

	/* Save argc, argv and envp */
	adrp	x9,:got:operatingsystem_parameter_argc
	ldr	x9,[x9,#:got_lo12:operatingsystem_parameter_argc]
	str	x0,[x9]
	adrp	x9,:got:operatingsystem_parameter_argv
	ldr	x9,[x9,#:got_lo12:operatingsystem_parameter_argv]
	str	x1,[x9]
	adrp	x9,:got:operatingsystem_parameter_envp
	ldr	x9,[x9,#:got_lo12:operatingsystem_parameter_envp]
	str	x2,[x9]

	/* Save initial stackpointer */
	adrp	x9,:got:__stkptr
	ldr	x9,[x9,#:got_lo12:__stkptr]
	mov	x10,sp
	str	x10,[x9]

	/* Call main */
	bl	PASCALMAIN

	/* Return */
	ldp	x29,x30,[sp],#16
	ret

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

	.comm __dl_fini,8
	.comm __stkptr,8

	.comm operatingsystem_parameter_envp,8
	.comm operatingsystem_parameter_argc,8
	.comm operatingsystem_parameter_argv,8

	.section .note.GNU-stack,"",%progbits
