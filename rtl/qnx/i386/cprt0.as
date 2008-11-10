//
//Copyright 2001, QNX Software Systems Ltd. All Rights Reserved
//
// QNX has kindly released this source code under the QNX open 
// Community license, expressly to be used with  the 
// Free Pascal runtime library
//

.extern		main
.extern		exit
.extern         _fini
.extern         _init
.extern		atexit
.extern		errno

	.text
	.byte	'N', 'I', 'A', 'M'      /* Used by debugger for setting a break point */
    .long	main
	
#if defined(VARIANT_wcc)
.globl		_cstart_
_cstart_:
    jmp		_CMain
	.type	_cstart_,@function
	.size	_cstart_,.-_cstart_
#else

.globl		_start
_start:
#if defined(__PIC__)
//	call	1f
//1:	popl	%ebx
//	addl	$_GLOBAL_OFFSET_TABLE_+[.-1b],%ebx
//    	call	_CMain@PLT
//#else

	/* Stash EDX to EBX */
	movl	%edx,%ebx
	/* Call _init_libc -- need to extract argc, argv, env and auxv first */
	// argc
	movl 0(%esp),%esi
	movl 0(%esp), %eax
	movl %eax, U_SYSTEM_ARGC
	// argv
	leal 4(%esp),%ecx
    movl  %ecx, U_SYSTEM_ARGV
	// envp
	leal (%ecx,%esi,4),%eax
    // auxv vector pointer
	leal 4(%eax),%edi
	// pointer to environment
	leal 8(%eax),%edx
	movl %edx, U_SYSTEM_ENVP
	cmpl $0,4(%eax)
	je .Ldoneargv
	.align 4
	// Scan for auxv
.Lloop:
	movl (%edx),%eax
	addl $4,%edx
	testl %eax,%eax
	jne .Lloop
.Ldoneargv:
	pushl %ebx
	pushl %edx
	pushl %edi
	pushl %ecx
	pushl %esi
	call _init_libc
	// Leave the args on the stack, we'll just pass them to main()
    pushl	$_fini
	call	atexit
	addl	$4,%esp
	call 	_init
.Lmain:
	movl	$0,errno
    	call	PASCALMAIN
	pushl	%eax
	call	exit
//#endif
	int		$3	/* Should never get here.... */
	.type	_start,@function
	.size	_start,.-_start
#endif
