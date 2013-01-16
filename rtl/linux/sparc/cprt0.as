        .section ".text"
        .align 4
        .global _start
        .type _start,#function
_start:

  /* Terminate the stack frame, and reserve space for functions to
     drop their arguments.  */
        mov     %g0, %fp
        sub     %sp, 6*4, %sp

.ifdef PIC
        /* Set %l7 to _GLOBAL_OFFSET_TABLE value */
        sethi %hi(_GLOBAL_OFFSET_TABLE_-8),%l7
        or %l7,%lo(_GLOBAL_OFFSET_TABLE_-4),%l7
        call FPC_GETGOT
        nop
.endif

  	/* Extract the arguments and environment as encoded on the stack.  The
     	   argument info starts after one register window (16 words) past the SP.  */
	       ld	[%sp+22*4], %o2
        sethi	%hi(operatingsystem_parameter_argc),%o1
        or	%o1,%lo(operatingsystem_parameter_argc),%o1
.ifdef PIC
        ld      [%o1+%l7],%o1
.endif
       	st	%o2, [%o1]

        add	%sp, 23*4, %o0
        sethi	%hi(operatingsystem_parameter_argv),%o1
       	or	%o1,%lo(operatingsystem_parameter_argv),%o1
.ifdef PIC
        ld      [%o1+%l7],%o1
.endif
       	st	%o0, [%o1]

  	/* envp=(argc+1)*4+argv */
       	inc     %o2
       	sll     %o2, 2, %o2
        add	%o2, %o0, %o2
       	sethi	%hi(operatingsystem_parameter_envp),%o1
       	or	%o1,%lo(operatingsystem_parameter_envp),%o1
.ifdef PIC
        ld      [%o1+%l7],%o1
.endif
       	st	%o2, [%o1]

    /* Save initial stackpointer */
	sethi	%hi(__stkptr),%o1
	or	%o1,%lo(__stkptr),%o1
.ifdef PIC
        ld      [%o1+%l7],%o1
.endif
	st	%sp, [%o1]

  /* reload the addresses for C startup code  */
        ld      [%sp+22*4], %o1
        add     %sp, 23*4, %o2


  /* Load the addresses of the user entry points.  */
        sethi   %hi(PASCALMAIN), %o0
        sethi   %hi(_init), %o3
        sethi   %hi(_fini), %o4
        or      %o0, %lo(PASCALMAIN), %o0
.ifdef PIC
        ld      [%o0+%l7],%o0
.endif
        or      %o3, %lo(_init), %o3
.ifdef PIC
        ld      [%o3+%l7],%o3
.endif
        or      %o4, %lo(_fini), %o4
.ifdef PIC
        ld      [%o4+%l7],%o4
.endif

  /* When starting a binary via the dynamic linker, %g1 contains the
     address of the shared library termination function, which will be
     registered with atexit().  If we are statically linked, this will
     be NULL.  */
        mov     %g1, %o5

  /* Let libc do the rest of the initialization, and call main.  */
        call    __libc_start_main
         nop

  /* Die very horribly if exit returns.  */
        unimp

        .size _start, .-_start

								.globl  _haltproc
        .type   _haltproc,@function
        _haltproc:
       	mov	188, %g1			/* "exit" system call */
       	ta	0x10			/* dot the system call */
       	nop				/* delay slot */
       	/* Die very horribly if exit returns.  */
       	unimp

.data

        .comm __stkptr,4

        .comm operatingsystem_parameter_envp,4
        .comm operatingsystem_parameter_argc,4
        .comm operatingsystem_parameter_argv,4
