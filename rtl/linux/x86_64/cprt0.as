/* This is the canonical entry point, usually the first thing in the text
   segment.  The SVR4/i386 ABI (pages 3-31, 3-32) says that when the entry
   point runs, most registers' values are unspecified, except for:

   %rdx		Contains a function pointer to be registered with `atexit'.
		This is how the dynamic linker arranges to have DT_FINI
		functions called for shared libraries that have been loaded
		before this code runs.

   %rsp		The stack contains the arguments and environment:
		0(%rsp)			argc
		8(%rsp)			argv[0]
		...
		(8*argc)(%rsp)		NULL
		(8*(argc+1))(%rsp)	envp[0]
		...
					NULL
*/

	.text
	.globl _start
	.type _start,@function
_start:
	/* Clear the frame pointer.  The ABI suggests this be done, to mark
	   the outermost frame obviously.  */
	xorq %rbp, %rbp

	/* Extract the arguments as encoded on the stack and set up
	   the arguments for __libc_start_main (int (*main) (int, char **, char **),
		   int argc, char *argv,
		   void (*init) (void), void (*fini) (void),
		   void (*rtld_fini) (void), void *stack_end).
	   The arguments are passed via registers and on the stack:
	main:		%rdi
	argc:		%rsi
	argv:		%rdx
	init:		%rcx
	fini:		%r8
	rtld_fini:	%r9
	stack_end:	stack.	*/

	movq %rdx, %r9		/* Address of the shared library termination
				   function.  */
	popq %rsi		/* Pop the argument count.  */
	movq %rsp, %rdx		/* argv starts just at the current stack top.  */

        movq    entryinfo@GOTPCREL(%rip),%r10 /* load address of entryinfo variable */

        movq    %rsi,56(%r10)
        movq    %rsp,64(%r10)   /* argv starts just at the current stack top */
        leaq    8(,%rsi,8),%rax
        addq    %rsp,%rax
        movq    %rax,72(%r10)

	/* Align the stack to a 16 byte boundary to follow the ABI.  */
	andq  $~15, %rsp

	pushq %rax		/* Push garbage because we push 8 more bytes.  */

	/* Provide the highest stack address to the user code (for stacks
	   which grow downwards).  */
	pushq %rsp

	/* Pass address of our own entry points to .fini and .init.  */
	movq __libc_csu_init@GOTPCREL(%rip), %rcx
	movq __libc_csu_fini@GOTPCREL(%rip), %r8

	movq main_stub@GOTPCREL(%rip), %rdi

	/* Call the user's main function, and exit with its value.
	   But let the libc call main.	  */
	call __libc_start_main@PLT

	hlt			/* Crash if somehow `exit' does return.	 */

/* fake main routine which will be run from libc */
	.globl main_stub
        .type main_stub,@function
main_stub:
        /* save return address */
        popq    %rax

	// stack alignment
	pushq	%rax

	movq    ___fpc_ret_rbp@GOTPCREL(%rip),%rcx
        movq    %rbp,(%rcx)
	movq    ___fpc_ret@GOTPCREL(%rip),%rcx
        movq    %rax,(%rcx)
        pushq   %rax

        /* fill the remaining fields of the entry information */
        movq    entryinfo@GOTPCREL(%rip),%rdi /* load address of entryinfo variable into argument for SysEntry*/

        /* Save initial stackpointer */
        movq    %rsp,80(%rdi)

        /* store stack length */
        movq    __stklen@GOTPCREL(%rip),%rax
        movq    %rax,88(%rdi)

        /* store pointer to _haltproc */
        movq    _haltproc@GOTPCREL(%rip),%rax
        movq    %rax,96(%rdi)

        /* populate the table pointers */
        movq    INITFINAL@GOTPCREL(%rip),%rax
        movq    %rax,(%rdi)
        movq    FPC_THREADVARTABLES@GOTPCREL(%rip),%rax
        movq    %rax,8(%rdi)
        movq    FPC_RESOURCESTRINGTABLES@GOTPCREL(%rip),%rax
        movq    %rax,16(%rdi)
        movq    FPC_RESSTRINITTABLES@GOTPCREL(%rip),%rax
        movq    %rax,24(%rdi)
        movq    FPC_RESLOCATION@GOTPCREL(%rip),%rax
        movq    %rax,32(%rdi)
        movq    PASCALMAIN@GOTPCREL(%rip),%rax
        movq    %rax,40(%rdi)
        /* valgrind_used can stay 0 */

        /* start the program */
        xorq    %rbp,%rbp
        call    FPC_SysEntry@PLT
        hlt
	.size   main_stub,.-main_stub


        .globl _haltproc
        .type _haltproc,@function
_haltproc:
        movl    %edi,%eax

        /* return to libc */
	movq    ___fpc_ret_rbp@GOTPCREL(%rip),%rcx
        movq    (%rcx),%rbp
	movq    ___fpc_ret@GOTPCREL(%rip),%rcx
        popq    %rdx
        movq    (%rcx),%rdx
        pushq    %rdx
	ret
	.size   _haltproc,.-_haltproc

/* Define a symbol for the first piece of initialized data.  */
	.data
	.globl __data_start
__data_start:
	.long 0
	.weak data_start
	data_start = __data_start

        .globl  ___fpc_brk_addr         /* heap management */
        .type   ___fpc_brk_addr,@object
        .size   ___fpc_brk_addr,8
___fpc_brk_addr:
        .quad   0

___fpc_ret:                             /* return address to libc */
        .quad   0
___fpc_ret_rbp:
        .quad   0

.bss
/* the entry information looks like this:

  TEntryInformation = record
    InitFinalTable : Pointer;           // offset 0
    ThreadvarTablesTable : Pointer;     // offset 8
    ResourceStringTables : Pointer;     // offset 16
    ResStrInitTables : Pointer;         // offset 24
    ResLocation : Pointer;              // offset 32
    PascalMain : Procedure;             // offset 40
    valgrind_used : boolean;            // offset 48
    OS : TEntryInformationOS;           // offset 56
  end;

  with TEntryInformationOS being

  TEntryInformationOS = record
    argc: longint;                         // offset 56
    argv: ppchar;                          // offset 64
    envp: ppchar;                          // offset 72
    stkptr: pointer;                       // offset 80
    stklen: sizeuint;                      // offset 88
    haltproc: procedure(e:longint);cdecl;  // offset 96
  end;

  The size of TEntryInformationOS including padding is 5 * sizeof(Pointer) = 40

  The size of TEntryInformation including padding without OS is 8 * sizeof(Pointer) = 64

  Thus the total size of TEntryInformation including padding is 104

*/
        .comm entryinfo,104

/* We need this stuff to make gdb behave itself, otherwise
   gdb will chokes with SIGILL when trying to debug apps.
*/
        .section ".note.ABI-tag", "a"
        .align 4
        .long 1f - 0f
        .long 3f - 2f
        .long  1
0:      .asciz "GNU"
1:      .align 4
2:      .long 0
        .long 2,4,0
3:      .align 4

	.section	.note.GNU-stack,"",@progbits
