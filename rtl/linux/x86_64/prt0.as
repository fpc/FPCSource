#
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 2002 by Florian Klaempfl
#   members of the Free Pascal development team.
#
#   See the file COPYING.FPC, included in this distribution,
#   for details about the copyright.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY;without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
#**********************************************************************}
#
# Linux ELF startup code for Free Pascal
#

/* This is the canonical entry point, usually the first thing in the text
   segment.  The document "System V Application Binary Interface AMD64
   Architecture Processor Supplement Version 0.99.5" pg. 30 defines that
   the entry point runs, most registers' values are unspecified, except
   for:

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
	.globl _dynamic_start
	.type _dynamic_start,@function
_dynamic_start:
        movq    __dl_fini@GOTPCREL(%rip),%rax
        movq    %rdx,(%rax)
        jmp _start

        .text
	.globl _start
	.type _start,@function
_start:
        movq    entryinfo@GOTPCREL(%rip),%rdi /* load address of entryinfo variable into argument for SysEntry */

	popq    %rsi      /* Pop the argument count.  */
        movq    %rsi,56(%rdi)
        movq    %rsp,64(%rdi)   /* argv starts just at the current stack top */
        leaq    8(,%rsi,8),%rax
        addq    %rsp,%rax
        movq    %rax,72(%rdi)

        andq    $~15,%rsp   /* Align the stack to a 16 byte boundary to follow the ABI */

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

        xorq    %rbp, %rbp
        call    FPC_SysEntry@PLT

	jmp	_haltproc

        .globl  _haltproc
        .type   _haltproc,@function
_haltproc:
        movq    __dl_fini@GOTPCREL(%rip),%rax
        movq    (%rax),%rax
        testq   %rax,%rax
        jz .LNoDlFiniCall
        call    *%rax
.LNoDlFiniCall:

        movq 	operatingsystem_result@GOTPCREL(%rip),%rax
        movzwl  (%rax),%edi
        movl    $231,%eax                 /* exit_group call */
        syscall
        jmp     _haltproc

/* Define a symbol for the first piece of initialized data.  */
	.data
	.globl __data_start
__data_start:
	.long 0
	.weak data_start
        data_start = __data_start

.bss
        .comm __dl_fini,8

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
