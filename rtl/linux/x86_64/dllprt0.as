#
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 2006 by Florian Klaempfl
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
.section .init
	.align 16
	.globl FPC_SHARED_LIB_START
	.type FPC_SHARED_LIB_START,@function
FPC_SHARED_LIB_START:
	jmp	_startlib@PLT

        .text
	.globl _startlib
	.type _startlib,@function
_startlib:
        movq    entryinfo@GOTPCREL(%rip),%r10 /* load address of entryinfo variable */

        movq    %rdi,56(%r10)
        movq    %rsi,64(%r10)   /* argv starts just at the current stack top */
        movq    %rdx,72(%r10)

        /* Save initial stackpointer */
        movq    %rsp,80(%r10)

        /* store stack length */
        movq    __stklen@GOTPCREL(%rip),%rax
        movq    %rax,88(%r10)

        /* store pointer to _haltproc */
        movq    _haltproc@GOTPCREL(%rip),%rax
        movq    %rax,96(%r10)

        /* populate the table pointers */
        movq    INITFINAL@GOTPCREL(%rip),%rax
        movq    %rax,(%r10)
        movq    FPC_THREADVARTABLES@GOTPCREL(%rip),%rax
        movq    %rax,8(%r10)
        movq    FPC_RESOURCESTRINGTABLES@GOTPCREL(%rip),%rax
        movq    %rax,16(%r10)
        movq    FPC_RESSTRINITTABLES@GOTPCREL(%rip),%rax
        movq    %rax,24(%r10)
        movq    FPC_RESLOCATION@GOTPCREL(%rip),%rax
        movq    %rax,32(%r10)
        movq    PASCALMAIN@GOTPCREL(%rip),%rax
        movq    %rax,40(%r10)
        /* valgrind_used can stay 0 */

        /* setup argument for FPC_SysEntry */
        movq    %r10,%rdi

        movq    operatingsystem_islibrary@GOTPCREL(%rip),%r10
        movb    $1,(%r10)

        call    FPC_SysEntry@PLT
	ret

/* this routine is only called when the halt() routine of the RTL embedded in
  the shared library is called */
        .globl  _haltproc
        .type   _haltproc,@function
_haltproc:
        movl    $231,%eax                 /* exit_group call */
        movq    operatingsystem_result@GOTPCREL(%rip),%rbx
        movzwl  (%rbx),%edi
        syscall
        jmp     _haltproc@PLT

/* Define a symbol for the first piece of initialized data.  */
	.data
	.globl __data_start
__data_start:
	.long 0
	.weak data_start
        data_start = __data_start

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

Makes ld choke:
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
*/
	.section	.note.GNU-stack,"",@progbits
