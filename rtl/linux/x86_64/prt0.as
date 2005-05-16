#
#   $Id: prt0.as,v 1.10 2005/02/05 23:02:37 florian Exp $
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
#       movq %rdx,%r9                 /* Address of the shared library termination
#               	                 function.  */
	popq     %rsi		      /* Pop the argument count.  */
        movq     %rsi,operatingsystem_parameter_argc
	movq     %rsp,operatingsystem_parameter_argv   /* argv starts just at the current stack top.  */
        leaq     8(,%rsi,8),%rax
        addq     %rsp,%rax
        movq     %rax,operatingsystem_parameter_envp
        andq     $~15,%rsp            /* Align the stack to a 16 byte boundary to follow the ABI.  */

        xorq    %rbp, %rbp
        call    PASCALMAIN
	jmp	_haltproc

        .globl  _haltproc
        .type   _haltproc,@function
_haltproc:
        movl    $60,%eax                 /* exit call */
        movzwl    operatingsystem_result,%edi
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
        .comm operatingsystem_parameter_envp,8
        .comm operatingsystem_parameter_argc,8
        .comm operatingsystem_parameter_argv,8


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

#
# $Log: prt0.as,v $
# Revision 1.10  2005/02/05 23:02:37  florian
#   + added some missing c types
#
# Revision 1.9  2004/11/02 15:26:21  florian
#   * fixed sse exception handling
#
# Revision 1.8  2004/07/03 21:50:31  daniel
#   * Modified bootstrap code so separate prt0.as/prt0_10.as files are no
#     longer necessary
#
# Revision 1.7  2004/04/24 17:14:09  florian
#   * prt0.as exit code handling fixed
#   * int64 mod int64 for negative numbers fixed
#
# Revision 1.6  2004/04/20 20:30:11  florian
#   * fixed halt code
#
# Revision 1.5  2004/04/12 19:05:55  florian
#   + haltproc added
#
# Revision 1.4  2004/02/20 23:48:27  peter
#   * c stub implemented
#
# Revision 1.3  2004/02/08 15:33:50  florian
#   * linking problems fixed
#   + abi tag added
#
# Revision 1.2  2004/02/02 21:02:38  peter
#   * fixed syntax errors
#
# Revision 1.1  2003/01/06 19:33:10  florian
#   + initial revision
#
#
