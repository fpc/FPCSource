#
#   $Id$
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
        movq     %rsi,U_SYSTEM_ARGC
	movq     %rsp,U_SYSTEM_ARGV   /* argv starts just at the current stack top.  */
        leaq     8(%rsi,,8),%rax
        addq     %esp,%rax
        movq     %rax,U_SYSTEM_ENVP
        andq     $~15,%rsp            /* Align the stack to a 16 byte boundary to follow the ABI.  */

/* !!!! CPU initialization? */

        xorq    %rbp, %rbp
        call    PASCALMAIN

	hlt                           /* Crash if somehow `exit' does return.	 */

/*!!!! hlt syscall? */

/* Define a symbol for the first piece of initialized data.  */
	.data
	.globl __data_start
__data_start:
	.long 0
	.weak data_start
        data_start = __data_start
#
# $Log$
# Revision 1.1  2003-01-06 19:33:10  florian
#   + initial revision
#
#
