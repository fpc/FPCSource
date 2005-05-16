#
#   $Id: prt0.as,v 1.5 2004/07/03 23:04:34 daniel Exp $
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 1999-2004 by Michael Van Canneyt, Peter Vreman,
#   & Daniel Mantione, members of the Free Pascal development team.
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
# The code in this file is the default startup code, it is used unless
# libc is linked in, profiling is enabled or you are compiling a shared
# library.
#
#
# Stack layout at program start:
# 
#         nil
#         envn
#         ....
#         ....           ENVIRONMENT VARIABLES
#         env1
#         env0
#         nil
#         argn
#         ....
#         ....           COMMAND LINE OPTIONS
#         arg1
#         arg0
#         argc <--- esp
#

        .file   "prt0.as"
        .text
        .globl  _start
        .type   _start,@function
_start:
        /* First locate the start of the environment variables */
        popl    %ecx                    /* Get argc in ecx */
        movl    %esp,%ebx               /* Esp now points to the arguments */
	leal    4(%esp,%ecx,4),%eax     /* The start of the environment is: esp+4*eax+4 */
        andl    $0xfffffff8,%esp        /* Align stack */

	leal    operatingsystem_parameters,%edi
	stosl	/* Move the environment pointer */
	xchg    %ecx,%eax
	stosl   /* Move the argument counter    */
	xchg	%ebx,%eax
	stosl   /* Move the argument pointer    */


        fninit                           /* initialize fpu */
        fwait
        fldcw   ___fpucw

        xorl    %ebp,%ebp
        call    PASCALMAIN

        .globl  _haltproc
        .type   _haltproc,@function
_haltproc:
_haltproc2:		# GAS <= 2.15 bug: generates larger jump if a label is exported
	xorl    %eax,%eax
	incl    %eax			/* eax=1, exit call */
        movzwl  operatingsystem_result,%ebx
        int     $0x80
        jmp     _haltproc2

.data
___fpucw:
        .long   0x1332


.bss
        .type   ___fpc_brk_addr,@object
	.comm   ___fpc_brk_addr,4        /* heap management */

operatingsystem_parameters:
	.skip 3*4

	.global operatingsystem_parameter_envp
	.global operatingsystem_parameter_argc
	.global operatingsystem_parameter_argv
	.set operatingsystem_parameter_envp,operatingsystem_parameters+0
	.set operatingsystem_parameter_argc,operatingsystem_parameters+4
	.set operatingsystem_parameter_argv,operatingsystem_parameters+8
#
# $Log: prt0.as,v $
# Revision 1.5  2004/07/03 23:04:34  daniel
#   * Updated comments
#
# Revision 1.4  2004/07/03 21:50:31  daniel
#   * Modified bootstrap code so separate prt0.as/prt0_10.as files are no
#     longer necessary
#
# Revision 1.3  2002/09/07 16:01:20  peter
#   * old logs removed and tabs fixed
#
