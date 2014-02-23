#
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

/*
   %a1          Contains a function pointer to be registered with `atexit'.
                This is how the dynamic linker arranges to have DT_FINI
                functions called for shared libraries that have been loaded
                before this code runs.

   %sp          The stack contains the arguments and environment:
                0(%sp)                        argc
                4(%sp)                        argv[0]
                ...
                (4*argc)(%sp)                NULL
                (4*(argc+1))(%sp)        envp[0]
                ...
                                        NULL
*/

        .text
        .globl _start
        .type _start,@function
_start:
        /* Clear the frame pointer.  The ABI suggests this be done, to mark
           the outermost frame obviously.  */
        sub.l %fp, %fp

        /* Extract the arguments as encoded on the stack.  */
        move.l (%sp), %d0
        move.l %d0, operatingsystem_parameter_argc
        lea.l 4(%sp), %a0
        move.l %a0, operatingsystem_parameter_argv
        lea.l 8(%sp,%d0.l*4), %a0
        move.l %a0, operatingsystem_parameter_envp

#        move.l 8(%sp), %d0
#        move.l %d0, operatingsystem_parameter_envp
#        move.l 4(%sp), %d0
#        move.l %d0, operatingsystem_parameter_argv
#        move.l (%sp), %d0
#        move.l %d0, operatingsystem_parameter_argc

        jbsr PASCALMAIN

        illegal                        /* Crash if somehow `exit' does return.  */

.file "prt0.as"
	.text
   .globl __entry
__entry:
   jsr  PASCALMAIN


        .globl  _haltproc
        .type   _haltproc,@function
_haltproc:
        move.l #1,%d0
        move.l operatingsystem_result,%d1
        trap  #0
        bras  _haltproc

.bss
        .type   __stkptr,@object
        .size   __stkptr,4
        .global __stkptr
__stkptr:
        .skip   4

        .type operatingsystem_parameters,@object
        .size operatingsystem_parameters,12
operatingsystem_parameters:
        .skip 3*4

        .global operatingsystem_parameter_envp
        .global operatingsystem_parameter_argc
        .global operatingsystem_parameter_argv
        .set operatingsystem_parameter_envp,operatingsystem_parameters+0
        .set operatingsystem_parameter_argc,operatingsystem_parameters+4
        .set operatingsystem_parameter_argv,operatingsystem_parameters+8

