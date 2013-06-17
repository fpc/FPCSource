#
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 2013 by Yury Sidorov and other
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
# Program startup code for Free Pascal. Android-i386 target.
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

/*
   In our entry point we should save pointers to cmd line arguments
   and environment vars, then pass control to libc startup code.
   It will call "PASCALMAIN" via alias "main".
*/
        .file   "prt0.as"
.text
        .align 4
        .globl _fpc_start
        .type _fpc_start,@function
_fpc_start:
        /* Clear the frame pointer since this is the outermost frame.  */
        xorl    %ebp,%ebp
        /* Save initial stackpointer */
        movl    %esp,__stkptr
        /* First locate the start of the environment variables */
        /* Get argc in ecx */
        movl    (%esp),%ecx
        /* Save argc */
        movl    %ecx,operatingsystem_parameter_argc
        /* Get argv pointer in ebx */
        leal    4(%esp),%ebx
        /* Save argv */
        movl    %ebx,operatingsystem_parameter_argv
        /* The start of the environment is: esp+ecx*4+12 */
        leal    12(%esp,%ecx,4),%eax
        /* Save envp */
        movl    %eax,operatingsystem_parameter_envp
        
        /* Finally go to libc startup code. It will call "PASCALMAIN" via alias "main" */
        jmp _start

/* --------------------------------------------------------- */
        .globl  _haltproc
        .type   _haltproc,@function
_haltproc:
        movzwl  operatingsystem_result,%ebx
        pushl   %ebx
        /* Call libc exit() */
        call    exit
        
/* --------------------------------------------------------- */
.data
/* Define a symbol for the first piece of initialized data.  */
        .globl __data_start
__data_start:
        .long 0
        .weak data_start
        data_start = __data_start

/* --------------------------------------------------------- */
.bss
        .comm __stkptr,4
        .comm operatingsystem_parameter_envp,4
        .comm operatingsystem_parameter_argc,4
        .comm operatingsystem_parameter_argv,4
