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
        /* GOT init */
        call    fpc_geteipasebx
        addl    $_GLOBAL_OFFSET_TABLE_,%ebx
        /* Clear the frame pointer since this is the outermost frame.  */
        xorl    %ebp,%ebp
        /* Save initial stackpointer */
        movl    __stkptr@GOT(%ebx),%eax
        movl    %esp,(%eax)
        /* First locate the start of the environment variables */
        /* Get argc in ecx */
        movl    (%esp),%ecx
        /* Save argc */
        movl    operatingsystem_parameter_argc@GOT(%ebx),%eax
        movl    %ecx,(%eax)
        /* Get argv pointer in edx */
        leal    4(%esp),%edx
        /* Save argv */
        movl    operatingsystem_parameter_argv@GOT(%ebx),%eax
        movl    %edx,(%eax)
        /* The start of the environment is: esp+ecx*4+12 */
        leal    12(%esp,%ecx,4),%edx
        /* Save envp */
        movl    operatingsystem_parameter_envp@GOT(%ebx),%eax
        movl    %edx,(%eax)
        
        /* Finally go to libc startup code. It will call "PASCALMAIN" via alias "main". */
        /* No need to align stack since it will aligned by libc. */
        jmp _start

/* --------------------------------------------------------- */
        .globl  _haltproc
        .type   _haltproc,@function
_haltproc:
        /* GOT init */
        call    fpc_geteipasebx
        addl    $_GLOBAL_OFFSET_TABLE_,%ebx
        /* Jump to libc exit(). _haltproc has the same declaration as exit. */
        jmp     exit@PLT

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
