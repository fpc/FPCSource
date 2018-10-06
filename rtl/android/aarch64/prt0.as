#
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 2018 by Yuriy Sydorov and other
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
# Program startup code for Free Pascal. Android-aarch64 target.
#

/* At this entry point, most registers' values are unspecified, except:

   sp           The stack contains the arguments and environment:
                0(sp)                   argc
                8(sp)                   argv[0]
                ...
                (8*argc)(sp)            NULL
                (8*(argc+1))(sp)        envp[0]
                ...
                                        NULL
*/

/*
   In our entry point we should save pointers to cmd line arguments
   and environment vars, then pass control to libc startup code.
   It will call "PASCALMAIN" via alias "main".
*/

.file   "prt0.as"
.text
.align 2
        .globl _fpc_start
        .type _fpc_start,#function
_fpc_start:
        /* Clear the frame pointer since this is the outermost frame.  */
        mov x29,#0
        /* Save initial stackpointer and slightly adjust it */
        adrp x14,:got:__stkptr
        ldr	x14,[x14,#:got_lo12:__stkptr]
        mov x0,sp
        add x1,x0,#-64
        str x1,[x14]
        /* Get argc off the stack and save a pointer to argv */
        ldr w1,[x0]
        adrp x14,:got:operatingsystem_parameter_argc
        ldr	x14,[x14,#:got_lo12:operatingsystem_parameter_argc]
        str w1,[x14]
        adrp x14,:got:operatingsystem_parameter_argv
        ldr	x14,[x14,#:got_lo12:operatingsystem_parameter_argv]
        add x0,x0,#8
        str x0,[x14]

        /* calc envp */
        add x1,x1,#1
        add x1,x0,x1,LSL #3
        adrp x14,:got:operatingsystem_parameter_envp
        ldr	x14,[x14,#:got_lo12:operatingsystem_parameter_envp]
        str x1,[x14]
        
        /* Finally go to libc startup code. It will call "PASCALMAIN" via alias "main" */
        b _start

/* --------------------------------------------------------- */
        .globl  _haltproc
        .type   _haltproc,#function
_haltproc:
        /* Simply call libc exit(). _haltproc has the same declaration as exit. */
        bl exit

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
        .comm __stkptr,8
        .comm operatingsystem_parameter_envp,8
        .comm operatingsystem_parameter_argc,4
        .comm operatingsystem_parameter_argv,8
