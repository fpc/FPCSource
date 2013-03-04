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
# Program startup code for Free Pascal. Android-ARM target.
#

/* At this entry point, most registers' values are unspecified, except:

   sp           The stack contains the arguments and environment:
                0(sp)                   argc
                4(sp)                   argv[0]
                ...
                (4*argc)(sp)            NULL
                (4*(argc+1))(sp)        envp[0]
                ...
                                        NULL
*/

/*
   In our entry point we should save pointers to cmd line arguments
   and environment vars, then pass control to libc startup code.
   It will call "PASCALMAIN" via alias "main".
*/

.text
        .globl _fpc_start
        .type _fpc_start,#function
_fpc_start:
        /* Clear the frame pointer since this is the outermost frame.  */
        mov fp, #0
        /* Save initial stackpointer */
        ldr ip,=__stkptr
        str sp,[ip]
        mov r4,sp
        /* Pop argc off the stack and save a pointer to argv */
        ldmia r4!, {r5}
        ldr ip,=operatingsystem_parameter_argc
        str r5,[ip]
        ldr ip,=operatingsystem_parameter_argv
        str r4,[ip]

        /* calc envp */
        add r5,r5,#1
        add r5,r4,r5,LSL #2
        ldr ip,=operatingsystem_parameter_envp
        str r5,[ip]
        
        /* Finally go to libc startup code. It will call "PASCALMAIN" via alias "main" */
        ldr ip,=_start
        bx ip

/* --------------------------------------------------------- */
        .globl  _haltproc
        .type   _haltproc,#function
_haltproc:
        .globl  _haltproc_eabi
        .type   _haltproc_eabi,#function
_haltproc_eabi:
        ldr r0,=operatingsystem_result
        ldr r0,[r0]
        /* Go to libc exit() */
        ldr ip,=exit
        bx ip

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
