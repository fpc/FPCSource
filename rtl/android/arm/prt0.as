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
        /* Get GOT */
        ldr r3,.L_GOT1
.LPIC1:
        add r3,pc,r3

        /* Clear the frame pointer since this is the outermost frame.  */
        mov fp, #0
        /* Save initial stackpointer */
        ldr ip,.L__stkptr
        ldr ip,[r3, ip]
        str sp,[ip]
        mov r0,sp
        /* Pop argc off the stack and save a pointer to argv */
        ldmia r0!, {r1}
        ldr ip,.Loperatingsystem_parameter_argc
        ldr ip,[r3, ip]
        str r1,[ip]
        ldr ip,.Loperatingsystem_parameter_argv
        ldr ip,[r3, ip]
        str r0,[ip]

        /* calc envp */
        add r1,r1,#1
        add r1,r0,r1,LSL #2
        ldr ip,.Loperatingsystem_parameter_envp
        ldr ip,[r3, ip]
        str r1,[ip]
        
        /* Finally go to libc startup code. It will call "PASCALMAIN" via alias "main" */
        ldr ip,.L_start
        ldr ip,[r3, ip]
        bx ip

.L_GOT1:
        .long _GLOBAL_OFFSET_TABLE_-.LPIC1-8
.L__stkptr:
        .word __stkptr(GOT)
.L_start:
        .word _start(GOT)
.Loperatingsystem_parameter_argc:
        .word operatingsystem_parameter_argc(GOT)
.Loperatingsystem_parameter_argv:
        .word operatingsystem_parameter_argv(GOT)
.Loperatingsystem_parameter_envp:
        .word operatingsystem_parameter_envp(GOT)

/* --------------------------------------------------------- */
        .globl  _haltproc
        .type   _haltproc,#function
_haltproc:
        .globl  _haltproc_eabi
        .type   _haltproc_eabi,#function
_haltproc_eabi:
        /* Simply call libc exit(). _haltproc has the same declaration as exit. */
        blx exit

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
