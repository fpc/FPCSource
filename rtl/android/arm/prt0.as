/* Startup code for ARM & ELF
   Copyright (C) 1995, 1996, 1997, 1998, 2001, 2002 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */

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

/* In our entry point we should save pointers to cmd line arguments
   and environment vars, then pass control to libc entry point.
   After needed libc init, it will call FPC main code.
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
        
        b PASCALMAIN
        
        /* Call __libc_init */
        /* ELF data block */
      	mov r0, sp
      	/* "onexit" function, not used on any platform supported by Bionic */
        mov r1, #0
        /* the "main" function of the program */
        ldr r2, =PASCALMAIN
        /* constructors and destructors list */
        adr r3, 1f
        b __libc_init

1:      .long __PREINIT_ARRAY__
        .long __INIT_ARRAY__
        .long __FINI_ARRAY__

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
        b exit

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
