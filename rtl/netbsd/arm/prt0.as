#
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 2017 by Karoly Balogh
#   member of the Free Pascal development team.
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
# NetBSD standard (shared) ELF/arm startup code for Free Pascal

# r12/ip = ps_strings on entry

.section .text
    .globl _start
_start:
        mov     ip, r0
        mov     r0, r2
        mov     r2, ip

        ldr     r3, [r2, #0]
        ldr     r4, =operatingsystem_parameter_argv
        str     r3, [r4]
        ldr     r3, [r2, #4]
        ldr     r4, =operatingsystem_parameter_argc
        str     r3, [r4]
        ldr     r3, [r2, #8]
        ldr     r4, =operatingsystem_parameter_envp
        str     r3, [r4]
        ldr     r4, =environ
        str     r3, [r4]

# cache align, just in case
        bic     sp, sp, #7

        bl      PASCALMAIN
        bl      _haltproc

    .globl _haltproc
_haltproc:
        ldr     r0,=operatingsystem_result
        ldr     r0,[r0]
        svc     0xa00001


.section .data
        .comm environ,4,4
        .comm operatingsystem_parameter_envp,4,4
        .comm operatingsystem_parameter_argc,4,4
        .comm operatingsystem_parameter_argv,4,4


.section ".note.netbsd.ident", "a"
        .long   2f-1f
        .long   4f-3f
        .long   1
1:      .asciz  "NetBSD"
2:      .p2align 2
3:      .long   400000000
4:      .p2align 2
