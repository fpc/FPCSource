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
# NetBSD standard (shared) ELF/m68k startup code for Free Pascal

.section .text
    .globl _start
_start:
|        move.l  %a2,-(%sp)              | ps_strings
|        move.l  %a0,-(%sp)              | obj
|        move.l  %a1,-(%sp)              | cleanup

        move.l  0(%a2),operatingsystem_parameter_argv
        move.l  4(%a2),operatingsystem_parameter_argc
        move.l  8(%a2),operatingsystem_parameter_envp
        move.l  8(%a2),environ

        jsr     PASCALMAIN
        jmp     _haltproc

    .globl _haltproc
_haltproc:
        move.l  operatingsystem_result,-(%sp)
        move.l  #0,-(%sp)
        moveq.l #1,%d0
        trap    #0
        rts


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
