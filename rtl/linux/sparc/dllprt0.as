/*
    This file is part of the Free Pascal run time library.
    Copyright (c) 2013 by Free Pascal development team

    Startup code for elf32-sparc

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*/

.globl  FPC_SHARED_LIB_START
.type   FPC_SHARED_LIB_START,#function
FPC_SHARED_LIB_START:
        save    %o6,-96,%o6
        call   1f
        sethi  %hi(_GLOBAL_OFFSET_TABLE_+4),%l7
1:      or     %l7,%lo(_GLOBAL_OFFSET_TABLE_+8),%l7
        add    %l7,%o7,%l7

        sethi  %hi(operatingsystem_parameter_argc),%o0
        or     %o0,%lo(operatingsystem_parameter_argc),%o0
        ld     [%o0+%l7],%o1
        st     %i0,[%o1]

        sethi  %hi(operatingsystem_parameter_argv),%o0
        or     %o0,%lo(operatingsystem_parameter_argv),%o0
        ld     [%o0+%l7],%o1
        st     %i1,[%o1]

        sethi  %hi(operatingsystem_parameter_envp),%o0
        or     %o0,%lo(operatingsystem_parameter_envp),%o0
        ld     [%o0+%l7],%o1
        st     %i2,[%o1]

        sethi  %hi(__stkptr),%o0
        or     %o0,%lo(__stkptr),%o0
        ld     [%o0+%l7],%o1
        st     %sp,[%o1]

        call   PASCALMAIN
        nop
        ret
        restore

.size FPC_SHARED_LIB_START,.-FPC_SHARED_LIB_START


.globl _haltproc
.type  _haltproc,#function
_haltproc:
        mov    188,%g1
        ta     16
        unimp

.size _haltproc,.-_haltproc

        .comm __stkptr,4
        .comm __dl_fini,4

        .comm operatingsystem_parameter_envp,4
        .comm operatingsystem_parameter_argc,4
        .comm operatingsystem_parameter_argv,4

.section .note.GNU-stack,"",@progbits
