#
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 1999-2000 by Marco van de Voort, Michael Van Canneyt
#                                                  and Peter Vreman
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
# FreeBSD standard (shared) ELF startup code for Free Pascal
#
# FreeBSD RunTime dynamic loader only
# calls the functions specified by -init
# as functions without parameters
# Thus, it is not possible to retrieve argc, argv and envp
# for libraries.
# On x86_64 CPU, using weak for the operatingsystem_parameter_XXX
# allows to bind to main program parameters,
# but this does not seem to work for i386 loader.
 
	.file	"crt1.c"
#APP
	.ident	"$FreeBSD: src/lib/csu/common/crtbrand.c,v 1.4 2003/10/17 15:43:13 peter Exp $"
#NO_APP
	.section	.note.ABI-tag,"a",@progbits
	.p2align 2
	.type	abitag, @object
	.size	abitag, 24
abitag:
	.long	8
	.long	4
	.long	1
	.string	"FreeBSD"
	.long	1400097

	.text
#APP
	.ident	"$FreeBSD: src/lib/csu/amd64/crt1.c,v 1.13 2003/04/30 19:27:07 peter Exp $"
#NO_APP
	.p2align 2,,3
	.globl FPC_LIB_START
	.type FPC_LIB_START,@function
FPC_LIB_START:
	.globl FPC_SHARED_LIB_START
	.type FPC_SHARED_LIB_START,@function
FPC_SHARED_LIB_START:
	# jmp	_startlib@PLT
        movb    $1,operatingsystem_islibrary(%rip)
        #movq    operatingsystem_islibrary@GOTPCREL(%rip),%rax
        #movb    $1,(%rax)
	call	PASCALMAIN@PLT
	ret
	.p2align 2,,3
        .globl  _haltproc
        .type   _haltproc,@function
/* this routine is only called when the halt() routine of the RTL embedded in
  the shared library is called */
_haltproc:
        call    FPC_LIB_EXIT@PLT
	movl    $1,%eax                 /* exit syscall */
        movq    operatingsystem_result(%rip),%rbx
        movzwl  (%rbx),%edi
        syscall
        jmp     _haltproc@PLT
	/* Do not fail linkage if argc, argv and envp are not found. */
	.weak   operatingsystem_parameter_argc
	.weak   operatingsystem_parameter_argv
	.weak   operatingsystem_parameter_envp

.section .note.GNU-stack,"",@progbits

