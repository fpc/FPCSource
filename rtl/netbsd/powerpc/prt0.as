	.file	"crt0.c"
gcc2_compiled.:
	.globl __progname
	.section	".data"	# .rodata
	.align 2
.LC0:
	.string	""
	.section	".sdata","aw"
	.align 2
	.type	 __progname,@object
	.size	 __progname,4
__progname:
.LCP0:
	.long (.LC0)@fixup
	.section	".fixup","aw"
	.align 2
	.long	.LCP0
	.previous
	.globl __ps_strings
	.align 2
	.type	 __ps_strings,@object
	.size	 __ps_strings,4
__ps_strings:
	.long 0
	.weak _DYNAMIC
	.weak _SDA_BASE_
	.weak _SDA2_BASE_
	.section	".got2","aw"
.LCTOC1 = .+32768
.LC1 = .-.LCTOC1
	.long __progname
.LC2 = .-.LCTOC1
	.long environ
.LC3 = .-.LCTOC1
	.long __ps_strings
.LC4 = .-.LCTOC1
	.long _DYNAMIC
.LC5 = .-.LCTOC1
	.section	".text"
	.align 2
	.globl _start
.LCL0:
	.long .LCTOC1-.LCF0
	.type	 _start,@function
_start:
	stwu 1,-48(1)
	mflr 0
	stw 24,16(1)
	stw 25,20(1)
	stw 26,24(1)
	stw 27,28(1)
	stw 28,32(1)
	stw 29,36(1)
	stw 30,40(1)
	stw 31,44(1)
	stw 0,52(1)
	bl .LCF0
.LCF0:
	mflr 30
	lwz 0,(.LCL0-.LCF0)(30)
	add 30,0,30
	mr 27,4
	mr 24,3
	mr 28,5
	mr 25,6
	mr 26,7
	mr 29,8
#	lis %r13,_SDA_BASE_@ha;addi %r13,%r13,_SDA_BASE_@l;lis %r2,_SDA2_BASE_@ha;addi %r2,%r2,_SDA2_BASE_@l
	lwz 31,0(27)
	cmpwi 0,31,0
	bc 12,2,.L7
	mr 3,31
	li 4,47
	bl _strrchr@local
	lwz 9,.LC1(30)
	cmpwi 0,3,0
	stw 3,0(9)
	bc 4,2,.L8
	stw 31,0(9)
	b .L12
.L8:
	addi 0,3,1
	stw 0,0(9)
.L12:
.L7:
	lwz 31,.LC2(30)
	cmpwi 0,29,0
	stw 28,0(31)
	bc 12,2,.L10
	lwz 9,.LC3(30)
	stw 29,0(9)
.L10:
	lwz 0,.LC4(30)
	cmpwi 0,0,0
	bc 12,2,.L11
	mr 3,26
	mr 4,25
.L11:
	lwz 3,.LC5(30)
	lwz 5,0(31)
	mr 3,24
	mr 4,27

        lis     11,operatingsystem_parameter_argc@ha
        stw     3,operatingsystem_parameter_argc@l(11);
        lis     11,operatingsystem_parameter_argv@ha
        stw     4,operatingsystem_parameter_argv@l(11); 
        
        lis     11,operatingsystem_parameter_envp@ha
        stw     5,operatingsystem_parameter_envp@l(11); 
	mtlr    0
	bl PASCALMAIN


        .globl  _haltproc
        .type   _haltproc,@function

_haltproc:
	li      0,1              /* exit call */
        lis     3,operatingsystem_result@h
        stw     3,operatingsystem_result@l(3)
        sc
        b       _haltproc



.Lfe1:
	.size	 _start,.Lfe1-_start
	.section .ident ; .asciz "$NetBSD: crt0.c,v 1.22 2002/05/09 20:32:59 matt Exp $" ; .text
	.align 2
	.type	 _strrchr,@function
_strrchr:
	rlwinm 4,4,0,0xff
	li 10,0
.L14:
	lbz 0,0(3)
	xor 9,0,4
	neg 9,9
	srawi 9,9,31
	andc 11,3,9
	cmpwi 0,0,0
	and 9,10,9
	or 10,9,11
	addi 3,3,1
	bc 4,2,.L14
	mr 3,10
	blr
.Lfe2:
	.size	 _strrchr,.Lfe2-_strrchr
	.section	".data"	# .rodata
	.align 2
	.comm	environ,4,4
	.comm	__mainprog_obj,4,4
	.ident	"GCC: (GNU) 2.95.3 20010315 (release) (NetBSD nb3)"

      .section ".data"
      .globl  __data_start
__data_start:
data_start:
        .globl  ___fpc_brk_addr         /* heap management */
        .type   ___fpc_brk_addr,@object
        .size   ___fpc_brk_addr,4
___fpc_brk_addr: 
        .long   0
