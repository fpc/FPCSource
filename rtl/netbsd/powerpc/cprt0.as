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
	.long _fini
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
	lis %r13,_SDA_BASE_@ha;addi %r13,%r13,_SDA_BASE_@l;lis %r2,_SDA2_BASE_@ha;addi %r2,%r2,_SDA2_BASE_@l
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
	bl _rtld_setup@plt
.L11:
	lwz 3,.LC5(30)
	bl atexit@plt
	bl _init@plt
	lwz 5,0(31)
	mr 3,24
	mr 4,27

        lis     11,U_SYSTEM_ARGC@ha
        stw     3,U_SYSTEM_ARGC@l(11);
        lis     11,U_SYSTEM_ARGV@ha
        stw     4,U_SYSTEM_ARGV@l(11); 
        
        lis     11,U_SYSTEM_ENVP@ha
        stw     5,U_SYSTEM_ENVP@l(11); 
	mtlr    0
	bl main@plt

_haltproc:
        lis     3,U_SYSTEM_EXITCODE@h
        stw     3,U_SYSTEM_EXITCODE@l(3)
	bl exit@plt
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
.LC6:
	.string	"Corrupt Obj_Entry pointer in GOT\n"
	.align 2
.LC8:
	.string	"Dynamic linker version mismatch\n"
	.section	".got2","aw"
.LC7 = .-.LCTOC1
	.long .LC6
.LC9 = .-.LCTOC1
	.long .LC8
	.section	".text"
	.align 2
	.globl _rtld_setup
.LCL1:
	.long .LCTOC1-.LCF1
	.type	 _rtld_setup,@function
_rtld_setup:
	stwu 1,-32(1)
	mflr 0
	stw 29,20(1)
	stw 30,24(1)
	stw 31,28(1)
	stw 0,36(1)
	bl .LCF1
.LCF1:
	mflr 30
	lwz 0,(.LCL1-.LCF1)(30)
	add 30,0,30
	mr. 31,4
	mr 29,3
	bc 12,2,.L22
	lwz 9,0(31)
	lis 0,0xd550
	ori 0,0,47226
	cmpw 0,9,0
	bc 12,2,.L21
.L22:
	lwz 6,.LC7(30)
	li 3,0
	li 4,4
	li 5,2
	li 7,33
	crxor 6,6,6
	bl __syscall@plt
	li 3,0
	li 4,1
	li 5,1
	crxor 6,6,6
	bl __syscall@plt
.L21:
	lwz 0,4(31)
	cmpwi 0,0,1
	bc 12,2,.L27
	lwz 6,.LC9(30)
	li 3,0
	li 4,4
	li 5,2
	li 7,32
	crxor 6,6,6
	bl __syscall@plt
	li 3,0
	li 4,1
	li 5,1
	crxor 6,6,6
	bl __syscall@plt
.L27:
	mr 3,29
	bl atexit@plt
	lwz 0,36(1)
	mtlr 0
	lwz 29,20(1)
	lwz 30,24(1)
	lwz 31,28(1)
	la 1,32(1)
	blr
.Lfe3:
	.size	 _rtld_setup,.Lfe3-_rtld_setup
	.weak dlopen ; dlopen = _dlopen
	.weak dlclose ; dlclose = _dlclose
	.weak dlsym ; dlsym = _dlsym
	.weak dlerror ; dlerror = _dlerror
	.weak dladdr ; dladdr = _dladdr
	.section	".got2","aw"
.LC10 = .-.LCTOC1
	.long __mainprog_obj
	.section	".text"
	.align 2
	.globl _dlopen
.LCL2:
	.long .LCTOC1-.LCF2
	.type	 _dlopen,@function
_dlopen:
	stwu 1,-16(1)
	mflr 0
	stw 30,8(1)
	stw 31,12(1)
	stw 0,20(1)
	bl .LCF2
.LCF2:
	mflr 30
	lwz 0,(.LCL2-.LCF2)(30)
	add 30,0,30
	lwz 9,.LC10(30)
	lwz 9,0(9)
	cmpwi 0,9,0
	bc 12,2,.L33
	lwz 0,140(9)
	mtlr 0
	blrl
	b .L34
.L33:
	li 3,0
.L34:
	lwz 0,20(1)
	mtlr 0
	lwz 30,8(1)
	lwz 31,12(1)
	la 1,16(1)
	blr
.Lfe4:
	.size	 _dlopen,.Lfe4-_dlopen
	.section	".got2","aw"
.LC11 = .-.LCTOC1
	.long __mainprog_obj
	.section	".text"
	.align 2
	.globl _dlclose
.LCL3:
	.long .LCTOC1-.LCF3
	.type	 _dlclose,@function
_dlclose:
	stwu 1,-16(1)
	mflr 0
	stw 30,8(1)
	stw 31,12(1)
	stw 0,20(1)
	bl .LCF3
.LCF3:
	mflr 30
	lwz 0,(.LCL3-.LCF3)(30)
	add 30,0,30
	lwz 9,.LC11(30)
	lwz 9,0(9)
	cmpwi 0,9,0
	bc 12,2,.L36
	lwz 0,152(9)
	mtlr 0
	blrl
	b .L37
.L36:
	li 3,-1
.L37:
	lwz 0,20(1)
	mtlr 0
	lwz 30,8(1)
	lwz 31,12(1)
	la 1,16(1)
	blr
.Lfe5:
	.size	 _dlclose,.Lfe5-_dlclose
	.section	".got2","aw"
.LC12 = .-.LCTOC1
	.long __mainprog_obj
	.section	".text"
	.align 2
	.globl _dlsym
.LCL4:
	.long .LCTOC1-.LCF4
	.type	 _dlsym,@function
_dlsym:
	stwu 1,-16(1)
	mflr 0
	stw 30,8(1)
	stw 31,12(1)
	stw 0,20(1)
	bl .LCF4
.LCF4:
	mflr 30
	lwz 0,(.LCL4-.LCF4)(30)
	add 30,0,30
	lwz 9,.LC12(30)
	lwz 9,0(9)
	cmpwi 0,9,0
	bc 12,2,.L39
	lwz 0,144(9)
	mtlr 0
	blrl
	b .L40
.L39:
	li 3,0
.L40:
	lwz 0,20(1)
	mtlr 0
	lwz 30,8(1)
	lwz 31,12(1)
	la 1,16(1)
	blr
.Lfe6:
	.size	 _dlsym,.Lfe6-_dlsym
	.section	".data"	# .rodata
	.align 2
.LC14:
	.string	"Dynamic linker interface not available"
	.section	".got2","aw"
.LC13 = .-.LCTOC1
	.long __mainprog_obj
.LC15 = .-.LCTOC1
	.long .LC14
	.section	".text"
	.align 2
	.globl _dlerror
.LCL5:
	.long .LCTOC1-.LCF5
	.type	 _dlerror,@function
_dlerror:
	stwu 1,-16(1)
	mflr 0
	stw 30,8(1)
	stw 31,12(1)
	stw 0,20(1)
	bl .LCF5
.LCF5:
	mflr 30
	lwz 0,(.LCL5-.LCF5)(30)
	add 30,0,30
	lwz 9,.LC13(30)
	lwz 9,0(9)
	cmpwi 0,9,0
	bc 12,2,.L42
	lwz 0,148(9)
	mtlr 0
	blrl
	b .L43
.L42:
	lwz 3,.LC15(30)
.L43:
	lwz 0,20(1)
	mtlr 0
	lwz 30,8(1)
	lwz 31,12(1)
	la 1,16(1)
	blr
.Lfe7:
	.size	 _dlerror,.Lfe7-_dlerror
	.section	".got2","aw"
.LC16 = .-.LCTOC1
	.long __mainprog_obj
	.section	".text"
	.align 2
	.globl _dladdr
.LCL6:
	.long .LCTOC1-.LCF6
	.type	 _dladdr,@function
_dladdr:
	stwu 1,-16(1)
	mflr 0
	stw 30,8(1)
	stw 31,12(1)
	stw 0,20(1)
	bl .LCF6
.LCF6:
	mflr 30
	lwz 0,(.LCL6-.LCF6)(30)
	add 30,0,30
	lwz 9,.LC16(30)
	lwz 9,0(9)
	cmpwi 0,9,0
	bc 12,2,.L45
	lwz 0,156(9)
	mtlr 0
	blrl
	b .L46
.L45:
	li 3,-1
.L46:
	lwz 0,20(1)
	mtlr 0
	lwz 30,8(1)
	lwz 31,12(1)
	la 1,16(1)
	blr

.Lfe8:
	.size	 _dladdr,.Lfe8-_dladdr
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
