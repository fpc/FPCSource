/*
    $Id$
*/
/* Copyright (C) 1995 DJ Delorie, see COPYING.DJ for details */
/*****************************************************************************\
 * Interface to 32-bit executable (from stub.asm)
 *
 *   cs:eip	according to COFF header
 *   ds		32-bit data segment for COFF program
 *   fs		selector for our data segment (fs:0 is stubinfo)
 *   ss:sp	our stack (ss to be freed)
 *   <others>	All unspecified registers have unspecified values in them.
\*****************************************************************************/
/* modified by Pierre Muller to become the prt0.s for FPK Pascal */

	.file "v2prt0.s"

/* #include "stubinfo.h" */
 STUBINFO = 0
 STUBINFO_MAGIC = 0
 STUBINFO_SIZE = 0x10
 STUBINFO_MINSTACK = 0x14
 STUBINFO_MEMORY_HANDLE = 0x18
 STUBINFO_INITIAL_SIZE = 0x1c
 STUBINFO_MINKEEP = 0x20
 STUBINFO_DS_SELECTOR = 0x22
 STUBINFO_DS_SEGMENT = 0x24
 STUBINFO_PSP_SELECTOR = 0x26
 STUBINFO_CS_SELECTOR = 0x28
 STUBINFO_ENV_SIZE = 0x2a
 STUBINFO_BASENAME = 0x2c
 STUBINFO_ARGV0 = 0x34
 STUBINFO_DPMI_SERVER = 0x44
 STUBINFO_END = 0x54


/*	.comm	__stklen, 4
        this is added to the compiler so that we can specify
        the stack size */
        .comm   __stkbottom,4
	.comm	__stubinfo, 4
	.comm	___djgpp_base_address, 4
	.comm	___djgpp_selector_limit, 4
	.comm	___djgpp_stack_limit, 4
	.lcomm	sel_buf, 8
/* ___djgpp_ds_alias defined in go32/exceptn.s */
/* inserted at the end of this file  */
/* we use a local copy that will be copied to exceptn.s */
   .globl ___v2prt0_ds_alias
___v2prt0_ds_alias:
   .long  0
/*        .comm   ___djgpp_ds_alias, 4  must be in locked code */
/* undef MULTIBLOCK */
/*  MULTIBLOCK = 0 does not work */
/* Win95 sometimes gives a block at an address lower than the base
address of _djgpp => big troubles
 That is why I removed the multiblocks
 Pierre Muller */
	.data

/* .ifdef MULTIBLOCK needed anyhow */
___djgpp_memory_handle_pointer:
	.long	___djgpp_memory_handle_list+8		/* Next free, first for stub */
	.comm	___djgpp_memory_handle_list, 2048	/* Enough for 256 handles */
/* .endif */

sbrk16_first_byte:
.include "sbrk16.ah"
sbrk16_last_byte:

sbrk16_api_ofs:
	.long	0
sbrk16_api_seg:
	.word	0
zero:
	.long	0

exit16_first_byte:
.include "exit16.ah"
exit16_last_byte:

/* hook_387_emulator:
	.long	___emu387_load_hook */

/* this pulls in the ident string, generated in .. */
/*	.long	___libc_ident_string */

/* this is for when main comes from a library */
	.long	_main

	.text

	.globl	start
start:

	pushl	%ds			/* set %es same as %ds */
	popl	%es			/* push/pop 4 bytes shorter than ax */

.if 0 /* we do this in the stub now */
	movl	$edata, %edi		/* set all BSS bytes to zero */
	movl	$end, %ecx
	subl	%edi, %ecx
	xorl	%eax, %eax		/* Zero fill value */
	shrl	$2, %ecx		/* div 4 Longwords not bytes */
	cld
	rep
	stosl
.endif

/* Enable NULL pointer protection if DPMI supports it */
	testb	$0x1, __crt0_startup_flags+1		/* include/crt0.h */
	jnz	1f
	movl	$start, %eax
	cmpl	$0x1000, %eax
	jl	1f
	movw	$0x507, %ax
	.byte 0x64 /* fs: */
	movl	STUBINFO_MEMORY_HANDLE, %esi
	xorl	%ebx, %ebx			/* Offset 0 in mem block */
	movl	$1, %ecx			/* Set one page */
	movl	$zero, %edx
	int	$0x31			/* Make null page uncommitted */
1:
/* Create an alias for DS to be used by real-mode callbacks (exception handler messes with DS itself) */

	movw	%ds, %bx
	movw	$0x000a, %ax
	int	$0x31
	jnc	ds_alias_ok
	movb	$0x4c, %ah
	int	$0x21
ds_alias_ok:
	movw	%ax, ___v2prt0_ds_alias
	movl	%eax, %ebx
	movw	$0x0009, %ax
	movw	%cs, %cx	/* get CPL from %cs */
	andl	$3, %ecx
	shll	$5, %ecx		/* move it into place */
	orw	$0xc093, %cx
	int	$0x31		/* set access rights for alias */

/* Maybe set our DS limit to 4Gb in size if flag set */
	testb	$0x80, __crt0_startup_flags		/* include/crt0.h */
	jz	2f
	movw	$0xffff, %cx
	movl	%ecx, %edx
	movw	$0x0008, %ax				/* reset alias limit to -1 */
	int	$0x31
	movw	%cs, %bx
	movw	$0x0008, %ax				/* reset DS limit to -1 */
	int	$0x31
	movw	%ds, %bx
	movw	$0x0008, %ax				/* reset DS limit to -1 */
	int	$0x31
	lsl	%ebx, %ebx				/* Should be -1 */
	incl	%ebx
	jz	2f
	andb	$0x7f, __crt0_startup_flags		/* clear it if failure */
2:
.ifdef MULTIBLOCK
	testb	$0x8, __crt0_startup_flags+1		/* include/crt0.h */
	jz	8f
.endif
/* Allocate some DOS memory and copy our sbrk helper into it. */
	movl	$sbrk16_first_byte, %esi
	movzwl	8(%esi), %ebx
	shrl	$4, %ebx
	movw	$0x0100, %ax
	int	$0x31
	jnc	dos_alloc_ok
	movb	$0x4c, %ah
	int	$0x21
dos_alloc_ok:
	movw	%cs, 2(%esi)
/* store API information */
	movw	%ds, 4(%esi)
	movw	%dx, 6(%esi)
/* selector for allocated block */

	movzwl	(%esi), %eax			/* calculate API address */
	movl	%eax, sbrk16_api_ofs

	pushl	%es				/* move the data */
	movw	%dx, %es
	movl	$(sbrk16_last_byte - sbrk16_first_byte), %ecx
	shrl	$2,%ecx
	xorl	%edi, %edi
	cld
	rep
	movsl
	popl	%es

	movl	%edx, %ebx			/* dos memory selector */
	movw	$0x000b, %ax			/* get descriptor */
	movl	$sel_buf, %edi
	int	$0x31

	andb	$0xbf, sel_buf+6		/* make 16-bit */
	andb	$0xf0, sel_buf+5		/* remove old type */
	orb	$0x0a, sel_buf+5		/* set new type to code/read */

	xorl	%eax, %eax			/* allocate new selector */
	movw	$0x0001, %cx
	int	$0x31
	movw	%ax, sbrk16_api_seg

	movl	%eax, %ebx
	movw	$0x000c, %ax			/* set descriptor */
	movl	$sel_buf, %edi
	int	$0x31
.ifdef MULTIBLOCK
8:	movl	$___djgpp_memory_handle_list+8, %edi
	movl	%edi, ___djgpp_memory_handle_pointer
	xorl	%eax, %eax
9:	cmpl	%eax, (%edi)
	je	10f
	mov	%eax, (%edi)
	addl	$4, %edi
	jmp	9b
10:	movw	%cs, %bx
	movw	$0x0006,%ax
	int	$0x31
	movl	%edx,___djgpp_base_address
	movw	%cx,___djgpp_base_address+2
.endif	/* MULTIBLOCK */

/* Initialize the brk/sbrk variables */

/*	movl	$end, __what_size_app_thinks_it_is */
	.byte 0x64 /* fs: */
	movl	STUBINFO_INITIAL_SIZE, %eax
	movl	%eax, __what_size_dpmi_thinks_we_are

/* Maybe lock the initial block, expects BX:CX */
	movl	%ecx,%ebx
	movl	%edx,%ecx
	addw	$4096,%cx			/* Skip null page */
	adcl	$0,%ebx
	subl	$4096,%eax
	pushl	%eax
	call	lock_memory

	.byte 0x64 /* fs: */
	movl	STUBINFO_MEMORY_HANDLE, %eax
	movl	%eax, ___djgpp_memory_handle_list

	.byte 0x64 /* fs: */			/* copy stubinfo into local memory */
	movl	STUBINFO_SIZE, %eax
	pushl	%eax
	call	___sbrk
	movl	%eax, __stubinfo
   	movl  %eax,U_SYSTEM_STUB_INFO
	movl	%eax, %edi
	.byte 0x64 /* fs: */
	movl	STUBINFO_SIZE, %ecx
	shrl	$2, %ecx
	xorl	%esi, %esi			/* Zero */
	pushl	%ds
	pushl	%fs
	popl	%ds
	cld
	rep
	movsl
	popl	%ds
	movl	__stklen, %eax		/* get program-requested stack size */
	.byte 0x64 /* fs: */
	movl	STUBINFO_MINSTACK, %ecx	/* get stub-requested stack size */
	cmpl	%ecx, %eax
	jge	use_stubinfo_stack_size	/* use the larger of the two */
	movl	%ecx, %eax
	movl	%eax, __stklen		/* store the actual stack length */
use_stubinfo_stack_size:
	pushl	%eax
	call	___sbrk			/* allocate the memory */
	cmpl	$-1, %eax
	je	no_memory
	movl	%eax, ___djgpp_stack_limit	/* Bottom of stack */
        addl    $256,%eax
        movl    %eax,__stkbottom               /* for stack checks */
        movl    %eax,U_SYSTEM_STACKBOTTOM

.ifdef LOCK_BOTTOM_STACK
/* test lock one page at bottom of stack to be sure that there is            */
/* not stack overflow, as the minimal size is 128 ko 4ko less is not much !! */
	testb	$0x1, __crt0_startup_flags+1		/* include/crt0.h */
	jnz	101f /* just to be sure it is not used */
	movl	%eax, %ebx			/* Offset __djgpp_stack_limit in mem block */
   addl $0xfff,%ebx
   andl $0xfffff000,%ebx    /* page align it */
	movw	$0x507, %ax
.ifdef MULTIBLOCK
	movl	___djgpp_memory_handle_pointer-8, %esi /* last memory block */
.else /* not MULTIBLOCK */
	movl	___djgpp_memory_handle_list, %esi /* last memory block */
.endif
	movl	$1, %ecx			/* Set one page */
	movl	$zero, %edx
	int	$0x31			/* Make first stack page page uncommitted */
101:
.endif /* LOCK_BOTTOM_STACK */
	movl	___djgpp_stack_limit,%eax	/* Bottom of stack */
	addl	__stklen, %eax
	movw	%ds, %dx		/* set stack */
	movw	%dx, %ss
	movl	%eax, %esp

	xorl	%ebp, %ebp
.if 0						/* done in crt1.c */
	.byte 0x64 /* fs: */			/* set up _go32_info_block structure */
	movzwl	STUBINFO_MINKEEP, %eax
	movl	%eax, U_SYSTEM_GO32_INFO_BLOCK+16	/* .size_of_transfer_buffer */
	.byte 0x64 /* fs: */
	movzwl	STUBINFO_DS_SEGMENT, %eax
	shll	$4, %eax
	movl	%eax, U_SYSTEM_GO32_INFO_BLOCK+12	/* .linear_address_of_transfer_buffer */
	xorl	%eax, %eax
	movl	$1, %ecx
	int	$0x31
	jc	no_selector
	movw	%ax, U_SYSTEM_GO32_INFO_BLOCK+26	/* .selector_for_linear_memory */
	movl	%eax, %ebx
	movl	$8, %eax
	movl	$0x0f, %ecx
	movw	$0xffff, %dx
	int	$0x31				/* Set limit 1Mb */
no_selector:
.endif

	call	___prt1_startup		/* run program */
	jmp	exit

no_memory:
	movb	$0xff, %al
	jmp	exit

/*-----------------------------------------------------------------------------*/

/* #define FREESEL(x) movw x, %bx; movw $0x0001, %ax; int $0x31 */
   .macro FREESEL x
     movw \x,%bx
     movw $0x0001,%ax
     int $0x31
   .endm

	.global	___exit
	.align	2
___exit:
	movb	4(%esp), %al
exit:
	movb	%al, %cl
	xorl	%eax,%eax
	movw	%ax,%fs
	movw	%ax,%gs
   cmpl $0,_exception_exit
   jz   no_exception
   pushl %ecx
   call *_exception_exit
   popl %ecx
   no_exception:
	cli				/* Just in case they didn't unhook ints */
	FREESEL U_SYSTEM_GO32_INFO_BLOCK+26	/* selector for linear memory */
	FREESEL ___v2prt0_ds_alias	/* DS alias for rmcb exceptions */
.ifdef MULTIBLOCK
	testb	$0x8, __crt0_startup_flags+1		/* include/crt0.h */
	jz	9f
.endif
	FREESEL sbrk16_api_seg		/* sbrk cs */
	movw	sbrk16_first_byte+6,%dx /* selector for allocated DOS mem */
	movw	$0x101, %ax
	int	$0x31			/* Free block and selector */
9:
	movl	__stubinfo, %edx
	movl	STUBINFO_CS_SELECTOR(%edx), %eax
	movw	%ax, sbrk16_api_seg
	xorl	%edi, %edi
	movl	%edi, sbrk16_api_ofs	/* Offset is zero */

	movw	STUBINFO_DS_SELECTOR(%edx), %es
	movb	%cl, %dl		/* Exit status */
	movl	$exit16_first_byte, %esi
	movl	$(exit16_last_byte - exit16_first_byte), %ecx
	cld
	rep
	movsb

	movw	%es,%ax			/* We will free stack! */
	movw	%ax,%ss
	movl	$0x400,%esp		/* Transfer buffer >= 1024 bytes */

.ifdef MULTIBLOCK
	movl	___djgpp_memory_handle_pointer, %ebx
	jmp	7f
6:	subl	$8, %ebx
	movl	(%ebx), %edi
	movw	2(%ebx), %si
	movw	$0x502, %ax
	int	$0x31
7:	cmpl	$___djgpp_memory_handle_list+8, %ebx
	jne	6b
.endif /* MULTIBLOCK */
	xorl	%ebp, %ebp				/* V1.10 bug fix */
	movl	___djgpp_memory_handle_list, %edi
	movl	___djgpp_memory_handle_list+2, %esi	/* Skip word prefixes */

	FREESEL %ds
	movw	%cs, %bx
/* Call exit procedure with BX=32-bit CS; SI+DI=32-bit handle; DL=exit status */
	.byte 0x2e
	ljmp	sbrk16_api_ofs

/*-----------------------------------------------------------------------------*/

/*	.lcomm	__what_size_app_thinks_it_is, 4 */
__what_size_app_thinks_it_is:
	.long	end
	.lcomm	__what_we_return_to_app_as_old_size, 4
	.lcomm	__what_size_dpmi_thinks_we_are, 4

lock_memory:
	/* BX:CX should be linear address; size is pushed on stack */
	testb	$0x10, __crt0_startup_flags+1		/* include/crt0.h */
	jz	13f
	pushl	%esi
	pushl	%edi
	pushl	%eax
	movl	16(%esp),%edi
	movw	18(%esp),%si
	movw	$0x600,%ax
	int	$0x31
	popl	%eax
	popl	%edi
	popl	%esi
13:	ret	$4			/* Pop the argument */


.if 0
brk_hook_ret:
	ret
	.globl ___sbrk_brk_hook
___sbrk_brk_hook:
	.long	brk_hook_ret
.endif

	.global	___sbrk
	.align	2
___sbrk:
	movl	__what_size_app_thinks_it_is, %eax
	movl	4(%esp), %ecx			/* Increment size */
	addl	%ecx, %eax
	jnc	brk_common
	/* Carry is only set if a negative increment or wrap happens.  Negative
	   increment is semi-OK, wrap (only for multiple zone sbrk) isn't. */
	test	$0x80000000, %ecx		/* Clears carry */
	jnz	brk_common
	stc					/* Put carry back */
	jmp	brk_common

	.globl	___brk
	.align	2
___brk:
	movl	4(%esp), %eax
	clc

brk_common:
	pushl	%esi
	pushl	%edi
	pushl	%ebx

	movl	__what_size_app_thinks_it_is, %edx		/* save info */
	movl	%edx, __what_we_return_to_app_as_old_size
	movl	%eax, __what_size_app_thinks_it_is

	jc	10f						/* Wrap for multi-zone */
	cmpl	__what_size_dpmi_thinks_we_are, %eax		/* don't bother shrinking */
	jbe	brk_nochange

.ifdef MULTIBLOCK
	testb	$0x8, __crt0_startup_flags+1		/* include/crt0.h */
	jz	10f
.endif
	addl	$0x0000ffff, %eax				/* round up to 64K block */
	andl	$0xffff0000, %eax
	push	%eax						/* size - save for later */

	movl	___djgpp_memory_handle_list, %edi		/* request new size */
	movw	___djgpp_memory_handle_list+2, %si
	movl	%eax, %ecx					/* size not limit */
	movl	%eax, %ebx					/* size not limit */
	shrl	$16, %ebx					/* BX:CX size */

	movw	$0x0900, %ax					/* disable interrupts */
	int	$0x31
	pushl	%eax

	lcall	sbrk16_api_ofs
	setc	%dl						/* Save carry */

	popl	%eax					/* restore interrupts */
	int	$0x31

	test	%dl,%dl
	popl	%edx
	jne	brk_error

	movl	%edi, ___djgpp_memory_handle_list		/* store new handle */
	movw	%si, ___djgpp_memory_handle_list+2
	movl	%ecx, ___djgpp_base_address			/* store new base address */
	movw	%bx, ___djgpp_base_address+2

	movl	%edx, %eax
	movl	__what_size_dpmi_thinks_we_are, %ecx
	subl	%ecx, %eax

	addl	___djgpp_base_address, %ecx
	movl	%ecx, %ebx
	shrl	$16, %ebx					/* BX:CX addr */
	pushl	%eax						/* Size */
	call	lock_memory

	decl	%edx						/* limit now, not size */
.ifdef MULTIBLOCK
	jmp	5f
/* Current allocation not large enough, get another block */
10:	movl	%ecx, %eax					/* Add amt */
	pushl	%eax						/* Save orig */
	addl	$0x0000ffff, %eax				/* round up to 64K block */
	andl	$0xffff0000, %eax
	movl	%eax, %edx					/* Save size */
	movl	%eax, %ecx
	movl	%eax, %ebx
	shrl	$16, %ebx					/* BX:CX size */
	movw	$0x501,%ax
	int	$0x31
	popl	%eax						/* Orig size */
	jc	brk_error

	pushl	%edx						/* Size */
	call	lock_memory

	pushw	%bx
	pushw	%cx
	popl	%ecx						/* Linear address */
        /* What if the new base address is lower than __djgpp_base_address !!!   */
	subl	___djgpp_base_address, %ecx			/* New dpmi size */
	cmpl	%ecx, __what_size_dpmi_thinks_we_are		/* Back to back ? */
	je	4f
	movl	%ecx, __what_size_dpmi_thinks_we_are
	movl	%ecx, __what_we_return_to_app_as_old_size
4:
	movl	__what_we_return_to_app_as_old_size, %ebx	/* Base for new block */
	addl	%ebx, %eax					/* Final address */
	movl	%eax, __what_size_app_thinks_it_is
/* Note - save adjusted memory base and memory handle SI:DI here */
	movl	___djgpp_memory_handle_pointer, %ebx
	movl	%edi, (%ebx)
	movw	%si, 2(%ebx)
	movl	%ecx, 4(%ebx)
	addl	$8, %ebx
	cmpl	$___djgpp_memory_handle_list+2040, %ebx		/* At end? */
	je	11f
	movl	%ebx, ___djgpp_memory_handle_pointer		/* Only if not at end */
11:
	addl	%ecx, %edx					/* Final address */
	decl	%edx						/* Limit to end */
/* If we get a block at a lower address we must skip the limit change */
	cmpl	___djgpp_selector_limit, %edx
	jbe	12f
.endif
5:	movl	%edx, ___djgpp_selector_limit
	orw	$0x0fff, %dx					/* low bits set */
	movw	$0x0008, %ax					/* reset CS limit */
	movw	%cs, %bx
	movl	%edx, %ecx
	shrl	$16, %ecx
	int	$0x31						/* CX:DX is limit */

	testb	$0x80, __crt0_startup_flags			/* include/crt0.h */
	jnz	3f
	movw	$0x0008, %ax					/* reset DS limit */
	movw	%ds, %bx
	int	$0x31

	movw	$0x0008, %ax					/* reset DS alias limit */
	movl	___v2prt0_ds_alias, %ebx
	int	$0x31
3:
	movw	$0x0007, %ax					/* reset DS alias base */
	movl	___v2prt0_ds_alias, %ebx
	movl	___djgpp_base_address, %edx
	movw	___djgpp_base_address+2, %cx
	int	$0x31

	movl	___djgpp_selector_limit, %edx
12:	incl	%edx						/* Size not limit */
	testb	$0x60, __crt0_startup_flags	/* include/crt0.h */
	jz	no_fill_sbrk_memory
	pushl	%ds
	popl	%es

	movl	__what_size_dpmi_thinks_we_are, %edi		/* set all newly resized bytes zero */
	movl	%edx, %ecx					/* Limit */
	subl	%edi, %ecx			/* Adjust count for base */
	xorl	%eax, %eax
	testb	$0x40, __crt0_startup_flags
	jz	no_deadbeef
	movl	$0xdeadbeef, %eax		/* something really easy to spot */
no_deadbeef:
	shrl	$2, %ecx			/* div 4 Longwords not bytes */
	cld
	rep
	stosl
no_fill_sbrk_memory:
	movl	%edx, __what_size_dpmi_thinks_we_are

.if 0						/* No purpose */
	pushl	___djgpp_memory_handle_list
	pushl	___djgpp_base_address
	movl	___sbrk_brk_hook, %eax
	call	%eax
	addl	$8, %esp
.endif

brk_nochange:					/* successful return */
	movl	__what_we_return_to_app_as_old_size, %eax
	jmp	brk_return

brk_error:					/* error return */
	movl	__what_we_return_to_app_as_old_size, %eax
	movl	%eax, __what_size_app_thinks_it_is
	movl	$-1, %eax

brk_return:
	popl	%ebx
	popl	%edi
	popl	%esi
	ret

	.globl	__crt0_init_mcount
__crt0_init_mcount:
.ifdef IN_GCRT0
	jmp	__mcount_init
.else
	ret
.endif

/* From here on this are parts of crt1.c converted to assembler
and without any call to libc, so that it works without anything else 
additions made by Pierre Muller*/
/* from dpmidefs.h * /
/* Copyright (C) 1995 DJ Delorie, see COPYING.DJ for details */
/* from include <libc/asmdefs.h> */
/* all macros removed here */
/* #define FUNC(x)		.globl x; x: */

/* #define ENTER		pushl %ebp; movl %esp,%ebp */

/* #define LEAVE(x)	movl %ebp,%esp; popl %ebp; ret $(x) */
/* #define ARG1		8(%ebp)
#define ARG1h		10(%ebp)
#define ARG2		12(%ebp)
#define ARG2h		14(%ebp)
#define ARG3		16(%ebp)
#define ARG4		20(%ebp)
#define ARG5		24(%ebp)
#define ARG6		28(%ebp)
#define ARG7		32(%ebp)
#define ARG8		36(%ebp) */

	.comm	___dpmi_error,2

/* from dpmi0000.s */
      	.globl ___dpmi_allocate_ldt_descriptors
___dpmi_allocate_ldt_descriptors:
   pushl %ebp; movl %esp,%ebp

	movl	8(%ebp), %ecx
	movl $0x0000, %eax
        int $0x31
        jnc .L_noerror0000
        movw %ax,___dpmi_error
        movl $-1,%eax
        jmp .L_leave0000
.L_noerror0000:
        movzwl	%ax,%eax
.L_leave0000:
	movl %ebp,%esp
	popl %ebp
	ret $4

/* from file dpmi0008.s */
	.globl ___dpmi_set_segment_limit
___dpmi_set_segment_limit:
   pushl %ebp; movl %esp,%ebp

	movl	8(%ebp), %ebx
	movzwl	12(%ebp), %edx
	movzwl	14(%ebp),%ecx

	movl     $0x0008,%eax
        int $0x31
        jnc .L_noerror0008
        movw %ax,___dpmi_error
        movl $-1,%eax
        jmp .L_leave0008
.L_noerror0008:
	xorl	%eax,%eax
.L_leave0008:
	movl %ebp,%esp
	popl %ebp
	ret $8

	.globl ___dpmi_get_version
___dpmi_get_version:
   pushl %ebp; movl %esp,%ebp

	movl     $0x0400,%eax
        int $0x31
        jnc .L_noerror0400
        movw %ax,___dpmi_error
        movl $-1,%eax
        jmp .L_leave0400
.L_noerror0400:
	movl	8(%ebp), %esi
	movb	%ah, (%esi)
	movb	%al, 1(%esi)
	movw	%bx, 2(%esi)
	movb	%cl, 4(%esi)
	movb	%dh, 5(%esi)
	movb	%dl, 6(%esi)

	xorl	%eax,%eax
.L_leave0400:

	movl %ebp,%esp
	popl %ebp
	ret $4

	.globl ___dpmi_get_segment_base_address
___dpmi_get_segment_base_address:
   pushl %ebp; movl %esp,%ebp

	movl	8(%ebp), %ebx
	movl     $0x0006,%eax
        int $0x31
        jnc .L_noerror0006
        movw %ax,___dpmi_error
        movl $-1,%eax
        jmp .L_leave0006
.L_noerror0006:

	movl	12(%ebp), %ebx
	movl	%edx, (%ebx)
	movw	%cx, 2(%ebx)

	xorl	%eax,%eax
.L_leave0006:
	movl %ebp,%esp
	popl %ebp
	ret $8

.globl ___bss_count
.data
	.align 2
___bss_count:
	.long 1
.text
	.align 2
	.globl _setup_core_selector
_setup_core_selector:
	pushl %ebp
	movl %esp,%ebp
	pushl $1
	call ___dpmi_allocate_ldt_descriptors
	/* addl $4,%esp */
	cmpl $-1,%eax
	jne .L24
	movw $0,U_SYSTEM_GO32_INFO_BLOCK+26
	leave
	ret
	.align 2,0x90
.L24:
	movw %ax,U_SYSTEM_GO32_INFO_BLOCK+26
	movw %ax,_core_selector
	pushl $0x10ffff
	andl $0xffff,%eax
	pushl %eax
	call ___dpmi_set_segment_limit
	leave
	ret
	.align 2
	.globl _setup_screens 
_setup_screens:
	pushl %ebp
	movl %esp,%ebp
	movw U_SYSTEM_GO32_INFO_BLOCK+26,%dx
	movl $1048563,%ecx
/APP
	movw %dx, %gs
	.byte 0x65
	movw (%ecx),%ax

/NO_APP
	cmpw $64896,%ax
	jne .L26
	movl $655360,U_SYSTEM_GO32_INFO_BLOCK+8
	movl $655360,U_SYSTEM_GO32_INFO_BLOCK+4
	leave
	ret
	.align 2,0x90
.L26:
	movl $1097,%ecx
/APP
	movw %dx,%gs
	.byte 0x65
	movb (%ecx),%al
/NO_APP
	cmpb $7,%al
	jne .L29
	movl $720896,U_SYSTEM_GO32_INFO_BLOCK+4
	movl $753664,U_SYSTEM_GO32_INFO_BLOCK+8
	leave
	ret
	.align 2,0x90
.L29:
	movl $753664,U_SYSTEM_GO32_INFO_BLOCK+4
	movl $720896,U_SYSTEM_GO32_INFO_BLOCK+8
	leave
	ret
	.align 2
	.globl _setup_go32_info_block
_setup_go32_info_block:
	pushl %ebp
	movl %esp,%ebp
	subl $8,%esp
	leal -8(%ebp),%eax
	pushl %eax
	call ___dpmi_get_version
	movl $40,U_SYSTEM_GO32_INFO_BLOCK
	movl __stubinfo,%edx
	movzwl 36(%edx),%eax
	sall $4,%eax
	movl %eax,U_SYSTEM_GO32_INFO_BLOCK+12
	movzwl 32(%edx),%ecx
	movl %ecx,U_SYSTEM_GO32_INFO_BLOCK+16
	movzwl 38(%edx),%ecx
	movl %ecx,U_SYSTEM_GO32_INFO_BLOCK+20
	movb -3(%ebp),%al
	movb %al,U_SYSTEM_GO32_INFO_BLOCK+24
	movb -2(%ebp),%al
	movb %al,U_SYSTEM_GO32_INFO_BLOCK+25
	movl $-1,U_SYSTEM_GO32_INFO_BLOCK+28
	pushl $U_SYSTEM_GO32_INFO_BLOCK+32
	movzwl 38(%edx),%eax
	pushl %eax
	call ___dpmi_get_segment_base_address
	movw $4,U_SYSTEM_GO32_INFO_BLOCK+36
	movb -8(%ebp),%dl
	salw $8,%dx
	movzbw -7(%ebp),%ax
	orw %ax,%dx
	movw %dx,U_SYSTEM_GO32_INFO_BLOCK+38
	leave
	ret
.globl ___PROXY
.data
___PROXY:
	.ascii " !proxy\0"
.globl ___PROXY_LEN
	.align 2
___PROXY_LEN:
	.long 7
.text
	.align 2
.globl ___prt1_startup
___prt1_startup:
	pushl %ebp
	movl %esp,%ebp
	pushl %ebx
	incl ___bss_count
	movl $0,___crt0_argv
	call _setup_core_selector
	call _setup_screens
	call _setup_go32_info_block
/*	call ___djgpp_exception_setup
	call _setup_environment */
	incl ___environ_changed
/*	pushl $0
	call __use_lfn
	addl $4,%esp
	call ___crt0_setup_arguments
	movl ___crt0_argv,%eax
	testl %eax,%eax
	je .L55
	movl (%eax),%ebx
	jmp .L56
	.align 2,0x90
.L55:
	movl U_SYSTEM_DOS_ARGV0,%ebx
.L56:
	pushl %ebx
	call ___crt0_load_environment_file
	pushl $0
	call __use_lfn
	pushl %ebx
	call __npxsetup
	call __crt0_init_mcount
	call ___main     */
	pushl U_SYSTEM_ENVIRON
	pushl ___crt0_argv
	pushl ___crt0_argc
	call _pascal_start
	pushl %eax
/*	call _exit changed to */
   call exit
	.align 2,0x90
/* .comm U_SYSTEM_DOS_ARGV0,4 */
.comm ___crt0_argc,4
.comm ___crt0_argv,4
        .globl ___environ_changed
___environ_changed:
        .long  0
        .globl _exception_exit
_exception_exit:
        .long  0
        .globl _swap_in
_swap_in:
        .long  0
        .globl _swap_out
_swap_out:
        .long  0
        .global _v2prt0_exceptions_on
_v2prt0_exceptions_on:
        .long  0
/*.comm __crt0_startup_flags,4
 .comm U_SYSTEM_ENVIRON,4 */

.ifdef test_go32v1
#///*
#//**	Called as start(argc, argv, envp)
#//*/
#///*	gs:edx points to prog_info structure.  All other registers are OBSOLETE
#//**	but included for backwards compatibility
#//*/

	.text
	.globl	old_start
old_start:
        popl    %ebx
        popl    %eax
	movl	%eax,__hard_master
	movl	%esi,___pid
	movl	%edi,___transfer_buffer
	movl	%ebx,_ScreenPrimary
	movl	%ebp,_ScreenSecondary

	cmpl	$0, %edx
	je	Lcopy_none
	movw	%gs,%cx
	movw	%ds,%ax
	cmpw	%cx,%ax
	je	Lcopy_none

	movl	%gs:(%edx), %ecx
	cmpl	U_SYSTEM_GO32_INFO_BLOCK, %ecx
	jbe	Lcopy_less
	movl	U_SYSTEM_GO32_INFO_BLOCK, %ecx
Lcopy_less:
	movl	$U_SYSTEM_GO32_INFO_BLOCK, %edi
	addl	$3, %ecx
	andl	$0xfffffffc, %ecx
	movl	%ecx, (%edi)
	addl	$4, %edi
	addl	$4, %edx
	subl	$4, %ecx
Lcopy_more:
	movl	%gs:(%edx), %eax
	movl	%eax, (%edi)
	addl	$4, %edx
	addl	$4, %edi
	subl	$4, %ecx
	jnz	Lcopy_more

	movl	U_SYSTEM_GO32_INFO_BLOCK+4, %eax
	movl	%eax, _ScreenPrimary
	movl	U_SYSTEM_GO32_INFO_BLOCK+8, %eax
	movl	%eax, _ScreenSecondary
        movl	U_SYSTEM_GO32_INFO_BLOCK+12, %eax
	movl	%eax, ___transfer_buffer
	movl	U_SYSTEM_GO32_INFO_BLOCK+20, %eax
	movl	%eax, ___pid
	movl	U_SYSTEM_GO32_INFO_BLOCK+24, %eax
	movl	%eax, __hard_master

	jmp	Lcopy_done

Lcopy_none:
	movl	%ebx,U_SYSTEM_GO32_INFO_BLOCK+4
	movl	%ebp,U_SYSTEM_GO32_INFO_BLOCK+8
	movl	%edi,U_SYSTEM_GO32_INFO_BLOCK+12
	movl	$4096,U_SYSTEM_GO32_INFO_BLOCK+16
	movl	%esi,U_SYSTEM_GO32_INFO_BLOCK+20
	movl	%eax,U_SYSTEM_GO32_INFO_BLOCK+24
	movl	$28, U_SYSTEM_GO32_INFO_BLOCK
Lcopy_done:

        movw    U_SYSTEM_GO32_INFO_BLOCK+36,%ax
        movw    %ax,_run_mode
	cmpw	$4,%ax
	jne	CanOnlyRunDPMI
	call	Correct_tbaddress
LtbaddressOK:
        movw    U_SYSTEM_GO32_INFO_BLOCK+26,%ax
        movw    %ax,_core_selector
        /*  core selector in %fs */
        movw    %ax,%fs
	xorl	%esi,%esi
	xorl	%edi,%edi
	xorl	%ebp,%ebp
	xorl	%ebx,%ebx

	movl	%esp,%ebx
        movl    $0x0,%ebp
	movl	%esp,%ebx
	movl	8(%ebx),%eax
	movl	%eax,U_SYSTEM_ENVIRON
	movl	4(%ebx),%eax
	movl	%eax,_args
	movl	(%ebx),%eax
	movl	%eax,_argc

	call	PASCALMAIN


exit_again:
	movl	$0x4c00,%eax
	int	$0x21
	jmp	exit_again

	ret

Correct_tbaddress:
	movl 	___transfer_buffer,%eax
	addl 	$1,%eax
	andl    $0xFFFFF,%eax
	movl	%eax,___transfer_buffer
	movl	%eax,U_SYSTEM_GO32_INFO_BLOCK+12
	ret
CanOnlyRunDPMI:
	movl	$0x4c01,%eax
	int	$0x21
        jmp     exit_again

        .ascii  "Can only run in DPMI "

/*	.data
        .globl _argc
_argc:
	.long   0
	.globl  _args
_args:
	.long	0
	.globl	_run_mode
_run_mode:
	.word	0
	.globl	_core_selector
_core_selector:
	.word	0
	.globl	_environ
_environ:
	.long	0 */

	.globl	___pid
___pid:
	.long	42

	.globl	___transfer_buffer
___transfer_buffer:
	.long	0

	.globl	_ScreenSecondary
_ScreenSecondary:
	.long	0

	.globl	__hard_master
	.globl	__hard_slave
	.globl	__core_select
__hard_master:
	.byte	0
__hard_slave:
	.byte	0

.endif /* test_go32v1 */

/* this was the prt0.s from the go32v1 version */
//
// call as start(argc, argv, envp) (C-calling convention)
//
	.globl	_pascal_start
_pascal_start:
        /* %ebx doesn't contain ScreenPrimary */
	movl    U_SYSTEM_GO32_INFO_BLOCK+4,%ebx
	movl	%ebx,_ScreenPrimary
        /*  core selector in %fs */
        movw    _core_selector,%ax
        movw    %ax,%fs

// Top of frame
        movl    $0x0,%ebp
	movl	%esp,%ebx
	movl	12(%ebx),%eax
	movl	%eax,U_SYSTEM_ENVIRON
	movl	8(%ebx),%eax
	movl	%eax,_args
	movl	4(%ebx),%eax
	movl	%eax,_argc

	call	PASCALMAIN
        movl    $0,%eax
        /* no error if passing here */
/*	movl	$0x4c00,%eax
	int	$0x21 */

	ret

	.data

/*	.comm	U_SYSTEM_ENVIRON,4 */
	.globl	_ScreenPrimary
_ScreenPrimary:
	.long	0
	.globl  _argc
_argc:
	.long   0
	.globl  _args
_args:
	.long	0
	.globl	_run_mode
_run_mode:
	.word	4
	.globl	_core_selector
_core_selector:
	.word	0

/* Here Pierre Muller added all what was in crt1.c  */
/* in assembler                                     */
/* Copyright (C) 1996 DJ Delorie, see COPYING.DJ for details */
/* Copyright (C) 1995 DJ Delorie, see COPYING.DJ for details */
/* adapted to assembler for FPK by Pierre Muller             */

/* Global variables */


/* This gets incremented each time the program is started.
   Programs (such as Emacs) which dump their code to create
   a new executable, cause this to be larger than 2.  Library
   functions that cache info in static variables should check
   the value of `__bss_count' if they need to reinitialize
   the static storage.  */
        .data
        .globl  ___bss_count
___bs_count:
        .long   1

	.globl 	__crt0_startup_flags
__crt0_startup_flags:
	.long	0

	.globl	__dos_ds
__dos_ds:
	.long	0

/*
  $Log$
  Revision 1.1.1.1  1998-03-25 11:18:42  root
  * Restored version

  Revision 1.7  1998/03/19 10:04:30  pierre
    + changed as files so that they can be compiled by GNU as directly
      changed the makefile accordingly

  Revision 1.6  1998/02/03 15:52:46  pierre
    * swapvectors really disable exception handling
      and interrupt redirection with go32v2
    * in dos.pp bug if arg path from fsearch had a directory part fixed

  Revision 1.5  1998/02/01 14:04:27  peter
    * Fixed exit status which was wrong when exception_exit was used

  Revision 1.4  1998/01/19 17:03:51  pierre
    * %fs was set in a commented part, corrected

  Revision 1.3  1998/01/16 16:46:53  pierre
    + %fs contains the core selector at startup

*/

