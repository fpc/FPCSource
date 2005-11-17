/*
 * Startup code for programs linked with GNU libc, PowerPC64
 * version.
 *
 * Adapted from the glibc-sources (2.3.5) in the file
 * 
 *     sysdeps/powerpc/powerpc64/elf/start.S
 *
 * Original header follows.
 */

/* Startup code for programs linked with GNU libc.  PowerPC64 version.
   Copyright (C) 1998,1999,2000,2001,2002,2003 Free Software Foundation, Inc.
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

/* some macros which simplify the startup code */

/* load the 64 bit value "value" into register ra */
.macro LOAD_64BIT_VAL ra, value 
    lis       \ra,\value@highest
    ori       \ra,\ra,\value@higher
    sldi      \ra,\ra,32
    oris      \ra,\ra,\value@h
    ori       \ra,\ra,\value@l    
.endm

/* create function prolog for symbol "fn" */
.macro FUNCTION_PROLOG fn
    .section  ".text"
    .align    2
    .globl    \fn
    .section  ".opd", "aw"
    .align    3
\fn:
    .quad     .\fn, .TOC.@tocbase, 0
    .previous
    .size     \fn, 24
    .type     \fn, @function	
    .globl    .\fn
.\fn:
.endm

/* "ptrgl" glue code */
.section ".text"
.align 3
.globl .ptrgl
.ptrgl:
    ld	    0, 0(11)
    std     2, 40(1)
    mtctr   0
    ld      2, 8(11)
    ld      11, 8(11)
    bctr
.long 0
.byte 0, 12, 0, 0, 0, 0, 0, 0
.type .ptrgl, @function
.size .ptrgl, . - .ptrgl

/* 
 * start_addresses is a structure containing the real 
 * entry point (next to other things not interesting to
 * us here).
 *
 * All references in the struct are function descriptors
 *
 */
    .section ".rodata"
    .align  3
start_addresses:
    .quad   0 /* was _SDA_BASE_  but not in 64-bit ABI*/
    .quad   main_stub
    .quad   __libc_csu_init
    .quad   __libc_csu_fini
    .size   start_adresses, .-start_addresses

/*
 * the real entry point for the program
 */
FUNCTION_PROLOG _start
    mr  9,1                   /* save the stack pointer */

    /* Set up an initial stack frame, and clear the LR.  */

    clrrdi  1,1,4
    li      0,0
    stdu    1,-128(1)
    mtlr    0
    std     0,0(1)

    /* put the address of start_addresses in r8...  */
    /* PPC64 ABI uses R13 for thread local, so we leave it alone */
    LOAD_64BIT_VAL 8, start_addresses
    
    b   .__libc_start_main
    nop                      /* a NOP for the linker */ 

/*
 * This is our FreePascal main procedure which is called by
 * libc after initializing.
 */

FUNCTION_PROLOG main_stub
    mflr    0
    std     0,16(1)
    stdu    1,-128(1)

    LOAD_64BIT_VAL 8, operatingsystem_parameter_argc
    stw     3,0(8)

    LOAD_64BIT_VAL 8, operatingsystem_parameter_argv
    std     4,0(8)

    LOAD_64BIT_VAL 8, operatingsystem_parameter_envp
    std     5,0(8)

    LOAD_64BIT_VAL 8, ___fpc_ret
    std     1,0(8)

    bl  .PASCALMAIN
    nop

    b   ._haltproc

FUNCTION_PROLOG _haltproc
    LOAD_64BIT_VAL 8, ___fpc_ret
    ld      1, 0(8)
    addi    1, 1, 128
    ld      0, 16(1)
    mtlr    0
    blr

#    li      0,1          /* exit call */
#    sc
#    b  ._haltproc

    /* Define a symbol for the first piece of initialized data.  */
    .section ".data"
    .globl  __data_start
__data_start:
data_start:

___fpc_ret:                            /* return address to libc */
    .quad   0

.text
    .comm operatingsystem_parameter_argc, 4
    .comm operatingsystem_parameter_argv, 8
    .comm operatingsystem_parameter_envp, 8
