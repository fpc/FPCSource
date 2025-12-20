/*
 * FreeBSD PowerPC64 (ELFv2) startup code for Free Pascal
 */

/* Load the *address* of a symbol via the TOC (ELFv2 PIC/PIE-safe).
   Sequence:
     addis  rA, r2, sym@toc@ha
     ld     rA, sym@toc@l(rA)
   After this, rA = &sym. */
.macro LOAD_TOC_ADDR ra, sym
    addis   \ra,2,\sym@toc@ha
    addi    \ra,\ra,\sym@toc@l
.endm

/* Regular function: assumes r2 already valid (normal call ABI). */
.macro FUNCTION_PROLOG fn
    .text
    .align  2
    .globl  \fn
    .type   \fn,@function
\fn:
    .localentry \fn, 0
.endm

/* Entry point: establish r2 (TOC) from r12, then mark localentry.
   At ELFv2 process entry, r12 contains the entry address. */
.macro ENTRY_PROLOG fn
    .text
    .align  2
    .globl  \fn
    .type   \fn,@function
\fn:
    bl      1f
1:  mflr    12
    addis   2,12,.TOC.-1b@ha
    addi    2,2,.TOC.-1b@l
    .localentry \fn, .-\fn
.endm

/* Entry point for dynamic executables */
ENTRY_PROLOG _dynamic_start
    /* r3 = argc, r4 = argv, r5 = envp */

    LOAD_TOC_ADDR 10, operatingsystem_parameter_argc
    stw     3,0(10)

    LOAD_TOC_ADDR 10, operatingsystem_parameter_argv
    std     4,0(10)

    LOAD_TOC_ADDR 10, operatingsystem_parameter_envp
    std     5,0(10)

    LOAD_TOC_ADDR 8,__stkptr
    std     1,0(8)

    mffs    0
    mtfsb1  31          # ensure NANQ bit (optional safety)
    mtfsf   0xff,0

    bl      PASCALMAIN
    nop

    trap  /* should never return */

/* Entry point for static executables */
ENTRY_PROLOG _start
    /* FreeBSD ELFv2: r3=argc, r4=argv, r5=envp already valid */

    LOAD_TOC_ADDR 10, operatingsystem_parameter_argc
    stw     3,0(10)

    LOAD_TOC_ADDR 10, operatingsystem_parameter_argv
    std     4,0(10)

    LOAD_TOC_ADDR 10, operatingsystem_parameter_envp
    std     5,0(10)

    LOAD_TOC_ADDR 8,__stkptr
    std     1,0(8)

    mffs    0
    mtfsb1  31          # ensure NANQ bit (optional safety)
    mtfsf   0xff,0

    bl      PASCALMAIN
    nop

    trap

FUNCTION_PROLOG _haltproc
    /* ELFv2 PIC-safe load of operatingsystem_result */
	addis	12,2,operatingsystem_result@got@ha
	ld		12,operatingsystem_result@got@l(12)
	
    lwz     3,0(3)
    li      0,1       /* syscall: exit */
    sc
    trap

/* BSS globals */
    .section .bss
    .align  3
    .globl  environ
    .type   environ,@object
    .size   environ,8
environ:
    .skip   8

    .section .data
    .align  3
    .globl  __progname
    .type   __progname,@object
    .size   __progname,8
__progname:
    .quad   0

    .globl __data_start
__data_start:
data_start:

    .bss
    .p2align 3
    .type __stkptr,@object
    .size __stkptr,8
    .globl __stkptr
__stkptr:
    .skip 8

    /* These MUST be real defined symbols: the RTL references them directly. */
    .globl operatingsystem_parameter_argc
    .type  operatingsystem_parameter_argc,@object
    .size  operatingsystem_parameter_argc,8
operatingsystem_parameter_argc:
    .skip 8

    .globl operatingsystem_parameter_argv
    .type  operatingsystem_parameter_argv,@object
    .size  operatingsystem_parameter_argv,8
operatingsystem_parameter_argv:
    .skip 8

    .globl operatingsystem_parameter_envp
    .type  operatingsystem_parameter_envp,@object
    .size  operatingsystem_parameter_envp,8
operatingsystem_parameter_envp:
    .skip 8
