/*
 * Free Pascal RTL — Shared library startup (FreeBSD, PowerPC64 ELFv2)
 */

        .machine        power8
        .abiversion     2

/* ELFv2: no function descriptors; establish TOC from r12 at entry */
.macro FUNCTION_PROLOG fn
    .text
    .align    4
    .globl    \fn
    .type     \fn, @function
\fn:
    addis     2,12,.TOC.-\fn@ha
    addi      2,2,.TOC.-\fn@l
    .localentry \fn, .-\fn
.endm

/* Load the *address* of a data symbol via TOC (large-TOC safe).
 * Canonical sequence:
 *   addis  ra,2,sym@toc@ha
 *   ld     ra,sym@toc@l(ra)
 */
.macro LOAD_TOC_ADDR ra, sym
    addis   \ra,2,\sym@toc@ha
    ld      \ra,\sym@toc@l(\ra)
.endm

/* --- code --------------------------------------------------------------- */

        .section ".text"

/*
 * Entry called by the loader for the shared object.
 * ELFv2 process/linker conventions: r3=argc, r4=argv, r5=envp.
 */
FUNCTION_PROLOG FPC_SHARED_LIB_START
    /* standard small frame */
    mflr    0
    mr      9,1
    stdu    1,-144(1)
    std     0,16(1)

    /* store argc/argv/envp to RTL slots */
    LOAD_TOC_ADDR 10, operatingsystem_parameter_argc
    stw     3,0(10)

    LOAD_TOC_ADDR 10, operatingsystem_parameter_argv
    std     4,0(10)

    LOAD_TOC_ADDR 10, operatingsystem_parameter_envp
    std     5,0(10)

    /* stash initial SP */
    LOAD_TOC_ADDR 8, __stkptr
    std     9,0(8)

    /* call library initialization */
    bl      PASCALMAIN
    nop

    /* epilogue / return to caller */
    ld      0,16(1)
    addi    1,1,144
    mtlr    0
    blr
.size FPC_SHARED_LIB_START, .-FPC_SHARED_LIB_START

/*
 * Called when the RTL in the shared library performs halt().
 * FreeBSD: no exit_group; just exit(status).
 */
        .text
        .align  4
        .globl  _haltproc
        .type   _haltproc, @function
_haltproc:
        .localentry _haltproc, 0
    /* r12 = &operatingsystem_result via GOT (large-range safe) */
	addis	12,2,operatingsystem_result@got@ha
	ld		12,operatingsystem_result@got@l(12)
	
    lwz     3,0(3)
    li      0,1       /* syscall: exit */
    sc
    nop
    /* should not return */
    trap
.size _haltproc, .-_haltproc

/* --- data/bss ----------------------------------------------------------- */

        /* first piece of initialized data (optional marker) */
        .section ".data"
        .globl  __data_start
__data_start:
data_start:

        .section ".bss"
        .p2align 3

        .type   __stkptr, @object
        .size   __stkptr, 8
        .globl  __stkptr
__stkptr:
        .skip   8

        .type   operatingsystem_parameters, @object
        .size   operatingsystem_parameters, 24
operatingsystem_parameters:
		.globl operatingsystem_parameter_argc
operatingsystem_parameter_argc:
		.skip 8
		.globl operatingsystem_parameter_argv
operatingsystem_parameter_argv:
		.skip 8
		.globl operatingsystem_parameter_envp
operatingsystem_parameter_envp:
		.skip 8
