/*
 * Free Pascal RTL — Shared library startup (FreeBSD, PowerPC64 ELFv2)
 */

        .machine        power8
        .abiversion     2

/* --- helpers ------------------------------------------------------------ */

.macro LOAD_64BIT_VAL ra, value
    lis       \ra,\value@highest
    ori       \ra,\ra,\value@higher
    sldi      \ra,\ra,32
    oris      \ra,\ra,\value@h
    ori       \ra,\ra,\value@l
.endm

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

/* --- code --------------------------------------------------------------- */

        .section ".text"

/*
 * Entry called by the loader for the shared object.
 * ELFv2 process/linker conventions: r3=argc, r4=argv, r5=envp.
 */
FUNCTION_PROLOG FPC_SHARED_LIB_START
    /* standard small frame */
    mflr    0
    stdu    1,-144(1)
    std     0,16(1)

    /* store argc/argv/envp to RTL slots */
    addis   10,2,operatingsystem_parameter_argc@toc@ha
    addi    10,10,operatingsystem_parameter_argc@toc@l
    stw     3,0(10)

    addis   10,2,operatingsystem_parameter_argv@toc@ha
    addi    10,10,operatingsystem_parameter_argv@toc@l
    std     4,0(10)

    addis   10,2,operatingsystem_parameter_envp@toc@ha
    addi    10,10,operatingsystem_parameter_envp@toc@l
    std     5,0(10)

    /* stash initial SP */
    addis   8,2,__stkptr@toc@ha
    addi    8,8,__stkptr@toc@l
    std     1,0(8)

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
FUNCTION_PROLOG _haltproc
    /* r3 = operatingsystem_result */
    addis   3,2,operatingsystem_result@toc@ha
    addi    3,3,operatingsystem_result@toc@l
    lwz     3,0(3)
    bl      exit
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

        .type   __stkptr, @object
        .size   __stkptr, 8
        .globl  __stkptr
__stkptr:
        .skip   8

        .type   operatingsystem_parameters, @object
        .size   operatingsystem_parameters, 24
operatingsystem_parameters:
        .skip   3*8

        .globl  operatingsystem_parameter_argc
        .globl  operatingsystem_parameter_argv
        .globl  operatingsystem_parameter_envp
        .set    operatingsystem_parameter_argc, operatingsystem_parameters+0
        .set    operatingsystem_parameter_argv, operatingsystem_parameters+8
        .set    operatingsystem_parameter_envp, operatingsystem_parameters+16

