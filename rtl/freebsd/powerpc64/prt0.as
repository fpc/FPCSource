/*
 * FreeBSD PowerPC64 (ELFv2) startup code for Free Pascal
 */

.macro LOAD_64BIT_VAL ra, value
    lis       \ra,\value@highest
    ori       \ra,\ra,\value@higher
    sldi      \ra,\ra,32
    oris      \ra,\ra,\value@h
    ori       \ra,\ra,\value@l
.endm

.macro FUNCTION_PROLOG fn
    .text
    .align  2
    .globl  \fn
    .type   \fn,@function
\fn:
.endm

/* Entry point for dynamic executables */
FUNCTION_PROLOG _dynamic_start
    /* r3 = argc, r4 = argv, r5 = envp */

    LOAD_64BIT_VAL 10, operatingsystem_parameter_argc
    stw     3,0(10)

    LOAD_64BIT_VAL 10, operatingsystem_parameter_argv
    std     4,0(10)

    LOAD_64BIT_VAL 10, operatingsystem_parameter_envp
    std     5,0(10)

    LOAD_64BIT_VAL 8,__stkptr
    std     1,0(8)

    mffs    0
    mtfsb1  31          # ensure NANQ bit (optional safety)
    mtfsf   0xff,0

    bl      PASCALMAIN
    nop

    trap  /* should never return */

/* Entry point for static executables */
FUNCTION_PROLOG _start
    /* FreeBSD ELFv2: r3=argc, r4=argv, r5=envp already valid */

    LOAD_64BIT_VAL 10, operatingsystem_parameter_argc
    stw     3,0(10)

    LOAD_64BIT_VAL 10, operatingsystem_parameter_argv
    std     4,0(10)

    LOAD_64BIT_VAL 10, operatingsystem_parameter_envp
    std     5,0(10)

    LOAD_64BIT_VAL 8,__stkptr
    std     1,0(8)

    mffs    0
    mtfsb1  31          # ensure NANQ bit (optional safety)
    mtfsf   0xff,0

    bl      PASCALMAIN
    nop

    trap

FUNCTION_PROLOG _haltproc
    /* just call FreeBSD exit() */
    LOAD_64BIT_VAL 3, operatingsystem_result
    lwz     3,0(3)    /* r3 = exit code */
    li      0,1       /* syscall: exit */
    sc
    trap

/* BSS globals */
    .data
    .globl __data_start
__data_start:
data_start:

    .bss
    .type __stkptr,@object
    .size __stkptr,8
    .globl __stkptr
__stkptr:
    .skip 8

    .type operatingsystem_parameters,@object
    .size operatingsystem_parameters,24
operatingsystem_parameters:
    .skip 3*8
    .globl operatingsystem_parameter_argc
    .globl operatingsystem_parameter_argv
    .globl operatingsystem_parameter_envp
    .set operatingsystem_parameter_argc,operatingsystem_parameters+0
    .set operatingsystem_parameter_argv,operatingsystem_parameters+8
    .set operatingsystem_parameter_envp,operatingsystem_parameters+16
