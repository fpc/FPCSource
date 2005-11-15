/*
 * Startup code for normal programs, PowerPC64 version.
 *
 */
   
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
 * Main Pascal entry point label (function)
 */
FUNCTION_PROLOG _start

    mr   26, 1            /* save stack pointer */
    /* Set up an initial stack frame, and clear the LR */
    clrrdi  1, 1, 5       /* align r1 */
    li      0, 0          
    stdu    1,-128(1)      
    mtlr    0             
    std     0, 0(1)       /* r1 = pointer to NULL value */

    /* store argument count (= 0(r1) )*/
    ld      3, 0(26)
    LOAD_64BIT_VAL 10, operatingsystem_parameter_argc
    stw     3, 0(10)
    /* calculate argument vector address and store (= 8(r1) + 8 ) */
    addi    4, 26, 8
    LOAD_64BIT_VAL 10, operatingsystem_parameter_argv
    std     4, 0(10)
    /* store environment pointer (= argv + (argc+1)* 8 ) */
    addi    5, 3, 1
    sldi    5, 5, 3
    add     5, 4, 5
    LOAD_64BIT_VAL 10, operatingsystem_parameter_envp
    std     5, 0(10)

    bl      .PASCALMAIN
    nop

    /* directly jump to exit procedure, not via the function pointer */
    b       ._haltproc
	
FUNCTION_PROLOG _haltproc
    /* exit call */
    li      0, 1
    sc
    b       ._haltproc

    /* Define a symbol for the first piece of initialized data.  */
    .section ".data"
    .globl  __data_start
__data_start:
data_start:

.text
    .comm operatingsystem_parameter_argc, 4
    .comm operatingsystem_parameter_argv, 8
    .comm operatingsystem_parameter_envp, 8

