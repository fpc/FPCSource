/*
Startup code for Android/MIPS
by Vsevolod Alekseyev
*/

    .set at
    .section ".text"
    .align 4

    .global _fpc_start
_fpc_start:
    .ent _fpc_start
    .set noreorder
    .cpload $t9

    /*Align the stack, save the old sp */
    move $t0, $sp
    li $t1, -8
    and $sp, $sp, $t1
    sw $t0, __stkptr

    /* Get argc/argv/envp from the stack; old sp is in t0 */
    lw $t1, ($t0)
    sw $t1, (operatingsystem_parameter_argc)
    addiu $t2, $t0, 4
    sw $t2, (operatingsystem_parameter_argv)
    addiu $t3, $t1, 1
    sll $t3, $t3, 2
    add $t2, $t2, $t3
    sw $t2, (operatingsystem_parameter_envp)

    la $t9, _start
    jr $t9
    nop
    .end _fpc_start

/*************************/

    .global _haltproc
_haltproc:
    .ent _haltproc
    .set noreorder
    .cpload $t9
    lw $a0, (operatingsystem_result)
    la $t9, exit
    jr $t9
    nop
    .end _haltproc
    .size _haltproc, .-_haltproc

/*************************/

    .comm __stkptr,4

    .comm operatingsystem_parameter_envp,4
    .comm operatingsystem_parameter_argc,4
    .comm operatingsystem_parameter_argv,4
