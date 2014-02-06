/*
Library startup code for Android/MIPS
by Vsevolod Alekseyev
*/

    .set noat
    .section ".text"
    .align 4

    .global FPC_SHARED_LIB_START
FPC_SHARED_LIB_START:
    .ent FPC_SHARED_LIB_START
    .frame $sp, 32, $ra
    .set noreorder
    .cpload $t9
    /*
    Stack structure:
    sp+16 - gp
    sp+20 - sp
    sp+24 - ra
    */

    /* Save old sp, align the stack, set up the frame, save regs */
    move $t0, $sp
    li $t1, -8
    and $sp, $sp, $t1
    subu $sp, $sp, 32
    .cprestore 16
    sw $t0, 20($sp)
    sw $ra, 24($sp)

    /* Save initial stack pointer, return address */
    la $t0, __stkptr
    sw $sp, ($t0)

    /* Get environment from libc */
    la $t0, environ
    lw $t0, ($t0)
    bne $t0, $zero, GotEnv
    nop
    la $t0, EmptyEnv
GotEnv:
    la $t1, operatingsystem_parameter_envp
    sw $t0, ($t1)

    /* Register exit handler */
    la $a0, FPC_LIB_EXIT
    jal atexit
    nop

    /* Call main, exit */
    jal PASCALMAIN
    nop

    /* restore registers, exit */
    lw $ra, 24($sp)
    jr $ra
    lw $sp, 20($sp)
    .end FPC_SHARED_LIB_START
    .size FPC_SHARED_LIB_START, .-FPC_SHARED_LIB_START

/*************************/

    .global _haltproc
_haltproc:
    .ent _haltproc
    .set noreorder
    .cpload $t9
    la $t9, exit
    jr $t9
    nop
    .end _haltproc
    .size _haltproc, .-_haltproc

/*************************/

    .comm __stkptr,4
    .comm operatingsystem_parameter_envp,4
operatingsystem_parameter_argc:
    .global operatingsystem_parameter_argc
    .long 1
operatingsystem_parameter_argv:
    .global operatingsystem_parameter_argv
    .long EmptyCmdLine
EmptyCmdLine:
    .long EmptyCmdStr
EmptyCmdStr:
    .ascii "\0"

EmptyEnv:
    .long 0
    .long 0
    .long 0
