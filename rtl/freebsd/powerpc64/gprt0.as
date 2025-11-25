/* FreeBSD/FPC startup for PowerPC64 ELFv2
 * Derived from glibc crt0 but stripped of ELFv1 .opd/ptrgl.
 */

        .section .text
        .align 2
        .globl _start
        .type  _start,@function
_start:
        # r3=argc, r4=argv, r5=envp (ELFv2)
        mr      9,1                  # save SP

        clrrdi  1,1,4                # align SP
        li      0,0
        stdu    1,-128(1)
        mtlr    0
        std     0,16(1)

        # Save argc/argv/envp for Pascal RTL
        lis     8, operatingsystem_parameter_argc@highest
        ori     8,8,operatingsystem_parameter_argc@higher
        sldi    8,8,32
        oris    8,8,operatingsystem_parameter_argc@h
        ori     8,8,operatingsystem_parameter_argc@l
        stw     3,0(8)

        lis     8, operatingsystem_parameter_argv@highest
        ori     8,8,operatingsystem_parameter_argv@higher
        sldi    8,8,32
        oris    8,8,operatingsystem_parameter_argv@h
        ori     8,8,operatingsystem_parameter_argv@l
        std     4,0(8)

        lis     8, operatingsystem_parameter_envp@highest
        ori     8,8,operatingsystem_parameter_envp@higher
        sldi    8,8,32
        oris    8,8,operatingsystem_parameter_envp@h
        ori     8,8,operatingsystem_parameter_envp@l
        std     5,0(8)

        lis     8, __stkptr@highest
        ori     8,8,__stkptr@higher
        sldi    8,8,32
        oris    8,8,__stkptr@h
        ori     8,8,__stkptr@l
        std     1,0(8)

        # Call Pascal entry
        bl      PASCALMAIN
        nop

        b       _haltproc

        .size _start, .-_start

        .globl _haltproc
        .type  _haltproc,@function
_haltproc:
        lis     8, ___fpc_ret@highest
        ori     8,8,___fpc_ret@higher
        sldi    8,8,32
        oris    8,8,___fpc_ret@h
        ori     8,8,___fpc_ret@l
        ld      1,0(8)
        addi    1,1,128
        ld      0,16(1)
        mtlr    0
        blr
		nop
        .size _haltproc, .-_haltproc

        .section .data
        .globl __data_start
__data_start:
data_start:

___fpc_ret:
        .quad 0

        .section .bss
        .type __stkptr,@object
        .size __stkptr,8
        .globl __stkptr
__stkptr:
        .skip 8

        .type operatingsystem_parameters,@object
        .size operatingsystem_parameters,24
        .globl operatingsystem_parameters
operatingsystem_parameters:
        .skip 24
        .globl operatingsystem_parameter_argc
        .globl operatingsystem_parameter_argv
        .globl operatingsystem_parameter_envp
        .set operatingsystem_parameter_argc,operatingsystem_parameters+0
        .set operatingsystem_parameter_argv,operatingsystem_parameters+8
        .set operatingsystem_parameter_envp,operatingsystem_parameters+16

        .section .note.ABI-tag,"a",@progbits
        .p2align 2
        .long 8,4,1              # name size, desc size, tag
        .string "FreeBSD"        # ABI name
        .p2align 2
        .long 0,0,0              # version fields

        .section .note.GNU-stack,"",%progbits
