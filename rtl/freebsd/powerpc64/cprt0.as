        .file   "cprt0.as"

        .machine        power8
        .abiversion     2

        .section .rodata
.LC0:
        .asciz  ""

        .globl  __progname
        .section .data
        .p2align 3
        .type   __progname, @object
        .size   __progname, 8
__progname:
        .quad   .LC0

		.comm	environ,8,8
        # libc provides 'environ'
        .globl  environ
        .type   environ, @object

        # Optional weak reference: nonzero if dynamically linked, 0 for static
        .weak   _DYNAMIC

        # Expose OS parameter slots (matching your layout)
        .globl  operatingsystem_parameter_argc
        .globl  operatingsystem_parameter_argv
        .globl  operatingsystem_parameter_envp
        .p2align 3
operatingsystem_parameter_argc:
        .quad   0
operatingsystem_parameter_argv:
        .quad   0
operatingsystem_parameter_envp:
        .quad   0

        .text
        .p2align 2
        .globl  _start
        .type   _start, @function
_start:
        # ELFv2 process entry: r12 holds entry address; set up TOC in r2
        addis   2,12,.TOC.-_start@ha
        addi    2,2,.TOC.-_start@l
        .localentry _start, .-_start

        # Minimal frame (not strictly needed, but harmless)
        stdu    1,-32(1)
        std     0,16(1)

        # r3=argc, r4=argv, r5=envp (ELFv2 entry convention)

        # Store argc (32-bit) / argv / envp into your globals (TOC-relative)
        addis   9,2,operatingsystem_parameter_argc@toc@ha
        addi    9,9,operatingsystem_parameter_argc@toc@l
        stw     3,0(9)

        addis   10,2,operatingsystem_parameter_argv@toc@ha
        addi    10,10,operatingsystem_parameter_argv@toc@l
        std     4,0(10)

        addis   11,2,operatingsystem_parameter_envp@toc@ha
        addi    11,11,operatingsystem_parameter_envp@toc@l
        std     5,0(11)

        # environ = envp
        addis   12,2,environ@toc@ha
        addi    12,12,environ@toc@l
        std     5,0(12)

        # if (argc > 0 && argv[0] != NULL) { __progname = argv[0]; scan for last '/' }
        cmpdi   3,0
        ble     1f

        ld      6,0(4)                  # r6 = argv[0]
        cmpdi   6,0
        beq     1f

        # __progname = argv[0]
        addis   7,2,__progname@toc@ha
        addi    7,7,__progname@toc@l
        std     6,0(7)

        # Scan for last '/' to set __progname past it
        mr      8,6
0:      lbz     9,0(8)
        cmpdi   9,0
        beq     1f
        cmpdi   9,47                    # '/'
        bne     2f
        addi    10,8,1
        std     10,0(7)
2:      addi    8,8,1
        b       0b

1:
        # Call main(argc, argv, envp)
        # r3,r4,r5 already set appropriately
        bl      main
        nop

        # exit(main_ret)
        mr      3,3
        bl      exit
        nop

        # Should not return; just in case, trap.
        trap

        .size   _start, .-_start

        .section .comment
        .ascii  "FreeBSD PowerPC64 ELFv2 crt1 (minimal)\0"

