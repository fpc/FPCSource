       .file   "dllprt.cpp"
.text
        .p2align 2
.globl _._7FPC_DLL
        .type    _._7FPC_DLL,@function
_._7FPC_DLL:
.LFB1:
        pushl %ebp
.LCFI0:
        movl %esp,%ebp
.LCFI1:
        pushl %esi
.LCFI2:
        pushl %ebx
.LCFI3:
        call .L7
.L7:
        popl %ebx
        addl $_GLOBAL_OFFSET_TABLE_+[.-.L7],%ebx
        movl 8(%ebp),%esi
.L3:
        movl 12(%ebp),%eax
        andl $1,%eax
        testl %eax,%eax
        je .L5
        pushl %esi
.LCFI4:
        call __builtin_delete@PLT
        addl $4,%esp
        jmp .L5
        .p2align 4,,7
.L4:
.L5:
.L2:
        leal -8(%ebp),%esp
        popl %ebx
        popl %esi
        movl %ebp,%esp
        popl %ebp
        ret
.LFE1:
.Lfe1:
        .size    _._7FPC_DLL,.Lfe1-_._7FPC_DLL
.section        .rodata
.LC0:
        .string "dll"
.data
        .align 4
        .type    _argv,@object
        .size    _argv,8
_argv:
        .long .LC0
        .long 0
        .align 4
        .type    _envp,@object
        .size    _envp,4
_envp:
        .long 0
.text
        .p2align 2
.globl __7FPC_DLL
        .type    __7FPC_DLL,@function
__7FPC_DLL:
.LFB2:
        pushl %ebp
.LCFI5:
        movl %esp,%ebp
.LCFI6:
        pushl %ebx
.LCFI7:
        call .L11
.L11:
        popl %ebx
        addl $_GLOBAL_OFFSET_TABLE_+[.-.L11],%ebx
        movl operatingsystem_parameter_argc@GOT(%ebx),%eax
        movl $0,(%eax)
        movl operatingsystem_parameter_argv@GOT(%ebx),%eax
        movl %ebx,%ecx
        addl $_argv@GOTOFF,%ecx
        movl %ecx,%edx
        movl %edx,(%eax)
        movl operatingsystem_parameter_envp@GOT(%ebx),%eax
        movl %ebx,%ecx
        addl $_envp@GOTOFF,%ecx
        movl %ecx,%edx
        movl %edx,(%eax)
        call PASCALMAIN__Fv@PLT
.L9:
        movl 8(%ebp),%eax
        jmp .L8
.L8:
        movl -4(%ebp),%ebx
        movl %ebp,%esp
        popl %ebp
        ret
.LFE2:
.Lfe2:
        .size    __7FPC_DLL,.Lfe2-__7FPC_DLL

.section        .eh_frame,"aw",@progbits
__FRAME_BEGIN__:
        .4byte  .LLCIE1
.LSCIE1:
        .4byte  0x0
        .byte   0x1
        .byte   0x0
        .byte   0x1
        .byte   0x7c
        .byte   0x8
        .byte   0xc
        .byte   0x4
        .byte   0x4
        .byte   0x88
        .byte   0x1
        .align 4
.LECIE1:
        .set    .LLCIE1,.LECIE1-.LSCIE1
        .4byte  .LLFDE1
.LSFDE1:
        .4byte  .LSFDE1-__FRAME_BEGIN__
        .4byte  .LFB1
        .4byte  .LFE1-.LFB1
        .byte   0x4
        .4byte  .LCFI0-.LFB1
        .byte   0xe
        .byte   0x8
        .byte   0x85
        .byte   0x2
        .byte   0x4
        .4byte  .LCFI1-.LCFI0
        .byte   0xd
        .byte   0x5
        .byte   0x4
        .4byte  .LCFI2-.LCFI1
        .byte   0x86
        .byte   0x3
        .byte   0x4
        .4byte  .LCFI3-.LCFI2
        .byte   0x83
        .byte   0x4
        .byte   0x4
        .4byte  .LCFI4-.LCFI3
        .byte   0x2e
        .byte   0x4
        .align 4
.LEFDE1:
        .set    .LLFDE1,.LEFDE1-.LSFDE1
        .4byte  .LLFDE3
.LSFDE3:
        .4byte  .LSFDE3-__FRAME_BEGIN__
        .4byte  .LFB2
        .4byte  .LFE2-.LFB2
        .byte   0x4
        .4byte  .LCFI5-.LFB2
        .byte   0xe
        .byte   0x8
        .byte   0x85
        .byte   0x2
        .byte   0x4
        .4byte  .LCFI6-.LCFI5
        .byte   0xd
        .byte   0x5
        .byte   0x4
        .4byte  .LCFI7-.LCFI6
        .byte   0x83
        .byte   0x3
        .align 4
.LEFDE3:
        .set    .LLFDE3,.LEFDE3-.LSFDE3
        .ident  "GCC: (GNU) 2.9-beos-991026"
