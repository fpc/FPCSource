/ code3.s (emx+gcc) -- Copyright (c) 1992-1996 by Eberhard Mattes

#include <emx/asm386.h>

        .globl  _DosQueryMessageCP

_DosQueryMessageCP:
        PROFILE_NOFRAME
        pushl   0(%esp)
        movl    $__msgseg32, %eax
        xchgl   20(%esp), %eax
        xchgl   16(%esp), %eax
        xchgl   12(%esp), %eax
        xchgl   8(%esp), %eax
        movl    %eax, 4(%esp)
        jmp     _DosIQueryMessageCP
