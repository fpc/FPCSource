/ code2.as (emx+fpk) -- Copyright (c) 1992-1999-2000 by Eberhard Mattes
/                       Changed for Free Pascal in 1999-2000 by Daniel Mantione.
/                       This code is _not_ under the Library GNU Public
/                       License, because the original is not. See copying.emx
/                       for details. You should have received it with this
/                       product, write the author if you haven't.

        .globl  DosQueryMessageCP

DosQueryMessageCP:
        pushl   0(%esp)
        movl    $_msgseg32, %eax
        xchgl   20(%esp), %eax
        xchgl   16(%esp), %eax
        xchgl   12(%esp), %eax
        xchgl   8(%esp), %eax
        movl    %eax, 4(%esp)
        jmp     _DOSCALLS$$_DOSIQUERYMESSAGECP$$$$$LONGINT$PCHAR$LONGINT$POINTER
