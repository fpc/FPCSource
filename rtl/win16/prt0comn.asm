; common startup code for all the memory models

%ifdef __TINY__
        %define __NEAR_CODE__
        %define __NEAR_DATA__
%elifdef __SMALL__
        %define __NEAR_CODE__
        %define __NEAR_DATA__
%elifdef __MEDIUM__
        %define __FAR_CODE__
        %define __NEAR_DATA__
%elifdef __COMPACT__
        %define __NEAR_CODE__
        %define __FAR_DATA__
%elifdef __LARGE__
        %define __FAR_CODE__
        %define __FAR_DATA__
%elifdef __HUGE__
        %define __FAR_CODE__
        %define __FAR_DATA__
%else
        %fatal "Memory model not defined."
%endif

        cpu 8086
        segment _TEXT use16 class=CODE align=1

        extern PASCALMAIN

        extern InitTask
        extern WaitEvent
        extern InitApp

..start:
        call far InitTask
        test ax, ax
        jz error

        mov [hInst], di

        xor ax, ax
        push ax
        call far WaitEvent
        push word [hInst]
        call far InitApp
        test ax, ax
        jz error

%ifdef __FAR_CODE__
        jmp far PASCALMAIN
%else
        jmp PASCALMAIN
%endif

error:
        mov ax, 4cffh
        int 21h


        segment _DATA use16 class=DATA align=2
        dw 0,0,5,0,0,0,0,0
hInst:  dw 0

        segment _STACK stack class=STACK align=16

        group DGROUP _DATA _STACK
