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
                extern __fpc_PrefixSeg
                extern __fpc_CmdLine
                extern __fpc_CmdShow
                extern __fpc_HInstance
                extern __fpc_HPrevInst
                extern __fpc_SelectorInc

                extern InitTask
                import InitTask KERNEL
                extern WaitEvent
                import WaitEvent KERNEL
                extern InitApp
                import InitApp USER
                extern __AHIncr
                import __AHIncr KERNEL

..start:        ; Win16 applications start with the following
                ; values in registers:
                ;
                ; AX = zero
                ; BX = the size, in bytes, of the stack
                ; CX = the size, in bytes, of the heap
                ; DI = handle, identifying the new application instance
                ; SI = handle, identifying the previous application instance
                ; BP = zero
                ; ES = segment of the Program Segment Prefix (PSP)
                ; DS = segment of the automatic data segment for the application
                ; SS = DS
                ; SP = offset of the first byte of the application stack

                ; call InitTask to initialize the task. Windows expects this to
                ; be the first function, called by the application code. On entry,
                ; it expects all the startup parameters in registers described above.
                call far InitTask
                ; InitTask result:
                ; AX = 1-success; 0-error
                test ax, ax
                jz error
                ; InitTask result:
                ; CX = stack limit, in bytes
                ; DI = instance handle for the new task
                ; DX = the nCmdShow parameter
                ; ES = segment of the Program Segment Prefix (PSP) for the new task
                ; ES:BX = the command line
                ; SI = instance handle for the previous application instance (if any)

                mov ax, es
                mov [__fpc_PrefixSeg], ax
                mov [__fpc_CmdLine+2], ax
                mov [__fpc_CmdLine], bx
                mov [__fpc_CmdShow], dx
                mov [__fpc_HInstance], di
                mov [__fpc_HPrevInst], si

                ; the offset of the Win16 kernel function __AHIncr (by definition)
                ; gives us the value of SelectorInc. The function __AHIncr is
                ; otherwise useless (when called, it increments AH by one :) )
                ; The value of SelectorInc is usually 8 in most (all?) win16
                ; implementations, but it's good practice not to hardcode it.
                mov word [__fpc_SelectorInc], __AHIncr

                ; call WaitEvent(0) to clear the event that started this task
                ; Windows expects this call immediately after InitTask
                xor ax, ax
                push ax
                call far WaitEvent

                ; call InitApp(hInstance) to initialize the queue and support
                ; routines for the app. Windows expects this to be the third
                ; call in the Win16 startup sequence.
                push word [__fpc_HInstance]
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
                ; the first 16 bytes of the automatic data segment are reserved.
                ; they are filled by the InitTask function with these values
                dw 0
oOldSP:         dw 0
hOldSS:         dw 5
pLocalHeap:     dw 0
pAtomTable:     dw 0
pStackTop:      dw 0
pStackMin:      dw 0
pStackBot:      dw 0
                ; end of reserved area, filled by InitTask



                segment _STACK stack class=STACK align=16

                group DGROUP _DATA _STACK
