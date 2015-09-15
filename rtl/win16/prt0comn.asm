; common startup code for all the memory models

%ifdef __TINY__
        %fatal "The tiny memory model is not supported by Windows."
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

%ifdef __FAR_CODE__
        extra_param_offset equ 2
%else
        extra_param_offset equ 0
%endif

%ifdef __FAR_DATA__
        extra_data_offset equ 2
%else
        extra_data_offset equ 0
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
                extern ___stack

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
                call far PASCALMAIN
%else
                call PASCALMAIN
%endif

error:
                mov ax, 4cffh
                int 21h


                global FPC_MSDOS_CARRY
FPC_MSDOS_CARRY:
                stc
                global FPC_MSDOS
FPC_MSDOS:
                mov al, 21h  ; not ax, because only the low byte is used
                pop dx
%ifdef __FAR_CODE__
                pop bx
%endif
                pop cx
%ifdef __FAR_DATA__
                pop si
%endif
                push ax
%ifdef __FAR_DATA__
                push si
%endif
                push cx
%ifdef __FAR_CODE__
                push bx
%endif
                push dx
;                global FPC_INTR
;FPC_INTR:
%ifdef __FAR_CODE__
                inc bp
%endif
                push bp
                mov bp, sp
                mov al, byte [bp + 6 + extra_param_offset + extra_data_offset]
;                mov byte [cs:int_number], al
                mov si, [bp + 4 + extra_param_offset]
                push ds
%ifdef __FAR_DATA__
                mov ax, [bp + 6 + extra_param_offset]
                mov ds, ax
%endif
                mov ax, word [si + 16]
                mov es, ax
                mov ax, word [si + 14]  ; ds
                push ax
                mov ax, word [si]
                mov bx, word [si + 2]
                mov cx, word [si + 4]
                mov dx, word [si + 6]
                mov bp, word [si + 8]
                mov di, word [si + 12]
                mov si, word [si + 10]

                pop ds
;                db 0CDh  ; opcode of INT xx
;int_number:
;                db 255
                int 21h

                pushf
                push ds
                push si
                push bp
                mov bp, sp
%ifdef __FAR_DATA__
                mov si, [bp + 16 + extra_param_offset]
%else
                mov si, word [bp + 8]
%endif
                mov ds, si
                mov si, word [bp + 14 + extra_param_offset]
                mov word [si], ax
                mov word [si + 2], bx
                mov word [si + 4], cx
                mov word [si + 6], dx
                mov word [si + 12], di
                mov ax, es
                mov word [si + 16], ax
                pop ax
                mov word [si + 8], ax
                pop ax
                mov word [si + 10], ax
                pop ax
                mov word [si + 14], ax
                pop ax
                mov word [si + 18], ax

                pop ds
                pop bp
%ifdef __FAR_CODE__
                dec bp
                retf 4 + extra_data_offset
%else
                ret 4 + extra_data_offset
%endif


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



                segment stack stack class=STACK align=16

                group DGROUP _DATA stack
