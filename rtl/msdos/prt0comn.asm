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

        segment text use16 class=code

        extern PASCALMAIN
        extern dos_psp
        extern dos_version
        extern __Test8086

        extern _edata  ; defined by WLINK, indicates start of BSS
        extern _end    ; defined by WLINK, indicates end of BSS

        extern __stklen
        extern __stktop
        extern __stkbottom

        extern __nearheap_start
        extern __nearheap_end

        extern __SaveInt00

        extern FPC_HANDLEERROR

%ifdef __TINY__
        resb 0100h
%endif
..start:
%ifdef __TINY__
        mov bx, cs
%else
        ; init the stack
        mov bx, dgroup
        mov ss, bx
        mov sp, stacktop
%endif

        ; zero fill the BSS section
        mov es, bx
        mov di, _edata wrt dgroup
        mov cx, _end wrt dgroup
        sub cx, di
        xor al, al
        cld
        rep stosb

        ; save the Program Segment Prefix
        push ds

        ; init DS
        mov ds, bx

        ; pop the PSP from stack and store it in the pascal variable dos_psp
        pop ax
        mov word [dos_psp], ax

        ; get DOS version and save it in the pascal variable dos_version
        mov ax, 3000h
        int 21h
        xchg al, ah
        mov word [dos_version], ax

        ; detect CPU
        xor bx, bx  ; 0=8086/8088/80186/80188/NEC V20/NEC V30
        ; on pre-286 processors, bits 12..15 of the FLAGS registers are always set
        pushf
        pop ax
        and ah, 0fh
        push ax
        popf
        pushf
        pop ax
        and ah, 0f0h
        cmp ah, 0f0h
        je cpu_detect_done
        ; at this point we have a 286 or higher
        inc bx
        ; on a true 286 in real mode, bits 12..15 are always clear
        pushf
        pop ax
        or ah, 0f0h
        push ax
        popf
        pushf
        pop ax
        and ah, 0f0h
        jz cpu_detect_done
        ; we have a 386+
        inc bx

cpu_detect_done:
        mov [__Test8086], bl

        ; allocate max heap
        ; TODO: also support user specified heap size
        ; try to resize our main DOS memory block until the end of the data segment
%ifdef __TINY__
        mov cx, cs
        mov dx, 1000h  ; 64kb in paragraphs
%else
        mov dx, word [dos_psp]
        mov cx, dx
        sub dx, dgroup
        neg dx  ; dx = (ds - psp) in paragraphs
        add dx, 1000h  ; 64kb in paragraphs
%endif

         ; get our MCB size in paragraphs
        dec cx
        mov es, cx
        mov bx, word [es:3]

        ; is it smaller than the maximum data segment size?
        cmp bx, dx
        jbe skip_mem_realloc

        mov bx, dx
        inc cx
        mov es, cx
        mov ah, 4Ah
        int 21h
        jc mem_realloc_err

skip_mem_realloc:

        ; bx = the new size in paragraphs
%ifndef __TINY__
        add bx, word [dos_psp]
        sub bx, dgroup
%endif
        mov cl, 4
        shl bx, cl
        sub bx, 2
        mov sp, bx

        mov word [__stktop], sp
        add bx, 2
        sub bx, word [__stklen]
        and bl, 0FEh
        mov word [__stkbottom], bx

        cmp bx, _end wrt dgroup
        jb not_enough_mem

        ; heap is between [ds:_end wrt dgroup] and [ds:__stkbottom - 1]
        mov word [__nearheap_start], _end wrt dgroup
        mov bx, word [__stkbottom]
        dec bx
        mov word [__nearheap_end], bx

%ifdef __FAR_CODE__
        jmp far PASCALMAIN
%else
        jmp PASCALMAIN
%endif

not_enough_mem:
        mov dx, not_enough_mem_msg
        jmp error_msg

mem_realloc_err:
        mov dx, mem_realloc_err_msg
error_msg:
        mov ah, 9
        int 21h
        mov ax, 4CFFh
        int 21h

FPC_INT00_HANDLER:
        sub sp, 4  ; reserve space on the stack for the retf

        push bx
        push cx
        push ds

        ; init ds
%ifdef __TINY__
        mov bx, cs
%else
        mov bx, dgroup
%endif
        mov ds, bx

        ; check whether we're running on the same stack
        mov cx, ss
        cmp bx, cx
        jne .call_previous_handler

%ifndef __FAR_CODE__
        ; check whether we're coming from the same code segment
        mov bx, sp
        mov cx, [bx + 3*2 + 6]  ; get caller segment
        mov bx, cs
        cmp bx, cx
        jne .call_previous_handler
%endif

        ; runerror 200
        mov bx, sp
        mov cx, [bx + 3*2 + 4]  ; get caller offset
%ifdef __FAR_CODE__
        mov dx, [bx + 3*2 + 6]  ; get caller segment
%endif
        add sp, 3*2 + 4 + 6
        xor ax, ax
        push ax
        mov ax, 200
        push ax
%ifdef __FAR_CODE__
        push dx
%endif
        push cx
        cld
%ifdef __FAR_CODE__
        jmp far FPC_HANDLEERROR
%else
        jmp FPC_HANDLEERROR
%endif

.call_previous_handler:
        mov bx, sp
        mov cx, [__SaveInt00]
        mov [ss:bx + 3*2], cx
        mov cx, [__SaveInt00+2]
        mov [ss:bx + 3*2 + 2], cx
        pop ds
        pop cx
        pop bx
        retf  ; jumps to the previous handler with all registers and stack intact



        global FPC_INSTALL_INTERRUPT_HANDLERS
FPC_INSTALL_INTERRUPT_HANDLERS:
        push ds

        ; save old int 00 handler
        mov ax, 3500h
        int 21h
        mov [__SaveInt00], bx
        mov bx, es
        mov [__SaveInt00+2], bx

        ; install the new int 00 handler
%ifndef __TINY__
        push cs
        pop ds
%endif
        mov dx, FPC_INT00_HANDLER
        mov ax, 2500h
        int 21h

        pop ds
%ifdef __FAR_CODE__
        retf
%else
        ret
%endif


        global FPC_RESTORE_INTERRUPT_HANDLERS
FPC_RESTORE_INTERRUPT_HANDLERS:
        push ds

        mov ax, 2500h
        lds dx, [__SaveInt00]
        int 21h

        pop ds
%ifdef __FAR_CODE__
        retf
%else
        ret
%endif


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
        global FPC_INTR
FPC_INTR:
        push bp
        mov bp, sp
        mov al, byte [bp + 6 + extra_param_offset + extra_data_offset]
        mov byte [cs:int_number], al
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
        db 0CDh  ; opcode of INT xx
int_number:
        db 255
        
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
        retf 4
%else
        ret 4
%endif

        global FPC_CHECK_NULLAREA
FPC_CHECK_NULLAREA:
%ifdef __TINY__
        ; tiny model has no nil pointer assignment checking; always return true.
        mov al, 1
%else
        push ds
        pop es
        xor di, di
        mov cx, 32
        mov al, 1
        cld
        repe scasb
        je .skip
        dec ax   ; 1 byte shorter than dec al
.skip:
%endif
%ifdef __FAR_CODE__
        retf
%else
        ret
%endif

        segment data
mem_realloc_err_msg:
        db 'Memory allocation error', 13, 10, '$'
not_enough_mem_msg:
        db 'Not enough memory', 13, 10, '$'

        segment bss class=bss

%ifndef __TINY__
        segment _NULL align=16 class=BEGDATA
        global __nullarea
__nullarea:
        dd 01010101h, 01010101h, 01010101h, 01010101h
        dd 01010101h, 01010101h, 01010101h, 01010101h

        segment _AFTERNULL align=2 class=BEGDATA
        dw 0

        segment stack stack class=stack
        resb 256
        stacktop:
%endif

%ifdef __TINY__
        group dgroup text data bss
%else
        group dgroup _NULL _AFTERNULL data bss stack
%endif
