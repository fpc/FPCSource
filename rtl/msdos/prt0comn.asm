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

        segment text use16 class=CODE

        extern PASCALMAIN
        extern __fpc_PrefixSeg
        extern dos_version
        extern __Test8086

        extern _edata  ; defined by WLINK, indicates start of BSS
        extern _end    ; defined by WLINK, indicates end of BSS

        extern __stklen
        extern __stktop
        extern __stkbottom

        extern __nearheap_start
        extern __nearheap_end

        extern ___heap

%ifdef __NEAR_DATA__
        extern __fpc_stackplusmaxheap_in_para
%endif

%ifndef __TINY__
    %ifdef __FAR_DATA__
        extern ___stack
    %endif
    %ifdef __NEAR_DATA__
        extern ___stacktop
    %endif
%endif

        extern __SaveInt00

        extern FPC_HANDLEERROR

%ifdef __TINY__
        resb 0100h
%endif
..start:
%ifdef __TINY__
        mov bx, cs
%else
        mov bx, DGROUP
    %ifdef __NEAR_DATA__
        ; init the stack
        mov ss, bx
        mov sp, ___stacktop wrt DGROUP
    %endif
%endif

        ; zero fill the BSS section
        mov es, bx
        mov di, _edata wrt DGROUP
        mov cx, _end wrt DGROUP
        sub cx, di
        xor al, al
        cld
        rep stosb

        ; save the Program Segment Prefix
        push ds

        ; init DS
        mov ds, bx

        ; pop the PSP from stack and store it in the pascal variable PrefixSeg
        pop ax
        mov word [__fpc_PrefixSeg], ax

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

%ifdef __NEAR_DATA__
; ****************************************************************************
; **                      near data memory layout setup                     **
; ****************************************************************************

        ; allocate max heap
        ; first we determine in paragraphs ax:=min(64kb, data+bss+stack+maxheap)
        mov ax, _end wrt DGROUP
        add ax, 15
        mov cl, 4
        shr ax, cl
        add ax, word [__fpc_stackplusmaxheap_in_para]
        cmp ax, 1000h  ; 1000h = 64k in paragraphs
        jbe data_with_maxheap_less_than_64k
        mov ax, 1000h
data_with_maxheap_less_than_64k:

        ; try to resize our main DOS memory block until the end of the data segment (or even smaller, if maxheap is small)
        mov cx, word [__fpc_PrefixSeg]
%ifdef __TINY__
        mov dx, cs
%else
        mov dx, DGROUP
%endif
        sub dx, cx  ; dx = (ds - psp) in paragraphs
        push dx  ; save (ds - psp)
        add dx, 1000h  ; 64kb in paragraphs

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
        pop cx  ; cx = (ds - psp)
        sub bx, cx
        mov cl, 4
        shl bx, cl
        sub bx, 2
        mov sp, bx

        mov word [__stktop], sp
        add bx, 2
        sub bx, word [__stklen]
        and bl, 0FEh
        mov word [__stkbottom], bx

        mov ax, _end wrt DGROUP
        cmp bx, ax
        jb not_enough_mem

        ; heap is between [ds:_end wrt DGROUP] and [ds:__stkbottom - 1]
        add ax, 3
        and al, 0FCh
        mov word [__nearheap_start], ax
        and bl, 0FCh
        mov word [__nearheap_end], bx

; ****************************************************************************
; **                      near data setup done                              **
; ****************************************************************************
%endif


%ifdef __FAR_DATA__
        mov word [__stktop], sp
        mov word [__stkbottom], 0
        mov ax, ss
        mov word [__stkbottom + 2], ax
        mov word [__stktop    + 2], ax

        mov dx, sp
        add dx, 15
        mov cl, 4
        shr dx, cl
        add ax, dx
        mov word [__nearheap_start], 0
        mov word [__nearheap_start + 2], ax

       ; get our MCB size in paragraphs
        mov cx, word [__fpc_PrefixSeg]
        dec cx
        mov es, cx
        mov bx, word [es:3]
        add bx, cx
        inc bx
        ; __nearheap_end := end_of_dos_memory_block
        mov word [__nearheap_end], 0
        mov word [__nearheap_end + 2], bx
%endif

%ifdef __FAR_CODE__
        jmp far PASCALMAIN
%else
        jmp PASCALMAIN
%endif

%ifdef __NEAR_DATA__
not_enough_mem:
        mov dx, not_enough_mem_msg
        jmp error_msg

mem_realloc_err:
        ; at this point there's still (ds-psp) pushed on the stack, but we won't
        ; bother popping it, because we exit to DOS with an error message here
        mov dx, mem_realloc_err_msg
error_msg:
        mov ah, 9
        int 21h
        mov ax, 4CFFh
        int 21h
%endif

FPC_INT00_HANDLER:
        sub sp, 4  ; reserve space on the stack for the retf

        push cx
        push ds
        push bp

        ; init ds
%ifdef __TINY__
        mov bp, cs
%else
        mov bp, DGROUP
%endif
        mov ds, bp

%ifdef __NEAR_DATA__
        ; in memory models, where SS=DS, also
        ; check whether we're running on the same stack
        mov cx, ss
        cmp bp, cx
        jne .call_previous_handler
%endif

%ifndef __FAR_CODE__
        ; check whether we're coming from the same code segment
        mov bp, sp
        mov cx, [bp + 3*2 + 6]  ; get caller segment
        mov bp, cs
        cmp bp, cx
        jne .call_previous_handler
%endif

        ; runerror 200
        mov bp, sp
        mov cx, [bp + 3*2 + 4]  ; get caller offset
%ifdef __FAR_CODE__
        mov dx, [bp + 3*2 + 6]  ; get caller segment
%endif
        pop bp
        add sp, 2*2 + 4 + 6
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
        mov bp, sp
        mov cx, [__SaveInt00]
        mov [bp + 3*2], cx
        mov cx, [__SaveInt00+2]
        mov [bp + 3*2 + 2], cx
        pop bp
        pop ds
        pop cx
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
        retf 4 + extra_data_offset
%else
        ret 4 + extra_data_offset
%endif

%ifndef __TINY__
        global FPC_CHECK_NULLAREA
FPC_CHECK_NULLAREA:
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
    %ifdef __FAR_CODE__
        retf
    %else
        ret
    %endif
%endif

        segment data class=DATA align=2
%ifdef __NEAR_DATA__
mem_realloc_err_msg:
        db 'Memory allocation error', 13, 10, '$'
not_enough_mem_msg:
        db 'Not enough memory', 13, 10, '$'
%endif
        ; add reference to the beginning of the minimal heap, so the object
        ; module, containing the heap segment doesn't get smartlinked away
        dw ___heap

        segment bss class=BSS align=2

%ifndef __TINY__
        segment _NULL align=16 class=BEGDATA
        global __nullarea
__nullarea:
        dd 01010101h, 01010101h, 01010101h, 01010101h
        dd 01010101h, 01010101h, 01010101h, 01010101h

        segment _AFTERNULL align=2 class=BEGDATA
        dw 0

    %ifdef __NEAR_DATA__
        segment stack stack class=STACK align=16
    %else
        segment data
        ; add reference to the beginning of stack, so the object module,
        ; containing the stack segment doesn't get smartlinked away
        dw ___stack
    %endif
%endif

%ifdef __TINY__
        group DGROUP text data bss
%else
    %ifdef __NEAR_DATA__
        group DGROUP _NULL _AFTERNULL data bss stack
    %else
        group DGROUP _NULL _AFTERNULL data bss
    %endif
%endif
