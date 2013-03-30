; nasm -f obj -o prt0.o prt0.asm

        cpu 8086

        segment text use16

        extern PASCALMAIN
        extern dos_psp

        extern _edata  ; defined by WLINK, indicates start of BSS
        extern _end    ; defined by WLINK, indicates end of BSS

        extern __stklen
        extern __stkbottom

        extern __nearheap_start
        extern __nearheap_end

..start:
        ; init the stack
        mov ax, dgroup
        mov ss, ax
        mov sp, stacktop

        ; save the Program Segment Prefix
        push ds

        ; init DS
        mov ds, ax

        ; pop the PSP from stack and store it in the pascal variable dos_psp
        pop ax
        mov word [dos_psp], ax

        ; allocate max heap
        ; TODO: also support user specified heap size
        ; try to resize our main DOS memory block until the end of the data segment
        mov bx, word [dos_psp]
        mov es, bx
        sub bx, dgroup
        neg bx  ; bx = (ds - psp) in paragraphs
        add bx, 1000h  ; 64kb in paragraphs
        mov ah, 4Ah
        int 21h
        jc mem_realloc_err

        ; init ES
        mov ax, dgroup
        mov es, ax

        ; bx = the new size in paragraphs
        add bx, word [dos_psp]
        sub bx, dgroup
        mov cl, 4
        shl bx, cl
        sub bx, 2
        mov sp, bx

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

        jmp PASCALMAIN

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

        global FPC_MSDOS_CARRY
FPC_MSDOS_CARRY:
        stc
        global FPC_MSDOS
FPC_MSDOS:
        mov dx, ax
        mov al, 21h
        global FPC_INTR
FPC_INTR:
        mov byte [cs:int_number], al
        push bp
        push es
        push di
        push bx
        push cx
        push si
        push dx
        push ds
        mov si, dx
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
        mov si, word [ss:bp + 8]
        mov ds, si
        mov si, word [ss:bp + 10]
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
        pop dx
        pop si
        pop cx
        pop bx
        pop di
        pop es
        pop bp
        ret

        segment data
mem_realloc_err_msg:
        db 'Memory allocation error', 13, 10, '$'
not_enough_mem_msg:
        db 'Not enough memory', 13, 10, '$'

        segment bss class=bss

        segment stack stack class=stack
        resb 256
        stacktop:

        group dgroup data bss stack
