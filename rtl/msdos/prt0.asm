; nasm -f obj -o prt0.o prt0.asm

	cpu 8086

	segment text use16

	extern PASCALMAIN

..start:
	mov ax, dgroup
	mov ss, ax
	mov sp, stacktop
	mov ds, ax
	mov es, ax
	jmp PASCALMAIN

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
	push ds
	mov si, dx
	mov ax, word [si + 16]
	mov es, ax
	mov ax, word [si + 18]  ; flags
	push ax
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
	popf
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
	pop si
	pop cx
	pop bx
	pop di
	pop es
        pop bp
	ret

	segment stack stack class=stack
	resb 4096
	stacktop:

	group dgroup stack
