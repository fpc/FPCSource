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

	segment stack stack
	resb 4096
	stacktop:

	group dgroup stack
