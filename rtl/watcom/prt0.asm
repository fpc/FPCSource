.387
.386p

	name prt0
	assume nothing
	extrn PASCALMAIN : near
	extrn HEAPSIZE : dword
	public start
	public ___exit
	public ___sbrk
	public HEAP
	public PSP_SELECTOR
	public ENV_SELECTOR
	public ENV_SIZE

.STACK 1000h
.CODE

start proc near
        	jmp     short main
        	db      "WATCOM"
	main:
		mov	ax,es             ; psp selector in es
		mov	PSP_SELECTOR,ax
		mov	gs,ax
		mov	bx,[gs:2Ch]       ; environment sel. at psp_sel:2C
		mov	ENV_SELECTOR,bx
		lsl	ecx,bx            ; get selector limit
		mov	ENV_SIZE,ecx
		push	ds
		pop	es
		push	ds
		pop	fs
		mov	eax,HEAPSIZE
		push	eax
		call	___sbrk           ; allocate heap  
		mov	HEAP,eax
		pop	eax
        	call    PASCALMAIN
start endp

___exit proc near
		pop	eax
		mov	ah,4Ch
		int	21h
___exit endp

___sbrk proc near
		mov	ebx,dword ptr [esp+4]
		mov	ecx,ebx
		shr	ebx,16
		mov	ax,501h
		int	31h
		jnc	sbrk_ok
		xor	eax,eax
		ret
	sbrk_ok:
		shl	ebx,16
		mov	bx,cx
		mov	eax,ebx
		ret
___sbrk endp

.DATA
	HEAP dd 0
	PSP_SELECTOR dw 0
	ENV_SELECTOR dw 0
	ENV_SIZE dd 0


end start
