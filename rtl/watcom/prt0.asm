
 ;  to do: command line, environment


.387
.386p

	name cstart
	assume nothing
	extrn PASCALMAIN : near
	public _cstart_
	public ___exit
	public ___sbrk

.STACK 1000h
.CODE

_cstart_ proc near
        	jmp     short main
        	db      "WATCOM"
	main:
		push	ds
		pop	es
		push	ds
		pop	fs
        	call    PASCALMAIN
_cstart_ endp

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
		mov	eax,-1
		ret
	sbrk_ok:
		shl	ebx,16
		mov	bx,cx
		mov	eax,ebx
		ret
___sbrk endp

end _cstart_
