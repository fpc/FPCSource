
 ;  to do: command line, environment


.387
.386p

        name prt0
        assume nothing
        extrn PASCALMAIN : near
        public start
        public ___exit
        public ___sbrk

.STACK 1000h
.CODE

start proc near
                jmp     short main
                db      "WATCOM"
        main:
                push    ds
                pop     es
                push    ds
                pop     fs
                call    PASCALMAIN
                mov     ah,4Ch
                int     21h
start endp

___exit proc near
                pop     eax
                mov     ah,4Ch
                int     21h
___exit endp

___sbrk proc near
                mov     ebx,dword ptr [esp+4] ; size
                mov     cx,bx
                shr     ebx,16
                mov     ax,501h
                int     31h
                jnc     sbrk_ok
        sbrk_failed:
                xor     eax,eax
                ret
        sbrk_ok:
                shl     ebx,16
                mov     bx,cx
                mov     eax,ebx
                ret
___sbrk endp

end start
