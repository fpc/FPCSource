{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Processor dependent part of strings.pp, that can be shared with
    sysutils unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ASMMODE ATT}

{$ifndef FPC_UNIT_HAS_STRICOMP}
{$define FPC_UNIT_HAS_STRICOMP}
function stricomp(str1,str2 : PAnsiChar) : longint; assembler; nostackframe;
{ eax = str1, edx = str2 }
asm
        push    %ebx
        push    %esi
        sub     %eax, %edx { edx = str2 - str1 }
        lea     -1(%eax), %esi { esi = str1 (-1), frees eax to use for str1^ for easier subtraction }
.balign 16
.LLoop:
        add     $1, %esi
        movzbl  (%esi), %eax { eax = str1^ }
        movzbl  (%esi,%edx), %ebx { ebx = str2^ }
        test    %eax, %eax
        jz      .LSub
        cmp     %ebx, %eax { Shortcut uppercasing if already equal. }
        je      .LLoop
        lea     -97(%eax), %ecx
        cmp     $25, %ecx
        ja      .LEaxUppercased
        sub     $32, %eax
.LEaxUppercased:
        lea     -97(%ebx), %ecx
        cmp     $25, %ecx
        ja      .LEbxUppercased
        sub     $32, %ebx
.LEbxUppercased:
        cmp     %ebx, %eax
        je      .LLoop
.LSub:
        sub     %ebx, %eax
        pop     %esi
        pop     %ebx
end;
{$endif FPC_UNIT_HAS_STRICOMP}



{$ifndef FPC_UNIT_HAS_STRLICOMP}
{$define FPC_UNIT_HAS_STRLICOMP}
function strlicomp(str1,str2 : PAnsiChar;l : sizeint) : longint; assembler; nostackframe;
{ eax = str1, edx = str2, ecx = L }
asm
        push    %ebx
        push    %esi
        push    %edi
        sub     %eax, %edx { edx = str2 - str1 }
        lea     -1(%eax), %esi { esi = str1 (-1), frees eax to use for str1^ for easier subtraction }
.balign 16
.LLoop:
        sub     $1, %ecx
        jl      .LNothing
        add     $1, %esi
        movzbl  (%esi), %eax { eax = str1^ }
        movzbl  (%esi,%edx), %ebx { ebx = str2^ }
        test    %eax, %eax
        jz      .LSub
        cmp     %ebx, %eax { Shortcut uppercasing if already equal. }
        je      .LLoop
        lea     -97(%eax), %edi
        cmp     $25, %edi
        ja      .LEaxUppercased
        sub     $32, %eax
.LEaxUppercased:
        lea     -97(%ebx), %edi
        cmp     $25, %edi
        ja      .LEbxUppercased
        sub     $32, %ebx
.LEbxUppercased:
        cmp     %ebx, %eax
        je      .LLoop
.LSub:
        sub     %ebx, %eax
        pop     %edi
        pop     %esi
        pop     %ebx
        ret
.LNothing:
        xor     %eax, %eax
        pop     %edi
        pop     %esi
        pop     %ebx
end;
{$endif FPC_UNIT_HAS_STRLICOMP}



{$ifndef FPC_UNIT_HAS_STRUPPER}
{$define FPC_UNIT_HAS_STRUPPER}
function strupper(p : PAnsiChar) : PAnsiChar; assembler; nostackframe;
{ eax = p }
asm
        mov     %eax, %ecx
        sub     $1, %eax
.balign 16
.LLoop:
        inc     %eax
        movzbl  (%eax), %edx
        test    %edx, %edx
        jz      .LDone
.LCheckLetterInEdx:
        sub     $97, %edx
        cmp     $25, %edx
        ja      .LLoop
        add     $(97 - 32), %edx
        mov     %dl, (%eax)
        inc     %eax { duplicate loop start instead of jmp .LLoop }
        movzbl  (%eax), %edx
        test    %edx, %edx
        jnz     .LCheckLetterInEdx
.LDone:
        mov     %ecx, %eax
end;
{$endif FPC_UNIT_HAS_STRUPPER}


{$ifndef FPC_UNIT_HAS_STRLOWER}
{$define FPC_UNIT_HAS_STRLOWER}
function strlower(p : PAnsiChar) : PAnsiChar; assembler; nostackframe;
{ eax = p }
asm
        mov     %eax, %ecx
        sub     $1, %eax
.balign 16
.LLoop:
        inc     %eax
        movzbl  (%eax), %edx
        test    %edx, %edx
        jz      .LDone
.LCheckLetterInEdx:
        sub     $65, %edx
        cmp     $25, %edx
        ja      .LLoop
        add     $(65 + 32), %edx
        mov     %dl, (%eax)
        inc     %eax { duplicate loop start instead of jmp .LLoop }
        movzbl  (%eax), %edx
        test    %edx, %edx
        jnz     .LCheckLetterInEdx
.LDone:
        mov     %ecx, %eax
end;
{$endif FPC_UNIT_HAS_STRLOWER}

