{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by the Free Pascal development team

    Include file with set operations called by the compiler

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$asmmode intel}

{$ifndef FPC_SYSTEM_HAS_FPC_VARSET_ADD_SETS}
procedure fpc_varset_add_sets(const set1,set2; var dest;size : ptrint); compilerproc; assembler; nostackframe;
{ Windows: rcx = set1, rdx = set2, r8 = dest, r9 = size
  Linux:   rdi = set1, rsi = set2, rdx = dest, rcx = size }
asm
    sub    size, 16
    jl     @Bytewise_Prepare { probably dead branch... }

@16x_Loop:
    movdqu xmm0, xmmword ptr [set1 + size]
    movdqu xmm1, xmmword ptr [set2 + size]
    por    xmm0, xmm1
    movdqu xmmword ptr [dest + size], xmm0
    sub    size, 16
    ja     @16x_Loop

    movdqu xmm0, xmmword ptr [set1] { Tail, just in case (if size is always divisible by 16, 16x_Loop can be altered to handle everything instead). }
    movdqu xmm1, xmmword ptr [set2]
    por    xmm0, xmm1
    movdqu xmmword ptr [dest], xmm0
    ret

@Bytewise_Prepare:
    add    size, 15
@Bytewise_Loop:
    movzx  eax, byte ptr [set1 + size]
    or     al, byte ptr [set2 + size]
    mov    byte ptr [dest + size], al
    sub    size, 1
    jae    @Bytewise_Loop
end;
{$define FPC_SYSTEM_HAS_FPC_VARSET_ADD_SETS}
{$endif ndef FPC_SYSTEM_HAS_FPC_VARSET_ADD_SETS}

{$ifndef FPC_SYSTEM_HAS_FPC_VARSET_MUL_SETS}
procedure fpc_varset_mul_sets(const set1,set2; var dest;size : ptrint); compilerproc; assembler; nostackframe;
{ Same as fpc_varset_add_sets but with 'and' instead of 'or'. }
asm
    sub    size, 16
    jl     @Bytewise_Prepare { probably dead branch... }

@16x_Loop:
    movdqu xmm0, xmmword ptr [set1 + size]
    movdqu xmm1, xmmword ptr [set2 + size]
    pand   xmm0, xmm1
    movdqu xmmword ptr [dest + size], xmm0
    sub    size, 16
    ja     @16x_Loop

    movdqu xmm0, xmmword ptr [set1] { Tail, just in case (if size is always divisible by 16, 16x_Loop can be altered to handle everything instead). }
    movdqu xmm1, xmmword ptr [set2]
    pand   xmm0, xmm1
    movdqu xmmword ptr [dest], xmm0
    ret

@Bytewise_Prepare:
    add    size, 15
@Bytewise_Loop:
    movzx  eax, byte ptr [set1 + size]
    and    al, byte ptr [set2 + size]
    mov    byte ptr [dest + size], al
    sub    size, 1
    jae    @Bytewise_Loop
end;
{$define FPC_SYSTEM_HAS_FPC_VARSET_MUL_SETS}
{$endif ndef FPC_SYSTEM_HAS_FPC_VARSET_MUL_SETS}

{$ifndef FPC_SYSTEM_HAS_FPC_VARSET_SUB_SETS}
procedure fpc_varset_sub_sets(const set1,set2; var dest;size : ptrint); compilerproc; assembler; nostackframe;
{ Windows: rcx = set1, rdx = set2, r8 = dest, r9 = size
  Linux:   rdi = set1, rsi = set2, rdx = dest, rcx = size }
asm
    sub    size, 16
    jl     @Bytewise_Prepare { probably dead branch... }

    movdqu xmm1, xmmword ptr [set1] { Tail, just in case (if size is always divisible by 16, 16x_Loop can be altered to handle everything instead). }
    movdqu xmm2, xmmword ptr [set2] { Precalculated because operation is not idempotent and dest can be equal to set1/set2. }
    pandn  xmm2, xmm1

@16x_Loop:
    movdqu xmm1, xmmword ptr [set1 + size]
    movdqu xmm0, xmmword ptr [set2 + size]
    pandn  xmm0, xmm1
    movdqu xmmword ptr [dest + size], xmm0
    sub    size, 16
    ja     @16x_Loop

    movdqu xmmword ptr [dest], xmm2 { Write precalculated tail. }
    ret

@Bytewise_Prepare:
    add    size, 15
@Bytewise_Loop:
    movzx  eax, byte ptr [set2 + size]
    not    eax
    and    al, byte ptr [set1 + size]
    mov    byte ptr [dest + size], al
    sub    size, 1
    jae    @Bytewise_Loop
end;
{$define FPC_SYSTEM_HAS_FPC_VARSET_SUB_SETS}
{$endif ndef FPC_SYSTEM_HAS_FPC_VARSET_SUB_SETS}

{$ifndef FPC_SYSTEM_HAS_FPC_VARSET_SYMDIF_SETS}
procedure fpc_varset_symdif_sets(const set1,set2; var dest;size : ptrint); compilerproc; assembler; nostackframe;
{ Same as fpc_varset_mul_sets but with 'xor' instead of 'and not'.

  Windows: rcx = set1, rdx = set2, r8 = dest, r9 = size
  Linux:   rdi = set1, rsi = set2, rdx = dest, rcx = size }
asm
    sub    size, 16
    jl     @Bytewise_Prepare { probably dead branch... }

    movdqu xmm2, xmmword ptr [set1] { Tail, just in case (if size is always divisible by 16, 16x_Loop can be altered to handle everything instead). }
    movdqu xmm1, xmmword ptr [set2] { Precalculated because operation is not idempotent and dest can be equal to set1/set2. }
    pxor   xmm2, xmm1

@16x_Loop:
    movdqu xmm0, xmmword ptr [set1 + size]
    movdqu xmm1, xmmword ptr [set2 + size]
    pxor   xmm0, xmm1
    movdqu xmmword ptr [dest + size], xmm0
    sub    size, 16
    ja     @16x_Loop

    movdqu xmmword ptr [dest], xmm2 { Write precalculated tail. }
    ret

@Bytewise_Prepare:
    add    size, 15
@Bytewise_Loop:
    movzx  eax, byte ptr [set2 + size]
    xor    al, byte ptr [set1 + size]
    mov    byte ptr [dest + size], al
    sub    size, 1
    jae    @Bytewise_Loop
end;
{$define FPC_SYSTEM_HAS_FPC_VARSET_SYMDIF_SETS}
{$endif ndef FPC_SYSTEM_HAS_FPC_VARSET_SYMDIF_SETS}

{$ifndef FPC_SYSTEM_HAS_FPC_VARSET_CONTAINS_SET}
function fpc_varset_contains_sets(const set1,set2;size : ptrint):boolean; compilerproc; assembler; nostackframe;
{ Windows: rcx = set1, rdx = set2, r8 = size
  Linux:   rdi = set1, rsi = set2, rdx = size }
asm
    sub    size, 16
    jl     @Bytewise_Prepare { probably dead branch... }

{$if false}
{ Scans 16 bytes at a time left to right with early exits.
  Would be better for large enough sets (maybe around 64 bytes or even more) — if they existed, but worse for actually existing 32.
  Kept for the future. }
    pxor   xmm2, xmm2 { xmm2 = 0 }
    add    set1, size
    add    set2, size
    neg    size { Now "size" = -(orig.size - 16), "set1" points to orig.set1 + orig.size - 16, "set2" points to orig.set2 + orig.size - 16.
                  Loop ends on "size" >= 0, leaving up to 16 tail bytes. }

@16x_Loop:
    movdqu xmm1, xmmword ptr [set1 + size]
    movdqu xmm0, xmmword ptr [set2 + size]
    pandn  xmm0, xmm1
    pcmpeqb xmm0, xmm2
    pmovmskb eax, xmm0
    inc    ax
    jnz    @No
    add    size, 16
    js     @16x_Loop

    movdqu xmm1, xmmword ptr [set1]
    movdqu xmm0, xmmword ptr [set2]
    pandn  xmm0, xmm1
{$else}
{ Folds all 16-byte "set1 and not set2" chunks with OR and checks the final result for zero. Better for small enough sets. }
    movdqu xmm1, xmmword ptr [set1]
    movdqu xmm2, xmmword ptr [set2]
    pandn  xmm2, xmm1

@16x_Loop:
    movdqu xmm1, xmmword ptr [set1 + size]
    movdqu xmm0, xmmword ptr [set2 + size]
    pandn  xmm0, xmm1
    por    xmm2, xmm0
    sub    size, 16
    ja     @16x_Loop

    pxor   xmm0, xmm0
{$endif}
    pcmpeqb xmm0, xmm2
    pmovmskb ecx, xmm0
    xor    eax, eax
    inc    cx
    setz   al
    ret

@No:
    xor    eax, eax
    ret

@Bytewise_Prepare:
    add    size, 16
    neg    size
    sub    set1, size
    sub    set2, size
@Bytewise_Loop:
    movzx  eax, byte ptr [set2 + size]
    not    eax
    test   byte ptr [set1 + size], al
    jnz    @No
    inc    size
    jnz    @Bytewise_Loop
    mov    eax, $1
end;
{$define FPC_SYSTEM_HAS_FPC_VARSET_CONTAINS_SET}
{$endif ndef FPC_SYSTEM_HAS_FPC_VARSET_CONTAINS_SET}

{$asmmode att}
