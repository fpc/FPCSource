{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Include file with set operations called by the compiler

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifndef FPC_SYSTEM_HAS_FPC_VARSET_ADD_SETS}
label
  fpc_varset_add_sets_plain_fallback;

procedure fpc_varset_add_sets_plain(const set1,set2; var dest;size : ptrint); assembler; nostackframe;
{ eax = set1, edx = set2, ecx = dest, [esp + 4] = size }
asm
    push   %ebx
fpc_varset_add_sets_plain_fallback:
    push   %esi
    mov    12(%esp), %esi { esi = size }
    sub    $4, %esi
    jl     .LBytewise_Prepare { probably dead branch... }

.L4x_Loop:
    mov    (%eax,%esi), %ebx
    or     (%edx,%esi), %ebx
    mov    %ebx, (%ecx,%esi)
    sub    $4, %esi
    ja     .L4x_Loop

    mov    (%eax), %ebx { Tail, just in case (if size is always divisible by 4, 4x_Loop can be altered to handle everything instead). }
    or     (%edx), %ebx
    mov    %ebx, (%ecx)
    pop    %esi
    pop    %ebx
    ret    $4

.LBytewise_Prepare:
    add    $3, %esi
.LBytewise_Loop:
    movzbl (%eax,%esi), %ebx
    or     (%edx,%esi), %bl
    mov    %bl, (%ecx,%esi)
    sub    $1, %esi
    jae    .LBytewise_Loop
    pop    %esi
    pop    %ebx
end;

procedure fpc_varset_add_sets_sse(const set1,set2; var dest;size : ptrint); assembler; nostackframe;
{ eax = set1, edx = set2, ecx = dest, [esp + 4] = size }
asm
    push   %ebx
    mov    8(%esp), %ebx
    sub    $16, %ebx { ebx = position }
    jl     fpc_varset_add_sets_plain_fallback { probably dead branch... }

.L16x_Loop:
    movups (%eax,%ebx), %xmm0
    movups (%edx,%ebx), %xmm1
    orps   %xmm1, %xmm0
    movups %xmm0, (%ecx,%ebx)
    sub    $16, %ebx
    ja     .L16x_Loop

    movups (%eax), %xmm0 { Tail, just in case (if size is always divisible by 16, 16x_Loop can be altered to handle everything instead). }
    movups (%edx), %xmm1
    orps   %xmm1, %xmm0
    movups %xmm0, (%ecx)
    pop    %ebx
end;

procedure fpc_varset_add_sets_dispatch(const set1,set2; var dest;size : ptrint); forward;

var
  fpc_varset_add_sets_impl: procedure(const set1,set2; var dest;size : ptrint) = @fpc_varset_add_sets_dispatch;

procedure fpc_varset_add_sets_dispatch(const set1,set2; var dest;size : ptrint);
begin
  if has_sse_support then
    fpc_varset_add_sets_impl:=@fpc_varset_add_sets_sse
  else
    fpc_varset_add_sets_impl:=@fpc_varset_add_sets_plain;
  fpc_varset_add_sets_impl(set1,set2,dest,size);
end;

procedure fpc_varset_add_sets(const set1,set2; var dest;size : ptrint); compilerproc; inline;
begin
  fpc_varset_add_sets_impl(set1,set2,dest,size);
end;
{$define FPC_SYSTEM_HAS_FPC_VARSET_ADD_SETS}
{$endif ndef FPC_SYSTEM_HAS_FPC_VARSET_ADD_SETS}

{$ifndef FPC_SYSTEM_HAS_FPC_VARSET_MUL_SETS}
label
  fpc_varset_mul_sets_plain_fallback;

procedure fpc_varset_mul_sets_plain(const set1,set2; var dest;size : ptrint); assembler; nostackframe;
{ Same as fpc_varset_add_sets_plain but with 'and' instead of 'or'. }
asm
    push   %ebx
fpc_varset_mul_sets_plain_fallback:
    push   %esi
    mov    12(%esp), %esi { esi = size }
    sub    $4, %esi
    jl     .LBytewise_Prepare { probably dead branch... }

.L4x_Loop:
    mov    (%eax,%esi), %ebx
    and    (%edx,%esi), %ebx
    mov    %ebx, (%ecx,%esi)
    sub    $4, %esi
    ja     .L4x_Loop

    mov    (%eax), %ebx { Tail, just in case (if size is always divisible by 4, 4x_Loop can be altered to handle everything instead). }
    and    (%edx), %ebx
    mov    %ebx, (%ecx)
    pop    %esi
    pop    %ebx
    ret    $4

.LBytewise_Prepare:
    add    $3, %esi
.LBytewise_Loop:
    movzbl (%eax,%esi), %ebx
    and    (%edx,%esi), %bl
    mov    %bl, (%ecx,%esi)
    sub    $1, %esi
    jae    .LBytewise_Loop
    pop    %esi
    pop    %ebx
end;

procedure fpc_varset_mul_sets_sse(const set1,set2; var dest;size : ptrint); assembler; nostackframe;
{ Same as fpc_varset_add_sets_sse but with 'and' instead of 'or'. }
asm
    push   %ebx
    mov    8(%esp), %ebx
    sub    $16, %ebx { ebx = position }
    jl     fpc_varset_mul_sets_plain_fallback { probably dead branch... }

.L16x_Loop:
    movups (%eax,%ebx), %xmm0
    movups (%edx,%ebx), %xmm1
    andps  %xmm1, %xmm0
    movups %xmm0, (%ecx,%ebx)
    sub    $16, %ebx
    ja     .L16x_Loop

    movups (%eax), %xmm0 { Tail, just in case (if size is always divisible by 16, 16x_Loop can be altered to handle everything instead). }
    movups (%edx), %xmm1
    andps   %xmm1, %xmm0
    movups %xmm0, (%ecx)
    pop    %ebx
end;

procedure fpc_varset_mul_sets_dispatch(const set1,set2; var dest;size : ptrint); forward;

var
  fpc_varset_mul_sets_impl: procedure(const set1,set2; var dest;size : ptrint) = @fpc_varset_mul_sets_dispatch;

procedure fpc_varset_mul_sets_dispatch(const set1,set2; var dest;size : ptrint);
begin
  if has_sse_support then
    fpc_varset_mul_sets_impl:=@fpc_varset_mul_sets_sse
  else
    fpc_varset_mul_sets_impl:=@fpc_varset_mul_sets_plain;
  fpc_varset_mul_sets_impl(set1,set2,dest,size);
end;

procedure fpc_varset_mul_sets(const set1,set2; var dest;size : ptrint); compilerproc; inline;
begin
  fpc_varset_mul_sets_impl(set1,set2,dest,size);
end;
{$define FPC_SYSTEM_HAS_FPC_VARSET_MUL_SETS}
{$endif ndef FPC_SYSTEM_HAS_FPC_VARSET_MUL_SETS}

{$ifndef FPC_SYSTEM_HAS_FPC_VARSET_SUB_SETS}
label
  fpc_varset_sub_sets_plain_fallback;

procedure fpc_varset_sub_sets_plain(const set1,set2; var dest;size : ptrint); assembler; nostackframe;
{ eax = set1, edx = set2, ecx = dest, [esp + 4] = size }
asm
    push   %ebx
fpc_varset_sub_sets_plain_fallback:
    push   %esi
    mov    12(%esp), %esi { esi = size }
    sub    $4, %esi
    jl     .LBytewise_Prepare { probably dead branch... }

    mov    (%edx), %ebx { Tail, just in case (if size is always divisible by 4, 4x_Loop can be altered to handle everything instead). }
    not    %ebx         { Precalculated because operation is not idempotent and dest can be equal to set1/set2. }
    and    (%eax), %ebx
    push   %ebx
.L4x_Loop:
    mov    (%edx,%esi), %ebx
    not    %ebx
    and    (%eax,%esi), %ebx
    mov    %ebx, (%ecx,%esi)
    sub    $4, %esi
    ja     .L4x_Loop

    pop    %ebx
    mov    %ebx, (%ecx) { Write precalculated tail. }
    pop    %esi
    pop    %ebx
    ret    $4

.LBytewise_Prepare:
    add    $3, %esi
.LBytewise_Loop:
    movzbl (%edx,%esi), %ebx
    not    %ebx
    and    (%eax,%esi), %bl
    mov    %bl, (%ecx,%esi)
    sub    $1, %esi
    jae    .LBytewise_Loop
    pop    %esi
    pop    %ebx
end;

procedure fpc_varset_sub_sets_sse(const set1,set2; var dest;size : ptrint); assembler; nostackframe;
{ eax = set1, edx = set2, ecx = dest, [esp + 4] = size }
asm
    push   %ebx
    mov    8(%esp), %ebx
    sub    $16, %ebx { ebx = position }
    jl     fpc_varset_sub_sets_plain_fallback { probably dead branch... }

    movups (%eax), %xmm1 { Tail, just in case (if size is always divisible by 16, 16x_Loop can be altered to handle everything instead). }
    movups (%edx), %xmm2 { Precalculated because operation is not idempotent and dest can be equal to set1/set2. }
    andnps %xmm1, %xmm2

.L16x_Loop:
    movups (%eax,%ebx), %xmm1
    movups (%edx,%ebx), %xmm0
    andnps %xmm1, %xmm0
    movups %xmm0, (%ecx,%ebx)
    sub    $16, %ebx
    ja     .L16x_Loop

    movups %xmm2, (%ecx) { Write precalculated tail. }
    pop    %ebx
end;

procedure fpc_varset_sub_sets_dispatch(const set1,set2; var dest;size : ptrint); forward;

var
  fpc_varset_sub_sets_impl: procedure(const set1,set2; var dest;size : ptrint) = @fpc_varset_sub_sets_dispatch;

procedure fpc_varset_sub_sets_dispatch(const set1,set2; var dest;size : ptrint);
begin
  if has_sse_support then
    fpc_varset_sub_sets_impl:=@fpc_varset_sub_sets_sse
  else
    fpc_varset_sub_sets_impl:=@fpc_varset_sub_sets_plain;
  fpc_varset_sub_sets_impl(set1,set2,dest,size);
end;

procedure fpc_varset_sub_sets(const set1,set2; var dest;size : ptrint); compilerproc; inline;
begin
  fpc_varset_sub_sets_impl(set1,set2,dest,size);
end;
{$define FPC_SYSTEM_HAS_FPC_VARSET_SUB_SETS}
{$endif ndef FPC_SYSTEM_HAS_FPC_VARSET_SUB_SETS}

{$ifndef FPC_SYSTEM_HAS_FPC_VARSET_SYMDIF_SETS}
label
  fpc_varset_symdif_sets_plain_fallback;

procedure fpc_varset_symdif_sets_plain(const set1,set2; var dest;size : ptrint); assembler; nostackframe;
{ Same as fpc_varset_sub_sets_plain but with 'xor' instead of 'and not'.
  eax = set1, edx = set2, ecx = dest, [esp + 4] = size }
asm
    push   %ebx
fpc_varset_symdif_sets_plain_fallback:
    push   %esi
    mov    12(%esp), %esi { esi = size }
    sub    $4, %esi
    jl     .LBytewise_Prepare { probably dead branch... }

    mov    (%eax), %ebx { Tail, just in case (if size is always divisible by 4, 4x_Loop can be altered to handle everything instead). }
    xor    (%edx), %ebx { Precalculated because operation is not idempotent and dest can be equal to set1/set2. }
    push   %ebx
.L4x_Loop:
    mov    (%eax,%esi), %ebx
    xor    (%edx,%esi), %ebx
    mov    %ebx, (%ecx,%esi)
    sub    $4, %esi
    ja     .L4x_Loop

    pop    %ebx
    mov    %ebx, (%ecx) { Write precalculated tail. }
    pop    %esi
    pop    %ebx
    ret    $4

.LBytewise_Prepare:
    add    $3, %esi
.LBytewise_Loop:
    movzbl (%eax,%esi), %ebx
    xor    (%edx,%esi), %bl
    mov    %bl, (%ecx,%esi)
    sub    $1, %esi
    jae    .LBytewise_Loop
    pop    %esi
    pop    %ebx
end;

procedure fpc_varset_symdif_sets_sse(const set1,set2; var dest;size : ptrint); assembler; nostackframe;
{ Same as fpc_varset_sub_sets_sse but with 'xor' instead of 'and not'.
  eax = set1, edx = set2, ecx = dest, [esp + 4] = size }
asm
    push   %ebx
    mov    8(%esp), %ebx
    sub    $16, %ebx { ebx = position }
    jl     fpc_varset_symdif_sets_plain_fallback { probably dead branch... }

    movups (%eax), %xmm1 { Tail, just in case (if size is always divisible by 16, 16x_Loop can be altered to handle everything instead). }
    movups (%edx), %xmm2 { Precalculated because operation is not idempotent and dest can be equal to set1/set2. }
    xorps  %xmm1, %xmm2

.L16x_Loop:
    movups (%eax,%ebx), %xmm1
    movups (%edx,%ebx), %xmm0
    xorps  %xmm1, %xmm0
    movups %xmm0, (%ecx,%ebx)
    sub    $16, %ebx
    ja     .L16x_Loop

    movups %xmm2, (%ecx) { Write precalculated tail. }
    pop    %ebx
end;

procedure fpc_varset_symdif_sets_dispatch(const set1,set2; var dest;size : ptrint); forward;

var
  fpc_varset_symdif_sets_impl: procedure(const set1,set2; var dest;size : ptrint) = @fpc_varset_symdif_sets_dispatch;

procedure fpc_varset_symdif_sets_dispatch(const set1,set2; var dest;size : ptrint);
begin
  if has_sse_support then
    fpc_varset_symdif_sets_impl:=@fpc_varset_symdif_sets_sse
  else
    fpc_varset_symdif_sets_impl:=@fpc_varset_symdif_sets_plain;
  fpc_varset_symdif_sets_impl(set1,set2,dest,size);
end;

procedure fpc_varset_symdif_sets(const set1,set2; var dest;size : ptrint); compilerproc; inline;
begin
  fpc_varset_symdif_sets_impl(set1,set2,dest,size);
end;
{$define FPC_SYSTEM_HAS_FPC_VARSET_SYMDIF_SETS}
{$endif ndef FPC_SYSTEM_HAS_FPC_VARSET_SYMDIF_SETS}

{$ifndef FPC_SYSTEM_HAS_FPC_VARSET_CONTAINS_SET}
function fpc_varset_contains_sets_plain(const set1,set2;size : ptrint):boolean; assembler; nostackframe;
{ eax = set1, edx = set2, ecx = size }
asm
    push   %ebx
    sub    $4, %ecx
    jl     .LBytewise_Prepare { probably dead branch... }
    add    %ecx, %eax
    add    %ecx, %edx
    neg    %ecx { Now ecx = -(size - 4), eax points to set1 + size - 4, edx points to set2 + size - 4. Loop ends on size >= 0, leaving up to 4 tail bytes. }

.L4x_Loop:
    mov    (%edx,%ecx), %ebx
    not    %ebx
    test   %ebx, (%eax,%ecx)
    jnz    .LNo
    add    $4, %ecx
    js     .L4x_Loop

    mov    (%edx), %ebx { Tail. }
    not    %ebx
    mov    %eax, %ecx { eax value is still required to access set1 tail, but eax is going to be xor-zeroed for setz. }
    xor    %eax, %eax
    test   %ebx, (%ecx)
    setz   %al
    pop    %ebx
    ret

.LNo:
    xor    %eax, %eax
    pop    %ebx
    ret

.LBytewise_Prepare:
    add    $4, %ecx
    neg    %ecx
    sub    %ecx, %eax
    sub    %ecx, %edx
.LBytewise_Loop:
    movzbl (%edx,%ecx), %ebx
    not    %ebx
    test   %bl, (%eax,%ecx)
    jnz    .LNo
    inc    %ecx
    jnz    .LBytewise_Loop
    mov    $1, %eax
    pop    %ebx
end;

function fpc_varset_contains_sets_sse2(const set1,set2;size : ptrint):boolean; assembler; nostackframe;
{ eax = set1, edx = set2, ecx = size }
asm
    sub    $16, %ecx
    jl     .LFallback  { probably dead branch... }

{ Folds all 16-byte "set1 and not set2" chunks with OR and checks the final result for zero. Better for small enough sets. }
    movdqu (%eax), %xmm1
    movdqu (%edx), %xmm2
    pandn  %xmm1, %xmm2

.L16x_Loop:
    movdqu (%eax,%ecx), %xmm1
    movdqu (%edx,%ecx), %xmm0
    pandn  %xmm1, %xmm0
    por    %xmm0, %xmm2
    sub    $16, %ecx
    ja     .L16x_Loop

    pxor   %xmm0, %xmm0
    pcmpeqb %xmm2,%xmm0
    pmovmskb %xmm0, %ecx
    xor    %eax, %eax
    inc    %cx
    setz   %al
    ret

.LFallback:
    add    $16, %ecx
    jmp    fpc_varset_contains_sets_plain
end;

function fpc_varset_contains_sets_dispatch(const set1,set2;size : ptrint):boolean; forward;

var
  fpc_varset_contains_sets_impl: function(const set1,set2;size : ptrint):boolean = @fpc_varset_contains_sets_dispatch;

function fpc_varset_contains_sets_dispatch(const set1,set2;size : ptrint):boolean;
begin
  if has_sse2_support then
    fpc_varset_contains_sets_impl:=@fpc_varset_contains_sets_sse2
  else
    fpc_varset_contains_sets_impl:=@fpc_varset_contains_sets_plain;
  result:=fpc_varset_contains_sets_impl(set1,set2,size);
end;

function fpc_varset_contains_sets(const set1,set2;size : ptrint):boolean; compilerproc; inline;
begin
  result:=fpc_varset_contains_sets_impl(set1,set2,size);
end;
{$define FPC_SYSTEM_HAS_FPC_VARSET_CONTAINS_SET}
{$endif ndef FPC_SYSTEM_HAS_FPC_VARSET_CONTAINS_SET}

