{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Florian Klaempfl and Sergei Gorelkin
    Members of the Free Pascal development team

    Processor dependent implementation for the system unit for
    the x86-64 architecture

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$asmmode GAS}

{****************************************************************************
                               Primitives
****************************************************************************}

{$define FPC_SYSTEM_HAS_SPTR}
Function Sptr : Pointer;assembler;nostackframe;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
        movq    %rsp,%rax
end;

{$IFNDEF INTERNAL_BACKTRACE}
{$define FPC_SYSTEM_HAS_GET_FRAME}
function get_frame:pointer;assembler;nostackframe;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
        movq    %rbp,%rax
end;
{$ENDIF not INTERNAL_BACKTRACE}

{$define FPC_SYSTEM_HAS_GET_PC_ADDR}
function get_pc_addr:pointer;assembler;nostackframe;
asm
        movq    (%rsp),%rax
end;

{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:pointer;addr:pointer=nil):pointer;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  get_caller_addr:=framebp;
  if assigned(framebp) then
    get_caller_addr:=PPointer(framebp)[1];
end;


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer;addr:pointer=nil):pointer;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  get_caller_frame:=framebp;
  if assigned(framebp) then
    get_caller_frame:=PPointer(framebp)^;
end;

// The following assembler procedures are disabled for FreeBSD due to
// multiple issues with its old GNU assembler (Mantis #19188).
// Even after fixing them, it can be enabled only for the trunk version,
// otherwise bootstrapping won't be possible.
// Modified to use oldbinutils as in cpu.pp source, to allow easier use for other targets.
{$ifdef freebsd}
  {$ifndef overridebinutils}
    {$define oldbinutils}
  {$endif}
{$endif freebsd}

{$ifndef oldbinutils}


{$ifndef FPC_SYSTEM_HAS_MOVE}
{$define FPC_SYSTEM_HAS_MOVE}
procedure Move(const source;var dest;count:SizeInt);[public, alias: 'FPC_MOVE'];assembler;nostackframe;
{ Linux: rdi source, rsi dest, rdx count
  win64: rcx source, rdx dest, r8 count }
asm
{$ifndef win64}
    mov    %rdx, %r8
    mov    %rsi, %rdx
    mov    %rdi, %rcx
{$endif win64}

    cmp    $3, %r8
    jle    .L3OrLess
    cmp    $8, %r8
    jle    .L4to8
    cmp    $16, %r8
    jle    .L9to16
    movdqu (%rcx), %xmm4         { First and last 16 bytes, used both in .L33OrMore and 17–32 branch. }
    movdqu -16(%rcx,%r8), %xmm5
    cmp    $32, %r8
    jg     .L33OrMore
    movdqu %xmm4, (%rdx)         { 17–32 bytes }
    movdqu %xmm5, -16(%rdx,%r8)
    ret

    .balign 16
.L3OrLess:
    cmp    $1, %r8
    jl     .LZero
    movzbl (%rcx), %eax
    je     .LOne
    movzwl -2(%rcx,%r8), %r9d
    mov    %r9w, -2(%rdx,%r8)
.LOne:
    mov    %al, (%rdx)
.LZero:
    ret

.L4to8:
    mov    (%rcx), %eax
    mov    -4(%rcx,%r8), %r9d
    mov    %eax, (%rdx)
    mov    %r9d, -4(%rdx,%r8)
    ret

.L9to16:
    mov    (%rcx), %rax
    mov    -8(%rcx,%r8), %r9
    mov    %rax, (%rdx)
    mov    %r9, -8(%rdx,%r8)
.Lquit:
    ret
    .byte  0x90,0x90,0x90        { Turns .balign 16 before .Lloop32f into a no-op. }

.L33OrMore:
    sub    %rdx, %rcx            { rcx = src - dest }
    jz     .Lquit                { exit if src=dest }
    jnb    .LForward             { src>dest => forward move }

    mov    %r8, %rax
    add    %rcx, %rax            { rcx is negative => r8+rcx > 0 if regions overlap }
    jb     .Lback                { if no overlap, still do forward move }

.LForward:
    mov    %rdx, %r9             { remember original dest to write first 16 bytes }
    add    %rdx, %r8             { Move dest to the next 16-byte boundary. +16 if already aligned, as first 16 bytes will be writen separately anyway. }
    add    $16, %rdx
    and    $-16, %rdx
    sub    %rdx, %r8

.LRestAfterNTf:
    sub    $32, %r8              { During the N× loop, r8 is N bytes less than actually remained to allow sub N+jae .LLoop instead of sub N+cmp N+jae .LLoop. }
    jbe    .LPost32f
    cmp    $0x40000, %r8         { this limit must be processor-specific (1/2 L2 cache size) }
    jae    .Lntf                 { might jump back right away after more checks, but the branch is taken only on huge moves so it's better to take these checks out of here... }

    .balign 16                   { no-op }
.Lloop32f:
    movdqu (%rcx,%rdx), %xmm0
    movdqa %xmm0, (%rdx)
    movdqu 16(%rcx,%rdx), %xmm0
    movdqa %xmm0, 16(%rdx)
    add    $32, %rdx
    sub    $32, %r8
    ja     .Lloop32f

.LPost32f:                       { +32 fixup not applied after 32× loop, r8 = remaining - 32 here. }
    cmp    $-16, %r8
    jle    .LFirstAndLast16f
    movdqu (%rcx,%rdx), %xmm0
    movdqa %xmm0, (%rdx)
.LFirstAndLast16f:
    movdqu %xmm5, 16(%rdx,%r8)   { Write first and last 16 bytes after everything else. }
    movdqu %xmm4, (%r9)          { Important for <16-byte step between src and dest. }
    ret

.Lntf:
    cmp    $0x1000, %rcx         { Maybe change mind: don't bother bypassing cache if src and dest are close to each other }
    jb     .Lloop32f             { (this check is performed here to not stand in the way of smaller counts) }
    sub    $0xFE0, %r8           { r8 = remaining - 0x1000, but 32 was subtracted already, so must subtract only (0x1000 - 32) = 0xFE0. }

.Lntloopf:
    mov    $32, %eax

    .balign 16
.Lpref:
    prefetchnta (%rcx,%rdx,1)
    prefetchnta 0x40(%rcx,%rdx,1)
    add    $0x80, %rdx
    dec    %eax
    jnz    .Lpref

    sub    $0x1000, %rdx
    mov    $64, %eax

    .balign 16
.Lntloop64f:
    add    $64, %rdx
    movdqu -64(%rcx,%rdx,1), %xmm0
    movntdq %xmm0, -64(%rdx)
    movdqu -48(%rcx,%rdx,1), %xmm0
    movntdq %xmm0, -48(%rdx)
    movdqu -32(%rcx,%rdx,1), %xmm0
    movntdq %xmm0, -32(%rdx)
    movdqu -16(%rcx,%rdx,1), %xmm0
    movntdq %xmm0, -16(%rdx)
    dec    %eax
    jnz    .Lntloop64f

    sub    $0x1000, %r8
    jae    .Lntloopf

    mfence
    add    $0x1000, %r8
    jmpq   .LRestAfterNTf        { go handle remaining bytes }
    .byte  0x90,0x90,0x90        { Turns .balign 16 before .Lloop32b into a no-op. }

{ backwards move }
.Lback:
    lea    (%rdx,%r8), %r9       { points to the end of dest; remember to write last 16 bytes }
    lea    -1(%r9), %r8          { move dest to the previous 16-byte boundary... }
    and    $-16, %r8
    sub    %rdx, %r8
    add    %r8, %rdx

.LRestAfterNTb:
    sub    $32, %r8
    jbe    .LPost32b
    cmp    $0x40000, %r8
    jae    .Lntb

    .balign 16                   { no-op }
.Lloop32b:
    sub    $32, %rdx
    movdqu 16(%rcx,%rdx), %xmm0
    movdqa %xmm0, 16(%rdx)
    movdqu (%rcx,%rdx), %xmm0
    movdqa %xmm0, (%rdx)
    sub    $32, %r8
    ja     .Lloop32b

.LPost32b:
    cmp    $-16, %r8
    jle    .LFirstAndLast16b
    movdqu -16(%rcx,%rdx), %xmm0
    movdqa %xmm0, -16(%rdx)
.LFirstAndLast16b:
    sub    %r8, %rdx
    movdqu %xmm4, -32(%rdx)
    movdqu %xmm5, -16(%r9)
    ret

.Lntb:
    cmp    $0xfffffffffffff000,%rcx
    jnb    .Lloop32b
    sub    $0xFE0, %r8

.Lntloopb:
    mov    $32, %eax

    .balign 16
.Lprefb:
    sub    $0x80, %rdx
    prefetchnta (%rcx,%rdx,1)
    prefetchnta 0x40(%rcx,%rdx,1)
    dec    %eax
    jnz    .Lprefb

    add    $0x1000, %rdx
    mov    $0x40, %eax

    .balign 16
.Lntloop64b:
    sub    $64, %rdx
    movdqu 48(%rcx,%rdx,1), %xmm0
    movntdq %xmm0, 48(%rdx)
    movdqu 32(%rcx,%rdx,1), %xmm0
    movntdq %xmm0, 32(%rdx)
    movdqu 16(%rcx,%rdx,1), %xmm0
    movntdq %xmm0, 16(%rdx)
    movdqu (%rcx,%rdx,1), %xmm0
    movntdq %xmm0, (%rdx)
    dec    %eax
    jnz    .Lntloop64b

    sub    $0x1000, %r8
    jae    .Lntloopb

    mfence
    add    $0x1000, %r8
    jmpq   .LRestAfterNTb
end;
{$endif FPC_SYSTEM_HAS_MOVE}

{$ifndef FPC_SYSTEM_HAS_FILLCHAR}
{$define FPC_SYSTEM_HAS_FILLCHAR}
Procedure FillChar(var x;count:SizeInt;value:byte);assembler;nostackframe;
  asm
{ win64: rcx dest, rdx count, r8b value
  linux: rdi dest, rsi count, rdx value }
{$ifndef win64}
    mov    %rdx, %r8
    mov    %rsi, %rdx
    mov    %rdi, %rcx
{$endif win64}

    mov    $0x01010101, %r9d
    movzbl %r8b, %eax
    imul   %r9d, %eax

    cmp    $16, %rdx
    jge    .LVecOrMore
    cmp    $3, %rdx
    jle    .L3OrLess

    mov    %eax, (%rcx)
    cmp    $8, %edx
    jle    .LLast4
    mov    %eax, 4(%rcx)
    mov    %eax, -8(%rcx,%rdx)
.LLast4:
    mov    %eax, -4(%rcx,%rdx)
    ret

.L3OrLess:
    test   %rdx, %rdx
    jle    .LQuit
    mov    %al, (%rcx)
    mov    %al, -1(%rcx,%rdx)
    shr    $1, %edx
    mov    %al, (%rcx,%rdx)
.LQuit:
    ret

.balign 16
.LVecOrMore:
    movd   %eax, %xmm0
    pshufd $0, %xmm0, %xmm0

    { x can start and end aligned or misaligned on the vector boundary:

      x = [UH][H1][H2][...][T2][T1]
      x = UH][H1][H2][...][T2][T1][UT

      UH (“unaligned head”) is written, potentially overlapping with H1, with the 'movdqu'. Has 1–16 bytes.
      H1 and so on are “heads”.
      T1 and so on are “tails”.
      UT (“unaligned tail”) is written with another 'movdqu' after the loop. Has 0–15 bytes. }

    movdqu %xmm0, (%rcx)
    lea    -64(%rcx,%rdx), %r8 { r8 = end of x - 64, loop bound }

    cmp    $32, %rdx
    jle    .LLastVec

    and    $-16, %rcx { align rcx to the LEFT (so needs to be offset by an additional +16 for a while). }
    movdqa %xmm0, 16(%rcx) { Write H1. }
    mov    %r8, %rax
    and    $-16, %rax { rax = “T4” (possibly fictive) = aligned r8. }
    cmp    $48, %rdx { 33~48 bytes might contain 1~2 heads+tails; write as H1 and T1. }
    jle    .LOneAlignedTailWrite
    movdqa %xmm0, 32(%rcx) { Write H2. }
    cmp    $80, %rdx  { 49~80 bytes might contain 2~4 heads+tails; write as H1–2 and T2–1. }
    jle    .LTwoAlignedTailWrites
    movdqa %xmm0, 48(%rcx) { Write H3. }
    cmp    $112, %rdx  { 81~112 bytes might contain 4~6 heads+tails; write as H1–3 and T3–1. }
    jle    .LThreeAlignedTailWrites

    add    $48, %rcx
    cmp    $0x80000, %rdx
    jae    .L64xNT_Body

.balign 16
.L64x_Body:
    movdqa %xmm0, (%rcx)
    movdqa %xmm0, 16(%rcx)
    movdqa %xmm0, 32(%rcx)
    movdqa %xmm0, 48(%rcx)
    add    $64, %rcx
    cmp    %r8, %rcx
    jb     .L64x_Body

.LLoopEnd:
    movdqa %xmm0, (%rax)
.LThreeAlignedTailWrites:
    movdqa %xmm0, 16(%rax)
.LTwoAlignedTailWrites:
    movdqa %xmm0, 32(%rax)
.LOneAlignedTailWrite:
    movdqa %xmm0, 48(%rax)
.LLastVec:
    movdqu %xmm0, 48(%r8)
    ret

.balign 16
.L64xNT_Body:
    movntdq %xmm0, (%rcx)
    movntdq %xmm0, 16(%rcx)
    movntdq %xmm0, 32(%rcx)
    movntdq %xmm0, 48(%rcx)
    add    $64, %rcx
    cmp    %r8, %rcx
    jb     .L64xNT_Body
    mfence
    jmp    .LLoopEnd
  end;
{$endif FPC_SYSTEM_HAS_FILLCHAR}

{$ifndef FPC_SYSTEM_HAS_INDEXBYTE}
{$define FPC_SYSTEM_HAS_INDEXBYTE}
function IndexByte(Const buf;len:SizeInt;b:byte):SizeInt; assembler; nostackframe;
{ win64: rcx buf, rdx len, r8b word
  linux: rdi buf, rsi len, rdx word }
asm
{$ifdef win64}
    movd   %r8d, %xmm1
{$else}
    movd   %edx, %xmm1
    movq   %rdi, %rcx
    movq   %rsi, %rdx
{$endif}
    mov    %rcx, %r8
    punpcklbw  %xmm1, %xmm1
    and    $-0x10, %rcx                { highest aligned address before buf }
    test   %rdx, %rdx
    punpcklbw  %xmm1, %xmm1
    jz     .Lnotfound                  { exit if len=0 }
    add    $16, %rcx                   { first aligned address after buf }
    pshufd $0, %xmm1, %xmm1
    movdqa -16(%rcx), %xmm0            { Fetch first 16 bytes (up to 15 bytes before target) }
    sub    %r8, %rcx                   { rcx=number of valid bytes, r8=original ptr }

    pcmpeqb %xmm1, %xmm0               { compare with pattern and get bitmask }
    pmovmskb %xmm0, %eax

    shl    %cl, %eax                   { shift valid bits into high word }
    and    $0xffff0000, %eax           { clear low word containing invalid bits }
    shr    %cl, %eax                   { shift back }
    jmp   .Lcontinue

    .balign 16
.Lloop:
    movdqa (%r8,%rcx), %xmm0           { r8 and rcx may have any values, }
    add    $16, %rcx                   { but their sum is evenly divisible by 16. }
    pcmpeqb %xmm1, %xmm0
    pmovmskb %xmm0, %eax
.Lcontinue:
    test   %eax, %eax
    jnz    .Lmatch
    cmp    %rcx, %rdx
    ja     .Lloop
.Lnotfound:
    or     $-1, %rax
    retq

.Lmatch:
    bsf    %eax, %eax
    lea    -16(%rcx,%rax), %rax
    cmp    %rax, %rdx                  { check against the buffer length }
    jbe    .Lnotfound
end;
{$endif FPC_SYSTEM_HAS_INDEXBYTE}

{$ifndef FPC_SYSTEM_HAS_INDEXWORD}
{$define FPC_SYSTEM_HAS_INDEXWORD}
function IndexWord(Const buf;len:SizeInt;b:word):SizeInt; assembler; nostackframe;
{ win64: rcx buf, rdx len, r8b word
  linux: rdi buf, rsi len, rdx word }
asm
{$ifdef win64}
    movd   %r8d, %xmm1
{$else}
    movd   %edx, %xmm1
    movq   %rdi, %rcx
    movq   %rsi, %rdx
{$endif}
    mov    %rcx, %r8
    punpcklwd  %xmm1, %xmm1
    and    $-0x10, %rcx
    test   %rdx, %rdx
    pshufd $0, %xmm1, %xmm1
    jz     .Lnotfound                  { exit if len=0 }
    add    $16, %rcx
    movdqa -16(%rcx), %xmm0            { Fetch first 16 bytes (up to 14 bytes before target) }
    sub    %r8, %rcx                   { rcx=number of valid bytes }

    test   $1, %r8b                    { if buffer isn't aligned to word boundary, }
    jnz    .Lunaligned                 { use a different algorithm }

    pcmpeqw  %xmm1, %xmm0
    pmovmskb %xmm0, %eax

    shl    %cl, %eax
    and    $0xffff0000, %eax
    shr    %cl, %eax
    shr    $1, %ecx                    { bytes->words }
    jmp    .Lcontinue

    .balign 16
.Lloop:
    movdqa (%r8,%rcx,2), %xmm0
    add    $8, %rcx
    pcmpeqw  %xmm1, %xmm0
    pmovmskb %xmm0, %eax
.Lcontinue:
    test   %eax, %eax
    jnz    .Lmatch
    cmp    %rcx, %rdx
    ja     .Lloop

.Lnotfound:
    or    $-1, %rax
    retq

.Lmatch:
    bsf    %eax, %eax
    shr    $1, %eax                    { in words }
    lea    -8(%rcx,%rax), %rax
    cmp    %rax, %rdx
    jbe    .Lnotfound                  { if match is after the specified length, ignore it }
    retq

.Lunaligned:
    movdqa  %xmm1, %xmm2               { (mis)align the pattern (in this particular case: }
    psllw   $8, %xmm1                  {   swap bytes of each word of pattern) }
    psrlw   $8, %xmm2
    por     %xmm2, %xmm1

    pcmpeqb  %xmm1, %xmm0
    pmovmskb %xmm0, %eax

    shl    %cl, %eax
    and    $0xffff0000, %eax
    shr    %cl, %eax

    add    %rdx, %rdx                  { length words -> bytes }
    xor    %r10d, %r10d                { nothing to merge yet }
    jmp    .Lcontinue_u

    .balign 16
.Lloop_u:
    movdqa (%r8,%rcx), %xmm0
    add    $16, %rcx
    pcmpeqb %xmm1, %xmm0               { compare by bytes }
    shr    $16, %r10d                  { bit 16 shifts into 0 }
    pmovmskb %xmm0, %eax
.Lcontinue_u:
    shl    $1, %eax                    { 15:0 -> 16:1 }
    or     %r10d, %eax                 { merge bit 0 from previous round }
    mov    %eax, %r10d
    shr    $1, %eax                    { now AND together adjacent pairs of bits }
    and    %r10d, %eax
    and    $0x5555, %eax               { also reset odd bits }
    jnz    .Lmatch_u
    cmpq   %rcx, %rdx
    ja     .Lloop_u

.Lnotfound_u:
    or     $-1, %rax
    retq
.Lmatch_u:
    bsf    %eax, %eax
    lea    -16(%rcx,%rax), %rax
    cmp    %rax, %rdx
    jbe    .Lnotfound_u                { if match is after the specified length, ignore it }
    sar    $1, %rax                    { in words }
end;
{$endif FPC_SYSTEM_HAS_INDEXWORD}

{$ifndef FPC_SYSTEM_HAS_INDEXDWORD}
{$define FPC_SYSTEM_HAS_INDEXDWORD}
function IndexDWord(Const buf;len:SizeInt;b:dword):SizeInt; assembler; nostackframe;
asm
{$ifndef win64}
    mov      %rdx, %r8
    mov      %rsi, %rdx
    mov      %rdi, %rcx
{$endif}
    mov      %rcx, %rax
    mov      %rdx, %r9
    shr      $61, %r9
    jnz      .LUnbounded
    lea      (%rcx,%rdx,4), %r10
    cmp      $3, %rdx
    jle      .LDWordwise_Test
    movd     %r8d, %xmm1
    pshufd   $0, %xmm1, %xmm1
    and      $-4, %rdx
    lea      (%rcx,%rdx,4), %r9

.balign 16
.L4x_Body:
    movdqu   (%rax), %xmm0
    pcmpeqd  %xmm1, %xmm0
    pmovmskb %xmm0, %edx
    test     %edx, %edx
    jnz      .L4x_Found
    add      $16, %rax
    cmp      %r9, %rax
    jne      .L4x_Body

    cmp      %r10, %rax
    je       .LNothing

    lea      -16(%r10), %rax
    movdqu   (%rax), %xmm0
    pcmpeqd  %xmm1, %xmm0
    pmovmskb %xmm0, %edx
    test     %edx, %edx
    jne      .L4x_Found
.LNothing:
    mov      $-1, %rax
    ret

.balign 16
.LDWordwise_Body:
    cmp      %r8d, (%rax)
    je       .LFound
    add      $4, %rax
.LDWordwise_Test:
    cmp      %r10, %rax
    jne      .LDWordwise_Body
    mov      $-1, %rax
    ret

.balign 16
.L4x_Found:
    bsf      %edx, %edx
    add      %rdx, %rax
.LFound:
    sub      %rcx, %rax
    shr      $2, %rax
    ret

.LUnbounded:
    mov      %rcx, %r10
    jmp      .LDWordwise_Body
end;
{$endif FPC_SYSTEM_HAS_INDEXDWORD}

{$endif freebsd}

{$ifndef FPC_SYSTEM_HAS_COMPAREBYTE}
{$define FPC_SYSTEM_HAS_COMPAREBYTE}
function CompareByte(Const buf1,buf2;len:SizeInt):SizeInt; assembler; nostackframe;
{ win64: rcx buf, rdx buf, r8 len
  linux: rdi buf, rsi buf, rdx len }
asm
{$ifndef win64}
    mov      %rdx, %r8
    mov      %rsi, %rdx
    mov      %rdi, %rcx
{$endif win64}
    lea      (%rcx,%r8), %r10 { r10 = buf1 end }
    cmp      $3, %r8
    jle      .LBytewise_Test
    mov      %r8, %r9
    and      $-16, %r9
    add      %rcx, %r9 { r9 = end of full XMMs in buf1 }
    cmp      %r9, %rcx
    jne      .L16x_Body
    lea      15(%r9), %eax { check if tails don't cross page boundaries and can be over-read to XMMs }
    lea      15(%rdx), %ecx
    xor      %r9d, %eax
    xor      %edx, %ecx
    or       %ecx, %eax
    cmp      $4095, %eax
    ja       .L4x_Prepare
    movdqu   (%r9), %xmm0
    movdqu   (%rdx), %xmm1
    pcmpeqb  %xmm1, %xmm0
    pmovmskb %xmm0, %eax
    xor      $65535, %eax
    jz       .L16x_Nothing
    bsf      %eax, %ecx
    add      %rcx, %r9
    cmp      %r10, %r9 { ignore over-read garbage bytes }
    jnb      .L16x_Nothing
    movzbl   (%r9), %eax
    movzbl   (%rdx,%rcx), %edx
    sub      %rdx, %rax
    ret

.balign 16
.L16x_Body:
    movdqu   (%rdx), %xmm0
    movdqu   (%rcx), %xmm1
    pcmpeqb  %xmm1, %xmm0
    pmovmskb %xmm0, %eax
    xor      $65535, %eax
    jnz      .L16x_Found
    add      $16, %rcx
    add      $16, %rdx
    cmp      %rcx, %r9
    jne      .L16x_Body

    cmp      %r9, %r10
    je       .L16x_Nothing

    sub      %rcx, %rdx
    lea      -16(%r10), %rcx
    add      %rcx, %rdx
    movdqu   (%rdx), %xmm0
    movdqu   (%rcx), %xmm1
    pcmpeqb  %xmm1, %xmm0
    pmovmskb %xmm0, %eax
    xor      $65535, %eax
    jnz      .L16x_Found
.L16x_Nothing:
    xor      %eax, %eax
    ret

.balign 16
.L16x_Found:
    bsf      %eax, %eax
    movzbl   (%rdx,%rax), %edx
    movzbl   (%rcx,%rax), %eax
    sub      %rdx, %rax
    ret

.L4x_Prepare:
    and      $12, %r8d { count to be handled with uint32s for 1 <= len <= 15: len mod 16 div 4 * 4 = len and %1100 = len and 12 }
    lea      (%r9,%r8), %rcx
    cmp      %rcx, %r9
    je       .LBytewise_Body
.L4x_Body:
    mov      (%r9), %eax
    mov      (%rdx), %r8d
    cmp      %r8d, %eax
    jne      .L4x_Found
    add      $4, %r9
    add      $4, %rdx
    cmp      %r9, %rcx
    jne      .L4x_Body
.LBytewise_Test:
    cmp      %r10, %rcx
    je       .LNothing
.LBytewise_Body:
    movzbl   (%rcx), %eax
    movzbl   (%rdx), %r8d
    sub      %r8, %rax
    jne      .LReturnRAX
    add      $1, %rcx
    add      $1, %rdx
    cmp      %r10, %rcx
    jne      .LBytewise_Body
.LNothing:
    xor      %eax, %eax
.LReturnRAX:
    ret

.L4x_Found:
    bswap    %r8d
    bswap    %eax
    sub      %r8, %rax
end;
{$endif FPC_SYSTEM_HAS_COMPAREBYTE}


{$ifndef FPC_SYSTEM_HAS_COMPAREWORD}
{$define FPC_SYSTEM_HAS_COMPAREWORD}
function CompareWord(Const buf1,buf2;len:SizeInt):SizeInt; assembler; nostackframe;
asm
{$ifndef win64}
    mov      %rdx, %r8
    mov      %rsi, %rdx
    mov      %rdi, %rcx
{$endif win64}
    mov      %r8, %rax
    shr      $62, %rax
    jnz      .LUnbounded
    lea      (%rcx,%r8,2), %r9
    cmp      $3, %r8
    jle      .LWordwise_Test
    and      $-8, %r8
    lea      (%rcx,%r8,2), %r8
    cmp      %r8, %rcx
    jne      .L8x_Body
    lea      15(%r8), %eax
    lea      15(%rdx), %ecx
    xor      %r8d, %eax
    xor      %edx, %ecx
    or       %ecx, %eax
    cmp      $4095, %eax
    ja       .LCantOverReadBoth
    movdqu   (%r8), %xmm0
    movdqu   (%rdx), %xmm2
    pcmpeqw  %xmm2, %xmm0
    pmovmskb %xmm0, %eax
    xor      $65535, %eax
    jz       .LNothing
    bsf      %eax, %eax
    add      %rax, %r8
    cmp      %r9, %r8
    jnb      .LNothing
    movzwl   (%r8), %ecx
    cmp      %cx, (%rdx,%rax)
    sbb      %rax, %rax
    and      $2, %eax
    sub      $1, %rax
    ret

.balign 16
.L8x_Body:
    movdqu   (%rdx), %xmm0
    movdqu   (%rcx), %xmm1
    pcmpeqw  %xmm1, %xmm0
    pmovmskb %xmm0, %eax
    xor      $65535, %eax
    jnz      .L8x_Found
    add      $16, %rcx
    add      $16, %rdx
    cmp      %rcx, %r8
    jne      .L8x_Body

    cmp      %r8, %r9
    je       .LNothing

    sub      %rcx, %rdx
    lea      -16(%r9), %rcx
    add      %rcx, %rdx
    movdqu   (%rdx), %xmm0
    movdqu   (%rcx), %xmm1
    pcmpeqw  %xmm1, %xmm0
    pmovmskb %xmm0, %eax
    xor      $65535, %eax
    jnz      .L8x_Found
.LNothing:
    xor      %eax, %eax
    ret

.balign 16
.L8x_Found:
    bsf      %eax, %eax
    movzwl   (%rcx,%rax), %ecx
    cmp      %cx, (%rdx,%rax)
.LDoSbb:
    sbb      %rax, %rax
    and      $2, %eax
    sub      $1, %rax
    ret

.LCantOverReadBoth:
    mov      %r8, %rcx
.LWordwise_Body:
    movzwl   (%rcx), %eax
    cmp      %ax, (%rdx)
    jne      .LDoSbb
.LWordwise_Next:
    add      $2, %rcx
    add      $2, %rdx
.LWordwise_Test:
    cmp      %r9, %rcx
    jne      .LWordwise_Body
    xor      %eax, %eax
    ret

.LUnbounded:
    mov     %rcx, %r9
    jmp     .LWordwise_Body
end;
{$endif FPC_SYSTEM_HAS_COMPAREWORD}


{$ifndef FPC_SYSTEM_HAS_COMPAREDWORD}
{$define FPC_SYSTEM_HAS_COMPAREDWORD}
function CompareDWord(Const buf1,buf2;len:SizeInt):SizeInt; assembler; nostackframe;
asm
{$ifndef win64}
    mov      %rdx, %r8
    mov      %rsi, %rdx
    mov      %rdi, %rcx
{$endif win64}
    mov      %r8, %rax
    shr      $61, %rax
    jnz      .LUnbounded
    cmp      $3, %r8
    lea      (%rcx,%r8,4), %r9
    jle      .LDwordwise_Test
    and      $-4, %r8
    lea      (%rcx,%r8,4), %r8
.balign 16
.L4x_Body:
    movdqu   (%rcx), %xmm1
    movdqu   (%rdx), %xmm0
    pcmpeqd  %xmm1, %xmm0
    pmovmskb %xmm0, %eax
    xor      $65535, %eax
    jnz      .L4x_Found
    add      $16, %rcx
    add      $16, %rdx
    cmp      %rcx, %r8
    jne      .L4x_Body

    cmp      %rcx, %r9
    je       .LNothing

    sub      %rcx, %rdx
    lea      -16(%r9), %rcx
    add      %rcx, %rdx
    movdqu   (%rcx), %xmm1
    movdqu   (%rdx), %xmm0
    pcmpeqd  %xmm1, %xmm0
    pmovmskb %xmm0, %eax
    xor      $65535, %eax
    jnz      .L4x_Found
.LNothing:
    xor      %eax, %eax
    ret

.balign 16
.L4x_Found:
    bsf      %eax, %eax
    mov      (%rcx,%rax), %ecx
    cmp      %ecx, (%rdx,%rax)
.LDoSbb:
    sbb      %rax, %rax
    and      $2, %eax
    sub      $1, %rax
    ret

.balign 16
.LDwordwise_Body:
    mov      (%rcx), %eax
    cmp      %eax, (%rdx)
    jne      .LDoSbb
    add      $4, %rcx
    add      $4, %rdx
.LDwordwise_Test:
    cmp      %rcx, %r9
    jne      .LDwordwise_Body
    xor      %eax, %eax
    ret

.LUnbounded:
    mov      %rcx, %r9
    jmp      .LDwordwise_Body
end;
{$endif FPC_SYSTEM_HAS_COMPAREDWORD}


{$define FPC_SYSTEM_HAS_DECLOCKED_LONGINT}
{ does a thread save inc/dec }
function declocked(var l : longint) : boolean;assembler; nostackframe;
  asm
     { this check should be done because a lock takes a lot }
     { of time!                                             }
{$ifdef FPC_PIC}
     movq       IsMultithread@GOTPCREL(%rip),%rax
     cmpl       $0,(%rax)
{$else FPC_PIC}
     cmpl       $0,IsMultithread(%rip)
{$endif FPC_PIC}
     jz         .Ldeclockedskiplock
     .byte      0xF0 // LOCK prefix.
.Ldeclockedskiplock:
     decl       {$ifdef win64} (%rcx) {$else} (%rdi) {$endif}
     setzb      %al
  end;


{$define FPC_SYSTEM_HAS_DECLOCKED_INT64}
function declocked(var l : int64) : boolean;assembler; nostackframe;
  asm
     { this check should be done because a lock takes a lot }
     { of time!                                             }
{$ifdef FPC_PIC}
     movq       IsMultithread@GOTPCREL(%rip),%rax
     cmpl       $0,(%rax)
{$else FPC_PIC}
     cmpl       $0,IsMultithread(%rip)
{$endif FPC_PIC}
     jz         .Ldeclockedskiplock
     .byte      0xF0 // LOCK prefix.
.Ldeclockedskiplock:
     decq       {$ifdef win64} (%rcx) {$else} (%rdi) {$endif}
     setzb      %al
  end;


{$define FPC_SYSTEM_HAS_INCLOCKED_LONGINT}
procedure inclocked(var l : longint);assembler; nostackframe;

  asm
     { this check should be done because a lock takes a lot }
     { of time!                                             }
{$ifdef FPC_PIC}
     movq       IsMultithread@GOTPCREL(%rip),%rax
     cmpl       $0,(%rax)
{$else FPC_PIC}
     cmpl       $0,IsMultithread(%rip)
{$endif FPC_PIC}
     jz         .Linclockedskiplock
     .byte      0xF0 // LOCK prefix.
.Linclockedskiplock:
     incl       {$ifdef win64} (%rcx) {$else} (%rdi) {$endif}
  end;


{$define FPC_SYSTEM_HAS_INCLOCKED_INT64}
procedure inclocked(var l : int64);assembler; nostackframe;

  asm
     { this check should be done because a lock takes a lot }
     { of time!                                             }
{$ifdef FPC_PIC}
     movq       IsMultithread@GOTPCREL(%rip),%rax
     cmpl       $0,(%rax)
{$else FPC_PIC}
     cmpl       $0,IsMultithread(%rip)
{$endif FPC_PIC}
     jz         .Linclockedskiplock
     .byte      0xF0 // LOCK prefix.
.Linclockedskiplock:
     incq       {$ifdef win64} (%rcx) {$else} (%rdi) {$endif}
  end;


function InterLockedDecrement (var Target: longint) : longint; assembler; nostackframe;
asm
{$ifdef win64}
        movq    %rcx,%rax
{$else win64}
        movq    %rdi,%rax
{$endif win64}
        movl    $-1,%edx
        xchgq   %rdx,%rax
        lock
        xaddl   %eax, (%rdx)
        decl    %eax
end;


function InterLockedIncrement (var Target: longint) : longint; assembler; nostackframe;
asm
{$ifdef win64}
        movq    %rcx,%rax
{$else win64}
        movq    %rdi,%rax
{$endif win64}
        movl    $1,%edx
        xchgq   %rdx,%rax
        lock
        xaddl   %eax, (%rdx)
        incl    %eax
end;


function InterLockedExchange (var Target: longint;Source : longint) : longint; assembler; nostackframe;
asm
{$ifdef win64}
        xchgl   (%rcx),%edx
        movl    %edx,%eax
{$else win64}
        xchgl   (%rdi),%esi
        movl    %esi,%eax
{$endif win64}
end;


function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint; assembler; nostackframe;
asm
{$ifdef win64}
        xchgq   %rcx,%rdx
        lock
        xaddl   %ecx, (%rdx)
        movl    %ecx,%eax
{$else win64}
        xchgq   %rdi,%rsi
        lock
        xaddl   %edi, (%rsi)
        movl    %edi,%eax
{$endif win64}
end;


function InterLockedCompareExchange(var Target: longint; NewValue, Comperand : longint): longint; assembler; nostackframe;
asm
{$ifdef win64}
        movl            %r8d,%eax
        lock
        cmpxchgl        %edx,(%rcx)
{$else win64}
        movl            %edx,%eax
        lock
        cmpxchgl        %esi,(%rdi)
{$endif win64}
end;


function InterLockedDecrement64 (var Target: int64) : int64; assembler; nostackframe;
asm
{$ifdef win64}
        movq    %rcx,%rax
{$else win64}
        movq    %rdi,%rax
{$endif win64}
        movq    $-1,%rdx
        xchgq   %rdx,%rax
        lock
        xaddq   %rax, (%rdx)
        decq    %rax
end;


function InterLockedIncrement64 (var Target: int64) : int64; assembler; nostackframe;
asm
{$ifdef win64}
        movq    %rcx,%rax
{$else win64}
        movq    %rdi,%rax
{$endif win64}
        movq    $1,%rdx
        xchgq   %rdx,%rax
        lock
        xaddq   %rax, (%rdx)
        incq    %rax
end;


function InterLockedExchange64 (var Target: int64;Source : int64) : int64; assembler; nostackframe;
asm
{$ifdef win64}
        xchgq   (%rcx),%rdx
        movq    %rdx,%rax
{$else win64}
        xchgq   (%rdi),%rsi
        movq    %rsi,%rax
{$endif win64}
end;


function InterLockedExchangeAdd64 (var Target: int64;Source : int64) : int64; assembler; nostackframe;
asm
{$ifdef win64}
        xchgq   %rcx,%rdx
        lock
        xaddq   %rcx, (%rdx)
        movq    %rcx,%rax
{$else win64}
        xchgq   %rdi,%rsi
        lock
        xaddq   %rdi, (%rsi)
        movq    %rdi,%rax
{$endif win64}
end;


function InterLockedCompareExchange64(var Target: int64; NewValue, Comperand : int64): int64; assembler; nostackframe;
asm
{$ifdef win64}
        movq            %r8,%rax
        lock
        cmpxchgq        %rdx,(%rcx)
{$else win64}
        movq            %rdx,%rax
        lock
        cmpxchgq        %rsi,(%rdi)
{$endif win64}
end;


{****************************************************************************
                                  FPU
****************************************************************************}

const
  { Internal constants for use in system unit }
  FPU_Invalid = 1;
  FPU_Denormal = 2;
  FPU_DivisionByZero = 4;
  FPU_Overflow = 8;
  FPU_Underflow = $10;
  FPU_StackUnderflow = $20;
  FPU_StackOverflow = $40;
  FPU_ExceptionMask = $ff;

  MM_Invalid = 1;
  MM_Denormal = 2;
  MM_DivisionByZero = 4;
  MM_Overflow = 8;
  MM_Underflow = $10;
  MM_Precicion = $20;
  MM_ExceptionMask = $3f;

  MM_MaskInvalidOp = %0000000010000000;
  MM_MaskDenorm    = %0000000100000000;
  MM_MaskDivZero   = %0000001000000000;
  MM_MaskOverflow  = %0000010000000000;
  MM_MaskUnderflow = %0000100000000000;
  MM_MaskPrecision = %0001000000000000;

{$define FPC_SYSTEM_HAS_FPC_CPUINIT}
procedure fpc_cpuinit;
  begin
    { don't let libraries influence the FPU cw set by the host program }
    if IsLibrary then
      begin
        Default8087CW:=Get8087CW;
        DefaultMXCSR:=GetMXCSR;
      end;
    SysResetFPU;
  end;

{$define FPC_SYSTEM_HAS_SYSINITFPU}
Procedure SysInitFPU;
begin
end;


{$define FPC_SYSTEM_HAS_SYSRESETFPU}
Procedure SysResetFPU;
  var
    { these locals are so we don't have to hack pic code in the assembler }
    localmxcsr: dword;
    localfpucw: word;
  begin
    localfpucw:=Default8087CW;
    localmxcsr:=DefaultMXCSR;
    asm
      fninit
      fwait
      fldcw   localfpucw
      ldmxcsr localmxcsr
    end;
  end;


{$ifndef FPC_SYSTEM_HAS_MEM_BARRIER}
{$define FPC_SYSTEM_HAS_MEM_BARRIER}

procedure ReadBarrier;assembler;nostackframe;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
  lfence
end;

procedure ReadDependencyBarrier;assembler;nostackframe;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
  { reads imply barrier on earlier reads depended on }
end;

procedure ReadWriteBarrier;assembler;nostackframe;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
  mfence
end;

procedure WriteBarrier;assembler;nostackframe;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
  sfence
end;

{$endif}

{****************************************************************************
                          Math Routines
****************************************************************************}

{$define FPC_SYSTEM_HAS_SWAPENDIAN}

{ SwapEndian(<16 Bit>) being inlined is faster than using assembler }
function SwapEndian(const AValue: SmallInt): SmallInt;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    { the extra Word type cast is necessary because the "AValue shr 8" }
    { is turned into "longint(AValue) shr 8", so if AValue < 0 then    }
    { the sign bits from the upper 16 bits are shifted in rather than  }
    { zeroes.                                                          }
    Result := SmallInt(((Word(AValue) shr 8) or (Word(AValue) shl 8)) and $ffff);
  end;


function SwapEndian(const AValue: Word): Word;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    Result := ((AValue shr 8) or (AValue shl 8)) and $ffff;
  end;


function SwapEndian(const AValue: LongInt): LongInt; assembler; nostackframe;
asm
{$ifdef win64}
  movl %ecx, %eax
{$else win64}
  movl %edi, %eax
{$endif win64}
  bswap %eax
end;


function SwapEndian(const AValue: DWord): DWord; assembler; nostackframe;
asm
{$ifdef win64}
  movl %ecx, %eax
{$else win64}
  movl %edi, %eax
{$endif win64}
  bswap %eax
end;


function SwapEndian(const AValue: Int64): Int64; assembler; nostackframe;
asm
{$ifdef win64}
  movq %rcx, %rax
{$else win64}
  movq %rdi, %rax
{$endif win64}
  bswap %rax
end;


function SwapEndian(const AValue: QWord): QWord; assembler; nostackframe;
asm
{$ifdef win64}
  movq %rcx, %rax
{$else win64}
  movq %rdi, %rax
{$endif win64}
  bswap %rax
end;


{$ifndef win64}
{$define FPC_SYSTEM_HAS_U128_DIV_U64_TO_U64}
function u128_div_u64_to_u64( const xh, xl: qword; const y: qword; out quotient, remainder: qword ): boolean;nostackframe;assembler;
{
  SysV:
  xh: RDI
  xl: RSI
  y: RDX
  quotient: RCX
  remainder: R8
}
label
  dodiv;
asm
  cmpq %rdi,%rdx
  ja   dodiv
  xorl %eax,%eax
  ret
dodiv:
  movq %rdx,%r9
  movq %rsi,%rax
  movq %rdi,%rdx
  divq %r9
  movq %rax,(%rcx)
  movq %rdx,(%r8)
  movl $1,%eax
end;
{$endif win64}
