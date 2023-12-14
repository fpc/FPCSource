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

{$ifndef win64}
  {$define use_fast_repmovstos} { REP STOS uses nonvolatile RDI and would require a stack frame on Win64 to be SEH-compliant. }
{$endif}

{$ifdef use_fast_repmovstos}
var
  fast_large_repmovstosb : boolean; { Enhanced REP MOVSB and STOSB (ERMSB) feature @ CPUID(7).ebx[9]. }
{$endif}

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

{$if not defined(FPC_SYSTEM_HAS_FILLCHAR)
  or not defined(FPC_SYSTEM_HAS_FILLWORD)
  or not defined(FPC_SYSTEM_HAS_FILLDWORD)
  or not defined(FPC_SYSTEM_HAS_FILLQWORD)}
procedure FillXxxx_MoreThanTwoXmms; assembler; nostackframe;
{ Input:
  rcx = 'x'
  rdx = byte count
  xmm0 = pattern for unaligned writes
  xmm1 = pattern for aligned writes }
const
{$ifdef use_fast_repmovstos}
  ErmsThreshold = 1536;
{$endif}
  NtThreshold = 4 * 1024 * 1024;
asm
    { x can start and end misaligned on the vector boundary:

      x = ~~][H1][H2][...][T2][T1]~
          [UH]                 [UT]

      UH (“unaligned head”) potentially overlaps with H1 and is already written with 'movdqu' by the caller.
      At least 1 of its bytes is exclusive to it, i.e. if x is already aligned, H1 starts at byte 16.

      H1 and so on are called “aligned heads” or just “heads”.
      T1 and so on are called “aligned tails” or just “tails”.

      UT (“unaligned tail”) is written with another 'movdqu' after the loop.
      At least 1 of its bytes is exclusive to it as well, that’s why 65 is subtracted below instead of 64. }

    lea    -65(%rcx,%rdx), %r8 { r8 = end of x - 65, to get the loop bound and to write UT later (why not write it right away though...). }
    and    $-16, %rcx { align rcx to the LEFT (so needs to be offset by an additional +16 for a while). }
    movdqa %xmm1, 16(%rcx) { Write H1. }
    mov    %r8, %rax
    and    $-16, %rax { rax = “T4” (possibly fictive) = aligned r8 = loop bound. }
    cmp    $49, %rdx { 33~49 bytes might contain 1~2 heads+tails; write as H1 and T1. }
    jle    .LOneAlignedTailWrite
    movdqa %xmm1, 32(%rcx) { Write H2. }
    cmp    $81, %rdx  { 50~81 bytes might contain 2~4 heads+tails; write as H1–2 and T2–1. }
    jle    .LTwoAlignedTailWrites
    cmp    $113, %rdx  { 82~113 bytes might contain 4~6 heads+tails; write as H1–2 and T4–1. }
    jle    .LFourAlignedTailWrites

    add    $48, %rcx
{$ifdef use_fast_repmovstos}
    cmp    $ErmsThreshold, %rdx
    jae    .LRepStos
{$else}
    cmp    $NtThreshold, %rdx
    jae    .L64xNT_Body
{$endif}

.balign 16
.L64x_Body:
    movdqa %xmm1, (%rcx)
    movdqa %xmm1, 16(%rcx)
    movdqa %xmm1, 32(%rcx)
    movdqa %xmm1, 48(%rcx)
    add    $64, %rcx
    cmp    %rax, %rcx
    jb     .L64x_Body

.LFourAlignedTailWrites:
    movdqa %xmm1, (%rax) { T4 }
    movdqa %xmm1, 16(%rax) { T3 }
.LTwoAlignedTailWrites:
    movdqa %xmm1, 32(%rax) { T2 }
.LOneAlignedTailWrite:
    movdqa %xmm1, 48(%rax) { T1 }
    movdqu %xmm0, 65-16(%r8) { UT }
    ret

{$ifdef use_fast_repmovstos}
.LRepStos:
{$ifdef FPC_PIC}
    movq   fast_large_repmovstosb@GOTPCREL(%rip), %r9
    cmpb   $1, (%r9)
{$else FPC_PIC}
    cmpb   $1, fast_large_repmovstosb(%rip)
{$endif FPC_PIC}
    jne    .LRepStosIsNotBetter
{$ifdef win64}
    push   %rdi { For tests on Windows; however this is SEH incompliant so the entire use_fast_repmovstos branch is disabled by default! }
{$endif}
    mov    %rcx, %rdi { rdi = REP STOS destination. }
    lea    65-16+8-1(%r8), %rcx
    sub    %rdi, %rcx
    shr    $3, %rcx { rcx = count of REP STOSQ blocks before UT. }
    movq   %xmm1, %rax { recover pattern for aligned writes back to GPR :) }
    rep stosq
    movdqu %xmm0, 65-16(%r8) { UT }
{$ifdef win64}
    pop    %rdi
{$endif}
    ret
{$endif}

.LRepStosIsNotBetter:
    cmp    $NtThreshold, %rdx
    jb     .L64x_Body

.balign 16
.L64xNT_Body:
    movntdq %xmm1, (%rcx)
    movntdq %xmm1, 16(%rcx)
    movntdq %xmm1, 32(%rcx)
    movntdq %xmm1, 48(%rcx)
    add    $64, %rcx
    cmp    %rax, %rcx
    jb     .L64xNT_Body
    sfence
    jmp    .LFourAlignedTailWrites
end;
{$endif FPC_SYSTEM_HAS_FILLxxxx}

{$ifndef FPC_SYSTEM_HAS_FILLCHAR}
{$define FPC_SYSTEM_HAS_FILLCHAR}
Procedure FillChar(var x;count:SizeInt;value:byte);assembler;nostackframe;
  asm
{ win64: rcx dest, rdx count, r8b value
  linux: rdi dest, rsi count, rdx value }
    movzbl {$ifdef win64} %r8b {$else} %dl {$endif}, %eax
    imul   $0x01010101, %eax
{$ifndef win64}
    mov    %rsi, %rdx
    mov    %rdi, %rcx
{$endif win64}

    cmp    $3, %rdx
    jle    .L3OrLess
    cmp    $16, %rdx
    jl     .L4to15

    movd   %eax, %xmm0
    pshufd $0, %xmm0, %xmm0
    movdqu %xmm0, (%rcx)
    movdqa %xmm0, %xmm1

    cmp    $32, %rdx
    jg     FillXxxx_MoreThanTwoXmms
    movdqu %xmm0, -16(%rcx,%rdx)
    ret

.L4to15:
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
  end;
{$endif FPC_SYSTEM_HAS_FILLCHAR}

{$ifndef FPC_SYSTEM_HAS_FILLWORD}
{$define FPC_SYSTEM_HAS_FILLWORD}
procedure FillWord(var x;count:SizeInt;value:word);assembler;nostackframe;
  asm
{$ifdef win64}
    movzwl %r8w, %eax
    shl    $16, %r8d
    or     %r8d, %eax
{$else}
    movzwl %dx, %eax
    shl    $16, %edx
    or     %edx, %eax
    mov    %rsi, %rdx
    mov    %rdi, %rcx
{$endif}

    cmp    $3, %rdx
    jle    .L3OrLess
    cmp    $8, %rdx
    jle    .L4to8

    movd   %eax, %xmm0
    pshufd $0, %xmm0, %xmm0 { xmm0 = pattern for unaligned writes }
    movdqu %xmm0, (%rcx)

    cmp    $16, %rdx
    jle    .LTail

    shl    $1, %rdx { rdx = byte count }
    mov    %rcx, %r8
    shl    $3, %ecx
    rol    %cl, %eax { misalign the pattern by the misalignment of x }
    mov    %r8, %rcx
    movd   %eax, %xmm1
    pshufd $0, %xmm1, %xmm1 { xmm1 = pattern for aligned writes }
    jmp    FillXxxx_MoreThanTwoXmms

.LTail:
    movdqu %xmm0, -16(%rcx,%rdx,2)
    ret

.L4to8:
    mov    %eax, %r8d
    shl    $32, %r8
    or     %r8, %rax
    mov    %rax, (%rcx)
    mov    %rax, -8(%rcx,%rdx,2)
    ret

.L3OrLess:
    test   %rdx, %rdx
    jle    .LQuit
    mov    %ax, (%rcx)
    mov    %ax, -2(%rcx,%rdx,2)
    shr    $1, %edx
    mov    %ax, (%rcx,%rdx,2)
.LQuit:
  end;
{$endif FPC_SYSTEM_HAS_FILLWORD}

{$ifndef FPC_SYSTEM_HAS_FILLDWORD}
{$define FPC_SYSTEM_HAS_FILLDWORD}
procedure FillDWord(var x;count:SizeInt;value:DWord);assembler;nostackframe;
  asm
{$ifdef win64}
    mov    %r8d, %eax
{$else}
    mov    %edx, %eax
    mov    %rsi, %rdx
    mov    %rdi, %rcx
{$endif win64}

    cmp    $3, %rdx
    jle    .L3OrLess
    cmp    $8, %rdx
    jle    .L4to8

    movd   %eax, %xmm0
    pshufd $0, %xmm0, %xmm0 { xmm0 = pattern for unaligned writes }
    movdqu %xmm0, (%rcx)

    shl    $2, %rdx { rdx = byte count }
    mov    %rcx, %r8
    shl    $3, %ecx
    rol    %cl, %eax { misalign the pattern by the misalignment of x }
    mov    %r8, %rcx
    movd   %eax, %xmm1
    pshufd $0, %xmm1, %xmm1 { xmm1 = pattern for aligned writes }
    jmp    FillXxxx_MoreThanTwoXmms

.L4to8:
{$ifndef win64} { on win64, eax = r8d already. }
    mov    %eax, %r8d
{$endif}
    shl    $32, %r8
    or     %r8, %rax
    mov    %rax, (%rcx)
    mov    %rax, 8(%rcx)
    mov    %rax, -16(%rcx,%rdx,4)
    mov    %rax, -8(%rcx,%rdx,4)
    ret

.L3OrLess:
    test   %rdx, %rdx
    jle    .LQuit
    mov    %eax, (%rcx)
    mov    %eax, -4(%rcx,%rdx,4)
    shr    $1, %edx
    mov    %eax, (%rcx,%rdx,4)
.LQuit:
  end;
{$endif FPC_SYSTEM_HAS_FILLDWORD}

{$ifndef FPC_SYSTEM_HAS_FILLQWORD}
{$define FPC_SYSTEM_HAS_FILLQWORD}
procedure FillQWord(var x;count:SizeInt;value:QWord);assembler;nostackframe;
  asm
{$ifdef win64}
    mov    %r8, %rax
{$else}
    mov    %rdx, %rax
    mov    %rsi, %rdx
    mov    %rdi, %rcx
{$endif win64}

    cmp    $2, %rdx
    jle    .L2OrLess
    cmp    $6, %rdx
    jle    .L3to6

    movq   %rax, %xmm0
    pshufd $0b01000100, %xmm0, %xmm0 { xmm0 = pattern for unaligned writes }
    movdqu %xmm0, (%rcx)

    shl    $3, %rdx { rdx = byte count }
    mov    %rcx, %r8
    shl    $3, %ecx
    rol    %cl, %rax { misalign the pattern by the misalignment of x }
    mov    %r8, %rcx
    movq   %rax, %xmm1
    pshufd $0b01000100, %xmm1, %xmm1 { xmm1 = pattern for aligned writes }
    jmp    FillXxxx_MoreThanTwoXmms

.L3to6:
    mov    %rax, (%rcx)
    mov    %rax, 8(%rcx)
    mov    %rax, 16(%rcx)
    mov    %rax, -24(%rcx,%rdx,8)
    mov    %rax, -16(%rcx,%rdx,8)
    mov    %rax, -8(%rcx,%rdx,8)
    ret

.L2OrLess:
    test   %rdx, %rdx
    jle    .LQuit
    mov    %rax, (%rcx)
    mov    %rax, -8(%rcx,%rdx,8)
.LQuit:
  end;
{$endif FPC_SYSTEM_HAS_FILLQWORD}

{$ifndef FPC_SYSTEM_HAS_INDEXBYTE}
{$define FPC_SYSTEM_HAS_INDEXBYTE}
function IndexByte(Const buf;len:SizeInt;b:byte):SizeInt; assembler; nostackframe;
{ win64: rcx buf, rdx len, r8b word
  linux: rdi buf, rsi len, rdx word }
asm
    test   {$ifdef win64} %rdx, %rdx {$else} %rsi, %rsi {$endif}
    jz     .Lnotfound                  { exit if len=0 }
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
    punpcklbw  %xmm1, %xmm1
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
    test   {$ifdef win64} %rdx, %rdx {$else} %rsi, %rsi {$endif}
    jz     .Lnotfound                  { exit if len=0 }
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
    pshufd $0, %xmm1, %xmm1
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
{$ifdef win64}
    mov      %rcx, %rax
{$else}
    mov      %rdx, %r8
    mov      %rsi, %rdx
    mov      %rdi, %rax
{$endif}
    cmp      $4, %rdx
    jle      .LDwordwise_Prepare
    sub      $4, %rdx
    movd     %r8d, %xmm1
    pshufd   $0, %xmm1, %xmm1
.balign 16
.L4x_Body:
    movdqu   (%rax), %xmm0
    pcmpeqd  %xmm1, %xmm0
    pmovmskb %xmm0, %r8d
    test     %r8d, %r8d
    jnz      .LFoundAtMask
    add      $16, %rax
    sub      $4, %rdx
    jg       .L4x_Body

    lea      (%rax,%rdx,4), %rax
    movdqu   (%rax), %xmm0
    pcmpeqd  %xmm1, %xmm0
    pmovmskb %xmm0, %r8d
    test     %r8d, %r8d
    jnz      .LFoundAtMask
    or       $-1, %rax
    ret

.balign 16 { no-op }
.LDwordwise_Body:
    cmp      (%rax), %r8d
    je       .LFoundAtRax
    add      $4, %rax
.LDwordwise_Prepare:
    sub      $1, %rdx
    jae      .LDwordwise_Body
    or       $-1, %rax
    ret

.LFoundAtMask:
    bsf      %r8d, %r8d
    add      %r8, %rax
.LFoundAtRax:
    sub      {$ifdef win64} %rcx {$else} %rdi {$endif}, %rax
    shr      $2, %rax
end;
{$endif FPC_SYSTEM_HAS_INDEXDWORD}

{$ifndef FPC_SYSTEM_HAS_INDEXQWORD}
{$define FPC_SYSTEM_HAS_INDEXQWORD}
function IndexQWord(Const buf;len:SizeInt;b:QWord):SizeInt; assembler; nostackframe;
{ win64: rcx=buf, rdx=len, r8=b
  else:  rdi=buf, rsi=len, rdx=b }
asm
    mov      {$ifdef win64} %rcx {$else} %rdi {$endif}, %rax
    sub      $8, %rax
.balign 16
.LQwordwise_Next:
    add      $8, %rax
    sub      $1, {$ifdef win64} %rdx {$else} %rsi {$endif}
    jb       .LNothing
    cmp      {$ifdef win64} %r8 {$else} %rdx {$endif}, (%rax)
    jne      .LQwordwise_Next
    sub      {$ifdef win64} %rcx {$else} %rdi {$endif}, %rax
    shr      $3, %rax
    ret

.LNothing:
    mov      $-1, %rax
end;
{$endif FPC_SYSTEM_HAS_INDEXQWORD}

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
    { rcx = buf1, rdx = buf2, r8 = len }
    cmp      $1, %r8
    jle      .L1OrLess

    cmp      $16, %r8
    jae      .LVecOrMore

    { 2 to 15 bytes: check for page cross. Pessimistic variant that has false positives, but is faster. }
    mov      %ecx, %eax
    or       %edx, %eax
    and      $4095, %eax
    cmp      $4080, %eax
    ja       .LCantOverReadBoth

    { Over-read both as XMMs. }
    movdqu   (%rcx), %xmm0
    movdqu   (%rdx), %xmm1
    pcmpeqb  %xmm1, %xmm0
    pmovmskb %xmm0, %eax
    inc      %ax
    jz       .LNothing
    bsf      %eax, %eax
    cmp      %r8d, %eax { Ignore garbage beyond 'len'. }
    jae      .LNothing
    movzbl   (%rdx,%rax), %edx
    movzbl   (%rcx,%rax), %eax
    sub      %rdx, %rax
    ret

.balign 16
.LNothing:
    xor      %eax, %eax
    ret

.LAligned32xLoop_TwoVectorsDiffer:
    add      %rcx, %rdx { restore rdx = buf2 }
    pmovmskb %xmm0, %r8d { Is there a difference in the first vector? }
    inc      %r8w
    jz       .LVec1Differs { No difference in the first vector, xmm0 is all ones, eax = pmovmskb(pcmpeqb(buf1 + 16, buf2 + 16)) from the loop body. }
    mov      %r8d, %eax
.LVec0Differs:
    bsf      %eax, %eax
    movzbl   (%rdx,%rax), %edx
    movzbl   (%rcx,%rax), %eax
    sub      %rdx, %rax
    ret
    .byte    0x66,0x2e,0x0f,0x1f,0x84,0x00,0x00,0x00,0x00,0x00 { Turn .balign 16 before .LAligned32xLoop_Body into a no-op. }

.LVecOrMore:
    { Compare first vectors. }
    movdqu   (%rcx), %xmm0
    movdqu   (%rdx), %xmm1
    pcmpeqb  %xmm1, %xmm0
    pmovmskb %xmm0, %eax
    inc      %ax
    jnz      .LVec0Differs

    sub      $32, %r8
    jbe      .LLastVec

    { Compare second vectors. }
    movdqu   16(%rcx), %xmm0
    movdqu   16(%rdx), %xmm1
    pcmpeqb  %xmm1, %xmm0
    pmovmskb %xmm0, %eax
    inc      %ax
    jnz      .LVec1Differs

    cmp      $32, %r8
    jbe      .LLastTwoVectors

    { More than four vectors: aligned loop. }
    lea      -32(%rcx,%r8), %r8 { buffer end - last two vectors handled separately - first two vectors already analyzed (by the fact ecx was still len - 32). }
    sub      %rcx, %rdx { rdx = buf2 - buf1 }
    and      $-16, %rcx { Align buf1. First two vectors already analyzed are skipped by +32 on the first loop iteration. }
    sub      %rcx, %r8 { r8 = count to be handled with loop }
.balign 16 { no-op }
.LAligned32xLoop_Body:
    add      $32, %rcx
    { Compare two XMMs, reduce the result with 'and'. }
    movdqu   (%rdx,%rcx), %xmm0
    pcmpeqb  (%rcx), %xmm0 { xmm0 = pcmpeqb(buf1, buf2) }
    movdqu   16(%rdx,%rcx), %xmm1
    pcmpeqb  16(%rcx), %xmm1
    pand     %xmm0, %xmm1 { xmm1 = xmm0 and pcmpeqb(buf1 + 16, buf2 + 16) }
    pmovmskb %xmm1, %eax
    inc      %ax
    jnz      .LAligned32xLoop_TwoVectorsDiffer
    sub      $32, %r8
    ja       .LAligned32xLoop_Body
    add      %rcx, %rdx { restore rdx = buf2 }
    add      $32, %r8
.LLastTwoVectors:
    movdqu   (%rcx,%r8), %xmm0
    movdqu   (%rdx,%r8), %xmm1
    pcmpeqb  %xmm1, %xmm0
    pmovmskb %xmm0, %eax
    inc      %ax
    jnz      .LVecEm2Differs
.LLastVec:
    movdqu   16(%rcx,%r8), %xmm0
    movdqu   16(%rdx,%r8), %xmm1
    pcmpeqb  %xmm1, %xmm0
    pmovmskb %xmm0, %eax
    inc      %ax
    jnz      .LVecEm1Differs
    xor      %eax, %eax
    ret

.LVec1Differs:
    xor      %r8d, %r8d
.LVecEm1Differs:
    add      $16, %r8
.LVecEm2Differs:
    bsf      %eax, %eax
    add      %r8, %rax
    movzbl   (%rdx,%rax), %edx
    movzbl   (%rcx,%rax), %eax
    sub      %rdx, %rax
    ret

.LCantOverReadBoth:
    cmp      $8, %r8d
    ja       .L9to15
    cmp      $3, %r8d
    jle      .L2to3
    mov      (%rcx), %eax
    mov      (%rdx), %r9d
    cmp      %r9d, %eax
    jne      .L4xOr8xDiffer
    mov      -4(%rcx,%r8), %eax
    mov      -4(%rdx,%r8), %r9d
    cmp      %r9d, %eax
    jne      .L4xOr8xDiffer
    xor      %eax, %eax
    ret

.L9to15:
    mov      (%rcx), %rax
    mov      (%rdx), %r9
    cmp      %r9, %rax
    jne      .L4xOr8xDiffer
    mov      -8(%rcx,%r8), %rax
    mov      -8(%rdx,%r8), %r9
    cmp      %r9, %rax
    jne      .L4xOr8xDiffer
    xor      %eax, %eax
    ret

.L4xOr8xDiffer:
    bswap    %r9
    bswap    %rax
    cmp      %r9, %rax
    sbb      %rax, %rax
    or       $1, %rax
    ret

.L2to3:
    movzwl   (%rcx), %eax
    bswap    %eax
    shr      $1, %eax
    mov      -1(%rcx,%r8), %al
    movzwl   (%rdx), %ecx
    bswap    %ecx
    shr      $1, %ecx
    mov      -1(%rdx,%r8), %cl
    sub      %rcx, %rax
    ret

.L1OrLess:
    jl       .LUnbounded_Prepare
    movzbl   (%rcx), %eax
    movzbl   (%rdx), %edx
    sub      %rdx, %rax
    ret

.LUnbounded_Prepare:
    sub      %rcx, %rdx { rdx = buf2 - buf1 }
    test     %r8, %r8
    jnz      .LUnbounded_Body
    xor      %eax, %eax
    ret

.balign 16
.LUnbounded_Next:
    add      $1, %rcx
.LUnbounded_Body:
    movzbl   (%rdx,%rcx), %eax
    cmp      %al, (%rcx)
    je       .LUnbounded_Next
    sbb      %rax, %rax
    or       $1, %rax
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
    sub      %rcx, %rdx { rdx = buf2 - buf1 }
    cmp      $1, %r8
    jle      .LWordwise_Prepare
    mov      %r8, %rax
    shr      $62, %rax
    jnz      .LWordwise_Prepare
    cmp      $8, %r8
    jge      .LVecOrMore

    lea      (%rdx,%rcx), %eax
    or       %ecx, %eax
    and      $4095, %eax
    cmp      $4080, %eax
    ja       .LWordwise_Prepare
    movdqu   (%rdx,%rcx), %xmm0
    movdqu   (%rcx), %xmm1
    pcmpeqw  %xmm1, %xmm0
    pmovmskb %xmm0, %eax
    shl      $1, %r8 { convert to bytes }
    inc      %ax
    jz       .LNothing
    bsf      %eax, %eax
    cmp      %r8d, %eax
    jb       .LSubtractWords
.LNothing:
    xor      %eax, %eax
    ret

.balign 16
.LWordwise_Body:
    movzwl  (%rdx,%rcx), %eax
    cmp     %ax, (%rcx)
    jne     .LDoSbb
    add     $2, %rcx
.LWordwise_Prepare:
    sub     $1, %r8
    jae     .LWordwise_Body
    xor     %eax, %eax
    ret

.LDoSbb:
    sbb      %rax, %rax
    or       $1, %rax
    ret

.LVec0Differs:
    bsf      %eax, %eax
.LSubtractWords:
    add      %rcx, %rdx { recover rdx = buf2 }
    movzwl   (%rdx,%rax), %edx
    movzwl   (%rcx,%rax), %eax
    sub      %rdx, %rax
    ret

.LVecOrMore:
    movdqu   (%rdx,%rcx), %xmm0 { Compare first vectors. }
    movdqu   (%rcx), %xmm1
    pcmpeqw  %xmm1, %xmm0
    pmovmskb %xmm0, %eax
    inc      %ax
    jnz      .LVec0Differs

    shl      $1, %r8 { convert to bytes }
    sub      $32, %r8 { first 16 bytes already analyzed + last 16 bytes analyzed separately }
    jle      .LLastVec

    mov      %rcx, %r9 { save original buf1 to recover word position if byte mismatch found (aligned loop works in bytes to support misaligned buf1). }
    add      %rcx, %r8
    and      $-16, %rcx { align buf1; +16 is performed by the loop. }
    sub      %rcx, %r8

.balign 16
.LAligned8xLoop_Body:
    add      $16, %rcx
    movdqu   (%rdx,%rcx), %xmm0
    pcmpeqb  (%rcx), %xmm0
    pmovmskb %xmm0, %eax
    inc      %ax
    jnz      .LAligned8xLoop_VecDiffers
    sub      $16, %r8
    ja       .LAligned8xLoop_Body
.LLastVec:
    lea      16(%rcx,%r8), %rcx { point to the last 16 bytes }
    movdqu   (%rdx,%rcx), %xmm0
    movdqu   (%rcx), %xmm1
    pcmpeqw  %xmm1, %xmm0
    pmovmskb %xmm0, %eax
    inc      %ax
    jnz      .LVec0Differs
    xor      %eax, %eax
    ret

.LAligned8xLoop_VecDiffers:
    bsf      %eax, %eax
    add      %rax, %rcx
    sub      %r9, %rcx
    and      $-2, %rcx
    add      %r9, %rcx
    movzwl   (%rdx,%rcx), %edx
    movzwl   (%rcx), %eax
    sub      %rdx, %rax
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
    sub      %rcx, %rdx { rdx = buf2 - buf1 }
    cmp      $4, %r8
    jle      .LDwordwise_Prepare
    mov      %r8, %rax
    shr      $61, %rax
    jnz      .LDwordwise_Prepare

    movdqu   (%rdx,%rcx), %xmm0 { Compare first vectors. }
    movdqu   (%rcx), %xmm1
    pcmpeqd  %xmm1, %xmm0
    pmovmskb %xmm0, %eax
    inc      %ax
    jnz      .LVec0Differs

    shl      $2, %r8 { convert to bytes }
    sub      $32, %r8 { first 16 bytes already analyzed + last 16 bytes analyzed separately }
    jle      .LLastVec

    mov      %rcx, %r9 { save original buf1 to recover word position if byte mismatch found (aligned loop works in bytes to support misaligned buf1). }
    add      %rcx, %r8
    and      $-16, %rcx { align buf1; +16 is performed by the loop. }
    sub      %rcx, %r8

.balign 16
.LAligned4xLoop_Body:
    add      $16, %rcx
    movdqu   (%rdx,%rcx), %xmm0
    pcmpeqb  (%rcx), %xmm0
    pmovmskb %xmm0, %eax
    inc      %ax
    jnz      .LAligned4xLoop_VecDiffers
    sub      $16, %r8
    ja       .LAligned4xLoop_Body
.LLastVec:
    lea      16(%rcx,%r8), %rcx { point to the last 16 bytes }
    movdqu   (%rdx,%rcx), %xmm0
    movdqu   (%rcx), %xmm1
    pcmpeqd  %xmm1, %xmm0
    pmovmskb %xmm0, %eax
    inc      %ax
    jnz      .LVec0Differs
    xor      %eax, %eax
    ret

.LVec0Differs:
    bsf      %eax, %eax
    add      %rcx, %rdx { recover rdx = buf2 }
    mov      (%rdx,%rax), %edx
    cmp      %edx, (%rcx,%rax)
    sbb      %rax, %rax
    or       $1, %rax
    ret

.LAligned4xLoop_VecDiffers:
    bsf      %eax, %eax
    add      %rax, %rcx
    sub      %r9, %rcx
    and      $-4, %rcx
    add      %r9, %rcx
    mov      (%rdx,%rcx), %edx
    cmp      %edx, (%rcx)
.LDoSbb:
    sbb      %rax, %rax
    or       $1, %rax
    ret

.balign 16
.LDwordwise_Body:
    mov     (%rdx,%rcx), %eax
    cmp     %eax, (%rcx)
    jne     .LDoSbb
    add     $4, %rcx
.LDwordwise_Prepare:
    sub     $1, %r8
    jae     .LDwordwise_Body
    xor     %eax, %eax
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
        movl    $-1,%eax
        lock
        xaddl   %eax, {$ifdef win64} (%rcx) {$else} (%rdi) {$endif}
        decl    %eax
end;


function InterLockedIncrement (var Target: longint) : longint; assembler; nostackframe;
asm
        movl    $1,%eax
        lock
        xaddl   %eax, {$ifdef win64} (%rcx) {$else} (%rdi) {$endif}
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
        lock
        xaddl   %edx, (%rcx)
        movl    %edx,%eax
{$else win64}
        lock
        xaddl   %esi, (%rdi)
        movl    %esi,%eax
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
        movq    $-1,%rax
        lock
        xaddq   %rax, {$ifdef win64} (%rcx) {$else} (%rdi) {$endif}
        decq    %rax
end;


function InterLockedIncrement64 (var Target: int64) : int64; assembler; nostackframe;
asm
        movq    $1,%rax
        lock
        xaddq   %rax, {$ifdef win64} (%rcx) {$else} (%rdi) {$endif}
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
        lock
        xaddq   %rdx, (%rcx)
        movq    %rdx,%rax
{$else win64}
        lock
        xaddq   %rsi, (%rdi)
        movq    %rsi,%rax
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
  var
    _eax,cpuid7_ebx,cpuid1_ecx : dword;
  begin
    { don't let libraries influence the FPU cw set by the host program }
    if IsLibrary then
      begin
        Default8087CW:=Get8087CW;
        DefaultMXCSR:=GetMXCSR;
      end;
    SysResetFPU;
    asm
      xorl %eax,%eax
      cpuid
      movl %eax,_eax
    end;
    if _eax>=7 then
      begin
        asm
          movl $1,%eax
          xorl %ecx,%ecx
          cpuid
          movl %ecx,cpuid1_ecx
          movl $7,%eax
          xorl %ecx,%ecx
          cpuid
          movl %ebx,cpuid7_ebx
        end;
{$ifdef use_fast_repmovstos}
        fast_large_repmovstosb:=cpuid7_ebx and (1 shl 9)<>0;
{$endif}
        { XGETBV support? }
        if (cpuid1_ecx and $8000000)<>0 then 
          begin
            asm
              xorl %ecx,%ecx
              .byte   0x0f,0x01,0xd0 { xgetbv }
              movl %eax,_eax
            end;
            if (_eax and 6)=6 then
              begin
                has_avx_support:=(cpuid1_ecx and $10000000)<>0;
                has_avx2_support:=(cpuid7_ebx and $20)<>0;
              end;
          end;
      end;
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
