{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Processor dependent implementation for the system unit for
    intel i386+

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$if defined(linux)}
  {$define FPC_SYSTEM_STACKALIGNMENT16}
{$endif defined(linux)}

{****************************************************************************
                               Primitives
****************************************************************************}
var
  os_supports_sse : boolean;
  { this variable is set to true, if currently an sse check is executed and no sig ill should be generated }
  sse_check : boolean;
  fast_large_repmovstosb : boolean; { Enhanced REP MOVSB and STOSB (ERMSB) feature @ CPUID(7).ebx[9]. }
  has_sse41_support : boolean;
  fpc_cpucodeinit_performed : boolean; { Code before fpc_cpucodeinit can call certain dispatched functions, such as Move. }

{$asmmode ATT}

function cpuid_support : boolean;assembler;nostackframe;
  {
    Check if the ID-flag can be changed, if changed then CpuID is supported.
    Tested under go32v1 and Linux on c6x86 with CpuID enabled and disabled (PFV)
  }
  asm
    pushfl
    movl    (%esp),%eax
    xorl    $0x200000,%eax
    pushl   %eax
    popfl
    pushfl
    popl    %eax
    xorl    (%esp),%eax
    popfl
    testl   $0x200000,%eax
    setnz   %al
  end;

{$define FPC_SYSTEM_HAS_FPC_CPUINIT}
procedure fpc_cpuinit;
  begin
    { because of the brain dead sse detection on x86, this test is post poned to fpc_cpucodeinit which
      must be implemented OS dependend (FK)
    has_sse_support:=sse_support;
    has_mmx_support:=mmx_support;
    }
  end;

{$ifndef darwin}
procedure fpc_geteipasebx; [public, alias: 'fpc_geteipasebx'];assembler; nostackframe;
asm
  movl    (%esp),%ebx
end;


procedure fpc_geteipasecx; [public, alias: 'fpc_geteipasecx'];assembler; nostackframe;
asm
  movl    (%esp),%ecx
end;
{$endif}

{$if not defined(FPC_SYSTEM_HAS_MOVE)
 and not defined(OLD_ASSEMBLER)
 and not defined(darwin)}
{$i fastmove.inc}
{$endif}

{$ifndef FPC_SYSTEM_HAS_MOVE}
{$define FPC_SYSTEM_HAS_MOVE}

procedure Move(const source;var dest;count:SizeInt);[public, alias: 'FPC_MOVE'];assembler;
var
  saveesi,saveedi : longint;
asm
        movl    %edi,saveedi
        movl    %esi,saveesi
        movl    %eax,%esi
        movl    %edx,%edi
        movl    %ecx,%edx
        movl    %edi,%eax
{ check for zero or negative count }
        cmpl    $0,%edx
        jle     .LMoveEnd
{ Check for back or forward }
        sub     %esi,%eax
        jz      .LMoveEnd               { Do nothing when source=dest }
        jc      .LFMove                 { Do forward, dest<source }
        cmp     %edx,%eax
        jb      .LBMove                 { Dest is in range of move, do backward }
{ Forward Copy }
.LFMove:
{$ifdef FPC_ENABLED_CLD}
        cld
{$endif FPC_ENABLED_CLD}
        cmpl    $15,%edx
        jl      .LFMove1
        movl    %edi,%ecx       { Align on 32bits }
        negl    %ecx
        andl    $3,%ecx
        subl    %ecx,%edx
        rep
        movsb
        movl    %edx,%ecx
        andl    $3,%edx
        shrl    $2,%ecx
        rep
        movsl
.LFMove1:
        movl    %edx,%ecx
        rep
        movsb
        jmp .LMoveEnd
{ Backward Copy }
.LBMove:
        std
        addl    %edx,%esi
        addl    %edx,%edi
        movl    %edi,%ecx
        decl    %esi
        decl    %edi
        cmpl    $15,%edx
        jl      .LBMove1
        negl    %ecx            { Align on 32bits }
        andl    $3,%ecx
        subl    %ecx,%edx
        rep
        movsb
        movl    %edx,%ecx
        andl    $3,%edx
        shrl    $2,%ecx
        subl    $3,%esi
        subl    $3,%edi
        rep
        movsl
        addl    $3,%esi
        addl    $3,%edi
.LBMove1:
        movl    %edx,%ecx
        rep
        movsb
        cld
.LMoveEnd:
        movl    saveedi,%edi
        movl    saveesi,%esi
end;

{$endif FPC_SYSTEM_HAS_MOVE}


{ Darwin uses Clang to assemble. Recent Clang versions (rightly) give an error when you add global labels in
  the middle of .cfi_startproc / .cfi_endproc pairs, since this means you could jump into it from other code
  whose CFI state is completely different without the compiler even having the theoretical ability to analyse
  all code and generate balanced information.

  Since FPC does not attempt it even for local labels, this kind of code is inherently unsafe.
}
{$if not defined(darwin) and
  (not defined(FPC_SYSTEM_HAS_FILLCHAR)
   or not defined(FPC_SYSTEM_HAS_FILLWORD)
   or not defined(FPC_SYSTEM_HAS_FILLDWORD)
   or not defined(FPC_SYSTEM_HAS_FILLQWORD)
)}

{$if not defined(FPC_SYSTEM_HAS_FILLCHAR)
   or not defined(FPC_SYSTEM_HAS_FILLWORD)
   or not defined(FPC_SYSTEM_HAS_FILLDWORD)}
const
  FillXxxx_RepStosThreshold_ERMS = 1024;
  FillXxxx_RepStosThreshold_NoERMS = 512 * 1024;

procedure FillXxxx_U32Pattern_RepStos_8OrMore; assembler; nostackframe;
{ eax — x, ecx — uint32 pattern, edx — byte count >= 8 (preferably >= FillXxxx_RepStosThreshold_(No)ERMS, depending on fast_large_repmovstosb). }
asm
{$ifdef FPC_ENABLED_CLD}
        cld
{$endif FPC_ENABLED_CLD}
        mov    %ecx, (%eax) { Write first 4 bytes unaligned. }
        push   %ecx { pattern }
        push   %edi
        mov    %eax, %edi { Move x to edi, as expected by ‘rep stosl’. }
        xchg   %eax, %ecx { now eax = pattern (as expected by ‘rep stosl’) and ecx = x (to rotate the pattern by its misalignment) }
        shl    $3, %ecx { ecx = misalignment of x in bits. }
        rol    %cl, %eax { misalign the pattern; no-op for FillChar, but handles misaligned cases of FillWord+. }
        add    %edi, %edx { edx = x end }
        lea    -1(%edx), %ecx { ecx = x end - 1. }
        add    $4, %edi
        and    $-4, %edi { edi = 4-byte aligned pointer strictly to the right of the start. }
        and    $-4, %ecx { ecx = 4-byte aligned pointer strictly to the left of the end. }
        sub    %edi, %ecx { ecx = byte count between them. }
        shr    $2, %ecx { ecx = uint32 count, as expected by ‘rep stosl’. }
        rep stosl
        pop    %edi
        pop    %ecx
        mov    %ecx, -4(%edx) { Write last 4 bytes unaligned. }
end;
{$endif FillChar/Word/DWord required.}

label
  FillXxxx_MoreThanTwoXMMs;

procedure FillXxxx_U32Pattern_SSE2_16OrMore; assembler; nostackframe;
{ eax — x, ecx — uint32 pattern, edx — byte count >= 16 (preferably > 16). }
const
  NtThreshold = 4 * 1024 * 1024;
asm
        movd   %ecx, %xmm0
        pshufd $0, %xmm0, %xmm0 { xmm0 = pattern for unaligned writes }
        movdqu %xmm0, (%eax)
        movdqu %xmm0, -16(%eax,%edx)
        cmp    $32, %edx
        ja     .LMoreThanTwoVectors
        ret
        .byte  144 { Turn .balign 16 before .L64x_Body into a no-op. }

      { x can start and end misaligned on the vector boundary:
        x = ~~][H1][H2][...][T2][T1]~
            [UH]                 [UT]
        UH/UT stands for “unaligned head/tail”, both have 1~16 bytes. }

.LMoreThanTwoVectors:
        push   %esi
        mov    %ecx, %esi { esi = pattern }
        mov    %eax, %ecx
        shl    $3, %ecx { ecx = misalignment of x in bits }
        rol    %cl, %esi { misalign the pattern }
        movd   %esi, %xmm0
        pshufd $0, %xmm0, %xmm0
        pop    %esi

{ FillChar (to skip the misaligning above) and FillQWord jump here.
  eax — x, edx — byte count > 32, xmm0 = pattern for ALIGNED writes, first and last 16 bytes written. }
FillXxxx_MoreThanTwoXMMs:
        lea    -65(%eax,%edx), %ecx
        and    $-16, %ecx { ecx = “T4” (possibly fictive) = loop bound. }
        mov    %ecx, %edx { Remember T4 to edx. }
        and    $-16, %eax { eax = H1 − 16. }
        sub    %eax, %ecx { ecx = aligned byte count − 48. }
        movdqa %xmm0, 16(%eax) { Write H1. }
        cmp    $32-48, %ecx
        jle    .LOneAlignedTailWrite
        movdqa %xmm0, 32(%eax) { Write H2. }
        cmp    $64-48, %ecx
        jle    .LTwoAlignedTailWrites
        sub    $48, %ecx { ecx = aligned byte count − 96 (32 bytes already written + 64 bytes written after loop). }
        jle    .LFourAlignedTailWrites { ecx was ≤ 96−48 }

        add    $48, %eax { eax = H3. }
        cmp    $NtThreshold, %ecx
        jae    .L64xNT_Body

.balign 16 { no-op }
.L64x_Body:
        movdqa %xmm0, (%eax)
        movdqa %xmm0, 16(%eax)
        movdqa %xmm0, 32(%eax)
        movdqa %xmm0, 48(%eax)
        add    $64, %eax
        sub    $64, %ecx
        ja     .L64x_Body
.LFourAlignedTailWrites:
        movdqa %xmm0, (%edx) { T4 }
        movdqa %xmm0, 16(%edx) { T3 }
.LTwoAlignedTailWrites:
        movdqa %xmm0, 32(%edx) { T2 }
.LOneAlignedTailWrite:
        movdqa %xmm0, 48(%edx) { T1 }
        ret

.balign 16
.L64xNT_Body:
        movntdq %xmm0, (%eax)
        movntdq %xmm0, 16(%eax)
        movntdq %xmm0, 32(%eax)
        movntdq %xmm0, 48(%eax)
        add    $64, %eax
        sub    $64, %ecx
        ja     .L64xNT_Body
        sfence
        jmp    .LFourAlignedTailWrites
end;

{$if not defined(FPC_SYSTEM_HAS_FILLCHAR)
  or not defined(FPC_SYSTEM_HAS_FILLWORD)
  or not defined(FPC_SYSTEM_HAS_FILLDWORD)}
{$ifndef CPUX86_HAS_SSE2}
procedure FillXxxx_U32Pattern_Plain_16OrMore; assembler; nostackframe;
{ eax — x, ecx — uint32 pattern, edx — byte count >= 12 (preferably >= 16). }
asm
        mov     %ecx, (%eax) { Write first 4 bytes. }
        lea     -9(%eax,%edx), %edx
        mov     %ecx, 5(%edx) { Write last 4 bytes. }
        and     $-4, %edx { edx = loop bound. }
        push    %esi
        mov     %ecx, %esi { esi = pattern }
        mov     %eax, %ecx
        shl     $3, %ecx { ecx = misalignment of x in bits }
        rol     %cl, %esi { misalign the pattern }
        add     $4, %eax
        and     $-4, %eax
.balign 16
.L8xLoop:
        mov     %esi, (%eax)
        mov     %esi, 4(%eax)
        add     $8, %eax
        cmp     %edx, %eax
        jb      .L8xLoop
        mov     %esi, (%edx)
        mov     %esi, 4(%edx)
        pop     %esi
end;
{$endif ndef CPUX86_HAS_SSE2 (need Fill*_Plain)}

procedure FillXxxx_U32Pattern_Ladder_4to16; assembler; nostackframe;
{ eax — x, ecx — uint32 pattern, edx — byte count, 4 <= edx <= 16. }
asm
        mov     %ecx, (%eax)
        cmp     $8, %edx
        jle     .LLast4
        mov     %ecx, 4(%eax)
        mov     %ecx, -8(%eax,%edx)
.LLast4:
        mov     %ecx, -4(%eax,%edx)
end;
{$endif FillChar/Word/DWord required.}
{$endif FillChar/Word/DWord/QWord required.}


{$if not defined(darwin) and
     not defined(FPC_SYSTEM_HAS_FILLCHAR)}
{$define FPC_SYSTEM_HAS_FILLCHAR}
procedure FillChar_3OrLess; assembler; nostackframe;
{ cl — x, edx — byte count, Low(int32) <= edx <= 3. }
asm
        test    %edx, %edx
        jle     .LQuit
        mov     %cl, (%eax)
        mov     %cl, -1(%eax,%edx)
        shr     $1, %edx
        mov     %cl, (%eax,%edx)
.LQuit:
end;

{$ifndef CPUX86_HAS_SSE2}
procedure FillChar_Plain(var x;count:SizeInt;value:byte);assembler;nostackframe;
asm
        cmp     $3, %edx
        jle     FillChar_3OrLess

        movzbl  %cl, %ecx
        imul    $0x01010101, %ecx
        cmp     $16, %edx
        jbe     FillXxxx_U32Pattern_Ladder_4to16
        jmp     FillXxxx_U32Pattern_Plain_16OrMore
end;
{$endif ndef CPUX86_HAS_SSE2}

procedure FillChar_SSE2(var x;count:SizeInt;value:byte);assembler;nostackframe;
asm
        cmp     $3, %edx
        jle     FillChar_3OrLess

        movzbl  %cl, %ecx
        imul    $0x01010101, %ecx
        cmp     $16, %edx
        jbe     FillXxxx_U32Pattern_Ladder_4to16
        cmp     $FillXxxx_RepStosThreshold_NoERMS, %edx
        jae     FillXxxx_U32Pattern_RepStos_8OrMore

        movd   %ecx, %xmm0
        pshufd $0, %xmm0, %xmm0 { xmm0 = pattern for unaligned writes }
        movdqu %xmm0, (%eax)
        movdqu %xmm0, -16(%eax,%edx)
        cmp    $32, %edx
        ja     FillXxxx_MoreThanTwoXMMs
end;

procedure FillChar_SSE2_ERMS(var x;count:SizeInt;value:byte);assembler;nostackframe;
asm
        cmp     $3, %edx
        jle     FillChar_3OrLess

        movzbl  %cl, %ecx
        imul    $0x01010101, %ecx
        cmp     $16, %edx
        jbe     FillXxxx_U32Pattern_Ladder_4to16
        cmp     $FillXxxx_RepStosThreshold_ERMS, %edx
        jae     FillXxxx_U32Pattern_RepStos_8OrMore

        movd   %ecx, %xmm0
        pshufd $0, %xmm0, %xmm0 { xmm0 = pattern for unaligned writes }
        movdqu %xmm0, (%eax)
        movdqu %xmm0, -16(%eax,%edx)
        cmp    $32, %edx
        ja     FillXxxx_MoreThanTwoXMMs
end;

procedure FillChar_Dispatch(var x;count:SizeInt;value:byte); forward;

var
  FillChar_Impl: procedure(var x;count:SizeInt;value:byte) = @FillChar_Dispatch;

procedure FillChar_Dispatch(var x;count:SizeInt;value:byte);
begin
  if not fpc_cpucodeinit_performed then
    begin
      {$ifdef CPUX86_HAS_SSE2} FillChar_SSE2 {$else} FillChar_Plain {$endif} (x, count, value);
      exit;
    end;
  if fast_large_repmovstosb then
    FillChar_Impl := @FillChar_SSE2_ERMS
  else {$ifndef CPUX86_HAS_SSE2} if has_sse2_support then {$endif}
    FillChar_Impl := @FillChar_SSE2
{$ifndef CPUX86_HAS_SSE2}
  else
    FillChar_Impl := @FillChar_Plain
{$endif ndef CPUX86_HAS_SSE2};
  FillChar_Impl(x, count, value);
end;

procedure FillChar(var x;count:SizeInt;value:byte);
begin
  FillChar_Impl(x, count, value);
end;
{$endif FPC_SYSTEM_HAS_FILLCHAR}


{$if not defined(darwin) and
     not defined(FPC_SYSTEM_HAS_FILLWORD)}
{$define FPC_SYSTEM_HAS_FILLWORD}
procedure FillWord_3OrLess; assembler; nostackframe;
asm
        test    %edx, %edx
        jle     .LQuit
        mov     %cx, (%eax)
        mov     %cx, -2(%eax,%edx,2)
        shr     $1, %edx
        mov     %cx, (%eax,%edx,2)
.LQuit:
end;

{$ifndef CPUX86_HAS_SSE2}
procedure FillWord_Plain(var x;count:SizeInt;value:word);assembler;nostackframe;
asm
        cmp     $3, %edx
        jle     FillWord_3OrLess

        shl     $1, %edx
        movzwl  %cx, %ecx
        imul    $0x00010001, %ecx
        cmp     $16, %edx
        jbe     FillXxxx_U32Pattern_Ladder_4to16
        jmp     FillXxxx_U32Pattern_Plain_16OrMore
end;
{$endif ndef CPUX86_HAS_SSE2}

procedure FillWord_SSE2(var x;count:SizeInt;value:word);assembler;nostackframe;
asm
        cmp     $3, %edx
        jle     FillWord_3OrLess

        shl     $1, %edx
        movzwl  %cx, %ecx
        imul    $0x00010001, %ecx
        cmp     $16, %edx
        jbe     FillXxxx_U32Pattern_Ladder_4to16
        cmp     $FillXxxx_RepStosThreshold_NoERMS, %edx
        jb      FillXxxx_U32Pattern_SSE2_16OrMore
        jmp     FillXxxx_U32Pattern_RepStos_8OrMore
end;

procedure FillWord_SSE2_ERMS(var x;count:SizeInt;value:word);assembler;nostackframe;
asm
        cmp     $3, %edx
        jle     FillWord_3OrLess

        shl     $1, %edx
        movzwl  %cx, %ecx
        imul    $0x00010001, %ecx
        cmp     $16, %edx
        jbe     FillXxxx_U32Pattern_Ladder_4to16
        cmp     $FillXxxx_RepStosThreshold_ERMS, %edx
        jb      FillXxxx_U32Pattern_SSE2_16OrMore
        jmp     FillXxxx_U32Pattern_RepStos_8OrMore
end;

procedure FillWord_Dispatch(var x;count:SizeInt;value:word); forward;

var
  FillWord_Impl: procedure(var x;count:SizeInt;value:word) = @FillWord_Dispatch;

procedure FillWord_Dispatch(var x;count:SizeInt;value:word);
begin
  if not fpc_cpucodeinit_performed then
    begin
      {$ifdef CPUX86_HAS_SSE2} FillWord_SSE2 {$else} FillWord_Plain {$endif} (x, count, value);
      exit;
    end;
  if fast_large_repmovstosb then
    FillWord_Impl := @FillWord_SSE2_ERMS
  else {$ifndef CPUX86_HAS_SSE2} if has_sse2_support then {$endif}
    FillWord_Impl := @FillWord_SSE2
{$ifndef CPUX86_HAS_SSE2}
  else
    FillWord_Impl := @FillWord_Plain
{$endif ndef CPUX86_HAS_SSE2};
  FillWord_Impl(x, count, value);
end;

procedure FillWord(var x;count:SizeInt;value:word);
begin
  FillWord_Impl(x, count, value);
end;
{$endif FPC_SYSTEM_HAS_FILLWORD}


{$if not defined(darwin) and
     not defined(FPC_SYSTEM_HAS_FILLDWORD)}
{$define FPC_SYSTEM_HAS_FILLDWORD}
procedure FillDWord_4OrLess; assembler; nostackframe;
asm
        cmp     $1, %edx
        jl      .LQuit
        mov     %ecx, (%eax)
        je      .LQuit
        mov     %ecx, 4(%eax)
        mov     %ecx, -8(%eax,%edx,4)
        mov     %ecx, -4(%eax,%edx,4)
.LQuit:
end;

{$ifndef CPUX86_HAS_SSE2}
procedure FillDWord_Plain(var x;count:SizeInt;value:dword);assembler;nostackframe;
asm
        cmp     $4, %edx
        jle     FillDWord_4OrLess
        shl     $2, %edx
        jmp     FillXxxx_U32Pattern_Plain_16OrMore
end;
{$endif ndef CPUX86_HAS_SSE2}

procedure FillDWord_SSE2(var x;count:SizeInt;value:dword);assembler;nostackframe;
asm
        cmp     $4, %edx
        jle     FillDWord_4OrLess
        shl     $2, %edx
        cmp     $FillXxxx_RepStosThreshold_NoERMS, %edx
        jb      FillXxxx_U32Pattern_SSE2_16OrMore
        jmp     FillXxxx_U32Pattern_RepStos_8OrMore
end;

procedure FillDWord_SSE2_ERMS(var x;count:SizeInt;value:dword);assembler;nostackframe;
asm
        cmp     $4, %edx
        jle     FillDWord_4OrLess
        shl     $2, %edx
        cmp     $FillXxxx_RepStosThreshold_ERMS, %edx
        jb      FillXxxx_U32Pattern_SSE2_16OrMore
        jmp     FillXxxx_U32Pattern_RepStos_8OrMore
end;

procedure FillDWord_Dispatch(var x;count:SizeInt;value:dword); forward;

var
  FillDWord_Impl: procedure(var x;count:SizeInt;value:dword) = @FillDWord_Dispatch;

procedure FillDWord_Dispatch(var x;count:SizeInt;value:dword);
begin
  if not fpc_cpucodeinit_performed then
    begin
      {$ifdef CPUX86_HAS_SSE2} FillDWord_SSE2 {$else} FillDWord_Plain {$endif}(x, count, value);
      exit;
    end;
  if fast_large_repmovstosb then
    FillDWord_Impl := @FillDWord_SSE2_ERMS
  else {$ifndef CPUX86_HAS_SSE2} if has_sse2_support then {$endif}
    FillDWord_Impl := @FillDWord_SSE2
{$ifndef CPUX86_HAS_SSE2}
  else
    FillDWord_Impl := @FillDWord_Plain
{$endif ndef CPUX86_HAS_SSE2};
  FillDWord_Impl(x, count, value);
end;

procedure FillDWord(var x;count:SizeInt;value:dword);
begin
  FillDWord_Impl(x, count, value);
end;
{$endif FPC_SYSTEM_HAS_FILLDWORD}


{$if not defined(darwin) and
     not defined(FPC_SYSTEM_HAS_FILLQWORD)}
{$define FPC_SYSTEM_HAS_FILLQWORD}
{$ifndef CPUX86_HAS_SSE2}
procedure FillQWord_Plain(var x;count:SizeInt;value:QWord);assembler;nostackframe;
{ eax = x, edx = count, [esp + 4] = value }
asm
        test    %edx, %edx { Don't care about clever things like separate small branches or aligning writes by misaligning the pattern, the function is dead. }
        jle     .LQuit
        push    %esi
        mov     4+4(%esp), %esi { esi = value[0:31] }
        mov     4+8(%esp), %ecx { ecx = value[32:63] }
.balign 16
.LLoop:
        mov     %esi, (%eax)
        mov     %ecx, 4(%eax)
        add     $8, %eax
        sub     $1, %edx
        jnz     .LLoop
        pop     %esi
.LQuit:
end;
{$endif ndef CPUX86_HAS_SSE2}

procedure {$ifdef CPUX86_HAS_SSE2} FillQWord {$else} FillQWord_SSE2 {$endif}(var x;count:SizeInt;value:QWord);assembler;nostackframe;
{ eax = x, edx = count, [esp + 4] = value }
asm
        cmp     $4, %edx
        jle     .L4OrLess
        movq    4(%esp), %xmm0
        punpcklqdq %xmm0, %xmm0
        { Stack is 12 bytes:
          [esp] = return address, [esp + 4] = value (not required anymore).
          Convert to 4 bytes expected by FillXxxx_MoreThanTwoXMMs:
          [esp] = return address. }
        mov     (%esp), %ecx
        add     $8, %esp
        mov     %ecx, (%esp)
        shl     $3, %edx
        movdqu  %xmm0, (%eax)
        movdqu  %xmm0, -16(%eax,%edx)
        test    $7, %eax { Since misaligning the pattern is not very trivial, shortcut if x is aligned. }
        jz      FillXxxx_MoreThanTwoXMMs
        mov     %eax, %ecx { Misalign uint64s in xmm0 by the misalignment of x. }
        shl     $3, %ecx
        and     $63, %ecx
        movd    %ecx, %xmm2
        movdqa  %xmm0, %xmm1
        psllq   %xmm2, %xmm1
        neg     %ecx      { Can also do not ecx; and $63, %ecx; inc ecx to support ecx = 0, as SSE shifts correctly handle count >= bitsizeof.  }
        and     $63, %ecx { But in the definitely unaligned case, ecx = 0 is impossible. }
        movd    %ecx, %xmm2
        psrlq   %xmm2, %xmm0
        por     %xmm1, %xmm0
        jmp     FillXxxx_MoreThanTwoXMMs

.L4OrLess: { Doing this with 64-bit half-XMM MOVQs is a lot simpler but 2x slower (Coffee Lake). :\ }
        cmp     $1, %edx
        jl      .LQuit
        mov     4(%esp), %ecx
        mov     %ecx, (%eax)
        je      .LSecondHalfOf1
        mov     %ecx, 8(%eax)
        mov     %ecx, -16(%eax,%edx,8)
        mov     %ecx, -8(%eax,%edx,8)
        mov     8(%esp), %ecx
        mov     %ecx, 4(%eax)
        mov     %ecx, 12(%eax)
        mov     %ecx, -12(%eax,%edx,8)
        mov     %ecx, -4(%eax,%edx,8)
.LQuit:
        ret     $8
.LSecondHalfOf1:
        mov     8(%esp), %ecx
        mov     %ecx, 4(%eax)
end;

{$ifndef CPUX86_HAS_SSE2}
procedure FillQWord_Dispatch(var x;count:SizeInt;value:qword); forward;

var
  FillQWord_Impl: procedure(var x;count:SizeInt;value:qword) = @FillQWord_Dispatch;

procedure FillQWord_Dispatch(var x;count:SizeInt;value:qword);
begin
  if not fpc_cpucodeinit_performed then
    begin
      FillQWord_Plain(x, count, value);
      exit;
    end;
  if has_sse2_support then
    FillQWord_Impl := @FillQWord_SSE2
  else
    FillQWord_Impl := @FillQWord_Plain;
  FillQWord_Impl(x, count, value);
end;

procedure FillQWord(var x;count:SizeInt;value:qword);
begin
  FillQWord_Impl(x, count, value);
end;
{$endif ndef CPUX86_HAS_SSE2 (need FillQWord dispatcher)}
{$endif FPC_SYSTEM_HAS_FILLQWORD}


{$ifndef FPC_SYSTEM_HAS_INDEXBYTE}
{$define FPC_SYSTEM_HAS_INDEXBYTE}
{$ifndef CPUX86_HAS_SSE2}
function IndexByte_Plain(Const buf;len:SizeInt;b:byte):SizeInt; assembler; nostackframe;
{ eax = buf, edx = len, cl = b }
asm
        test  %edx,%edx
        jz    .Lnothing0
        push  %eax                  { save initial value of 'buf' }

        test  $3,%al
        jz    .Laligned4
.Lalignloop:                        { align to 4 bytes }
        cmp   %cl,(%eax)
        je    .Lfoundateax
        inc   %eax
        dec   %edx
        jz    .Lnothing1
        test  $3,%al
        jnz   .Lalignloop

.Laligned4:                         { align to 8 bytes }
        push  %esi
        push  %edi

        mov    %cl,%ch              { prepare pattern }
        movzwl %cx,%esi
        shl    $16,%ecx
        or     %esi,%ecx

        test  $7,%al
        jz    .Lloop
        test  %edx,%edx             { Adjust buf+len and reuse the second unroll from the loop body. Careful with len < 0 (esp. len = −1). }
        jl    .Ldontfixuplen
        add   $4,%edx
.Ldontfixuplen:
        sub   $4,%eax
        jmp   .Lalignfrom4to8

.balign 16
.Lloop:                             { Requires 8-byte alignment of eax, to safely over-read up to 7 bytes on last iteration. }
        mov   (%eax),%esi           { load dword }
        xor   %ecx,%esi             { XOR with pattern, bytes equal to target are now 0 }
        lea   -0x01010101(%esi),%edi
        not   %esi
        and   $0x80808080,%esi
        and   %edi,%esi             { (x-0x01010101) and (not x) and 0x80808080 }
        jnz   .Lfound0              { one of the bytes matches }

.Lalignfrom4to8:
        mov   4(%eax),%esi
        xor   %ecx,%esi
        lea   -0x01010101(%esi),%edi
        not   %esi
        and   $0x80808080,%esi
        and   %edi,%esi
        jnz   .Lfound1

        add   $8,%eax
        sub   $8,%edx
        ja    .Lloop
.Lnothing3:
        pop   %edi
        pop   %esi
.Lnothing1:
        pop   %edx
.Lnothing0:
        or    $-1,%eax
        ret

.Lfound1:
        sub   $4,%edx
        jbe   .Lnothing3
        add   $4,%eax
.Lfound0:
        bsf   %esi,%esi
        shr   $3,%esi
        cmp   %edx,%esi             { Garbage after remaining length? }
        jae   .Lnothing3
        add   %esi,%eax
        pop   %edi
        pop   %esi
.Lfoundateax:
        pop   %ecx
        sub   %ecx,%eax
end;
{$endif ndef CPUX86_HAS_SSE2}

function {$ifdef CPUX86_HAS_SSE2} IndexByte {$else} IndexByte_SSE2 {$endif} (const buf;len:SizeInt;b:byte):SizeInt; assembler; nostackframe;
asm
        test      %edx, %edx
        jz        .Lnotfound                 { exit if len=0 }

        movd      %ecx, %xmm1
        mov       %eax, %ecx
        punpcklbw %xmm1, %xmm1
        punpcklbw %xmm1, %xmm1
        and       $4095, %ecx
        pshufd    $0, %xmm1, %xmm1

        cmp       $4080, %ecx
        ja        .LCrossPage

        movdqu    (%eax), %xmm0              { Analyze first 16 bytes, unaligned. }
        pcmpeqb   %xmm1, %xmm0
        pmovmskb  %xmm0, %ecx
        test      %ecx, %ecx
        jz        .LContinueAligned

        bsf       %ecx, %eax
        cmp       %edx, %eax
        jae       .Lnotfound
        ret

        .byte     144                        { Make .balign 16 before .Lloop a no-op. }
.LContinueAligned:
        cmp       $16, %edx                  { Length might be explicitly set to 16 or less; if so, skip a bit of work. }
        jbe       .Lnotfound                 { (Or rather, this check is *required* unless jumping to .Lcontinue instead of going directly to .Lloop) }

        push      %ebx
        lea       16(%eax), %ecx             { eax = original ptr, ecx = buf + 16 for aligning & shifts. }
        and       $-0x10, %ecx               { first aligned address after buf }
        sub       %eax, %ecx                 { ecx=number of valid bytes, eax=original ptr }

    .balign 16
.Lloop:
        movdqa    (%eax,%ecx), %xmm0         { eax and ecx may have any values, }
        add       $16, %ecx                  { but their sum is evenly divisible by 16. }
        pcmpeqb   %xmm1, %xmm0
        pmovmskb  %xmm0, %ebx
        test      %ebx, %ebx
        jnz       .Lmatch
.Lcontinue:
        cmp       %ecx, %edx
        ja        .Lloop
        pop       %ebx
.Lnotfound:
        or        $-1, %eax
        ret

.LCrossPage:
        push      %ebx
        lea       16(%eax), %ecx             { eax = original ptr, ecx = buf + 16 for aligning & shifts. }
        and       $-0x10, %ecx               { first aligned address after buf }
        movdqa    -16(%ecx), %xmm0           { Fetch first 16 bytes (up to 15 bytes before target) }
        sub       %eax, %ecx                 { ecx=number of valid bytes, eax=original ptr }

        pcmpeqb   %xmm1, %xmm0               { compare with pattern and get bitmask }
        pmovmskb  %xmm0, %ebx

        shl       %cl, %ebx                  { shift valid bits into high word }
        and       $0xffff0000, %ebx          { clear low word containing invalid bits }
        shr       %cl, %ebx                  { shift back }
        jz        .Lcontinue
.Lmatch:
        bsf       %ebx, %ebx
        lea       -16(%ecx,%ebx), %eax
        pop       %ebx
        cmp       %eax, %edx                 { check against the buffer length }
        jbe       .Lnotfound
end;

{$ifndef CPUX86_HAS_SSE2}
function IndexByte_Dispatch(const buf;len:SizeInt;b:byte):SizeInt; forward;

var
  IndexByte_Impl: function(const buf;len:SizeInt;b:byte):SizeInt = @IndexByte_Dispatch;

function IndexByte_Dispatch(const buf;len:SizeInt;b:byte):SizeInt;
begin
  if not fpc_cpucodeinit_performed then
    exit(IndexByte_Plain(buf,len,b));
  if has_sse2_support then
    IndexByte_Impl:=@IndexByte_SSE2
  else
    IndexByte_Impl:=@IndexByte_Plain;
  result:=IndexByte_Impl(buf,len,b);
end;

function IndexByte(const buf;len:SizeInt;b:byte):SizeInt;
begin
  result:=IndexByte_Impl(buf,len,b);
end;
{$endif ndef CPUX86_HAS_SSE2}
{$endif FPC_SYSTEM_HAS_INDEXBYTE}


{$ifndef FPC_SYSTEM_HAS_INDEXWORD}
{$define FPC_SYSTEM_HAS_INDEXWORD}
{$ifndef CPUX86_HAS_SSE2}
function IndexWord_Plain(Const buf;len:SizeInt;b:word):SizeInt; assembler; nostackframe;
asm
        test    %edx, %edx
        jz      .LNotFound
        push    %eax
.LWordwise_Body: { Loop does not cross cache line if the function entry is aligned on 16 bytes. }
        cmp     %cx, (%eax)
        je      .LFound
        add     $2, %eax
        dec     %edx
        jnz     .LWordwise_Body
        pop     %edx
.LNotFound:
        or      $-1, %eax
        ret

.LFound:
        pop     %edx
        sub     %edx, %eax
        shr     $1, %eax
end;
{$endif ndef CPUX86_HAS_SSE2}

function {$ifdef CPUX86_HAS_SSE2} IndexWord {$else} IndexWord_SSE2 {$endif} (const buf;len:SizeInt;b:word):SizeInt; assembler; nostackframe;
asm
        test      %edx, %edx       { exit if len=0 }
        je        .Lnotfound
        push      %ebx
        movd      %ecx, %xmm1
        punpcklwd %xmm1, %xmm1
        pshufd    $0, %xmm1, %xmm1
        lea       16(%eax), %ecx
        and       $-16, %ecx
        movdqa    -16(%ecx), %xmm0 { Fetch first 16 bytes (up to 14 bytes before target) }
        sub       %eax, %ecx

        test      $1, %eax         { if buffer isn't aligned to word boundary, }
        jnz       .Lunaligned      { use a different algorithm }

        pcmpeqw   %xmm1, %xmm0
        pmovmskb  %xmm0, %ebx

        shl       %cl, %ebx
        and       $0xffff0000, %ebx
        shr       %cl, %ebx
        shr       $1, %ecx         { ecx=number of valid bytes }
        test      %ebx, %ebx
        jz        .Lcontinue
.Lmatch:
        bsf       %ebx, %ebx
        shr       $1, %ebx         { in words }
        lea       -8(%ecx,%ebx), %eax
        pop       %ebx
        cmp       %eax, %edx
        jbe       .Lnotfound       { if match is after the specified length, ignore it }
        ret

.balign 16
.Lloop:
        movdqa    (%eax,%ecx,2), %xmm0
        add       $8, %ecx
        pcmpeqw   %xmm1, %xmm0
        pmovmskb  %xmm0, %ebx
        test      %ebx, %ebx
        jnz       .Lmatch
.Lcontinue:
        cmp       %ecx, %edx
        ja        .Lloop
        pop       %ebx
.Lnotfound:
        or        $-1, %eax
        ret

.Lunaligned:
        push      %esi
        movdqa    %xmm1, %xmm2     { (mis)align the pattern (in this particular case: }
        psllw     $8, %xmm1        {   swap bytes of each word of pattern) }
        psrlw     $8, %xmm2
        por       %xmm2, %xmm1

        pcmpeqb   %xmm1, %xmm0
        pmovmskb  %xmm0, %ebx

        shl       %cl, %ebx
        and       $0xffff0000, %ebx
        shr       %cl, %ebx

        xor       %esi, %esi       { nothing to merge yet }
        add       %edx, %edx       { length words -> bytes }
        jmp       .Lcontinue_u

.balign 16
.Lloop_u:
        movdqa    (%eax,%ecx), %xmm0
        add       $16, %ecx
        pcmpeqb   %xmm1, %xmm0     { compare by bytes }
        shr       $16, %esi        { bit 16 shifts into 0 }
        pmovmskb  %xmm0, %ebx
.Lcontinue_u:
        shl       $1, %ebx         { 15:0 -> 16:1 }
        or        %esi, %ebx       { merge bit 0 from previous round }
        mov       %ebx, %esi
        shr       $1, %ebx         { now AND together adjacent pairs of bits }
        and       %esi, %ebx
        and       $0x5555, %ebx    { also reset odd bits }
        jnz       .Lmatch_u
        cmp       %ecx, %edx
        ja        .Lloop_u
.Lnotfound_u:
        pop       %esi
        pop       %ebx
        or        $-1, %eax
        ret

.Lmatch_u:
        bsf       %ebx, %ebx
        lea       -16(%ecx,%ebx), %eax
        cmp       %eax, %edx
        jbe       .Lnotfound_u     { if match is after the specified length, ignore it }
        sar       $1, %eax         { in words }
        pop       %esi
        pop       %ebx
end;

{$ifndef CPUX86_HAS_SSE2}
function IndexWord_Dispatch(const buf;len:SizeInt;b:word):SizeInt; forward;

var
  IndexWord_Impl: function(const buf;len:SizeInt;b:word):SizeInt = @IndexWord_Dispatch;

function IndexWord_Dispatch(const buf;len:SizeInt;b:word):SizeInt;
begin
  if not fpc_cpucodeinit_performed then
    exit(IndexWord_Plain(buf,len,b));
  if has_sse2_support then
    IndexWord_Impl:=@IndexWord_SSE2
  else
    IndexWord_Impl:=@IndexWord_Plain;
  result:=IndexWord_Impl(buf,len,b);
end;

function IndexWord(const buf;len:SizeInt;b:word):SizeInt; inline;
begin
  result:=IndexWord_Impl(buf,len,b);
end;
{$endif ndef CPUX86_HAS_SSE2}
{$endif FPC_SYSTEM_HAS_INDEXWORD}


{$ifndef FPC_SYSTEM_HAS_INDEXDWORD}
{$define FPC_SYSTEM_HAS_INDEXDWORD}
{$ifndef CPUX86_HAS_SSE2}
function IndexDWord_Plain(Const buf;len:SizeInt;b:DWord):SizeInt; assembler; nostackframe;
asm
        push    %eax
        sub     $4, %eax
.LDWordwise_Next: { Loop does not cross cache line if the function entry is aligned on 16 bytes. }
        add     $4, %eax
        sub     $1, %edx
        jb      .LNotFound
        cmp     %ecx, (%eax)
        jne     .LDWordwise_Next
        pop     %edx
        sub     %edx, %eax
        shr     $2, %eax
        ret

.LNotFound:
        pop     %edx
        mov     $-1, %eax
end;
{$endif ndef CPUX86_HAS_SSE2}

function {$ifdef CPUX86_HAS_SSE2} IndexDWord {$else} IndexDWord_SSE2 {$endif} (const buf;len:SizeInt;b:DWord):SizeInt; assembler; nostackframe;
asm
        push     %eax
        sub      $4, %edx
        jle      .LDwordwise_Prepare
        movd     %ecx, %xmm1
        pshufd   $0, %xmm1, %xmm1
.balign 16 { 1-byte NOP. }
.L4x_Body:
        movdqu   (%eax), %xmm0
        pcmpeqd  %xmm1, %xmm0
        pmovmskb %xmm0, %ecx
        test     %ecx, %ecx
        jnz      .LFoundAtMask
        add      $16, %eax
        sub      $4, %edx
        jg       .L4x_Body

        lea      (%eax,%edx,4), %eax
        movdqu   (%eax), %xmm0
        pcmpeqd  %xmm1, %xmm0
        pmovmskb %xmm0, %ecx
        test     %ecx, %ecx
        jz       .LNothing
.LFoundAtMask:
        bsf      %ecx, %ecx
        add      %ecx, %eax
.LFoundAtEax:
        pop      %edx
        sub      %edx, %eax
        shr      $2, %eax
        ret
        nop { Turns .balign 16 before .LDwordwise_Body into a no-op. }

.LDwordwise_Prepare:
        add      $3, %edx
        cmp      $-1, %edx
        je       .LNothing
.balign 16 { no-op }
.LDwordwise_Body:
        cmp      (%eax), %ecx
        je       .LFoundAtEax
        add      $4, %eax
        sub      $1, %edx
        jae      .LDwordwise_Body
.LNothing:
        pop      %edx
        or       $-1, %eax
end;

{$ifndef CPUX86_HAS_SSE2}
function IndexDWord_Dispatch(const buf;len:SizeInt;b:DWord):SizeInt; forward;

var
  IndexDWord_Impl: function(const buf;len:SizeInt;b:DWord):SizeInt = @IndexDWord_Dispatch;

function IndexDWord_Dispatch(const buf;len:SizeInt;b:DWord):SizeInt;
begin
  if not fpc_cpucodeinit_performed then
    exit(IndexDWord_Plain(buf,len,b));
  if has_sse2_support then
    IndexDWord_Impl:=@IndexDWord_SSE2
  else
    IndexDWord_Impl:=@IndexDWord_Plain;
  result:=IndexDWord_Impl(buf,len,b);
end;

function IndexDWord(const buf;len:SizeInt;b:DWord):SizeInt;
begin
  result:=IndexDWord_Impl(buf,len,b);
end;
{$endif CPUX86_HAS_SSE2}
{$endif FPC_SYSTEM_HAS_INDEXDWORD}


{$ifndef FPC_SYSTEM_HAS_INDEXQWORD}
{$define FPC_SYSTEM_HAS_INDEXQWORD}
function IndexQWord_Plain(const buf;len:SizeInt;b:QWord):SizeInt; assembler; nostackframe;
{ eax = buf, edx = len, [esp+4] = b }
asm
        push    %ebx
        mov     8(%esp), %ecx { ecx = b[0:31] }
        mov     12(%esp), %ebx { ebx = b[32:63] }
        mov     %eax, 8(%esp) { remember original buf }
        sub     $8, %eax

.balign 16 { no-op }
.LQWordwise_Next:
        add     $8, %eax
        sub     $1, %edx
        jb      .LNotFound
        cmp     %ecx, (%eax)
        jne     .LQWordwise_Next
        cmp     %ebx, 4(%eax)
        jne     .LQWordwise_Next
        sub     8(%esp), %eax
        pop     %ebx
        shr     $3, %eax
        ret     $8

.LNotFound:
        pop     %ebx
        mov     $-1, %eax
end;

function {$ifdef CPUX86_HAS_SSE4_1} IndexQWord {$else} IndexQWord_SSE41 {$endif}(Const buf;len:SizeInt;b:QWord):SizeInt; assembler; nostackframe;
{ eax = buf, edx = len, [esp+4] = b }
asm
    cmp      $6, len
    jle      IndexQWord_Plain
    movddup  4(%esp), %xmm0 { xmm0 = pattern of 'b's. }
    mov      %eax, %ecx { ecx = original buf }
    sub      $6, len
.balign 16
.L6x_Loop:
    movdqu   (%eax), %xmm1
    pcmpeqq  %xmm0, %xmm1 { xmm1 = cmpeq(vec 0, pattern) }
    movdqu   16(%eax), %xmm2
    pcmpeqq  %xmm0, %xmm2
    por      %xmm1, %xmm2 { xmm2 = cmpeq(vec 0, pattern) or cmpeq(vec 1, pattern) }
    movdqu   32(%eax), %xmm3
    pcmpeqq  %xmm0, %xmm3
    por      %xmm2, %xmm3 { xmm3 = cmpeq(vec 0, pattern) or cmpeq(vec 1, pattern) or cmpeq(vec 2, pattern) }
    ptest    %xmm3, %xmm3
    jnz      .LFound
    add      $48, %eax
    sub      $6, len
    jge      .L6x_Loop
    lea      (%eax,%edx,8), %eax { Point to last 3 vectors. }
    cmp      $-5, len
    jge      .L6x_Loop { Reuse .L6x_Loop to compare last 3 vectors, if not compared already. }
    mov      $-1, %eax
    ret      $8

.LFound:
    sub      %ecx, %eax
    ptest    %xmm1, %xmm1
    jnz      .LFoundAtXmm1
    ptest    %xmm2, %xmm2
    jnz      .LFoundAtXmm2
    add      $16, %eax
    movdqa   %xmm3, %xmm2
.LFoundAtXmm2:
    add      $16, %eax
    movdqa   %xmm2, %xmm1
.LFoundAtXmm1:
    pmovmskb %xmm1, %ecx
    bsf      %ecx, %ecx
    add      %ecx, %eax
    shr      $3, %eax
end;

{$ifndef CPUX86_HAS_SSE4_1}
function IndexQWord_Dispatch(const buf;len:SizeInt;b:QWord):SizeInt; forward;

var
  IndexQWord_Impl: function(const buf;len:SizeInt;b:QWord):SizeInt = @IndexQWord_Dispatch;

function IndexQWord_Dispatch(const buf;len:SizeInt;b:QWord):SizeInt;
begin
  if not fpc_cpucodeinit_performed then
    exit(IndexQWord_Plain(buf,len,b));
  if has_sse41_support then
    IndexQWord_Impl:=@IndexQWord_SSE41
  else
    IndexQWord_Impl:=@IndexQWord_Plain;
  result:=IndexQWord_Impl(buf,len,b);
end;

function IndexQWord(const buf;len:SizeInt;b:QWord):SizeInt;
begin
  result:=IndexQWord_Impl(buf,len,b);
end;
{$endif ndef CPUX86_HAS_SSE4_1}
{$endif FPC_SYSTEM_HAS_INDEXQWORD}


{$ifndef FPC_SYSTEM_HAS_COMPAREBYTE}
{$define FPC_SYSTEM_HAS_COMPAREBYTE}
{$ifndef CPUX86_HAS_SSE2}
function CompareByte_Plain(Const buf1,buf2;len:SizeInt):SizeInt; assembler; nostackframe;
asm
        { eax = buf1, edx = buf2, ecx = len }
        push    %ebx
        sub     %eax, %edx { edx = buf2 - buf1 }
        cmp     $3, %ecx
        jle     .LBytewise_Prepare

        { Align buf1 on 4 bytes. }
        mov     (%edx,%eax), %ebx
        cmp     (%eax), %ebx
        jne     .L4xDiffer
        lea     -4(%eax,%ecx), %ecx { ecx = buf1 end - (4 + buf1 and -4) = count remaining }
        and     $-4, %eax
        sub     %eax, %ecx

.balign 16
.L4x_Next:
        add     $4, %eax
        sub     $4, %ecx { at .LLast4, ecx is 4 less than remaining bytes }
        jle     .LLast4
        mov     (%edx,%eax), %ebx
        cmp     (%eax), %ebx
        je      .L4x_Next
.L4xDiffer:
        mov     (%eax), %edx
{$ifdef CPUX86_HAS_BSWAP}
        bswap   %ebx
        bswap   %edx
{$else}
        rol     $8, %bx
        rol     $16, %ebx
        rol     $8, %bx
        rol     $8, %dx
        rol     $16, %edx
        rol     $8, %dx
{$endif}
        cmp     %ebx, %edx
.LDoSbb:
        sbb     %eax, %eax
        or      $1, %eax
        pop     %ebx
        ret

.LLast4:
        add     %ecx, %eax
        mov     (%edx,%eax), %ebx
        cmp     (%eax), %ebx
        jne     .L4xDiffer
        xor     %eax, %eax
        pop     %ebx
        ret

.LBytewise_Prepare:
        sub     $1, %ecx
        jb      .LNothing
.balign 16 { no-op }
.LBytewise_Body:
        movzbl  (%edx,%eax), %ebx
        cmp     %bl, (%eax)
        jne     .LDoSbb
        add     $1, %eax
        sub     $1, %ecx
        jae     .LBytewise_Body
.LNothing:
        xor     %eax, %eax
        pop     %ebx
end;
{$endif ndef CPUX86_HAS_SSE2}

label
  CompareByte_1OrLess, CompareByte_CantOverReadBoth_AVX2;

function CompareByte_SSE2(const buf1, buf2; len: SizeInt): SizeInt; assembler; nostackframe;
asm
        { eax = buf1, edx = buf2, ecx = len }
        cmp      $1, %ecx
        jle      CompareByte_1OrLess

        push     %ebx
        cmp      $16, %ecx
        jae      .LVecOrMore

        { 2 to 15 bytes: check for page cross. Pessimistic variant that has false positives, but uses 1 less register and 2 less instructions. }
        mov      %eax, %ebx
        or       %edx, %ebx
        and      $4095, %ebx
        cmp      $4080, %ebx
        ja       .LCantOverReadBoth

        { Over-read both as XMMs. }
        movdqu   (%eax), %xmm0
        movdqu   (%edx), %xmm1
        pcmpeqb  %xmm1, %xmm0
        pmovmskb %xmm0, %ebx
        inc      %bx { Here and below, 2-byte 'inc r16' after 'pmovmskb' can be replaced with 5-byte 'add $1, r16' or 6-byte 'xor $65535, r32'. }
        jz       .LNothing
        bsf      %ebx, %ebx
        cmp      %ecx, %ebx { Ignore garbage beyond 'len'. }
        jae      .LNothing
        movzbl   (%eax,%ebx), %eax
        movzbl   (%edx,%ebx), %edx
        sub      %edx, %eax
        pop      %ebx
        ret

.LNothing:
        pop      %ebx
        xor      %eax, %eax
        ret

.LAligned32xLoop_TwoVectorsDiffer:
        add      %eax, %edx { restore edx = buf2 }
        pmovmskb %xmm0, %ecx { Is there a difference in the first vector? }
        inc      %cx
        jz       .LVec1Differs { No difference in the first vector, xmm0 is all ones, ebx = pmovmskb(pcmpeqb(buf1 + 16, buf2 + 16)) from the loop body. }
        mov      %ecx, %ebx
.LVec0Differs:
        bsf      %ebx, %ebx
        movzbl   (%eax,%ebx), %eax
        movzbl   (%edx,%ebx), %edx
        sub      %edx, %eax
        pop      %ebx
        ret

        .byte    144 { Turn .balign 16 before .LAligned32xLoop_Body into a no-op. }
CompareByte_CantOverReadBoth_AVX2:
        cmp      $16, %ecx
        jb       .LCantOverReadBoth
.LVecOrMore:
        { Compare first vectors. }
        movdqu   (%eax), %xmm0
        movdqu   (%edx), %xmm1
        pcmpeqb  %xmm1, %xmm0
        pmovmskb %xmm0, %ebx
        inc      %bx
        jnz      .LVec0Differs

        sub      $32, %ecx { now ecx is len - 32. }
        jbe      .LLastVec

        { Compare second vectors. }
        movdqu   16(%eax), %xmm0
        movdqu   16(%edx), %xmm1
        pcmpeqb  %xmm1, %xmm0
        pmovmskb %xmm0, %ebx
        inc      %bx
        jnz      .LVec1Differs

        cmp      $32, %ecx
        jbe      .LLastTwoVectors

        { More than four vectors: aligned loop. }
        lea      -32(%eax,%ecx), %ecx { buffer end - last two vectors handled separately - first two vectors already analyzed (by the fact ecx was still len - 32) }
        sub      %eax, %edx { edx = buf2 - buf1 }
        and      $-16, %eax { Align buf1. First two vectors already analyzed are skipped by +32 on the first loop iteration. }
        sub      %eax, %ecx { ecx = count to be handled with loop }
.balign 16 { No-op. }
.LAligned32xLoop_Body:
        add      $32, %eax
        { Compare two XMMs, reduce the result with 'and'. }
        movdqu   (%edx,%eax), %xmm0
        pcmpeqb  (%eax), %xmm0 { xmm0 = pcmpeqb(buf1, buf2) }
        movdqu   16(%edx,%eax), %xmm1
        pcmpeqb  16(%eax), %xmm1
        pand     %xmm0, %xmm1 { xmm1 = xmm0 and pcmpeqb(buf1 + 16, buf2 + 16) }
        pmovmskb %xmm1, %ebx
        inc      %bx
        jnz      .LAligned32xLoop_TwoVectorsDiffer
        sub      $32, %ecx
        ja       .LAligned32xLoop_Body
        add      %eax, %edx { restore edx = buf2 }
        add      $32, %ecx
.LLastTwoVectors:
        movdqu   (%eax,%ecx), %xmm0
        movdqu   (%edx,%ecx), %xmm1
        pcmpeqb  %xmm1, %xmm0
        pmovmskb %xmm0, %ebx
        inc      %bx
        jnz      .LVecEm2Differs
.LLastVec:
        movdqu   16(%eax,%ecx), %xmm0
        movdqu   16(%edx,%ecx), %xmm1
        pcmpeqb  %xmm1, %xmm0
        pmovmskb %xmm0, %ebx
        inc      %bx
        jnz      .LVecEm1Differs
        pop      %ebx
        xor      %eax, %eax
        ret

.LVec1Differs:
        xor      %ecx, %ecx
.LVecEm1Differs:
        add      $16, %ecx
.LVecEm2Differs:
        bsf      %ebx, %ebx
        add      %ecx, %ebx
        movzbl   (%eax,%ebx), %eax
        movzbl   (%edx,%ebx), %edx
        sub      %edx, %eax
        pop      %ebx
        ret

.LCantOverReadBoth:
        cmp      $3, %ecx
        jle      .L2to3
        push     %esi
        mov      (%eax), %ebx
        mov      (%edx), %esi
        cmp      %esi, %ebx
        jne      .L4xDiffer
        cmp      $8, %ecx
        jbe      .LLast4x
        mov      4(%eax), %ebx
        mov      4(%edx), %esi
        cmp      %esi, %ebx
        jne      .L4xDiffer
        mov      -8(%eax,%ecx), %ebx
        mov      -8(%edx,%ecx), %esi
        cmp      %esi, %ebx
        jne      .L4xDiffer
.LLast4x:
        mov      -4(%eax,%ecx), %ebx
        mov      -4(%edx,%ecx), %esi
        cmp      %esi, %ebx
        jne      .L4xDiffer
        pop      %esi
        pop      %ebx
        xor      %eax, %eax
        ret

.L4xDiffer:
        bswap    %ebx
        bswap    %esi
        cmp      %esi, %ebx
        pop      %esi
        sbb      %eax, %eax
        or       $1, %eax
        pop      %ebx
        ret

.L2to3:
        movzwl   (%edx), %ebx
        bswap    %ebx
        shr      $1, %ebx
        mov      -1(%edx,%ecx), %bl
        movzwl   (%eax), %edx
        bswap    %edx
        shr      $1, %edx
        mov      -1(%eax,%ecx), %dl
        mov      %edx, %eax
        sub      %ebx, %eax
        pop      %ebx
        ret

CompareByte_1OrLess:
        jl       .LUnbounded_Prepare
        movzbl   (%eax), %eax
        movzbl   (%edx), %edx
        sub      %edx, %eax
        ret

.LUnbounded_Prepare:
        sub      %eax, %edx { edx = buf2 - buf1 }
        test     %ecx, %ecx
        jnz      .LUnbounded_Body
        xor      %eax, %eax
        ret

.balign 16
.LUnbounded_Next:
        add      $1, %eax
.LUnbounded_Body:
        movzbl   (%edx,%eax), %ecx
        cmp      %cl, (%eax)
        je       .LUnbounded_Next
        sbb      %eax, %eax
        or       $1, %eax
end;

function {$ifdef CPUX86_HAS_BMI1} CompareByte {$else} CompareByte_AVX2 {$endif} (const buf1, buf2; len: SizeInt): SizeInt; assembler; nostackframe;
asm
        { eax = buf1, edx = buf2, ecx = len }
        cmp       $1, %ecx
        jle       CompareByte_1OrLess

        push      %ebx
        cmp       $32, %ecx
        jae       .LVecOrMore

        { 2 to 31 bytes: check for page cross. Pessimistic variant that has false positives, but uses 1 less register and 2 less instructions. }
        mov       %eax, %ebx
        or        %edx, %ebx
        and       $4095, %ebx
        cmp       $4064, %ebx
        ja        CompareByte_CantOverReadBoth_AVX2

        { Over-read both as YMMs. }
        vmovdqu   (%eax), %ymm0
        vpcmpeqb  (%edx), %ymm0, %ymm0
        vpmovmskb %ymm0, %ebx
        inc       %ebx
        { bzhi      %ecx, %ebx, %ecx }
        .byte     0xc4,0xe2,0x70,0xf5,0xcb { bootstrap compiler doesn't know bzhi }
        jnz       .LVec0Differs
        vzeroupper
        pop       %ebx
        xor       %eax, %eax
        ret

        .byte     144 { Turn .balign 16 before .LAligned64xLoop_Body into a no-op. }
.LAligned64xLoop_TwoVectorsDiffer:
        add       %eax, %edx { restore edx = buf2 }
        vpmovmskb %ymm0, %ecx { Is there a difference in the first vector? }
        inc       %ecx
        jz        .LVec1Differs { No difference in the first vector, ymm0 is all ones, ebx = vpmovmskb(vpcmpeqb(buf1 + 32, buf2 + 32)) from the loop body. }
        mov       %ecx, %ebx
.LVec0Differs:
        vzeroupper
        tzcnt     %ebx, %ebx
        movzbl    (%eax,%ebx), %eax
        movzbl    (%edx,%ebx), %edx
        sub       %edx, %eax
        pop       %ebx
        ret

.LVecOrMore:
        { Compare first vectors. }
        vmovdqu   (%eax), %ymm0
        vpcmpeqb  (%edx), %ymm0, %ymm0
        vpmovmskb %ymm0, %ebx
        inc       %ebx
        jnz       .LVec0Differs

        sub       $64, %ecx { now ecx is len - 64. }
        jbe       .LLastVec

        { Compare second vectors. }
        vmovdqu   32(%eax), %ymm0
        vpcmpeqb  32(%edx), %ymm0, %ymm0
        vpmovmskb %ymm0, %ebx
        inc       %ebx
        jnz       .LVec1Differs

        cmp       $64, %ecx
        jbe       .LLastTwoVectors

        { More than four vectors: aligned loop. }
        lea       -64(%eax,%ecx), %ecx { buffer end - last two vectors handled separately - first two vectors already analyzed (by the fact ecx was still len - 64) }
        sub       %eax, %edx { edx = buf2 - buf1 }
        and       $-32, %eax { Align buf1. First two vectors already analyzed are skipped by +64 on the first loop iteration. }
        sub       %eax, %ecx { ecx = count to be handled with loop }
.balign 16 { No-op. }
.LAligned64xLoop_Body:
        add       $64, %eax
        { Compare two YMMs, reduce the result with 'and'. }
        vmovdqu   (%edx,%eax), %ymm0
        vpcmpeqb  (%eax), %ymm0, %ymm0 { ymm0 = vpcmpeqb(buf1, buf2) }
        vmovdqu   32(%edx,%eax), %ymm1
        vpcmpeqb  32(%eax), %ymm1, %ymm1
        vpand     %ymm0, %ymm1, %ymm1 { ymm1 = ymm0 and vpcmpeqb(buf1 + 32, buf2 + 32) }
        vpmovmskb %ymm1, %ebx
        inc       %ebx
        jnz       .LAligned64xLoop_TwoVectorsDiffer
        sub       $64, %ecx
        ja        .LAligned64xLoop_Body
        add       %eax, %edx { restore edx = buf2 }
        add       $64, %ecx
.LLastTwoVectors:
        vmovdqu   (%eax,%ecx), %ymm0
        vpcmpeqb  (%edx,%ecx), %ymm0, %ymm0
        vpmovmskb %ymm0, %ebx
        inc       %ebx
        jnz       .LVecEm2Differs
.LLastVec:
        vmovdqu   32(%eax,%ecx), %ymm0
        vpcmpeqb  32(%edx,%ecx), %ymm0, %ymm0
        vpmovmskb %ymm0, %ebx
        inc       %ebx
        jnz       .LVecEm1Differs
        vzeroupper
        pop       %ebx
        xor       %eax, %eax
        ret

.LVec1Differs:
        xor      %ecx, %ecx
.LVecEm1Differs:
        add      $32, %ecx
.LVecEm2Differs:
        vzeroupper
        tzcnt    %ebx, %ebx
        add      %ecx, %ebx
        movzbl   (%eax,%ebx), %eax
        movzbl   (%edx,%ebx), %edx
        sub      %edx, %eax
        pop      %ebx
end;

{$ifndef CPUX86_HAS_BMI1}
function CompareByte_Dispatch(const buf1, buf2; len: SizeInt): SizeInt; forward;

var
  CompareByte_Impl: function(const buf1, buf2; len: SizeInt): SizeInt = @CompareByte_Dispatch;

function CompareByte_Dispatch(const buf1, buf2; len: SizeInt): SizeInt;
begin
  if not fpc_cpucodeinit_performed then
    exit({$ifdef CPUX86_HAS_SSE2} CompareByte_SSE2 {$else} CompareByte_Plain {$endif} (buf1, buf2, len));
  if has_avx2_support then
    CompareByte_Impl:=@CompareByte_AVX2
  else {$ifndef CPUX86_HAS_SSE2} if has_sse2_support then {$endif}
    CompareByte_Impl:=@CompareByte_SSE2
{$ifndef CPUX86_HAS_SSE2}
  else
    CompareByte_Impl:=@CompareByte_Plain
{$endif};
  result:=CompareByte_Impl(buf1, buf2, len);
end;

function CompareByte(const buf1, buf2; len: SizeInt): SizeInt;
begin
  result:=CompareByte_Impl(buf1, buf2, len);
end;
{$endif ndef CPUX86_HAS_BMI1 (need CompareByte dispatcher)}
{$endif FPC_SYSTEM_HAS_COMPAREBYTE}


{$ifndef FPC_SYSTEM_HAS_COMPAREWORD}
{$define FPC_SYSTEM_HAS_COMPAREWORD}
{$ifndef CPUX86_HAS_SSE2}
function CompareWord_Plain(Const buf1,buf2;len:SizeInt):SizeInt; assembler; nostackframe;
asm
        push    %ebx
        sub     %eax, %edx { edx = buf2 - buf1 }
        lea     -4(%ecx), %ebx { Go wordwise if ecx <= 3 or ecx > 1073741823 (High(int32) div 2) ==> uint32(ecx - 4) > 1073741819. }
        cmp     $1073741819, %ebx
        ja      .LWordwise_Prepare
        test    $2, %al
        je      .LAlignedToPtrUintOrNaturallyMisaligned
        movzwl  (%edx,%eax), %ebx
        cmp     %bx, (%eax)
        jne     .LDoSbb
        add     $2, %eax
        sub     $1, %ecx
.LAlignedToPtrUintOrNaturallyMisaligned:
        sub     $2, %ecx
.balign 16
.LPtrUintWise_Next:
        mov     (%edx,%eax), %ebx
        cmp     %ebx, (%eax)
        jne     .LPtrUintsDiffer
        add     $4, %eax
        sub     $2, %ecx
        jg      .LPtrUintWise_Next
        lea     (%eax,%ecx,2), %eax
        mov     (%edx,%eax), %ebx
        cmp     %ebx, (%eax)
        jne     .LPtrUintsDiffer
        pop     %ebx
        xor     %eax, %eax
        ret

.LPtrUintsDiffer:
        cmp     %bx, (%eax)
        jne     .LDoSbb
        shr     $16, %ebx
        cmp     %bx, 2(%eax)
.LDoSbb:
        sbb     %eax, %eax
        or      $1, %eax
        pop     %ebx
        ret

.balign 16
.LWordwise_Body:
        movzwl  (%edx,%eax), %ebx
        cmp     %bx, (%eax)
        jne     .LDoSbb
        add     $2, %eax
.LWordwise_Prepare:
        sub     $1, %ecx
        jnb     .LWordwise_Body
        pop     %ebx
        xor     %eax, %eax
end;
{$endif ndef CPUX86_HAS_SSE2}

function {$ifdef CPUX86_HAS_SSE2} CompareWord {$else} CompareWord_SSE2 {$endif} (Const buf1,buf2;len:SizeInt):SizeInt; assembler; nostackframe;
asm
        push     %ebx
        sub      %eax, %edx { edx = buf2 - buf1 }
        lea      -2(%ecx), %ebx { Go wordwise if ecx <= 1 or uint32(ecx) > 1073741823 (High(int32) div 2) ==> uint32(ecx - 2) > 1073741821. }
        cmp      $1073741821, %ebx
        ja       .LWordwise_Prepare
        cmp      $8, %ecx
        jge      .LVecOrMore

        lea      (%edx,%eax), %ebx
        or       %eax, %ebx
        and      $4095, %ebx
        cmp      $4080, %ebx
        ja       .LWordwise_Prepare
        movdqu   (%edx,%eax), %xmm0
        movdqu   (%eax), %xmm1
        pcmpeqw  %xmm1, %xmm0
        pmovmskb %xmm0, %ebx
        inc      %bx
        jz       .LNothing
        shl      $1, %ecx { convert to bytes }
        bsf      %ebx, %ebx
        cmp      %ecx, %ebx
        jb       .LSubtractWords
.LNothing:
        pop      %ebx
        xor      %eax, %eax
        ret

.balign 16
.LWordwise_Body:
        movzwl  (%edx,%eax), %ebx
        cmp     %bx, (%eax)
        jne     .LDoSbb
        add     $2, %eax
.LWordwise_Prepare:
        sub     $1, %ecx
        jae     .LWordwise_Body
        xor     %eax, %eax
        pop     %ebx
        ret

.LDoSbb:
        sbb     %eax, %eax
        or      $1, %eax
        pop     %ebx
        ret

.LVecOrMore:
        movdqu   (%edx,%eax), %xmm0 { Compare first vectors. }
        movdqu   (%eax), %xmm1
        pcmpeqw  %xmm1, %xmm0
        pmovmskb %xmm0, %ebx
        inc      %bx
        jnz      .LVec0Differs

        shl      $1, %ecx { convert to bytes }
        sub      $32, %ecx { first 16 bytes already analyzed + last 16 bytes analyzed separately }
        jle      .LLastVec

        push     %eax { save original buf1 to recover word position if byte mismatch found (aligned loop works in bytes to support misaligned buf1). }
        add      %eax, %ecx
        and      $-16, %eax { align buf1; +16 is performed by the loop. }
        sub      %eax, %ecx

.balign 16
.LAligned8xLoop_Body:
        add      $16, %eax
        movdqu   (%edx,%eax), %xmm0
        pcmpeqb  (%eax), %xmm0
        pmovmskb %xmm0, %ebx
        inc      %bx
        jnz      .LAligned8xLoop_VecDiffers
        sub      $16, %ecx
        ja       .LAligned8xLoop_Body
        pop      %ebx { drop original buf1 }
.LLastVec:
        lea      16(%eax,%ecx), %eax { point to the last 16 bytes }
        movdqu   (%edx,%eax), %xmm0
        movdqu   (%eax), %xmm1
        pcmpeqw  %xmm1, %xmm0
        pmovmskb %xmm0, %ebx
        inc      %bx
        jnz      .LVec0Differs
        pop      %ebx
        xor      %eax, %eax
        ret

.LVec0Differs:
        bsf      %ebx, %ebx
.LSubtractWords:
        add      %eax, %edx
        movzwl   (%eax,%ebx), %eax
        movzwl   (%edx,%ebx), %edx
        sub      %edx, %eax
        pop      %ebx
        ret

.LAligned8xLoop_VecDiffers:
        bsf      %ebx, %ebx
        add      %ebx, %eax
        pop      %ecx
        sub      %ecx, %eax
        and      $-2, %eax
        add      %ecx, %eax
        movzwl   (%edx,%eax), %edx
        movzwl   (%eax), %eax
        sub      %edx, %eax
        pop      %ebx
end;

{$ifndef CPUX86_HAS_SSE2}
function CompareWord_Dispatch(const buf1, buf2; len: SizeInt): SizeInt; forward;

var
  CompareWord_Impl: function(const buf1, buf2; len: SizeInt): SizeInt = @CompareWord_Dispatch;

function CompareWord_Dispatch(const buf1, buf2; len: SizeInt): SizeInt;
begin
  if not fpc_cpucodeinit_performed then
    exit(CompareWord_Plain(buf1, buf2, len));
  if has_sse2_support then
    CompareWord_Impl:=@CompareWord_SSE2
  else
    CompareWord_Impl:=@CompareWord_Plain;
  result:=CompareWord_Impl(buf1, buf2, len);
end;

function CompareWord(const buf1, buf2; len: SizeInt): SizeInt;
begin
  result:=CompareWord_Impl(buf1, buf2, len);
end;
{$endif ndef CPUX86_HAS_SSE2 (need CompareWord dispatcher)}
{$endif FPC_SYSTEM_HAS_COMPAREWORD}


{$ifndef FPC_SYSTEM_HAS_COMPAREDWORD}
{$define FPC_SYSTEM_HAS_COMPAREDWORD}
{$ifndef CPUX86_HAS_SSE2}
function CompareDWord_Plain(Const buf1,buf2;len:SizeInt):SizeInt; assembler; nostackframe;
asm
        sub     $1, %ecx
        jb      .LNothing
        push    %ebx
        sub     %eax, %edx
.balign 16
.LDwordwise_Body:
        mov     (%edx,%eax), %ebx
        cmp     %ebx, (%eax)
        jne     .LDoSbb
        add     $4, %eax
        sub     $1, %ecx
        jnb     .LDwordwise_Body
        pop     %ebx
.LNothing:
        xor %eax, %eax
        ret

.LDoSbb:
        pop     %ebx
        sbb     %eax, %eax
        or      $1, %eax
end;
{$endif}

function {$ifdef CPUX86_HAS_SSE2} CompareDWord {$else} CompareDWord_SSE2 {$endif} (Const buf1,buf2;len:SizeInt):SizeInt; assembler; nostackframe;
asm
        push     %ebx
        sub      %eax, %edx { edx = buf2 - buf1 }
        lea      -5(%ecx), %ebx { Go dwordwise if ecx <= 4 or ecx > 536870911 (High(int32) div 4) ==> uint32(ecx - 5) > 536870906. }
        cmp      $536870906, %ebx
        ja       .LDwordwise_Prepare
        shl      $2, %ecx { convert to bytes }

        movdqu   (%edx,%eax), %xmm1 { Compare first vectors. }
        movdqu   (%eax), %xmm0
        pcmpeqd  %xmm1, %xmm0
        pmovmskb %xmm0, %ebx
        inc      %bx
        jnz      .LVec0Differs

        sub      $32, %ecx { first 16 bytes already analyzed + last 16 bytes analyzed separately }
        jle      .LLastVec

        push     %eax { save original buf1 to recover uint32 position if byte mismatch found (aligned loop works in bytes to support misaligned buf1). }
        add      %eax, %ecx
        and      $-16, %eax { align buf1; +16 is performed by the loop. }
        sub      %eax, %ecx

.balign 16
.LAligned4xLoop_Body:
        add      $16, %eax
        movdqu   (%eax,%edx), %xmm0
        pcmpeqb  (%eax), %xmm0
        pmovmskb %xmm0, %ebx
        inc      %bx
        jnz      .LAligned4xLoop_VecDiffers
        sub      $16, %ecx
        ja       .LAligned4xLoop_Body
        pop      %ebx { drop original buf1 }
.LLastVec:
        lea      16(%eax,%ecx), %eax { point to the last 16 bytes }
        movdqu   (%edx,%eax), %xmm1
        movdqu   (%eax), %xmm0
        pcmpeqd  %xmm1, %xmm0
        pmovmskb %xmm0, %ebx
        inc      %bx
        jnz      .LVec0Differs
        pop      %ebx
        xor      %eax, %eax
        ret

.LVec0Differs:
        bsf      %ebx, %ebx
        add      %eax, %edx { recover edx = buf2 }
        mov      (%edx,%ebx), %edx
        cmp      %edx, (%eax,%ebx)
        sbb      %eax, %eax
        or       $1, %eax
        pop      %ebx
        ret

.LAligned4xLoop_VecDiffers:
        bsf      %ebx, %ebx
        add      %ebx, %eax
        pop      %ecx
        sub      %ecx, %eax
        and      $-4, %eax
        add      %ecx, %eax
        mov      (%edx,%eax), %edx
        cmp      %edx, (%eax)
.LDoSbb:
        sbb      %eax, %eax
        or       $1, %eax
        pop      %ebx
        ret

.balign 16
.LDwordwise_Body:
        mov     (%edx,%eax), %ebx
        cmp     %ebx, (%eax)
        jne     .LDoSbb
        add     $4, %eax
.LDwordwise_Prepare:
        sub     $1, %ecx
        jnb     .LDwordwise_Body
        pop     %ebx
        xor     %eax, %eax
end;

{$ifndef CPUX86_HAS_SSE2}
function CompareDWord_Dispatch(const buf1, buf2; len: SizeInt): SizeInt; forward;

var
  CompareDWord_Impl: function(const buf1, buf2; len: SizeInt): SizeInt = @CompareDWord_Dispatch;

function CompareDWord_Dispatch(const buf1, buf2; len: SizeInt): SizeInt;
begin
  if not fpc_cpucodeinit_performed then
    exit(CompareDWord_Plain(buf1, buf2, len));
  if has_sse2_support then
    CompareDWord_Impl:=@CompareDWord_SSE2
  else
    CompareDWord_Impl:=@CompareDWord_Plain;
  result:=CompareDWord_Impl(buf1, buf2, len);
end;

function CompareDWord(const buf1, buf2; len: SizeInt): SizeInt;
begin
  result:=CompareDWord_Impl(buf1, buf2, len);
end;
{$endif ndef CPUX86_HAS_SSE2 (need CompareDWord dispatcher)}
{$endif FPC_SYSTEM_HAS_COMPAREDWORD}


{$ifndef FPC_SYSTEM_HAS_INDEXCHAR0}
{$define FPC_SYSTEM_HAS_INDEXCHAR0}
function IndexChar0(Const buf;len:SizeInt;b:AnsiChar):SizeInt; assembler;
var
  saveesi,saveebx : longint;
asm
        movl    %esi,saveesi
        movl    %ebx,saveebx
// Can't use scasb, or will have to do it twice, think this
//   is faster for small "len"
        movl    %eax,%esi        // Load address
        movzbl  %cl,%ebx          // Load searchpattern
        testl   %edx,%edx
        je      .LFound
        xorl    %ecx,%ecx       // zero index in Buf
        xorl    %eax,%eax       // To make DWord compares possible
        .balign 4
.LLoop:
        movb    (%esi),%al      // Load byte
        cmpb    %al,%bl
        je      .LFound         //  byte the same?
        incl    %ecx
        incl    %esi
        cmpl    %edx,%ecx       // Maximal distance reached?
        je      .LNotFound
        testl   %eax,%eax       // Nullchar = end of search?
        jne     .LLoop
.LNotFound:
        movl    $-1,%ecx        // Not found return -1
.LFound:
        movl    %ecx,%eax
        movl    saveesi,%esi
        movl    saveebx,%ebx
end;
{$endif FPC_SYSTEM_HAS_INDEXCHAR0}


{****************************************************************************
                                 String
****************************************************************************}

{$ifndef FPC_SYSTEM_HAS_FPC_SHORTSTR_ASSIGN}
{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_ASSIGN}

procedure fpc_shortstr_to_shortstr(out res:shortstring; const sstr: shortstring);assembler;[public,alias:'FPC_SHORTSTR_TO_SHORTSTR']; compilerproc;
{$ifndef FPC_PROFILE}
  nostackframe;
{$endif}
  { eax = res, edx = high(res), ecx = sstr }
  asm
  {$ifdef FPC_PROFILE}
        push  %eax
        push  %edx
        push  %ecx
        call  mcount
        pop   %ecx
        pop   %edx
        pop   %eax
  {$endif FPC_PROFILE}
        cmp     (%ecx), %dl { length(sstr) fits into res? }
        jbe     .LEdxIsLen { use high(res) if length(sstr) does not fit }
        movzbl  (%ecx), %edx { use length(sstr) }
.LEdxIsLen:
        mov     %dl, (%eax) { store length to res[0] }
        xchg    %ecx, %edx { ecx = length = Move count, edx = sstr }
        xchg    %eax, %edx { eax = sstr = Move src, edx = res = Move dest }
        inc     %eax
        inc     %edx
{$ifdef FPC_PROFILE}
{$ifdef FPC_SYSTEM_STACKALIGNMENT16}
        lea     -8(%esp), %esp
{$endif FPC_SYSTEM_STACKALIGNMENT16}
        call    Move
{$ifdef FPC_SYSTEM_STACKALIGNMENT16}
        lea     8(%esp), %esp
{$endif FPC_SYSTEM_STACKALIGNMENT16}
{$else FPC_PROFILE}
        jmp     Move
{$endif FPC_PROFILE}
  end;


procedure fpc_shortstr_assign(len:longint;sstr,dstr:pointer);[public,alias:'FPC_SHORTSTR_ASSIGN'];
begin
  asm
  {$ifdef FPC_PROFILE}
        push  %eax
        push  %edx
        push  %ecx
        call  mcount
        pop   %ecx
        pop   %edx
        pop   %eax
  {$endif FPC_PROFILE}
        pushl   %eax
        pushl   %ecx
{$ifdef FPC_ENABLED_CLD}
        cld
{$endif FPC_ENABLED_CLD}
        movl    dstr,%edi
        movl    sstr,%esi
        xorl    %eax,%eax
        movl    len,%ecx
        lodsb
        cmpl    %ecx,%eax
        jbe     .LStrCopy1
        movl    %ecx,%eax
.LStrCopy1:
        stosb
        cmpl    $7,%eax
        jl      .LStrCopy2
        movl    %edi,%ecx       { Align on 32bits }
        negl    %ecx
        andl    $3,%ecx
        subl    %ecx,%eax
        rep
        movsb
        movl    %eax,%ecx
        andl    $3,%eax
        shrl    $2,%ecx
        rep
        movsl
.LStrCopy2:
        movl    %eax,%ecx
        rep
        movsb
        popl    %ecx
        popl    %eax
  end ['ESI','EDI'];
end;
{$endif FPC_SYSTEM_HAS_FPC_SHORTSTR_ASSIGN}



{$ifndef FPC_SYSTEM_HAS_FPC_SHORTSTR_COMPARE}
{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_COMPARE}

function fpc_shortstr_compare(const left,right:shortstring): longint;assembler; [public,alias:'FPC_SHORTSTR_COMPARE']; compilerproc;
{ eax = left, edx = right }
asm
{$ifdef FPC_PROFILE}
        push  %eax
        push  %edx
        push  %ecx
        call  mcount
        pop   %ecx
        pop   %edx
        pop   %eax
{$endif FPC_PROFILE}
        push    %ebx
        movzbl  (%eax), %ecx { ecx = len(left) }
        movzbl  (%edx), %ebx { ebx = len(right) }
        cmp     %ebx, %ecx
{$ifdef CPUX86_HAS_CMOV}
        cmovg   %ebx, %ecx
{$else}
        jle     .LEcxIsLen
        mov     %ebx, %ecx
.LEcxIsLen:
{$endif}
        push    %eax { save left }
        inc     %eax
        inc     %edx
        { stack is already aligned to 16 bytes if required: return address + push ebp + push ebx + push eax. }
{$if defined(FPC_PIC) or not declared(CompareByte_Impl)}
        call    CompareByte
{$else}
        call    CompareByte_Impl { manually inline CompareByte }
{$endif}
        pop     %edx { restore left }
        test    %eax, %eax
        jnz     .LReturn
        movzbl  (%edx), %eax
        sub     %ebx, %eax
.LReturn:
        pop     %ebx
end;
{$endif FPC_SYSTEM_HAS_FPC_SHORTSTR_COMPARE}


{$ifndef FPC_SYSTEM_HAS_FPC_SHORTSTR_COMPARE_EQUAL}
{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_COMPARE_EQUAL}
function fpc_shortstr_compare_equal(const left,right:shortstring) : longint;assembler;nostackframe; [public,alias:'FPC_SHORTSTR_COMPARE_EQUAL']; compilerproc; nostackframe;
{ eax = left, edx = right }
asm
        movzbl  (%eax), %ecx
        cmp     (%edx), %cl
        jne     .LNotEqual
        inc     %eax
        inc     %edx
{$if defined(FPC_PIC) or not declared(CompareByte_Impl)}
        jmp     CompareByte
{$else}
        jmp     CompareByte_Impl { manually inline CompareByte }
{$endif}
.LNotEqual:
        or      $-1, %eax
end;
{$endif FPC_SYSTEM_HAS_FPC_SHORTSTR_COMPARE_EQUAL}

{$ifndef FPC_SYSTEM_HAS_FPC_PCHAR_TO_SHORTSTR}
{$define FPC_SYSTEM_HAS_FPC_PCHAR_TO_SHORTSTR}
procedure fpc_pchar_to_shortstr(out res : shortstring;p:PAnsiChar);assembler;[public,alias:'FPC_PCHAR_TO_SHORTSTR']; compilerproc;
{$ifndef FPC_PROFILE}
  nostackframe;
{$endif}
// eax = res, edx = high(res), ecx = p
asm
{$ifdef FPC_PROFILE}
        push  %eax
        push  %edx
        push  %ecx
        call  mcount
        pop   %ecx
        pop   %edx
        pop   %eax
{$endif FPC_PROFILE}
        test    %ecx, %ecx
        jz      .LEmpty
        push    %eax { save res }
        push    %ecx { save p }
        push    %edx { save high(res) }
        mov     %ecx, %eax { eax = IndexByte.buf }
        { edx is already high(res) = IndexByte.count.
          Careful: using high(res) instead of -1 limits the scan by high(res) which is a good thing,
          but assumes that IndexByte is “safe” and won’t read potentially invalid memory past the searched byte even if formally (and wrongly) allowed by ‘count’.
          Generic and x86 versions are “safe”. }
        xor     %ecx, %ecx { ecx = 0 = IndexByte.value }
        { Stack is already aligned on 16 bytes if the function is nostackframe: return address + push eax + push ecx + push edx.
          With a stack frame, there is an additional push ebp and need 12 more bytes to align. }
{$if defined(FPC_SYSTEM_STACKALIGNMENT16) and defined(FPC_PROFILE)}
        leal    -12(%esp), %esp
{$endif defined(FPC_SYSTEM_STACKALIGNMENT16) and defined(FPC_PROFILE)}
{$if defined(FPC_PIC) or not declared(IndexByte_Impl)}
        call    IndexByte
{$else}
        call    IndexByte_Impl { manually inline IndexByte }
{$endif}
{$if defined(FPC_SYSTEM_STACKALIGNMENT16) and defined(FPC_PROFILE)}
        leal    12(%esp), %esp
{$endif defined(FPC_SYSTEM_STACKALIGNMENT16) and defined(FPC_PROFILE)}
        pop     %ecx { ecx = high(res) = Move.len }
        test    %eax, %eax { If IndexByte result (eax) is non-negative (terminator is among first high(res) characters), use it, otherwise keep high(res). }
{$ifdef CPUX86_HAS_CMOV}
        cmovns  %eax, %ecx
{$else}
        js      .LEcxIsLen
        mov     %eax, %ecx
.LEcxIsLen:
{$endif}
        pop     %eax { pop p to eax = Move.src }
        pop     %edx { pop res to edx }
        mov     %cl, (%edx) { res[0] := len }
        inc     %edx { res[1] = Move.dst }
{$ifdef FPC_PROFILE}
{$ifdef FPC_SYSTEM_STACKALIGNMENT16}
        leal    -12(%esp), %esp
{$endif FPC_SYSTEM_STACKALIGNMENT16}
        call    Move
{$ifdef FPC_SYSTEM_STACKALIGNMENT16}
        leal    12(%esp), %esp
{$endif FPC_SYSTEM_STACKALIGNMENT16}
        jmp     .LReturn
{$else FPC_PROFILE}
        jmp     Move { can perform a tail call }
{$endif FPC_PROFILE}

.LEmpty:
        movb    $0, (%eax)
{$ifdef FPC_PROFILE}
.LReturn:
{$endif}
end;
{$endif FPC_SYSTEM_HAS_FPC_PCHAR_TO_SHORTSTR}

{$IFNDEF INTERNAL_BACKTRACE}
{$define FPC_SYSTEM_HAS_GET_FRAME}
function get_frame:pointer;assembler;nostackframe;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
        movl    %ebp,%eax
end;
{$ENDIF not INTERNAL_BACKTRACE}


{$define FPC_SYSTEM_HAS_GET_PC_ADDR}
Function Get_pc_addr : Pointer;assembler;nostackframe;
asm
        movl    (%esp),%eax
end;


{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:pointer;addr:pointer=nil):pointer;
{$if defined(win32)}
{ Windows has StackTop always properly set }
begin
  if assigned(framebp) and (framebp<=StackTop) and (framebp>=Sptr) then
    Result:=PPointer(framebp+4)^
  else
    Result:=nil;
end;
{$else defined(win32)}
nostackframe;assembler;
asm
        orl     %eax,%eax
        jz      .Lg_a_null
        movl    4(%eax),%eax
.Lg_a_null:
end;
{$endif defined(win32)}


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer;addr:pointer=nil):pointer;
{$if defined(win32)}
{ Windows has StackTop always properly set }
begin
  if assigned(framebp) and (framebp<=StackTop) and (framebp>=Sptr) then
    Result:=PPointer(framebp)^
  else
    Result:=nil;
end;
{$else defined(win32)}
nostackframe;assembler;
asm
        orl     %eax,%eax
        jz      .Lgnf_null
        movl    (%eax),%eax
.Lgnf_null:
end;
{$endif defined(win32)}


{$define FPC_SYSTEM_HAS_SPTR}
Function Sptr : Pointer;assembler;nostackframe;
asm
        movl    %esp,%eax
end;

{****************************************************************************
                                 Str()
****************************************************************************}

{$if defined(disabled) and defined(regcall) }
{$define FPC_SYSTEM_HAS_INT_STR_LONGWORD}
{$define FPC_SYSTEM_HAS_INT_STR_LONGINT}

label str_int_shortcut;


procedure int_str(l:longword;out s:shortstring);assembler;nostackframe;

asm
  pushl %esi
  pushl %edi
  pushl %ebx
  mov %edx,%edi
  xor %edx,%edx
  jmp str_int_shortcut
end;

procedure int_str(l:longint;out s:shortstring);assembler;nostackframe;

{Optimized for speed, but balanced with size.}

const digits:array[0..9] of cardinal=(0,10,100,1000,10000,
                                      100000,1000000,10000000,
                                      100000000,1000000000);

asm
{$ifdef FPC_PROFILE}
  push  %eax
  push  %edx
  push  %ecx
  call  mcount
  pop   %ecx
  pop   %edx
  pop   %eax
{$endif FPC_PROFILE}
  push %esi
  push %edi
  push %ebx
  movl %edx,%edi

  { Calculate absolute value and put sign in edx}
  cltd
  xorl %edx,%eax
  subl %edx,%eax
  negl %edx
str_int_shortcut:
  movl %ecx,%esi
  {Calculate amount of digits in ecx.}
  xorl %ecx,%ecx
  bsrl %eax,%ecx
  incl %ecx
  imul $1233,%ecx
  shr $12,%ecx
{$ifdef FPC_PIC}
  call fpc_geteipasebx
  {$ifdef darwin}
  movl digits-.Lpic(%ebx),%ebx
  {$else}
  addl $_GLOBAL_OFFSET_TABLE_,%ebx
  movl digits@GOT(%ebx),%ebx
  {$endif}
  cmpl (%ebx,%ecx,4),%eax
{$else}
  cmpl digits(,%ecx,4),%eax
{$endif}
  cmc
  adcl $0,%ecx               {Nr. digits ready in ecx.}

  {Write length & sign.}
  lea (%edx,%ecx),%ebx
  movb $45,%bh               {movb $'-,%bh   Not supported by our ATT reader.}
  movw %bx,(%edi)
  addl %edx,%edi
  subl %edx,%esi

  {Skip digits beyond string length.}
  movl %eax,%edx
  subl %ecx,%esi
  jae .Lloop_write
	.balign 4
.Lloop_skip:
  movl $0xcccccccd,%eax      {Divide by 10 using mul+shr}
  mull %edx
  shrl $3,%edx
  decl %ecx
  jz .Ldone                  {If (l<0) and (high(s)=1) this jump is taken.}
  incl %esi
  jnz .Lloop_skip

  {Write out digits.}
	.balign 4
.Lloop_write:
  movl $0xcccccccd,%eax      {Divide by 10 using mul+shr}
  {Pre-add '0'}
  leal 48(%edx),%ebx         {leal $'0(,%edx),%ebx Not supported by our ATT reader.}
  mull %edx
  shrl $3,%edx
  leal (%edx,%edx,8),%eax    {x mod 10 = x-10*(x div 10)}
  subl %edx,%ebx
  subl %eax,%ebx
  movb %bl,(%edi,%ecx)
  decl %ecx
  jnz .Lloop_write
.Ldone:
  popl %ebx
  popl %edi
  popl %esi
end;
{$endif}

{****************************************************************************
                               Bounds Check
****************************************************************************}


{ do a thread-safe inc/dec }
{$define FPC_SYSTEM_HAS_DECLOCKED_LONGINT}
function cpudeclocked(var l : longint) : boolean;assembler;nostackframe;

  asm
     lock
     decl       (%eax)
     setzb      %al
  end;

{$define FPC_SYSTEM_HAS_INCLOCKED_LONGINT}
procedure cpuinclocked(var l : longint);assembler;nostackframe;

  asm
     lock
     incl       (%eax)
  end;

// inline SMP check and normal lock.
// the locked one is so slow, inlining doesn't matter.
function declocked(var l : longint) : boolean; inline;

begin
  if not ismultithread then
    begin
     dec(l);
     declocked:=l=0;
    end
   else
    declocked:=cpudeclocked(l);
end;

procedure inclocked(var l : longint); inline;

begin
  if not ismultithread then
    inc(l)
   else
    cpuinclocked(l);
end;



function InterLockedDecrement (var Target: longint) : longint; assembler; nostackframe;
asm
        movl    $-1,%edx
        lock
        xaddl   %edx, (%eax)
        lea     -1(%edx),%eax
end;


function InterLockedIncrement (var Target: longint) : longint; assembler; nostackframe;
asm
        movl    $1,%edx
        lock
        xaddl   %edx, (%eax)
        lea     1(%edx),%eax
end;


function InterLockedExchange (var Target: longint;Source : longint) : longint; assembler; nostackframe;
asm
        xchgl   (%eax),%edx
        movl    %edx,%eax
end;


function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint; assembler; nostackframe;
asm
        lock
        xaddl   %edx, (%eax)
        movl    %edx,%eax
end;


function InterlockedCompareExchange(var Target: longint; NewValue: longint; Comperand: longint): longint; assembler; nostackframe;
asm
        xchgl   %eax,%ecx
        lock
        cmpxchgl   %edx, (%ecx)
end;


function InterlockedCompareExchange64(var Target: int64; NewValue: int64; Comperand: int64): int64; assembler;
asm
        pushl       %ebx
        pushl       %edi
        movl        %eax,%edi
        movl        Comperand+4,%edx
        movl        Comperand+0,%eax
        movl        NewValue+4,%ecx
        movl        NewValue+0,%ebx
        lock cmpxchg8b (%edi)
        pop         %edi
        pop         %ebx
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
    asm
      fninit
      fwait
      fldcw   localfpucw
    end;
    if has_sse_support then
      begin
        localmxcsr:=DefaultMXCSR;
        asm
          { setup sse exceptions }
        {$ifndef OLD_ASSEMBLER}
          ldmxcsr localmxcsr
        {$else OLD_ASSEMBLER}
          mov     localmxcsr,%eax
          subl    $4,%esp
          mov     %eax,(%esp)
          //ldmxcsr (%esp)
          .byte   0x0f,0xae,0x14,0x24
          addl    $4,%esp
        {$endif OLD_ASSEMBLER}
        end;
      end;
  end;


{ because of the brain dead sse detection on x86, this test is post poned }
procedure fpc_cpucodeinit;
  var
    _eax,_ecx_cpuid1,_edx_cpuid1,_ebx_cpuid7 : longint;
  begin
    if cpuid_support then
      begin
        asm
            movl $1,%eax
            xorl %ecx,%ecx
            cpuid
            movl %edx,_edx_cpuid1
            movl %ecx,_ecx_cpuid1
        end ['ebx'];
        has_mmx_support:=(_edx_cpuid1 and $800000)<>0;
        if ((_edx_cpuid1 and $2000000)<>0) then
          begin
            os_supports_sse:=true;
            sse_check:=true;
            asm
              { force an sse exception if no sse is supported, the exception handler sets
                os_supports_sse to false then }
              { don't change this instruction, the code above depends on its size }
            {$ifdef OLD_ASSEMBLER}
              .byte  0x0f,0x28,0xf7
            {$else}
              movaps %xmm7, %xmm6
            {$endif not EMX}
            end;
            sse_check:=false;
            has_sse_support:=os_supports_sse;
          end;
        if has_sse_support then
          begin
            has_sse2_support:=((_edx_cpuid1 and $4000000)<>0);
            has_sse3_support:=((_ecx_cpuid1 and $200)<>0);
            has_sse41_support:=boolean(_ecx_cpuid1 shr 19 and 1);

            { now avx }
            asm
              xorl %eax,%eax
              cpuid
              movl %eax,_eax
            end;
            if _eax>=7 then
              begin
                asm
                  movl $7,%eax
                  xorl %ecx,%ecx
                  cpuid
                  movl %ebx,_ebx_cpuid7
                end;
                fast_large_repmovstosb:=_ebx_cpuid7 and (1 shl 9)<>0;
                if (_ecx_cpuid1 and $08000000<>0 {XGETBV support?}) then
                  begin
                    asm
                      xorl %ecx,%ecx
                      .byte   0x0f,0x01,0xd0 { xgetbv }
                      movl %eax,_eax
                    end;
                    if (_eax and 6)=6 then
                      begin
                        has_avx_support:=(_ecx_cpuid1 and $10000000)<>0;
                        has_avx2_support:=(_ebx_cpuid7 and $20)<>0;
                      end;
                  end;
              end;
          end;
      end;

    { don't let libraries influence the FPU cw set by the host program }
    if IsLibrary then
      begin
        Default8087CW:=Get8087CW;
        if has_sse_support then
          DefaultMXCSR:=GetMXCSR;
      end;

    SysResetFPU;
    fpc_cpucodeinit_performed:=true;
  end;


{$if not defined(darwin) and defined(regcall) }
{ darwin requires that the stack is aligned to 16 bytes when calling another function }

{$ifdef FPC_HAS_FEATURE_ANSISTRINGS}

{$define FPC_SYSTEM_HAS_ANSISTR_DECR_REF}
Procedure fpc_AnsiStr_Decr_Ref (Var S : Pointer); [Public,Alias:'FPC_ANSISTR_DECR_REF']; compilerproc; nostackframe; assembler;
asm
        movl    (%eax),%edx
        testl   %edx,%edx
        jz      .Lquit
        movl    $0,(%eax)          // s:=nil
        cmpl    $0,-8(%edx)        // exit if refcount<0
        jl      .Lquit
  {$ifdef FPC_PIC}
        call	fpc_geteipasecx
        addl	$_GLOBAL_OFFSET_TABLE_,%ecx
        movl	ismultithread@GOT(%ecx),%ecx
        cmpl	$0,(%ecx)
  {$else FPC_PIC}
        cmpl    $0,ismultithread
  {$endif FPC_PIC}
        je      .Lskiplock
        .byte   0xF0               // LOCK prefix, jumped over if IsMultiThread = false. FPC assembler does not accept disjoint LOCK mnemonic.
.Lskiplock:
        decl    -8(%edx)
        jz      .Lfree
.Lquit:
        ret
.Lfree:
        leal    -12(%edx),%eax     // points to start of allocation
        { freemem is not an assembler leaf function like fpc_geteipasecx, so it
          needs to be called with proper stack alignment }
{$ifdef FPC_SYSTEM_STACKALIGNMENT16}
        leal    -12(%esp),%esp
        call    FPC_FREEMEM
        leal    12(%esp),%esp
{$else  FPC_SYSTEM_STACKALIGNMENT16}
        jmp     FPC_FREEMEM        // can perform a tail call
{$endif FPC_SYSTEM_STACKALIGNMENT16}
end;

function fpc_truely_ansistr_unique(Var S : Pointer): Pointer; forward;

{$define FPC_SYSTEM_HAS_ANSISTR_UNIQUE}
Function fpc_ansistr_Unique(Var S : Pointer): Pointer; [Public,Alias : 'FPC_ANSISTR_UNIQUE']; compilerproc; nostackframe;assembler;
asm
        movl    (%eax),%edx
        testl   %edx,%edx
        jz      .Lunchanged
        cmpl    $1,-8(%edx)
        jne     fpc_truely_ansistr_unique
.Lunchanged:
        movl    %edx,%eax
end;

{$endif FPC_HAS_FEATURE_ANSISTRINGS}

{$endif ndef darwin and defined(regcall) }

{$ifndef FPC_SYSTEM_HAS_MEM_BARRIER}
{$define FPC_SYSTEM_HAS_MEM_BARRIER}

procedure ReadBarrier;assembler;nostackframe;
asm
{$ifdef CPUX86_HAS_SSE2}
  lfence
{$else CPUX86_HAS_SSE2}
  lock
  addl $0,0(%esp)
{$endif CPUX86_HAS_SSE2}
end;

procedure ReadDependencyBarrier;
begin
  { reads imply barrier on earlier reads depended on }
end;

procedure ReadWriteBarrier;assembler;nostackframe;
asm
{$ifdef CPUX86_HAS_SSE2}
  mfence
{$else CPUX86_HAS_SSE2}
  lock
  addl $0,0(%esp)
{$endif CPUX86_HAS_SSE2}
end;

procedure WriteBarrier;assembler;nostackframe;
asm
{$ifdef CPUX86_HAS_SSEUNIT}
  sfence
{$endif CPUX86_HAS_SSEUNIT}
end;

{$endif}

{$ifndef FPC_SYSTEM_HAS_BSF_QWORD}
{$define FPC_SYSTEM_HAS_BSF_QWORD}
function BsfQWord(Const AValue : QWord): cardinal; assembler; nostackframe;
asm
{$ifdef CPUX86_HINT_BSX_DEST_UNCHANGED_ON_ZF_1}
     mov     $255-32,%eax { On AMD, BSF/R are documented to not change the destination on zero input. }
     bsfl    8(%esp),%eax { On Intel, destination is formally undefined on zero input, but in practice the behavior is the same. }
     add     $32,%eax
     bsfl    4(%esp),%eax
{$else}
     bsfl    4(%esp),%eax
     jz     .L1
     ret     $8
.L1:
     bsfl    8(%esp),%eax
     jz      .L2
     add     $32,%eax
     ret     $8
.L2:
     movl    $255,%eax
{$endif}
end;
{$endif FPC_SYSTEM_HAS_BSF_QWORD}


{$ifndef FPC_SYSTEM_HAS_BSR_QWORD}
{$define FPC_SYSTEM_HAS_BSR_QWORD}
function BsrQWord(Const AValue : QWord): cardinal; assembler; nostackframe;
asm
{$ifdef CPUX86_HINT_BSX_DEST_UNCHANGED_ON_ZF_1}
     mov     $255,%eax
     bsrl    4(%esp),%eax
     sub     $32,%eax
     bsrl    8(%esp),%eax
     add     $32,%eax
{$else}
     mov     8(%esp),%eax
     test    %eax,%eax
     jnz    .L1 { Speculate Hi(q) = 0. }
     bsrl    4(%esp),%eax
     jz     .L2
     ret     $8
.L1:
     bsrl    %eax,%eax
     add     $32,%eax
     ret     $8
.L2:
     movl    $255,%eax
{$endif}
end;
{$endif FPC_SYSTEM_HAS_BSR_QWORD}

{$ifndef FPC_SYSTEM_HAS_SAR_QWORD}
{$define FPC_SYSTEM_HAS_SAR_QWORD}
function fpc_SarInt64(Const AValue : Int64;const Shift : Byte): Int64; [Public,Alias:'FPC_SARINT64']; compilerproc; assembler; nostackframe;
asm
        movl   8(%esp),%edx
        movzbl %al,%ecx
        cmpb   $32,%al
        jnb    .L1
        movl   4(%esp),%eax
        shrdl  %cl,%edx,%eax
        sarl   %cl,%edx
        ret    $8
.L1:
        movl   %edx,%eax
        sarl   $31,%edx
        sarl   %cl,%eax // uses 5 lower bits of cl.
end;
{$endif FPC_SYSTEM_HAS_SAR_QWORD}
