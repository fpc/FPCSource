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

{$if not(defined(VER3_0)) and defined(linux)}
  {$define FPC_SYSTEM_STACKALIGNMENT16}
{$endif not(defined(VER3_0)) and defined(linux)}

{****************************************************************************
                               Primitives
****************************************************************************}
var
  os_supports_sse : boolean;
  { this variable is set to true, if currently an sse check is executed and no sig ill should be generated }
  sse_check : boolean;
  fast_large_repmovstosb : boolean; { Enhanced REP MOVSB and STOSB (ERMSB) feature @ CPUID(7).ebx[9]. }
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


{$ifndef FPC_SYSTEM_HAS_FILLCHAR}
{$define FPC_SYSTEM_HAS_FILLCHAR}
Procedure FillChar(var x;count:SizeInt;value:byte);assembler; nostackframe;
asm
        cmpl    $22,%edx          { empirically determined value on a Core 2 Duo Conroe }
        jg      .LFillFull
        orl     %edx,%edx
        jle     .LFillZero

.LFillLoop:
        movb    %cl,(%eax)
        incl    %eax
        decl    %edx
        jne     .LFillLoop
.LFillZero:
        ret

.LFillFull:
{$ifdef FPC_ENABLED_CLD}
        cld
{$endif FPC_ENABLED_CLD}
        push    %edi
        movl    %eax,%edi
        movzbl  %cl,%eax
        movl    %edx,%ecx
        imul    $0x01010101,%eax  { Expand al into a 4 subbytes of eax}
        shrl    $2,%ecx
        andl    $3,%edx
        rep
        stosl
        movl    %edx,%ecx
.LFill1:
        rep
        stosb
.LFillEnd:
        pop %edi
end;
{$endif FPC_SYSTEM_HAS_FILLCHAR}


{$ifndef FPC_SYSTEM_HAS_FILLWORD}
{$define FPC_SYSTEM_HAS_FILLWORD}
procedure fillword(var x;count : SizeInt;value : word);assembler;
var
  saveedi : longint;
asm
        movl    %edi,saveedi
        movl    %eax,%edi
        movzwl  %cx,%eax
        movl    %edx,%ecx
{ check for zero or negative count }
        cmpl    $0,%ecx
        jle     .LFillWordEnd
        movl    %eax,%edx
        shll    $16,%eax
        orl     %edx,%eax
        movl    %ecx,%edx
        shrl    $1,%ecx
{$ifdef FPC_ENABLED_CLD}
        cld
{$endif FPC_ENABLED_CLD}
        rep
        stosl
        movl    %edx,%ecx
        andl    $1,%ecx
        rep
        stosw
.LFillWordEnd:
        movl    saveedi,%edi
end;
{$endif FPC_SYSTEM_HAS_FILLWORD}


{$ifndef FPC_SYSTEM_HAS_FILLDWORD}
{$define FPC_SYSTEM_HAS_FILLDWORD}
procedure filldword(var x;count : SizeInt;value : dword);assembler;
var
  saveedi : longint;
asm
        movl    %edi,saveedi
        movl    %eax,%edi
        movl    %ecx,%eax
        movl    %edx,%ecx
{ check for zero or negative count }
        cmpl    $0,%ecx
        jle     .LFillDWordEnd
{$ifdef FPC_ENABLED_CLD}
        cld
{$endif FPC_ENABLED_CLD}
        rep
        stosl
.LFillDWordEnd:
        movl    saveedi,%edi
end;
{$endif FPC_SYSTEM_HAS_FILLDWORD}


{$ifndef FPC_SYSTEM_HAS_INDEXBYTE}
{$define FPC_SYSTEM_HAS_INDEXBYTE}
function IndexByte_Plain(Const buf;len:SizeInt;b:byte):SizeInt; assembler; nostackframe;
asm
        push  %esi
        push  %edi
        push  %eax                  { save initial value of 'buf' }

        cmp   $4,%edx               { less than 4 bytes, just test byte by byte. }
        jb    .Ltail

        mov    %cl,%ch              { prepare pattern }
        movzwl %cx,%esi
        shl    $16,%ecx
        or     %esi,%ecx

.Lalignloop:
        test  $3,%al                { align to 4 bytes if necessary }
        je    .Laligned
        cmp   %cl,(%eax)
        je    .Lexit
        inc   %eax
        dec   %edx
        jmp   .Lalignloop

.balign 16                      { Main loop, unrolled 4 times for speed }

.Lloop:
        mov   (%eax),%esi           { load dword }
        xor   %ecx,%esi             { XOR with pattern, bytes equal to target are now 0 }
        lea   -0x01010101(%esi),%edi
        xor   %esi,%edi             { (x-0x01010101) xor x }
        not   %esi
        and   $0x80808080,%esi
        and   %edi,%esi             { ((x-0x01010101) xor x) and (not x) and 0x80808080 }
        jnz   .Lfound               { one of the bytes matches }

        mov   4(%eax),%esi
        xor   %ecx,%esi
        lea   -0x01010101(%esi),%edi
        xor   %esi,%edi
        not   %esi
        and   $0x80808080,%esi
        and   %edi,%esi
        jnz   .Lfound4

        mov   8(%eax),%esi
        xor   %ecx,%esi
        lea   -0x01010101(%esi),%edi
        xor   %esi,%edi
        not   %esi
        and   $0x80808080,%esi
        and   %edi,%esi
        jnz   .Lfound8

        mov   12(%eax),%esi
        xor   %ecx,%esi
        lea   -0x01010101(%esi),%edi
        xor   %esi,%edi
        not   %esi
        and   $0x80808080,%esi
        and   %edi,%esi
        jnz   .Lfound12

        add   $16,%eax
.Laligned:
        sub   $16,%edx
        jae   .Lloop                { Still more than 16 bytes remaining }

{ Process remaining bytes (<16 left at this point) }
{ length is offset by -16 at this point }
.Lloop2:
        cmp   $4-16,%edx            { < 4 bytes left? }
        jb    .Ltail

        mov   (%eax),%esi
        xor   %ecx,%esi
        lea   -0x01010101(%esi),%edi
        xor   %esi,%edi
        not   %esi
        and   $0x80808080,%esi
        and   %edi,%esi
        jne   .Lfound

        add   $4,%eax
        sub   $4,%edx
        jmp   .Lloop2

.Ltail:                         { Less than 4 bytes remaining, check one by one }
        and   $3, %edx
        jz    .Lnotfound
.Lloop3:
        cmp   %cl,(%eax)
        je    .Lexit
        inc   %eax
        dec   %edx
        jnz   .Lloop3

.Lnotfound:
        or    $-1,%eax
        jmp   .Lexit1

{ add missing source pointer increments }
.Lfound12:
        add   $4,%eax
.Lfound8:
        add   $4,%eax
.Lfound4:
        add   $4,%eax

.Lfound:
        test  $0xff,%esi
        jnz   .Lexit
        inc   %eax

        test  $0xff00,%esi
        jnz   .Lexit
        inc   %eax

        test  $0xff0000,%esi
        jnz   .Lexit
        inc   %eax

.Lexit:
        sub   (%esp),%eax
.Lexit1:
        pop   %ecx               { removes initial 'buf' value }
        pop   %edi
        pop   %esi
end;

function IndexByte_SSE2(const buf;len:SizeInt;b:byte):SizeInt; assembler; nostackframe;
asm
        test      %edx, %edx
        jz        .Lnotfound                 { exit if len=0 }
        push      %ebx
        movd      %ecx, %xmm1
        lea       16(%eax), %ecx             { eax = original ptr, ecx = buf + 16 for aligning & shifts. }
        punpcklbw %xmm1, %xmm1
        and       $-0x10, %ecx               { first aligned address after buf }
        punpcklbw %xmm1, %xmm1
        pshufd    $0, %xmm1, %xmm1
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
        ret

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
end;

function IndexByte_Dispatch(const buf;len:SizeInt;b:byte):SizeInt; forward;

var
  IndexByte_Impl: function(const buf;len:SizeInt;b:byte):SizeInt = @IndexByte_Dispatch;
{$define has_i386_IndexByte_Impl} { used in assembler to manually inline IndexByte }

function IndexByte_Dispatch(const buf;len:SizeInt;b:byte):SizeInt;
begin
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
{$endif FPC_SYSTEM_HAS_INDEXBYTE}


{$ifndef FPC_SYSTEM_HAS_INDEXWORD}
{$define FPC_SYSTEM_HAS_INDEXWORD}
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

function IndexWord_SSE2(const buf;len:SizeInt;b:word):SizeInt; assembler; nostackframe;
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

function IndexWord_Dispatch(const buf;len:SizeInt;b:word):SizeInt; forward;

var
  IndexWord_Impl: function(const buf;len:SizeInt;b:word):SizeInt = @IndexWord_Dispatch;

function IndexWord_Dispatch(const buf;len:SizeInt;b:word):SizeInt;
begin
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
{$endif FPC_SYSTEM_HAS_INDEXWORD}


{$ifndef FPC_SYSTEM_HAS_INDEXDWORD}
{$define FPC_SYSTEM_HAS_INDEXDWORD}
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

function IndexDWord_SSE2(const buf;len:SizeInt;b:DWord):SizeInt; assembler; nostackframe;
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

function IndexDWord_Dispatch(const buf;len:SizeInt;b:DWord):SizeInt; forward;

var
  IndexDWord_Impl: function(const buf;len:SizeInt;b:DWord):SizeInt = @IndexDWord_Dispatch;

function IndexDWord_Dispatch(const buf;len:SizeInt;b:DWord):SizeInt;
begin
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
{$endif FPC_SYSTEM_HAS_INDEXDWORD}


{$ifndef FPC_SYSTEM_HAS_INDEXQWORD}
{$define FPC_SYSTEM_HAS_INDEXQWORD}
function IndexQWord(const buf;len:SizeInt;b:QWord):SizeInt; assembler; nostackframe;
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
{$endif FPC_SYSTEM_HAS_INDEXQWORD}


{$ifndef FPC_SYSTEM_HAS_COMPAREBYTE}
{$define FPC_SYSTEM_HAS_COMPAREBYTE}
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

function CompareByte_SSE2(const buf1, buf2; len: SizeInt): SizeInt; assembler; nostackframe;
asm
        { eax = buf1, edx = buf2, ecx = len }
        cmp      $1, %ecx
        jle      .L1OrLess

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

.LVecOrMore:
        { Compare first vectors. }
        movdqu   (%eax), %xmm0
        movdqu   (%edx), %xmm1
        pcmpeqb  %xmm1, %xmm0
        pmovmskb %xmm0, %ebx
        inc      %bx
        jnz      .LVec0Differs

        sub      $32, %ecx { now ecx is len - 32... mostly just to save bytes on offsets improving .LAligned32xLoop_Body alignment :) }
        jbe      .LLastVec

        { Compare second vectors. }
        movdqu   16(%eax), %xmm0
        movdqu   16(%edx), %xmm1
        pcmpeqb  %xmm1, %xmm0
        pmovmskb %xmm0, %ebx
        inc      %bx
        jnz      .LVec1Differs

        { More than four vectors: aligned loop. }
        cmp      $32, %ecx
        ja       .LAligned32xLoop_Prepare

        { Compare last two vectors. }
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

.LVecEm2Differs:
        sub      $16, %ecx
.LVecEm1Differs:
        bsf      %ebx, %ebx
        add      %ecx, %ebx
        movzbl   16(%eax,%ebx), %eax
        movzbl   16(%edx,%ebx), %edx
        sub      %edx, %eax
        pop      %ebx
        ret
        nop      { Turn .balign 16 before .LAligned32xLoop_Body into a no-op. }

.LAligned32xLoop_Prepare:
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

        { Compare last two vectors after the loop by doing one more loop iteration, modified. }
        lea      32(%eax,%ecx), %eax
        movdqu   (%edx,%eax), %xmm0
        movdqu   (%eax), %xmm2
        pcmpeqb  %xmm2, %xmm0
        movdqu   16(%edx,%eax), %xmm1
        movdqu   16(%eax), %xmm2
        pcmpeqb  %xmm2, %xmm1
        pand     %xmm0, %xmm1
        pmovmskb %xmm1, %ebx
        inc      %bx
        jnz      .LAligned32xLoop_TwoVectorsDiffer
        pop      %ebx
        xor      %eax, %eax
        ret

.LAligned32xLoop_TwoVectorsDiffer:
        add      %eax, %edx { restore edx = buf2 }
        pmovmskb %xmm0, %ecx { Is there a difference in the first vector? }
        inc      %cx
        jz       .LVec1Differs { No difference in the first vector, xmm0 is all ones, ebx = pmovmskb(pcmpeqb(buf1 + 16, buf2 + 16)) from the loop body. }
        bsf      %ecx, %ebx
        movzbl   (%eax,%ebx), %eax
        movzbl   (%edx,%ebx), %edx
        sub      %edx, %eax
        pop      %ebx
        ret

.LVec1Differs:
        add      $16, %eax
        add      $16, %edx
.LVec0Differs:
        bsf      %ebx, %ebx
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

.L1OrLess:
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

function CompareByte_Dispatch(const buf1, buf2; len: SizeInt): SizeInt; forward;

var
  CompareByte_Impl: function(const buf1, buf2; len: SizeInt): SizeInt = @CompareByte_Dispatch;

function CompareByte_Dispatch(const buf1, buf2; len: SizeInt): SizeInt;
begin
  if has_sse2_support then
    CompareByte_Impl:=@CompareByte_SSE2
  else
    CompareByte_Impl:=@CompareByte_Plain;
  result:=CompareByte_Impl(buf1, buf2, len);
end;

function CompareByte(const buf1, buf2; len: SizeInt): SizeInt;
begin
  result:=CompareByte_Impl(buf1, buf2, len);
end;
{$endif FPC_SYSTEM_HAS_COMPAREBYTE}


{$ifndef FPC_SYSTEM_HAS_COMPAREWORD}
{$define FPC_SYSTEM_HAS_COMPAREWORD}
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

function CompareWord_SSE2(Const buf1,buf2;len:SizeInt):SizeInt; assembler; nostackframe;
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

function CompareWord_Dispatch(const buf1, buf2; len: SizeInt): SizeInt; forward;

var
  CompareWord_Impl: function(const buf1, buf2; len: SizeInt): SizeInt = @CompareWord_Dispatch;

function CompareWord_Dispatch(const buf1, buf2; len: SizeInt): SizeInt;
begin
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
{$endif FPC_SYSTEM_HAS_COMPAREWORD}


{$ifndef FPC_SYSTEM_HAS_COMPAREDWORD}
{$define FPC_SYSTEM_HAS_COMPAREDWORD}
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

function CompareDWord_SSE2(Const buf1,buf2;len:SizeInt):SizeInt; assembler; nostackframe;
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

function CompareDWord_Dispatch(const buf1, buf2; len: SizeInt): SizeInt; forward;

var
  CompareDWord_Impl: function(const buf1, buf2; len: SizeInt): SizeInt = @CompareDWord_Dispatch;

function CompareDWord_Dispatch(const buf1, buf2; len: SizeInt): SizeInt;
begin
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
  var
   saveesi,saveedi : longint;
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
        movl    %edi,saveedi
        movl    %esi,saveesi
{$ifdef FPC_ENABLED_CLD}
        cld
{$endif FPC_ENABLED_CLD}
        movl    res,%edi
        movl    sstr,%esi
        movl    %edx,%ecx
        xorl    %eax,%eax
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
        movl    saveedi,%edi
        movl    saveesi,%esi
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
var
  saveesi,saveedi,saveebx : longint;
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
        movl    %edi,saveedi
        movl    %esi,saveesi
        movl    %ebx,saveebx
{$ifdef FPC_ENABLED_CLD}
        cld
{$endif FPC_ENABLED_CLD}
        movl    right,%esi
        movl    left,%edi
        movzbl  (%esi),%eax
        movzbl  (%edi),%ebx
        movl    %eax,%edx
        incl    %esi
        incl    %edi
        cmpl    %ebx,%eax
        jbe     .LStrCmp1
        movl    %ebx,%eax
.LStrCmp1:
        cmpl    $7,%eax
        jl      .LStrCmp2
        movl    %edi,%ecx       { Align on 32bits }
        negl    %ecx
        andl    $3,%ecx
        subl    %ecx,%eax
        orl     %ecx,%ecx
        repe
        cmpsb
        jne     .LStrCmp3
        movl    %eax,%ecx
        andl    $3,%eax
        shrl    $2,%ecx
        orl     %ecx,%ecx
        repe
        cmpsl
        je      .LStrCmp2
        movl    $4,%eax
        subl    %eax,%esi
        subl    %eax,%edi
.LStrCmp2:
        movl    %eax,%ecx
        orl     %eax,%eax
        repe
        cmpsb
        je      .LStrCmp4
.LStrCmp3:
        movzbl  -1(%esi),%edx      // Compare failing (or equal) position
        movzbl  -1(%edi),%ebx
.LStrCmp4:
        movl    %ebx,%eax          // Compare length or position
        subl    %edx,%eax
        movl    saveedi,%edi
        movl    saveesi,%esi
        movl    saveebx,%ebx
end;
{$endif FPC_SYSTEM_HAS_FPC_SHORTSTR_COMPARE}


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
{$if defined(FPC_PIC) or not defined(has_i386_IndexByte_Impl)}
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

{$undef has_i386_IndexByte_Impl} { no longer required }

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



function InterLockedDecrement (var Target: longint) : longint; assembler;
asm
        movl    $-1,%edx
        xchgl   %edx,%eax
        lock
        xaddl   %eax, (%edx)
        decl    %eax
end;


function InterLockedIncrement (var Target: longint) : longint; assembler;
asm
        movl    $1,%edx
        xchgl   %edx,%eax
        lock
        xaddl   %eax, (%edx)
        incl    %eax
end;


function InterLockedExchange (var Target: longint;Source : longint) : longint; assembler;
asm
        xchgl   (%eax),%edx
        movl    %edx,%eax
end;


function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint; assembler;
asm
        xchgl   %eax,%edx
        lock
        xaddl   %eax, (%edx)
end;


function InterlockedCompareExchange(var Target: longint; NewValue: longint; Comperand: longint): longint; assembler;
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
// Var S located in register
// Var $result located in register
        movl    %eax,%edx
// [437] pointer(result) := pointer(s);
        movl    (%eax),%eax
// [438] If Pointer(S)=Nil then
        testl   %eax,%eax
        je      .Lj4031
.Lj4036:
// [440] if PAnsiRec(Pointer(S)-Firstoff)^.Ref<>1 then
        movl    -8(%eax),%ecx
        cmpl    $1,%ecx
        je      .Lj4038
// [441] result:=fpc_truely_ansistr_unique(s);
        movl    %edx,%eax
{$ifdef FPC_SYSTEM_STACKALIGNMENT16}
        leal    -12(%esp),%esp
{$endif FPC_SYSTEM_STACKALIGNMENT16}
        call    fpc_truely_ansistr_unique
{$ifdef FPC_SYSTEM_STACKALIGNMENT16}
        leal    12(%esp),%esp
{$endif FPC_SYSTEM_STACKALIGNMENT16}
.Lj4038:
.Lj4031:
// [442] end;
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

procedure ReadDependencyBarrier;{$ifdef SYSTEMINLINE}inline;{$endif}
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
     bsfl    4(%esp),%eax
     jnz     .L2
.L1:
     bsfl    8(%esp),%eax
     jnz     .L3
     movl    $223,%eax
.L3:
     addl    $32,%eax
.L2:
end;
{$endif FPC_SYSTEM_HAS_BSF_QWORD}


{$ifndef FPC_SYSTEM_HAS_BSR_QWORD}
{$define FPC_SYSTEM_HAS_BSR_QWORD}
function BsrQWord(Const AValue : QWord): cardinal; assembler; nostackframe;
asm
     bsrl    8(%esp),%eax
     jz     .L1
     add     $32,%eax
     jmp     .L2
.L1:
     bsrl    4(%esp),%eax
     jnz     .L2
     movl    $255,%eax
.L2:
end;
{$endif FPC_SYSTEM_HAS_BSR_QWORD}

{$ifndef FPC_SYSTEM_HAS_SAR_QWORD}
{$define FPC_SYSTEM_HAS_SAR_QWORD}
function fpc_SarInt64(Const AValue : Int64;const Shift : Byte): Int64; [Public,Alias:'FPC_SARINT64']; compilerproc; assembler; nostackframe;
asm
        movb   %al,%cl
        movl   8(%esp),%edx
        movl   4(%esp),%eax
        andb   $63,%cl
        cmpb   $32,%cl
        jnb    .L1
        shrdl  %cl,%edx,%eax
        sarl   %cl,%edx
        jmp    .Lexit
.L1:
        movl   %edx,%eax
        sarl   $31,%edx
        andb   $31,%cl
        sarl   %cl,%eax
.Lexit:
end;
{$endif FPC_SYSTEM_HAS_SAR_QWORD}
