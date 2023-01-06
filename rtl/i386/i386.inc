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

{$ifndef FPC_PIC}
{$ifndef FPC_SYSTEM_HAS_MOVE}
{$ifndef OLD_ASSEMBLER}
{$define USE_FASTMOVE}
{$i fastmove.inc}
{$endif not OLD_ASSEMBLER}
{$endif FPC_SYSTEM_HAS_MOVE}
{$endif FPC_PIC}

{$define FPC_SYSTEM_HAS_FPC_CPUINIT}
procedure fpc_cpuinit;
  begin
    { because of the brain dead sse detection on x86, this test is post poned to fpc_cpucodeinit which
      must be implemented OS dependend (FK)
    has_sse_support:=sse_support;
    has_mmx_support:=mmx_support;
    setup_fastmove;
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
        je        .LNothing
        push      %ebx
        mov       %eax, %ebx
        and       $-16, %eax
        pxor      %xmm1, %xmm1
        movd      %ecx, %xmm1
        punpcklbw %xmm1, %xmm1
        punpcklwd %xmm1, %xmm1
        pshufd    $0, %xmm1, %xmm1
        lea       16(%eax), %ecx
        movdqa    %xmm1, %xmm0
        pcmpeqb   (%eax), %xmm0
        sub       %ebx, %ecx
        pmovmskb  %xmm0, %eax
        sal       %cl, %eax
        xor       %ax, %ax
        shr       %cl, %eax
        jz        .L16xAligned_Test
        sub       $16, %ecx
.LFound:
        bsf       %eax, %eax
        add       %ecx, %eax
        pop       %ebx
        cmp       %edx, %eax
        jnb       .LNothing
        ret

.balign 16
.L16xAligned_Body:
        movdqa    %xmm1, %xmm0
        pcmpeqb   (%ebx,%ecx), %xmm0
        pmovmskb   %xmm0, %eax
        test      %eax, %eax
        jne       .LFound
        add       $16, %ecx
.L16xAligned_Test:
        cmp       %edx, %ecx
        jb        .L16xAligned_Body
        pop       %ebx
.LNothing:
        mov       $-1, %eax
end;

function IndexByte_Dispatch(const buf;len:SizeInt;b:byte):SizeInt; forward;

var
  IndexByte_Impl: function(const buf;len:SizeInt;b:byte):SizeInt = @IndexByte_Dispatch;

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
        push    %eax
        cmp     $1073741823, %edx
        ja      .LUnbounded
        lea     (%eax,%edx,2), %edx
        cmp     %edx, %eax
        je      .LNotFound
.LWordwise_Body:
        cmp     %cx, (%eax)
        je      .LFound
        add     $2, %eax
        cmp     %edx, %eax
        jne     .LWordwise_Body
.LNotFound:
        pop     %eax
        mov     $-1, %eax
        ret

.LFound:
        pop     %edx
        sub     %edx, %eax
        shr     $1, %eax
        ret

.LUnbounded:
        mov     %eax, %edx
        jmp     .LWordwise_Body
end;

function IndexWord_SSE2(const buf;len:SizeInt;b:word):SizeInt; assembler; nostackframe;
asm
        test      %edx, %edx
        je        .LInstantNothing
        push      %edi
        movd      %ecx, %xmm0
        push      %esi
        mov       %eax, %esi
        push      %ebx
        and       $-0x10, %esi
        punpcklwd %xmm0, %xmm0
        movdqa    (%esi), %xmm2
        sub       %eax, %esi
        mov       %edx, %edi
        pshufd    $0, %xmm0, %xmm0
        lea       16(%esi), %edx
        mov       %eax, %ebx
        movdqa    %xmm0, %xmm1
        mov       %edx, %ecx

        test      $1, %al
        jnz       .LUnaligned

        pcmpeqw   %xmm0, %xmm2
        pmovmskb  %xmm2, %eax

        shl       %cl, %eax
        xor       %ax, %ax
        shr       $1, %edx
        shr       %cl, %eax
        jz        .LLoopTest
        lea       -8(%edx), %ecx
.LMatch:
        bsf       %eax, %eax
        shr       $1, %eax
        add       %ecx, %eax
        cmp       %edi, %eax
        jnb       .LNothing
        pop       %ebx
        pop       %esi
        pop       %edi
        ret

.balign 16
.LLoop:
        movdqa    (%ebx,%edx,2), %xmm0
        mov       %edx, %ecx
        add       $8, %edx
        pcmpeqw   %xmm1, %xmm0
        pmovmskb  %xmm0, %eax
        test      %eax, %eax
        jne       .LMatch
.LLoopTest:
        cmp       %edi, %edx
        jb        .LLoop
.LNothing:
        pop       %ebx
        pop       %esi
        pop       %edi
.LInstantNothing:
        mov       $-1, %eax
        ret

.LUnaligned:
        psllw     $8, %xmm1
        add       %edi, %edi
        psrlw     $8, %xmm0
        por       %xmm1, %xmm0
        pcmpeqb   %xmm0, %xmm2
        movdqa    %xmm0, %xmm1
        pmovmskb  %xmm2, %eax
        shl       %cl, %eax
        xor       %ax, %ax
        shr       %cl, %eax
        lea       (%eax,%eax), %ecx
        and       %ecx, %eax
        and       $0x5555, %eax
        je        .LUnalignedLoopTest
.LUnalignedMatch:
        bsf       %eax, %eax
        add       %esi, %eax
        cmp       %edi, %eax
        jnb       .LNothing
        pop       %ebx
        shr       $1, %eax
        pop       %esi
        pop       %edi
        ret

.balign 16
.LUnalignedLoop:
        movdqa    (%ebx,%edx), %xmm0
        shr       $16, %ecx
        mov       %edx, %esi
        add       $16, %edx
        pcmpeqb   %xmm1, %xmm0
        pmovmskb  %xmm0, %eax
        add       %eax, %eax
        or        %eax, %ecx
        mov       %ecx, %eax
        shr       $1, %eax
        and       %ecx, %eax
        and       $0x5555, %eax
        jne       .LUnalignedMatch
.LUnalignedLoopTest:
        cmp       %edi, %edx
        jb        .LUnalignedLoop
        pop       %ebx
        pop       %esi
        pop       %edi
        mov       $-1, %eax
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
        cmp     $536870911, %edx
        ja      .LUnbounded
        lea     (%eax,%edx,4), %edx
        cmp     %edx, %eax
        je      .LNotFound
.LDWordwise_Body:
        cmp     %ecx, (%eax)
        je      .LFound
        add     $4, %eax
        cmp     %edx, %eax
        jne     .LDWordwise_Body
.LNotFound:
        pop     %eax
        mov     $-1, %eax
        ret

.LFound:
        pop     %edx
        sub     %edx, %eax
        shr     $2, %eax
        ret

.LUnbounded:
        mov     %eax, %edx
        jmp     .LDWordwise_Body
end;

function IndexDWord_SSE2(const buf;len:SizeInt;b:DWord):SizeInt; assembler; nostackframe;
asm
        push     %esi
        lea      (%eax,%edx,4), %esi
        push     %ebx
        mov      %eax, %ebx
        cmp      $536870911, %edx
        ja       .LUnbounded
        and      $-4, %edx
        jz       .LDWordwise_Test
        push     %edi
        shl      $2, %edx
        movd     %ecx, %xmm2
        add      %eax, %edx
        pshufd   $0, %xmm2, %xmm1

.balign 16
.L4x_Body:
        movdqu   (%eax), %xmm0
        pcmpeqd  %xmm1, %xmm0
        pmovmskb %xmm0, %edi
        test     %edi, %edi
        jnz      .L4x_Found
.L4x_Next:
        add      $16, %eax
        cmp      %eax, %edx
        jne      .L4x_Body

        cmp      %esi, %eax
        je       .LNothing

        lea      -16(%esi), %eax
        movdqu   (%eax), %xmm0
        pcmpeqd  %xmm1, %xmm0
        pmovmskb %xmm0, %edi
        test     %edi, %edi
        jnz      .L4x_Found
.LNothing:
        pop      %edi
        pop      %ebx
        pop      %esi
        mov      $-1, %eax
        ret

.balign 16
.L4x_Found:
        bsf      %edi, %edi
        add      %edi, %eax
        pop      %edi
.LDWordwise_Found:
        sub      %ebx, %eax
        shr      $2, %eax
        pop      %ebx
        pop      %esi
        ret

.balign 16
.LDWordwise_Body:
        cmp      %ecx, (%eax)
        je       .LDWordwise_Found
        add      $4, %eax
.LDWordwise_Test:
        cmp      %esi, %eax
        jne      .LDWordwise_Body
        mov      $-1, %eax
        pop      %ebx
        pop      %esi
        ret

.LUnbounded:
        mov      %eax, %esi
        jmp      .LDWordwise_Body
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


{$ifndef FPC_SYSTEM_HAS_COMPAREBYTE}
{$define FPC_SYSTEM_HAS_COMPAREBYTE}
function CompareByte_Plain(Const buf1,buf2;len:SizeInt):SizeInt; assembler; nostackframe;
asm
        sub     %eax, %edx
        cmp     $6, %ecx
        push    %esi
        lea     (%eax,%ecx), %esi
        jle     .LBytewiseTail_Prepare
        push    %ebx
        lea     3(%eax), %ebx
        and     $-4, %ebx
        cmp     %ebx, %eax
        jne     .LBytewiseHead_Body
.L4x_Prepare:
        mov     %esi, %eax
        and     $-4, %eax
        jmp     .L4x_Body

.balign 16
.L4x_Next:
        add     $4, %ebx
        cmp     %ebx, %eax
        je      .LBytewiseTail_PrepareFromHeadAnd4x
.L4x_Body:
        mov     (%ebx,%edx), %ecx
        cmp     %ecx, (%ebx)
        je      .L4x_Next
        mov     (%ebx), %eax
{$ifdef CPUX86_HAS_BSWAP}
        bswap   %ecx
{$else}
        rol     $8, %cx
        rol     $16, %ecx
        rol     $8, %cx
{$endif}
        pop     %ebx
        pop     %esi
{$ifdef CPUX86_HAS_BSWAP}
        bswap   %eax
{$else}
        rol     $8, %ax
        rol     $16, %eax
        rol     $8, %ax
{$endif}
        cmp     %eax, %ecx
        sbb     %eax, %eax
        and     $2, %eax
        sub     $1, %eax
        ret

.LBytewiseHead_Next:
        add     $1, %eax
        cmp     %eax, %ebx
        je      .L4x_Prepare
.LBytewiseHead_Body:
        movzbl  (%eax,%edx), %ecx
        cmp     (%eax), %cl
        je      .LBytewiseHead_Next
        pop     %ebx
        jmp     .LBytesDiffer

.LBytewiseTail_PrepareFromHeadAnd4x:
        pop     %ebx
.LBytewiseTail_Prepare:
        cmp     %esi, %eax
        jne     .LBytewiseTail_Body
.LNothingFound:
        xor     %eax, %eax
        pop     %esi
        ret

.LBytewiseTail_Next:
        add     $1, %eax
        cmp     %eax, %esi
        je      .LNothingFound
.LBytewiseTail_Body:
        movzbl  (%eax,%edx), %ecx
        cmp     (%eax), %cl
        je      .LBytewiseTail_Next
.LBytesDiffer:
        movzbl  (%eax), %eax
        pop     %esi
        sub     %ecx, %eax
end;

function CompareByte_SSE2(const buf1, buf2; len: SizeInt): SizeInt; assembler; nostackframe;
asm
        cmp      $3, %ecx
        push     %esi
        lea      (%eax,%ecx), %esi { esi = buf1 end }
        jle      .LBytewise_Test
        push     %ebx
        and      $-16, %ecx
        lea      (%eax,%ecx), %ebx { ebx = end of full XMMs in buf1 }
        cmp      %ebx, %eax
        jne      .L16x_Body
        lea      15(%ebx), %eax { check if tails don't cross page boundaries and can be over-read to XMMs }
        lea      15(%edx), %ecx
        xor      %ebx, %eax
        xor      %edx, %ecx
        or       %ecx, %eax
        cmp      $4095, %eax
        ja       .LCantOverReadBoth
        movdqu   (%ebx), %xmm0
        movdqu   (%edx), %xmm2
        pcmpeqb  %xmm2, %xmm0
        pmovmskb %xmm0, %eax
        xor      $65535, %eax
        jz       .LReturnEAX
        bsf      %eax, %ecx
        add      %ecx, %ebx
        cmp      %esi, %ebx { ignore over-read garbage bytes }
        jnb      .L16x_Nothing
        movzbl   (%ebx), %eax
        movzbl   (%edx,%ecx), %edx
        sub      %edx, %eax
.LReturnEAX:
        pop      %ebx
        pop      %esi
        ret

.balign 16
.L16x_Body:
        movdqu   (%edx), %xmm0
        movdqu   (%eax), %xmm1
        pcmpeqb  %xmm1, %xmm0
        pmovmskb %xmm0, %ecx
        xor      $65535, %ecx
        jnz      .L16x_Found
        add      $16, %eax
        add      $16, %edx
        cmp      %eax, %ebx
        jne      .L16x_Body

        cmp      %ebx, %esi
        je       .L16x_Nothing

        sub      %eax, %edx
        lea      -16(%esi), %eax
        add      %eax, %edx
        movdqu   (%edx), %xmm0
        movdqu   (%eax), %xmm1
        pcmpeqb  %xmm1, %xmm0
        pmovmskb %xmm0, %ecx
        xor      $65535, %ecx
        jnz      .L16x_Found
.L16x_Nothing:
        pop      %ebx
        xor      %eax, %eax
        pop      %esi
        ret

.balign 16
.L16x_Found:
        bsf      %ecx, %ecx
        pop      %ebx
        movzbl   (%eax,%ecx), %eax
        movzbl   (%edx,%ecx), %edx
        pop      %esi
        sub      %edx, %eax
        ret

.LCantOverReadBoth:
        mov      %esi, %eax
        sub      %ebx, %eax
        and      $-4, %eax
        add      %ebx, %eax
        cmp      %eax, %ebx
        je       .LPopEbxAndGoBytewise
.L4x_Body:
        mov      (%ebx), %ecx
        cmp      (%edx), %ecx
        jne      .L4x_Found
        add      $4, %ebx
        add      $4, %edx
        cmp      %ebx, %eax
        jne      .L4x_Body
.LPopEbxAndGoBytewise:
        pop      %ebx
.LBytewise_Test:
        cmp      %esi, %eax
        je       .LBytewise_Nothing
.LBytewise_Body:
        movzbl   (%edx), %ecx
        cmp      (%eax), %cl
        jne      .LDoSbb
        add      $1, %eax
        add      $1, %edx
        cmp      %esi, %eax
        jne      .LBytewise_Body
.LBytewise_Nothing:
        xor      %eax, %eax
        pop      %esi
        ret

.L4x_Found:
        mov      (%edx), %eax
        bswap    %ecx
        bswap    %eax
        cmp      %ecx, %eax
        pop      %ebx
.LDoSbb:
        sbb      %eax, %eax
        and      $2, %eax
        sub      $1, %eax
        pop      %esi
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
        sub     %eax, %edx
        push    %esi
        cmp     $1073741823, %ecx
        ja      .LUnbounded
        cmp     $3, %ecx
        lea     (%eax,%ecx,2), %esi
        jle     .LWordwise_Test
        push    %ebx
        test    $3, %al
        je      .LPtrUintWise_Prepare
        movzwl  (%edx,%eax), %ebx
        cmp     (%eax), %bx
        jne     .LPopEbxAndDoSbb
        add     $2, %eax
        sub     $1, %ecx
.LPtrUintWise_Prepare:
        and     $-2, %ecx
        lea     (%eax,%ecx,2), %ecx
.balign 16
.LPtrUintWise_Next:
        mov     (%edx,%eax), %ebx
        cmp     (%eax), %ebx
        jne     .LPtrUintsDiffer
        add     $4, %eax
        cmp     %eax, %ecx
        jne     .LPtrUintWise_Next
        pop     %ebx
.LWordwise_Test:
        cmp     %esi, %eax
        je      .LNothingFound
.LWordwise_Body:
        movzwl  (%edx,%eax), %ecx
        cmp     (%eax), %cx
        jne     .LDoSbb
        add     $2, %eax
        cmp     %esi, %eax
        jne     .LWordwise_Body
.LNothingFound:
        xor     %eax, %eax
        pop     %esi
        ret

.LPtrUintsDiffer:
        cmp     (%eax), %bx
        jne     .LPopEbxAndDoSbb
        shr     $16, %ebx
        cmp     2(%eax), %bx
.LPopEbxAndDoSbb:
        pop     %ebx
.LDoSbb:
        sbb     %eax, %eax
        and     $2, %eax
        sub     $1, %eax
        pop     %esi
        ret

.LUnbounded:
        mov     %eax, %esi
        jmp     .LWordwise_Body
end;

function CompareWord_SSE2(Const buf1,buf2;len:SizeInt):SizeInt; assembler; nostackframe;
asm
        push     %ebx
        cmp      $1073741823, %ecx
        ja       .LUnbounded
        lea      (%eax,%ecx,2), %ebx { ebx = buf1 end }
        cmp      $3, %ecx
        jle      .LWordwise_Test
        push     %esi
        and      $-8, %ecx
        lea      (%eax,%ecx,2), %esi { esi = end of full XMMs in buf1 }
        cmp      %esi, %eax
        jne      .L8x_Body
        lea      15(%esi), %eax
        lea      15(%edx), %ecx
        xor      %esi, %eax
        xor      %edx, %ecx
        or       %ecx, %eax
        cmp      $4095, %eax
        ja       .LCantOverReadBoth
        movdqu   (%esi), %xmm0
        movdqu   (%edx), %xmm2
        pcmpeqw  %xmm2, %xmm0
        pmovmskb %xmm0, %eax
        xor      $65535, %eax
        jz       .LReturnEAX
        bsf      %eax, %eax
        lea      (%esi,%eax), %ecx
        cmp      %ebx, %ecx
        jnb      .LNothing
        movzwl   (%esi,%eax), %ebx
        cmp      %bx, (%edx,%eax)
.L8x_DoSbb:
        pop      %esi
.LWordwise_DoSbb:
        pop      %ebx
        sbb      %eax, %eax
        and      $2, %eax
        sub      $1, %eax
        ret

.balign 16
.L8x_Body:
        movdqu   (%edx), %xmm0
        movdqu   (%eax), %xmm1
        pcmpeqw  %xmm1, %xmm0
        pmovmskb %xmm0, %ecx
        xor      $65535, %ecx
        jnz      .L8x_Found
        add      $16, %eax
        add      $16, %edx
        cmp      %eax, %esi
        jne      .L8x_Body

        cmp      %esi, %ebx
        je       .LNothing

        sub      %eax, %edx
        lea      -16(%ebx), %eax
        add      %eax, %edx
        movdqu   (%edx), %xmm0
        movdqu   (%eax), %xmm1
        pcmpeqw  %xmm1, %xmm0
        pmovmskb %xmm0, %ecx
        xor      $65535, %ecx
        jnz      .L8x_Found
.LNothing:
        xor      %eax, %eax
.LReturnEAX:
        pop      %esi
        pop      %ebx
        ret

.L8x_Found:
        bsf      %ecx, %ecx
        movzwl   (%eax,%ecx), %eax
        cmp      %ax, (%edx,%ecx)
        jmp      .L8x_DoSbb

.LCantOverReadBoth:
        mov      %esi, %eax
        pop      %esi
.LWordwise_Body:
        movzwl   (%eax), %ecx
        cmp      %cx, (%edx)
        jne      .LWordwise_DoSbb
.LWordwise_Next:
        add      $2, %eax
        add      $2, %edx
.LWordwise_Test:
        cmp      %ebx, %eax
        jne      .LWordwise_Body
        xor      %eax, %eax
        pop      %ebx
        ret

.LUnbounded:
        mov      %eax, %ebx
        jmp      .LWordwise_Body
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
        cmp     $536870912, %ecx
        push    %ebx
        jnb     .LUnbounded
        lea     (%eax,%ecx,4), %ebx
        cmp     %ebx, %eax
        je      .LNothing

.balign 16
.LDwordwise_Body:
        mov     (%edx), %ecx
        cmp     (%eax), %ecx
        jne     .LDoSbb
        add     $4, %eax
        add     $4, %edx
        cmp     %eax, %ebx
        jne     .LDwordwise_Body
.LNothing:
        xor     %eax, %eax
        pop     %ebx
        ret

.LDoSbb:
        pop     %ebx
        sbb     %eax, %eax
        and     $2, %eax
        sub     $1, %eax
        ret

.LUnbounded:
        mov     %eax, %ebx
        jmp     .LDwordwise_Body
end;

function CompareDWord_SSE2(Const buf1,buf2;len:SizeInt):SizeInt; assembler; nostackframe;
asm
        push     %esi
        cmp      $536870912, %ecx
        jnb      .LUnbounded
        lea      (%eax,%ecx,4), %esi { esi = buf1 end }
        cmp      $3, %ecx
        jle      .LDWordwise_Test
        push     %ebx
        and      $-4, %ecx
        lea      (%eax,%ecx,4), %ecx { ecx = end of full XMMs in buf1 }
.balign 16
.L4x_Body:
        movdqu   (%edx), %xmm0
        movdqu   (%eax), %xmm1
        pcmpeqd  %xmm1, %xmm0
        pmovmskb %xmm0, %ebx
        xor      $65535, %ebx
        jnz      .L4x_Found
        add      $16, %eax
        add      $16, %edx
        cmp      %eax, %ecx
        jne      .L4x_Body

        cmp      %esi, %ecx
        je       .LNothing

        sub      %eax, %edx
        lea      -16(%esi), %eax
        add      %eax, %edx
        movdqu   (%edx), %xmm0
        movdqu   (%eax), %xmm1
        pcmpeqd  %xmm1, %xmm0
        pmovmskb %xmm0, %ebx
        xor      $65535, %ebx
        jnz      .L4x_Found
.LNothing:
        pop      %ebx
        pop      %esi
        xor      %eax, %eax
        ret

.balign 16
.LDWordwise_Body:
        mov      (%eax), %ecx
        cmp      %ecx, (%edx)
        jne      .LDoSbb
        add      $4, %eax
        add      $4, %edx
.LDWordwise_Test:
        cmp      %esi, %eax
        jne      .LDWordwise_Body
        xor      %eax, %eax
        pop      %esi
        ret

.L4x_Found:
        bsf      %ebx, %ebx
        mov      (%eax,%ebx), %eax
        cmp      %eax, (%edx,%ebx)
        pop      %ebx
.LDoSbb:
        pop      %esi
        sbb      %eax, %eax
        and      $2, %eax
        sub      $1, %eax
        ret

.LUnbounded:
        mov      %eax, %esi
        jmp      .LDWordwise_Body
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
var
  saveres,saveebx,saveesi,saveedi : longint;
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
        movl    %ebx,saveebx
        movl    %esi,saveesi
        movl    %edi,saveedi
        movl    %ecx,%esi
        movl    %eax,%edi
        movl    %edi,saveres
        movl    $1,%ecx
        testl   %esi,%esi
        movl    %esi,%eax
        jz      .LStrPasDone
        leal    3(%esi),%edx
        andl    $-4,%edx
        // skip length byte
        incl    %edi
        subl    %esi,%edx
        jz      .LStrPasAligned
        // align source to multiple of 4 (not dest, because we can't read past
        // the end of the source, since that may be past the end of the heap
        // -> sigsegv!!)
.LStrPasAlignLoop:
        movb    (%esi),%al
        incl    %esi
        testb   %al,%al
        jz      .LStrPasDone
        incl    %edi
        incb    %cl
        decb    %dl
        movb    %al,-1(%edi)
        jne     .LStrPasAlignLoop
        .balign  16
.LStrPasAligned:
        movl    (%esi),%ebx
        addl    $4,%edi
        leal    0x0fefefeff(%ebx),%eax
        movl    %ebx,%edx
        addl    $4,%esi
        notl    %edx
        andl    %edx,%eax
        addl    $4,%ecx
        andl    $0x080808080,%eax
        movl    %ebx,-4(%edi)
        jnz     .LStrPasEndFound
        cmpl    $252,%ecx
        ja      .LStrPasPreEndLoop
        jmp     .LStrPasAligned
.LStrPasEndFound:
        subl    $4,%ecx
        // this won't overwrite data since the result = 255 AnsiChar string
        // and we never process more than the first 255 chars of p
        shrl    $8,%eax
        jc      .LStrPasDone
        incl    %ecx
        shrl    $8,%eax
        jc      .LStrPasDone
        incl    %ecx
        shrl    $8,%eax
        jc      .LStrPasDone
        incl    %ecx
        jmp     .LStrPasDone
.LStrPasPreEndLoop:
        testb   %cl,%cl
        jz      .LStrPasDone
        movl    (%esi),%eax
.LStrPasEndLoop:
        testb   %al,%al
        jz      .LStrPasDone
        movb    %al,(%edi)
        shrl    $8,%eax
        incl    %edi
        incb    %cl
        jnz     .LStrPasEndLoop
.LStrPasDone:
        movl    saveres,%edi
        addb    $255,%cl
        movb    %cl,(%edi)
        movl    saveesi,%esi
        movl    saveedi,%edi
        movl    saveebx,%ebx
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
    _ecx,_edx : longint;
  begin
    if cpuid_support then
      begin
        asm
            movl $1,%eax
            cpuid
            movl %edx,_edx
            movl %ecx,_ecx
        end ['ebx'];
        has_mmx_support:=(_edx and $800000)<>0;
        if ((_edx and $2000000)<>0) then
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
            has_sse2_support:=((_edx and $4000000)<>0);
            has_sse3_support:=((_ecx and $200)<>0);
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
{$ifdef USE_FASTMOVE}
    setup_fastmove;
{$endif}
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
