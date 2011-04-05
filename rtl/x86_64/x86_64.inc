{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Florian Klaempfl.
    Member of the Free Pascal development team

    Parts of this code are derived from the x86-64 linux port
    Copyright 2002 Andi Kleen

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
Function Sptr : Pointer;assembler;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
        movq    %rsp,%rax
end ['RAX'];

{$IFNDEF INTERNAL_BACKTRACE}
{$define FPC_SYSTEM_HAS_GET_FRAME}
function get_frame:pointer;assembler;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
        movq    %rbp,%rax
end ['RAX'];
{$ENDIF not INTERNAL_BACKTRACE}


{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:pointer):pointer;assembler;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
{$ifdef win64}
        orq     %rcx,%rcx
        jz      .Lg_a_null
        movq    8(%rcx),%rax
{$else win64}
        { %rdi = framebp }
        orq     %rdi,%rdi
        jz      .Lg_a_null
        movq    8(%rdi),%rax
{$endif win64}
.Lg_a_null:
end ['RAX'];


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer):pointer;assembler;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
{$ifdef win64}
        orq     %rcx,%rcx
        jz      .Lg_a_null
        movq    (%rcx),%rax
{$else win64}
        { %rdi = framebp }
        orq     %rdi,%rdi
        jz      .Lg_a_null
        movq    (%rdi),%rax
{$endif win64}
.Lg_a_null:
end ['RAX'];

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

    mov    %r8, %rax
    sub    %rdx, %rcx            { rcx = src - dest }
    jz     .Lquit                { exit if src=dest }
    jnb    .L1                   { src>dest => forward move }

    add    %rcx, %rax            { rcx is negative => r8+rcx > 0 if regions overlap }
    jb     .Lback                { if no overlap, still do forward move }

.L1:
    cmp    $8, %r8
    jl     .Lless8f              { signed compare, negative count not allowed }
    test   $7, %dl
    je     .Ldestaligned

    test   $1, %dl               { align dest by moving first 1+2+4 bytes }
    je     .L2f
    mov    (%rcx,%rdx,1),%al
    dec    %r8
    mov    %al, (%rdx)
    add    $1, %rdx
.L2f:
    test   $2, %dl
    je     .L4f
    mov    (%rcx,%rdx,1),%ax
    sub    $2, %r8
    mov    %ax, (%rdx)
    add    $2, %rdx
.L4f:
    test   $4, %dl
    je     .Ldestaligned
    mov    (%rcx,%rdx,1),%eax
    sub    $4, %r8
    mov    %eax, (%rdx)
    add    $4, %rdx

.Ldestaligned:
    mov    %r8, %r9
    shr    $5, %r9
    jne    .Lmore32

.Ltail:
    mov    %r8, %r9
    shr    $3, %r9
    je     .Lless8f

    .balign 16
.Lloop8f:                             { max. 8 iterations }
    mov    (%rcx,%rdx,1),%rax
    mov    %rax, (%rdx)
    add    $8, %rdx
    dec    %r9
    jne    .Lloop8f
    and    $7, %r8

.Lless8f:
    test   %r8, %r8
    jle    .Lquit

    .balign 16
.Lloop1f:
    mov    (%rcx,%rdx,1),%al
    mov    %al,(%rdx)
    inc    %rdx
    dec    %r8
    jne    .Lloop1f
.Lquit:
    retq


.Lmore32:
    cmp    $0x2000, %r9          { this limit must be processor-specific (1/2 L2 cache size) }
    jnae   .Lloop32
    cmp    $0x1000, %rcx         { but don't bother bypassing cache if src and dest }
    jnb    .Lntloopf             { are close to each other}

    .balign 16
.Lloop32:
    add    $32,%rdx
    mov    -32(%rcx,%rdx,1),%rax
    mov    -24(%rcx,%rdx,1),%r10
    mov    %rax,-32(%rdx)
    mov    %r10,-24(%rdx)
    dec    %r9
    mov    -16(%rcx,%rdx,1),%rax
    mov    -8(%rcx,%rdx,1),%r10
    mov    %rax,-16(%rdx)
    mov    %r10,-8(%rdx)
    jne    .Lloop32

    and    $0x1f, %r8
    jmpq   .Ltail

.Lntloopf:
    mov    $32, %eax

    .balign 16
.Lpref:
    prefetchnta (%rcx,%rdx,1)
    prefetchnta 0x40(%rcx,%rdx,1)
    add    $0x80, %rdx
    dec    %eax
    jne    .Lpref

    sub    $0x1000, %rdx
    mov    $64, %eax

    .balign 16
.Loop64:
    add    $64, %rdx
    mov    -64(%rcx,%rdx,1), %r9
    mov    -56(%rcx,%rdx,1), %r10
    movnti %r9, -64(%rdx)
    movnti %r10, -56(%rdx)

    mov    -48(%rcx,%rdx,1), %r9
    mov    -40(%rcx,%rdx,1), %r10
    movnti %r9, -48(%rdx)
    movnti %r10, -40(%rdx)
    dec    %eax
    mov    -32(%rcx,%rdx,1), %r9
    mov    -24(%rcx,%rdx,1), %r10
    movnti %r9, -32(%rdx)
    movnti %r10, -24(%rdx)

    mov    -16(%rcx,%rdx,1), %r9
    mov    -8(%rcx,%rdx,1), %r10
    movnti %r9, -16(%rdx)
    movnti %r10, -8(%rdx)
    jne    .Loop64

    sub    $0x1000, %r8
    cmp    $0x1000, %r8
    jae    .Lntloopf

    mfence
    jmpq    .Ldestaligned        { go handle remaining bytes }

{ backwards move }
.Lback:
    add    %r8, %rdx             { points to the end of dest }
    cmp    $8, %r8
    jl     .Lless8b              { signed compare, negative count not allowed }
    test   $7, %dl
    je     .Ldestalignedb
    test   $1, %dl
    je     .L2b
    dec    %rdx
    mov    (%rcx,%rdx,1), %al
    dec    %r8
    mov    %al, (%rdx)
.L2b:
    test   $2, %dl
    je     .L4b
    sub    $2, %rdx
    mov    (%rcx,%rdx,1), %ax
    sub    $2, %r8
    mov    %ax, (%rdx)
.L4b:
    test   $4, %dl
    je     .Ldestalignedb
    sub    $4, %rdx
    mov    (%rcx,%rdx,1), %eax
    sub    $4, %r8
    mov    %eax, (%rdx)

.Ldestalignedb:
    mov    %r8, %r9
    shr    $5, %r9
    jne    .Lmore32b

.Ltailb:
    mov    %r8, %r9
    shr    $3, %r9
    je     .Lless8b

.Lloop8b:
    sub    $8, %rdx
    mov    (%rcx,%rdx,1), %rax
    dec    %r9
    mov    %rax, (%rdx)
    jne    .Lloop8b
    and    $7, %r8

.Lless8b:
    test   %r8, %r8
    jle    .Lquit2

    .balign 16
.Lsmallb:
    dec   %rdx
    mov   (%rcx,%rdx,1), %al
    dec   %r8
    mov   %al,(%rdx)
    jnz   .Lsmallb
.Lquit2:
    retq

.Lmore32b:
    cmp   $0x2000, %r9
    jnae  .Lloop32b
    cmp    $0xfffffffffffff000,%rcx
    jb     .Lntloopb

    .balign 16
.Lloop32b:
    sub    $32, %rdx
    mov    24(%rcx,%rdx,1), %rax
    mov    16(%rcx,%rdx,1), %r10
    mov    %rax, 24(%rdx)
    mov    %r10, 16(%rdx)
    dec    %r9
    mov    8(%rcx,%rdx,1),%rax
    mov    (%rcx,%rdx,1), %r10
    mov    %rax, 8(%rdx)
    mov    %r10, (%rdx)
    jne    .Lloop32b
    and    $0x1f, %r8
    jmpq   .Ltailb


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
.Lloop64b:
    sub    $64, %rdx
    mov    56(%rcx,%rdx,1), %r9
    mov    48(%rcx,%rdx,1), %r10
    movnti %r9, 56(%rdx)
    movnti %r10, 48(%rdx)

    mov    40(%rcx,%rdx,1), %r9
    mov    32(%rcx,%rdx,1), %r10
    movnti %r9, 40(%rdx)
    movnti %r10, 32(%rdx)
    dec    %eax
    mov    24(%rcx,%rdx,1), %r9
    mov    16(%rcx,%rdx,1), %r10
    movnti %r9, 24(%rdx)
    movnti %r10, 16(%rdx)

    mov    8(%rcx,%rdx,1), %r9
    mov    (%rcx,%rdx,1), %r10
    movnti %r9, 8(%rdx)
    movnti %r10, (%rdx)
    jne    .Lloop64b

    sub    $0x1000, %r8
    cmp    $0x1000, %r8
    jae    .Lntloopb
    mfence
    jmpq   .Ldestalignedb
end;

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

    cmp    $8, %rdx
    jl     .Ltiny

    { expand byte value  }
    movzbl %r8b, %r8
    mov    $0x0101010101010101,%r9
    imul   %r9, %r8

    test   $7, %cl
    je     .Laligned

    { align dest to 8 bytes }
    test   $1, %cl
    je     .L2
    movb   %r8b, (%rcx)
    add    $1, %rcx
    sub    $1, %rdx
.L2:
    test   $2, %cl
    je     .L4
    movw   %r8w, (%rcx)
    add    $2, %rcx
    sub    $2, %rdx
.L4:
    test   $4, %cl
    je     .Laligned
    movl   %r8d, (%rcx)
    add    $4, %rcx
    sub    $4, %rdx

.Laligned:
    mov    %rdx, %rax
    and    $0x3f, %rdx
    shr    $6, %rax
    jne    .Lmore64

.Lless64:
    mov    %rdx, %rax
    and    $7, %rdx
    shr    $3, %rax
    je     .Ltiny

    .balign 16
.Lloop8:                               { max. 8 iterations }
    mov    %r8, (%rcx)
    add    $8, %rcx
    dec    %rax
    jne    .Lloop8
.Ltiny:
    test   %rdx, %rdx
    jle    .Lquit
.Lloop1:
    movb   %r8b, (%rcx)
    inc    %rcx
    dec    %rdx
    jnz    .Lloop1
.Lquit:
    retq

.Lmore64:
    cmp    $0x2000,%rax
    jae    .Lloop64nti

    .balign 16
.Lloop64:
    add    $64, %rcx
    mov    %r8, -64(%rcx)
    mov    %r8, -56(%rcx)
    mov    %r8, -48(%rcx)
    mov    %r8, -40(%rcx)
    dec    %rax
    mov    %r8, -32(%rcx)
    mov    %r8, -24(%rcx)
    mov    %r8, -16(%rcx)
    mov    %r8, -8(%rcx)
    jne    .Lloop64
    jmp    .Lless64

    .balign 16
.Lloop64nti:
    add    $64, %rcx
    movnti %r8, -64(%rcx)
    movnti %r8, -56(%rcx)
    movnti %r8, -48(%rcx)
    movnti %r8, -40(%rcx)
    dec    %rax
    movnti %r8, -32(%rcx)
    movnti %r8, -24(%rcx)
    movnti %r8, -16(%rcx)
    movnti %r8, -8(%rcx)
    jnz    .Lloop64nti
    mfence
    jmp    .Lless64
  end;


{$define FPC_SYSTEM_HAS_DECLOCKED_LONGINT}
{ does a thread save inc/dec }
function declocked(var l : longint) : boolean;assembler;
  asm
{$ifdef win64}
     {
       l: %rcx
     }
     { this check should be done because a lock takes a lot }
     { of time!                                             }
     cmpb       $0,IsMultithread{$ifdef FPC_HAS_RIP_RELATIVE}(%rip){$endif}
     jz         .Ldeclockednolock
     lock
     decl       (%rcx)
     jmp        .Ldeclockedend
.Ldeclockednolock:
     decl       (%rcx)
.Ldeclockedend:
     setzb      %al
{$else win64}
     {
       l: %rdi
     }
     { this check should be done because a lock takes a lot }
     { of time!                                             }
{$ifdef FPC_PIC}
     movq       IsMultithread@GOTPCREL(%rip),%rax
     cmpb       $0,(%rax)
{$else FPC_PIC}
     cmpb       $0,IsMultithread{$ifdef FPC_HAS_RIP_RELATIVE}(%rip){$endif}
{$endif FPC_PIC}
     jz         .Ldeclockednolock
     lock
     decl       (%rdi)
     jmp        .Ldeclockedend
.Ldeclockednolock:
     decl       (%rdi)
.Ldeclockedend:
     setzb      %al
{$endif win64}
  end;


{$define FPC_SYSTEM_HAS_DECLOCKED_INT64}
function declocked(var l : int64) : boolean;assembler;
  asm
{$ifdef win64}
     {
       l: %rcx
     }
     { this check should be done because a lock takes a lot }
     { of time!                                             }
     cmpb       $0,IsMultithread{$ifdef FPC_HAS_RIP_RELATIVE}(%rip){$endif}
     jz         .Ldeclockednolock
     lock
     decq       (%rcx)
     jmp        .Ldeclockedend
.Ldeclockednolock:
     decq       (%rcx)
.Ldeclockedend:
     setzb      %al
{$else win64}
     {
       l: %rdi
     }
     { this check should be done because a lock takes a lot }
     { of time!                                             }
{$ifdef FPC_PIC}
     movq       IsMultithread@GOTPCREL(%rip),%rax
     cmpb       $0,(%rax)
{$else FPC_PIC}
     cmpb       $0,IsMultithread{$ifdef FPC_HAS_RIP_RELATIVE}(%rip){$endif}
{$endif FPC_PIC}
     jz         .Ldeclockednolock
     lock
     decq       (%rdi)
     jmp        .Ldeclockedend
.Ldeclockednolock:
     decq       (%rdi)
.Ldeclockedend:
     setzb      %al
{$endif win64}
  end;


{$define FPC_SYSTEM_HAS_INCLOCKED_LONGINT}
procedure inclocked(var l : longint);assembler;

  asm
{$ifdef win64}
     {
       l: %rcx
     }
     { this check should be done because a lock takes a lot }
     { of time!                                             }
     cmpb       $0,IsMultithread{$ifdef FPC_HAS_RIP_RELATIVE}(%rip){$endif}
     jz         .Linclockednolock
     lock
     incl       (%rcx)
     jmp        .Linclockedend
.Linclockednolock:
     incl       (%rcx)
.Linclockedend:
{$else win64}
     {
       l: %rdi
     }
     { this check should be done because a lock takes a lot }
     { of time!                                             }
{$ifdef FPC_PIC}
     movq       IsMultithread@GOTPCREL(%rip),%rax
     cmpb       $0,(%rax)
{$else FPC_PIC}
     cmpb       $0,IsMultithread{$ifdef FPC_HAS_RIP_RELATIVE}(%rip){$endif}
{$endif FPC_PIC}
     jz         .Linclockednolock
     lock
     incl       (%rdi)
     jmp        .Linclockedend
.Linclockednolock:
     incl       (%rdi)
.Linclockedend:
{$endif win64}
  end;


{$define FPC_SYSTEM_HAS_INCLOCKED_INT64}
procedure inclocked(var l : int64);assembler;

  asm
{$ifdef win64}
     {
       l: %rcx
     }
     { this check should be done because a lock takes a lot }
     { of time!                                             }
     cmpb       $0,IsMultithread{$ifdef FPC_HAS_RIP_RELATIVE}(%rip){$endif}
     jz         .Linclockednolock
     lock
     incq       (%rcx)
     jmp        .Linclockedend
.Linclockednolock:
     incq       (%rcx)
.Linclockedend:
{$else win64}
     {
       l: %rdi
     }
     { this check should be done because a lock takes a lot }
     { of time!                                             }
{$ifdef FPC_PIC}
     movq       IsMultithread@GOTPCREL(%rip),%rax
     cmpb       $0,(%rax)
{$else FPC_PIC}
     cmpb       $0,IsMultithread{$ifdef FPC_HAS_RIP_RELATIVE}(%rip){$endif}
{$endif FPC_PIC}
     jz         .Linclockednolock
     lock
     incq       (%rdi)
     jmp        .Linclockedend
.Linclockednolock:
     incq       (%rdi)
.Linclockedend:
{$endif win64}
  end;


function InterLockedDecrement (var Target: longint) : longint; assembler;
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


function InterLockedIncrement (var Target: longint) : longint; assembler;
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


function InterLockedExchange (var Target: longint;Source : longint) : longint; assembler;
asm
{$ifdef win64}
        xchgl   (%rcx),%edx
        movl    %edx,%eax
{$else win64}
        xchgl   (%rdi),%esi
        movl    %esi,%eax
{$endif win64}
end;


function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint; assembler;
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


function InterLockedCompareExchange(var Target: longint; NewValue, Comperand : longint): longint; assembler;
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


function InterLockedDecrement64 (var Target: int64) : int64; assembler;
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


function InterLockedIncrement64 (var Target: int64) : int64; assembler;
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


function InterLockedExchange64 (var Target: int64;Source : int64) : int64; assembler;
asm
{$ifdef win64}
        xchgq   (%rcx),%rdx
        movq    %rdx,%rax
{$else win64}
        xchgq   (%rdi),%rsi
        movq    %rsi,%rax
{$endif win64}
end;


function InterLockedExchangeAdd64 (var Target: int64;Source : int64) : int64; assembler;
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


function InterLockedCompareExchange64(var Target: int64; NewValue, Comperand : int64): int64; assembler;
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

  fpucw : word = $1300 or FPU_StackUnderflow or FPU_Underflow or FPU_Denormal;

  MM_MaskInvalidOp = %0000000010000000;
  MM_MaskDenorm    = %0000000100000000;
  MM_MaskDivZero   = %0000001000000000;
  MM_MaskOverflow  = %0000010000000000;
  MM_MaskUnderflow = %0000100000000000;
  MM_MaskPrecision = %0001000000000000;

  mxcsr : dword = MM_MaskUnderflow or MM_MaskPrecision or MM_MaskDenorm;

procedure fpc_cpuinit;
  begin
    { don't let libraries influence the FPU cw set by the host program }
    if IsLibrary then
      begin
        Default8087CW:=Get8087CW;
        mxcsr:=GetSSECSR;
      end;
    SysResetFPU;
    if not(IsLibrary) then
      SysInitFPU;
  end;

{$define FPC_SYSTEM_HAS_SYSINITFPU}
Procedure SysInitFPU;
  var
    { these locals are so we don't have to hack pic code in the assembler }
    localmxcsr: dword;
    localfpucw: word;
 
begin
  localmxcsr:=mxcsr;
  localfpucw:=fpucw;
  asm
    fldcw localfpucw
    { set sse exceptions }
    ldmxcsr localmxcsr
  end ['RAX'];
  { x86-64 might use softfloat code }
  softfloat_exception_mask:=float_flag_underflow or float_flag_inexact or float_flag_denormal;
end;


{$define FPC_SYSTEM_HAS_SYSRESETFPU}
Procedure SysResetFPU;
  var
    { these locals are so we don't have to hack pic code in the assembler }
    localmxcsr: dword;
    localfpucw: word;
  begin
    localfpucw:=Default8087CW;
    localmxcsr:=mxcsr;
    asm
      fninit
      fwait
      fldcw   localfpucw
      ldmxcsr localmxcsr
    end;
    { x86-64 might use softfloat code }
    softfloat_exception_flags:=0;
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
    Result := SmallInt((Word(AValue) shr 8) or (Word(AValue) shl 8));
  end;


function SwapEndian(const AValue: Word): Word;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    Result := Word((AValue shr 8) or (AValue shl 8));
  end;


function SwapEndian(const AValue: LongInt): LongInt; assembler;
asm
{$ifdef win64}
  movl %ecx, %eax
{$else win64}
  movl %edi, %eax
{$endif win64}
  bswap %eax
end;


function SwapEndian(const AValue: DWord): DWord; assembler;
asm
{$ifdef win64}
  movl %ecx, %eax
{$else win64}
  movl %edi, %eax
{$endif win64}
  bswap %eax
end;


function SwapEndian(const AValue: Int64): Int64; assembler;
asm
{$ifdef win64}
  movq %rcx, %rax
{$else win64}
  movq %rdi, %rax
{$endif win64}
  bswap %rax
end;


function SwapEndian(const AValue: QWord): QWord; assembler;
asm
{$ifdef win64}
  movq %rcx, %rax
{$else win64}
  movq %rdi, %rax
{$endif win64}
  bswap %rax
end;
