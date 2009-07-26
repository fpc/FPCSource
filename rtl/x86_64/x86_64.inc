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

procedure fpc_cpuinit;
  begin
    SysResetFPU;
    if not(IsLibrary) then
      SysInitFPU;
  end;

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

(*
{$define FPC_SYSTEM_HAS_MOVE}
procedure Move(const source;var dest;count:longint);[public, alias: 'FPC_MOVE'];assembler;
  asm
     { rdi destination
       rsi source
       rdx count
     }
     pushq %rbx
     prefetcht0 (%rsi)  // for more hopefully the hw prefetch will kick in
     movq %rdi,%rax

     movl %edi,%ecx
     andl $7,%ecx
     jnz  .Lbad_alignment
.Lafter_bad_alignment:
     movq %rdx,%rcx
     movl $64,%ebx
     shrq $6,%rcx
     jz .Lhandle_tail

.Lloop_64:
     { no prefetch because we assume the hw prefetcher does it already
       and we have no specific temporal hint to give. XXX or give a nta
       hint for the source? }
     movq (%rsi),%r11
     movq 8(%rsi),%r8
     movq 2*8(%rsi),%r9
     movq 3*8(%rsi),%r10
     movnti %r11,(%rdi)
     movnti %r8,1*8(%rdi)
     movnti %r9,2*8(%rdi)
     movnti %r10,3*8(%rdi)

     movq 4*8(%rsi),%r11
     movq 5*8(%rsi),%r8
     movq 6*8(%rsi),%r9
     movq 7*8(%rsi),%r10
     movnti %r11,4*8(%rdi)
     movnti %r8,5*8(%rdi)
     movnti %r9,6*8(%rdi)
     movnti %r10,7*8(%rdi)

     addq %rbx,%rsi
     addq %rbx,%rdi
     loop .Lloop_64

.Lhandle_tail:
     movl %edx,%ecx
     andl $63,%ecx
     shrl $3,%ecx
     jz   .Lhandle_7
     movl $8,%ebx
.Lloop_8:
     movq (%rsi),%r8
     movnti %r8,(%rdi)
     addq %rbx,%rdi
     addq %rbx,%rsi
     loop .Lloop_8

.Lhandle_7:
     movl %edx,%ecx
     andl $7,%ecx
     jz .Lende
.Lloop_1:
     movb (%rsi),%r8b
     movb %r8b,(%rdi)
     incq %rdi
     incq %rsi
     loop .Lloop_1

     jmp .Lende

     { align destination }
     { This is simpleminded. For bigger blocks it may make sense to align
        src and dst to their aligned subset and handle the rest separately }
.Lbad_alignment:
     movl $8,%r9d
     subl %ecx,%r9d
     movl %r9d,%ecx
     subq %r9,%rdx
     js   .Lsmall_alignment
     jz   .Lsmall_alignment
.Lalign_1:
     movb (%rsi),%r8b
     movb %r8b,(%rdi)
     incq %rdi
     incq %rsi
     loop .Lalign_1
     jmp .Lafter_bad_alignment
.Lsmall_alignment:
     addq %r9,%rdx
     jmp .Lhandle_7

.Lende:
     sfence
     popq %rbx
  end;
*)

(*
{$define FPC_SYSTEM_HAS_FILLCHAR}
Procedure FillChar(var x;count:longint;value:byte);assembler;
  asm
    { rdi   destination
      rsi   value (char)
      rdx   count (bytes)
    }
    movq %rdi,%r10
    movq %rdx,%r11

    { expand byte value  }
    movzbl %sil,%ecx
    movabs $0x0101010101010101,%rax
    mul    %rcx         { with rax, clobbers rdx }

    { align dst }
    movl  %edi,%r9d
    andl  $7,%r9d
    jnz  .Lbad_alignment
.Lafter_bad_alignment:

     movq %r11,%rcx
     movl $64,%r8d
     shrq $6,%rcx
     jz  .Lhandle_tail

.Lloop_64:
     movnti  %rax,(%rdi)
     movnti  %rax,8(%rdi)
     movnti  %rax,16(%rdi)
     movnti  %rax,24(%rdi)
     movnti  %rax,32(%rdi)
     movnti  %rax,40(%rdi)
     movnti  %rax,48(%rdi)
     movnti  %rax,56(%rdi)
     addq    %r8,%rdi
     loop    .Lloop_64

     { Handle tail in loops. The loops should be faster than hard
        to predict jump tables. }
.Lhandle_tail:
     movl       %r11d,%ecx
     andl    $56,%ecx
     jz     .Lhandle_7
     shrl       $3,%ecx
.Lloop_8:
     movnti  %rax,(%rdi)
     addq    $8,%rdi
     loop    .Lloop_8
.Lhandle_7:
     movl       %r11d,%ecx
     andl       $7,%ecx
     jz      .Lende
.Lloop_1:
     movb       %al,(%rdi)
     addq       $1,%rdi
     loop       .Lloop_1

     jmp .Lende

.Lbad_alignment:
     cmpq $7,%r11
     jbe .Lhandle_7
     movnti %rax,(%rdi) (* unaligned store *)
     movq $8,%r8
     subq %r9,%r8
     addq %r8,%rdi
     subq %r8,%r11
     jmp .Lafter_bad_alignment

.Lende:
     movq       %r10,%rax
  end;
*)


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

{$define FPC_SYSTEM_HAS_SYSINITFPU}
Procedure SysInitFPU;
begin
  asm
{$ifdef FPC_PIC}
    movq fpucw@GOTPCREL(%rip),%rax
    fldcw (%rax)
    { set sse exceptions }
    movq mxcsr@GOTPCREL(%rip),%rax
    ldmxcsr (%rax)
{$else FPC_PIC}
    fldcw fpucw{$ifdef FPC_HAS_RIP_RELATIVE}(%rip){$endif}
    { set sse exceptions }
    ldmxcsr mxcsr{$ifdef FPC_HAS_RIP_RELATIVE}(%rip){$endif}
{$endif FPC_PIC}
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
  { no write reordering on intel CPUs (yet) }
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
