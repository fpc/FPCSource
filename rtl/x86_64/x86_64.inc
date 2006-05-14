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
{$ENDIF}
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
     cmpb       $0,IsMultithread
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
     cmpb       $0,IsMultithread
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
     cmpb       $0,IsMultithread
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
     cmpb       $0,IsMultithread
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
     cmpb       $0,IsMultithread
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
     cmpb       $0,IsMultithread
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
     cmpb       $0,IsMultithread
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
     cmpb       $0,IsMultithread
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


{****************************************************************************
                                  FPU
****************************************************************************}

const
  fpucw : word = $1332;

  MM_MaskInvalidOp = %0000000010000000;
  MM_MaskDenorm    = %0000000100000000;
  MM_MaskDivZero   = %0000001000000000;
  MM_MaskOverflow  = %0000010000000000;
  MM_MaskUnderflow = %0000100000000000;
  MM_MaskPrecision = %0001000000000000;
  mxcsr : dword = MM_MaskOverflow or MM_MaskUnderflow or MM_MaskPrecision;

  { Internal constants for use in system unit }
  FPU_Invalid = 1;
  FPU_Denormal = 2;
  FPU_DivisionByZero = 4;
  FPU_Overflow = 8;
  FPU_Underflow = $10;
  FPU_StackUnderflow = $20;
  FPU_StackOverflow = $40;
  FPU_ExceptionMask = $ff;

{$define FPC_SYSTEM_HAS_SYSRESETFPU}
Procedure SysResetFPU;assembler;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
{$ifndef WIN64}
  { initialize fpu }
  fninit
  fwait
{$endif WIN64}
{$ifdef FPC_PIC}
  movq fpucw@GOTPCREL(%rip),%rax
  fldcw (%rax)
  { set sse exceptions }
  movq mxcsr@GOTPCREL(%rip),%rax
  ldmxcsr (%rax)
{$else FPC_PIC}
  fldcw fpucw
  { set sse exceptions }
  ldmxcsr mxcsr
{$endif FPC_PIC}
end;
