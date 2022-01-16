{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl

    This unit contains some routines to get informations about the
    processor

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
unit cpu;

  interface

    uses
      sysutils;

    { returns true, if the processor supports the cpuid instruction }
    function cpuid_support : boolean;

    { returns true, if floating point is done by an emulator }
    function floating_point_emulation : boolean;

    { returns the contents of the cr0 register }
    function cr0 : longint;

//    function InterlockedCompareExchange128Support : boolean;
//    function AESSupport : boolean;inline;
//    function AVXSupport: boolean;inline;
//    function AVX2Support: boolean;inline;
//    function FMASupport: boolean;inline;

//    var
//      is_sse3_cpu : boolean = false;

    function InterlockedCompareExchange128(var Target: Int128Rec; NewValue: Int128Rec; Comperand: Int128Rec): Int128Rec;

  implementation

{$ASMMODE INTEL}
{$ASMCPU 80386}
//    var
//      _AVXSupport,
//      _AVX2Support,
//      _AESSupport,
//      _FMASupport : boolean;


    function InterlockedCompareExchange128(var Target: Int128Rec; NewValue: Int128Rec; Comperand: Int128Rec): Int128Rec;
      begin
        RunError(217);
      end;


    function cpuid_support : boolean;assembler;nostackframe;
      {
        Check if the ID-flag can be changed, if changed then CpuID is supported.
      }
      asm
        xor ax, ax
{$IFDEF FPC_MM_HUGE}
        mov bx, SEG Test8086
        mov es, bx
        cmp byte ptr es:[Test8086], 2  { 2 = 80386 or later }
{$ELSE FPC_MM_HUGE}
        cmp byte ptr [Test8086], 2  { 2 = 80386 or later }
{$ENDIF FPC_MM_HUGE}
        jb @@Done  { Test8086<2 means 80286 or earlier }
        pushfd
        pushfd
        pop bx
        pop cx
        mov dx, cx
        xor cx, 20h
        push cx
        push bx
        popfd
        pushfd
        pop bx
        pop cx
        popfd
        and cx, 20h
        and dx, 20h
        cmp cx, dx
        je @@Done
        inc ax
      @@Done:
      end;


    function cr0 : longint;assembler;nostackframe;
      asm
        int 3
        xor ax, ax
        xor dx, dx
{$IFDEF FPC_MM_HUGE}
        mov bx, SEG Test8086
        mov es, bx
        mov al, byte ptr es:[Test8086]
{$ELSE FPC_MM_HUGE}
        mov al, byte ptr [Test8086]
{$ENDIF FPC_MM_HUGE}
        test al, al  { 0 = 8086/8088/80186/80188/NEC v20/v30: no cr0/msw register; simply return 0 }
        jz @@Done
        cmp al, 1  { 1 = 80286 }
        jne @@386_or_later
        { on 80286 we use SMSW (high 16 bits are 0, because dx=0) }
        smsw ax
        jmp @@Done
      @@386_or_later:
        { use smsw first, to see if we are in real mode (mov eax, cr0 isn't supported in virtual 8086 mode) }
        smsw ax
        test al, 1
        jz @@386_read_cr0  { real mode: we can read cr0 }
        { 386 in protected mode; check the virtual 8086 mode flag }
        { unfortunately, this doesn't work, because pushfd never pushes the virtual 8086 mode flag as 1 }
        //pushfd
        //pop cx
        //pop cx
        //test cl, 2
        //jnz @@Done  {if in virtual 8086 mode, we can't read cr0, so just return the SMSW result }
        { so, we must assume virtual 8086 mode and just return the SMSW result }
        { TODO: this can be updated for e.g. protected mode 286 targets, where we
          can safely assume the CPU is not in virtual 8086 mode, but proper protected mode
          and still try to read cr0 }
        jmp @@Done
@@386_read_cr0:
        pushf  { save the previous state of the interrupt flag }
        cli    { disable interrupts, because some buggy TSR may destroy the high
                 16 bits of eax in an interrupt handler (BP7's 32-bit mul/div
                 helpers do this, when they detect a 386 or later, so e.g. a BP7
                 TSR that hooks the timer interrupt could do this) }
        { store eax, so that we preserve the high 16 bits of eax }
        push eax
        mov eax, cr0
        push eax
        pop cx
        pop dx
        pop eax  { restore eax }
        mov ax, cx
        popf  { restore interrupts to their previous state }
      @@Done:
      end;


    function floating_point_emulation : boolean;
      begin
         {!!!! I don't know currently the position of the EM flag }
         { $4 after Ralf Brown's list }
         floating_point_emulation:=(cr0 and $4)<>0;
      end;


{$ASMMODE ATT}
//    function XGETBV(i : dword) : int64;assembler;
//      asm
//        movl %eax,%ecx
//        // older FPCs don't know the xgetbv opcode
//        .byte 0x0f,0x01,0xd0
//      end;


//    procedure SetupSupport;
//      var
//         _ecx,_ebx : longint;
//      begin
//        is_sse3_cpu:=false;
//         if cpuid_support then
//           begin
//              asm
//                 pushl %ebx
//                 movl $1,%eax
//                 cpuid
//                 movl %ecx,_ecx
//                 popl %ebx
//              end;
//              _AESSupport:=(_ecx and $2000000)<>0;
//
//              _AVXSupport:=
//                { XGETBV suspport? }
//                ((_ecx and $08000000)<>0) and
//                { xmm and ymm state enabled? }
//                ((XGETBV(0) and %110)=%110) and
//                { avx supported? }
//                ((_ecx and $10000000)<>0);
//
//              is_sse3_cpu:=(_ecx and $1)<>0;
//
//              _FMASupport:=_AVXSupport and ((_ecx and $1000)<>0);
//
//              asm
//                 pushl %ebx
//                 movl $7,%eax
//                 movl $0,%ecx
//                 cpuid
//                 movl %ebx,_ebx
//                 popl %ebx
//              end;
//              _AVX2Support:=_AVXSupport and ((_ebx and $20)<>0);
//           end;
//      end;


//    function InterlockedCompareExchange128Support : boolean;
//      begin
//        { 32 Bit CPUs have no 128 Bit interlocked exchange support }
//        result:=false;
//      end;


//    function AESSupport : boolean;
//      begin
//        result:=_AESSupport;
//      end;


//    function AVXSupport: boolean;inline;
//      begin
//        result:=_AVXSupport;
//      end;


//    function AVX2Support: boolean;inline;
//      begin
//        result:=_AVX2Support;
//      end;


//    function FMASupport: boolean;inline;
//      begin
//        result:=_FMASupport;
//      end;

begin
//  SetupSupport;
end.
