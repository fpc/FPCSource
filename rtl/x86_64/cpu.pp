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
{$IFNDEF FPC_DOTTEDUNITS}
unit cpu;
{$ENDIF FPC_DOTTEDUNITS}

  interface

  {$ifdef freebsd}                 // FreeBSD 7/8 have binutils version that don't support cmpxchg16b
			           // Unless overridebinutils is defined (for ports usage), use db instead of the instruction
     {$ifndef overridebinutils}
       {$define oldbinutils}
     {$endif}
  {$endif}

type
    TCpuidResult = record
      eax, ebx, ecx, edx: uint32;
    end;

    function CPUID(in_eax: uint32; in_ecx: uint32 = 0): TCpuidResult; inline;
    function CPUBrandString: shortstring;

    function InterlockedCompareExchange128Support : boolean;inline;
    function TSCSupport: boolean;inline;
    function MMXSupport: boolean;inline;
    function CMOVSupport : boolean;inline;
    function AESSupport : boolean;inline;
    function AVXSupport : boolean;inline;
    function AVX2Support: boolean;inline;
    function AVX101Support: boolean;inline; { AVX10.1 }
    function AVX102Support: boolean;inline; { AVX10.2 }
    function AVX10_256Support: boolean;inline; { AVX10/256 indicates that 256-bit vector support is present }
    function AVX10_512Support: boolean;inline; { AVX10/512 indicates that 512-bit vector support is present }
    function APXSupport: boolean;inline;  { APX_F Advanced Performance Extension Foundation }
    function AVX512FSupport: boolean;inline;
    function AVX512DQSupport: boolean;inline;
    function AVX512IFMASupport: boolean;inline;
    function AVX512PFSupport: boolean;inline;
    function AVX512ERSupport: boolean;inline;
    function AVX512CDSupport: boolean;inline;
    function AVX512BWSupport: boolean;inline;
    function AVX512VLSupport: boolean;inline;
    function AVX512VBMISupport: boolean;inline;
    function AVX512VBMI2Support: boolean;inline;
    function AVX512VNNISupport: boolean;inline;
    function AVX512VPOPCNTDQSupport: boolean;inline;
    function AVX512BF16Support: boolean;inline;
    function AVX512FP16Support: boolean;inline;
    function AVX512VP2INTERSECTSupport: boolean;inline;
    function AVX5124VNNIWSupport: boolean;inline;
    function AVX5124FMAPSSupport: boolean;inline;
    function GFNISupport: boolean;inline;
    function VAESSupport: boolean;inline;
    function VCLMULSupport: boolean;inline;
    function AVX512BITALGSupport: boolean;inline;
    function RDSEEDSupport: boolean;inline;
    function ADXSupport: boolean;inline;
    function SHASupport: boolean;inline;
    function SHA512Support: boolean;inline;
    function SM3Support: boolean;inline;
    function SM4Support: boolean;inline;
    function FMASupport: boolean;inline;
    function CMPXCHG16BSupport: boolean;inline;
    function POPCNTSupport: boolean;inline;
    function LZCNTSupport: boolean;inline;
    function SSESupport: boolean;inline;
    function SSE2Support: boolean;inline;
    function SSE3Support: boolean;inline;
    function SSSE3Support: boolean;inline;
    function SSE41Support: boolean;inline;
    function SSE42Support: boolean;inline;
    function MOVBESupport: boolean;inline;
    function F16CSupport: boolean;inline;
    function RDRANDSupport: boolean;inline;
    function RTMSupport: boolean;inline;
    function BMI1Support: boolean;inline;
    function BMI2Support: boolean;inline;


    var
      is_sse3_cpu : boolean = false;

    function InterlockedCompareExchange128(var Target: Int128Rec; NewValue: Int128Rec; Comperand: Int128Rec): Int128Rec;

  implementation

    var
      data: record
        cpuid1, cpuid7_0, cpuid7_1 : TCpuidResult;
        cpuid24_0_ebx : dword;
        AVXSupport,
        LZCNTSupport: boolean;
      end;

{$ASMMODE ATT}

    procedure CPUID(in_eax: uint32; in_ecx: uint32; out res: TCpuidResult); assembler; nostackframe;
      // ^ I don't know how 16-byte result is handled in SysV, if it is returned in RDX:RAX as GCC does things become complex,
      // that's why this internal version with "out res" exists...
      // Win64: ecx = in_eax, edx = in_ecx, r8 = res.
      // SysV:  edi = in_eax, esi = in_ecx, rdx = res.
      asm
        push  %rbx
{$ifndef win64}
        mov   %rdx, %r8 // r8 = res
{$endif}
        mov   in_eax, %eax
        mov   in_ecx, %ecx
        cpuid
        mov   %eax, TCpuidResult.eax(%r8)
        mov   %ebx, TCpuidResult.ebx(%r8)
        mov   %ecx, TCpuidResult.ecx(%r8)
        mov   %edx, TCpuidResult.edx(%r8)
        pop   %rbx
      end;


    function CPUID(in_eax: uint32; in_ecx: uint32 = 0): TCpuidResult;
      begin
        CPUID(in_eax, in_ecx, result);
      end;


    function CPUBrandString: shortstring;
      begin
        if CPUID($80000000).eax<$80000004 then
          exit('');
        TCpuidResult(pointer(@result[1])^):=CPUID($80000002);
        TCpuidResult(pointer(@result[17])^):=CPUID($80000003);
        TCpuidResult(pointer(@result[33])^):=CPUID($80000004);
        result[49]:=#0;
        result[0]:=chr(length(PAnsiChar(@result[1])));
      end;


    function InterlockedCompareExchange128(var Target: Int128Rec; NewValue: Int128Rec; Comperand: Int128Rec): Int128Rec; assembler;
     {
        win64:
          rcx ... pointer to result
          rdx ... target
          r8  ... NewValue
          r9  ... Comperand
      }
    {$ifdef win64}
      asm
        pushq %rbx

        { store result pointer for later use }
        pushq %rcx

        { load new value }
        movq (%r8),%rbx
        movq 8(%r8),%rcx

        { save target pointer for later use }
        movq %rdx,%r8

        { load comperand }
        movq (%r9),%rax
        movq 8(%r9),%rdx

        {$ifdef oldbinutils}
           .byte 0xF0,0x49,0x0F,0xC7,0x08
        {$else}
        lock cmpxchg16b (%r8)
        {$endif}
        { restore result pointer }
        popq %rcx

        { store result }
        movq %rax,(%rcx)
        movq %rdx,8(%rcx)

        popq %rbx
      end;
    {$else win64}
    {
      linux:
        rdi       ... target
        [rsi:rdx] ... NewValue
        [rcx:r8]  ... Comperand
        [rdx:rax] ... result
    }
      asm
        pushq %rbx

        movq %rsi,%rbx          // new value low
        movq %rcx,%rax          // comperand low
        movq %rdx,%rcx          // new value high
        movq %r8,%rdx           // comperand high
        {$ifdef oldbinutils}
        .byte 0xF0,0x48,0x0F,0xC7,0x0F
        {$else}
        lock cmpxchg16b (%rdi)
        {$endif}

        popq %rbx
      end;
    {$endif win64}


    function XGETBV(i : dword) : int64;assembler;
      asm
    {$ifndef win64}
        movq %rdi,%rcx
    {$endif win64}
        // older FPCs don't know the xgetbv opcode
        .byte 0x0f,0x01,0xd0
        shlq $32,%rdx
        orq %rdx,%rax
      end;


    procedure SetupSupport;
      var
        maxcpuidvalue : longint;
      begin
        maxcpuidvalue:=CPUID(0).eax;
        CPUID(1, 0, data.cpuid1);
        { very early x86-64 CPUs might not support eax=7 }
        if maxcpuidvalue>=7 then
          begin
            CPUID(7, 0, data.cpuid7_0);
            CPUID(7, 1, data.cpuid7_1);
          end;

        is_sse3_cpu:=(data.cpuid1.ecx and (1 shl 0))<>0;

        data.AVXSupport:=
          { cpuid(1).ecx[27]: XGETBV support, cpuid(1).ecx[28]: AVX support }
          (data.cpuid1.ecx shr 27 and %11=%11) and
          { xmm and ymm state enabled? }
          ((XGETBV(0) and %110)=%110);

        if (data.cpuid7_1.edx and (1 shl 19))<>0 then { CPUID.(EAX=24H) leaf is supported }
          data.cpuid24_0_ebx:=CPUID($24, 0).ebx;

        data.LZCNTSupport:=(CPUID($80000001).ecx and (1 shl 5))<>0;
      end;


    function InterlockedCompareExchange128Support : boolean;inline;
      begin
        result:=(data.cpuid1.ecx and (1 shl 13))<>0;
      end;


    function TSCSupport: boolean;
      begin
        result:=(data.cpuid1.edx and (1 shl 4))<>0;
      end;


    function MMXSupport: boolean;
      begin
        result:=(data.cpuid1.edx and (1 shl 23))<>0;
      end;


    function CMOVSupport : boolean;
      begin
        result:=true;
      end;


    function AESSupport : boolean;inline;
      begin
        result:=(data.cpuid1.ecx and (1 shl 25))<>0;
      end;


    function AVXSupport: boolean;inline;
      begin
        result:=data.AVXSupport;
      end;


    function AVX2Support: boolean;inline;
      begin
        result:=data.AVXSupport and ((data.cpuid7_0.ebx and (1 shl 5))<>0);
      end;


    function AVX101Support: boolean;inline; { AVX10.1 }
      begin
        result:=(data.cpuid24_0_ebx and $ff)>=1;
      end;


    function AVX102Support: boolean;inline; { AVX10.2 }
      begin
        result:=(data.cpuid24_0_ebx and $ff)>=2;
      end;


    function AVX10_256Support: boolean;inline; { AVX10/256 }
      begin
        result:=(data.cpuid24_0_ebx and (1 shl 17))<>0;
      end;


    function AVX10_512Support: boolean;inline; { AVX10/512 }
      begin
        result:=(data.cpuid24_0_ebx and (1 shl 18))<>0;
      end;


    function APXSupport: boolean;inline;  { APX_F Advanced Performance Extension Foundation }
      begin
        result:=(data.cpuid7_1.edx and (1 shl 21))<>0;
      end;


    function AVX512FSupport: boolean;inline;
      begin
        result:=(data.cpuid7_0.ebx and (1 shl 16))<>0;
      end;


    function AVX512DQSupport: boolean;inline;
      begin
        result:=(data.cpuid7_0.ebx and (1 shl 17))<>0;
      end;


    function AVX512IFMASupport: boolean;inline;
      begin
        result:=(data.cpuid7_0.ebx and (1 shl 21))<>0;
      end;


    function AVX512PFSupport: boolean;inline;
      begin
        result:=(data.cpuid7_0.ebx and (1 shl 26))<>0;
      end;


    function AVX512ERSupport: boolean;inline;
      begin
        result:=(data.cpuid7_0.ebx and (1 shl 27))<>0;
      end;


    function AVX512CDSupport: boolean;inline;
      begin
        result:=(data.cpuid7_0.ebx and (1 shl 28))<>0;
      end;


    function AVX512BWSupport: boolean;inline;
      begin
        result:=(data.cpuid7_0.ebx and (1 shl 30))<>0;
      end;


    function AVX512VLSupport: boolean;inline;
      begin
        result:=(data.cpuid7_0.ebx and (1 shl 31))<>0;
      end;


    function AVX512VBMISupport: boolean;inline;
      begin
        result:=(data.cpuid7_0.ecx and (1 shl 1))<>0;
      end;


    function AVX512VBMI2Support: boolean;inline;
      begin
        result:=(data.cpuid7_0.ecx and (1 shl 6))<>0;
      end;


    function GFNISupport: boolean;inline;
      begin
        result:=(data.cpuid7_0.ecx and (1 shl 8))<>0;
      end;


    function VAESSupport: boolean;inline;
      begin
        result:=(data.cpuid7_0.ecx and (1 shl 9))<>0;
      end;


    function VCLMULSupport: boolean;inline;
      begin
        result:=(data.cpuid7_0.ecx and (1 shl 10))<>0;
      end;


    function AVX512VNNISupport: boolean;inline;
      begin
        result:=(data.cpuid7_0.ecx and (1 shl 11))<>0;
      end;


    function AVX512BITALGSupport: boolean;inline;
      begin
        result:=(data.cpuid7_0.ecx and (1 shl 12))<>0;
      end;


    function AVX512VPOPCNTDQSupport: boolean;inline;
      begin
        result:=(data.cpuid7_0.ecx and (1 shl 14))<>0;
      end;


    function AVX512BF16Support: boolean;inline;
      begin
        result:=(data.cpuid7_1.eax and (1 shl 5))<>0;
      end;


    function AVX512FP16Support: boolean;inline;
      begin
        result:=(data.cpuid7_0.edx and (1 shl 23))<>0;
      end;


    function AVX512VP2INTERSECTSupport: boolean;inline;
      begin
        result:=(data.cpuid7_0.edx and (1 shl 8))<>0;
      end;


    function AVX5124VNNIWSupport: boolean;inline;
      begin
        result:=(data.cpuid7_0.edx and (1 shl 2))<>0;
      end;


    function AVX5124FMAPSSupport: boolean;inline;
      begin
        result:=(data.cpuid7_0.edx and (1 shl 3))<>0;
      end;


    function RDSEEDSupport: boolean;inline;
      begin
        result:=(data.cpuid7_0.ebx and (1 shl 18))<>0;
      end;


    function ADXSupport: boolean;inline;
      begin
        result:=(data.cpuid7_0.ebx and (1 shl 19))<>0;
      end;


    function SHASupport: boolean;inline;
      begin
        result:=(data.cpuid7_0.ebx and (1 shl 29))<>0;
      end;


    function SHA512Support: boolean;inline;
      begin
        result:=(data.cpuid7_1.eax and 1)<>0;
      end;


    function SM3Support: boolean;inline;
      begin
        result:=(data.cpuid7_1.eax and (1 shl 1))<>0;
      end;


    function SM4Support: boolean;inline;
      begin
        result:=(data.cpuid7_1.eax and (1 shl 2))<>0;
      end;


    function FMASupport: boolean;inline;
      begin
        result:=data.AVXSupport and ((data.cpuid1.ecx and (1 shl 12))<>0);
      end;


    function CMPXCHG16BSupport: boolean;inline;
      begin
        result:=(data.cpuid1.ecx and (1 shl 13))<>0;
      end;


    function POPCNTSupport: boolean;inline;
      begin
        result:=(data.cpuid1.ecx and (1 shl 23))<>0;
      end;


    function LZCNTSupport: boolean;inline;
      begin
        result:=data.LZCNTSupport;
      end;


    function SSESupport: boolean;inline;
      begin
        result:=true;
      end;


    function SSE2Support: boolean;inline;
      begin
        result:=true;
      end;


    function SSE3Support: boolean;inline;
      begin
        result:=(data.cpuid1.ecx and (1 shl 0))<>0;
      end;


    function SSSE3Support: boolean;inline;
      begin
        result:=(data.cpuid1.ecx and (1 shl 9))<>0;
      end;


    function SSE41Support: boolean;inline;
      begin
        result:=(data.cpuid1.ecx and (1 shl 19))<>0;
      end;


    function SSE42Support: boolean;inline;
      begin
        result:=(data.cpuid1.ecx and (1 shl 20))<>0;
      end;


    function MOVBESupport: boolean;inline;
      begin
        result:=(data.cpuid1.ecx and (1 shl 22))<>0;
      end;


    function F16CSupport: boolean;inline;
      begin
        result:=(data.cpuid1.ecx and (1 shl 29))<>0;
      end;


    function RDRANDSupport: boolean;inline;
      begin
        result:=(data.cpuid1.ecx and (1 shl 30))<>0;
      end;


    function RTMSupport: boolean;inline;
      begin
        result:=((data.cpuid7_0.ebx and (1 shl 11))<>0) and (data.cpuid7_0.edx and (1 shl 11)=0 {RTM_ALWAYS_ABORT});
      end;


    function BMI1Support: boolean;inline;
      begin
        result:=(data.cpuid7_0.ebx and (1 shl 3))<>0;
      end;


    function BMI2Support: boolean;inline;
      begin
        result:=(data.cpuid7_0.ebx and (1 shl 8))<>0;
      end;


begin
  SetupSupport;
end.
