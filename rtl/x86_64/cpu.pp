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

{$IFDEF FPC_DOTTEDUNITS}
    uses
      System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
    uses
      sysutils;
{$ENDIF FPC_DOTTEDUNITS}

type
    TCpuidResult = record
      eax, ebx, ecx, edx: uint32;
    end;

    function CPUID(in_eax: uint32; in_ecx: uint32 = 0): TCpuidResult; inline;
    function CPUBrandString: shortstring;

    function InterlockedCompareExchange128Support : boolean;inline;
    function CMOVSupport : boolean;inline;
    function AESSupport : boolean;inline;
    function AVXSupport : boolean;inline;
    function AVX2Support: boolean;inline;
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
    function VAESSupport: boolean;inline;
    function VCLMULSupport: boolean;inline;
    function AVX512BITALGSupport: boolean;inline;
    function RDSEEDSupport: boolean;inline;
    function ADXSupport: boolean;inline;
    function SHASupport: boolean;inline;    
    function FMASupport: boolean;inline;
    function CMPXCHG16BSupport: boolean;inline;
    function POPCNTSupport: boolean;inline;
    function LZCNTSupport: boolean;inline;
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

{$asmmode att}

    var
      _AESSupport,
      _AVXSupport,
      _InterlockedCompareExchange128Support,
      _AVX2Support,
      _AVX512FSupport,
      _AVX512DQSupport,
      _AVX512IFMASupport,
      _AVX512PFSupport,
      _AVX512ERSupport,
      _AVX512CDSupport,
      _AVX512BWSupport,
      _AVX512VLSupport,
      _AVX512VBMISupport,
      _AVX512VBMI2Support,
      _CMPXCHG16BSupport,
      _VAESSupport,
      _VCLMULSupport,
      _AVX512VNNISupport,
      _AVX512BITALGSupport,
      _RDSEEDSupport,
      _ADXSupport,
      _SHASupport,
      _FMASupport,
      _POPCNTSupport,
      _LZCNTSupport,
      _SSE3Support,
      _SSSE3Support,
      _SSE41Support,
      _SSE42Support,
      _MOVBESupport,
      _F16CSupport,
      _RDRANDSupport,
      _RTMSupport,
      _BMI1Support,
      _BMI2Support: boolean;


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
        andl $0xffffffff,%eax
        shlq $32,%rdx
        orq %rdx,%rax
      end;


    procedure SetupSupport;
      var
        maxcpuidvalue : longint;
        cpuid1,cpuid7 : TCpuidResult;
      begin
        maxcpuidvalue:=CPUID(0).eax;
        cpuid1:=CPUID(1);
        _InterlockedCompareExchange128Support:=(cpuid1.ecx and $2000)<>0;
        _AESSupport:=(cpuid1.ecx and $2000000)<>0;
        _POPCNTSupport:=(cpuid1.ecx and $800000)<>0;
        _SSE3Support:=(cpuid1.ecx and $1)<>0;
        _SSSE3Support:=(cpuid1.ecx and $200)<>0;
        _SSE41Support:=(cpuid1.ecx and $80000)<>0;
        _SSE42Support:=(cpuid1.ecx and $100000)<>0;
        _MOVBESupport:=(cpuid1.ecx and $400000)<>0;
        _F16CSupport:=(cpuid1.ecx and $20000000)<>0;
        _RDRANDSupport:=(cpuid1.ecx and $40000000)<>0;

        _AVXSupport:=
          { XGETBV suspport? }
          ((cpuid1.ecx and $08000000)<>0) and
          { xmm and ymm state enabled? }
          ((XGETBV(0) and %110)=%110) and
          { avx supported? }
          ((cpuid1.ecx and $10000000)<>0);

        is_sse3_cpu:=(cpuid1.ecx and $1)<>0;

        _FMASupport:=_AVXSupport and ((cpuid1.ecx and $1000)<>0);
        _CMPXCHG16BSupport:=(cpuid1.ecx and $2000)<>0;

        _LZCNTSupport:=(CPUID($80000001).ecx and $20)<>0;

        { very early x86-64 CPUs might not support eax=7 }
        if maxcpuidvalue>=7 then
          begin
            cpuid7:=CPUID(7);
            _AVX2Support:=_AVXSupport and ((cpuid7.ebx and $20)<>0);
            _AVX512FSupport:=(cpuid7.ebx and $10000)<>0;
            _AVX512DQSupport:=(cpuid7.ebx and $20000)<>0;
            _RDSEEDSupport:=(cpuid7.ebx and $40000)<>0;
            _ADXSupport:=(cpuid7.ebx and $80000)<>0;
            _AVX512IFMASupport:=(cpuid7.ebx and $200000)<>0;
            _AVX512PFSupport:=(cpuid7.ebx and $4000000)<>0;
            _AVX512ERSupport:=(cpuid7.ebx and $8000000)<>0;
            _AVX512CDSupport:=(cpuid7.ebx and $10000000)<>0;
            _SHASupport:=(cpuid7.ebx and $20000000)<>0;
            _AVX512BWSupport:=(cpuid7.ebx and $40000000)<>0;
            _AVX512VLSupport:=(cpuid7.ebx and $80000000)<>0;
            _AVX512VBMISupport:=(cpuid7.ecx and $00000002)<>0;
            _AVX512VBMI2Support:=(cpuid7.ecx and $00000040)<>0;
            _VAESSupport:=(cpuid7.ecx and $00000200)<>0;
            _VCLMULSupport:=(cpuid7.ecx and $00000400)<>0;
            _AVX512VNNISupport:=(cpuid7.ecx and $00000800)<>0;
            _AVX512BITALGSupport:=(cpuid7.ecx and $00001000)<>0;
            _BMI1Support:=(cpuid7.ebx and $8)<>0;
            _BMI2Support:=(cpuid7.ebx and $100)<>0;
            _RTMSupport:=((cpuid7.ebx and $800)<>0) and (cpuid7.edx and (1 shl 11)=0 {RTM_ALWAYS_ABORT});
          end;
      end;


    function InterlockedCompareExchange128Support : boolean;inline;
      begin
        result:=_InterlockedCompareExchange128Support;
      end;


    function CMOVSupport : boolean;
      begin
        result:=true;
      end;


    function AESSupport : boolean;inline;
      begin
        result:=_AESSupport;
      end;


    function AVXSupport: boolean;inline;
      begin
        result:=_AVXSupport;
      end;


    function AVX2Support: boolean;inline;
      begin
        result:=_AVX2Support;
      end;


    function AVX512FSupport: boolean;inline;
      begin
        result:=_AVX512FSupport;
      end;


    function AVX512DQSupport: boolean;inline;
      begin
        result:=_AVX512DQSupport;
      end;


    function AVX512IFMASupport: boolean;inline;    
      begin
        result:=_AVX512IFMASupport;
      end;


    function AVX512PFSupport: boolean;inline;    
      begin
        result:=_AVX512PFSupport;
      end;


    function AVX512ERSupport: boolean;inline;    
      begin
        result:=_AVX512ERSupport;
      end;


    function AVX512CDSupport: boolean;inline;    
      begin
        result:=_AVX512CDSupport;
      end;


    function AVX512BWSupport: boolean;inline;    
      begin
        result:=_AVX512BWSupport;
      end;


    function AVX512VLSupport: boolean;inline;    
      begin
        result:=_AVX512VLSupport;
      end;


    function AVX512VBMISupport: boolean;inline;
      begin
        result:=_AVX512VBMISupport;
      end;


    function AVX512VBMI2Support: boolean;inline;
      begin
        result:=_AVX512VBMI2Support;
      end;


    function VAESSupport: boolean;inline;
      begin
        result:=_VAESSupport;
      end;


    function VCLMULSupport: boolean;inline;
      begin
        result:=_VCLMULSupport;
      end;


    function AVX512VNNISupport: boolean;inline;    
      begin
        result:=_AVX512VNNISupport;
      end;


    function AVX512BITALGSupport: boolean;inline;    
      begin
        result:=_AVX512BITALGSupport;
      end;


    function RDSEEDSupport: boolean;inline;
      begin
        result:=_RDSEEDSupport;
      end;


    function ADXSupport: boolean;inline;
      begin
        result:=_ADXSupport;
      end;


    function SHASupport: boolean;inline;    
      begin
        result:=_SHASupport;
      end;


    function FMASupport: boolean;inline;
      begin
        result:=_FMASupport;
      end;


    function CMPXCHG16BSupport: boolean;inline;
      begin
        result:=_CMPXCHG16BSupport;
      end;


    function POPCNTSupport: boolean;inline;
      begin
        result:=_POPCNTSupport;
      end;


    function LZCNTSupport: boolean;inline;
      begin
        result:=_LZCNTSupport;
      end;


    function SSE3Support: boolean;inline;
      begin
        result:=_SSE3Support;
      end;


    function SSSE3Support: boolean;inline;
      begin
        result:=_SSSE3Support;
      end;


    function SSE41Support: boolean;inline;
      begin
        result:=_SSE41Support;
      end;


    function SSE42Support: boolean;inline;
      begin
        result:=_SSE42Support;
      end;


    function MOVBESupport: boolean;inline;
      begin
        result:=_MOVBESupport;
      end;


    function F16CSupport: boolean;inline;
      begin
        result:=_F16CSupport;
      end;


    function RDRANDSupport: boolean;inline;
      begin
        result:=_RDRANDSupport;
      end;


    function RTMSupport: boolean;inline;
      begin
        result:=_RTMSupport;
      end;


    function BMI1Support: boolean;inline;
      begin
        result:=_BMI1Support;
      end;


    function BMI2Support: boolean;inline;
      begin
        result:=_BMI2Support;
      end;


begin
  SetupSupport;
end.
