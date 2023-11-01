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
{$goto on}
{$IFNDEF FPC_DOTTEDUNITS}
unit cpu;
{$ENDIF FPC_DOTTEDUNITS}

  interface

{$IFDEF FPC_DOTTEDUNITS}
    uses
      System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
    uses
      sysutils;
{$ENDIF FPC_DOTTEDUNITS}

    { returns true, if the processor supports the cpuid instruction }
    function cpuid_support : boolean;

type
    TCpuidResult = record
      eax, ebx, ecx, edx: uint32;
    end;

    function CPUID(in_eax: uint32; in_ecx: uint32 = 0): TCpuidResult; inline;
    function CPUBrandString: shortstring;

    { returns true, if floating point is done by an emulator }
    function floating_point_emulation : boolean;

    { returns the contents of the cr0 register }
    function cr0 : longint;

    function CMOVSupport : boolean;inline;
    function InterlockedCompareExchange128Support : boolean;
    function AESSupport : boolean;inline;
    function AVXSupport: boolean;inline;
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

{$ASMMODE INTEL}
    var
      _CMOVSupport,
      _AESSupport,
      _AVXSupport,
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

{$ASMMODE ATT}

    function InterlockedCompareExchange128(var Target: Int128Rec; NewValue: Int128Rec; Comperand: Int128Rec): Int128Rec;
      begin
{$if FPC_FULLVERSION >= 30101}
{$ifndef FPC_PIC}
        if _RTMSupport then
          begin
            asm
{$ifdef USE_REAL_INSTRUCTIONS}
         .Lretry:
              xbegin .Lretry
{$else}
{   3d:	c7 f8 fa ff ff ff    	xbegin    }
         .byte 0xc7,0xf8, 0xfa, 0xff, 0xff, 0xff
{$endif}
            end;
            Result:=Target;
            if (Result.Lo=Comperand.Lo) and (Result.Hi=Comperand.Hi) then
              Target:=NewValue;
            asm
{$ifdef USE_REAL_INSTRUCTIONS}
              xend
{$else}
  { 8a:	0f 01 d5             	xend    }
         .byte 0x0f, 0x01, 0xd5
{$endif}
            end;
          end
        else
{$endif FPC_PIC}
{$endif FPC_FULLVERSION >= 30101}
          RunError(217);
      end;

{$ASMMODE INTEL}

    function cpuid_support : boolean;assembler;
      {
        Check if the ID-flag can be changed, if changed then CpuID is supported.
        Tested under go32v1 and Linux on c6x86 with CpuID enabled and disabled (PFV)
      }
      asm
         push    ebx
         pushfd
         pushfd
         pop     eax
         mov     ebx,eax
         xor     eax,200000h
         push    eax
         popfd
         pushfd
         pop     eax
         popfd
         and     eax,200000h
         and     ebx,200000h
         cmp     eax,ebx
         setnz   al
         pop     ebx
      end;


    procedure CPUID(in_eax: uint32; in_ecx: uint32; out res: TCpuidResult); assembler; nostackframe;
      // eax = in_eax, edx = in_ecx, ecx = res
      asm
        push ebx
        push esi
        mov  esi, ecx // esi = res
        mov  ecx, edx // ecx = in_ecx
        cpuid
        mov  TCpuidResult.eax[esi], eax
        mov  TCpuidResult.ebx[esi], ebx
        mov  TCpuidResult.ecx[esi], ecx
        mov  TCpuidResult.edx[esi], edx
        pop  esi
        pop  ebx
      end;


    function CPUID(in_eax: uint32; in_ecx: uint32 = 0): TCpuidResult;
      begin
        CPUID(in_eax, in_ecx, result);
      end;


    function CPUBrandString: shortstring;
      begin
        if not cpuid_support or (CPUID($80000000).eax<$80000004) then
          exit('');
        TCpuidResult(pointer(@result[1])^):=CPUID($80000002);
        TCpuidResult(pointer(@result[17])^):=CPUID($80000003);
        TCpuidResult(pointer(@result[33])^):=CPUID($80000004);
        result[49]:=#0;
        result[0]:=chr(length(PAnsiChar(@result[1])));
      end;


    function cr0 : longint;assembler;
      asm
{$ifdef USE_REAL_INSTRUCTIONS}
         mov eax,cr0
{$else}
         DB 0Fh,20h,0C0h
{$endif}
         { mov eax,cr0
           special registers are not allowed in the assembler
                parsers }
      end;


    function floating_point_emulation : boolean;
      begin
         {!!!! I don't know currently the position of the EM flag }
         { $4 after Ralf Brown's list }
         floating_point_emulation:=(cr0 and $4)<>0;
      end;


{$ASMMODE ATT}
    function XGETBV(i : dword) : int64;assembler;
      asm
        movl %eax,%ecx
{$ifdef USE_REAL_INSTRUCTIONS}
        xgetbv
{$else}
        // older FPCs don't know the xgetbv opcode
        .byte 0x0f,0x01,0xd0
{$endif}
      end;


    procedure SetupSupport;
      var
        maxcpuidvalue : longint;
        cpuid1,cpuid7 : TCpuidResult;
      begin
        is_sse3_cpu:=false;
         if cpuid_support then
           begin
              maxcpuidvalue:=CPUID(0).eax;
              cpuid1:=CPUID(1);
              _CMOVSupport:=(cpuid1.edx and $8000)<>0;
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

              _LZCNTSupport:=(CPUID($80000001).ecx and $20)<>0;


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
                  _AVX512BWSupport:=(cpuid7.ebx and $40000000)<>0;
                  _AVX512VBMISupport:=(cpuid7.ecx and $00000002)<>0;
                  _AVX512VBMI2Support:=(cpuid7.ecx and $00000040)<>0;
                   _VAESSupport:=(cpuid7.ecx and $00000200)<>0;
                  _VCLMULSupport:=(cpuid7.ecx and $00000400)<>0;
                  _AVX512VNNISupport:=(cpuid7.ecx and $00000800)<>0;
                  _AVX512BITALGSupport:=(cpuid7.ecx and $00001000)<>0;
                  _SHASupport:=(cpuid7.ebx and $20000000)<>0;
                  _AVX512VLSupport:=(cpuid7.ebx and $80000000)<>0;
                  _BMI1Support:=(cpuid7.ebx and $8)<>0;
                  _BMI2Support:=(cpuid7.ebx and $100)<>0;
                  _RTMSupport:=((cpuid7.ebx and $800)<>0) and (cpuid7.edx and (1 shl 11)=0 {RTM_ALWAYS_ABORT});
                end;
           end;
      end;


    function InterlockedCompareExchange128Support : boolean;
      begin
        { 32 Bit CPUs have no 128 Bit interlocked exchange support,
          but it can simulated using RTM }
        result:=_RTMSupport;
      end;


    function CMOVSupport : boolean;
      begin
        result:=_CMOVSupport;
      end;


    function AESSupport : boolean;
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
