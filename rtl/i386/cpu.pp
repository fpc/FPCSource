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

    function TSCSupport: boolean;inline;
    function MMXSupport: boolean;inline;
    function CMOVSupport: boolean;inline;
    function InterlockedCompareExchange128Support: boolean;
    function AESSupport: boolean;inline;
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
    function SHA512Support: boolean;inline;
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
      data: record
        cpuid1, cpuid7_0, cpuid7_1: TCpuidResult;
        AVXSupport,
        LZCNTSupport: boolean;
      end;

{$ASMMODE ATT}

    function InterlockedCompareExchange128(var Target: Int128Rec; NewValue: Int128Rec; Comperand: Int128Rec): Int128Rec;
      begin
{$if FPC_FULLVERSION >= 30101}
{$ifndef FPC_PIC}
        if RTMSupport then
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
      begin
        if cpuid_support then
          begin
            maxcpuidvalue:=CPUID(0).eax;
            CPUID(1, 0, data.cpuid1);
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

            data.LZCNTSupport:=(CPUID($80000001).ecx and (1 shl 5))<>0;
         end;
      end;


    function InterlockedCompareExchange128Support : boolean;
      begin
        { 32 Bit CPUs have no 128 Bit interlocked exchange support,
          but it can simulated using RTM }
        result:=RTMSupport;
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
        result:=(data.cpuid1.edx and (1 shl 15))<>0;
      end;


    function AESSupport : boolean;
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


    function FMASupport: boolean;inline;
      begin
        result:=data.AVXSupport and ((data.cpuid1.ecx and (1 shl 12))<>0);
      end;


    function POPCNTSupport: boolean;inline;
      begin
        result:=(data.cpuid1.ecx and (1 shl 23))<>0;
      end;


    function LZCNTSupport: boolean;inline;
      begin
        result:=data.LZCNTSupport;
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
