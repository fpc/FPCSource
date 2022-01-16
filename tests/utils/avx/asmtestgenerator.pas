{

  Copyright (C) <avx-testfile-generator> <Torsten Grundke>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
  MA 02110-1301, USA.
}

{$mode objfpc}

unit asmtestgenerator;

interface

uses BaseList, Classes;

type
  TOpType = (otUnknown, otXMMReg, otXMMRM, otXMMRM16, otXMMRM8, otYMMReg, otYMMRM, otZMMReg, otZMMRM, otEAX, otRAX, otMem32,
             otMem8, otMem16, otMem64, otMem128, otMem256, otMem512, otREG64, otREG32, otREG16, otREG8, otRM32, otRM64, otIMM8,
             otXMEM32, otXMEM64, otYMEM32, otYMEM64, otZMEM32, otZMEM64,
             otB32, otB64, otKREG);

  TOpMemType = Set of TOpType;

  TAsmCompareMode = (cmKORTESTNC, cmXORTestNZ);

  TOpMode = (omUnknown,
             omMX, omMY, omMZ,
             omXB32, omXB64, omYB32, omYB64, omZB32, omZB64,
             omXM, omYM, omZM,


             omKXM, omKYM, omKZM,
             omKXB32, omKXB64, omKYB32, omKYB64, omKZB32, omKZB64,
             omKXB32I, omKXB64I, omKXMI, omKYB32I, omKYB64I, omKYMI, omKZB32I, omKZB64I, omKZMI,

             omKMI, omKB32I, omKB64I,
             omMXI, omMYI, omMZI,
             omXXM, omXXB32, omXXB64, omXMI, omXB32I, omXB64I,
             omXXMI, omXXB32I, omXXB64I,

             omYYM, omYYB32, omYYB64, omYMI, omYB32I, omYB64I,
             omYYMI, omYYB32I, omYYB64I,
             omZZM, omZZB32, omZZB64, omZMI, omZB32I, omZB64I,
             omZZMI, omZZB32I, omZZB64I);

  TOperandListItem = class(TObject)
  private
    FOpActive: boolean;
    FOpNumber: integer;
    FOpTyp: TOpType;
    FValues: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    property OpNumber: integer read FOpNumber write FOpNumber;
    property OpTyp: TOpType read FOpTyp write FOpTyp;
    property OpActive: boolean read FOpActive write FOpActive;

    property Values: TStringList read FValues;
  end;

  TOperandList = class(TBaseList)
  private
    function GetItems(aIndex: integer): TOperandListItem;

  public
    function Add(aItem: TOperandListItem): integer;

    property Items[aIndex: integer]: TOperandListItem read GetItems;
  end;


  { TAsmTestGenerator }

  TAsmTestGenerator = class(TObject)
  private
    FReg8          : TStringList;
    FReg16         : TStringList;
    FReg32Base     : TStringList;
    FReg32Index    : TStringList;
    FReg64Base     : TStringList;
    FReg64Index    : TStringList;
    FReg6432Base   : TStringList;
    FReg6432Index  : TStringList;
    FReg32XMMIndex : TStringList;
    FReg32YMMIndex : TStringList;
    FReg32ZMMIndex : TStringList;
    FReg64XMMIndex : TStringList;
    FReg64YMMIndex : TStringList;
    FReg64ZMMIndex : TStringList;
    FRegKREG       : TStringList;

    Fx64: boolean;
    FAVX512: boolean;
    FSAE: boolean;

    procedure MemRegBaseIndexCombi(const aPrefix, aSuffix: String; aSLBaseReg, aSLIndexReg, aRList: TStringList);
    procedure MemRegBaseIndexCombiCDISP8N(const aPrefix, aSuffix: String; aSLBaseReg, aSLIndexReg, aRList: TStringList);
    procedure VectorMemRegBaseIndexCombi(const aPrefix, aSuffix: String; aSLBaseReg, aSLIndexReg, aRList: TStringList);
    function ParseBaseIndexReg(const aOp: string; var aBaseReg, aIndexReg: string): boolean;

    function InternalCalcTestData(const aInst, aOp1, aOp2, aOp3, aOp4: String): TStringList;
    function InternalCalcTestDataMREF(const aInst, aOp1, aOp2, aOp3, aOp4: String): TStringList;
    function InternalCalcTestDataCDISP8(const aInst, aOp1, aOp2, aOp3, aOp4: String): TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    class procedure CalcTestData(aX64, aAVX512, aSAE: boolean; const aInst, aOp1, aOp2, aOp3, aOp4: String; aSL: TStringList);
    class procedure CalcTestDataMREF(aX64, aAVX512, aSAE: boolean; const aInst, aOp1, aOp2, aOp3, aOp4: String; aSL: TStringList);
    class procedure CalcTestDataCDisp8(aX64, aAVX512, aSAE: boolean; const aInst, aOp1, aOp2, aOp3, aOp4: String; aSL: TStringList);


    class procedure CalcTestInstFile;
    class procedure ListMemRefState;

    property x64: boolean read Fx64;
  end;

implementation

uses SysUtils, Dialogs, typinfo;

type
  TAsmOp={$i ../../../compiler/x86_64/x8664op.inc}
  TAttSuffix = (AttSufNONE,AttSufINT,AttSufFPU,AttSufFPUint,AttSufINTdual,AttSufMM,AttSufMMX,AttSufMMS);

  TMemRefSizeInfo = (msiUnknown, msiUnsupported, msiNoSize, msiNoMemRef,
                     msiMultiple, msiMultipleMinSize8, msiMultipleMinSize16, msiMultipleMinSize32,
                     msiMultipleMinSize64, msiMultipleMinSize128, msiMultipleminSize256, msiMultipleMinSize512,
                     msiMemRegSize, msiMemRegx16y32, msiMemRegx16y32z64, msiMemRegx32y64, msiMemRegx32y64z128, msiMemRegx64y128, msiMemRegx64y128z256,
                     msiMemRegx64y256, msiMemRegx64y256z512,
                     msiMem8, msiMem16, msiMem32, msiBMem32, msiMem64, msiBMem64, msiMem128, msiMem256, msiMem512,
                     msiXMem32, msiXMem64, msiYMem32, msiYMem64, msiZMem32, msiZMem64,
                     msiVMemMultiple, msiVMemRegSize,
                     msiMemRegConst128,msiMemRegConst256,msiMemRegConst512);

  TMemRefSizeInfoBCST = (msbUnknown, msbBCST32, msbBCST64, msbMultiple);
  TMemRefSizeInfoBCSTType = (btUnknown, bt1to2, bt1to4, bt1to8, bt1to16);

  TEVEXTupleState = (etsUnknown, etsIsTuple, etsNotTuple);
  TConstSizeInfo  = (csiUnknown, csiMultiple, csiNoSize, csiMem8, csiMem16, csiMem32, csiMem64);


  TInsTabMemRefSizeInfoRec = record
    MemRefSize               : TMemRefSizeInfo;
    MemRefSizeBCST           : TMemRefSizeInfoBCST;
    BCSTXMMMultiplicator     : byte;
    ExistsSSEAVX             : boolean;
    ConstSize                : TConstSizeInfo;
    BCSTTypes                : Set of TMemRefSizeInfoBCSTType;
    RegXMMSizeMask           : int64;
    RegYMMSizeMask           : int64;
    RegZMMSizeMask           : int64;
  end;


  TInsTabMemRefSizeInfoCache=array[TasmOp] of TInsTabMemRefSizeInfoRec;
  PInsTabMemRefSizeInfoCache=^TInsTabMemRefSizeInfoCache;

  TInsTabCache=array[TasmOp] of longint;
  PInsTabCache=^TInsTabCache;


const
  instabentries = {$i ../../../compiler/x86_64/x8664nop.inc}
  gas_needsuffix:array[tasmop] of TAttSuffix={$i ../../../compiler/x86_64/x8664ats.inc}

  MemRefMultiples: set of TMemRefSizeInfo = [msiMultiple, msiMultipleMinSize8,
                                             msiMultipleMinSize16, msiMultipleMinSize32,
                                             msiMultipleMinSize64, msiMultipleMinSize128,
                                             msiMultipleMinSize256, msiMultipleMinSize512,
                                             msiVMemMultiple];

  MemRefSizeInfoVMems: Set of TMemRefSizeInfo = [msiXMem32, msiXMem64, msiYMem32, msiYMem64,
                                                 msiZMem32, msiZMem64,
                                                 msiVMemMultiple, msiVMemRegSize];

  MEMTYPES: TOpMemType = [otXMMRM, otXMMRM16, otXMMRM8, otYMMRM, otZMMRM,
                          otMem8, otMem16, otMem32, otMem64, otMem128, otMem256, otMem512,
                          otRM32, otRM64];
  BMEMTYPES: TOpMemType = [otB32, otB64];

var
  InsTabCache : PInsTabCache;
  InsTabMemRefSizeInfoCache: PInsTabMemRefSizeInfoCache;

type


    op2strtable=array[tasmop] of string[16];

    {Instruction flags }
    tinsflag = (
      { please keep these in order and in sync with IF_SMASK }
      IF_SM,                  { size match first two operands  }
      IF_SM2,
      IF_SB,                  { unsized operands can't be non-byte  }
      IF_SW,                  { unsized operands can't be non-word  }
      IF_SD,                  { unsized operands can't be nondword  }

      { unsized argument spec }
      { please keep these in order and in sync with IF_ARMASK }
      IF_AR0,                 { SB, SW, SD applies to argument 0  }
      IF_AR1,                 { SB, SW, SD applies to argument 1  }
      IF_AR2,                 { SB, SW, SD applies to argument 2  }

      IF_PRIV,                { it's a privileged instruction  }
      IF_SMM,                 { it's only valid in SMM  }
      IF_PROT,                { it's protected mode only  }
      IF_NOX86_64,            { removed instruction in x86_64  }
      IF_UNDOC,               { it's an undocumented instruction  }
      IF_FPU,                 { it's an FPU instruction  }
      IF_MMX,                 { it's an MMX instruction  }
      { it's a 3DNow! instruction  }
      IF_3DNOW,
      { it's a SSE (KNI, MMX2) instruction  }
      IF_SSE,
      { SSE2 instructions  }
      IF_SSE2,
      { SSE3 instructions  }
      IF_SSE3,
      { SSE64 instructions  }
      IF_SSE64,
      { SVM instructions  }
      IF_SVM,
      { SSE4 instructions  }
      IF_SSE4,
      IF_SSSE3,
      IF_SSE41,
      IF_SSE42,
      IF_MOVBE,
      IF_CLMUL,
      IF_AVX,
      IF_AVX2,
      IF_AVX512,
      IF_BMI1,
      IF_BMI2,
      { Intel ADX (Multi-Precision Add-Carry Instruction Extensions) }
      IF_ADX,
      IF_16BITONLY,
      IF_FMA,
      IF_FMA4,
      IF_TSX,
      IF_RAND,
      IF_XSAVE,
      IF_PREFETCHWT1,
      IF_SHA,

      { mask for processor level }
      { please keep these in order and in sync with IF_PLEVEL }
      IF_8086,                { 8086 instruction  }
      IF_186,                 { 186+ instruction  }
      IF_286,                 { 286+ instruction  }
      IF_386,                 { 386+ instruction  }
      IF_486,                 { 486+ instruction  }
      IF_PENT,                { Pentium instruction  }
      IF_P6,                  { P6 instruction  }
      IF_KATMAI,              { Katmai instructions  }
      IF_WILLAMETTE,          { Willamette instructions }
      IF_PRESCOTT,            { Prescott instructions }
      IF_X86_64,
      IF_SANDYBRIDGE,         { Sandybridge-specific instruction }
      IF_NEC,                 { NEC V20/V30 instruction }

      { the following are not strictly part of the processor level, because
        they are never used standalone, but always in combination with a
        separate processor level flag. Therefore, they use bits outside of
        IF_PLEVEL, otherwise they would mess up the processor level they're
        used in combination with.
        The following combinations are currently used:
        [IF_AMD, IF_P6],
        [IF_CYRIX, IF_486],
        [IF_CYRIX, IF_PENT],
        [IF_CYRIX, IF_P6] }
      IF_CYRIX,               { Cyrix, Centaur or VIA-specific instruction }
      IF_AMD,                 { AMD-specific instruction  }

      { added flags }
      IF_PRE,                 { it's a prefix instruction }
      IF_PASS2,               { if the instruction can change in a second pass }
      IF_IMM4,                { immediate operand is a nibble (must be in range [0..15]) }
      IF_IMM3,                 { immediate operand is a triad (must be in range [0..7]) }

      IF_BCST2,
      IF_BCST4,
      IF_BCST8,
      IF_BCST16,
      IF_T2,                  { disp8 - tuple - 2 }
      IF_T4,                  { disp8 - tuple - 4 }
      IF_T8,                  { disp8 - tuple - 8 }
      IF_T1S,                 { disp8 - tuple - 1 scalar }
      IF_T1S8,
      IF_T1S16,
      IF_T1F32,
      IF_T1F64,
      IF_TMDDUP,
      IF_TFV,                 { disp8 - tuple - full vector }
      IF_TFVM,                { disp8 - tuple - full vector memory }
      IF_TQVM,
      IF_TMEM128,
      IF_THV,
      IF_THVM,
      IF_TOVM,

      IF_SCL32,
      IF_SCL64


    );
    tinsflags=set of tinsflag;


      tinsentry=packed record
        opcode  : tasmop;
        ops     : byte;
        //optypes : array[0..max_operands-1] of longint;
        optypes : array[0..3] of int64; //TG
        code    : array[0..11] of char;
        flags   : tinsflags;
      end;
      pinsentry=^tinsentry;

const
      OT_NONE      = $00000000;

      { Bits 0..7: sizes }
      OT_BITS8     = $00000001;
      OT_BITS16    = $00000002;
      OT_BITS32    = $00000004;
      OT_BITS64    = $00000008;  { x86_64 and FPU }
      //OT_BITS128   = $10000000;  { 16 byte SSE }
      //OT_BITS256   = $20000000;  { 32 byte AVX }
      //OT_BITS512   = $40000000;  { 64 byte AVX512 }
      OT_BITS128   = $20000000;  { 16 byte SSE }
      OT_BITS256   = $40000000;  { 32 byte AVX }
      OT_BITS512   = $80000000;  { 64 byte AVX512 }

      OT_VECTORMASK = $1000000000;  { OPTIONAL VECTORMASK AVX512}
      OT_VECTORZERO = $2000000000;  { OPTIONAL ZERO-FLAG  AVX512}
      OT_VECTORBCST = $4000000000;  { BROADCAST-MEM-FLAG  AVX512}
      OT_VECTORSAE  = $8000000000;  { OPTIONAL SAE-FLAG  AVX512}
      OT_VECTORER   = $10000000000; { OPTIONAL ER-FLAG-FLAG  AVX512}


      OT_BITSB32    = OT_BITS32 or OT_VECTORBCST;
      OT_BITSB64    = OT_BITS64 or OT_VECTORBCST;


      OT_VECTOR_EXT_MASK = OT_VECTORMASK or OT_VECTORZERO or OT_VECTORBCST;

      OT_BITS80    = $00000010;  { FPU only  }
      OT_FAR       = $00000020;  { this means 16:16 or 16:32, like in CALL/JMP }
      OT_NEAR      = $00000040;
      OT_SHORT     = $00000080;

      { TODO: FAR/NEAR/SHORT are sizes too, they should be included into size mask,
        but this requires adjusting the opcode table }
      //OT_SIZE_MASK = $3000001F;  { all the size attributes  }
      OT_SIZE_MASK = $E000001F;  { all the size attributes  }
      OT_NON_SIZE  = int64(not int64(OT_SIZE_MASK));

      { Bits 8..11: modifiers }
      OT_SIGNED    = $00000100;  { the operand need to be signed -128-127 }
      OT_TO        = $00000200;  { reverse effect in FADD, FSUB &c  }
      OT_COLON     = $00000400;  { operand is followed by a colon  }
      OT_MODIFIER_MASK = $00000F00;

      { Bits 12..15: type of operand }
      OT_REGISTER  = $00001000;
      OT_IMMEDIATE = $00002000;
      OT_MEMORY    = $0000C000;  { always includes 'OT_REGMEM' bit as well }
      OT_REGMEM    = $00008000;  { for r/m, ie EA, operands  }
      OT_TYPE_MASK = OT_REGISTER or OT_IMMEDIATE or OT_MEMORY or OT_REGMEM;

      OT_REGNORM   = OT_REGISTER or OT_REGMEM;  { 'normal' reg, qualifies as EA  }

      { Bits 20..22, 24..26: register classes
        otf_* consts are not used alone, only to build other constants. }
      otf_reg_cdt  = $00100000;
      otf_reg_gpr  = $00200000;
      otf_reg_sreg = $00400000;
      otf_reg_k    = $00800000;
      otf_reg_fpu  = $01000000;
      otf_reg_mmx  = $02000000;
      otf_reg_xmm  = $04000000;
      otf_reg_ymm  = $08000000;
      otf_reg_zmm  = $10000000;


      otf_reg_extra_mask = $0F000000;
      { Bits 16..19: subclasses, meaning depends on classes field }
      otf_sub0     = $00010000;
      otf_sub1     = $00020000;
      otf_sub2     = $00040000;
      otf_sub3     = $00080000;
      OT_REG_SMASK = otf_sub0 or otf_sub1 or otf_sub2 or otf_sub3;

      //OT_REG_EXTRA_MASK = $0F000000;
      OT_REG_EXTRA_MASK = $1F000000;

      OT_REG_TYPMASK = otf_reg_cdt or otf_reg_gpr or otf_reg_sreg or otf_reg_k or otf_reg_extra_mask;
      { register class 0: CRx, DRx and TRx }
      OT_REG_CDT   = OT_REGISTER or otf_reg_cdt or OT_BITS64;
      OT_REG_CREG  = OT_REG_CDT or otf_sub0;  { CRn  }
      OT_REG_DREG  = OT_REG_CDT or otf_sub1;  { DRn  }
      OT_REG_TREG  = OT_REG_CDT or otf_sub2;  { TRn  }
      OT_REG_CR4   = OT_REG_CDT or otf_sub3;  { CR4 (Pentium only)  }

      { register class 1: general-purpose registers }
      OT_REG_GPR   = OT_REGNORM or otf_reg_gpr;
      OT_RM_GPR    = OT_REGMEM or otf_reg_gpr;
      OT_REG8      = OT_REG_GPR or OT_BITS8;  { 8-bit GPR }
      OT_REG16     = OT_REG_GPR or OT_BITS16;
      OT_REG32     = OT_REG_GPR or OT_BITS32;
      OT_REG64     = OT_REG_GPR or OT_BITS64;

      { GPR subclass 0: accumulator: AL, AX, EAX or RAX }
      OT_REG_ACCUM = OT_REG_GPR or otf_sub0;
      OT_REG_AL    = OT_REG_ACCUM or OT_BITS8;
      OT_REG_AX    = OT_REG_ACCUM or OT_BITS16;
      OT_REG_EAX   = OT_REG_ACCUM or OT_BITS32;
      OT_REG_RAX   = OT_REG_ACCUM or OT_BITS64;
      { GPR subclass 1: counter: CL, CX, ECX or RCX }
      OT_REG_COUNT = OT_REG_GPR or otf_sub1;
      OT_REG_CL    = OT_REG_COUNT or OT_BITS8;
      OT_REG_CX    = OT_REG_COUNT or OT_BITS16;
      OT_REG_ECX   = OT_REG_COUNT or OT_BITS32;
      OT_REG_RCX   = OT_REG_COUNT or OT_BITS64;
      { GPR subclass 2: data register: DL, DX, EDX or RDX }
      OT_REG_DX    = OT_REG_GPR or otf_sub2 or OT_BITS16;
      OT_REG_EDX   = OT_REG_GPR or otf_sub2 or OT_BITS32;

      { register class 2: Segment registers }
      OT_REG_SREG  = OT_REGISTER or otf_reg_sreg or OT_BITS16;
      OT_REG_CS    = OT_REG_SREG or otf_sub0;  { CS  }
      OT_REG_DESS  = OT_REG_SREG or otf_sub1;  { DS, ES, SS (non-CS 86 registers)  }
      OT_REG_FSGS  = OT_REG_SREG or otf_sub2;  { FS, GS (386 extended registers)  }

      { register class 3: FPU registers }
      OT_FPUREG    = OT_REGISTER or otf_reg_fpu;
      OT_FPU0      = OT_FPUREG or otf_sub0;    { FPU stack register zero  }

      { register class 4: MMX (both reg and r/m) }
      OT_MMXREG    = OT_REGNORM or otf_reg_mmx;
      OT_MMXRM     = OT_REGMEM or otf_reg_mmx;

      { register class 5: XMM (both reg and r/m) }
      OT_XMMREG    = OT_REGNORM or otf_reg_xmm;
      OT_XMMRM     = OT_REGMEM or otf_reg_xmm;
      OT_XMEM32    = OT_REGNORM or otf_reg_xmm or otf_reg_gpr or OT_BITS32;
      OT_XMEM32_M  = OT_XMEM32 or OT_VECTORMASK;
      OT_XMEM64    = OT_REGNORM or otf_reg_xmm or otf_reg_gpr or OT_BITS64;
      OT_XMEM64_M  = OT_XMEM64 or OT_VECTORMASK;

      OT_XMMREG_M   = OT_XMMREG or OT_VECTORMASK;
      OT_XMMREG_MZ  = OT_XMMREG or OT_VECTORMASK or OT_VECTORZERO;
      OT_XMMRM_MZ   = OT_XMMRM  or OT_VECTORMASK or OT_VECTORZERO;
      OT_XMMREG_SAE = OT_XMMREG or OT_VECTORSAE;
      OT_XMMRM_SAE  = OT_XMMRM  or OT_VECTORSAE;
      OT_XMMREG_ER  = OT_XMMREG or OT_VECTORER;
      OT_XMMRM_ER   = OT_XMMRM  or OT_VECTORER;



      { register class 5: YMM (both reg and r/m) }
      OT_YMMREG     = OT_REGNORM or otf_reg_ymm;
      OT_YMMRM      = OT_REGMEM or otf_reg_ymm;
      OT_YMEM32     = OT_REGNORM or otf_reg_ymm or otf_reg_gpr or OT_BITS32;
      OT_YMEM32_M   = OT_YMEM32 or OT_VECTORMASK;
      OT_YMEM64     = OT_REGNORM or otf_reg_ymm or otf_reg_gpr or OT_BITS64;
      OT_YMEM64_M   = OT_YMEM64 or OT_VECTORMASK;

      OT_YMMREG_M   = OT_YMMREG or OT_VECTORMASK;
      OT_YMMREG_MZ  = OT_YMMREG or OT_VECTORMASK or OT_VECTORZERO;
      OT_YMMRM_MZ   = OT_YMMRM  or OT_VECTORMASK or OT_VECTORZERO;
      OT_YMMREG_SAE = OT_YMMREG or OT_VECTORSAE;
      OT_YMMRM_SAE  = OT_YMMRM  or OT_VECTORSAE;
      OT_YMMREG_ER  = OT_YMMREG or OT_VECTORER;
      OT_YMMRM_ER   = OT_YMMRM  or OT_VECTORER;


      { register class 5: ZMM (both reg and r/m) }
      OT_ZMMREG     = OT_REGNORM or otf_reg_zmm;
      OT_ZMMRM      = OT_REGMEM or otf_reg_zmm;
      OT_ZMEM32     = OT_REGNORM or otf_reg_zmm or otf_reg_gpr or OT_BITS32;
      OT_ZMEM32_M   = OT_ZMEM32 or OT_VECTORMASK;
      OT_ZMEM64     = OT_REGNORM or otf_reg_zmm or otf_reg_gpr or OT_BITS64;
      OT_ZMEM64_M   = OT_ZMEM64 or OT_VECTORMASK;

      OT_ZMMREG_M   = OT_ZMMREG or OT_VECTORMASK;
      OT_ZMMREG_MZ  = OT_ZMMREG or OT_VECTORMASK or OT_VECTORZERO;
      OT_ZMMRM_MZ   = OT_ZMMRM  or OT_VECTORMASK or OT_VECTORZERO;
      OT_ZMMREG_SAE = OT_ZMMREG or OT_VECTORSAE;
      OT_ZMMRM_SAE  = OT_ZMMRM  or OT_VECTORSAE;
      OT_ZMMREG_ER  = OT_ZMMREG or OT_VECTORER;
      OT_ZMMRM_ER   = OT_ZMMRM  or OT_VECTORER;


      OT_KREG       = OT_REGNORM or otf_reg_k;
      OT_KREG_M     = OT_KREG or OT_VECTORMASK;

      { Vector-Memory operands }
      OT_VMEM_ANY  = OT_XMEM32 or OT_XMEM64 or OT_YMEM32 or OT_YMEM64 or OT_ZMEM32 or OT_ZMEM64;

      { Memory operands }
      OT_MEM8      = OT_MEMORY or OT_BITS8;
      OT_MEM16     = OT_MEMORY or OT_BITS16;
      OT_MEM16_M   = OT_MEM16  or OT_VECTORMASK;
      OT_MEM32     = OT_MEMORY or OT_BITS32;
      OT_MEM32_M   = OT_MEMORY or OT_BITS32 or OT_VECTORMASK;
      OT_BMEM32    = OT_MEMORY or OT_BITS32 or OT_VECTORBCST;
      OT_BMEM32_SAE= OT_MEMORY or OT_BITS32 or OT_VECTORBCST or OT_VECTORSAE;
      OT_MEM64     = OT_MEMORY or OT_BITS64;
      OT_MEM64_M   = OT_MEMORY or OT_BITS64 or OT_VECTORMASK;
      OT_BMEM64    = OT_MEMORY or OT_BITS64 or OT_VECTORBCST;
      OT_BMEM64_SAE= OT_MEMORY or OT_BITS64 or OT_VECTORBCST or OT_VECTORSAE;
      OT_MEM128    = OT_MEMORY or OT_BITS128;
      OT_MEM128_M  = OT_MEMORY or OT_BITS128 or OT_VECTORMASK;
      OT_MEM256    = OT_MEMORY or OT_BITS256;
      OT_MEM256_M  = OT_MEMORY or OT_BITS256 or OT_VECTORMASK;
      OT_MEM512    = OT_MEMORY or OT_BITS512;
      OT_MEM512_M  = OT_MEMORY or OT_BITS512 or OT_VECTORMASK;
      OT_MEM80     = OT_MEMORY or OT_BITS80;




      OT_MEM_OFFS  = OT_MEMORY or otf_sub0;  { special type of EA  }
                                             { simple [address] offset  }

      { Matches any type of r/m operand }
      OT_MEMORY_ANY = OT_MEMORY or OT_RM_GPR or OT_XMMRM or OT_MMXRM or OT_YMMRM or OT_ZMMRM or OT_REG_EXTRA_MASK;

      { Immediate operands }
      OT_IMM8      = OT_IMMEDIATE or OT_BITS8;
      OT_IMM16     = OT_IMMEDIATE or OT_BITS16;
      OT_IMM32     = OT_IMMEDIATE or OT_BITS32;
      OT_IMM64     = OT_IMMEDIATE or OT_BITS64;

      OT_ONENESS   = otf_sub0;  { special type of immediate operand  }
      OT_UNITY     = OT_IMMEDIATE or OT_ONENESS;  { for shift/rotate instructions  }

      std_op2str:op2strtable={$i ../../../compiler/x86_64/x8664int.inc}

  InsTab:array[0..instabentries-1] of TInsEntry={$i ../../../compiler/x86_64/x8664tab.inc}

  procedure BuildInsTabCache;
  var
    i : longint;
  begin
    new(instabcache);
    FillChar(instabcache^,sizeof(tinstabcache),$ff);
    i:=0;
    while (i<InsTabEntries) do
     begin
       if InsTabCache^[InsTab[i].OPcode]=-1 then
        InsTabCache^[InsTab[i].OPcode]:=i;
       inc(i);
     end;
  end;

  procedure BuildInsTabMemRefSizeInfoCache;
  var
    AsmOp: TasmOp;
    i,j: longint;
    insentry  : PInsEntry;

    MRefInfo: TMemRefSizeInfo;
    SConstInfo: TConstSizeInfo;
    actRegSize: int64;
    actMemSize: int64;
    actConstSize: int64;
    actRegCount: integer;
    actMemCount: integer;
    actConstCount: integer;
    actRegTypes  : int64;
    actRegMemTypes: int64;
    NewRegSize: int64;

    actVMemCount  : integer;
    actVMemTypes  : int64;

    RegMMXSizeMask: int64;
    RegXMMSizeMask: int64;
    RegYMMSizeMask: int64;
    RegZMMSizeMask: int64;

    RegMMXConstSizeMask: int64;
    RegXMMConstSizeMask: int64;
    RegYMMConstSizeMask: int64;
    RegZMMConstSizeMask: int64;

    RegBCSTSizeMask: int64;
    RegBCSTXMMSizeMask: int64;
    RegBCSTYMMSizeMask: int64;
    RegBCSTZMMSizeMask: int64;
    ExistsMemRef      : boolean;

    bitcount          : integer;
    ExistsCode336     : boolean;
    ExistsCode337     : boolean;
    ExistsSSEAVXReg   : boolean;

    function bitcnt(aValue: int64): integer;
    var
      i: integer;
    begin
      result := 0;

      for i := 0 to 63 do
      begin
        if (aValue mod 2) = 1 then
        begin
          inc(result);
        end;

        aValue := aValue shr 1;
      end;
    end;

  begin
    new(InsTabMemRefSizeInfoCache);
    FillChar(InsTabMemRefSizeInfoCache^,sizeof(TInsTabMemRefSizeInfoCache),0);

    for AsmOp := low(TAsmOp) to high(TAsmOp) do
    begin
      i := InsTabCache^[AsmOp];

      if i >= 0 then
      begin


        InsTabMemRefSizeInfoCache^[AsmOp].MemRefSize           := msiUnknown;
        InsTabMemRefSizeInfoCache^[AsmOp].MemRefSizeBCST       := msbUnknown;
        InsTabMemRefSizeInfoCache^[AsmOp].BCSTXMMMultiplicator := 0;
        InsTabMemRefSizeInfoCache^[AsmOp].ConstSize            := csiUnknown;
        InsTabMemRefSizeInfoCache^[AsmOp].ExistsSSEAVX         := false;
        InsTabMemRefSizeInfoCache^[AsmOp].BCSTTypes            := [];

        insentry:=@instab[i];

        RegMMXSizeMask := 0;
        RegXMMSizeMask := 0;
        RegYMMSizeMask := 0;
        RegZMMSizeMask := 0;

        RegMMXConstSizeMask := 0;
        RegXMMConstSizeMask := 0;
        RegYMMConstSizeMask := 0;
        RegZMMConstSizeMask := 0;

        RegBCSTSizeMask:= 0;
        RegBCSTXMMSizeMask := 0;
        RegBCSTYMMSizeMask := 0;
        RegBCSTZMMSizeMask := 0;
        ExistsMemRef       := false;

        while (insentry^.opcode=AsmOp) do
        begin
          MRefInfo         := msiUnknown;

          actRegSize       := 0;
          actRegCount      := 0;
          actRegTypes      := 0;
          NewRegSize       := 0;

          actMemSize       := 0;
          actMemCount      := 0;
          actRegMemTypes   := 0;

          actVMemCount     := 0;
          actVMemTypes     := 0;

          actConstSize     := 0;
          actConstCount    := 0;

          ExistsCode336   := false; // indicate fixed operand size 32 bit
          ExistsCode337   := false; // indicate fixed operand size 64 bit
          ExistsSSEAVXReg := false;

          // parse insentry^.code for &336 and &337
          // &336 (octal) = 222 (decimal) == fixed operand size 32 bit
          // &337 (octal) = 223 (decimal) == fixed operand size 64 bit
          for i := low(insentry^.code) to high(insentry^.code) do
          begin
            case insentry^.code[i] of
              #222: ExistsCode336 := true;
              #223: ExistsCode337 := true;
              #0,#1,#2,#3: break;
            end;
          end;

          for i := 0 to insentry^.ops -1 do
          begin
            if (insentry^.optypes[i] and OT_REGISTER) = OT_REGISTER then
             case insentry^.optypes[i] and (OT_XMMREG or OT_YMMREG or OT_ZMMREG or OT_KREG or OT_REG_EXTRA_MASK) of
                OT_XMMREG,
                OT_YMMREG,
                OT_ZMMREG: ExistsSSEAVXReg := true;
                      else;
             end;
          end;


          for j := 0 to insentry^.ops -1 do
          begin
            if ((insentry^.optypes[j] and OT_XMEM32) = OT_XMEM32) OR
               ((insentry^.optypes[j] and OT_XMEM64) = OT_XMEM64) OR
               ((insentry^.optypes[j] and OT_YMEM32) = OT_YMEM32) OR
               ((insentry^.optypes[j] and OT_YMEM64) = OT_YMEM64) OR
               ((insentry^.optypes[j] and OT_ZMEM32) = OT_ZMEM32) OR
               ((insentry^.optypes[j] and OT_ZMEM64) = OT_ZMEM64) then
            begin
              inc(actVMemCount);

              case insentry^.optypes[j] and (OT_XMEM32 OR OT_XMEM64 OR OT_YMEM32 OR OT_YMEM64 OR OT_ZMEM32 OR OT_ZMEM64) of
                OT_XMEM32: actVMemTypes := actVMemTypes or OT_XMEM32;
                OT_XMEM64: actVMemTypes := actVMemTypes or OT_XMEM64;
                OT_YMEM32: actVMemTypes := actVMemTypes or OT_YMEM32;
                OT_YMEM64: actVMemTypes := actVMemTypes or OT_YMEM64;
                OT_ZMEM32: actVMemTypes := actVMemTypes or OT_ZMEM32;
                OT_ZMEM64: actVMemTypes := actVMemTypes or OT_ZMEM64;
                      else;
              end;
            end
            else if (insentry^.optypes[j] and OT_REGISTER) = OT_REGISTER then
            begin
              inc(actRegCount);

                NewRegSize := (insentry^.optypes[j] and OT_SIZE_MASK);
                if NewRegSize = 0 then
                  begin
                    case insentry^.optypes[j] and (OT_MMXREG or OT_XMMREG or OT_YMMREG or OT_ZMMREG or OT_KREG or OT_REG_EXTRA_MASK) of
                      OT_MMXREG: begin
                                   NewRegSize := OT_BITS64;
                                 end;
                      OT_XMMREG: begin
                                   NewRegSize := OT_BITS128;
                                   InsTabMemRefSizeInfoCache^[AsmOp].ExistsSSEAVX := true;
                                 end;
                      OT_YMMREG: begin
                                   NewRegSize := OT_BITS256;
                                   InsTabMemRefSizeInfoCache^[AsmOp].ExistsSSEAVX := true;
                                 end;
                      OT_ZMMREG: begin
                                   NewRegSize := OT_BITS512;
                                   InsTabMemRefSizeInfoCache^[AsmOp].ExistsSSEAVX := true;
                                 end;
                        OT_KREG: begin
                                   InsTabMemRefSizeInfoCache^[AsmOp].ExistsSSEAVX := true;
                                 end;

                            else NewRegSize := not(0);
                    end;
                end;

              actRegSize  := actRegSize or NewRegSize;
              actRegTypes := actRegTypes or (insentry^.optypes[j] and (OT_MMXREG or OT_XMMREG or OT_YMMREG or OT_ZMMREG or OT_KREG or OT_REG_EXTRA_MASK));
              end
            else if ((insentry^.optypes[j] and OT_MEMORY) <> 0) then
              begin
                inc(actMemCount);


                if ExistsSSEAVXReg and ExistsCode336 then
                  actMemSize := actMemSize or OT_BITS32
                else if ExistsSSEAVXReg and ExistsCode337 then
                  actMemSize := actMemSize or OT_BITS64
                else
                  actMemSize:=actMemSize or (insentry^.optypes[j] and (OT_SIZE_MASK OR OT_VECTORBCST));

                if (insentry^.optypes[j] and OT_REGMEM) = OT_REGMEM then
                  begin
                    actRegMemTypes  := actRegMemTypes or insentry^.optypes[j];
                  end;
              end
            else if ((insentry^.optypes[j] and OT_IMMEDIATE) = OT_IMMEDIATE) then
            begin
              inc(actConstCount);

              actConstSize    := actConstSize or (insentry^.optypes[j] and OT_SIZE_MASK);
            end
          end;

          if actConstCount > 0 then
          begin
            case actConstSize of
              0: SConstInfo := csiNoSize;
              OT_BITS8: SConstInfo := csiMem8;
              OT_BITS16: SConstInfo := csiMem16;
              OT_BITS32: SConstInfo := csiMem32;
              OT_BITS64: SConstInfo := csiMem64;
              else SConstInfo := csiMultiple;
            end;

            if InsTabMemRefSizeInfoCache^[AsmOp].ConstSize = csiUnknown then
            begin
              InsTabMemRefSizeInfoCache^[AsmOp].ConstSize := SConstInfo;
            end
            else if InsTabMemRefSizeInfoCache^[AsmOp].ConstSize <> SConstInfo then
            begin
              InsTabMemRefSizeInfoCache^[AsmOp].ConstSize := csiMultiple;
            end;
          end;

          if actVMemCount > 0 then
          begin
            if actVMemCount = 1 then
            begin
              if actVMemTypes > 0 then
              begin
                case actVMemTypes of
                  OT_XMEM32: MRefInfo := msiXMem32;
                  OT_XMEM64: MRefInfo := msiXMem64;
                  OT_YMEM32: MRefInfo := msiYMem32;
                  OT_YMEM64: MRefInfo := msiYMem64;
                  OT_ZMEM32: MRefInfo := msiZMem32;
                  OT_ZMEM64: MRefInfo := msiZMem64;
                        else;
                end;

                case actRegTypes of
                  OT_XMMREG: case MRefInfo of
                               msiXMem32,
                               msiXMem64: RegXMMSizeMask := RegXMMSizeMask or OT_BITS128;
                               msiYMem32,
                               msiYMem64: RegXMMSizeMask := RegXMMSizeMask or OT_BITS256;
                               msiZMem32,
                               msiZMem64: RegXMMSizeMask := RegXMMSizeMask or OT_BITS512;
                                     else;
                             end;
                  OT_YMMREG: case MRefInfo of
                               msiXMem32,
                               msiXMem64: RegYMMSizeMask := RegYMMSizeMask or OT_BITS128;
                               msiYMem32,
                               msiYMem64: RegYMMSizeMask := RegYMMSizeMask or OT_BITS256;
                               msiZMem32,
                               msiZMem64: RegYMMSizeMask := RegYMMSizeMask or OT_BITS512;
                                     else;
                             end;
                  OT_ZMMREG: case MRefInfo of
                               msiXMem32,
                               msiXMem64: RegZMMSizeMask := RegZMMSizeMask or OT_BITS128;
                               msiYMem32,
                               msiYMem64: RegZMMSizeMask := RegZMMSizeMask or OT_BITS256;
                               msiZMem32,
                               msiZMem64: RegZMMSizeMask := RegZMMSizeMask or OT_BITS512;
                                     else;
                             end;

                        //else InternalError(777209);
                end;


                if InsTabMemRefSizeInfoCache^[AsmOp].MemRefSize = msiUnknown then
                begin
                  InsTabMemRefSizeInfoCache^[AsmOp].MemRefSize := MRefInfo;
                end
                else if InsTabMemRefSizeInfoCache^[AsmOp].MemRefSize <> MRefInfo then
                begin
                  if InsTabMemRefSizeInfoCache^[AsmOp].MemRefSize in [msiXMem32, msiXMem64, msiYMem32, msiYMem64, msiZMem32, msiZMem64] then
                  begin
                    InsTabMemRefSizeInfoCache^[AsmOp].MemRefSize := msiVMemMultiple;
                  end
                  else if InsTabMemRefSizeInfoCache^[AsmOp].MemRefSize <> msiVMemMultiple then;
                end;

              end;
            end
            else;
          end
          else
            begin
              if (actMemCount=2) and ((AsmOp=A_MOVS) or (AsmOp=A_CMPS)) then actMemCount:=1;

              ExistsMemRef := ExistsMemRef or (actMemCount > 0);

              case actMemCount of
                0: ; // nothing todo
                1: begin
                     MRefInfo := msiUnknown;

                     if not(ExistsCode336 or ExistsCode337) then
                       case actRegMemTypes and (OT_MMXRM or OT_XMMRM or OT_YMMRM or OT_ZMMRM or OT_REG_EXTRA_MASK) of
                         OT_MMXRM: actMemSize := actMemSize or OT_BITS64;
                         OT_XMMRM: actMemSize := actMemSize or OT_BITS128;
                         OT_YMMRM: actMemSize := actMemSize or OT_BITS256;
                         OT_ZMMRM: actMemSize := actMemSize or OT_BITS512;
                       end;

                     case actMemSize of
                                0: MRefInfo := msiNoSize;
                         OT_BITS8: MRefInfo := msiMem8;
                        OT_BITS16: MRefInfo := msiMem16;
                        OT_BITS32: MRefInfo := msiMem32;
                       OT_BITSB32: MRefInfo := msiBMem32;
                        OT_BITS64: MRefInfo := msiMem64;
                       OT_BITSB64: MRefInfo := msiBMem64;
                       OT_BITS128: MRefInfo := msiMem128;
                       OT_BITS256: MRefInfo := msiMem256;
                       OT_BITS512: MRefInfo := msiMem512;
                       OT_BITS80,
                       OT_FAR,
                       OT_NEAR,
                       OT_SHORT: ; // ignore
                       else
                         begin
                           bitcount := bitcnt(actMemSize);

                           if bitcount > 1 then MRefInfo := msiMultiple
                           else;
                         end;
                     end;

                     if InsTabMemRefSizeInfoCache^[AsmOp].MemRefSize = msiUnknown then
                       begin
                         InsTabMemRefSizeInfoCache^[AsmOp].MemRefSize := MRefInfo;
                       end
                     else
                     begin
                       // ignore broadcast-memory
                       if not(MRefInfo in [msiBMem32, msiBMem64]) then
                       begin
                         if InsTabMemRefSizeInfoCache^[AsmOp].MemRefSize <> MRefInfo then
                         begin
                           with InsTabMemRefSizeInfoCache^[AsmOp] do
                           begin
                             if ((MemRefSize in [msiMem8, msiMULTIPLEMinSize8]) OR (MRefInfo = msiMem8))   then MemRefSize := msiMultipleMinSize8
                             else if ((MemRefSize in [ msiMem16,  msiMULTIPLEMinSize16])  OR (MRefInfo =  msiMem16)) then MemRefSize := msiMultipleMinSize16
                             else if ((MemRefSize in [ msiMem32,  msiMULTIPLEMinSize32])  OR (MRefInfo =  msiMem32)) then MemRefSize := msiMultipleMinSize32
                             else if ((MemRefSize in [ msiMem64,  msiMULTIPLEMinSize64])  OR (MRefInfo =  msiMem64)) then MemRefSize := msiMultipleMinSize64
                             else if ((MemRefSize in [msiMem128, msiMULTIPLEMinSize128])  OR (MRefInfo = msiMem128)) then MemRefSize := msiMultipleMinSize128
                             else if ((MemRefSize in [msiMem256, msiMULTIPLEMinSize256])  OR (MRefInfo = msiMem256)) then MemRefSize := msiMultipleMinSize256
                             else if ((MemRefSize in [msiMem512, msiMULTIPLEMinSize512])  OR (MRefInfo = msiMem512)) then MemRefSize := msiMultipleMinSize512
                             else MemRefSize := msiMultiple;
                           end;
                         end;
                       end;
                     end;

                     //if not(MRefInfo in [msiBMem32, msiBMem64]) and (actRegCount > 0) then
                     if actRegCount > 0 then
                     begin
                       if MRefInfo in [msiBMem32, msiBMem64] then
                       begin
                         if IF_BCST2  in insentry^.flags then InsTabMemRefSizeInfoCache^[AsmOp].BCSTTypes := InsTabMemRefSizeInfoCache^[AsmOp].BCSTTypes + [bt1to2];
                         if IF_BCST4  in insentry^.flags then InsTabMemRefSizeInfoCache^[AsmOp].BCSTTypes := InsTabMemRefSizeInfoCache^[AsmOp].BCSTTypes + [bt1to4];
                         if IF_BCST8  in insentry^.flags then InsTabMemRefSizeInfoCache^[AsmOp].BCSTTypes := InsTabMemRefSizeInfoCache^[AsmOp].BCSTTypes + [bt1to8];
                         if IF_BCST16 in insentry^.flags then InsTabMemRefSizeInfoCache^[AsmOp].BCSTTypes := InsTabMemRefSizeInfoCache^[AsmOp].BCSTTypes + [bt1to16];

                         //InsTabMemRefSizeInfoCache^[AsmOp].BCSTTypes

                         // BROADCAST - OPERAND
                         RegBCSTSizeMask := RegBCSTSizeMask or actMemSize;

                         case actRegTypes and (OT_XMMREG or OT_YMMREG or OT_ZMMREG or OT_REG_EXTRA_MASK) of
                           OT_XMMREG: RegBCSTXMMSizeMask := RegBCSTXMMSizeMask or actMemSize;
                           OT_YMMREG: RegBCSTYMMSizeMask := RegBCSTYMMSizeMask or actMemSize;
                           OT_ZMMREG: RegBCSTZMMSizeMask := RegBCSTZMMSizeMask or actMemSize;
                                 else begin

                                        RegBCSTXMMSizeMask := not(0);
                                        RegBCSTYMMSizeMask := not(0);
                                        RegBCSTZMMSizeMask := not(0);
                                      end;
                         end;
                       end
                       else
                       case actRegTypes and (OT_MMXREG or OT_XMMREG or OT_YMMREG or OT_ZMMREG or OT_REG_EXTRA_MASK) of
                         OT_MMXREG: if actConstCount > 0 then RegMMXConstSizeMask := RegMMXConstSizeMask or actMemSize
                                     else RegMMXSizeMask := RegMMXSizeMask or actMemSize;
                         OT_XMMREG: if actConstCount > 0 then RegXMMConstSizeMask := RegXMMConstSizeMask or actMemSize
                                     else RegXMMSizeMask := RegXMMSizeMask or actMemSize;
                         OT_YMMREG: if actConstCount > 0 then RegYMMConstSizeMask := RegYMMConstSizeMask or actMemSize
                                     else RegYMMSizeMask := RegYMMSizeMask or actMemSize;
                         OT_ZMMREG: if actConstCount > 0 then RegZMMConstSizeMask := RegZMMConstSizeMask or actMemSize
                                     else RegZMMSizeMask := RegZMMSizeMask or actMemSize;
                               else begin
                                      RegMMXSizeMask := not(0);
                                      RegXMMSizeMask := not(0);
                                      RegYMMSizeMask := not(0);
                                      RegZMMSizeMask := not(0);

                                      RegMMXConstSizeMask := not(0);
                                      RegXMMConstSizeMask := not(0);
                                      RegYMMConstSizeMask := not(0);
                                      RegZMMConstSizeMask := not(0);
                                    end;
                       end;
                     end
                     else


                   end
                else;
              end;
            end;

          inc(insentry);
        end;

        if InsTabMemRefSizeInfoCache^[AsmOp].ExistsSSEAVX then
        begin
          case RegBCSTSizeMask of
                    0: ; // ignore;
            OT_BITSB32: begin
                          InsTabMemRefSizeInfoCache^[AsmOp].MemRefSizeBCST       := msbBCST32;
                          InsTabMemRefSizeInfoCache^[AsmOp].BCSTXMMMultiplicator := 4;
                        end;
            OT_BITSB64: begin
                          InsTabMemRefSizeInfoCache^[AsmOp].MemRefSizeBCST       := msbBCST64;
                          InsTabMemRefSizeInfoCache^[AsmOp].BCSTXMMMultiplicator := 2;
                        end;
                  else begin
                         InsTabMemRefSizeInfoCache^[AsmOp].MemRefSizeBCST := msbMultiple;
                       end;
          end;
        end;


        if (InsTabMemRefSizeInfoCache^[AsmOp].MemRefSize in MemRefMultiples) and
           (InsTabMemRefSizeInfoCache^[AsmOp].ExistsSSEAVX)then
        begin
          if InsTabMemRefSizeInfoCache^[AsmOp].MemRefSize = msiVMemMultiple then
          begin
            if ((RegXMMSizeMask = OT_BITS128) or (RegXMMSizeMask = 0))     and
               ((RegYMMSizeMask = OT_BITS256) or (RegYMMSizeMask = 0))     and
               ((RegZMMSizeMask = OT_BITS512) or (RegZMMSizeMask = 0))     and
               ((RegXMMSizeMask or RegYMMSizeMask or RegZMMSizeMask) <> 0) then
            begin
              InsTabMemRefSizeInfoCache^[AsmOp].MemRefSize := msiVMemRegSize;
            end;
          end
          else if (RegMMXSizeMask or RegMMXConstSizeMask) <> 0 then
          begin
            if ((RegMMXSizeMask or RegMMXConstSizeMask) = OT_BITS64)  and
               ((RegXMMSizeMask or RegXMMConstSizeMask) = OT_BITS128) and
               ((RegYMMSizeMask or RegYMMConstSizeMask) = 0)          and
               ((RegZMMSizeMask or RegZMMConstSizeMask) = 0) then
            begin
              InsTabMemRefSizeInfoCache^[AsmOp].MemRefSize := msiMemRegSize;
            end;
          end
          else if (((RegXMMSizeMask or RegXMMConstSizeMask) = OT_BITS128) or ((RegXMMSizeMask or RegXMMConstSizeMask) = 0)) and
                  (((RegYMMSizeMask or RegYMMConstSizeMask) = OT_BITS256) or ((RegYMMSizeMask or RegYMMConstSizeMask) = 0)) and
                  (((RegZMMSizeMask or RegZMMConstSizeMask) = OT_BITS512) or ((RegZMMSizeMask or RegZMMConstSizeMask) = 0)) and
                  (((RegXMMSizeMask or RegXMMConstSizeMask or
                     RegYMMSizeMask or RegYMMConstSizeMask or
                     RegZMMSizeMask or RegZMMConstSizeMask)) <> 0) then
          begin
            InsTabMemRefSizeInfoCache^[AsmOp].MemRefSize := msiMemRegSize;
          end
          else if (RegXMMSizeMask or RegXMMConstSizeMask = OT_BITS16) and
                  (RegYMMSizeMask or RegYMMConstSizeMask = OT_BITS32) and
                  (RegZMMSizeMask or RegZMMConstSizeMask = 0) then
          begin
            InsTabMemRefSizeInfoCache^[AsmOp].MemRefSize := msiMemRegx16y32;
          end
          else if (RegXMMSizeMask or RegXMMConstSizeMask = OT_BITS16) and
                  (RegYMMSizeMask or RegYMMConstSizeMask = OT_BITS32) and
                  (RegZMMSizeMask or RegZMMConstSizeMask = OT_BITS64) then
          begin
            InsTabMemRefSizeInfoCache^[AsmOp].MemRefSize := msiMemRegx16y32z64;
          end
          else if ((RegXMMSizeMask or RegXMMConstSizeMask) = OT_BITS32) and
                  ((RegYMMSizeMask or RegYMMConstSizeMask) = OT_BITS64) then
          begin
            if ((RegZMMSizeMask or RegZMMConstSizeMask) = 0) then
            begin
              InsTabMemRefSizeInfoCache^[AsmOp].MemRefSize := msiMemRegx32y64;
            end
            else if ((RegZMMSizeMask or RegZMMConstSizeMask) = OT_BITS128) then
            begin
              InsTabMemRefSizeInfoCache^[AsmOp].MemRefSize := msiMemRegx32y64z128;
            end;
          end
          else if ((RegXMMSizeMask or RegXMMConstSizeMask) = OT_BITS64)  and
                  ((RegYMMSizeMask or RegYMMConstSizeMask) = OT_BITS128) and
                  ((RegZMMSizeMask or RegZMMConstSizeMask) = 0) then
          begin
            InsTabMemRefSizeInfoCache^[AsmOp].MemRefSize := msiMemRegx64y128;
          end
          else if ((RegXMMSizeMask or RegXMMConstSizeMask) = OT_BITS64)  and
                  ((RegYMMSizeMask or RegYMMConstSizeMask) = OT_BITS128) and
                  ((RegZMMSizeMask or RegZMMConstSizeMask) = OT_BITS256) then
          begin
            InsTabMemRefSizeInfoCache^[AsmOp].MemRefSize := msiMemRegx64y128z256;
          end
          else if ((RegXMMSizeMask or RegXMMConstSizeMask) = OT_BITS64)  and
                  ((RegYMMSizeMask or RegYMMConstSizeMask) = OT_BITS256) and
                  ((RegZMMSizeMask or RegZMMConstSizeMask) = 0) then
          begin
            InsTabMemRefSizeInfoCache^[AsmOp].MemRefSize := msiMemRegx64y256;
          end
          else if ((RegXMMSizeMask or RegXMMConstSizeMask) = OT_BITS64)  and
                  ((RegYMMSizeMask or RegYMMConstSizeMask) = OT_BITS256) and
                  ((RegZMMSizeMask or RegZMMConstSizeMask) = OT_BITS512) then
          begin
            InsTabMemRefSizeInfoCache^[AsmOp].MemRefSize := msiMemRegx64y256z512;
          end
          else if ((RegXMMConstSizeMask = 0) or (RegXMMConstSizeMask = OT_BITS128))     and
                  ((RegYMMConstSizeMask = 0) or (RegYMMConstSizeMask = OT_BITS256))     and
                  ((RegZMMConstSizeMask = 0) or (RegZMMConstSizeMask = OT_BITS512))     and
                  ((RegXMMConstSizeMask or RegYMMConstSizeMask or RegZMMConstSizeMask) <> 0) and
                  (
                   ((RegXMMSizeMask or RegYMMSizeMask or RegZMMSizeMask) = OT_BITS128) or
                   ((RegXMMSizeMask or RegYMMSizeMask or RegZMMSizeMask) = OT_BITS256) or
                   ((RegXMMSizeMask or RegYMMSizeMask or RegZMMSizeMask) = OT_BITS512)
                  ) then
          begin
            case RegXMMSizeMask or RegYMMSizeMask or RegZMMSizeMask of
              OT_BITS128: InsTabMemRefSizeInfoCache^[AsmOp].MemRefSize := msiMemRegConst128;
              OT_BITS256: InsTabMemRefSizeInfoCache^[AsmOp].MemRefSize := msiMemRegConst256;
              OT_BITS512: InsTabMemRefSizeInfoCache^[AsmOp].MemRefSize := msiMemRegConst512;
                     else InsTabMemRefSizeInfoCache^[AsmOp].MemRefSize := msiMultiple;
            end;
          end
          else
          begin
            if not(
                   (AsmOp = A_CVTSI2SS) or
                   (AsmOp = A_CVTSI2SD) or
                   (AsmOp = A_CVTPD2DQ) or
                   (AsmOp = A_VCVTPD2DQ) or
                   (AsmOp = A_VCVTPD2PS) or
                   (AsmOp = A_VCVTSI2SD) or
                   (AsmOp = A_VCVTSI2SS) or
                   (AsmOp = A_VCVTTPD2DQ) or
                   (AsmOp = A_VCVTPD2UDQ) or
                   (AsmOp = A_VCVTQQ2PS) or
                   (AsmOp = A_VCVTTPD2UDQ) or
                   (AsmOp = A_VCVTUQQ2PS) or
                   (AsmOp = A_VCVTUSI2SD) or
                   (AsmOp = A_VCVTUSI2SS) or


                   // TODO check
                   (AsmOp = A_VCMPSS)


                  ) then;

          end;

        end
        else if (InsTabMemRefSizeInfoCache^[AsmOp].ExistsSSEAVX) and
                (InsTabMemRefSizeInfoCache^[AsmOp].MemRefSize = msiUnknown) and
                (not(ExistsMemRef)) then
        begin
          InsTabMemRefSizeInfoCache^[AsmOp].MemRefSize := msiNoMemRef;
        end;

        InsTabMemRefSizeInfoCache^[AsmOp].RegXMMSizeMask:=RegXMMSizeMask;
        InsTabMemRefSizeInfoCache^[AsmOp].RegYMMSizeMask:=RegYMMSizeMask;
        InsTabMemRefSizeInfoCache^[AsmOp].RegZMMSizeMask:=RegZMMSizeMask;

      end;
    end;

    for AsmOp := low(TAsmOp) to high(TAsmOp) do
    begin


      // only supported intructiones with SSE- or AVX-operands
      if not(InsTabMemRefSizeInfoCache^[AsmOp].ExistsSSEAVX) then
      begin
        InsTabMemRefSizeInfoCache^[AsmOp].MemRefSize  := msiUnknown;
        InsTabMemRefSizeInfoCache^[AsmOp].ConstSize   := csiUnknown;
      end;
    end;
  end;

{ TOperandListItem }

constructor TOperandListItem.Create;
begin
  inherited;

  FOpActive := false;
  FOpNumber := -1;
  FOpTyp    := otUnknown;
  FValues := TStringList.Create;
end;

destructor TOperandListItem.Destroy;
begin
  FreeAndNil(FValues);
  inherited;
end;

{ TOperandList }

function TOperandList.Add(aItem: TOperandListItem): integer;
begin
  result := FList.Add(aItem);
end;

function TOperandList.GetItems(aIndex: integer): TOperandListItem;
begin
  result := TOperandListItem(FList[aIndex]);
end;

{ TAsmTestGenerator }

function TAsmTestGenerator.InternalCalcTestData(const aInst, aOp1, aOp2, aOp3,
  aOp4: String): TStringList;
var
  i: integer;
  Item: TOperandListItem;
  OItem1: TOperandListItem;
  OItem2: TOperandListItem;
  OItem3: TOperandListItem;
  OItem4: TOperandListItem;

  il_Op: integer;
  il_Op1: integer;
  il_Op2: integer;
  il_Op3: integer;
  il_Op4: integer;

  sSuffix: string;
  sl_Operand: String;
  sl_Inst   : String;
  sl_RegCombi: String;
  sl_Prefix: String;
  UsePrefix: boolean;
  il_Operands: integer;
  UsedParams: cardinal;
  UseDefault: boolean;
  sl_RegCombi1: string;
  sl_RegCombi2: string;
  sl_RegCombi3: string;
  MaskRegNeeded:boolean;


  function PrepareOperandTyp(const aTyp: String): String;
  begin
    result := aTyp;
    if copy(result, length(result), 1) = '*' then result := copy(result, 1, length(result) - 1);
    if result = 'XMMRM128' then result := 'XMMRM';
    if result = 'YMMRM256' then result := 'YMMRM';
  end;


begin
  result := TStringList.Create;

  OItem1 := TOperandListItem.Create;
  try
    OItem2 := TOperandListItem.Create;
    try
      OItem3 := TOperandListItem.Create;
      try
        OItem4 := TOperandListItem.Create;
        try

          UsePrefix := (UpperCase(aInst) = 'VCVTPD2DQ') OR
                       (UpperCase(aInst) = 'VCVTPD2PS') OR
                       (UpperCase(aInst) = 'VCVTSI2SD') OR
                       (UpperCase(aInst) = 'VCVTSI2SS') OR
                       (UpperCase(aInst) = 'VCVTTPD2DQ') or
                       (UpperCase(aInst) = 'VPMOVZXWQ') or
                       (UpperCase(aInst) = 'VCVTPD2UDQ') or
                       (UpperCase(aInst) = 'VCVTPD2UDQ') or
                       (UpperCase(aInst) = 'VCVTTPD2UDQ') or
                       (UpperCase(aInst) = 'VCVTUQQ2PS') or
                       (UpperCase(aInst) = 'VCVTQQ2PS') or
                       (UpperCase(aInst) = 'VCVTUSI2SD') or
                       (UpperCase(aInst) = 'VCVTUSI2SS') or
                       (UpperCase(aInst) = 'VFPCLASSPD') or
                       (UpperCase(aInst) = 'VFPCLASSPS') or
                       (UpperCase(aInst) = 'VCMPSS')

                       ;

            
          MaskRegNeeded := (Pos('VGATHER', Uppercase(aInst)) = 1) or
                           (Pos('VPGATHER', Uppercase(aInst)) = 1) or
			   (Pos('VPSCATTER', Uppercase(aInst)) = 1) or
			   (Pos('VSCATTER', Uppercase(aInst)) = 1);

          for il_Op := 1 to 4 do
          begin
            sl_Prefix := '';

            case il_Op of
              1: begin
                   Item := OItem1;
                   sl_Operand := aOp1;
                 end;
              2: begin
                   Item := OItem2;
                   sl_Operand := aOp2;
                 end;
              3: begin
                   Item := OItem3;
                   sl_Operand := aOp3;
                 end;
              4: begin
                   Item := OItem4;
                   sl_Operand := aOp4;
                 end;
            end;

            sl_Operand := PrepareOperandTyp(sl_Operand);

            if (AnsiSameText(sl_Operand, 'XMMREG')) or
               (AnsiSameText(sl_Operand, 'XMMREG_M')) or
               (AnsiSameText(sl_Operand, 'XMMREG_MZ')) or
               (AnsiSameText(sl_Operand, 'XMMREG_ER')) or
               (AnsiSameText(sl_Operand, 'XMMREG_SAE')) then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otXMMReg;
              Item.OpActive := true;

              sSuffix := '';
              if Pos('_MZ', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1} {z}'
               else if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';

              if Pos('_ER', sl_Operand) > 0 then sSuffix := ', {ru-sae}'
               else if FSAE and (Pos('_SAE', AnsiUppercase(sl_Operand)) > 0) then sSuffix := ', {sae}';

              Item.Values.Add('XMM0' + sSuffix);
              Item.Values.Add('XMM1' + sSuffix);
              Item.Values.Add('XMM2' + sSuffix);
              Item.Values.Add('XMM3' + sSuffix);
              Item.Values.Add('XMM4' + sSuffix);
              Item.Values.Add('XMM5' + sSuffix);
              Item.Values.Add('XMM6' + sSuffix);
              Item.Values.Add('XMM7' + sSuffix);

              if x64 then
              begin
                Item.Values.Clear;
                if FAVX512 then
                begin
                  Item.Values.Add('XMM0' + sSuffix);
                  Item.Values.Add('XMM9' + sSuffix);
                  Item.Values.Add('XMM18' + sSuffix);
                  Item.Values.Add('XMM27' + sSuffix);
                  Item.Values.Add('XMM31' + sSuffix);

                  if (sSuffix <> '') and
		     (MaskRegNeeded = false)  then
                  begin
                    Item.Values.Add('XMM0');
                    Item.Values.Add('XMM9');
                    Item.Values.Add('XMM18');
                    Item.Values.Add('XMM27');
                    Item.Values.Add('XMM31');
                  end;    
                end
                else
                begin
                  Item.Values.Add('XMM0' + sSuffix);
                  Item.Values.Add('XMM4' + sSuffix);
                  Item.Values.Add('XMM8' + sSuffix);
                  Item.Values.Add('XMM12' + sSuffix);
                  Item.Values.Add('XMM15' + sSuffix);

                  if (sSuffix <> '') and
		     (MaskRegNeeded = false)  then
                  begin
                    Item.Values.Add('XMM0');
                    Item.Values.Add('XMM4');
                    Item.Values.Add('XMM8');
                    Item.Values.Add('XMM12');
                    Item.Values.Add('XMM15');
                  end;    
                end;
              end;
            end
            else if (AnsiSameText(sl_Operand, 'XMMRM')) or
                    (AnsiSameText(sl_Operand, 'XMMRM_M')) or
                    (AnsiSameText(sl_Operand, 'XMMRM_MZ')) or
                    (AnsiSameText(sl_Operand, 'XMMRM_ER')) or
                    (AnsiSameText(sl_Operand, 'XMMRM_SAE')) then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otXMMRM;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'oword ';
	      
	      sSuffix := '';
              if Pos('_MZ', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1} {z}'
               else if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';

              if Pos('_ER', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ', {rd-sae}'
               else if FSAE and (Pos('_SAE', AnsiUppercase(sl_Operand)) > 0) then sSuffix := ', {sae}';

              Item.Values.Add('XMM0' + sSuffix);
              Item.Values.Add('XMM1' + sSuffix);
              Item.Values.Add('XMM2' + sSuffix);
              Item.Values.Add('XMM3' + sSuffix);
              Item.Values.Add('XMM4' + sSuffix);
              Item.Values.Add('XMM5' + sSuffix);
              Item.Values.Add('XMM6' + sSuffix);
              Item.Values.Add('XMM7' + sSuffix);

              if x64 then
              begin
                Item.Values.Clear;
                if FAVX512 then
                begin
                  Item.Values.Add('XMM0' + sSuffix);
                  Item.Values.Add('XMM0');
                  Item.Values.Add('XMM9' + sSuffix);
                  Item.Values.Add('XMM9');
                  Item.Values.Add('XMM18' + sSuffix);
                  Item.Values.Add('XMM18');
                  Item.Values.Add('XMM27' + sSuffix);
                  Item.Values.Add('XMM27');
                  Item.Values.Add('XMM31' + sSuffix);
                  Item.Values.Add('XMM31');
                end
                else
                begin
                  Item.Values.Add('XMM0' + sSuffix);
                  Item.Values.Add('XMM0');
                  Item.Values.Add('XMM4' + sSuffix);
                  Item.Values.Add('XMM4');
                  Item.Values.Add('XMM8' + sSuffix);
                  Item.Values.Add('XMM8');
                  Item.Values.Add('XMM12' + sSuffix);
                  Item.Values.Add('XMM15');
                  Item.Values.Add('XMM15' + sSuffix);
                end;

                MemRegBaseIndexCombi(sl_Prefix, '', FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else
              begin
                MemRegBaseIndexCombi(sl_Prefix, '', FReg32Base, FReg32Index, Item.Values);
              end;
            end
            else if (AnsiSameText(sl_Operand, 'XMMRM8')) or
                    (AnsiSameText(sl_Operand, 'XMMRM8_M')) or
                    (AnsiSameText(sl_Operand, 'XMMRM8_MZ')) or
                    (AnsiSameText(sl_Operand, 'XMMRM8_ER')) or
                    (AnsiSameText(sl_Operand, 'XMMRM8_SAE')) then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otXMMRM8;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'byte ';

              sSuffix := '';
              if Pos('_MZ', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1} {z}'
               else if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';

              if Pos('_ER', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ', {rd-sae}'
               else if FSAE and (Pos('_SAE', AnsiUppercase(sl_Operand)) > 0) then sSuffix := ', {sae}';

              Item.Values.Add('XMM0' + sSuffix);
              Item.Values.Add('XMM1' + sSuffix);
              Item.Values.Add('XMM2' + sSuffix);
              Item.Values.Add('XMM3' + sSuffix);
              Item.Values.Add('XMM4' + sSuffix);
              Item.Values.Add('XMM5' + sSuffix);
              Item.Values.Add('XMM6' + sSuffix);
              Item.Values.Add('XMM7' + sSuffix);

              if x64 then
              begin
                Item.Values.Clear;
                if FAVX512 then
                begin
                  Item.Values.Add('XMM0' + sSuffix);
                  Item.Values.Add('XMM0');
                  Item.Values.Add('XMM9' + sSuffix);
                  Item.Values.Add('XMM9');
                  Item.Values.Add('XMM18' + sSuffix);
                  Item.Values.Add('XMM18');
                  Item.Values.Add('XMM27' + sSuffix);
                  Item.Values.Add('XMM27');
                  Item.Values.Add('XMM31' + sSuffix);
                  Item.Values.Add('XMM31');
                end
                else
                begin
                  Item.Values.Add('XMM0' + sSuffix);
                  Item.Values.Add('XMM0');
                  Item.Values.Add('XMM4' + sSuffix);
                  Item.Values.Add('XMM4');
                  Item.Values.Add('XMM8' + sSuffix);
                  Item.Values.Add('XMM8');
                  Item.Values.Add('XMM12' + sSuffix);
                  Item.Values.Add('XMM12');
                  Item.Values.Add('XMM15' + sSuffix);
                  Item.Values.Add('XMM15');
                end;

                //Item.Values.Add('[RIP]');
                //Item.Values.Add('[RIP + 16]');

                MemRegBaseIndexCombi(sl_Prefix, '', FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else
              begin
                MemRegBaseIndexCombi(sl_Prefix, '', FReg32Base, FReg32Index, Item.Values);
              end;
            end
            else if (AnsiSameText(sl_Operand, 'XMMRM16')) or
                    (AnsiSameText(sl_Operand, 'XMMRM16_M')) or
                    (AnsiSameText(sl_Operand, 'XMMRM16_MZ')) or
                    (AnsiSameText(sl_Operand, 'XMMRM16_ER')) or
                    (AnsiSameText(sl_Operand, 'XMMRM16_SAE'))
                    then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otXMMRM16;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'word ';

              sSuffix := '';
              if Pos('_MZ', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1} {z}'
               else if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';

              if Pos('_ER', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ', {rd-sae}'
               else if FSAE and (Pos('_SAE', AnsiUppercase(sl_Operand)) > 0) then sSuffix := ', {sae}';

              Item.Values.Add('XMM0' + sSuffix);
              Item.Values.Add('XMM1' + sSuffix);
              Item.Values.Add('XMM2' + sSuffix);
              Item.Values.Add('XMM3' + sSuffix);
              Item.Values.Add('XMM4' + sSuffix);
              Item.Values.Add('XMM5' + sSuffix);
              Item.Values.Add('XMM6' + sSuffix);
              Item.Values.Add('XMM7' + sSuffix);

              if x64 then
              begin
                Item.Values.Clear;
                if FAVX512 then
                begin
                  Item.Values.Add('XMM0' + sSuffix);
                  Item.Values.Add('XMM0');
                  Item.Values.Add('XMM9' + sSuffix);
                  Item.Values.Add('XMM9');
                  Item.Values.Add('XMM18' + sSuffix);
                  Item.Values.Add('XMM18');
                  Item.Values.Add('XMM27' + sSuffix);
                  Item.Values.Add('XMM27');
                  Item.Values.Add('XMM31' + sSuffix);
                  Item.Values.Add('XMM31');
                end
                else
                begin
                  Item.Values.Add('XMM0' + sSuffix);
                  Item.Values.Add('XMM0');
                  Item.Values.Add('XMM4' + sSuffix);
                  Item.Values.Add('XMM4');
                  Item.Values.Add('XMM8' + sSuffix);
                  Item.Values.Add('XMM8');
                  Item.Values.Add('XMM12' + sSuffix);
                  Item.Values.Add('XMM12');
                  Item.Values.Add('XMM15' + sSuffix);
                  Item.Values.Add('XMM15');
                end;

                //Item.Values.Add('[RIP]');
                //Item.Values.Add('[RIP + 16]');

                MemRegBaseIndexCombi(sl_prefix, '', FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else
              begin
                MemRegBaseIndexCombi(sl_prefix, '', FReg32Base, FReg32Index, Item.Values);
              end;
            end
            else if (AnsiSameText(sl_Operand, 'YMMREG')) or
                    (AnsiSameText(sl_Operand, 'YMMREG_M')) or
                    (AnsiSameText(sl_Operand, 'YMMREG_MZ')) or
                    (AnsiSameText(sl_Operand, 'YMMREG_ER')) or
                    (AnsiSameText(sl_Operand, 'YMMREG_SAE'))
                    then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otYMMReg;
              Item.OpActive := true;

              sSuffix := '';
              if Pos('_MZ', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1} {z}'
               else if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';

              if Pos('_ER', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ', {rd-sae}'
               else if FSAE and (Pos('_SAE', AnsiUppercase(sl_Operand)) > 0) then sSuffix := ', {sae}';

              Item.Values.Add('YMM0' + sSuffix);
              Item.Values.Add('YMM1' + sSuffix);
              Item.Values.Add('YMM2' + sSuffix);
              Item.Values.Add('YMM3' + sSuffix);
              Item.Values.Add('YMM4' + sSuffix);
              Item.Values.Add('YMM5' + sSuffix);
              Item.Values.Add('YMM6' + sSuffix);
              Item.Values.Add('YMM7' + sSuffix);

              if x64 then
              begin
                Item.Values.Clear;
                if FAVX512 then
                begin
                  Item.Values.Add('YMM0' + sSuffix);
                  Item.Values.Add('YMM9' + sSuffix);
                  Item.Values.Add('YMM18' + sSuffix);
                  Item.Values.Add('YMM27' + sSuffix);
                  Item.Values.Add('YMM31' + sSuffix);
                  
                  if (sSuffix <> '') and
		     (MaskRegNeeded = false)  then
                  begin
                    Item.Values.Add('YMM0');
                    Item.Values.Add('YMM9');
                    Item.Values.Add('YMM18');
                    Item.Values.Add('YMM27');
                    Item.Values.Add('YMM31');                      
                  end;
                end
                else
                begin
                  Item.Values.Add('YMM0' + sSuffix);
                  Item.Values.Add('YMM4' + sSuffix);
                  Item.Values.Add('YMM8' + sSuffix);
                  Item.Values.Add('YMM12' + sSuffix);
                  Item.Values.Add('YMM15' + sSuffix);

                  if (sSuffix <> '') and
		     (MaskRegNeeded = false)  then
                  begin
                    Item.Values.Add('YMM0');
                    Item.Values.Add('YMM4');
                    Item.Values.Add('YMM8');
                    Item.Values.Add('YMM12');
                    Item.Values.Add('YMM15');
                  end;    
                end;
              end;
            end
            else if (AnsiSameText(sl_Operand, 'YMMRM'))  or
                    (AnsiSameText(sl_Operand, 'YMMRM_M')) or
                    (AnsiSameText(sl_Operand, 'YMMRM_MZ')) or
                    (AnsiSameText(sl_Operand, 'YMMRM_ER')) or
                    (AnsiSameText(sl_Operand, 'YMMRM_SAE'))
                    then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otYMMRM;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'yword ';

              sSuffix := '';
              if Pos('_MZ', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1} {z}'
               else if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';

              if Pos('_ER', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ', {rd-sae}'
               else if FSAE and (Pos('_SAE', AnsiUppercase(sl_Operand)) > 0) then sSuffix := ', {sae}';

              Item.Values.Add('YMM0' + sSuffix);
              Item.Values.Add('YMM1' + sSuffix);
              Item.Values.Add('YMM2' + sSuffix);
              Item.Values.Add('YMM3' + sSuffix);
              Item.Values.Add('YMM4' + sSuffix);
              Item.Values.Add('YMM5' + sSuffix);
              Item.Values.Add('YMM6' + sSuffix);
              Item.Values.Add('YMM7' + sSuffix);

              if x64 then
              begin
                Item.Values.Clear;
                if FAVX512 then
                begin
                  Item.Values.Add('YMM0' + sSuffix);
                  Item.Values.Add('YMM9' + sSuffix);
                  Item.Values.Add('YMM18' + sSuffix);
                  Item.Values.Add('YMM27' +  sSuffix);
                  Item.Values.Add('YMM31' + sSuffix);

                  if (sSuffix <> '') and
		     (MaskRegNeeded = false)  then
                  begin
                    Item.Values.Add('YMM0');
                    Item.Values.Add('YMM9');
                    Item.Values.Add('YMM18');
                    Item.Values.Add('YMM27');
                    Item.Values.Add('YMM31');
                  end;    
                end
                else
                begin
                  Item.Values.Add('YMM0' + sSuffix);
                  Item.Values.Add('YMM4' + sSuffix);
                  Item.Values.Add('YMM8' + sSuffix);
                  Item.Values.Add('YMM12' + sSuffix);
                  Item.Values.Add('YMM15' + sSuffix);

                  if (sSuffix <> '') and
		     (MaskRegNeeded = false)  then
                  begin
                    Item.Values.Add('YMM0');
                    Item.Values.Add('YMM4');
                    Item.Values.Add('YMM8');
                    Item.Values.Add('YMM12');
                    Item.Values.Add('YMM15');
                  end;    
                end;

                MemRegBaseIndexCombi(sl_prefix, '', FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else
              begin
                MemRegBaseIndexCombi(sl_Prefix, '', FReg32Base, FReg32Index, Item.Values);
              end;
            end
            else if (AnsiSameText(sl_Operand, 'ZMMREG')) or
                    (AnsiSameText(sl_Operand, 'ZMMREG_M')) or
                    (AnsiSameText(sl_Operand, 'ZMMREG_MZ')) or
                    (AnsiSameText(sl_Operand, 'ZMMREG_ER')) or
                    (AnsiSameText(sl_Operand, 'ZMMREG_SAE'))
                    then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otZMMReg;
              Item.OpActive := true;

              sSuffix := '';
              if Pos('_MZ', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1} {z}'
               else if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';

              if Pos('_ER', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ', {rd-sae}'
               else if FSAE and (Pos('_SAE', AnsiUppercase(sl_Operand)) > 0) then sSuffix := ', {sae}';

              Item.Values.Add('ZMM0' + sSuffix);
              Item.Values.Add('ZMM1' + sSuffix);
              Item.Values.Add('ZMM2' + sSuffix);
              Item.Values.Add('ZMM3' + sSuffix);
              Item.Values.Add('ZMM4' + sSuffix);
              Item.Values.Add('ZMM5' + sSuffix);
              Item.Values.Add('ZMM6' + sSuffix);
              Item.Values.Add('ZMM7' + sSuffix);

              if x64 then
              begin
                Item.Values.Clear;
                if FAVX512 then
                begin
                  Item.Values.Add('ZMM0' + sSuffix);
                  Item.Values.Add('ZMM9' + sSuffix);
                  Item.Values.Add('ZMM18' + sSuffix);
                  Item.Values.Add('ZMM27' + sSuffix);
                  Item.Values.Add('ZMM31' + sSuffix);

                  if (sSuffix <> '') and
		     (MaskRegNeeded = false)  then
                  begin
                    Item.Values.Add('ZMM0');
                    Item.Values.Add('ZMM9');
                    Item.Values.Add('ZMM18');
                    Item.Values.Add('ZMM27');
                    Item.Values.Add('ZMM31');
                  end;    
                end
                else
                begin
                  Item.Values.Add('ZMM0' + sSuffix);
                  Item.Values.Add('ZMM4' + sSuffix);
                  Item.Values.Add('ZMM8' + sSuffix);
                  Item.Values.Add('ZMM12' + sSuffix);
                  Item.Values.Add('ZMM15' + sSuffix);
                end;
              end;
            end
            else if (AnsiSameText(sl_Operand, 'ZMMRM')) or
                    (AnsiSameText(sl_Operand, 'ZMMRM_M')) or
                    (AnsiSameText(sl_Operand, 'ZMMRM_MZ')) or
                    (AnsiSameText(sl_Operand, 'XMMRM_ER')) or
                    (AnsiSameText(sl_Operand, 'XMMRM_SAE'))
                    then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otZMMRM;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'zword ';

              sSuffix := '';
              if Pos('_MZ', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1} {z}'
               else if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';

              if Pos('_ER', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ', {rd-sae}'
               else if FSAE and (Pos('_SAE', AnsiUppercase(sl_Operand)) > 0) then sSuffix := ', {sae}';

              Item.Values.Add('ZMM0' + sSuffix);
              Item.Values.Add('ZMM1' + sSuffix);
              Item.Values.Add('ZMM2' + sSuffix);
              Item.Values.Add('ZMM3' + sSuffix);
              Item.Values.Add('ZMM4' + sSuffix);
              Item.Values.Add('ZMM5' + sSuffix);
              Item.Values.Add('ZMM6' + sSuffix);
              Item.Values.Add('ZMM7' + sSuffix);

              if x64 then
              begin
                Item.Values.Clear;
                if FAVX512 then
                begin
                  Item.Values.Add('ZMM0' + sSuffix);
                  Item.Values.Add('ZMM9' + sSuffix);
                  Item.Values.Add('ZMM18' + sSuffix);
                  Item.Values.Add('ZMM27' + sSuffix);
                  Item.Values.Add('ZMM31' + sSuffix);

                  if (sSuffix <> '') and
		     (MaskRegNeeded = false)  then
                  begin
                    Item.Values.Add('ZMM0');
                    Item.Values.Add('ZMM9');
                    Item.Values.Add('ZMM18');
                    Item.Values.Add('ZMM27');
                    Item.Values.Add('ZMM31');
                  end;    
                end
                else
                begin
                  Item.Values.Add('ZMM0' + sSuffix);
                  Item.Values.Add('ZMM4' + sSuffix);
                  Item.Values.Add('ZMM8' + sSuffix);
                  Item.Values.Add('ZMM12' + sSuffix);
                  Item.Values.Add('ZMM15' + sSuffix);
                end;

                MemRegBaseIndexCombi(sl_prefix, '', FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else
              begin
                MemRegBaseIndexCombi(sl_Prefix, '', FReg32Base, FReg32Index, Item.Values);
              end;
            end
            else if AnsiSameText(sl_Operand, 'MEM8') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otMEM8;
              Item.OpActive := true;



	      if UsePrefix then sl_Prefix := 'byte ';

              
              sSuffix := '';
	      
	      if x64 then
              begin
                MemRegBaseIndexCombi(sl_Prefix, '', FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombi(sl_prefix, '', FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, 'MEM16') or
                    AnsiSameText(sl_Operand, 'MEM16_M') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otMEM16;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'word ';

              sSuffix := '';
              if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';

              if x64 then
              begin
                MemRegBaseIndexCombi(sl_Prefix, sSuffix, FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombi(sl_Prefix, sSuffix, FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, 'MEM32') or
                    AnsiSameText(sl_Operand, 'MEM32_M') or
                    AnsiSameText(sl_Operand, 'MEM32_MZ') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otMEM32;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'dword ';

              sSuffix := '';
              
	      if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';
              if Pos('_MZ', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1} {z}';


              if x64 then
              begin
                MemRegBaseIndexCombi(sl_prefix, sSuffix, FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombi(sl_prefix, sSuffix, FReg32Base, FReg32Index, Item.Values);
            end
            else if (AnsiSameText(sl_Operand, 'MEM64')) or
                    (AnsiSameText(sl_Operand, 'MEM64_M')) or
                    (AnsiSameText(sl_Operand, 'MEM64_MZ')) then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otMEM64;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'qword ';

              sSuffix := '';
              if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';
              if Pos('_MZ', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1} {z}';

              if x64 then
              begin
                MemRegBaseIndexCombi(sl_Prefix, sSuffix, FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombi(sl_prefix, sSuffix, FReg32Base, FReg32Index, Item.Values);
            end
            else if (AnsiSameText(sl_Operand, 'MEM128')) or
                    (AnsiSameText(sl_Operand, 'MEM128_M')) or
                    (AnsiSameText(sl_Operand, 'MEM128_MZ')) then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otMEM128;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'oword ';

              sSuffix := '';
              if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';
              if Pos('_MZ', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1} {z}';

              if x64 then
              begin
                MemRegBaseIndexCombi(sl_prefix, sSuffix, FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombi(sl_prefix, sSuffix, FReg32Base, FReg32Index, Item.Values);
            end
            else if (AnsiSameText(sl_Operand, 'MEM256')) or
                    (AnsiSameText(sl_Operand, 'MEM256_M')) or
                    (AnsiSameText(sl_Operand, 'MEM256_MZ')) then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otMEM256;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'yword ';

              sSuffix := '';
              if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';
              if Pos('_MZ', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1} {z}';


              if x64 then
              begin
                MemRegBaseIndexCombi(sl_prefix, sSuffix, FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombi(sl_prefix, sSuffix, FReg32Base, FReg32Index, Item.Values);
            end
            else if (AnsiSameText(sl_Operand, 'MEM512')) or
                    (AnsiSameText(sl_Operand, 'MEM512_M')) or
                    (AnsiSameText(sl_Operand, 'MEM512_MZ')) then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otMEM512;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'zword ';

              sSuffix := '';
              if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';
              if Pos('_MZ', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1} {z}';


              if x64 then
              begin
                MemRegBaseIndexCombi(sl_prefix, sSuffix, FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombi(sl_prefix, sSuffix, FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, 'REG8') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otREG8;
              Item.OpActive := true;

              if x64 then
              begin
                Item.Values.AddStrings(FReg8);
              end
              else Item.Values.AddStrings(FReg8);
            end
            else if AnsiSameText(sl_Operand, 'REG16') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otREG16;
              Item.OpActive := true;

              if x64 then
              begin
                Item.Values.AddStrings(FReg16);
              end
              else Item.Values.AddStrings(FReg16);
            end
            else if AnsiSameText(sl_Operand, 'REG32') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otREG32;
              Item.OpActive := true;

              if x64 then
              begin
                Item.Values.AddStrings(FReg32Base);
              end
              else Item.Values.AddStrings(FReg32Base);
            end
            else if AnsiSameText(sl_Operand, 'REG64') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otREG64;
              Item.OpActive := true;

              if x64 then
              begin
                Item.Values.AddStrings(FReg64Base);
              end;
            end
            else if AnsiSameText(sl_Operand, 'RM32') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otRM32;
              Item.OpActive := true;

              Item.Values.AddStrings(FReg32Base);

              if UsePrefix then sl_Prefix := 'dword ';

              if x64 then
              begin
                MemRegBaseIndexCombi(sl_Prefix, '', FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombi(sl_prefix, '', FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, 'RM64') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otRM32;
              Item.OpActive := true;



              if UsePrefix then sl_Prefix := 'qword ';

              if x64 then
              begin
                Item.Values.AddStrings(FReg64Base);
                MemRegBaseIndexCombi(sl_Prefix, '', FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombi(sl_prefix, '', FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, 'IMM8') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otIMM8;
              Item.OpActive := true;

              Item.Values.Add('0');
            end
            else if AnsiSameText(sl_Operand, 'XMEM32') or
                    AnsiSameText(sl_Operand, 'XMEM32_M') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otXMEM32;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'oword ';

              sSuffix := '';
              if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';

              if x64 then
              begin
		VectorMemRegBaseIndexCombi(sl_prefix, sSuffix, FReg64Base, FReg64XMMIndex, Item.Values);
		if (sSuffix <> '') and
                   (MaskRegNeeded = false) then
 		 VectorMemRegBaseIndexCombi(sl_prefix, '', FReg64Base, FReg64XMMIndex, Item.Values);
              end
              else
              begin
                VectorMemRegBaseIndexCombi(sl_prefix, sSuffix, FReg32Base, FReg32XMMIndex, Item.Values);
		if (sSuffix <> '') and
                   (MaskRegNeeded = false) then
                 VectorMemRegBaseIndexCombi(sl_prefix, '', FReg32Base, FReg32XMMIndex, Item.Values);
              end;
            end
            else if AnsiSameText(sl_Operand, 'XMEM64') or
                    AnsiSameText(sl_Operand, 'XMEM64_M') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otXMEM64;
              Item.OpActive := true;

              //if UsePrefix then sl_Prefix := 'oword ';
              //
              //if x64 then
              //begin
              //  VectorMemRegBaseIndexCombi(sl_prefix, FReg64Base, FReg64XMMIndex, Item.Values);
              //end
              //else
              //begin
              //  VectorMemRegBaseIndexCombi(sl_prefix, FReg32Base, FReg32XMMIndex, Item.Values);
              //end;

              sSuffix := '';
              if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';

              if x64 then
              begin
                VectorMemRegBaseIndexCombi(sl_prefix, sSuffix, FReg64Base, FReg64XMMIndex, Item.Values);
                if (sSuffix <> '') and
                   (MaskRegNeeded = false)
                        then
 		 VectorMemRegBaseIndexCombi(sl_prefix, '', FReg64Base, FReg64XMMIndex, Item.Values);
              end
              else
              begin
                VectorMemRegBaseIndexCombi(sl_prefix, sSuffix, FReg32Base, FReg32XMMIndex, Item.Values);
                if (sSuffix <> '') and
                   (MaskRegNeeded = false)
                        then
 		 VectorMemRegBaseIndexCombi(sl_prefix, '', FReg32Base, FReg32XMMIndex, Item.Values);
              end;

            end
            else if AnsiSameText(sl_Operand, 'YMEM32') or
                    AnsiSameText(sl_Operand, 'YMEM32_M') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otYMEM32;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'yword ';

              //if x64 then
              //begin
              //  VectorMemRegBaseIndexCombi(sl_prefix, FReg64Base, FReg64YMMIndex, Item.Values);
              //end
              //else
              //begin
              //  VectorMemRegBaseIndexCombi(sl_prefix, FReg32Base, FReg32YMMIndex, Item.Values);
              //end;

              sSuffix := '';
              if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';

              if x64 then
              begin
                VectorMemRegBaseIndexCombi(sl_prefix, sSuffix, FReg64Base, FReg64YMMIndex, Item.Values);
                if (sSuffix <> '') and
                   (MaskRegNeeded = false)
                        then
		 VectorMemRegBaseIndexCombi(sl_prefix, '', FReg64Base, FReg64YMMIndex, Item.Values);
              end
              else
              begin
                VectorMemRegBaseIndexCombi(sl_prefix, sSuffix, FReg32Base, FReg32YMMIndex, Item.Values);
                if (sSuffix <> '') and
                   (MaskRegNeeded = false)
                        then
		 VectorMemRegBaseIndexCombi(sl_prefix, '', FReg32Base, FReg32YMMIndex, Item.Values);
              end;

            end
            else if AnsiSameText(sl_Operand, 'YMEM64') or
                    AnsiSameText(sl_Operand, 'YMEM64_M') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otYMEM64;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'yword ';

              //if x64 then
              //begin
              //  VectorMemRegBaseIndexCombi(sl_prefix, FReg64Base, FReg64YMMIndex, Item.Values);
              //end
              //else
              //begin
              //  VectorMemRegBaseIndexCombi(sl_prefix, FReg32Base, FReg32YMMIndex, Item.Values);
              //end;

              sSuffix := '';
              if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';

              if x64 then
              begin
                VectorMemRegBaseIndexCombi(sl_prefix, sSuffix, FReg64Base, FReg64YMMIndex, Item.Values);
		if (sSuffix <> '') and
                   (MaskRegNeeded = false)
                        then
                 VectorMemRegBaseIndexCombi(sl_prefix, '', FReg64Base, FReg64YMMIndex, Item.Values);
              end
              else
              begin
                VectorMemRegBaseIndexCombi(sl_prefix, sSuffix, FReg32Base, FReg32YMMIndex, Item.Values);
                if (sSuffix <> '') and
                   (MaskRegNeeded = false)
                        then
		 VectorMemRegBaseIndexCombi(sl_prefix, '', FReg32Base, FReg32YMMIndex, Item.Values);
              end;

            end
            else if AnsiSameText(sl_Operand, 'ZMEM32') or
                    AnsiSameText(sl_Operand, 'ZMEM32_M')  then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otZMEM32;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'zword ';

              //if x64 then
              //begin
              //  VectorMemRegBaseIndexCombi(sl_prefix, FReg64Base, FReg64ZMMIndex, Item.Values);
              //end
              //else
              //begin
              //  VectorMemRegBaseIndexCombi(sl_prefix, FReg32Base, FReg32ZMMIndex, Item.Values);
              //end;

              sSuffix := '';
              if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';

              if x64 then
              begin
                VectorMemRegBaseIndexCombi(sl_prefix, sSuffix, FReg64Base, FReg64ZMMIndex, Item.Values);
		if (sSuffix <> '') and
                   (MaskRegNeeded = false)
                        then
                 VectorMemRegBaseIndexCombi(sl_prefix, '', FReg64Base, FReg64ZMMIndex, Item.Values);
              end
              else
              begin
                VectorMemRegBaseIndexCombi(sl_prefix, sSuffix, FReg32Base, FReg32ZMMIndex, Item.Values);
		if (sSuffix <> '') and
                   (MaskRegNeeded = false)
                        then
                 VectorMemRegBaseIndexCombi(sl_prefix, '', FReg32Base, FReg32ZMMIndex, Item.Values);
              end;

            end
            else if AnsiSameText(sl_Operand, 'ZMEM64') or
                    AnsiSameText(sl_Operand, 'ZMEM64_M') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otZMEM64;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'zword ';

              //if x64 then
              //begin
              //  VectorMemRegBaseIndexCombi(sl_prefix, FReg64Base, FReg64ZMMIndex, Item.Values);
              //end
              //else
              //begin
              //  VectorMemRegBaseIndexCombi(sl_prefix, FReg32Base, FReg32ZMMIndex, Item.Values);
              //end;

              sSuffix := '';
              if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';

              if x64 then
              begin
                VectorMemRegBaseIndexCombi(sl_prefix, sSuffix, FReg64Base, FReg64ZMMIndex, Item.Values);
		if (sSuffix <> '') and
                   (MaskRegNeeded = false)
                        then
                 VectorMemRegBaseIndexCombi(sl_prefix, '', FReg64Base, FReg64ZMMIndex, Item.Values);
              end
              else
              begin
                VectorMemRegBaseIndexCombi(sl_prefix, sSuffix, FReg32Base, FReg32ZMMIndex, Item.Values);
                if (sSuffix <> '') and
                   (MaskRegNeeded = false)
                        then
		 VectorMemRegBaseIndexCombi(sl_prefix, '', FReg32Base, FReg32ZMMIndex, Item.Values);
              end;

            end
            else if AnsiSameText(sl_Operand, '2B32') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otB32;
              Item.OpActive := true;


              if x64 then
              begin
                MemRegBaseIndexCombi(sl_Prefix, ' {1to2}',  FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombi(sl_prefix, ' {1to2}', FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, '4B32') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otB32;
              Item.OpActive := true;


              if x64 then
              begin
                MemRegBaseIndexCombi(sl_Prefix, ' {1to4}',  FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombi(sl_prefix, ' {1to4}', FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, '8B32') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otB32;
              Item.OpActive := true;


              if x64 then
              begin
                MemRegBaseIndexCombi(sl_Prefix, ' {1to8}',  FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombi(sl_prefix, ' {1to8}', FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, '16B32') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otB32;
              Item.OpActive := true;


              if x64 then
              begin
                MemRegBaseIndexCombi(sl_Prefix, ' {1to16}',  FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombi(sl_prefix, ' {1to16}', FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, '2B64') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otB64;
              Item.OpActive := true;


              if x64 then
              begin
                MemRegBaseIndexCombi(sl_Prefix, ' {1to2}',  FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombi(sl_prefix, ' {1to2}', FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, '4B64') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otB64;
              Item.OpActive := true;


              if x64 then
              begin
                MemRegBaseIndexCombi(sl_Prefix, ' {1to4}',  FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombi(sl_prefix, ' {1to4}', FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, '8B64') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otB64;
              Item.OpActive := true;


              if x64 then
              begin
                MemRegBaseIndexCombi(sl_Prefix, ' {1to8}',  FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombi(sl_prefix, ' {1to8}', FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, '16B64') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otB64;
              Item.OpActive := true;


              if x64 then
              begin
                MemRegBaseIndexCombi(sl_Prefix, ' {1to16}',  FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombi(sl_prefix, ' {1to16}', FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, 'KREG') or
                    AnsiSameText(sl_Operand, 'KREG_M') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otKREG;
              Item.OpActive := true;

              sSuffix := '';
              if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';

              if UsePrefix then sl_Prefix := '';

              for i := 0 to FRegKREG.Count - 1 do
               Item.Values.Add(FRegKREG[i] + sSuffix);
            end
            else if trim(sl_Operand) = '' then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otUnknown;
              Item.OpActive := false;

              Item.Values.Add('');
            end
            else
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otUnknown;
              Item.OpActive := false;

              Item.Values.Add('?' + sl_Operand);
            end

          end;

          sl_RegCombi := '';


          il_Operands := 0;
          UsedParams  := 0;

          if OItem1.OpActive then
          begin
            inc(il_Operands);
            UsedParams := UsedParams or 1;
          end;

          if OItem2.OpActive then
          begin
            inc(il_Operands);
            UsedParams := UsedParams or 2;
          end;

          if OItem3.OpActive then
          begin
            inc(il_Operands);
            UsedParams := UsedParams or 4;
          end;

          if OItem4.OpActive then
          begin
            inc(il_Operands);
            UsedParams := UsedParams or 8;
          end;

          case il_Operands of
              1: UseDefault := UsedParams <> 1;
              2: UseDefault := UsedParams <> 3;
              3: UseDefault := UsedParams <> 7;
              4: UseDefault := UsedParams <> 15;
            else UseDefault := true;
          end;

          //UseDefault := true;

          if UseDefault then
          begin
            sl_Inst := format('%-20s', [aInst]);

            for il_Op1 := 0 to OItem1.Values.Count - 1 do
            begin
              for il_Op2 := 0 to OItem2.Values.Count - 1 do
              begin
                for il_Op3 := 0 to OItem3.Values.Count - 1 do
                begin
                  for il_Op4 := 0 to OItem4.Values.Count - 1 do
                  begin
                    sl_RegCombi := '';

                    if OItem1.OpActive then
                    begin
                      if sl_RegCombi <> '' then sl_RegCombi := sl_RegCombi + ', ';
                      sl_RegCombi := sl_RegCombi + OItem1.Values[il_Op1];
                    end;

                    if OItem2.OpActive then
                    begin
                      if sl_RegCombi <> '' then sl_RegCombi := sl_RegCombi + ', ';
                      sl_RegCombi := sl_RegCombi + OItem2.Values[il_Op2];
                    end;

                    if OItem3.OpActive then
                    begin
                      if sl_RegCombi <> '' then sl_RegCombi := sl_RegCombi + ', ';
                      sl_RegCombi := sl_RegCombi + OItem3.Values[il_Op3];
                    end;

                    if OItem4.OpActive then
                    begin
                      if sl_RegCombi <> '' then sl_RegCombi := sl_RegCombi + ', ';
                      sl_RegCombi := sl_RegCombi + OItem4.Values[il_Op4];
                    end;

                    if sl_RegCombi <> '' then
                    begin
                      //result.Add(format('%-20s%s', [aInst, sl_RegCombi]));
                      result.Add(sl_Inst + sl_RegCombi);
                      sl_RegCombi := '';
                    end;
                  end;
                end;
              end;
            end;
          end
          else
          begin
            sl_Inst := format('%-20s', [aInst]);

            for il_Op1 := 0 to OItem1.Values.Count - 1 do
            begin
              if OItem1.OpActive then
              begin
                sl_RegCombi1 := OItem1.Values[il_Op1];
              end
              else sl_RegCombi1 := '';

              for il_Op2 := 0 to OItem2.Values.Count - 1 do
              begin
                if OItem2.OpActive then
                begin
                  sl_RegCombi2 := sl_RegCombi1 + ', ' + OItem2.Values[il_Op2];
                end
                else sl_RegCombi2 := sl_RegCombi1;

                for il_Op3 := 0 to OItem3.Values.Count - 1 do
                begin
                  if OItem3.OpActive then
                  begin
                    sl_RegCombi3 := sl_RegCombi2 + ', ' + OItem3.Values[il_Op3];
                  end
                  else sl_RegCombi3 := sl_RegCombi2;

                  for il_Op4 := 0 to OItem4.Values.Count - 1 do
                  begin
                    if OItem4.OpActive then
                    begin
                      sl_RegCombi := sl_RegCombi3 + ', ' + OItem4.Values[il_Op4];
                    end
                    else sl_RegCombi := sl_RegCombi3;

                    if sl_RegCombi <> '' then
                    begin
                      //result.Add(format('%-20s%s', [aInst, sl_RegCombi]));
                      result.Add(sl_Inst + sl_RegCombi);
                      sl_RegCombi := '';
                    end;
                  end;
                end;
              end;
            end;
          end;
        finally
          FreeAndNil(OItem4);
        end;
      finally
        FreeAndNil(OItem3);
      end;
    finally
      FreeAndNil(OItem2);
    end;
  finally
    FreeAndNil(OItem1);
  end;
end;


function TAsmTestGenerator.InternalCalcTestDataMREF(const aInst, aOp1, aOp2, aOp3,
  aOp4: String): TStringList;
var
  i: integer;
  Item: TOperandListItem;
  OItem1: TOperandListItem;
  OItem2: TOperandListItem;
  OItem3: TOperandListItem;
  OItem4: TOperandListItem;

  il_Op: integer;
  il_Op1: integer;
  il_Op2: integer;
  il_Op3: integer;
  il_Op4: integer;

  sSuffix: string;
  sl_Operand: String;
  sl_Inst   : String;
  sl_RegCombi: String;
  sl_Prefix: String;
  UsePrefix: boolean;
  il_Operands: integer;
  UsedParams: cardinal;
  UseDefault: boolean;
  sl_RegCombi1: string;
  sl_RegCombi2: string;
  sl_RegCombi3: string;

  function PrepareOperandTyp(const aTyp: String): String;
  begin
    result := aTyp;
    if copy(result, length(result), 1) = '*' then result := copy(result, 1, length(result) - 1);
    if result = 'XMMRM128' then result := 'XMMRM';
    if result = 'YMMRM256' then result := 'YMMRM';
  end;


begin
  result := TStringList.Create;

  OItem1 := TOperandListItem.Create;
  try
    OItem2 := TOperandListItem.Create;
    try
      OItem3 := TOperandListItem.Create;
      try
        OItem4 := TOperandListItem.Create;
        try

          UsePrefix := (UpperCase(aInst) = 'VCVTPD2DQ') OR
                       (UpperCase(aInst) = 'VCVTPD2PS') OR
                       (UpperCase(aInst) = 'VCVTSI2SD') OR
                       (UpperCase(aInst) = 'VCVTSI2SS') OR
                       (UpperCase(aInst) = 'VCVTTPD2DQ') or
                       (UpperCase(aInst) = 'VPMOVZXWQ') or
                       (UpperCase(aInst) = 'VCVTPD2UDQ') or
                       (UpperCase(aInst) = 'VCVTPD2UDQ') or
                       (UpperCase(aInst) = 'VCVTTPD2UDQ') or
                       (UpperCase(aInst) = 'VCVTUQQ2PS') or
                       (UpperCase(aInst) = 'VCVTQQ2PS') or
                       (UpperCase(aInst) = 'VCVTUSI2SD') or
                       (UpperCase(aInst) = 'VCVTUSI2SS') or
                       (UpperCase(aInst) = 'VFPCLASSPD') or
                       (UpperCase(aInst) = 'VFPCLASSPS') or
                       (UpperCase(aInst) = 'VCMPSS')

                       ;



          for il_Op := 1 to 4 do
          begin
            sl_Prefix := '';

            case il_Op of
              1: begin
                   Item := OItem1;
                   sl_Operand := aOp1;
                 end;
              2: begin
                   Item := OItem2;
                   sl_Operand := aOp2;
                 end;
              3: begin
                   Item := OItem3;
                   sl_Operand := aOp3;
                 end;
              4: begin
                   Item := OItem4;
                   sl_Operand := aOp4;
                 end;
            end;

            sl_Operand := PrepareOperandTyp(sl_Operand);

            if (AnsiSameText(sl_Operand, 'XMMREG')) or
               (AnsiSameText(sl_Operand, 'XMMREG_M')) or
               (AnsiSameText(sl_Operand, 'XMMREG_MZ')) or
               (AnsiSameText(sl_Operand, 'XMMREG_ER')) or
               (AnsiSameText(sl_Operand, 'XMMREG_SAE')) then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otXMMReg;
              Item.OpActive := true;

              Item.Values.Add('XMM0');
           end
            else if (AnsiSameText(sl_Operand, 'XMMRM')) or
                    (AnsiSameText(sl_Operand, 'XMMRM_M')) or
                    (AnsiSameText(sl_Operand, 'XMMRM_MZ')) or
                    (AnsiSameText(sl_Operand, 'XMMRM_ER')) or
                    (AnsiSameText(sl_Operand, 'XMMRM_SAE')) then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otXMMRM;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'oword ';

              Item.Values.Add(' lOWord');
              Item.Values.Add(' gOWord');
              Item.Values.Add(' clOWord');
              Item.Values.Add(' cgOWord');

              Item.Values.Add(' oword lOWord');
              Item.Values.Add(' oword gOWord');
              Item.Values.Add(' oword clOWord');
              Item.Values.Add(' oword cgOWord');

              Item.Values.Add(' byte lOWord');
              Item.Values.Add(' byte gOWord');
              Item.Values.Add(' byte clOWord');
              Item.Values.Add(' byte cgOWord');

              Item.Values.Add(' lRec');
              Item.Values.Add(' gRec');

              Item.Values.Add(' oword lRec');
              Item.Values.Add(' oword gRec');

              Item.Values.Add(' oword lRec.rOWord');
              Item.Values.Add(' oword gRec.rOWord');

              Item.Values.Add(' lRec.rByte');
              Item.Values.Add(' gRec.rByte');

              Item.Values.Add(' lRec.rWord');
              Item.Values.Add(' gRec.rWord');

              Item.Values.Add(' lRec.rDWord');
              Item.Values.Add(' gRec.rDWord');

              Item.Values.Add(' lRec.rQWord');
              Item.Values.Add(' gRec.rQWord');

              Item.Values.Add(' lRec.rOWord');
              Item.Values.Add(' gRec.rOWord');

              Item.Values.Add(' lRec.rYWord');
              Item.Values.Add(' gRec.rYWord');

              Item.Values.Add(' lRec.rZWord');
              Item.Values.Add(' gRec.rZWord');

            end
            else if (AnsiSameText(sl_Operand, 'XMMRM8')) or
                    (AnsiSameText(sl_Operand, 'XMMRM8_M')) or
                    (AnsiSameText(sl_Operand, 'XMMRM8_MZ')) or
                    (AnsiSameText(sl_Operand, 'XMMRM8_ER')) or
                    (AnsiSameText(sl_Operand, 'XMMRM8_SAE')) then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otXMMRM8;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'byte ';

              Item.Values.Add('lbyte');
              Item.Values.Add('gbyte');
              Item.Values.Add('clbyte');
              Item.Values.Add('cgbyte');

              Item.Values.Add('byte lbyte');
              Item.Values.Add('byte gbyte');
              Item.Values.Add('byte clbyte');
              Item.Values.Add('byte cgbyte');

              Item.Values.Add(' lRec');
              Item.Values.Add(' gRec');

              Item.Values.Add(' byte lRec');
              Item.Values.Add(' byte gRec');

              Item.Values.Add(' lRec');
              Item.Values.Add(' gRec');

              Item.Values.Add(' lRec.rByte');
              Item.Values.Add(' gRec.rByte');

              Item.Values.Add(' lRec.rWord');
              Item.Values.Add(' gRec.rWord');

              Item.Values.Add(' lRec.rDWord');
              Item.Values.Add(' gRec.rDWord');

              Item.Values.Add(' lRec.rQWord');
              Item.Values.Add(' gRec.rQWord');

              Item.Values.Add(' lRec.rOWord');
              Item.Values.Add(' gRec.rOWord');

              Item.Values.Add(' lRec.rYWord');
              Item.Values.Add(' gRec.rYWord');

              Item.Values.Add(' lRec.rZWord');
              Item.Values.Add(' gRec.rZWord');

            end
            else if (AnsiSameText(sl_Operand, 'XMMRM16')) or
                    (AnsiSameText(sl_Operand, 'XMMRM16_M')) or
                    (AnsiSameText(sl_Operand, 'XMMRM16_MZ')) or
                    (AnsiSameText(sl_Operand, 'XMMRM16_ER')) or
                    (AnsiSameText(sl_Operand, 'XMMRM16_SAE'))
                    then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otXMMRM16;
              Item.OpActive := true;

              Item.Values.Add('lword');
              Item.Values.Add('gword');
              Item.Values.Add('clword');
              Item.Values.Add('cgword');

              Item.Values.Add('word lword');
              Item.Values.Add('word gword');
              Item.Values.Add('word clword');
              Item.Values.Add('word cgword');

              Item.Values.Add(' lRec');
              Item.Values.Add(' gRec');

              Item.Values.Add(' word lRec');
              Item.Values.Add(' word gRec');

              Item.Values.Add(' lRec.rByte');
              Item.Values.Add(' gRec.rByte');

              Item.Values.Add(' lRec.rWord');
              Item.Values.Add(' gRec.rWord');

              Item.Values.Add(' lRec.rDWord');
              Item.Values.Add(' gRec.rDWord');

              Item.Values.Add(' lRec.rQWord');
              Item.Values.Add(' gRec.rQWord');

              Item.Values.Add(' lRec.rOWord');
              Item.Values.Add(' gRec.rOWord');

              Item.Values.Add(' lRec.rYWord');
              Item.Values.Add(' gRec.rYWord');

              Item.Values.Add(' lRec.rZWord');
              Item.Values.Add(' gRec.rZWord');

            end
            else if (AnsiSameText(sl_Operand, 'YMMREG')) or
                    (AnsiSameText(sl_Operand, 'YMMREG_M')) or
                    (AnsiSameText(sl_Operand, 'YMMREG_MZ')) or
                    (AnsiSameText(sl_Operand, 'YMMREG_ER')) or
                    (AnsiSameText(sl_Operand, 'YMMREG_SAE'))
                    then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otYMMReg;
              Item.OpActive := true;

              Item.Values.Add('YMM0');
            end
            else if (AnsiSameText(sl_Operand, 'YMMRM'))  or
                    (AnsiSameText(sl_Operand, 'YMMRM_M')) or
                    (AnsiSameText(sl_Operand, 'YMMRM_MZ')) or
                    (AnsiSameText(sl_Operand, 'YMMRM_ER')) or
                    (AnsiSameText(sl_Operand, 'YMMRM_SAE'))
                    then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otYMMRM;
              Item.OpActive := true;

              Item.Values.Add('lYWord');
              Item.Values.Add('gYWord');
              Item.Values.Add('clYWord');
              Item.Values.Add('cgYWord');

              Item.Values.Add('yword lYWord');
              Item.Values.Add('yword gYWord');
              Item.Values.Add('yword clYWord');
              Item.Values.Add('yword cgYWord');

              Item.Values.Add(' lRec');
              Item.Values.Add(' gRec');

              Item.Values.Add(' yword lRec');
              Item.Values.Add(' yword gRec');

              Item.Values.Add(' lRec.rByte');
              Item.Values.Add(' gRec.rByte');

              Item.Values.Add(' lRec.rWord');
              Item.Values.Add(' gRec.rWord');

              Item.Values.Add(' lRec.rDWord');
              Item.Values.Add(' gRec.rDWord');

              Item.Values.Add(' lRec.rQWord');
              Item.Values.Add(' gRec.rQWord');

              Item.Values.Add(' lRec.rOWord');
              Item.Values.Add(' gRec.rOWord');

              Item.Values.Add(' lRec.rYWord');
              Item.Values.Add(' gRec.rYWord');

              Item.Values.Add(' lRec.rZWord');
              Item.Values.Add(' gRec.rZWord');

            end
            else if (AnsiSameText(sl_Operand, 'ZMMREG')) or
                    (AnsiSameText(sl_Operand, 'ZMMREG_M')) or
                    (AnsiSameText(sl_Operand, 'ZMMREG_MZ')) or
                    (AnsiSameText(sl_Operand, 'ZMMREG_ER')) or
                    (AnsiSameText(sl_Operand, 'ZMMREG_SAE'))
                    then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otZMMReg;
              Item.OpActive := true;

              Item.Values.Add('ZMM0');
            end
            else if (AnsiSameText(sl_Operand, 'ZMMRM')) or
                    (AnsiSameText(sl_Operand, 'ZMMRM_M')) or
                    (AnsiSameText(sl_Operand, 'ZMMRM_MZ')) or
                    (AnsiSameText(sl_Operand, 'XMMRM_ER')) or
                    (AnsiSameText(sl_Operand, 'XMMRM_SAE'))
                    then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otZMMRM;
              Item.OpActive := true;

              Item.Values.Add('lZWord');
              Item.Values.Add('gZWord');
              Item.Values.Add('clZWord');
              Item.Values.Add('cgZWord');

              Item.Values.Add('zword lZWord');
              Item.Values.Add('zword gZWord');
              Item.Values.Add('zword clZWord');
              Item.Values.Add('zword cgZWord');

              Item.Values.Add(' lRec');
              Item.Values.Add(' gRec');

              Item.Values.Add(' zword lRec');
              Item.Values.Add(' zword gRec');

              Item.Values.Add(' lRec.rByte');
              Item.Values.Add(' gRec.rByte');

              Item.Values.Add(' lRec.rWord');
              Item.Values.Add(' gRec.rWord');

              Item.Values.Add(' lRec.rDWord');
              Item.Values.Add(' gRec.rDWord');

              Item.Values.Add(' lRec.rQWord');
              Item.Values.Add(' gRec.rQWord');

              Item.Values.Add(' lRec.rOWord');
              Item.Values.Add(' gRec.rOWord');

              Item.Values.Add(' lRec.rYWord');
              Item.Values.Add(' gRec.rYWord');

              Item.Values.Add(' lRec.rZWord');
              Item.Values.Add(' gRec.rZWord');

            end
            else if AnsiSameText(sl_Operand, 'MEM8') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otMEM8;
              Item.OpActive := true;

              Item.Values.Add('lByte');
              Item.Values.Add('gByte');
              Item.Values.Add('clByte');
              Item.Values.Add('cgByte');

              Item.Values.Add('byte lByte');
              Item.Values.Add('byte gByte');
              Item.Values.Add('byte clByte');
              Item.Values.Add('byte cgByte');

              Item.Values.Add(' lRec');
              Item.Values.Add(' gRec');

              Item.Values.Add(' byte lRec');
              Item.Values.Add(' byte gRec');

              Item.Values.Add(' lRec.rByte');
              Item.Values.Add(' gRec.rByte');
            end
            else if AnsiSameText(sl_Operand, 'MEM16') or
                    AnsiSameText(sl_Operand, 'MEM16_M') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otMEM16;
              Item.OpActive := true;

              Item.Values.Add('lWord');
              Item.Values.Add('gWord');
              Item.Values.Add('clWord');
              Item.Values.Add('cgWord');

              Item.Values.Add('word lWord');
              Item.Values.Add('word gWord');
              Item.Values.Add('word clWord');
              Item.Values.Add('word cgWord');

              Item.Values.Add(' word lRec');
              Item.Values.Add(' word gRec');

              Item.Values.Add(' lRec.rWord');
              Item.Values.Add(' gRec.rWord');
            end
            else if AnsiSameText(sl_Operand, 'MEM32') or
                    AnsiSameText(sl_Operand, 'MEM32_M') or
                    AnsiSameText(sl_Operand, 'MEM32_MZ') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otMEM32;
              Item.OpActive := true;

              Item.Values.Add('lDWord');
              Item.Values.Add('gDWord');
              Item.Values.Add('clDWord');
              Item.Values.Add('cgDWord');

              Item.Values.Add('dword lDWord');
              Item.Values.Add('dword gDWord');
              Item.Values.Add('dword clDWord');
              Item.Values.Add('dword cgDWord');

              Item.Values.Add(' dword lRec');
              Item.Values.Add(' dword gRec');

              Item.Values.Add(' lRec.rDWord');
              Item.Values.Add(' gRec.rDWord');
            end
            else if (AnsiSameText(sl_Operand, 'MEM64')) or
                    (AnsiSameText(sl_Operand, 'MEM64_M')) or
                    (AnsiSameText(sl_Operand, 'MEM64_MZ')) then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otMEM64;
              Item.OpActive := true;

              Item.Values.Add('lQWord');
              Item.Values.Add('gQWord');
              Item.Values.Add('clQWord');
              Item.Values.Add('cgQWord');

              Item.Values.Add('qword lQWord');
              Item.Values.Add('qword gQWord');
              Item.Values.Add('qword clQWord');
              Item.Values.Add('qword cgQWord');

              Item.Values.Add(' qword lRec');
              Item.Values.Add(' qword gRec');

              Item.Values.Add(' lRec.rQWord');
              Item.Values.Add(' gRec.rQWord');
            end
            else if (AnsiSameText(sl_Operand, 'MEM128')) or
                    (AnsiSameText(sl_Operand, 'MEM128_M')) or
                    (AnsiSameText(sl_Operand, 'MEM128_MZ')) then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otMEM128;
              Item.OpActive := true;

              Item.Values.Add('lOWord');
              Item.Values.Add('gOWord');
              Item.Values.Add('clOWord');
              Item.Values.Add('cgOWord');

              Item.Values.Add('oword lOWord');
              Item.Values.Add('oword gOWord');
              Item.Values.Add('oword clOWord');
              Item.Values.Add('oword cgOWord');

              Item.Values.Add(' lRec');
              Item.Values.Add(' gRec');

              Item.Values.Add(' oword lRec');
              Item.Values.Add(' oword gRec');

              Item.Values.Add(' lRec.rByte');
              Item.Values.Add(' gRec.rByte');

              Item.Values.Add(' lRec.rWord');
              Item.Values.Add(' gRec.rWord');

              Item.Values.Add(' lRec.rDWord');
              Item.Values.Add(' gRec.rDWord');

              Item.Values.Add(' lRec.rQWord');
              Item.Values.Add(' gRec.rQWord');

              Item.Values.Add(' lRec.rOWord');
              Item.Values.Add(' gRec.rOWord');

              Item.Values.Add(' lRec.rYWord');
              Item.Values.Add(' gRec.rYWord');

              Item.Values.Add(' lRec.rZWord');
              Item.Values.Add(' gRec.rZWord');

            end
            else if (AnsiSameText(sl_Operand, 'MEM256')) or
                    (AnsiSameText(sl_Operand, 'MEM256_M')) or
                    (AnsiSameText(sl_Operand, 'MEM256_MZ')) then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otMEM256;
              Item.OpActive := true;

              Item.Values.Add('lYWord');
              Item.Values.Add('gYWord');
              Item.Values.Add('clYWord');
              Item.Values.Add('cgYWord');

              Item.Values.Add('yword lYWord');
              Item.Values.Add('yword gYWord');
              Item.Values.Add('yword clYWord');
              Item.Values.Add('yword cgYWord');

              Item.Values.Add(' lRec');
              Item.Values.Add(' gRec');

              Item.Values.Add(' yword lRec');
              Item.Values.Add(' yword gRec');

              Item.Values.Add(' lRec.rByte');
              Item.Values.Add(' gRec.rByte');

              Item.Values.Add(' lRec.rWord');
              Item.Values.Add(' gRec.rWord');

              Item.Values.Add(' lRec.rDWord');
              Item.Values.Add(' gRec.rDWord');

              Item.Values.Add(' lRec.rQWord');
              Item.Values.Add(' gRec.rQWord');

              Item.Values.Add(' lRec.rOWord');
              Item.Values.Add(' gRec.rOWord');

              Item.Values.Add(' lRec.rYWord');
              Item.Values.Add(' gRec.rYWord');

              Item.Values.Add(' lRec.rZWord');
              Item.Values.Add(' gRec.rZWord');

            end
            else if (AnsiSameText(sl_Operand, 'MEM512')) or
                    (AnsiSameText(sl_Operand, 'MEM512_M')) or
                    (AnsiSameText(sl_Operand, 'MEM512_MZ')) then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otMEM512;
              Item.OpActive := true;

              Item.Values.Add('lZWord');
              Item.Values.Add('gZWord');
              Item.Values.Add('clZWord');
              Item.Values.Add('cgZWord');

              Item.Values.Add('zword lZWord');
              Item.Values.Add('zword gZWord');
              Item.Values.Add('zword clZWord');
              Item.Values.Add('zword cgZWord');

              Item.Values.Add(' lRec');
              Item.Values.Add(' gRec');

              Item.Values.Add(' zword lRec');
              Item.Values.Add(' zword gRec');


            end
            else if AnsiSameText(sl_Operand, 'REG8') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otREG8;
              Item.OpActive := true;

              Item.Values.Add('al');
            end
            else if AnsiSameText(sl_Operand, 'REG16') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otREG16;
              Item.OpActive := true;

              Item.Values.Add('ax');
            end
            else if AnsiSameText(sl_Operand, 'REG32') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otREG32;
              Item.OpActive := true;

              Item.Values.Add('eax');
            end
            else if AnsiSameText(sl_Operand, 'REG64') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otREG64;
              Item.OpActive := true;

              Item.Values.Add('rax');
            end
            else if AnsiSameText(sl_Operand, 'RM32') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otRM32;
              Item.OpActive := true;

              Item.Values.Add('lDWord');
              Item.Values.Add('gDWord');
            end
            else if AnsiSameText(sl_Operand, 'RM64') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otRM64;
              Item.OpActive := true;

              Item.Values.Add('lQWord');
              Item.Values.Add('gQWord');
            end
            else if AnsiSameText(sl_Operand, 'IMM8') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otIMM8;
              Item.OpActive := true;

              Item.Values.Add('0');
            end
            else if AnsiSameText(sl_Operand, 'XMEM32') or
                    AnsiSameText(sl_Operand, 'XMEM32_M') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otXMEM32;
              Item.OpActive := true;
            end
            else if AnsiSameText(sl_Operand, 'XMEM64') or
                    AnsiSameText(sl_Operand, 'XMEM64_M') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otXMEM64;
              Item.OpActive := true;
            end
            else if AnsiSameText(sl_Operand, 'YMEM32') or
                    AnsiSameText(sl_Operand, 'YMEM32_M') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otYMEM32;
              Item.OpActive := true;
            end
            else if AnsiSameText(sl_Operand, 'YMEM64') or
                    AnsiSameText(sl_Operand, 'YMEM64_M') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otYMEM64;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'yword ';

            end
            else if AnsiSameText(sl_Operand, 'ZMEM32') or
                    AnsiSameText(sl_Operand, 'ZMEM32_M')  then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otZMEM32;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'zword ';

            end
            else if AnsiSameText(sl_Operand, 'ZMEM64') or
                    AnsiSameText(sl_Operand, 'ZMEM64_M') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otZMEM64;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'zword ';

            end
            else if AnsiSameText(sl_Operand, '2B32') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otB32;
              Item.OpActive := true;

              Item.Values.Add('lDWord {1to2}');
              Item.Values.Add('gDWord {1to2}');
            end
            else if AnsiSameText(sl_Operand, '4B32') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otB32;
              Item.OpActive := true;

              Item.Values.Add('lDWord {1to4}');
              Item.Values.Add('gDWord {1to4}');
            end
            else if AnsiSameText(sl_Operand, '8B32') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otB32;
              Item.OpActive := true;

              Item.Values.Add('lDWord {1to8}');
              Item.Values.Add('gDWord {1to8}');
            end
            else if AnsiSameText(sl_Operand, '16B32') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otB32;
              Item.OpActive := true;

              Item.Values.Add('lDWord {1to16}');
              Item.Values.Add('gDWord {1to16}');
           end
            else if AnsiSameText(sl_Operand, '2B64') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otB64;
              Item.OpActive := true;

              Item.Values.Add('lQWord {1to2}');
              Item.Values.Add('gQWord {1to2}');
            end
            else if AnsiSameText(sl_Operand, '4B64') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otB64;
              Item.OpActive := true;

              Item.Values.Add('lQWord {1to4}');
              Item.Values.Add('gQWord {1to4}');
            end
            else if AnsiSameText(sl_Operand, '8B64') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otB64;
              Item.OpActive := true;

              Item.Values.Add('lQWord {1to8}');
              Item.Values.Add('gQWord {1to8}');
            end
            else if AnsiSameText(sl_Operand, '16B64') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otB64;
              Item.OpActive := true;

              Item.Values.Add('lQWord {1to16}');
              Item.Values.Add('gQWord {1to16}');
           end
            else if AnsiSameText(sl_Operand, 'KREG') or
                    AnsiSameText(sl_Operand, 'KREG_M') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otKREG;
              Item.OpActive := true;

              Item.Values.Add('k1');
            end
            else if trim(sl_Operand) = '' then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otUnknown;
              Item.OpActive := false;

              Item.Values.Add('');
            end
            else
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otUnknown;
              Item.OpActive := false;

              Item.Values.Add('?' + sl_Operand);
            end

          end;

          sl_RegCombi := '';


          il_Operands := 0;
          UsedParams  := 0;

          if OItem1.OpActive then
          begin
            inc(il_Operands);
            UsedParams := UsedParams or 1;
          end;

          if OItem2.OpActive then
          begin
            inc(il_Operands);
            UsedParams := UsedParams or 2;
          end;

          if OItem3.OpActive then
          begin
            inc(il_Operands);
            UsedParams := UsedParams or 4;
          end;

          if OItem4.OpActive then
          begin
            inc(il_Operands);
            UsedParams := UsedParams or 8;
          end;

          case il_Operands of
              1: UseDefault := UsedParams <> 1;
              2: UseDefault := UsedParams <> 3;
              3: UseDefault := UsedParams <> 7;
              4: UseDefault := UsedParams <> 15;
            else UseDefault := true;
          end;

          //UseDefault := true;

          if UseDefault then
          begin
            sl_Inst := format('%-20s', [aInst]);

            for il_Op1 := 0 to OItem1.Values.Count - 1 do
            begin
              for il_Op2 := 0 to OItem2.Values.Count - 1 do
              begin
                for il_Op3 := 0 to OItem3.Values.Count - 1 do
                begin
                  for il_Op4 := 0 to OItem4.Values.Count - 1 do
                  begin
                    sl_RegCombi := '';

                    if OItem1.OpActive then
                    begin
                      if sl_RegCombi <> '' then sl_RegCombi := sl_RegCombi + ', ';
                      sl_RegCombi := sl_RegCombi + OItem1.Values[il_Op1];
                    end;

                    if OItem2.OpActive then
                    begin
                      if sl_RegCombi <> '' then sl_RegCombi := sl_RegCombi + ', ';
                      sl_RegCombi := sl_RegCombi + OItem2.Values[il_Op2];
                    end;

                    if OItem3.OpActive then
                    begin
                      if sl_RegCombi <> '' then sl_RegCombi := sl_RegCombi + ', ';
                      sl_RegCombi := sl_RegCombi + OItem3.Values[il_Op3];
                    end;

                    if OItem4.OpActive then
                    begin
                      if sl_RegCombi <> '' then sl_RegCombi := sl_RegCombi + ', ';
                      sl_RegCombi := sl_RegCombi + OItem4.Values[il_Op4];
                    end;

                    if sl_RegCombi <> '' then
                    begin
                      //result.Add(format('%-20s%s', [aInst, sl_RegCombi]));
                      result.Add(sl_Inst + sl_RegCombi);
                      sl_RegCombi := '';
                    end;
                  end;
                end;
              end;
            end;
          end
          else
          begin
            sl_Inst := format('%-20s', [aInst]);

            for il_Op1 := 0 to OItem1.Values.Count - 1 do
            begin
              if OItem1.OpActive then
              begin
                sl_RegCombi1 := OItem1.Values[il_Op1];
              end
              else sl_RegCombi1 := '';

              for il_Op2 := 0 to OItem2.Values.Count - 1 do
              begin
                if OItem2.OpActive then
                begin
                  sl_RegCombi2 := sl_RegCombi1 + ', ' + OItem2.Values[il_Op2];
                end
                else sl_RegCombi2 := sl_RegCombi1;

                for il_Op3 := 0 to OItem3.Values.Count - 1 do
                begin
                  if OItem3.OpActive then
                  begin
                    sl_RegCombi3 := sl_RegCombi2 + ', ' + OItem3.Values[il_Op3];
                  end
                  else sl_RegCombi3 := sl_RegCombi2;

                  for il_Op4 := 0 to OItem4.Values.Count - 1 do
                  begin
                    if OItem4.OpActive then
                    begin
                      sl_RegCombi := sl_RegCombi3 + ', ' + OItem4.Values[il_Op4];
                    end
                    else sl_RegCombi := sl_RegCombi3;

                    if sl_RegCombi <> '' then
                    begin
                      //result.Add(format('%-20s%s', [aInst, sl_RegCombi]));
                      result.Add(sl_Inst + sl_RegCombi);
                      sl_RegCombi := '';
                    end;
                  end;
                end;
              end;
            end;
          end;
        finally
          FreeAndNil(OItem4);
        end;
      finally
        FreeAndNil(OItem3);
      end;
    finally
      FreeAndNil(OItem2);
    end;
  finally
    FreeAndNil(OItem1);
  end;
end;

function TAsmTestGenerator.InternalCalcTestDataCDISP8(const aInst, aOp1, aOp2,
  aOp3, aOp4: String): TStringList;
var
  i: integer;
  Item: TOperandListItem;
  OItem1: TOperandListItem;
  OItem2: TOperandListItem;
  OItem3: TOperandListItem;
  OItem4: TOperandListItem;

  il_Op: integer;
  il_Op1: integer;
  il_Op2: integer;
  il_Op3: integer;
  il_Op4: integer;
  iAsmCounter: integer;

  sSuffix: string;
  sReg: string;
  sl_Operand: String;
  sl_Inst   : String;
  sRegCombi: String;
  sRegCombi1: String;
  sRegCombi2: String;
  sRegCombi3: String;
  sRegCombi4: String;
  sBaseReg  : String;
  sIndexReg : String;

  sl_Prefix: String;
  UsePrefix: boolean;
  il_Operands: integer;
  UsedParams: cardinal;
  UseDefault: boolean;
  sl_RegCombi1: string;
  sl_RegCombi2: string;
  sl_RegCombi3: string;
  sInstruction: string;
  sMREF: string;
  sLogMsg: string;
  MaskRegNeeded:boolean;
  slRegCombi: TStringList;
  OpMode: TOpMode;
  iOpNumMRef: integer;

  function PrepareOperandTyp(const aTyp: String): String;
  begin
    result := aTyp;
    if copy(result, length(result), 1) = '*' then result := copy(result, 1, length(result) - 1);
    if result = 'XMMRM128' then result := 'XMMRM';
    if result = 'YMMRM256' then result := 'YMMRM';
  end;

  procedure SplitOperands(const aOperand1, aOperand2, aOperand3, aOperand4: string; var aRegCombi0, aRegCombi1, aRegCombi2, aRegCombi3, aRegCombi4: string);
  var
    i: integer;
    s1: string;
    s2: string;
    s3: string;
    s4: string;
    iCnt1: integer;
    iCnt2: integer;
    iCnt3: integer;
    iCnt4: integer;
    iMaxCnt: integer;

  begin
    with TStringList.Create do
    try
      Text := StringReplace(trim(aOperand1), '|', #13#10, [rfReplaceAll]);
      iCnt1 := Count;

      Text := StringReplace(trim(aOperand2), '|', #13#10, [rfReplaceAll]);
      iCnt2 := Count;

      Text := StringReplace(trim(aOperand3), '|', #13#10, [rfReplaceAll]);
      iCnt3 := Count;

      Text := StringReplace(trim(aOperand4), '|', #13#10, [rfReplaceAll]);
      iCnt4 := Count;

      iMaxCnt := iCnt1;
      if iCnt2 > iMaxCnt then iMaxCnt := iCnt2;
      if iCnt3 > iMaxCnt then iMaxCnt := iCnt3;
      if iCnt4 > iMaxCnt then iMaxCnt := iCnt4;


      if (aOperand1 <> '') and (aRegCombi0 <> '') then
      begin
        aRegCombi0 := aRegCombi0 + ',';
        aRegCombi1 := aRegCombi1 + ',';
        aRegCombi2 := aRegCombi2 + ',';
        aRegCombi3 := aRegCombi3 + ',';
        aRegCombi4 := aRegCombi4 + ',';
      end;


      Text := StringReplace(trim(aOperand1), '|', #13#10, [rfReplaceAll]);
      if Count = iMaxCnt then
      begin
        for i := 0 to iMaxCnt - 1 do
        begin
          case i of
            0: aRegCombi0 := aRegCombi0 + ',' + Strings[i];
            1: aRegCombi1 := aRegCombi1 + ',' + Strings[i];
            2: aRegCombi2 := aRegCombi2 + ',' + Strings[i];
            3: aRegCombi3 := aRegCombi3 + ',' + Strings[i];
            4: aRegCombi4 := aRegCombi4 + ',' + Strings[i];
          end;
        end;
      end
      else
      begin
        if Count = 1 then
        begin
          for i := 0 to iMaxCnt - 1 do
          begin
            case i of
              0: aRegCombi0 := aRegCombi0 + ',' + Strings[0];
              1: aRegCombi1 := aRegCombi1 + ',' + Strings[0];
              2: aRegCombi2 := aRegCombi2 + ',' + Strings[0];
              3: aRegCombi3 := aRegCombi3 + ',' + Strings[0];
              4: aRegCombi4 := aRegCombi4 + ',' + Strings[0];
            end;
          end;
        end
        else
        begin
          // TODO log
        end;
      end;
    finally
      Free;
    end;
  end;

  function MapOperand(aOpTyp: TOpType): String;
  begin
    case aOpTyp of
        otXMMReg: result := 'X';
        otYMMReg: result := 'Y';
        otZMMReg: result := 'Z';
           otEAX,
           otRAX,
         otREG64,
         otREG32,
         otREG16,
          otREG8: result := 'R';
          otRM32,
          otRM64,
         otXMMRM,
        otXMMRM8,
       otXMMRM16,
         otYMMRM,
         otZMMRM,
         otMem32,
          otMem8,
         otMem16,
         otMem64,
        otMem128,
        otMem256,
        otMem512: result := 'M';
          otIMM8: result := 'I';
        otXMEM32: result := 'X32';
        otXMEM64: result := 'X64';
        otYMEM32: result := 'Y32';
        otYMEM64: result := 'Y64';
        otZMEM32: result := 'Z32';
        otZMEM64: result := 'Z64';
           otB32: result := 'B32';
           otB64: result := 'B64';
          otKREG: result := 'K';
             else result := '';
    end;
  end;

  function AsmCodeBlockCompare(aAsmCounter: integer; aCompareMode: TAsmCompareMode): String;
  var
    sReg: string;
  begin
    result := '';

    case Fx64 of
      true: sReg := 'RAX';
       else sReg := 'EAX';
    end;

    with TStringList.Create do
    try
      Add(format('%20s%6s  ',             ['    push', sReg]));
      Add(format('%20s%6s,%s',            ['     mov', sReg, inttostr(aAsmCounter)]));
      Add(format('%20s%6s,%s',            ['   kmovd', 'K7', 'EAX']));
      Add(format('%20s%6s',               ['     pop', sReg]));

      case aComparemode of
        //cmKORTESTNC: begin
        //               Add(format('%20s%6s, %s',           ['ktestb', 'K2', 'K1']));
        //               Add(format('%20s  %6s',             ['     jnc', '@@CHECKRESULT']));
        //             end;
        //cmXORTestNZ: begin
        //               Add(format('%20s%6s, %s',           ['kortestq', 'K2', 'K2']));
        //               Add(format('%20s  %6s',             ['     jnz', '@@CHECKRESULT']));
        //             end;
         cmKORTESTNC: begin
                        Add(format('%20s%6s, %s',           ['ktestb', 'K2', 'K1']));
                        Add(format('%20s%6s, %s',           [' kmovq', 'R10', 'K6']));
                        Add(format('%20s%6s, @@%d[RIP]',    ['cmovc', 'R10', aAsmCounter]));
                        Add(format('%20s  %6s',             ['  jmp', 'R10']));
                        Add(format('        @@%d%s',               [aAsmCounter, ':']));
                      end;
         cmXORTestNZ: begin
                        Add(format('%20s%6s, %s',           ['kortestq', 'K2', 'K2']));
                        Add(format('%20s%6s, %s',           [' kmovq', 'R10', 'K6']));
                        Add(format('%20s%6s, @@%d[RIP]',    ['cmovz', 'R10', aAsmCounter]));
                        Add(format('%20s  %6s',             ['  jmp', 'R10']));
                        Add(format('        @@%d%s',         [aAsmCounter, ':']));
                      end;

      end;

      result := Text;
    finally
      Free;
    end;
  end;

begin
  result := TStringList.Create;

  iAsmCounter := 0;

  OItem1 := TOperandListItem.Create;
  try
    OItem2 := TOperandListItem.Create;
    try
      OItem3 := TOperandListItem.Create;
      try
        OItem4 := TOperandListItem.Create;
        try

          UsePrefix := (UpperCase(aInst) = 'VCVTPD2DQ') OR
                       (UpperCase(aInst) = 'VCVTPD2PS') OR
                       (UpperCase(aInst) = 'VCVTSI2SD') OR
                       (UpperCase(aInst) = 'VCVTSI2SS') OR
                       (UpperCase(aInst) = 'VCVTTPD2DQ') or
                       (UpperCase(aInst) = 'VPMOVZXWQ') or
                       (UpperCase(aInst) = 'VCVTPD2UDQ') or
                       (UpperCase(aInst) = 'VCVTPD2UDQ') or
                       (UpperCase(aInst) = 'VCVTTPD2UDQ') or
                       (UpperCase(aInst) = 'VCVTUQQ2PS') or
                       (UpperCase(aInst) = 'VCVTQQ2PS') or
                       (UpperCase(aInst) = 'VCVTUSI2SD') or
                       (UpperCase(aInst) = 'VCVTUSI2SS') or
                       (UpperCase(aInst) = 'VFPCLASSPD') or
                       (UpperCase(aInst) = 'VFPCLASSPS') or
                       (UpperCase(aInst) = 'VCMPSS')

                       ;


          MaskRegNeeded := (Pos('VGATHER', Uppercase(aInst)) = 1) or
                           (Pos('VPGATHER', Uppercase(aInst)) = 1) or
			   (Pos('VPSCATTER', Uppercase(aInst)) = 1) or
			   (Pos('VSCATTER', Uppercase(aInst)) = 1);

          for il_Op := 1 to 4 do
          begin
            sl_Prefix := '';

            case il_Op of
              1: begin
                   Item := OItem1;
                   sl_Operand := aOp1;
                 end;
              2: begin
                   Item := OItem2;
                   sl_Operand := aOp2;
                 end;
              3: begin
                   Item := OItem3;
                   sl_Operand := aOp3;
                 end;
              4: begin
                   Item := OItem4;
                   sl_Operand := aOp4;
                 end;
            end;

            sl_Operand := PrepareOperandTyp(sl_Operand);

            if (AnsiSameText(sl_Operand, 'XMMREG')) then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otXMMReg;
              Item.OpActive := true;

              Item.Values.Add('XMM16');
            end
            else if (AnsiSameText(sl_Operand, 'XMMREG_M')) or
                    (AnsiSameText(sl_Operand, 'XMMREG_MZ')) or
                    (AnsiSameText(sl_Operand, 'XMMREG_ER')) or
                    (AnsiSameText(sl_Operand, 'XMMREG_SAE')) then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otXMMReg;
              Item.OpActive := true;

              //sSuffix := '';
              //if Pos('_MZ', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1} {z}'
              // else if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';
              //
              //if Pos('_ER', sl_Operand) > 0 then sSuffix := ', {ru-sae}'
              // else if FSAE and (Pos('_SAE', AnsiUppercase(sl_Operand)) > 0) then sSuffix := ', {sae}';
              //
              //Item.Values.Add('XMM0' + sSuffix);
              //if (sSuffix <> '') and
              //   (MaskRegNeeded = false) then Item.Values.Add('XMM0');

              Item.Values.Add('XMM16');
            end
            else if (AnsiSameText(sl_Operand, 'XMMRM')) or
                    (AnsiSameText(sl_Operand, 'XMMRM_M')) or
                    (AnsiSameText(sl_Operand, 'XMMRM_MZ')) or
                    (AnsiSameText(sl_Operand, 'XMMRM_ER')) or
                    (AnsiSameText(sl_Operand, 'XMMRM_SAE')) then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otXMMRM;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'oword ';

              if x64 then MemRegBaseIndexCombiCDISP8N(sl_Prefix, '', FReg64Base, FReg64Index, Item.Values)
               else MemRegBaseIndexCombiCDISP8N(sl_Prefix, '', FReg32Base, FReg32Index, Item.Values);
            end
            else if (AnsiSameText(sl_Operand, 'XMMRM8')) or
                    (AnsiSameText(sl_Operand, 'XMMRM8_M')) or
                    (AnsiSameText(sl_Operand, 'XMMRM8_MZ')) or
                    (AnsiSameText(sl_Operand, 'XMMRM8_ER')) or
                    (AnsiSameText(sl_Operand, 'XMMRM8_SAE')) then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otXMMRM8;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'byte ';

              if x64 then MemRegBaseIndexCombiCDISP8N(sl_Prefix, '', FReg64Base, FReg64Index, Item.Values)
               else MemRegBaseIndexCombiCDISP8N(sl_Prefix, '', FReg32Base, FReg32Index, Item.Values);
            end
            else if (AnsiSameText(sl_Operand, 'XMMRM16')) or
                    (AnsiSameText(sl_Operand, 'XMMRM16_M')) or
                    (AnsiSameText(sl_Operand, 'XMMRM16_MZ')) or
                    (AnsiSameText(sl_Operand, 'XMMRM16_ER')) or
                    (AnsiSameText(sl_Operand, 'XMMRM16_SAE'))
                    then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otXMMRM16;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'word ';

              if x64 then MemRegBaseIndexCombiCDISP8N(sl_Prefix, '', FReg64Base, FReg64Index, Item.Values)
               else MemRegBaseIndexCombiCDISP8N(sl_Prefix, '', FReg32Base, FReg32Index, Item.Values);
            end
            else if (AnsiSameText(sl_Operand, 'YMMREG')) then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otYMMReg;
              Item.OpActive := true;

              Item.Values.Add('YMM16');
            end
            else if (AnsiSameText(sl_Operand, 'YMMREG_M')) or
                    (AnsiSameText(sl_Operand, 'YMMREG_MZ')) or
                    (AnsiSameText(sl_Operand, 'YMMREG_ER')) or
                    (AnsiSameText(sl_Operand, 'YMMREG_SAE'))
                    then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otYMMReg;
              Item.OpActive := true;

              //sSuffix := '';
              //if Pos('_MZ', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1} {z}'
              // else if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';
              //
              //if Pos('_ER', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ', {rd-sae}'
              // else if FSAE and (Pos('_SAE', AnsiUppercase(sl_Operand)) > 0) then sSuffix := ', {sae}';
              //
              //Item.Values.Add('YMM0' + sSuffix);
              //if (sSuffix <> '') and
              //   (MaskRegNeeded = false) then Item.Values.Add('YMM0');

              Item.Values.Add('YMM16');
            end
            else if (AnsiSameText(sl_Operand, 'YMMRM'))  or
                    (AnsiSameText(sl_Operand, 'YMMRM_M')) or
                    (AnsiSameText(sl_Operand, 'YMMRM_MZ')) or
                    (AnsiSameText(sl_Operand, 'YMMRM_ER')) or
                    (AnsiSameText(sl_Operand, 'YMMRM_SAE'))
                    then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otYMMRM;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'yword ';

              //sSuffix := '';
              //if Pos('_MZ', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1} {z}'
              // else if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';
              //
              //if Pos('_ER', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ', {rd-sae}'
              // else if FSAE and (Pos('_SAE', AnsiUppercase(sl_Operand)) > 0) then sSuffix := ', {sae}';

              if x64 then MemRegBaseIndexCombiCDISP8N(sl_Prefix, '', FReg64Base, FReg64Index, Item.Values)
               else MemRegBaseIndexCombiCDISP8N(sl_Prefix, '', FReg32Base, FReg32Index, Item.Values);
            end
            else if (AnsiSameText(sl_Operand, 'ZMMREG')) then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otZMMReg;
              Item.OpActive := true;

              Item.Values.Add('ZMM16');
            end
            else if (AnsiSameText(sl_Operand, 'ZMMREG_M')) or
                    (AnsiSameText(sl_Operand, 'ZMMREG_MZ')) or
                    (AnsiSameText(sl_Operand, 'ZMMREG_ER')) or
                    (AnsiSameText(sl_Operand, 'ZMMREG_SAE'))
                    then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otZMMReg;
              Item.OpActive := true;

              //sSuffix := '';
              //if Pos('_MZ', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1} {z}'
              // else if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';
              //
              //if Pos('_ER', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ', {rd-sae}'
              // else if FSAE and (Pos('_SAE', AnsiUppercase(sl_Operand)) > 0) then sSuffix := ', {sae}';

              Item.Values.Add('ZMM16');
            end
            else if (AnsiSameText(sl_Operand, 'ZMMRM')) or
                    (AnsiSameText(sl_Operand, 'ZMMRM_M')) or
                    (AnsiSameText(sl_Operand, 'ZMMRM_MZ')) or
                    (AnsiSameText(sl_Operand, 'ZMMRM_ER')) or
                    (AnsiSameText(sl_Operand, 'ZMMRM_SAE'))
                    then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otZMMRM;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'zword ';

              //sSuffix := '';
              //if Pos('_MZ', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1} {z}'
              // else if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';
              //
              //if Pos('_ER', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ', {rd-sae}'
              // else if FSAE and (Pos('_SAE', AnsiUppercase(sl_Operand)) > 0) then sSuffix := ', {sae}';

              if x64 then MemRegBaseIndexCombiCDISP8N(sl_Prefix, '', FReg64Base, FReg64Index, Item.Values)
               else MemRegBaseIndexCombiCDISP8N(sl_Prefix, '', FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, 'MEM8') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otMEM8;
              Item.OpActive := true;



	      if UsePrefix then sl_Prefix := 'byte ';


              sSuffix := '';

	      if x64 then
              begin
                MemRegBaseIndexCombiCDISP8N(sl_Prefix, '', FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombiCDISP8N(sl_prefix, '', FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, 'MEM16') or
                    AnsiSameText(sl_Operand, 'MEM16_M') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otMEM16;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'word ';

              sSuffix := '';
              if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';

              if x64 then
              begin
                MemRegBaseIndexCombiCDISP8N(sl_Prefix, sSuffix, FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombiCDISP8N(sl_Prefix, sSuffix, FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, 'MEM32') or
                    AnsiSameText(sl_Operand, 'MEM32_M') or
                    AnsiSameText(sl_Operand, 'MEM32_MZ') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otMEM32;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'dword ';

              sSuffix := '';

	      if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';
              if Pos('_MZ', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1} {z}';


              if x64 then
              begin
                MemRegBaseIndexCombiCDISP8N(sl_prefix, sSuffix, FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombiCDISP8N(sl_prefix, sSuffix, FReg32Base, FReg32Index, Item.Values);
            end
            else if (AnsiSameText(sl_Operand, 'MEM64')) or
                    (AnsiSameText(sl_Operand, 'MEM64_M')) or
                    (AnsiSameText(sl_Operand, 'MEM64_MZ')) then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otMEM64;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'qword ';

              sSuffix := '';
              if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';
              if Pos('_MZ', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1} {z}';

              if x64 then
              begin
                MemRegBaseIndexCombiCDISP8N(sl_Prefix, sSuffix, FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombiCDISP8N(sl_prefix, sSuffix, FReg32Base, FReg32Index, Item.Values);
            end
            else if (AnsiSameText(sl_Operand, 'MEM128')) or
                    (AnsiSameText(sl_Operand, 'MEM128_M')) or
                    (AnsiSameText(sl_Operand, 'MEM128_MZ')) then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otMEM128;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'oword ';

              sSuffix := '';
              if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';
              if Pos('_MZ', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1} {z}';

              if x64 then
              begin
                MemRegBaseIndexCombiCDISP8N(sl_prefix, sSuffix, FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombiCDISP8N(sl_prefix, sSuffix, FReg32Base, FReg32Index, Item.Values);
            end
            else if (AnsiSameText(sl_Operand, 'MEM256')) or
                    (AnsiSameText(sl_Operand, 'MEM256_M')) or
                    (AnsiSameText(sl_Operand, 'MEM256_MZ')) then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otMEM256;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'yword ';

              sSuffix := '';
              if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';
              if Pos('_MZ', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1} {z}';


              if x64 then
              begin
                MemRegBaseIndexCombiCDISP8N(sl_prefix, sSuffix, FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombiCDISP8N(sl_prefix, sSuffix, FReg32Base, FReg32Index, Item.Values);
            end
            else if (AnsiSameText(sl_Operand, 'MEM512')) or
                    (AnsiSameText(sl_Operand, 'MEM512_M')) or
                    (AnsiSameText(sl_Operand, 'MEM512_MZ')) then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otMEM512;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'zword ';

              sSuffix := '';
              if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';
              if Pos('_MZ', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1} {z}';


              if x64 then
              begin
                MemRegBaseIndexCombiCDISP8N(sl_prefix, sSuffix, FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombiCDISP8N(sl_prefix, sSuffix, FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, 'REG8') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otREG8;
              Item.OpActive := true;

              if x64 then
              begin
                Item.Values.AddStrings(FReg8);
              end
              else Item.Values.AddStrings(FReg8);
            end
            else if AnsiSameText(sl_Operand, 'REG16') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otREG16;
              Item.OpActive := true;

              if x64 then
              begin
                Item.Values.AddStrings(FReg16);
              end
              else Item.Values.AddStrings(FReg16);
            end
            else if AnsiSameText(sl_Operand, 'REG32') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otREG32;
              Item.OpActive := true;

              if x64 then
              begin
                Item.Values.AddStrings(FReg32Base);
              end
              else Item.Values.AddStrings(FReg32Base);
            end
            else if AnsiSameText(sl_Operand, 'REG64') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otREG64;
              Item.OpActive := true;

              if x64 then
              begin
                Item.Values.AddStrings(FReg64Base);
              end;
            end
            else if AnsiSameText(sl_Operand, 'RM32') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otRM32;
              Item.OpActive := true;

              Item.Values.AddStrings(FReg32Base);

              if UsePrefix then sl_Prefix := 'dword ';

              if x64 then
              begin
                MemRegBaseIndexCombiCDISP8N(sl_Prefix, '', FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombiCDISP8N(sl_prefix, '', FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, 'RM64') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otRM32;
              Item.OpActive := true;



              if UsePrefix then sl_Prefix := 'qword ';

              if x64 then
              begin
                Item.Values.AddStrings(FReg64Base);
                MemRegBaseIndexCombiCDISP8N(sl_Prefix, '', FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombiCDISP8N(sl_prefix, '', FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, 'IMM8') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otIMM8;
              Item.OpActive := true;

              Item.Values.Add('0');
            end
            else if AnsiSameText(sl_Operand, 'XMEM32') or
                    AnsiSameText(sl_Operand, 'XMEM32_M') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otXMEM32;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'oword ';

              sSuffix := '';
              if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';

              if x64 then
              begin
		VectorMemRegBaseIndexCombi(sl_prefix, sSuffix, FReg64Base, FReg64XMMIndex, Item.Values);
		if (sSuffix <> '') and
                   (MaskRegNeeded = false) then
 		 VectorMemRegBaseIndexCombi(sl_prefix, '', FReg64Base, FReg64XMMIndex, Item.Values);
              end
              else
              begin
                VectorMemRegBaseIndexCombi(sl_prefix, sSuffix, FReg32Base, FReg32XMMIndex, Item.Values);
		if (sSuffix <> '') and
                   (MaskRegNeeded = false) then
                 VectorMemRegBaseIndexCombi(sl_prefix, '', FReg32Base, FReg32XMMIndex, Item.Values);
              end;
            end
            else if AnsiSameText(sl_Operand, 'XMEM64') or
                    AnsiSameText(sl_Operand, 'XMEM64_M') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otXMEM64;
              Item.OpActive := true;

              //if UsePrefix then sl_Prefix := 'oword ';
              //
              //if x64 then
              //begin
              //  VectorMemRegBaseIndexCombi(sl_prefix, FReg64Base, FReg64XMMIndex, Item.Values);
              //end
              //else
              //begin
              //  VectorMemRegBaseIndexCombi(sl_prefix, FReg32Base, FReg32XMMIndex, Item.Values);
              //end;

              sSuffix := '';
              if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';

              if x64 then
              begin
                VectorMemRegBaseIndexCombi(sl_prefix, sSuffix, FReg64Base, FReg64XMMIndex, Item.Values);
                if (sSuffix <> '') and
                   (MaskRegNeeded = false)
                        then
 		 VectorMemRegBaseIndexCombi(sl_prefix, '', FReg64Base, FReg64XMMIndex, Item.Values);
              end
              else
              begin
                VectorMemRegBaseIndexCombi(sl_prefix, sSuffix, FReg32Base, FReg32XMMIndex, Item.Values);
                if (sSuffix <> '') and
                   (MaskRegNeeded = false)
                        then
 		 VectorMemRegBaseIndexCombi(sl_prefix, '', FReg32Base, FReg32XMMIndex, Item.Values);
              end;

            end
            else if AnsiSameText(sl_Operand, 'YMEM32') or
                    AnsiSameText(sl_Operand, 'YMEM32_M') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otYMEM32;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'yword ';

              //if x64 then
              //begin
              //  VectorMemRegBaseIndexCombi(sl_prefix, FReg64Base, FReg64YMMIndex, Item.Values);
              //end
              //else
              //begin
              //  VectorMemRegBaseIndexCombi(sl_prefix, FReg32Base, FReg32YMMIndex, Item.Values);
              //end;

              sSuffix := '';
              if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';

              if x64 then
              begin
                VectorMemRegBaseIndexCombi(sl_prefix, sSuffix, FReg64Base, FReg64YMMIndex, Item.Values);
                if (sSuffix <> '') and
                   (MaskRegNeeded = false)
                        then
		 VectorMemRegBaseIndexCombi(sl_prefix, '', FReg64Base, FReg64YMMIndex, Item.Values);
              end
              else
              begin
                VectorMemRegBaseIndexCombi(sl_prefix, sSuffix, FReg32Base, FReg32YMMIndex, Item.Values);
                if (sSuffix <> '') and
                   (MaskRegNeeded = false)
                        then
		 VectorMemRegBaseIndexCombi(sl_prefix, '', FReg32Base, FReg32YMMIndex, Item.Values);
              end;

            end
            else if AnsiSameText(sl_Operand, 'YMEM64') or
                    AnsiSameText(sl_Operand, 'YMEM64_M') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otYMEM64;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'yword ';

              //if x64 then
              //begin
              //  VectorMemRegBaseIndexCombi(sl_prefix, FReg64Base, FReg64YMMIndex, Item.Values);
              //end
              //else
              //begin
              //  VectorMemRegBaseIndexCombi(sl_prefix, FReg32Base, FReg32YMMIndex, Item.Values);
              //end;

              sSuffix := '';
              if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';

              if x64 then
              begin
                VectorMemRegBaseIndexCombi(sl_prefix, sSuffix, FReg64Base, FReg64YMMIndex, Item.Values);
		if (sSuffix <> '') and
                   (MaskRegNeeded = false)
                        then
                 VectorMemRegBaseIndexCombi(sl_prefix, '', FReg64Base, FReg64YMMIndex, Item.Values);
              end
              else
              begin
                VectorMemRegBaseIndexCombi(sl_prefix, sSuffix, FReg32Base, FReg32YMMIndex, Item.Values);
                if (sSuffix <> '') and
                   (MaskRegNeeded = false)
                        then
		 VectorMemRegBaseIndexCombi(sl_prefix, '', FReg32Base, FReg32YMMIndex, Item.Values);
              end;

            end
            else if AnsiSameText(sl_Operand, 'ZMEM32') or
                    AnsiSameText(sl_Operand, 'ZMEM32_M')  then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otZMEM32;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'zword ';

              //if x64 then
              //begin
              //  VectorMemRegBaseIndexCombi(sl_prefix, FReg64Base, FReg64ZMMIndex, Item.Values);
              //end
              //else
              //begin
              //  VectorMemRegBaseIndexCombi(sl_prefix, FReg32Base, FReg32ZMMIndex, Item.Values);
              //end;

              sSuffix := '';
              if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';

              if x64 then
              begin
                VectorMemRegBaseIndexCombi(sl_prefix, sSuffix, FReg64Base, FReg64ZMMIndex, Item.Values);
		if (sSuffix <> '') and
                   (MaskRegNeeded = false)
                        then
                 VectorMemRegBaseIndexCombi(sl_prefix, '', FReg64Base, FReg64ZMMIndex, Item.Values);
              end
              else
              begin
                VectorMemRegBaseIndexCombi(sl_prefix, sSuffix, FReg32Base, FReg32ZMMIndex, Item.Values);
		if (sSuffix <> '') and
                   (MaskRegNeeded = false)
                        then
                 VectorMemRegBaseIndexCombi(sl_prefix, '', FReg32Base, FReg32ZMMIndex, Item.Values);
              end;

            end
            else if AnsiSameText(sl_Operand, 'ZMEM64') or
                    AnsiSameText(sl_Operand, 'ZMEM64_M') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otZMEM64;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'zword ';

              //if x64 then
              //begin
              //  VectorMemRegBaseIndexCombi(sl_prefix, FReg64Base, FReg64ZMMIndex, Item.Values);
              //end
              //else
              //begin
              //  VectorMemRegBaseIndexCombi(sl_prefix, FReg32Base, FReg32ZMMIndex, Item.Values);
              //end;

              sSuffix := '';
              if Pos('_M', AnsiUppercase(sl_Operand)) > 0 then sSuffix := ' {k1}';

              if x64 then
              begin
                VectorMemRegBaseIndexCombi(sl_prefix, sSuffix, FReg64Base, FReg64ZMMIndex, Item.Values);
		if (sSuffix <> '') and
                   (MaskRegNeeded = false)
                        then
                 VectorMemRegBaseIndexCombi(sl_prefix, '', FReg64Base, FReg64ZMMIndex, Item.Values);
              end
              else
              begin
                VectorMemRegBaseIndexCombi(sl_prefix, sSuffix, FReg32Base, FReg32ZMMIndex, Item.Values);
                if (sSuffix <> '') and
                   (MaskRegNeeded = false)
                        then
		 VectorMemRegBaseIndexCombi(sl_prefix, '', FReg32Base, FReg32ZMMIndex, Item.Values);
              end;

            end
            else if AnsiSameText(sl_Operand, '2B32') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otB32;
              Item.OpActive := true;


              if x64 then
              begin
                MemRegBaseIndexCombiCDISP8N(sl_Prefix, ' {1to2}',  FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombiCDISP8N(sl_prefix, ' {1to2}', FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, '4B32') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otB32;
              Item.OpActive := true;


              if x64 then
              begin
                MemRegBaseIndexCombiCDISP8N(sl_Prefix, ' {1to4}',  FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombiCDISP8N(sl_prefix, ' {1to4}', FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, '8B32') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otB32;
              Item.OpActive := true;


              if x64 then
              begin
                MemRegBaseIndexCombiCDISP8N(sl_Prefix, ' {1to8}',  FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombiCDISP8N(sl_prefix, ' {1to8}', FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, '16B32') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otB32;
              Item.OpActive := true;


              if x64 then
              begin
                MemRegBaseIndexCombiCDISP8N(sl_Prefix, ' {1to16}',  FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombiCDISP8N(sl_prefix, ' {1to16}', FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, '2B64') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otB64;
              Item.OpActive := true;


              if x64 then
              begin
                MemRegBaseIndexCombiCDISP8N(sl_Prefix, ' {1to2}',  FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombiCDISP8N(sl_prefix, ' {1to2}', FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, '4B64') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otB64;
              Item.OpActive := true;


              if x64 then
              begin
                MemRegBaseIndexCombiCDISP8N(sl_Prefix, ' {1to4}',  FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombiCDISP8N(sl_prefix, ' {1to4}', FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, '8B64') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otB64;
              Item.OpActive := true;


              if x64 then
              begin
                MemRegBaseIndexCombiCDISP8N(sl_Prefix, ' {1to8}',  FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombiCDISP8N(sl_prefix, ' {1to8}', FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, '16B64') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otB64;
              Item.OpActive := true;


              if x64 then
              begin
                MemRegBaseIndexCombiCDISP8N(sl_Prefix, ' {1to16}',  FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombiCDISP8N(sl_prefix, ' {1to16}', FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, 'KREG') or
                    AnsiSameText(sl_Operand, 'KREG_M') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otKREG;
              Item.OpActive := true;

              Item.Values.Add('K1');
            end
            else if trim(sl_Operand) = '' then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otUnknown;
              Item.OpActive := false;

              Item.Values.Add('');
            end
            else
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otUnknown;
              Item.OpActive := false;

              Item.Values.Add('?' + sl_Operand);
            end

          end;

          //sl_RegCombi := '';


          il_Operands := 0;
          UsedParams  := 0;

          if OItem1.OpActive then
          begin
            inc(il_Operands);
            UsedParams := UsedParams or 1;
          end;

          if OItem2.OpActive then
          begin
            inc(il_Operands);
            UsedParams := UsedParams or 2;
          end;

          if OItem3.OpActive then
          begin
            inc(il_Operands);
            UsedParams := UsedParams or 4;
          end;

          if OItem4.OpActive then
          begin
            inc(il_Operands);
            UsedParams := UsedParams or 8;
          end;

          case il_Operands of
              1: UseDefault := UsedParams <> 1;
              2: UseDefault := UsedParams <> 3;
              3: UseDefault := UsedParams <> 7;
              4: UseDefault := UsedParams <> 15;
            else UseDefault := true;
          end;

          // TODO deaktivieren
          //UseDefault := true;

          if UseDefault then
          begin
            sl_Inst := format('%-20s', [aInst]);

            for il_Op1 := 0 to OItem1.Values.Count - 1 do
            begin
              for il_Op2 := 0 to OItem2.Values.Count - 1 do
              begin
                for il_Op3 := 0 to OItem3.Values.Count - 1 do
                begin
                  for il_Op4 := 0 to OItem4.Values.Count - 1 do
                  begin
                    slRegCombi := TStringList.Create;
                    try
                      while slRegCombi.Count < 5 do
                       slRegCombi.Add('');


                      //SplitOperands(OItem1.Values[il_Op1], OItem2.Values[il_Op2], OItem3.Values[il_Op3], OItem4.Values[il_Op4]
                      if OItem1.OpActive then
                      begin
                      //  SplitOperand(OItem1.Values[il_Op1], sRegCombi0,sRegCombi1,sRegCombi2,sRegCombi3,sRegCombi4);
                      //
                      end;
                    finally
                      FreeAndNil(slRegCombi);
                    end;

                    //sRegCombi0 := '';
                    //sRegCombi1 := '';
                    //sRegCombi2 := '';
                    //sRegCombi3 := '';
                    //sRegCombi4 := '';
                    //
                    //if OItem1.OpActive then
                    //begin
                    //  if sRegCombi0 <> '' then
                    //  begin
                    //    sRegCombi0 := sRegCombi0 + ', ';
                    //    sRegCombi1 := sRegCombi1 + ', ';
                    //    sRegCombi2 := sRegCombi2 + ', ';
                    //    sRegCombi3 := sRegCombi3 + ', ';
                    //    sRegCombi4 := sRegCombi4 + ', ';
                    //  end;
                    //
                    //  if Pos('|', OItem1.Values[il_Op1]) > 0 then
                    //  begin
                    //    with TStringList.Create do
                    //    try
                    //      Text := Stringreplace(OItem1.Values[il_Op1], '|', #13#10,[rfReplaceAll]);
                    //      iCnt := Count;
                    //      while Count < 5 do Add('');
                    //
                    //      sRegCombi0 := sRegCombi0 + Strings[0];
                    //      sRegCombi1 := sRegCombi1 + Strings[1];
                    //      sRegCombi2 := sRegCombi2 + Strings[2];
                    //      sRegCombi3 := sRegCombi3 + Strings[3];
                    //      sRegCombi4 := sRegCombi4 + Strings[4];
                    //
                    //    finally
                    //      Free;
                    //    end;
                    //  end
                    //  else
                    //  begin
                    //    sRegCombi0 := sRegCombi0 + OItem1.Values[il_Op1];
                    //    sRegCombi1 := sRegCombi1 + OItem1.Values[il_Op1];
                    //    sRegCombi2 := sRegCombi2 + OItem1.Values[il_Op1];
                    //    sRegCombi3 := sRegCombi3 + OItem1.Values[il_Op1];
                    //    sRegCombi4 := sRegCombi4 + OItem1.Values[il_Op1];
                    //  end;
                    //end;
                    //
                    //if OItem2.OpActive then
                    //begin
                    //  if sl_RegCombi <> '' then sl_RegCombi := sl_RegCombi + ', ';
                    //  sl_RegCombi := sl_RegCombi + OItem2.Values[il_Op2];
                    //end;
                    //
                    //if OItem3.OpActive then
                    //begin
                    //  if sl_RegCombi <> '' then sl_RegCombi := sl_RegCombi + ', ';
                    //  sl_RegCombi := sl_RegCombi + OItem3.Values[il_Op3];
                    //end;
                    //
                    //if OItem4.OpActive then
                    //begin
                    //  if sl_RegCombi <> '' then sl_RegCombi := sl_RegCombi + ', ';
                    //  sl_RegCombi := sl_RegCombi + OItem4.Values[il_Op4];
                    //end;
                    //
                    //if sl_RegCombi <> '' then
                    //begin
                    //  //result.Add(format('%-20s%s', [aInst, sl_RegCombi]));
                    //  result.Add(sl_Inst + sl_RegCombi);
                    //  sl_RegCombi := '';
                    //end;
                  end;
                end;
              end;
            end;
          end
          else
          begin
            OpMode := omUnknown;

            iOpNumMRef := -1;
            if (OItem1.OpTyp in MEMTYPES) or
               (OItem1.OpTyp in BMEMTYPES) then iOpNumMRef := 1
             else if (OItem2.OpTyp in MEMTYPES) or
                     (OItem2.OpTyp in BMEMTYPES) then iOpNumMRef := 2
             else if (OItem3.OpTyp in MEMTYPES) or
                     (OItem3.OpTyp in BMEMTYPES) then iOpNumMRef := 3
             else if (OItem4.OpTyp in MEMTYPES) or
                     (OItem4.OpTyp in BMEMTYPES) then iOpNumMRef := 4;


            // TODO delete
            //if il_Operands = 4 then

            case il_Operands of
                2: begin
                     if (OItem1.OpTyp in MEMTYPES) and
                        (OItem2.OpTyp = otXMMReg) then OpMode := omMX
                      else if (OItem1.OpTyp in MEMTYPES) and
                              (OItem2.OpTyp = otYMMReg) then OpMode := omMY
                      else if (OItem1.OpTyp in MEMTYPES) and
                              (OItem2.OpTyp = otZMMReg) then OpMode := omMZ

                      else if (OItem1.OpTyp = otXMMReg) and
                              (OItem2.OpTyp = otB32) then OpMode := omXB32
                      else if (OItem1.OpTyp = otXMMReg) and
                              (OItem2.OpTyp = otB64) then OpMode := omXB64

                      else if (OItem1.OpTyp = otYMMReg) and
                              (OItem2.OpTyp = otB32) then OpMode := omYB32
                      else if (OItem1.OpTyp = otYMMReg) and
                              (OItem2.OpTyp = otB64) then OpMode := omYB64

                      else if (OItem1.OpTyp = otZMMReg) and
                              (OItem2.OpTyp = otB32) then OpMode := omZB32
                      else if (OItem1.OpTyp = otZMMReg) and
                              (OItem2.OpTyp = otB64) then OpMode := omZB64

                      else if (OItem1.OpTyp = otXMMReg) and
                              (OItem2.OpTyp in MEMTYPES) then OpMode := omXM
                      else if (OItem1.OpTyp = otYMMReg) and
                              (OItem2.OpTyp in MEMTYPES) then OpMode := omYM
                      else if (OItem1.OpTyp = otZMMReg) and
                              (OItem2.OpTyp in MEMTYPES) then OpMode := omZM




                     else
                     begin
                       sLogMsg := '';
                       sLogMsg := MapOperand(OItem1.Optyp) + MapOperand(OItem2.Optyp) + MapOperand(OItem3.Optyp);
                       if sLogMsg <> '' then
                       begin

                         if (sLogMsg <> 'KX') and
                            (sLogMsg <> 'KY') and
                            (sLogMsg <> 'KZ') and
                            (sLogMsg <> 'RM') and
                            (sLogMsg <> 'RX') and
                            (sLogMsg <> 'XK') and
                            (sLogMsg <> 'XR') and
                            (sLogMsg <> 'XX') and
                            (sLogMsg <> 'XY') and
                            (sLogMsg <> 'XZ') and
                            (sLogMsg <> 'YK') and
                            (sLogMsg <> 'YR') and
                            (sLogMsg <> 'YX') and
                            (sLogMsg <> 'YY') and
                            (sLogMsg <> 'YZ') and
                            (sLogMsg <> 'ZK') and
                            (sLogMsg <> 'ZR') and
                            (sLogMsg <> 'ZX') and
                            (sLogMsg <> 'ZY') and
                            (sLogMsg <> 'ZZ') then

                         writeln('offen : ' + sLogMsg + ' (' + aInst + ')');
                       end;
                     end;
                   end;
                3: if (OItem1.OpTyp = otKReg) and
                      (OItem2.OpTyp = otXMMReg) and
                      (OItem3.OpTyp in MEMTYPES) then OpMode := omKXM
                    else if (OItem1.OpTyp = otKReg) and
                            (OItem2.OpTyp = otYMMReg) and
                            (OItem3.OpTyp in MEMTYPES) then OpMode := omKYM
                    else if (OItem1.OpTyp = otKReg) and
                            (OItem2.OpTyp = otZMMReg) and
                            (OItem3.OpTyp in MEMTYPES) then OpMode := omKZM

                    else if (OItem1.OpTyp = otKReg) and
                            (OItem2.OpTyp = otXMMReg) and
                            (OItem3.OpTyp = otB32) then OpMode := omKXB32
                    else if (OItem1.OpTyp = otKReg) and
                            (OItem2.OpTyp = otXMMReg) and
                            (OItem3.OpTyp = otB64) then OpMode := omKXB64
                    else if (OItem1.OpTyp = otKReg) and
                            (OItem2.OpTyp = otYMMReg) and
                            (OItem3.OpTyp = otB32) then OpMode := omKYB32
                    else if (OItem1.OpTyp = otKReg) and
                            (OItem2.OpTyp = otYMMReg) and
                            (OItem3.OpTyp = otB64) then OpMode := omKYB64
                    else if (OItem1.OpTyp = otKReg) and
                            (OItem2.OpTyp = otZMMReg) and
                            (OItem3.OpTyp = otB32) then OpMode := omKZB32
                    else if (OItem1.OpTyp = otKReg) and
                            (OItem2.OpTyp = otZMMReg) and
                            (OItem3.OpTyp = otB64) then OpMode := omKZB64

                    else if (OItem1.OpTyp = otKReg)    and
                            (OItem2.OpTyp in MEMTYPES) and
                            (OItem3.OpTyp = otIMM8) then OpMode := omKMI
                    else if (OItem1.OpTyp = otKReg)    and
                            (OItem2.OpTyp = otB32)     and
                            (OItem3.OpTyp = otIMM8) then OpMode := omKB32I
                    else if (OItem1.OpTyp = otKReg)    and
                            (OItem2.OpTyp = otB64)     and
                            (OItem3.OpTyp = otIMM8) then OpMode := omKB64I

                    else if (OItem1.OpTyp in MEMTYPES) and
                            (OItem2.OpTyp = otXMMReg)  and
                            (OItem3.OpTyp = otIMM8) then OpMode := omMXI
                    else if (OItem1.OpTyp in MEMTYPES) and
                            (OItem2.OpTyp = otYMMReg)  and
                            (OItem3.OpTyp = otIMM8) then OpMode := omMYI
                    else if (OItem1.OpTyp in MEMTYPES) and
                            (OItem2.OpTyp = otZMMReg)  and
                            (OItem3.OpTyp = otIMM8) then OpMode := omMZI

                    else if (OItem1.OpTyp = otXMMReg) and
                            (OItem2.OpTyp = otXMMReg) and
                            (OItem3.OpTyp in MEMTYPES) then OpMode := omXXM
                    else if (OItem1.OpTyp = otXMMReg) and
                            (OItem2.OpTyp = otXMMReg) and
                            (OItem3.OpTyp = otB32) then OpMode := omXXB32
                    else if (OItem1.OpTyp = otXMMReg) and
                            (OItem2.OpTyp = otXMMReg) and
                            (OItem3.OpTyp = otB64) then OpMode := omXXB64
                    else if (OItem1.OpTyp = otXMMReg) and
                            (OItem2.OpTyp = otB32) and
                            (OItem3.OpTyp = otIMM8) then OpMode := omXB32I
                    else if (OItem1.OpTyp = otXMMReg) and
                            (OItem2.OpTyp = otB64) and
                            (OItem3.OpTyp = otIMM8) then OpMode := omXB64I
                    else if (OItem1.OpTyp = otXMMReg) and
                            (OItem2.OpTyp in MEMTYPES) and
                            (OItem3.OpTyp = otIMM8) then OpMode := omXMI

                    else if (OItem1.OpTyp = otYMMReg) and
                            (OItem2.OpTyp = otYMMReg) and
                            (OItem3.OpTyp in MEMTYPES) then OpMode := omYYM
                    else if (OItem1.OpTyp = otYMMReg) and
                            (OItem2.OpTyp = otYMMReg) and
                            (OItem3.OpTyp = otB32) then OpMode := omYYB32
                    else if (OItem1.OpTyp = otYMMReg) and
                            (OItem2.OpTyp = otYMMReg) and
                            (OItem3.OpTyp = otB64) then OpMode := omYYB64
                    else if (OItem1.OpTyp = otYMMReg) and
                            (OItem2.OpTyp = otB32) and
                            (OItem3.OpTyp = otIMM8) then OpMode := omYB32I
                    else if (OItem1.OpTyp = otYMMReg) and
                            (OItem2.OpTyp = otB64) and
                            (OItem3.OpTyp = otIMM8) then OpMode := omYB64I
                    else if (OItem1.OpTyp = otYMMReg) and
                            (OItem2.OpTyp in MEMTYPES) and
                            (OItem3.OpTyp = otIMM8) then OpMode := omYMI

                    else if (OItem1.OpTyp = otZMMReg) and
                            (OItem2.OpTyp = otZMMReg) and
                            (OItem3.OpTyp in MEMTYPES) then OpMode  := omZZM
                    else if (OItem1.OpTyp = otZMMReg) and
                            (OItem2.OpTyp = otZMMReg) and
                            (OItem3.OpTyp = otB32) then OpMode := omZZB32
                    else if (OItem1.OpTyp = otZMMReg) and
                            (OItem2.OpTyp = otZMMReg) and
                            (OItem3.OpTyp = otB64) then OpMode := omZZB64
                            else if (OItem1.OpTyp = otZMMReg) and
                                    (OItem2.OpTyp = otB32) and
                                    (OItem3.OpTyp = otIMM8) then OpMode := omZB32I
                            else if (OItem1.OpTyp = otZMMReg) and
                                    (OItem2.OpTyp = otB64) and
                                    (OItem3.OpTyp = otIMM8) then OpMode := omZB64I
                    else if (OItem1.OpTyp = otZMMReg) and
                            (OItem2.OpTyp in MEMTYPES) and
                            (OItem3.OpTyp = otIMM8) then OpMode := omZMI


                    else
                    begin
                      sLogMsg := '';
                      sLogMsg := MapOperand(OItem1.Optyp) + MapOperand(OItem2.Optyp) + MapOperand(OItem3.Optyp);
                      if sLogMsg <> '' then
                      begin
                       if (sLogMsg <> 'RMI') and
                          (sLogMsg <> 'RRM') and
                          (sLogMsg <> 'RMR') and
                          (sLogMsg <> 'KKK') and
                          (sLogMsg <> 'KKI') and
                          (sLogMsg <> 'XXX') and
                          (sLogMsg <> 'YYY') and
                          (sLogMsg <> 'ZZZ') and
                          (sLogMsg <> 'XXI') and
                          (sLogMsg <> 'YYI') and
                          (sLogMsg <> 'ZZI') and
                          (sLogMsg <> 'XYI') and
                          (sLogMsg <> 'YZI') and
                          (sLogMsg <> 'XZI') and
                          (sLogMsg <> 'RXI') and
                          (sLogMsg <> 'RYI') and
                          (sLogMsg <> 'RZI') and


                          (sLogMsg <> 'XXR') then

                       writeln('offen : ' + sLogMsg + ' (' + aInst + ')');
                      end;
                    end;
                 4: if (OItem1.OpTyp = otKReg)       and
                       (OItem2.OpTyp = otXMMReg)     and
                       (OItem3.OpTyp = otB32)        and
                       (OItem4.OpTyp = otIMM8) then OpMode := omKXB32I
                     else if (OItem1.OpTyp = otKReg)       and
                             (OItem2.OpTyp = otXMMReg)     and
                             (OItem3.OpTyp = otB64)        and
                             (OItem4.OpTyp = otIMM8) then OpMode := omKXB64I
                     else if (OItem1.OpTyp = otKReg)       and
                             (OItem2.OpTyp = otXMMReg)     and
                             (OItem3.OpTyp in MEMTYPES)    and
                             (OItem4.OpTyp = otIMM8) then OpMode := omKXMI

                     else if (OItem1.OpTyp = otKReg)       and
                             (OItem2.OpTyp = otYMMReg)     and
                             (OItem3.OpTyp = otB32)        and
                             (OItem4.OpTyp = otIMM8) then OpMode := omKYB32I
                     else if (OItem1.OpTyp = otKReg)       and
                             (OItem2.OpTyp = otYMMReg)     and
                             (OItem3.OpTyp = otB64)        and
                             (OItem4.OpTyp = otIMM8) then OpMode := omKYB64I
                     else if (OItem1.OpTyp = otKReg)       and
                             (OItem2.OpTyp = otYMMReg)     and
                             (OItem3.OpTyp in MEMTYPES)    and
                             (OItem4.OpTyp = otIMM8) then OpMode := omKYMI

                     else if (OItem1.OpTyp = otKReg)       and
                             (OItem2.OpTyp = otZMMReg)     and
                             (OItem3.OpTyp = otB32)        and
                             (OItem4.OpTyp = otIMM8) then OpMode := omKZB32I
                     else if (OItem1.OpTyp = otKReg)       and
                             (OItem2.OpTyp = otZMMReg)     and
                             (OItem3.OpTyp = otB64)        and
                             (OItem4.OpTyp = otIMM8) then OpMode := omKZB64I
                     else if (OItem1.OpTyp = otKReg)       and
                             (OItem2.OpTyp = otZMMReg)     and
                             (OItem3.OpTyp in MEMTYPES)    and
                             (OItem4.OpTyp = otIMM8) then OpMode := omKZMI

                     else if (OItem1.OpTyp = otXMMReg) and
                             (OItem2.OpTyp = otXMMReg) and
                             (OItem3.OpTyp = otB32)    and
                             (OItem4.OpTyp = otIMM8) then OpMode := omXXB32I
                     else if (OItem1.OpTyp = otXMMReg) and
                             (OItem2.OpTyp = otXMMReg) and
                             (OItem3.OpTyp = otB64)    and
                             (OItem4.OpTyp = otIMM8) then OpMode := omXXB64I
                     else if (OItem1.OpTyp = otYMMReg) and
                             (OItem2.OpTyp = otYMMReg) and
                             (OItem3.OpTyp = otB32)    and
                             (OItem4.OpTyp = otIMM8) then OpMode := omYYB32I
                     else if (OItem1.OpTyp = otYMMReg) and
                             (OItem2.OpTyp = otYMMReg) and
                             (OItem3.OpTyp = otB64)    and
                             (OItem4.OpTyp = otIMM8) then OpMode := omYYB64I
                     else if (OItem1.OpTyp = otZMMReg) and
                             (OItem2.OpTyp = otZMMReg) and
                             (OItem3.OpTyp = otB32)    and
                             (OItem4.OpTyp = otIMM8) then OpMode := omZZB32I
                     else if (OItem1.OpTyp = otZMMReg) and
                             (OItem2.OpTyp = otZMMReg) and
                             (OItem3.OpTyp = otB64)    and
                             (OItem4.OpTyp = otIMM8) then OpMode := omZZB64I


                     else if (OItem1.OpTyp = otXMMReg) and
                             (OItem2.OpTyp = otXMMReg) and
                             (OItem3.OpTyp in MEMTYPES) and
                             (OItem4.OpTyp = otIMM8) then OpMode := omXXMI
                     else if (OItem1.OpTyp = otYMMReg) and
                             (OItem2.OpTyp = otYMMReg) and
                             (OItem3.OpTyp in MEMTYPES) and
                             (OItem4.OpTyp = otIMM8) then OpMode := omYYMI
                     else if (OItem1.OpTyp = otZMMReg) and
                             (OItem2.OpTyp = otZMMReg) and
                             (OItem3.OpTyp in MEMTYPES) and
                             (OItem4.OpTyp = otIMM8) then OpMode := omZZMI
                     else
                     begin
                       sLogMsg := '';
                       sLogMsg := MapOperand(OItem1.Optyp) + MapOperand(OItem2.Optyp) + MapOperand(OItem3.Optyp) + MapOperand(OItem4.Optyp);
                       if sLogMsg <> '' then
                       begin

                         if (sLogMsg <> 'KXXI') and
                            (sLogMsg <> 'KYYI') and
                            (sLogMsg <> 'KZZI') and
                            (sLogMsg <> 'XXRI') and
                            (sLogMsg <> 'XXXI') and
                            (sLogMsg <> 'YYYI') and
                            (sLogMsg <> 'ZZZI') then

                         writeln('offen : ' + sLogMsg + ' (' + aInst + ')');
                       end;
                     end;

              else;
            end;

            if OpMode <> omUnknown then
            begin
              sInstruction := format('%20s', [aInst]);

              for il_Op1 := 0 to OItem1.Values.Count - 1 do
              begin
                for il_Op2 := 0 to OItem2.Values.Count - 1 do
                begin
                  for il_Op3 := 0 to OItem3.Values.Count - 1 do
                  begin
                    for il_Op4 := 0 to OItem4.Values.Count - 1 do
                    begin
                      sRegCombi := '';

                      if OItem1.OpActive then
                      begin
                        if sRegCombi <> '' then sRegCombi := sRegCombi + ', ';
                        sRegCombi := sRegCombi + OItem1.Values[il_Op1];
                      end;

                      if OItem2.OpActive then
                      begin
                        if sRegCombi <> '' then sRegCombi := sRegCombi + ', ';
                        sRegCombi := sRegCombi + OItem2.Values[il_Op2];
                      end;

                      if OItem3.OpActive then
                      begin
                        if sRegCombi <> '' then sRegCombi := sRegCombi + ', ';
                        sRegCombi := sRegCombi + OItem3.Values[il_Op3];
                      end;

                      if OItem4.OpActive then
                      begin
                        if sRegCombi <> '' then sRegCombi := sRegCombi + ', ';
                        sRegCombi := sRegCombi + OItem4.Values[il_Op4];
                      end;

                      if sRegCombi <> '' then
                      begin
                        case iOpNumMRef of
                            1: sMRef := OItem1.Values[il_Op1];
                            2: sMRef := OItem2.Values[il_Op2];
                            3: sMRef := OItem3.Values[il_Op3];
                            4: sMRef := OItem1.Values[il_Op4];
                          else sMRef := '';
                        end;

                        if ParseBaseIndexReg(sMRef, sBaseReg, sIndexReg) then
                        begin
                          result.Add(format('%20s %s',               ['    pop', sBaseReg]));
                          result.Add(format('%20s %s',               ['   push', sBaseReg]));

                          if trim(sIndexReg) <> '' then
                           result.Add(format('%20s%6s, %s',          ['   xor', sIndexReg, sIndexReg]));

                          if OpMode in [omMXI, omMYI, omMZI] then
                          begin
                            case Fx64 of
                              true: begin
                                      result.Add(format('%20s %6s', ['push', 'RDI']));
                                      result.Add(format('%20s %6s', ['push', 'RCX']));
                                      result.Add(format('%20s %6s', ['push', 'RAX']));

                                      result.Add(format('%20s %6s', ['push', sBaseReg]));
                                      result.Add(format('%20s %6s', ['pop',  'RDI']));
                                      result.Add(format('%20s %6s', ['mov',  'RCX, sizeof(DataBlock)']));
                                      result.Add(format('%20s %6s, %s', ['xor',  'RAX', 'RAX']));
                                      result.Add(format('%20s ',    ['rep stosb']));
                                      result.Add(format('%20s %6s', ['pop',  'RAX']));
                                      result.Add(format('%20s %6s', ['pop',  'RCX']));
                                      result.Add(format('%20s %6s', ['pop',  'RDI']));
                                    end;
                               else begin
                                      result.Add(format('%20s %6s', ['push', 'EDI']));
                                      result.Add(format('%20s %6s', ['push', 'ECX']));
                                      result.Add(format('%20s %6s', ['push', 'EAX']));

                                      result.Add(format('%20s %6s', ['push', sBaseReg]));
                                      result.Add(format('%20s %6s', ['pop',  'EDI']));
                                      result.Add(format('%20s %6s', ['mov',  'ECX, sizeof(DataBlock)']));
                                      result.Add(format('%20s %6s, %s', ['xor',  'EAX', 'EAX']));
                                      result.Add(format('%20s ',    ['rep stosb']));
                                      result.Add(format('%20s %6s', ['pop',  'EAX']));
                                      result.Add(format('%20s %6s', ['pop',  'ECX']));
                                      result.Add(format('%20s %6s', ['pop',  'EDI']));
                                    end;
                            end;
                          end;

                        end;


                        //result.Add(format('%-20s%s', [aInst, sl_RegCombi]));
                        result.Add(format('%-20s %6s', [sInstruction, sRegCombi]));

                        inc(iAsmCounter);
                        case OpMode of
                            omKXM,
                            omKYM,
                            omKZM: begin
                                     result.Add(format('%20s %6s,%6s, %s + $2000', [aInst, 'K2', OItem2.Values[il_Op2], OItem3.Values[il_Op3] ]));
                                     result.Add(format('%20s %6s,%6s, %s', ['kxorq', 'K2', OItem1.Values[il_Op1], 'K2']));

                                     result.Add(AsmCodeBlockCompare(iAsmCounter, cmXORTestNZ));
                                   end;
                          omKXB32,
                          omKXB64,
                          omKYB32,
                          omKYB64,
                          omKZB32,
                          omKZB64: begin
                                     sMREF := OItem3.Values[il_Op3];
                                     //if Pos('{1to4}', sMREF) > 0 then sMREF := StringReplace(sMREF, '{1to4}', '{1to2}', [])
                                     // else if Pos('{1to8}', sMREF) > 0 then sMREF := StringReplace(sMREF, '{1to8}', '{1to4}', [])
                                     // else if Pos('{1to16}', sMREF) > 0 then sMREF := StringReplace(sMREF, '{1to16}', '{1to8}', []);

                                     result.Add(format('%20s %6s,%6s, %s + $2000', [aInst, 'K2', OItem2.Values[il_Op2], sMREF]));
                                     result.Add(format('%20s %6s,%6s, %s',         ['kxorq', 'K2', OItem1.Values[il_Op1], 'K2']));

                                     result.Add(AsmCodeBlockCompare(iAsmCounter, cmXORTestNZ));
                                   end;
                            omKMI: begin
                                     result.Add(format('%20s %6s,%6s + $2000, %s', [aInst, 'K2', OItem2.Values[il_Op2], OItem3.Values[il_Op3] ]));
                                     result.Add(format('%20s %6s,%6s, %s', ['kxorq', 'K2', OItem1.Values[il_Op1], 'K2']));

                                     result.Add(AsmCodeBlockCompare(iAsmCounter, cmXORTestNZ));
                                   end;
                            omKB32I,
                            omKB64I:
                                   begin
                                     result.Add(format('%20s %6s,%6s + $2000, %s', [aInst, 'K2', OItem2.Values[il_Op2], OItem3.Values[il_Op3] ]));
                                     result.Add(format('%20s %6s,%6s, %s', ['kxorq', 'K2', OItem1.Values[il_Op1], 'K2']));

                                     result.Add(AsmCodeBlockCompare(iAsmCounter, cmXORTestNZ));
                                   end;

                             omMX: begin
                                     result.Add(format('%20s %6s + $2000, %s', [aInst, OItem1.Values[il_Op1], OItem2.Values[il_Op2]]));
                                     result.Add(format('%20s %6s, %s',         ['vmovdqu', 'xmm0', OItem1.Values[il_Op1]]));
                                     result.Add(format('%20s %6s, %s + $2000', ['vmovdqu', 'xmm1', OItem1.Values[il_Op1]]));

                                     result.Add(format('%20s %6s, %6s, %s',    ['vpcmpeqw', 'K2', 'XMM0', 'XMM1']));

                                     result.Add(AsmCodeBlockCompare(iAsmCounter, cmKORTESTNC));
                                   end;

                            omMXI: begin
                                     result.Add(format('%20s %6s + $2000, %6s, %s', [aInst, OItem1.Values[il_Op1], OItem2.Values[il_Op2], OItem3.Values[il_Op3] ]));
                                     result.Add(format('%20s %6s, %s',              ['vmovdqu', 'xmm0', OItem1.Values[il_Op1]]));
                                     result.Add(format('%20s %6s, %s + $2000',      ['vmovdqu', 'xmm1', OItem1.Values[il_Op1]]));

                                     result.Add(format('%20s %6s, %6s, %s',         ['vpcmpeqw', 'K2', 'XMM0', 'XMM1']));

                                     result.Add(AsmCodeBlockCompare(iAsmCounter, cmKORTESTNC));
                                   end;

                             omMY: begin
                                     result.Add(format('%20s %6s + $2000, %s', [aInst, OItem1.Values[il_Op1], OItem2.Values[il_Op2]]));
                                     result.Add(format('%20s %6s, %s',         ['vmovdqu', 'ymm0', OItem1.Values[il_Op1]]));
                                     result.Add(format('%20s %6s, %s + $2000', ['vmovdqu', 'ymm1', OItem1.Values[il_Op1]]));

                                     result.Add(format('%20s %6s, %6s, %s',    ['vpcmpeqd', 'K2', 'YMM0', 'YMM1']));

                                     result.Add(AsmCodeBlockCompare(iAsmCounter, cmKORTESTNC));
                                   end;
                            omMYI: begin
                                     result.Add(format('%20s %6s + $2000, %6s, %s', [aInst, OItem1.Values[il_Op1], OItem2.Values[il_Op2], OItem3.Values[il_Op3] ]));
                                     result.Add(format('%20s %6s, %s',              ['vmovdqu', 'ymm0', OItem1.Values[il_Op1]]));
                                     result.Add(format('%20s %6s, %s + $2000',      ['vmovdqu', 'ymm1', OItem1.Values[il_Op1]]));

                                     result.Add(format('%20s %6s, %6s, %s',         ['vpcmpeqd', 'K2', 'YMM0', 'YMM1']));

                                     result.Add(AsmCodeBlockCompare(iAsmCounter, cmKORTESTNC));
                                   end;

                            omMZ: begin
                                    result.Add(format('%20s %6s + $2000, %s', [aInst, OItem1.Values[il_Op1], OItem2.Values[il_Op2]]));
                                    result.Add(format('%20s %6s, %s',         ['vmovdqu8', 'zmm0', OItem1.Values[il_Op1]]));
                                    result.Add(format('%20s %6s, %s + $2000', ['vmovdqu8', 'zmm1', OItem1.Values[il_Op1]]));

                                    result.Add(format('%20s %6s, %6s, %s',    ['vpcmpeqq', 'K2', 'ZMM0', 'ZMM1']));

                                    result.Add(AsmCodeBlockCompare(iAsmCounter, cmKORTESTNC));
                                  end;

                            omMZI: begin
                                     result.Add(format('%20s %6s + $2000, %6s, %s', [aInst, OItem1.Values[il_Op1], OItem2.Values[il_Op2], OItem3.Values[il_Op3] ]));
                                     result.Add(format('%20s %6s, %s',              ['vmovdqu8', 'zmm0', OItem1.Values[il_Op1]]));
                                     result.Add(format('%20s %6s, %s + $2000',      ['vmovdqu8', 'zmm1', OItem1.Values[il_Op1]]));

                                     result.Add(format('%20s %6s, %6s, %s',         ['vpcmpeqq', 'K2', 'ZMM0', 'ZMM1']));

                                     result.Add(AsmCodeBlockCompare(iAsmCounter, cmKORTESTNC));
                                   end;

                           omXB32,
                           omXB64: begin
                                     sMREF := OItem2.Values[il_Op2];
                                     result.Add(format('%20s%6s,%6s + $2000', [aInst, 'XMM1', sMREF]));
                                     result.Add(format('%20s%6s,%6s, %s',         ['vpcmpeqw', 'K2', OItem1.Values[il_Op1], 'XMM1']));

                                     result.Add(AsmCodeBlockCompare(iAsmCounter, cmKORTESTNC));
                                   end;


                          omXB32I,
                          omXB64I: begin
                                     sMREF := OItem2.Values[il_Op2];
                                     //if Pos('{1to4}', sMREF) > 0 then sMREF := StringReplace(sMREF, '{1to4}', '{1to2}', [])
                                     // else if Pos('{1to8}', sMREF) > 0 then sMREF := StringReplace(sMREF, '{1to8}', '{1to4}', [])
                                     // else if Pos('{1to16}', sMREF) > 0 then sMREF := StringReplace(sMREF, '{1to16}', '{1to8}', []);

                                     result.Add(format('%20s%6s,%6s + $2000, %s', [aInst, 'XMM1', sMREF, OItem3.Values[il_Op3]]));
                                     result.Add(format('%20s%6s,%6s, %s',         ['vpcmpeqw', 'K2', OItem1.Values[il_Op1], 'XMM1']));

                                     result.Add(AsmCodeBlockCompare(iAsmCounter, cmKORTESTNC));
                                   end;

                             omXM: begin
                                     result.Add(format('%20s %6s, %s + $2000', [aInst, 'XMM1', OItem2.Values[il_Op2] ]));
                                     result.Add(format('%20s %6s, %6s, %s',         ['vpcmpeqw', 'K2', OItem1.Values[il_Op1], 'XMM1']));

                                     result.Add(AsmCodeBlockCompare(iAsmCounter, cmKORTESTNC));
                                   end;
                            omXXM: begin

                                     result.Add(format('%20s %6s,%6s, %s + $2000', [aInst, 'XMM1', 'XMM1', OItem3.Values[il_Op3] ]));
                                     result.Add(format('%20s %6s,%6s, %s',         ['vpcmpeqw', 'K2', OItem1.Values[il_Op1], 'XMM1']));

                                     result.Add(AsmCodeBlockCompare(iAsmCounter, cmKORTESTNC));
                                   end;
                          omXXB32,
                          omXXB64: begin
                                     sMREF := OItem3.Values[il_Op3];
                                     //if Pos('{1to4}', sMREF) > 0 then sMREF := StringReplace(sMREF, '{1to4}', '{1to2}', [])
                                     // else if Pos('{1to8}', sMREF) > 0 then sMREF := StringReplace(sMREF, '{1to8}', '{1to4}', [])
                                     // else if Pos('{1to16}', sMREF) > 0 then sMREF := StringReplace(sMREF, '{1to16}', '{1to8}', []);

                                     result.Add(format('%20s %6s,%6s, %s + $2000', [aInst, 'XMM1', 'XMM1', sMREF]));
                                     result.Add(format('%20s %6s,%6s, %s',         ['vpcmpeqw', 'K2', OItem1.Values[il_Op1], 'XMM1']));

                                     result.Add(AsmCodeBlockCompare(iAsmCounter, cmKORTESTNC));
                                   end;
                            omXMI: begin
                                     result.Add(format('%20s %6s,%6s + $2000, %s', [aInst, 'XMM1', OItem2.Values[il_Op2], OItem3.Values[il_Op3]]));
                                     result.Add(format('%20s %6s,%6s, %s',       ['vpcmpeqw', 'K2', OItem1.Values[il_Op1], 'XMM1']));

                                     result.Add(AsmCodeBlockCompare(iAsmCounter, cmKORTESTNC));
                                   end;

                            omYB32,
                            omYB64: begin
                                      sMREF := OItem2.Values[il_Op2];
                                      result.Add(format('%20s%6s,%6s + $2000', [aInst, 'YMM1', sMREF]));
                                      result.Add(format('%20s%6s,%6s, %s',         ['vpcmpeqd', 'K2', OItem1.Values[il_Op1], 'YMM1']));

                                      result.Add(AsmCodeBlockCompare(iAsmCounter, cmKORTESTNC));
                                    end;
                            omYB32I,
                            omYB64I:
                                   begin
                                     sMREF := OItem2.Values[il_Op2];
                                     //if Pos('{1to4}', sMREF) > 0 then sMREF := StringReplace(sMREF, '{1to4}', '{1to2}', [])
                                     // else if Pos('{1to8}', sMREF) > 0 then sMREF := StringReplace(sMREF, '{1to8}', '{1to4}', [])
                                     // else if Pos('{1to16}', sMREF) > 0 then sMREF := StringReplace(sMREF, '{1to16}', '{1to8}', []);

                                     //result.Add(format('%20s%6s,%6s + $00, %s', [aInst, 'XMM1', sMREF, OItem3.Values[il_Op3]]));
                                     //result.Add(format('%20s%6s,%6s + $10, %s', [aInst, 'XMM2', sMREF, OItem3.Values[il_Op3]]));
                                     //
                                     //result.Add(format('%20s%6s,%6s, %s, 01',   ['vinserti32x4', 'YMM1', 'YMM1', 'XMM2']));
                                     //result.Add(format('%20s%6s,%6s, %s',       ['vpcmpeqb', 'K2', OItem1.Values[il_Op1], 'YMM1']));
                                     //result.Add(format('%20s%6s,%6s, %s',       ['kandq', 'K1', 'K1', 'K2']));
                                     //result.Add('');
                                     result.Add(format('%20s%6s,%6s + $2000, %s', [aInst, 'YMM1', sMREF, OItem3.Values[il_Op3]]));
                                     result.Add(format('%20s%6s,%6s, %s',         ['vpcmpeqd', 'K2', OItem1.Values[il_Op1], 'YMM1']));

                                     result.Add(AsmCodeBlockCompare(iAsmCounter, cmKORTESTNC));
                                   end;
                             omYM: begin
                                     result.Add(format('%20s %6s, %s + $2000', [aInst, 'YMM1', OItem2.Values[il_Op2] ]));
                                     result.Add(format('%20s %6s, %6s, %s',         ['vpcmpeqd', 'K2', OItem1.Values[il_Op1], 'YMM1']));

                                     result.Add(AsmCodeBlockCompare(iAsmCounter, cmKORTESTNC));
                                   end;

                            omYYM: begin
                                     //result.Add(format('%20s%6s,%6s, %s + $00', [aInst, 'XMM1', 'XMM1', OItem3.Values[il_Op3]]));
                                     //result.Add(format('%20s%6s,%6s, %s + $10', [aInst, 'XMM2', 'XMM2', OItem3.Values[il_Op3]]));
                                     //
                                     //result.Add(format('%20s%6s,%6s, %s, 01',   ['vinserti32x4', 'YMM1', 'YMM1', 'XMM2']));
                                     //result.Add(format('%20s%6s,%6s, %s',       ['vpcmpeqb', 'K2', OItem1.Values[il_Op1], 'YMM1']));
                                     //result.Add(format('%20s%6s,%6s, %s',       ['kandq', 'K1', 'K1', 'K2']));
                                     //result.Add('');

                                     result.Add(format('%20s %6s,%6s, %s + $2000', [aInst, 'YMM1', 'YMM1', OItem3.Values[il_Op3] ]));
                                     result.Add(format('%20s %6s,%6s, %s',         ['vpcmpeqd', 'K2', OItem1.Values[il_Op1], 'YMM1']));

                                     result.Add(AsmCodeBlockCompare(iAsmCounter, cmKORTESTNC));
                                   end;
                          omYYB32,
                          omYYB64: begin
                                     sMREF := OItem3.Values[il_Op3];
                                     //if Pos('{1to4}', sMREF) > 0 then sMREF := StringReplace(sMREF, '{1to4}', '{1to2}', [])
                                     // else if Pos('{1to8}', sMREF) > 0 then sMREF := StringReplace(sMREF, '{1to8}', '{1to4}', [])
                                     // else if Pos('{1to16}', sMREF) > 0 then sMREF := StringReplace(sMREF, '{1to16}', '{1to8}', []);

                                     //result.Add(format('%20s%6s,%6s, %s + $00', [aInst, 'XMM1', 'XMM1', sMREF]));
                                     //result.Add(format('%20s%6s,%6s, %s + $10', [aInst, 'XMM2', 'XMM2', sMREF]));
                                     //
                                     //result.Add(format('%20s%6s,%6s, %s, 01',   ['vinserti32x4', 'YMM1', 'YMM1', 'XMM2']));
                                     //result.Add(format('%20s%6s,%6s, %s',       ['vpcmpeqb', 'K2', OItem1.Values[il_Op1], 'YMM1']));
                                     //result.Add(format('%20s%6s,%6s, %s',       ['kandq', 'K1', 'K1', 'K2']));
                                     //result.Add('');
                                     result.Add(format('%20s %6s,%6s, %s + $2000', [aInst, 'YMM1', 'YMM1', sMREF]));
                                     result.Add(format('%20s %6s,%6s, %s',         ['vpcmpeqd', 'K2', OItem1.Values[il_Op1], 'YMM1']));

                                     result.Add(AsmCodeBlockCompare(iAsmCounter, cmKORTESTNC));

                                   end;
                            omYMI: begin
                                     //result.Add(format('%20s%6s,%6s + $00, %s', [aInst, 'XMM1', OItem2.Values[il_Op2], OItem3.Values[il_Op3]]));
                                     //result.Add(format('%20s%6s,%6s + $10, %s', [aInst, 'XMM2', OItem2.Values[il_Op2], OItem3.Values[il_Op3]]));
                                     //
                                     //result.Add(format('%20s%6s,%6s, %s, 01',   ['vinserti32x4', 'YMM1', 'YMM1', 'XMM2']));
                                     //result.Add(format('%20s%6s,%6s, %s',       ['vpcmpeqb', 'K2', OItem1.Values[il_Op1], 'YMM1']));
                                     //result.Add(format('%20s%6s,%6s, %s',       ['kandq', 'K1', 'K1', 'K2']));
                                     //result.Add('');
                                     result.Add(format('%20s %6s,%6s + $2000, %s', [aInst, 'YMM1', OItem2.Values[il_Op2], OItem3.Values[il_Op3]]));
                                     result.Add(format('%20s %6s,%6s, %s',       ['vpcmpeqd', 'K2', OItem1.Values[il_Op1], 'YMM1']));

                                     result.Add(AsmCodeBlockCompare(iAsmCounter, cmKORTESTNC));
                                   end;

                            omZB32,
                            omZB64: begin
                                      sMREF := OItem2.Values[il_Op2];
                                      result.Add(format('%20s%6s,%6s + $2000', [aInst, 'ZMM1', sMREF]));
                                      result.Add(format('%20s%6s,%6s, %s',         ['vpcmpeqq', 'K2', OItem1.Values[il_Op1], 'ZMM1']));

                                      result.Add(AsmCodeBlockCompare(iAsmCounter, cmKORTESTNC));
                                    end;
                            omZB32I,
                            omZB64I:
                                    begin
                                      sMREF := OItem2.Values[il_Op2];
                                       //if Pos('{1to8}', sMREF) > 0 then sMREF := StringReplace(sMREF, '{1to8}', '{1to2}', [])
                                       // else if Pos('{1to16}', sMREF) > 0 then sMREF := StringReplace(sMREF, '{1to16}', '{1to4}', []);

                                       //result.Add(format('%20s%6s,%6s + $00, %s', [aInst, 'XMM1', sMREF, OItem3.Values[il_Op3]]));
                                       //result.Add(format('%20s%6s,%6s + $10, %s', [aInst, 'XMM2', sMREF, OItem3.Values[il_Op3]]));
                                       //result.Add(format('%20s%6s,%6s + $20, %s', [aInst, 'XMM3', sMREF, OItem3.Values[il_Op3]]));
                                       //
                                       //result.Add(format('%20s%6s,%6s, %s, 01',   ['vinserti32x4', 'ZMM1', 'ZMM1', 'XMM2']));
                                       //result.Add(format('%20s%6s,%6s, %s, 02',   ['vinserti32x4', 'ZMM1', 'ZMM1', 'XMM3']));
                                       //result.Add(format('%20s%6s,%6s, %s, 03',   ['vinserti32x4', 'ZMM1', 'ZMM1', 'XMM4']));
                                       //
                                       //result.Add(format('%20s%6s,%6s, %s',       ['vpcmpeqb', 'K2', OItem1.Values[il_Op1], 'ZMM1']));
                                       //result.Add(format('%20s%6s,%6s, %s',       ['kandq', 'K1', 'K1', 'K2']));
                                       //result.Add('');

                                       result.Add(format('%20s %6s,%6s + $2000, %s', [aInst, 'ZMM1', sMREF, OItem3.Values[il_Op3]]));
                                       result.Add(format('%20s %6s,%6s, %s',         ['vpcmpeqq', 'K2', OItem1.Values[il_Op1], 'ZMM1']));

                                       result.Add(AsmCodeBlockCompare(iAsmCounter, cmKORTESTNC));

                                     end;
                             omZM: begin
                                     result.Add(format('%20s %6s, %s + $2000', [aInst, 'ZMM1', OItem2.Values[il_Op2] ]));
                                     result.Add(format('%20s %6s, %6s, %s',         ['vpcmpeqq', 'K2', OItem1.Values[il_Op1], 'ZMM1']));

                                     result.Add(AsmCodeBlockCompare(iAsmCounter, cmKORTESTNC));
                                   end;
                            omZZM: begin
                                     //result.Add(format('%20s%6s,%6s, %s + $00', [aInst, 'XMM1', 'XMM1', OItem3.Values[il_Op3]]));
                                     //result.Add(format('%20s%6s,%6s, %s + $10', [aInst, 'XMM2', 'XMM2', OItem3.Values[il_Op3]]));
                                     //result.Add(format('%20s%6s,%6s, %s + $20', [aInst, 'XMM3', 'XMM3', OItem3.Values[il_Op3]]));
                                     //result.Add(format('%20s%6s,%6s, %s + $30', [aInst, 'XMM4', 'XMM4', OItem3.Values[il_Op3]]));
                                     //
                                     //result.Add(format('%20s%6s,%6s, %s, 01',   ['vinserti32x4', 'ZMM1', 'ZMM1', 'XMM2']));
                                     //result.Add(format('%20s%6s,%6s, %s, 02',   ['vinserti32x4', 'ZMM1', 'ZMM1', 'XMM3']));
                                     //result.Add(format('%20s%6s,%6s, %s, 03',   ['vinserti32x4', 'ZMM1', 'ZMM1', 'XMM4']));
                                     //
                                     //result.Add(format('%20s%6s,%6s, %s',       ['vpcmpeqb', 'K2', OItem1.Values[il_Op1], 'ZMM1']));
                                     //result.Add(format('%20s%6s,%6s, %s',       ['kandq', 'K1', 'K1', 'K2']));
                                     //result.Add('');

                                     result.Add(format('%20s %6s,%6s, %s + $2000', [aInst, 'ZMM1', 'ZMM1', OItem3.Values[il_Op3] ]));
                                     result.Add(format('%20s %6s,%6s, %s',         ['vpcmpeqq', 'K2', OItem1.Values[il_Op1], 'ZMM1']));

                                     result.Add(AsmCodeBlockCompare(iAsmCounter, cmKORTESTNC));
                                   end;
                          omZZB32,
                          omZZB64: begin
                                     sMREF := OItem3.Values[il_Op3];
                                     //if Pos('{1to8}', sMREF) > 0 then sMREF := StringReplace(sMREF, '{1to8}', '{1to2}', [])
                                     // else if Pos('{1to16}', sMREF) > 0 then sMREF := StringReplace(sMREF, '{1to16}', '{1to4}', []);

                                     //result.Add(format('%20s%6s,%6s, %s + $00', [aInst, 'XMM1', 'XMM1', sMREF]));
                                     //result.Add(format('%20s%6s,%6s, %s + $10', [aInst, 'XMM2', 'XMM2', sMREF]));
                                     //result.Add(format('%20s%6s,%6s, %s + $20', [aInst, 'XMM3', 'XMM3', sMREF]));
                                     //result.Add(format('%20s%6s,%6s, %s + $30', [aInst, 'XMM4', 'XMM4', sMREF]));
                                     //
                                     //result.Add(format('%20s%6s,%6s, %s, 01',   ['vinserti32x4', 'ZMM1', 'ZMM1', 'XMM2']));
                                     //result.Add(format('%20s%6s,%6s, %s, 02',   ['vinserti32x4', 'ZMM1', 'ZMM1', 'XMM3']));
                                     //result.Add(format('%20s%6s,%6s, %s, 03',   ['vinserti32x4', 'ZMM1', 'ZMM1', 'XMM4']));
                                     //
                                     //result.Add(format('%20s%6s,%6s, %s',       ['vpcmpeqb', 'K2', OItem1.Values[il_Op1], 'ZMM1']));
                                     //result.Add(format('%20s%6s,%6s, %s',       ['kandq', 'K1', 'K1', 'K2']));
                                     //result.Add('');

                                     result.Add(format('%20s %6s,%6s, %s + $2000', [aInst, 'ZMM1', 'ZMM1', sMREF]));
                                     result.Add(format('%20s %6s,%6s, %s',         ['vpcmpeqq', 'K2', OItem1.Values[il_Op1], 'ZMM1']));

                                     result.Add(AsmCodeBlockCompare(iAsmCounter, cmKORTESTNC));

                                   end;
                            omZMI: begin
                                     //result.Add(format('%20s%6s,%6s + $00, %s', [aInst, 'XMM1', OItem2.Values[il_Op2], OItem3.Values[il_Op3]]));
                                     //result.Add(format('%20s%6s,%6s + $10, %s', [aInst, 'XMM2', OItem2.Values[il_Op2], OItem3.Values[il_Op3]]));
                                     //result.Add(format('%20s%6s,%6s + $20, %s', [aInst, 'XMM3', OItem2.Values[il_Op2], OItem3.Values[il_Op3]]));
                                     //result.Add(format('%20s%6s,%6s + $30, %s', [aInst, 'XMM4', OItem2.Values[il_Op2], OItem3.Values[il_Op3]]));
                                     //
                                     //result.Add(format('%20s%6s,%6s, %s, 01',   ['vinserti32x4', 'ZMM1', 'ZMM1', 'XMM2']));
                                     //result.Add(format('%20s%6s,%6s, %s, 02',   ['vinserti32x4', 'ZMM1', 'ZMM1', 'XMM3']));
                                     //result.Add(format('%20s%6s,%6s, %s, 03',   ['vinserti32x4', 'ZMM1', 'ZMM1', 'XMM4']));
                                     //
                                     //result.Add(format('%20s%6s,%6s, %s',       ['vpcmpeqb', 'K2', OItem1.Values[il_Op1], 'ZMM1']));
                                     //result.Add(format('%20s%6s,%6s, %s',       ['kandq', 'K1', 'K1', 'K2']));
                                     //result.Add('');
                                     result.Add(format('%20s %6s,%6s + $2000, %s', [aInst, 'ZMM1', OItem2.Values[il_Op2], OItem3.Values[il_Op3]]));
                                     result.Add(format('%20s %6s,%6s, %s',       ['vpcmpeqq', 'K2', OItem1.Values[il_Op1], 'ZMM1']));

                                     result.Add(AsmCodeBlockCompare(iAsmCounter, cmKORTESTNC));
                                   end;

                            omKXB32I,
                            omKXB64I:
                                   begin
                                     result.Add(format('%20s %6s, %6s, %6s + $2000, %s', [aInst, 'K2', OItem2.Values[il_Op2], OItem3.Values[il_Op3], OItem4.Values[il_Op4]]));
                                     result.Add(format('%20s %6s,%6s, %s', ['kxorq', 'K2', OItem1.Values[il_Op1], 'K2']));

                                     result.Add(AsmCodeBlockCompare(iAsmCounter, cmXORTestNZ));
                                   end;
                             omKXMI:
                                   begin
                                     result.Add(format('%20s %6s, %6s, %6s + $2000, %s', [aInst, 'K2', OItem2.Values[il_Op2], OItem3.Values[il_Op3], OItem4.Values[il_Op4]]));
                                     result.Add(format('%20s %6s,%6s, %s', ['kxorq', 'K2', OItem1.Values[il_Op1], 'K2']));

                                     result.Add(AsmCodeBlockCompare(iAsmCounter, cmXORTestNZ));
                                   end;
                           omKYB32I,
                           omKYB64I:
                                 begin
                                   result.Add(format('%20s %6s, %6s, %6s + $2000, %s', [aInst, 'K2', OItem2.Values[il_Op2], OItem3.Values[il_Op3], OItem4.Values[il_Op4]]));
                                   result.Add(format('%20s %6s,%6s, %s', ['kxorq', 'K2', OItem1.Values[il_Op1], 'K2']));

                                   result.Add(AsmCodeBlockCompare(iAsmCounter, cmXORTestNZ));
                                 end;
                             omKYMI:
                                 begin
                                   result.Add(format('%20s %6s, %6s, %6s + $2000, %s', [aInst, 'K2', OItem2.Values[il_Op2], OItem3.Values[il_Op3], OItem4.Values[il_Op4]]));
                                   result.Add(format('%20s %6s,%6s, %s', ['kxorq', 'K2', OItem1.Values[il_Op1], 'K2']));

                                   result.Add(AsmCodeBlockCompare(iAsmCounter, cmXORTestNZ));
                                 end;

                            omXXB32I,
                            omXXB64I:
                                   begin
                                     sMREF := OItem3.Values[il_Op3];

                                     result.Add(format('%20s %6s, %6s, %6s + $2000, %s', [aInst, 'XMM1', OItem2.Values[il_Op2], sMREF, OItem4.Values[il_Op4]]));
                                     result.Add(format('%20s %6s, %6s, %s',         ['vpcmpeqw', 'K2', OItem1.Values[il_Op1], 'XMM1']));

                                     result.Add(AsmCodeBlockCompare(iAsmCounter, cmKORTESTNC));
                                   end;
                             omXXMI:
                                   begin
                                     sMREF := OItem3.Values[il_Op3];

                                     result.Add(format('%20s %6s, %6s, %6s + $2000, %s', [aInst, 'XMM1', OItem2.Values[il_Op2], sMREF, OItem4.Values[il_Op4]]));
                                     result.Add(format('%20s %6s, %6s, %s',              ['vpcmpeqw', 'K2', OItem1.Values[il_Op1], 'XMM1']));

                                     result.Add(AsmCodeBlockCompare(iAsmCounter, cmKORTESTNC));
                                   end;
                             omYYB32I,
                             omYYB64I,
                               omYYMI:
                                   begin
                                     sMREF := OItem3.Values[il_Op3];

                                     result.Add(format('%20s %6s, %6s, %6s + $2000, %s', [aInst, 'YMM1', OItem2.Values[il_Op2], sMREF, OItem4.Values[il_Op4]]));
                                     result.Add(format('%20s %6s, %6s, %s',         ['vpcmpeqd', 'K2', OItem1.Values[il_Op1], 'YMM1']));

                                     result.Add(AsmCodeBlockCompare(iAsmCounter, cmKORTESTNC));
                                   end;
                            omKZB32I,
                            omKZB64I,
                              omKZMI:
                                  begin
                                    result.Add(format('%20s %6s, %6s, %6s + $2000, %s', [aInst, 'K2', OItem2.Values[il_Op2], OItem3.Values[il_Op3], OItem4.Values[il_Op4]]));
                                    result.Add(format('%20s %6s, %6s, %s', ['kxorq', 'K2', OItem1.Values[il_Op1], 'K2']));

                                    result.Add(AsmCodeBlockCompare(iAsmCounter, cmXORTestNZ));
                                  end;
                            //omKZMI:
                            //      begin
                            //        sMREF := OItem3.Values[il_Op3];
                            //
                            //        result.Add(format('%20s %6s, %6s, %6s + $2000, %s', [aInst, 'ZMM1', OItem2.Values[il_Op2], sMREF, OItem4.Values[il_Op4]]));
                            //        result.Add(format('%20s %6s, %6s, %6s, %s',         ['vpcmpeqq', 'K2', OItem1.Values[il_Op1], 'ZMM1']));
                            //
                            //        result.Add(AsmCodeBlockCompare(iAsmCounter, cmKORTESTNC));
                            //      end;

                            omZZB32I,
                            omZZB64I,
                              omZZMI:
                                  begin
                                    sMREF := OItem3.Values[il_Op3];

                                    result.Add(format('%20s %6s, %6s, %6s + $2000, %s', [aInst, 'ZMM1', OItem2.Values[il_Op2], sMREF, OItem4.Values[il_Op4]]));
                                    result.Add(format('%20s %6s, %6s, %s',              ['vpcmpeqq', 'K2', OItem1.Values[il_Op1], 'ZMM1']));

                                    result.Add(AsmCodeBlockCompare(iAsmCounter, cmKORTESTNC));
                                  end;
                             else begin
                                    sLogMsg := '';
                                    sLogMsg := MapOperand(OItem1.Optyp) + MapOperand(OItem2.Optyp) + MapOperand(OItem3.Optyp);
                                    writeln('offen - : ' + sLogMsg + ' (' + aInst + ')');
                                  end;
                        end;

                        sRegCombi := '';
                      end;
                    end;
                  end;
                end;
              end;
            end;



            //sl_Inst := format('%-20s', [aInst]);
            //
            //for il_Op1 := 0 to OItem1.Values.Count - 1 do
            //begin
            //  if OItem1.OpActive then
            //  begin
            //    sl_RegCombi1 := OItem1.Values[il_Op1];
            //  end
            //  else sl_RegCombi1 := '';
            //
            //  for il_Op2 := 0 to OItem2.Values.Count - 1 do
            //  begin
            //    if OItem2.OpActive then
            //    begin
            //      sl_RegCombi2 := sl_RegCombi1 + ', ' + OItem2.Values[il_Op2];
            //    end
            //    else sl_RegCombi2 := sl_RegCombi1;
            //
            //    for il_Op3 := 0 to OItem3.Values.Count - 1 do
            //    begin
            //      if OItem3.OpActive then
            //      begin
            //        sl_RegCombi3 := sl_RegCombi2 + ', ' + OItem3.Values[il_Op3];
            //      end
            //      else sl_RegCombi3 := sl_RegCombi2;
            //
            //      for il_Op4 := 0 to OItem4.Values.Count - 1 do
            //      begin
            //        if OItem4.OpActive then
            //        begin
            //          sl_RegCombi := sl_RegCombi3 + ', ' + OItem4.Values[il_Op4];
            //        end
            //        else sl_RegCombi := sl_RegCombi3;
            //
            //        if (sl_RegCombi <> '') and
            //           (
            //            (OItem1.OpActive and (OItem1.OpTyp in OTMEMTYPES)) or
            //            (OItem2.OpActive and (OItem2.OpTyp in OTMEMTYPES)) or
            //            (OItem3.OpActive and (OItem3.OpTyp in OTMEMTYPES)) or
            //            (OItem4.OpActive and (OItem4.OpTyp in OTMEMTYPES))
            //           ) then
            //        begin
            //          //result.Add(format('%-20s%s', [aInst, sl_RegCombi]));
            //          result.Add(sl_Inst + sl_RegCombi);
            //
            //
            //
            //          sl_RegCombi := '';
            //        end;
            //      end;
            //    end;
            //  end;
            //end;
          end;
        finally
          FreeAndNil(OItem4);
        end;
      finally
        FreeAndNil(OItem3);
      end;
    finally
      FreeAndNil(OItem2);
    end;
  finally
    FreeAndNil(OItem1);
  end;
end;


constructor TAsmTestGenerator.Create;
begin
  inherited;

  FX64 := true;
  FAVX512 := false;

  FReg8          := TStringList.Create;
  FReg16         := TStringList.Create;

  FReg32Base     := TStringList.Create;
  FReg32Index    := TStringList.Create;
  FReg64Base     := TStringList.Create;
  FReg64Index    := TStringList.Create;
  FReg6432Base   := TStringList.Create;
  FReg6432Index  := TStringList.Create;
  FReg32XMMIndex := TStringList.Create;
  FReg32YMMIndex := TStringList.Create;
  FReg32ZMMIndex := TStringList.Create;
  FReg64XMMIndex := TStringList.Create;
  FReg64YMMIndex := TStringList.Create;
  FReg64ZMMIndex := TStringList.Create;
  FRegKREG       := TStringList.Create;

  FReg8.Add('AL');
  FReg8.Add('BL');
  FReg8.Add('CL');
  FReg8.Add('DL');

  
  FReg16.Add('AX');
  FReg16.Add('BX');
  FReg16.Add('CX');
  FReg16.Add('DX');


  FReg32Base.Add('EAX');
  FReg32Base.Add('EBX');
  FReg32Base.Add('ECX');
  FReg32Base.Add('EDX');
  //FReg32Base.Add('ESP');
  //FReg32Base.Add('EBP');
  FReg32Base.Add('EDI');
  FReg32Base.Add('ESI');


  FReg32Index.Add('EAX');
  FReg32Index.Add('EBX');
  FReg32Index.Add('ECX');
  FReg32Index.Add('EDX');
  //FReg32Index.Add('EBP');
  FReg32Index.Add('EDI');
  FReg32Index.Add('ESI');


  FReg64Base.Add('RAX');
  FReg64Base.Add('RBX');
  FReg64Base.Add('RCX');
  FReg64Base.Add('RDX');
  //FReg64Base.Add('RSP');
  //FReg64Base.Add('RBP');
  FReg64Base.Add('RDI');
  FReg64Base.Add('RSI');
  FReg64Base.Add('R8');
  FReg64Base.Add('R9');
  FReg64Base.Add('R10');
  FReg64Base.Add('R11');
  FReg64Base.Add('R12');
  FReg64Base.Add('R13');
  FReg64Base.Add('R14');
  FReg64Base.Add('R15');

  FReg64Index.Add('RAX');
  FReg64Index.Add('RBX');
  FReg64Index.Add('RCX');
  FReg64Index.Add('RDX');
  //FReg64Index.Add('RBP');
  FReg64Index.Add('RDI');
  FReg64Index.Add('RSI');
  FReg64Index.Add('R8');
  FReg64Index.Add('R9');
  FReg64Index.Add('R10');
  FReg64Index.Add('R11');
  FReg64Index.Add('R12');
  FReg64Index.Add('R13');
  FReg64Index.Add('R14');
  FReg64Index.Add('R15');

  FReg6432Base.Add('EAX');
  FReg6432Base.Add('EBX');
  FReg6432Base.Add('ECX');
  FReg6432Base.Add('EDX');
  FReg6432Base.Add('ESP');
  //FReg6432Base.Add('EBP');
  FReg6432Base.Add('EDI');
  FReg6432Base.Add('ESI');
  FReg6432Base.Add('R8D');
  FReg6432Base.Add('R9D');
  FReg6432Base.Add('R10D');
  FReg6432Base.Add('R11D');
  FReg6432Base.Add('R12D');
  FReg6432Base.Add('R13D');
  FReg6432Base.Add('R14D');
  FReg6432Base.Add('R15D');

  FReg6432Index.Add('EAX');
  FReg6432Index.Add('EBX');
  FReg6432Index.Add('ECX');
  FReg6432Index.Add('EDX');
  //FReg6432Index.Add('EBP');
  FReg6432Index.Add('EDI');
  FReg6432Index.Add('ESI');
  FReg6432Index.Add('R8D');
  FReg6432Index.Add('R9D');
  FReg6432Index.Add('R10D');
  FReg6432Index.Add('R11D');
  FReg6432Index.Add('R12D');
  FReg6432Index.Add('R13D');
  FReg6432Index.Add('R14D');
  FReg6432Index.Add('R15D');

  FReg32XMMIndex.ADD('XMM0');
  FReg32XMMIndex.ADD('XMM1');
  FReg32XMMIndex.ADD('XMM2');
  FReg32XMMIndex.ADD('XMM3');
  FReg32XMMIndex.ADD('XMM4');
  FReg32XMMIndex.ADD('XMM5');
  FReg32XMMIndex.ADD('XMM6');
  FReg32XMMIndex.ADD('XMM7');

  FReg32YMMIndex.ADD('YMM0');
  FReg32YMMIndex.ADD('YMM1');
  FReg32YMMIndex.ADD('YMM2');
  FReg32YMMIndex.ADD('YMM3');
  FReg32YMMIndex.ADD('YMM4');
  FReg32YMMIndex.ADD('YMM5');
  FReg32YMMIndex.ADD('YMM6');
  FReg32YMMIndex.ADD('YMM7');

  FReg32ZMMIndex.ADD('ZMM0');
  FReg32ZMMIndex.ADD('ZMM1');
  FReg32ZMMIndex.ADD('ZMM2');
  FReg32ZMMIndex.ADD('ZMM3');
  FReg32ZMMIndex.ADD('ZMM4');
  FReg32ZMMIndex.ADD('ZMM5');
  FReg32ZMMIndex.ADD('ZMM6');
  FReg32ZMMIndex.ADD('ZMM7');


  FReg64XMMIndex.ADD('XMM0');
  FReg64XMMIndex.ADD('XMM1');
  FReg64XMMIndex.ADD('XMM2');
  FReg64XMMIndex.ADD('XMM3');
  FReg64XMMIndex.ADD('XMM4');
  FReg64XMMIndex.ADD('XMM5');
  FReg64XMMIndex.ADD('XMM6');
  FReg64XMMIndex.ADD('XMM7');
  FReg64XMMIndex.ADD('XMM8');
  FReg64XMMIndex.ADD('XMM9');
  FReg64XMMIndex.ADD('XMM10');
  FReg64XMMIndex.ADD('XMM11');
  FReg64XMMIndex.ADD('XMM12');
  FReg64XMMIndex.ADD('XMM13');
  FReg64XMMIndex.ADD('XMM14');
  FReg64XMMIndex.ADD('XMM15');


  FReg64YMMIndex.ADD('YMM0');
  FReg64YMMIndex.ADD('YMM1');
  FReg64YMMIndex.ADD('YMM2');
  FReg64YMMIndex.ADD('YMM3');
  FReg64YMMIndex.ADD('YMM4');
  FReg64YMMIndex.ADD('YMM5');
  FReg64YMMIndex.ADD('YMM6');
  FReg64YMMIndex.ADD('YMM7');
  FReg64YMMIndex.ADD('YMM8');
  FReg64YMMIndex.ADD('YMM9');
  FReg64YMMIndex.ADD('YMM10');
  FReg64YMMIndex.ADD('YMM11');
  FReg64YMMIndex.ADD('YMM12');
  FReg64YMMIndex.ADD('YMM13');
  FReg64YMMIndex.ADD('YMM14');
  FReg64YMMIndex.ADD('YMM15');

  FReg64ZMMIndex.ADD('ZMM0');
  FReg64ZMMIndex.ADD('ZMM1');
  FReg64ZMMIndex.ADD('ZMM2');
  FReg64ZMMIndex.ADD('ZMM3');
  FReg64ZMMIndex.ADD('ZMM4');
  FReg64ZMMIndex.ADD('ZMM5');
  FReg64ZMMIndex.ADD('ZMM6');
  FReg64ZMMIndex.ADD('ZMM7');
  FReg64ZMMIndex.ADD('ZMM8');
  FReg64ZMMIndex.ADD('ZMM9');
  FReg64ZMMIndex.ADD('ZMM10');
  FReg64ZMMIndex.ADD('ZMM11');
  FReg64ZMMIndex.ADD('ZMM12');
  FReg64ZMMIndex.ADD('ZMM13');
  FReg64ZMMIndex.ADD('ZMM14');
  FReg64ZMMIndex.ADD('ZMM15');
  FReg64ZMMIndex.ADD('ZMM16');
  FReg64ZMMIndex.ADD('ZMM17');
  FReg64ZMMIndex.ADD('ZMM18');
  FReg64ZMMIndex.ADD('ZMM19');
  FReg64ZMMIndex.ADD('ZMM20');
  FReg64ZMMIndex.ADD('ZMM21');
  FReg64ZMMIndex.ADD('ZMM22');
  FReg64ZMMIndex.ADD('ZMM23');
  FReg64ZMMIndex.ADD('ZMM24');
  FReg64ZMMIndex.ADD('ZMM25');
  FReg64ZMMIndex.ADD('ZMM26');
  FReg64ZMMIndex.ADD('ZMM27');
  FReg64ZMMIndex.ADD('ZMM28');
  FReg64ZMMIndex.ADD('ZMM29');
  FReg64ZMMIndex.ADD('ZMM30');
  FReg64ZMMIndex.ADD('ZMM31');

  FRegKREG.ADD('K0');
  FRegKREG.ADD('K1');
  FRegKREG.ADD('K2');
  FRegKREG.ADD('K3');
  FRegKREG.ADD('K4');
  FRegKREG.ADD('K5');
  FRegKREG.ADD('K6');
  FRegKREG.ADD('K7');

end;

destructor TAsmTestGenerator.Destroy;
begin
  FreeAndNil(FReg8);
  FreeAndNil(FReg16);

  FreeAndNil(FReg32Base);
  FreeAndNil(FReg32Index);
  FreeAndNil(FReg64Base);
  FreeAndNil(FReg64Index);
  FreeAndNil(FReg6432Base);
  FreeAndNil(FReg6432Index);

  FreeAndNil(FReg32XMMIndex);
  FreeAndNil(FReg32YMMIndex);
  FreeAndNil(FReg32ZMMIndex);
  FreeAndNil(FReg64XMMIndex);
  FreeAndNil(FReg64YMMIndex);
  FreeAndNil(FReg64ZMMIndex);

  FreeAndnil(FRegKREG);

  inherited;
end;

procedure TAsmTestGenerator.MemRegBaseIndexCombi(const aPrefix, aSuffix: String; aSLBaseReg,
  aSLIndexReg, aRList: TStringList);
var
  il_Base: integer;
  il_Index: integer;
begin

  for il_Base := 0 to aSLBaseReg.Count - 1 do
  begin
    aRList.Add(format(aPrefix + '[%s]%s', [aSLBaseReg[il_Base], aSuffix]));

    for il_Index := 0 to aSLIndexReg.Count - 1 do
    begin
      aRList.Add(format(aPrefix + '[%s + %s]%s', [aSLBaseReg[il_Base], aSLIndexReg[il_Index], aSuffix]));
      aRList.Add(format(aPrefix + '[%s + %s + $10]%s', [aSLBaseReg[il_Base], aSLIndexReg[il_Index], aSuffix]));
      aRList.Add(format(aPrefix + '[%s + %s + $40]%s', [aSLBaseReg[il_Base], aSLIndexReg[il_Index], aSuffix]));
      aRList.Add(format(aPrefix + '[%s + %s - $10]%s', [aSLBaseReg[il_Base], aSLIndexReg[il_Index], aSuffix]));
      aRList.Add(format(aPrefix + '[%s + %s - $40]%s', [aSLBaseReg[il_Base], aSLIndexReg[il_Index], aSuffix]));


      aRList.Add(format(aPrefix + '[%s + %s * 2]%s', [aSLBaseReg[il_Base], aSLIndexReg[il_Index], aSuffix]));
      aRList.Add(format(aPrefix + '[%s + %s * 4]%s', [aSLBaseReg[il_Base], aSLIndexReg[il_Index], aSuffix]));
      aRList.Add(format(aPrefix + '[%s + %s * 8]%s', [aSLBaseReg[il_Base], aSLIndexReg[il_Index], aSuffix]));

      aRList.Add(format(aPrefix + '[%s + %s * 2 + 16]%s', [aSLBaseReg[il_Base], aSLIndexReg[il_Index], aSuffix]));
      aRList.Add(format(aPrefix + '[%s + %s * 4 + 32]%s', [aSLBaseReg[il_Base], aSLIndexReg[il_Index], aSuffix]));
      aRList.Add(format(aPrefix + '[%s + %s * 8 + 64]%s', [aSLBaseReg[il_Base], aSLIndexReg[il_Index], aSuffix]));
    end;
  end;
end;

procedure TAsmTestGenerator.MemRegBaseIndexCombiCDISP8N(const aPrefix,
  aSuffix: String; aSLBaseReg, aSLIndexReg, aRList: TStringList);
var
  iBase: integer;
  iIndex: integer;
  iOffset: integer;
begin

  for iBase := 0 to aSLBaseReg.Count - 1 do
  begin
    for iOffset := 0 to 63 do
    begin
      aRList.Add(format(aPrefix + '[%s +%2d]', [aSLBaseReg[iBase], iOffset]));

      for iIndex := 0 to aSLIndexReg.Count - 1 do
      begin
        if aSLBaseReg[iBase] <> aSLIndexReg[iIndex] then
        begin
          aRList.Add(format(aPrefix + '[%s + %s + %2d]', [aSLBaseReg[iBase], aSLIndexReg[iIndex], iOffset]));
          aRList.Add(format(aPrefix + '[%s + %s * 2 + %2d]', [aSLBaseReg[iBase], aSLIndexReg[iIndex], iOffset]));
          aRList.Add(format(aPrefix + '[%s + %s * 4 + %2d]', [aSLBaseReg[iBase], aSLIndexReg[iIndex], iOffset]));
          aRList.Add(format(aPrefix + '[%s + %s * 8 + %2d]', [aSLBaseReg[iBase], aSLIndexReg[iIndex], iOffset]));
        end;
      end;
    end;
  end;
end;

procedure TAsmTestGenerator.VectorMemRegBaseIndexCombi(const aPrefix, aSuffix: String;
  aSLBaseReg, aSLIndexReg, aRList: TStringList);
var
  il_Base: integer;
  il_Index: integer;
begin

  //for il_Index := 0 to aSLIndexReg.Count - 1 do
  //begin
  //  aRList.Add(format(aPrefix + '[%s]', [aSLIndexReg[il_Index]]));
  //
  //  aRList.Add(format(aPrefix + '[%s * 2]', [aSLIndexReg[il_Index]]));
  //  aRList.Add(format(aPrefix + '[%s * 4]', [aSLIndexReg[il_Index]]));
  //  aRList.Add(format(aPrefix + '[%s * 8]', [aSLIndexReg[il_Index]]));
  //
  //  aRList.Add(format(aPrefix + '[%s * 2 + 16]', [aSLIndexReg[il_Index]]));
  //  aRList.Add(format(aPrefix + '[%s * 4 + 32]', [aSLIndexReg[il_Index]]));
  //  aRList.Add(format(aPrefix + '[%s * 8 + 48]', [aSLIndexReg[il_Index]]));
  //end;


  for il_Base := 0 to aSLBaseReg.Count - 1 do
  begin
    //aRList.Add(format(aPrefix + '[%s]', [aSLBaseReg[il_Base]]));

    for il_Index := 0 to aSLIndexReg.Count - 1 do
    begin
      aRList.Add(format(aPrefix + '[%s + %s]%s', [aSLBaseReg[il_Base], aSLIndexReg[il_Index], aSuffix]));

      aRList.Add(format(aPrefix + '[%s + %s * 2]%s', [aSLBaseReg[il_Base], aSLIndexReg[il_Index], aSuffix]));
      aRList.Add(format(aPrefix + '[%s + %s * 4]%s', [aSLBaseReg[il_Base], aSLIndexReg[il_Index], aSuffix]));
      aRList.Add(format(aPrefix + '[%s + %s * 8]%s', [aSLBaseReg[il_Base], aSLIndexReg[il_Index], aSuffix]));

      //aRList.Add(format(aPrefix + '[%s + %s * 2 + 16]', [aSLBaseReg[il_Base], aSLIndexReg[il_Index]]));
      //aRList.Add(format(aPrefix + '[%s + %s * 4 + 32]', [aSLBaseReg[il_Base], aSLIndexReg[il_Index]]));
      //aRList.Add(format(aPrefix + '[%s + %s * 8 + 48]', [aSLBaseReg[il_Base], aSLIndexReg[il_Index]]));


      aRList.Add(format(aPrefix + '[%s + %s]%s', [aSLIndexReg[il_Index], aSLBaseReg[il_Base], aSuffix]));

      //aRList.Add(format(aPrefix + '[%s + %s + 16]', [aSLIndexReg[il_Index], aSLBaseReg[il_Base]]));
    end;
  end;
end;

function TAsmTestGenerator.ParseBaseIndexReg(const aOp: string; var aBaseReg,
  aIndexReg: string): boolean;
var
  iStartPos: integer;
  iEndPos: integer;
  iPos: integer;
  sOp: string;
  sBaseReg: string;
  sIndexReg: string;
begin
  result := false;

  aBaseReg  := '';
  aIndexReg := '';

  iStartPos := Pos('[', aOp);
  iEndPos   := Pos(']', aOp);

  if (iStartPos > 0) and
     (iEndPos > 0) and
     (iStartPos < iEndPos) then
  begin
    sOp  := trim(copy(aOp, iStartPos + 1, iEndPos - iStartPos - 1));

    with TStringList.Create do
    try
      CommaText := StringReplace(sOp, '+', ',', [rfReplaceAll]);

      while Count < 2 do Add('');

      sBaseReg := trim(Strings[0]);

      if (FReg32Base.IndexOf(sBasereg) >= 0) or
         (FReg64Base.IndexOf(sBasereg) >= 0) or
         (FReg6432Base.IndexOf(sBasereg) >= 0) then
       aBaseReg := sBaseReg;

      sIndexReg := trim(Strings[1]);

      if (FReg32Index.IndexOf(sIndexReg) >= 0) or
         (FReg64Index.IndexOf(sIndexReg) >= 0) or
         (FReg6432Index.IndexOf(sIndexReg) >= 0) then
       aIndexReg := sIndexReg;

      result := trim(aBasereg) <> '';
    finally
      Free;
    end;
  end;
end;

class procedure TAsmTestGenerator.CalcTestData(aX64, aAVX512, aSAE: boolean; const aInst, aOp1, aOp2, aOp3,
  aOp4: String; aSL: TStringList);
var
  sl: TStringList;
begin
  with TAsmTestGenerator.Create do
  try
    Fx64 := aX64;
    FAVX512 := aAVX512;
    FSAE    := aSAE;

    sl := InternalCalcTestData(aInst, aOp1, aOp2, aOp3, aOp4);
    try
      aSL.AddStrings(sl);
    finally
      FreeAndNil(sl);
    end;
  finally
    Free;
  end;
end;

class procedure TAsmTestGenerator.CalcTestDataMREF(aX64, aAVX512, aSAE: boolean; const aInst, aOp1, aOp2, aOp3,
  aOp4: String;  aSL: TStringList);
var
  sl: TStringList;
begin
  with TAsmTestGenerator.Create do
  try
    Fx64 := aX64;
    FAVX512 := aAVX512;
    FSAE    := aSAE;

    sl := InternalCalcTestDataMREF(aInst, aOp1, aOp2, aOp3, aOp4);
    try
      aSL.AddStrings(sl);
    finally
      FreeAndNil(sl);
    end;
  finally
    Free;
  end;
end;

class procedure TAsmTestGenerator.CalcTestDataCDisp8(aX64, aAVX512,
  aSAE: boolean; const aInst, aOp1, aOp2, aOp3, aOp4: String; aSL: TStringList);
var
  sl: TStringList;
begin
  with TAsmTestGenerator.Create do
  try
    Fx64 := aX64;
    FAVX512 := aAVX512;
    FSAE    := aSAE;

    sl := InternalCalcTestDataCDisp8(aInst, aOp1, aOp2, aOp3, aOp4);
    try
      aSL.AddStrings(sl);
    finally
      FreeAndNil(sl);
    end;
  finally
    Free;
  end;
end;



class procedure TAsmTestGenerator.CalcTestInstFile;
var
  i,j: integer;
  sInst: string;
  sI386: string;
  sX8664: string;
  sAVX512: string;
  sOperands: string;

  sLine: string;


  sl: TStringList;
  bVEX: boolean;
  bEVEX: boolean;
  b256 : boolean;
  b512 : boolean;
begin
  sl := TStringList.Create;
  try

    //tinsentry=packed record
    //  opcode  : tasmop;
    //  ops     : byte;
    //  //optypes : array[0..max_operands-1] of longint;
    //  optypes : array[0..3] of int64; //TG
    //  code    : array[0..11] of char;
    //  flags   : tinsflags;
    //end;

    for i := 0 to length(InsTab) - 1 do
    begin
      bVEX := false;
      bEVEX := false;
      b256 := false;
      b512 := false;

      //TG TODO delete
      if instab[i].opcode = a_vtestps then
      begin
        b512 := b512;
      end;

      for j := 0 to length(InsTab[i].code) - 1 do
      begin
        case ord(InsTab[i].code[j]) of
            0: break;
            1,2,3: break;
          232: bEVEX := true;
          233: b512 := true;
          242: bVEX := true;
          244: b256 := true;
        end;
      end;

      if bVEX or bEVEX then
      begin
        sInst  :=  std_op2str[InsTab[i].opcode];
        sI386  := '1';
        sX8664 := '1';
        if IF_X86_64 in InsTab[i].flags then
        begin
          sI386  := '0';
        end;

        if bEVEX then sAVX512 := '1'
         else sAVX512 := '0';

        sOperands := '';
        for j := 0 to 3 do
        begin
          case InsTab[i].optypes[j] of
               OT_XMMREG: sOperands := sOperands + 'XMMREG,';
             OT_XMMREG_M: sOperands := sOperands + 'XMMREG_M,';
            OT_XMMREG_MZ: sOperands := sOperands + 'XMMREG_MZ,';
            OT_XMMREG_ER: sOperands := sOperands + 'XMMREG_ER,';
           OT_XMMREG_SAE: sOperands := sOperands + 'XMMREG_SAE,';
                OT_XMMRM: sOperands := sOperands + 'XMMRM,';
             OT_XMMRM_MZ: sOperands := sOperands + 'XMMRM_MZ,';

               OT_YMMREG: sOperands := sOperands + 'YMMREG,';
             OT_YMMREG_M: sOperands := sOperands + 'YMMREG_M,';
            OT_YMMREG_MZ: sOperands := sOperands + 'YMMREG_MZ,';
            OT_YMMREG_ER: sOperands := sOperands + 'YMMREG_ER,';
           OT_YMMREG_SAE: sOperands := sOperands + 'YMMREG_SAE,';
                OT_YMMRM: sOperands := sOperands + 'YMMRM,';
             OT_YMMRM_MZ: sOperands := sOperands + 'YMMRM_MZ,';

               OT_ZMMREG: sOperands := sOperands + 'ZMMREG,';
             OT_ZMMREG_M: sOperands := sOperands + 'ZMMREG_M,';
            OT_ZMMREG_MZ: sOperands := sOperands + 'ZMMREG_MZ,';
            OT_ZMMREG_ER: sOperands := sOperands + 'ZMMREG_ER,';
           OT_ZMMREG_SAE: sOperands := sOperands + 'ZMMREG_SAE,';
                OT_ZMMRM: sOperands := sOperands + 'ZMMRM,';
             OT_ZMMRM_MZ: sOperands := sOperands + 'ZMMRM_MZ,';

                OT_MEM32: sOperands := sOperands + 'MEM32,';
                OT_MEM64: sOperands := sOperands + 'MEM64,';
               OT_MEM128: sOperands := sOperands + 'MEM128,';
               OT_MEM256: sOperands := sOperands + 'MEM256,';
               OT_MEM512: sOperands := sOperands + 'MEM512,';

                OT_REG32: sOperands := sOperands + 'REG32,';
                OT_REG64: sOperands := sOperands + 'REG64,';
                 ot_rm_gpr or ot_bits32:
                          sOperands := sOperands + 'RM32,';
                 ot_rm_gpr or ot_bits64:
                          sOperands := sOperands + 'RM64,';

               OT_XMEM32: sOperands := sOperands + 'XMEM32,';
               OT_XMEM64: sOperands := sOperands + 'XMEM64,';

               OT_YMEM32: sOperands := sOperands + 'YMEM32,';
               OT_YMEM64: sOperands := sOperands + 'YMEM64,';

                 OT_IMM8: sOperands := sOperands + 'IMM8,';
                 OT_NONE: sOperands := sOperands + ',';

               OT_BMEM32: if b512 then sOperands := sOperands + '16B32,'
                           else if b256 then sOperands := sOperands + '8B32,'
                           else sOperands := sOperands + '4B32,';
               OT_BMEM64: if b512 then sOperands := sOperands + '8B32,'
                           else if b256 then sOperands := sOperands + '4B32,'
                           else sOperands := sOperands + '2B64,';

                 OT_KREG: sOperands := sOperands + 'KREG,';
               OT_KREG_M: sOperands := sOperands + 'KREG_M,';

            else  sOperands := sOperands;
          end;
        end;

        sOperands := copy(sOperands, 1, length(sOperands) - 1);

        sl.Add(format('FOpCodeList.Add(''%s,%s,%s,%s,%s'');', [sInst, sI386, sX8664, sAVX512, sOperands]));
      end;

    end;

    sl.Savetofile('/tmp/fpcavx512.txt');

   // std_op2str

  finally
    FreeAndnil(sl);
  end;
end;

class procedure TAsmTestGenerator.ListMemRefState;
var
  i: integer;
  sGasSufffix: string;
  mrsize: TMemRefSizeInfo;
  opcode: tasmop;
  sl: TStringList;
  slEmpty: TStringList;
begin
  BuildInsTabCache;
  BuildInsTabMemRefSizeInfoCache;

  slEmpty := TStringList.Create;
  try
    for mrsize := low(TMemRefSizeInfo) to high(TMemRefSizeInfo) do
    begin

      sl := TStringList.Create;
      try
        for opcode:=low(tasmop) to high(tasmop) do
        begin
          if InsTabMemRefSizeInfoCache^[opcode].MemRefSize = mrsize then
          begin
            sGasSufffix:='';
            if gas_needsuffix[opcode] <> AttSufNone then
             sGasSufffix:=GetEnumName(Typeinfo(TAttSuffix), ord(gas_needsuffix[opcode]));

            sl.add(format('%-25s:   %s: %s', [GetEnumName(Typeinfo(TMemRefSizeInfo), ord(mrsize)), std_op2str[opcode], sGasSufffix]));
          end;
        end;

        sl.Sort;

        if sl.Count > 0 then
        begin
          writeln;

          writeln(sl.text);
        end
        else slEmpty.Add(GetEnumName(Typeinfo(TMemRefSizeInfo), ord(mrsize)));


      finally
        FreeAndNil(sl);
      end;
    end;

    slEmpty.Sort;
    writeln('');
    writeln(slEmpty.Text);
  finally
    FreeAndNil(slEmpty);
  end;

  if assigned(instabcache) then
  begin
    dispose(instabcache);
    instabcache:=nil;
  end;

  if assigned(InsTabMemRefSizeInfoCache) then
  begin
    dispose(InsTabMemRefSizeInfoCache);
    InsTabMemRefSizeInfoCache:=nil;
  end;


end;

end.
