{******************************************************************************
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl and Peter Vreman

    Contains the base types for the Scalable Processor ARChitecture (SPARC)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 ****************************************************************************}
unit cpuBase;
{$INCLUDE fpcdefs.inc}
interface
uses globals,cutils,cclasses,aasmbase,cpuinfo,cginfo;
const
  {Size of the instruction table converted by nasmconv.pas}
  maxinfolen=8;
  {Defines the default address size for a processor}
  OS_ADDR=OS_32;
  {the natural int size for a processor}
  OS_INT=OS_32;
  {the maximum float size for a processor}
  OS_FLOAT=OS_F64;
  {the size of a vector register for a processor}
  OS_VECTOR=OS_M64;
  {Operand types}
  OT_NONE      = $00000000;
  OT_BITS8     = $00000001;  { size, and other attributes, of the operand  }
  OT_BITS16    = $00000002;
  OT_BITS32    = $00000004;
  OT_BITS64    = $00000008;  { FPU only  }
  OT_BITS80    = $00000010;
  OT_FAR       = $00000020;  { this means 16:16 or 16:32, like in CALL/JMP }
  OT_NEAR      = $00000040;
  OT_SHORT     = $00000080;
  OT_SIZE_MASK = $000000FF;  { all the size attributes  }
  OT_NON_SIZE  = LongInt(not OT_SIZE_MASK);
  OT_SIGNED    = $00000100;  { the operand need to be signed -128-127 }
  OT_TO        = $00000200;  { operand is followed by a colon  }
                             { reverse effect in FADD, FSUB &c  }
  OT_COLON     = $00000400;
  OT_REGISTER  = $00001000;
  OT_IMMEDIATE = $00002000;
  OT_IMM8      = $00002001;
  OT_IMM16     = $00002002;
  OT_IMM32     = $00002004;
  OT_IMM64     = $00002008;
  OT_IMM80     = $00002010;
  OT_REGMEM    = $00200000;  { for r/m, ie EA, operands  }
  OT_REGNORM   = $00201000;  { 'normal' reg, qualifies as EA  }
  OT_REG8      = $00201001;
  OT_REG16     = $00201002;
  OT_REG32     = $00201004;
  OT_MMXREG    = $00201008;  { MMX registers  }
  OT_XMMREG    = $00201010;  { Katmai registers  }
  OT_MEMORY    = $00204000;  { register number in 'basereg'  }
  OT_MEM8      = $00204001;
  OT_MEM16     = $00204002;
  OT_MEM32     = $00204004;
  OT_MEM64     = $00204008;
  OT_MEM80     = $00204010;
  OT_FPUREG    = $01000000;  { floating point stack registers  }
  OT_FPU0      = $01000800;  { FPU stack register zero  }
  OT_REG_SMASK = $00070000;  { special register operands: these may be treated differently  }
                             { a mask for the following  }
  OT_REG_ACCUM = $00211000;  { accumulator: AL, AX or EAX  }
  OT_REG_AL    = $00211001;    { REG_ACCUM | BITSxx  }
  OT_REG_AX    = $00211002;    { ditto  }
  OT_REG_EAX   = $00211004;    { and again  }
  OT_REG_COUNT = $00221000;  { counter: CL, CX or ECX  }
  OT_REG_CL    = $00221001;    { REG_COUNT | BITSxx  }
  OT_REG_CX    = $00221002;    { ditto  }
  OT_REG_ECX   = $00221004;    { another one  }
  OT_REG_DX    = $00241002;

  OT_REG_SREG  = $00081002;  { any segment register  }
  OT_REG_CS    = $01081002;  { CS  }
  OT_REG_DESS  = $02081002;  { DS, ES, SS (non-CS 86 registers)  }
  OT_REG_FSGS  = $04081002;  { FS, GS (386 extENDed registers)  }

  OT_REG_CDT   = $00101004;  { CRn, DRn and TRn  }
  OT_REG_CREG  = $08101004;  { CRn  }
  OT_REG_CR4   = $08101404;  { CR4 (Pentium only)  }
  OT_REG_DREG  = $10101004;  { DRn  }
  OT_REG_TREG  = $20101004;  { TRn  }

  OT_MEM_OFFS  = $00604000;  { special type of EA  }
                             { simple [address] offset  }
  OT_ONENESS   = $00800000;  { special type of immediate operand  }
                             { so UNITY == IMMEDIATE | ONENESS  }
  OT_UNITY     = $00802000;  { for shift/rotate instructions  }

{Instruction flags }
  IF_NONE   = $00000000;
  IF_SM     = $00000001;        { size match first two operands  }
  IF_SM2    = $00000002;
  IF_SB     = $00000004;  { unsized operands can't be non-byte  }
  IF_SW     = $00000008;  { unsized operands can't be non-word  }
  IF_SD     = $00000010;  { unsized operands can't be nondword  }
  IF_AR0    = $00000020;  { SB, SW, SD applies to argument 0  }
  IF_AR1    = $00000040;  { SB, SW, SD applies to argument 1  }
  IF_AR2    = $00000060;  { SB, SW, SD applies to argument 2  }
  IF_ARMASK = $00000060;  { mask for unsized argument spec  }
  IF_PRIV   = $00000100;  { it's a privileged instruction  }
  IF_SMM    = $00000200;  { it's only valid in SMM  }
  IF_PROT   = $00000400;  { it's protected mode only  }
  IF_UNDOC  = $00001000;  { it's an undocumented instruction  }
  IF_FPU    = $00002000;  { it's an FPU instruction  }
  IF_MMX    = $00004000;  { it's an MMX instruction  }
  IF_3DNOW  = $00008000;  { it's a 3DNow! instruction  }
  IF_SSE    = $00010000;  { it's a SSE (KNI, MMX2) instruction  }
  IF_PMASK  = LongInt($FF000000);  { the mask for processor types  }
  IF_PFMASK = LongInt($F001FF00);  { the mask for disassembly "prefer"  }
  IF_V7     = $00000000;  { SPARC V7 instruction only (not supported)}
  IF_V8     = $01000000;  { SPARC V8 instruction (the default)}
  IF_V9     = $02000000;  { SPARC V9 instruction (not yet supported)}
  { added flags }
  IF_PRE    = $40000000;  { it's a prefix instruction }
  IF_PASS2  = LongInt($80000000);{instruction can change in a second pass?}
TYPE
{$WARNING CPU32 opcodes do not fully include the Ultra SPRAC instruction set.}
  { don't change the order of these opcodes! }
  TAsmOp=({$INCLUDE opcode.inc});
  op2strtable=ARRAY[TAsmOp]OF STRING[11];
CONST
  FirstOp=Low(TAsmOp);
  LastOp=High(TAsmOp);
  std_op2str:op2strtable=({$INCLUDE strinst.inc});
{*****************************************************************************
                                Operand Sizes
*****************************************************************************}
TYPE
  TOpSize=(S_NO,
           S_B,{Byte}
           S_H,{Half word}
           S_W,{Word}
           S_L:=S_W,
           S_D,{Double Word}
           S_Q,{Quad word}
           S_IQ:=S_Q,
           S_SB,{Signed byte}
           S_SH,{Signed half word}
           S_SW,{Signed word}
           S_SD,{Signed double word}
           S_SQ,{Signed quad word}
           S_FS,{Float single word}
           S_FX:=S_FS,
           S_FD,{Float double word}
           S_FQ,{Float quad word}
           S_NEAR,
           S_FAR,
           S_SHORT);
{*****************************************************************************}
{                               Conditions                                    }
{*****************************************************************************}
TYPE
  TAsmCond=(C_None,
    C_A,C_AE,C_B,C_BE,C_C,C_E,C_G,C_GE,C_L,C_LE,C_NA,C_NAE,
    C_NB,C_NBE,C_NC,C_NE,C_NG,C_NGE,C_NL,C_NLE,C_NO,C_NP,
    C_NS,C_NZ,C_O,C_P,C_PE,C_PO,C_S,C_Z
  );
CONST
  cond2str:ARRAY[TAsmCond] of string[3]=('',
    'a','ae','b','be','c','e','g','ge','l','le','na','nae',
    'nb','nbe','nc','ne','ng','nge','nl','nle','no','np',
    'ns','nz','o','p','pe','po','s','z'
  );
  inverse_cond:ARRAY[TAsmCond] of TAsmCond=(C_None,
    C_NA,C_NAE,C_NB,C_NBE,C_NC,C_NE,C_NG,C_NGE,C_NL,C_NLE,C_A,C_AE,
    C_B,C_BE,C_C,C_E,C_G,C_GE,C_L,C_LE,C_O,C_P,
    C_S,C_Z,C_NO,C_NP,C_NP,C_P,C_NS,C_NZ
  );
CONST
  CondAsmOps=3;
  CondAsmOp:ARRAY[0..CondAsmOps-1] of TAsmOp=(A_FCMPd, A_JMPL, A_FCMPs);
  CondAsmOpStr:ARRAY[0..CondAsmOps-1] of string[7]=('FCMPd','JMPL','FCMPs');
{*****************************************************************************}
{                                 Registers                                   }
{*****************************************************************************}
TYPE
  { enumeration for registers, don't change the order }
  { it's used by the register size conversions        }
  ToldRegister=({$INCLUDE registers.inc});
  Tnewregister=word;
  Tsuperregister=byte;
  Tsubregister=byte;
  Tregister=record
    enum:Toldregister;
    number:Tnewregister;
  end;
  TRegister64=PACKED RECORD
  {A type to store register locations for 64 Bit values.}
     RegLo,RegHi:TRegister;
  END;
  treg64=tregister64;{alias for compact code}
  TRegisterSet=SET OF ToldRegister;
  Tsupregset=set of Tsuperregister;
CONST
  R_NO=R_NONE;
  firstreg = low(Toldregister);
  lastreg  = high(R_ASR31);

{General registers.}

const
  NR_NO=$0000;
  NR_G0=$0001;
  NR_G1=$0002;
  NR_G2=$0003;
  NR_G3=$0004;
  NR_G4=$0005;
  NR_G5=$0006;
  NR_G6=$0007;
  NR_G7=$0008;
  NR_O0=$0100;
  NR_O1=$0200;
  NR_O2=$0300;
  NR_O3=$0400;
  NR_O4=$0500;
  NR_O5=$0600;
  NR_O6=$0700;
  NR_O7=$0800;
  NR_L0=$0900;
  NR_L1=$0A00;
  NR_L2=$0B00;
  NR_L3=$0C00;
  NR_L4=$0D00;
  NR_L5=$0E00;
  NR_L6=$0F00;
  NR_L7=$1000;
  NR_I0=$1100;
  NR_I1=$1200;
  NR_I2=$1300;
  NR_I3=$1400;
  NR_I4=$1500;
  NR_I5=$1600;
  NR_I6=$1700;
  NR_I7=$1800;

{Superregisters.}

const
  RS_O0=$01;
  RS_O1=$02;
  RS_O2=$03;
  RS_O3=$04;
  RS_O4=$05;
  RS_O5=$06;
  RS_O6=$07;
  RS_O7=$08;
  RS_L0=$09;
  RS_L1=$0A;
  RS_L2=$0B;
  RS_L3=$0C;
  RS_L4=$0D;
  RS_L5=$0E;
  RS_L6=$0F;
  RS_L7=$10;
  RS_I0=$11;
  RS_I1=$12;
  RS_I2=$13;
  RS_I3=$14;
  RS_I4=$15;
  RS_I5=$16;
  RS_I6=$17;
  RS_I7=$18;

  first_supreg = $01;
  last_supreg = $18;

  {Subregisters; nothing known about.}
  R_SUBWHOLE=$00;
  R_SUBL=$00;
  
type
  reg2strtable=ARRAY[firstreg..lastreg] OF STRING[7];

const
  std_reg2str:reg2strtable=({$INCLUDE strregs.inc});
{*****************************************************************************
                                   Flags
*****************************************************************************}
TYPE
  TResFlags=(
    F_E,  {Equal}
    F_NE, {Not Equal}
    F_G,  {Greater}
    F_L,  {Less}
    F_GE, {Greater or Equal}
    F_LE, {Less or Equal}
    F_C,  {Carry}
    F_NC, {Not Carry}
    F_A,  {Above}
    F_AE, {Above or Equal}
    F_B,  {Below}
    F_BE  {Below or Equal}
  );
{*****************************************************************************
                                Reference
*****************************************************************************}
TYPE
  trefoptions=(ref_none,ref_parafixup,ref_localfixup,ref_selffixup);

  { immediate/reference record }
  poperreference = ^treference;
  Preference=^Treference;
  treference = packed record
     segment,
     base,
     index       : tregister;
     scalefactor : byte;
     offset      : LongInt;
     symbol      : tasmsymbol;
     offsetfixup : LongInt;
     options     : trefoptions;
     alignment   : byte;
  END;
      { reference record }
  PParaReference=^TParaReference;
  TParaReference=PACKED RECORD
    Index:TRegister;
    Offset:longint;
  END;
{*****************************************************************************
                                Operands
*****************************************************************************}
  { Types of operand }
  toptype=(top_none,top_reg,top_ref,top_const,top_symbol,top_raddr,top_caddr);
  toper=record
    ot:LongInt;
    case typ:toptype of
      top_none:();
      top_reg:(reg:tregister);
      top_ref:(ref:poperreference);
      top_const:(val:aword);
      top_symbol:(sym:tasmsymbol;symofs:LongInt);
      top_raddr:(reg1,reg2:TRegister);
      top_caddr:(regb:TRegister;const13:Integer);
  end;
{*****************************************************************************
                             Argument Classification
*****************************************************************************}
TYPE
  TArgClass = (
     { the following classes should be defined by all processor implemnations }
     AC_NOCLASS,
     AC_MEMORY,
     AC_INTEGER,
     AC_FPU,
     { the following argument classes are i386 specific }
     AC_FPUUP,
     AC_SSE,
     AC_SSEUP);

{*****************************************************************************
                               Generic Location
*****************************************************************************}
TYPE
  TLoc=(              {information about the location of an operand}
    LOC_INVALID,      { added for tracking problems}
    LOC_CONSTANT,     { CONSTant value }
    LOC_JUMP,         { boolean results only, jump to false or true label }
    LOC_FLAGS,        { boolean results only, flags are set }
    LOC_CREFERENCE,   { in memory CONSTant value }
    LOC_REFERENCE,    { in memory value }
    LOC_REGISTER,     { in a processor register }
    LOC_CREGISTER,    { Constant register which shouldn't be modified }
    LOC_FPUREGISTER,  { FPU stack }
    LOC_CFPUREGISTER, { if it is a FPU register variable on the fpu stack }
    LOC_MMXREGISTER,  { MMX register }
    LOC_CMMXREGISTER, { MMX register variable }
    LOC_MMREGISTER,
    LOC_CMMREGISTER
  );
{tparamlocation describes where a parameter for a procedure is stored.
References are given from the caller's point of view. The usual TLocation isn't
used, because contains a lot of unnessary fields.}
  TParaLocation=PACKED RECORD
    Size:TCGSize;
    Loc:TLoc;
    sp_fixup:LongInt;
    CASE TLoc OF
      LOC_REFERENCE:(reference:tparareference);
            { segment in reference at the same place as in loc_register }
      LOC_REGISTER,LOC_CREGISTER : (
        CASE LongInt OF
          1 : (register,registerhigh : tregister);
              { overlay a registerlow }
          2 : (registerlow : tregister);
              { overlay a 64 Bit register type }
          3 : (reg64 : tregister64);
          4 : (register64 : tregister64);
        );
            { it's only for better handling }
      LOC_MMXREGISTER,LOC_CMMXREGISTER : (mmxreg : tregister);
    END;
    TLocation=PACKED RECORD
         loc  : TLoc;
         size : TCGSize;
         case TLoc of
            LOC_FLAGS : (resflags : tresflags);
            LOC_CONSTANT : (
              case longint of
                1 : (value : AWord);
                2 : (valuelow, valuehigh:AWord);
                { overlay a complete 64 Bit value }
                3 : (valueqword : qword);
              );
            LOC_CREFERENCE,
            LOC_REFERENCE : (reference : treference);
            { segment in reference at the same place as in loc_register }
            LOC_REGISTER,LOC_CREGISTER : (
              case longint of
                1 : (register,registerhigh,segment : tregister);
                { overlay a registerlow }
                2 : (registerlow : tregister);
                { overlay a 64 Bit register type }
                3 : (reg64 : tregister64);
                4 : (register64 : tregister64);
              );
            { it's only for better handling }
            LOC_MMXREGISTER,LOC_CMMXREGISTER : (mmxreg : tregister);
      end;
{*****************************************************************************
                                 Constants
*****************************************************************************}

const
  general_registers = [R_G0..R_I7];
  general_superregisters = [RS_O0..RS_I7];
  { legend:                                                                }
  { xxxregs = set of all possibly used registers of that type in the code  }
  {           generator                                                    }
  { usableregsxxx = set of all 32bit components of registers that can be   }
  {           possible allocated to a regvar or using getregisterxxx (this }
  {           excludes registers which can be only used for parameter      }
  {           passing on ABI's that define this)                           }
  { c_countusableregsxxx = amount of registers in the usableregsxxx set    }
  IntRegs=[R_G0..R_I7];
  usableregsint=[RS_O0..RS_I7];
  c_countusableregsint = 24;
  fpuregs=[R_F0..R_F31];
  usableregsfpu=[R_F0..R_F31];
  c_countusableregsfpu=32;
  mmregs=[];
  usableregsmm=[];
  c_countusableregsmm=0;
  { no distinction on this platform }      
  maxaddrregs = 0;
  addrregs    = [];
  usableregsaddr = [];
  c_countusableregsaddr = 0;
  
  
  firstsaveintreg = RS_O0;
  lastsaveintreg = RS_I7;
  firstsavefpureg = R_F0;
  lastsavefpureg = R_F31;
  firstsavemmreg = R_I0;
  lastsavemmreg = R_I7;
  lowsavereg = R_G0;
  highsavereg = R_I7;

  ALL_REGISTERS = [lowsavereg..highsavereg];
  ALL_INTREGISTERS = [1..255];

  lvaluelocations = [LOC_REFERENCE,LOC_CFPUREGISTER,
    LOC_CREGISTER,LOC_MMXREGISTER,LOC_CMMXREGISTER];
{*****************************************************************************
                               GDB Information
*****************************************************************************}
  {# Register indexes for stabs information, when some parameters or variables
  are stored in registers.
  Taken from rs6000.h (DBX_REGISTER_NUMBER) from GCC 3.x source code.}
  stab_regindex:ARRAY[firstreg..lastreg]OF ShortInt=({$INCLUDE stabregi.inc});
{*************************** generic register names **************************}
  stack_pointer_reg   = R_O6;
  NR_STACK_POINTER_REG = NR_O6;
  RS_STACK_POINTER_REG = RS_O6;
  frame_pointer_reg   = R_I6;
  NR_FRAME_POINTER_REG = NR_I6;
  RS_FRAME_POINTER_REG = RS_I6;
  {the return_result_reg, is used inside the called function to store its return
  value when that is a scalar value otherwise a pointer to the address of the
  result is placed inside it}
  return_result_reg   = R_I0;
  NR_RETURN_RESULT_REG = NR_I0;
  RS_RETURN_RESULT_REG = RS_I0;
  {the function_result_reg contains the function result after a call to a scalar
  function othewise it contains a pointer to the returned result}
  function_result_reg = R_O0;
  NR_FUNCTION_RESULT_REG = NR_O0;
  RS_FUNCTION_RESULT_REG = RS_O0;
  self_pointer_reg  =R_G5;
  NR_SELF_POINTER_REG = NR_G5;
{  RS_SELF_POINTER_REG = RS_G5;}
  {There is no accumulator in the SPARC architecture. There are just families
  of registers. All registers belonging to the same family are identical except
  in the "global registers" family where GO is different from the others :
  G0 gives always 0 when it is red and thows away any value written to it.
  Nevertheless, scalar routine results are returned onto R_O0.}
  accumulator     = R_O0;
  NR_ACCUMULATOR = NR_O0;
  RS_ACCUMULATOR = RS_O1;
  accumulatorhigh = R_O1;
  NR_ACCUMULATORHIGH = NR_O1;
  RS_ACCUMULATORHIGH = RS_O1;
  fpu_result_reg  =R_F0;
  mmresultreg     =R_G0;
{*****************************************************************************}
{                        GCC /ABI linking information                         }
{*****************************************************************************}
{# Registers which must be saved when calling a routine declared as cppdecl,
cdecl, stdcall, safecall, palmossyscall. The registers saved should be the ones
as defined in the target ABI and / or GCC.

This value can be deduced from the CALLED_USED_REGISTERS array in the GCC
source.}
  std_saved_registers=[RS_O6];
{# Required parameter alignment when calling a routine declared as stdcall and
cdecl. The alignment value should be the one defined by GCC or the target ABI.

The value of this constant is equal to the constant
PARM_BOUNDARY / BITS_PER_UNIT in the GCC source.}
  std_param_align=4;
{# Registers which are defined as scratch and no need to save across routine
calls or in assembler blocks.}
  ScratchRegsCount=8;
  scratch_regs:ARRAY[1..ScratchRegsCount] OF Tsuperregister=(RS_L0,RS_L1,RS_L2,RS_L3,RS_L4,RS_L5,RS_L6,RS_L7);
{ low and high of the available maximum width integer general purpose }
{ registers                                                           }
  LoGPReg = R_G0;
  HiGPReg = R_I7;

{ low and high of every possible width general purpose register (same as }
{ above on most architctures apart from the 80x86)                       }
  LoReg = R_G0;
  HiReg = R_I7;

  cpuflags = [];

  { sizes }
  pointersize   = 4;
  extENDed_size = 8;{SPARC architecture uses IEEE floating point numbers}
  mmreg_size = 8;
  SizePostfix_pointer = S_SW;


{*****************************************************************************
                              Instruction table
*****************************************************************************}

{$ifndef NOAG386BIN}
TYPE
  tinsentry=packed record
    opcode  : tasmop;
    ops     : byte;
    optypes : ARRAY[0..2] of LongInt;
    code    : ARRAY[0..maxinfolen] of char;
    flags   : LongInt;
  END;
  pinsentry=^tinsentry;

  TInsTabCache=ARRAY[TasmOp] of LongInt;
  PInsTabCache=^TInsTabCache;
VAR
  InsTabCache : PInsTabCache;
{$ENDif NOAG386BIN}
{*****************************************************************************
                                  Helpers
*****************************************************************************}
const
  maxvarregs=30;
  VarRegs:ARRAY[1..maxvarregs]OF ToldRegister=(
             R_G0,R_G1,R_G2,R_G3,R_G4,R_G5,R_G6,R_G7,
             R_O0,R_O1,R_O2,R_O3,R_O4,R_O5,{R_R14=R_SP}R_O7,
             R_L0,R_L1,R_L2,R_L3,R_L4,R_L5,R_L6,R_L7,
             R_I0,R_I1,R_I2,R_I3,R_I4,R_I5,{R_R30=R_FP}R_I7
        );
  maxfpuvarregs = 8;
  max_operands = 3;
  maxintregs = maxvarregs;
  maxfpuregs = maxfpuvarregs;
  
  

FUNCTION is_calljmp(o:tasmop):boolean;
FUNCTION flags_to_cond(CONST f:TResFlags):TAsmCond;
procedure convert_register_to_enum(var r:Tregister);
function cgsize2subreg(s:Tcgsize):Tsubregister;

IMPLEMENTATION

uses  verbose;

const
  CallJmpOp=[A_JMPL..A_CBccc];
function is_calljmp(o:tasmop):boolean;
  begin
    if o in CallJmpOp
    then
      is_calljmp:=true
    else
      is_calljmp:=false;
  end;
function flags_to_cond(const f:TResFlags):TAsmCond;
  CONST
    flags_2_cond:ARRAY[TResFlags]OF TAsmCond=
    (C_E,C_NE,C_G,C_L,C_GE,C_LE,C_C,C_NC,C_A,C_AE,C_B,C_BE);
  BEGIN
    result:=flags_2_cond[f];
  END;

procedure convert_register_to_enum(var r:Tregister);
begin
  if r.enum=R_INTREGISTER
  then
    case r.number of
      NR_NO: r.enum:= R_NO;
      NR_G0: r.enum:= R_G0;
      NR_G1: r.enum:= R_G1;
      NR_G2: r.enum:= R_G2;
      NR_G3: r.enum:= R_G3;
      NR_G4: r.enum:= R_G4;
      NR_G5: r.enum:= R_G5;
      NR_G6: r.enum:= R_G6;
      NR_G7: r.enum:= R_G7;
      NR_O0: r.enum:= R_O0;
      NR_O1: r.enum:= R_O1;
      NR_O2: r.enum:= R_O2;
      NR_O3: r.enum:= R_O3;
      NR_O4: r.enum:= R_O4;
      NR_O5: r.enum:= R_O5;
      NR_O6: r.enum:= R_O6;
      NR_O7: r.enum:= R_O7;
      NR_L0: r.enum:= R_L0;
      NR_L1: r.enum:= R_L1;
      NR_L2: r.enum:= R_L2;
      NR_L3: r.enum:= R_L3;
      NR_L4: r.enum:= R_L4;
      NR_L5: r.enum:= R_L5;
      NR_L6: r.enum:= R_L6;
      NR_L7: r.enum:= R_L7;
      NR_I0: r.enum:= R_I0;
      NR_I1: r.enum:= R_I1;
      NR_I2: r.enum:= R_I2;
      NR_I3: r.enum:= R_I3;
      NR_I4: r.enum:= R_I4;
      NR_I5: r.enum:= R_I5;
      NR_I6: r.enum:= R_I6;
      NR_I7: r.enum:= R_I7;
      else
        internalerror(200301082);
    end;
end;

function cgsize2subreg(s:Tcgsize):Tsubregister;

begin
  cgsize2subreg:=R_SUBWHOLE;
end;

END.



{
  $Log$
  Revision 1.23  2003-02-19 22:00:17  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.22  2003/02/02 19:25:54  carl
    * Several bugfixes for m68k target (register alloc., opcode emission)
    + VIS target
    + Generic add more complete (still not verified)

  Revision 1.21  2003/01/20 22:21:36  mazen
  * many stuff related to RTL fixed

  Revision 1.20  2003/01/09 20:41:00  daniel
    * Converted some code in cgx86.pas to new register numbering

  Revision 1.19  2003/01/09 15:49:56  daniel
    * Added register conversion

  Revision 1.18  2003/01/08 18:43:58  daniel
   * Tregister changed into a record

  Revision 1.17  2003/01/05 20:39:53  mazen
  * warnings about FreeTemp already free fixed with appropriate registers handling

  Revision 1.16  2002/10/28 20:59:17  mazen
  * TOpSize values changed S_L --> S_SW

  Revision 1.15  2002/10/28 20:37:44  mazen
  * TOpSize values changed S_L --> S_SW

  Revision 1.14  2002/10/20 19:01:38  mazen
  + op_raddr_reg and op_caddr_reg added to fix functions prologue

  Revision 1.13  2002/10/19 20:35:07  mazen
  * carl's patch applied

  Revision 1.12  2002/10/11 13:35:14  mazen
  *** empty log message ***

  Revision 1.11  2002/10/10 19:57:51  mazen
  * Just to update repsitory

  Revision 1.10  2002/10/02 22:20:28  mazen
  + out registers allocator for the first 6 scalar parameters which must be passed into %o0..%o5

  Revision 1.9  2002/10/01 21:06:29  mazen
  attinst.inc --> strinst.inc

  Revision 1.8  2002/09/30 19:12:14  mazen
  * function prologue fixed

  Revision 1.7  2002/09/27 04:30:53  mazen
  * cleanup made

  Revision 1.6  2002/09/24 03:57:53  mazen
  * some cleanup  was made

}
