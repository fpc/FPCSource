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
type
{$WARNING CPU32 opcodes do not fully include the Ultra SPRAC instruction set.}
  { don't change the order of these opcodes! }
  TAsmOp=({$INCLUDE opcode.inc});
  op2strtable=array[TAsmOp]OF STRING[11];
CONST
  FirstOp=Low(TAsmOp);
  LastOp=High(TAsmOp);
  std_op2str:op2strtable=({$INCLUDE strinst.inc});
{*****************************************************************************
                                Operand Sizes
*****************************************************************************}
type
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
type
  TAsmCond=(C_None,
    C_A,C_AE,C_B,C_BE,C_C,C_E,C_G,C_GE,C_L,C_LE,C_NA,C_NAE,
    C_NB,C_NBE,C_NC,C_NE,C_NG,C_NGE,C_NL,C_NLE,C_NO,C_NP,
    C_NS,C_NZ,C_O,C_P,C_PE,C_PO,C_S,C_Z
  );
CONST
  cond2str:array[TAsmCond] of string[3]=('',
    'a','ae','b','be','c','e','g','ge','l','le','na','nae',
    'nb','nbe','nc','ne','ng','nge','nl','nle','no','np',
    'ns','nz','o','p','pe','po','s','z'
  );
  inverse_cond:array[TAsmCond] of TAsmCond=(C_None,
    C_NA,C_NAE,C_NB,C_NBE,C_NC,C_NE,C_NG,C_NGE,C_NL,C_NLE,C_A,C_AE,
    C_B,C_BE,C_C,C_E,C_G,C_GE,C_L,C_LE,C_O,C_P,
    C_S,C_Z,C_NO,C_NP,C_NP,C_P,C_NS,C_NZ
  );
CONST
  CondAsmOps=3;
  CondAsmOp:array[0..CondAsmOps-1] of TAsmOp=(A_FCMPd, A_JMPL, A_FCMPs);
  CondAsmOpStr:array[0..CondAsmOps-1] of string[7]=('FCMPd','JMPL','FCMPs');
{*****************************************************************************}
{                                 Registers                                   }
{*****************************************************************************}
type
  { enumeration for registers, don't change the order }
  { it's used by the register size conversions        }
  TCpuRegister=({$INCLUDE cpuregs.inc});
  TOldRegister=TCpuRegister;
  Tnewregister=word;
  Tsuperregister=byte;
  Tsubregister=byte;
  Tregister=record
    enum:TCpuRegister;
    number:Tnewregister;
  end;
  TRegister64=PACKED RECORD
  {A type to store register locations for 64 Bit values.}
     RegLo,RegHi:TRegister;
  END;
  treg64=tregister64;{alias for compact code}
  TRegisterSet=SET OF TCpuRegister;
  Tsupregset=set of Tsuperregister;
const
  R_NO=R_NONE;
  firstreg = Succ(R_NONE);
  lastreg  = Pred(R_INTREGISTER);
{General registers.}

const
  NR_NONE=$0000;
  NR_NO=NR_NONE;
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
{Floating point}
  NR_F0=$2000;
  NR_F1=$2000;
  NR_F2=$2000;
  NR_F3=$2000;
  NR_F4=$2000;
  NR_F5=$2000;
  NR_F6=$2000;
  NR_F7=$2000;
  NR_F8=$2000;
  NR_F9=$2000;
  NR_F10=$2000;
  NR_F11=$2000;
  NR_F12=$2000;
  NR_F13=$2000;
  NR_F14=$2000;
  NR_F15=$2000;
  NR_F16=$2000;
  NR_F17=$2000;
  NR_F18=$2000;
  NR_F19=$2000;
  NR_F20=$2000;
  NR_F21=$2000;
  NR_F22=$2000;
  NR_F23=$2000;
  NR_F24=$2000;
  NR_F25=$2000;
  NR_F26=$2000;
  NR_F27=$2000;
  NR_F28=$2000;
  NR_F29=$2000;
  NR_F30=$2000;
  NR_F31=$2000;
{Coprocessor point}
  NR_C0=$3000;
  NR_C1=$3000;
  NR_C2=$3000;
  NR_C3=$3000;
  NR_C4=$3000;
  NR_C5=$3000;
  NR_C6=$3000;
  NR_C7=$3000;
  NR_C8=$3000;
  NR_C9=$3000;
  NR_C10=$3000;
  NR_C11=$3000;
  NR_C12=$3000;
  NR_C13=$3000;
  NR_C14=$3000;
  NR_C15=$3000;
  NR_C16=$3000;
  NR_C17=$3000;
  NR_C18=$3000;
  NR_C19=$3000;
  NR_C20=$3000;
  NR_C21=$3000;
  NR_C22=$3000;
  NR_C23=$3000;
  NR_C24=$3000;
  NR_C25=$3000;
  NR_C26=$3000;
  NR_C27=$3000;
  NR_C28=$3000;
  NR_C29=$3000;
  NR_C30=$3000;
  NR_C31=$3000;
{ASR}
  NR_ASR0=$4000;
  NR_ASR1=$4000;
  NR_ASR2=$4000;
  NR_ASR3=$4000;
  NR_ASR4=$4000;
  NR_ASR5=$4000;
  NR_ASR6=$4000;
  NR_ASR7=$4000;
  NR_ASR8=$4000;
  NR_ASR9=$4000;
  NR_ASR10=$4000;
  NR_ASR11=$4000;
  NR_ASR12=$4000;
  NR_ASR13=$4000;
  NR_ASR14=$4000;
  NR_ASR15=$4000;
  NR_ASR16=$4000;
  NR_ASR17=$4000;
  NR_ASR18=$4000;
  NR_ASR19=$4000;
  NR_ASR20=$4000;
  NR_ASR21=$4000;
  NR_ASR22=$4000;
  NR_ASR23=$4000;
  NR_ASR24=$4000;
  NR_ASR25=$4000;
  NR_ASR26=$4000;
  NR_ASR27=$4000;
  NR_ASR28=$4000;
  NR_ASR29=$4000;
  NR_ASR30=$4000;
  NR_ASR31=$4000;
{Floating point status/"front of queue" registers}
  NR_FSR=$5000;
  NR_FQ=$50001;
  NR_CSR=$5000;
  NR_CQ=$5000;
  NR_PSR=$5000;
  NR_TBR=$5000;
  NR_WIM=$5000;
  NR_Y=$5000;

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

  first_imreg = $19;
  last_imreg = $ff;

  {Subregisters; nothing known about.}
  R_SUBWHOLE=$00;
  R_SUBL=$00;

type
  reg2strtable=array[TCpuRegister] OF STRING[7];
  TCpuReg=array[TCpuRegister]of TRegister;
const
  std_reg2str:reg2strtable=({$INCLUDE strregs.inc});
  CpuReg:TCpuReg=({$INCLUDE registers.inc});
{*****************************************************************************
                                   Flags
*****************************************************************************}
type
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
type
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
type
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
type
{tparamlocation describes where a parameter for a procedure is stored.
References are given from the caller's point of view. The usual TLocation isn't
used, because contains a lot of unnessary fields.}
  TParaLocation=PACKED RECORD
    Size:TCGSize;
    Loc:TCGLoc;
    sp_fixup:LongInt;
    CASE TCGLoc OF
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
         loc  : TCGLoc;
         size : TCGSize;
         case TCGLoc of
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
  firstsavemmreg = R_NONE;
  lastsavemmreg = R_NONE;
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
  stab_regindex:array[TCpuRegister]OF ShortInt=({$INCLUDE stabregi.inc});
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
  scratch_regs:array[1..ScratchRegsCount] OF Tsuperregister=(RS_L0,RS_L1,RS_L2,RS_L3,RS_L4,RS_L5,RS_L6,RS_L7);
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
type
  tinsentry=packed record
    opcode  : tasmop;
    ops     : byte;
    optypes : array[0..2] of LongInt;
    code    : array[0..maxinfolen] of char;
    flags   : LongInt;
  END;
  pinsentry=^tinsentry;

  TInsTabCache=array[TasmOp] of LongInt;
  PInsTabCache=^TInsTabCache;
VAR
  InsTabCache : PInsTabCache;
{$ENDif NOAG386BIN}
{*****************************************************************************
                                  Helpers
*****************************************************************************}
const
  maxvarregs=30;
  VarRegs:array[1..maxvarregs]OF TCpuRegister=(
             R_G0,R_G1,R_G2,R_G3,R_G4,R_G5,R_G6,R_G7,
             R_O0,R_O1,R_O2,R_O3,R_O4,R_O5,{R_R14=R_SP}R_O7,
             R_L0,R_L1,R_L2,R_L3,R_L4,R_L5,R_L6,R_L7,
             R_I0,R_I1,R_I2,R_I3,R_I4,R_I5,{R_R30=R_FP}R_I7
        );
  maxfpuvarregs = 8;
  max_operands = 3;
  maxintregs = maxvarregs;
  maxfpuregs = maxfpuvarregs;
  max_scratch_regs=8;


function is_calljmp(o:tasmop):boolean;
function flags_to_cond(CONST f:TResFlags):TAsmCond;
procedure convert_register_to_enum(var r:Tregister);
function cgsize2subreg(s:Tcgsize):Tsubregister;
implementation
uses
  verbose;
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
    flags_2_cond:array[TResFlags]OF TAsmCond=
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
end.
{
  $Log$
  Revision 1.30  2003-05-06 14:58:46  mazen
  - non used constants OT_* removed
  * some keywords moved lower case

  Revision 1.29  2003/04/29 12:03:52  mazen
  * TOldRegister isnow just an alias for TCpuRegister
  * TCpuRegister is used to define cpu register set physically available
  + CpuRegs array to easially create correspondence between TCpuRegister and TRegister

  Revision 1.28  2003/04/28 09:46:30  mazen
  + max_scratch_regs variable added because requested by common compiler code

  Revision 1.27  2003/04/23 13:35:39  peter
    * fix sparc compile

  Revision 1.26  2003/04/23 12:35:35  florian
    * fixed several issues with powerpc
    + applied a patch from Jonas for nested function calls (PowerPC only)
    * ...
}
