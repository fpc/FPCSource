{*****************************************************************************}
{ File                   : cpubase.pas                                        }
{ Author                 : Mazen NEIFER                                       }
{ Project                : Free Pascal Compiler (FPC)                         }
{ Creation date          : 2002\04\26                                         }
{ Licence                : GPL                                                }
{ Bug report             : mazen.neifer.01@supaero.org                        }
{*****************************************************************************}
{    $Id$
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
UNIT cpuBase;
{$INCLUDE fpcdefs.inc}
INTERFACE
USES globals,cutils,cclasses,aasmbase,cpuinfo,cginfo;
CONST
{Size of the instruction table converted by nasmconv.pas}
  maxinfolen=8;
{Defines the default address size for a processor}
  OS_ADDR=OS_32;
{the natural int size for a processor}
  OS_INT=OS_32;
{the maximum float size for a processor}
  OS_FLOAT=OS_F80;{$WARNING "OS_FLOAT" was set to "OS_F80" but not verified!}
{the size of a vector register for a processor}
  OS_VECTOR=OS_M64;{$WARNING "OS_VECTOR" was set to "OS_M64" but not verified!}
CONST
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
  IF_V7			= $00000000;  { SPARC V7 instruction only (not supported)}
  IF_V8			= $01000000;  { SPARC V8 instruction (the default)}
  IF_V9			= $02000000;  { SPARC V9 instruction (not yet	supported)}
  { added flags }
  IF_PRE    = $40000000;  { it's a prefix instruction }
  IF_PASS2 	=	LongInt($80000000);{instruction can change in a second pass?}
TYPE
{$WARNING CPU32 opcodes do not fully include the Ultra SPRAC instruction set.}
  TAsmOp=({$INCLUDE opcode.inc});
  op2strtable=ARRAY[TAsmOp]OF STRING[11];
CONST
  FirstOp=Low(TAsmOp);
  LastOp=High(TAsmOp);
  std_op2str:op2strtable=({$INCLUDE attinstr.inc});
{*****************************************************************************
                                Operand Sizes
*****************************************************************************}
TYPE
       { S_NO = No Size of operand }
       { S_B  = Byte size operand  }
       { S_W  = Word size operand  }
       { S_L  = DWord size operand }
       { USED FOR conversions in x86}
       { S_BW = Byte to word       }
       { S_BL = Byte to long       }
       { S_WL = Word to long       }
       { Floating point types      }
       { S_FS  = single type (32 bit) }
       { S_FL  = double/64bit integer }
       { S_FX  = ExtENDed type      }
       { S_IS  = integer on 16 bits   }
       { S_IL  = integer on 32 bits   }
       { S_IQ  = integer on 64 bits   }
  TOpSize=(S_NO,
           S_B,
           S_W,
           S_L,
           S_BW,
           S_BL,
           S_WL,
           S_IS,
           S_IL,
           S_IQ,
           S_FS,
           S_FL,
           S_FX,
           S_D,
           S_Q,
           S_FV,
           S_NEAR,
           S_FAR,
           S_SHORT);
CONST
  { Intel style operands ! }
  opsize_2_type:ARRAY[0..2,topsize] of LongInt=(
    (OT_NONE,
     OT_BITS8,OT_BITS16,OT_BITS32,OT_BITS16,OT_BITS32,OT_BITS32,
     OT_BITS16,OT_BITS32,OT_BITS64,
     OT_BITS32,OT_BITS64,OT_BITS80,OT_BITS64,OT_BITS64,OT_BITS64,
     OT_NEAR,OT_FAR,OT_SHORT
    ),
    (OT_NONE,
     OT_BITS8,OT_BITS16,OT_BITS32,OT_BITS8,OT_BITS8,OT_BITS16,
     OT_BITS16,OT_BITS32,OT_BITS64,
     OT_BITS32,OT_BITS64,OT_BITS80,OT_BITS64,OT_BITS64,OT_BITS64,
     OT_NEAR,OT_FAR,OT_SHORT
    ),
    (OT_NONE,
     OT_BITS8,OT_BITS16,OT_BITS32,OT_NONE,OT_NONE,OT_NONE,
     OT_BITS16,OT_BITS32,OT_BITS64,
     OT_BITS32,OT_BITS64,OT_BITS80,OT_BITS64,OT_BITS64,OT_BITS64,
     OT_NEAR,OT_FAR,OT_SHORT
    )
  );

{$IFDEF ATTOP}
  att_opsize2str : ARRAY[topsize] of string[2] = ('',
    'b','w','l','bw','bl','wl',
    's','l','q',
    's','l','t','d','q','v',
    '','',''
  );
{$ENDIF}
{*****************************************************************************
                                Conditions
*****************************************************************************}
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
  CondAsmOpStr:ARRAY[0..CondAsmOps-1] of string[4]=('FCMPd','JMPL','FCMPs');
{*****************************************************************************
                                  Registers
*****************************************************************************}
TYPE
  { enumeration for registers, don't change the order }
  { it's used by the register size conversions        }
  TRegister=({$INCLUDE registers.inc});
  TRegister64=PACKED RECORD
  {A type to store register locations for 64 Bit values.}
     RegLo,RegHi:TRegister;
  END;
  treg64=tregister64;{alias for compact code}
  TRegisterSet=SET OF TRegister;
  reg2strtable=ARRAY[tregister] OF STRING[6];
CONST
  firstreg = low(tregister);
  lastreg  = high(tregister);
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
  treference = packed record
     segment,
     base,
     index       : tregister;
     scalefactor : byte;
     offset      : LongInt;
     symbol      : tasmsymbol;
     offsetfixup : LongInt;
     options     : trefoptions;
{$ifdef newcg}
     alignment   : byte;
{$ENDif newcg}
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
        toptype=(top_none,top_reg,top_ref,top_CONST,top_symbol);

        toper=record
          ot  : LongInt;
          case typ : toptype of
           top_none   : ();
           top_reg    : (reg:tregister);
           top_ref    : (ref:poperreference);
           top_CONST  : (val:aword);
           top_symbol : (sym:tasmsymbol;symofs:LongInt);
        END;



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

CONST
  general_registers = [R_G0..R_I7];

  { legEND:                                                                }
  { xxxregs = set of all possibly used registers of that type in the code  }
  {           generator                                                    }
  { usableregsxxx = set of all 32bit components of registers that can be   }
  {           possible allocated to a regvar or using getregisterxxx (this }
  {           excludes registers which can be only used for parameter      }
  {           passing on ABI's that define this)                           }
  { c_countusableregsxxx = amount of registers in the usableregsxxx set    }

  intregs = [R_G0..R_I7];
  usableregsint = general_registers;
  c_countusableregsint = 4;

  fpuregs = [R_F0..R_F31];
  usableregsfpu = [];
  c_countusableregsfpu = 0;

  mmregs = [R_G0..R_G7];
  usableregsmm = [R_G0..R_G7];
  c_countusableregsmm  = 8;

  firstsaveintreg = R_G0;
  lastsaveintreg = R_I7;
  firstsavefpureg = R_F0;
  lastsavefpureg = R_F31;
  firstsavemmreg = R_G0;
  lastsavemmreg = R_I7;
  lowsavereg = R_G0;
  highsavereg = R_I7;

  ALL_REGISTERS = [lowsavereg..highsavereg];

  lvaluelocations = [LOC_REFERENCE,LOC_CFPUREGISTER,
    LOC_CREGISTER,LOC_MMXREGISTER,LOC_CMMXREGISTER];
{
  registers_saved_on_cdecl = [R_ESI,R_EDI,R_EBX];}

{*****************************************************************************
                               GDB Information
*****************************************************************************}

      {# Register indexes for stabs information, when some
         parameters or variables are stored in registers.

         Taken from rs6000.h (DBX_REGISTER_NUMBER)
         from GCC 3.x source code. PowerPC has 1:1 mapping
         according to the order of the registers defined
         in GCC

      }

  stab_regindex:ARRAY[tregister]OF ShortInt=({$INCLUDE stabregi.inc});
  { generic register names }
  stack_pointer_reg =R_O6;
  frame_pointer_reg =R_I6;
  self_pointer_reg  =R_G5;
{There is no accumulator in the SPARC architecture. There are just families of
registers. All registers belonging to the same family are identical except in
the "global registers" family where GO is different from the others : G0 gives
always 0 when it is red and thows away any value written to it}
  accumulator     = R_L0;
  accumulatorhigh = R_L7;
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
  std_saved_registers=[R_O6];
{# Required parameter alignment when calling a routine declared as stdcall and
cdecl. The alignment value should be the one defined by GCC or the target ABI.

The value of this constant is equal to the constant
PARM_BOUNDARY / BITS_PER_UNIT in the GCC source.}
  std_param_align=4;
{# Registers which are defined as scratch and no need to save across routine
calls or in assembler blocks.}
  ScratchRegsCount=3;
  scratch_regs:ARRAY[1..ScratchRegsCount]OF TRegister=(R_O4,R_O5,R_I7);
  {$WARNING FIXME : Scratch registers list has to be verified}
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
  sizepostfix_pointer = S_L;


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

    CONST
       maxvarregs=30;
       VarRegs:ARRAY[1..maxvarregs]OF TRegister=(
             R_G0,R_G1,R_G2,R_G3,R_G4,R_G5,R_G6,R_G7,
             R_O0,R_O1,R_O2,R_O3,R_O4,R_O5,{R_R14=R_SP}R_O7,
             R_L0,R_L1,R_L2,R_L3,R_L4,R_L5,R_L6,R_L7,
             R_I0,R_I1,R_I2,R_I3,R_I4,R_I5,{R_R30=R_FP}R_I7
        );
       maxfpuvarregs = 8;
       max_operands = 3;

       maxintregs = maxvarregs;
       maxfpuregs = maxfpuvarregs;

FUNCTION reg2str(r:tregister):string;
FUNCTION is_calljmp(o:tasmop):boolean;
FUNCTION flags_to_cond(CONST f:TResFlags):TAsmCond;
IMPLEMENTATION
FUNCTION reg2str(r:tregister):string;
  TYPE
    TStrReg=ARRAY[TRegister]OF STRING[5];
  CONST
    StrReg:TStrReg=({$INCLUDE strregs.inc});
  BEGIN
    reg2str:=StrReg[r];
  END;
FUNCTION is_calljmp(o:tasmop):boolean;
  BEGIN
    CASE o OF
    A_CALL,A_JMPL:
      is_calljmp:=true;
    ELSE
      is_calljmp:=false;
    END;
  END;
FUNCTION flags_to_cond(CONST f:TResFlags):TAsmCond;
  CONST
    flags_2_cond:ARRAY[TResFlags]OF TAsmCond=(C_E,C_NE,C_G,C_L,C_GE,C_LE,C_C,C_NC,C_A,C_AE,C_B,C_BE);
  BEGIN
    result:=flags_2_cond[f];
  END;
END.
{
  $Log$
  Revision 1.6  2002-09-24 03:57:53  mazen
  * some cleanup  was made

}
