{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl and Peter Vreman

    Contains the base types for the i386

    * This code was inspired by the NASM sources
      The Netwide Assembler is copyright (C) 1996 Simon Tatham and
      Julian Hall. All rights reserved.

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

 ****************************************************************************
}
{# Base unit for processor information. This unit contains
   enumerations of registers, opcodes, sizes, and other
   such things which are processor specific.
}
unit cpubase;

{$i defines.inc}

interface

uses
  globals,cutils,cclasses,aasm,cpuinfo,cginfo;

const
{ Size of the instruction table converted by nasmconv.pas }
  instabentries = {$i i386nop.inc}
  maxinfolen    = 8;

{ By default we want everything }
{$define ATTOP}
{$define INTELOP}
{$define ITTABLE}

{ We Don't need the intel style opcodes if we don't have a intel
  reader or generator }
{$ifdef NORA386INT}
  {$ifdef NOAG386NSM}
    {$ifdef NOAG386INT}
      {$undef INTELOP}
    {$endif}
  {$endif}
{$endif}

{ We Don't need the AT&T style opcodes if we don't have a AT&T
  reader or generator }
{$ifdef NORA386ATT}
  {$ifdef NOAG386ATT}
    {$undef ATTOP}
    {$ifdef NOAG386DIR}
       {$undef ATTREG}
    {$endif}
  {$endif}
{$endif}

{ We need the AT&T suffix table for both asm readers and AT&T writer }
{$define ATTSUF}
{$ifdef NORA386INT}
  {$ifdef NORA386ATT}
    {$ifdef NOAG386ATT}
      {$undef ATTSUF}
    {$endif}
  {$endif}
{$endif}

const
{ Operand types }
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
  OT_NON_SIZE  = longint(not OT_SIZE_MASK);

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
  OT_REG_FSGS  = $04081002;  { FS, GS (386 extended registers)  }

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
  IF_PMASK  =
     longint($FF000000);  { the mask for processor types  }
  IF_PFMASK =
     longint($F001FF00);  { the mask for disassembly "prefer"  }
  IF_8086   = $00000000;  { 8086 instruction  }
  IF_186    = $01000000;  { 186+ instruction  }
  IF_286    = $02000000;  { 286+ instruction  }
  IF_386    = $03000000;  { 386+ instruction  }
  IF_486    = $04000000;  { 486+ instruction  }
  IF_PENT   = $05000000;  { Pentium instruction  }
  IF_P6     = $06000000;  { P6 instruction  }
  IF_KATMAI = $07000000;  { Katmai instructions  }
  IF_CYRIX  = $10000000;  { Cyrix-specific instruction  }
  IF_AMD    = $20000000;  { AMD-specific instruction  }
  { added flags }
  IF_PRE    = $40000000;  { it's a prefix instruction }
  IF_PASS2  =
     longint($80000000);  { if the instruction can change in a second pass }

type

  TAsmOp={$i i386op.inc}

  {# This should define the array of instructions as string }
  op2strtable=array[tasmop] of string[11];

Const

  {# First value of opcode enumeration }
  firstop = low(tasmop);
  {# Last value of opcode enumeration  }
  lastop  = high(tasmop);


{*****************************************************************************
                                Operand Sizes
*****************************************************************************}

type
  topsize = (S_NO,
    S_B,S_W,S_L,S_BW,S_BL,S_WL,
    S_IS,S_IL,S_IQ,
    S_FS,S_FL,S_FX,S_D,S_Q,S_FV,
    S_NEAR,S_FAR,S_SHORT
  );

const
  { Intel style operands ! }
  opsize_2_type:array[0..2,topsize] of longint=(
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



{*****************************************************************************
                                Conditions
*****************************************************************************}

type
  TAsmCond=(C_None,
    C_A,C_AE,C_B,C_BE,C_C,C_E,C_G,C_GE,C_L,C_LE,C_NA,C_NAE,
    C_NB,C_NBE,C_NC,C_NE,C_NG,C_NGE,C_NL,C_NLE,C_NO,C_NP,
    C_NS,C_NZ,C_O,C_P,C_PE,C_PO,C_S,C_Z
  );

const
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


{*****************************************************************************
                                  Registers
*****************************************************************************}

type
  {# Enumeration for all possible registers for cpu. It
    is to note that all registers of the same type
    (for example all FPU registers), should be grouped
    together.
  }
  { don't change the order }
  { it's used by the register size conversions        }
  tregister = (R_NO,
    R_EAX,R_ECX,R_EDX,R_EBX,R_ESP,R_EBP,R_ESI,R_EDI,
    R_AX,R_CX,R_DX,R_BX,R_SP,R_BP,R_SI,R_DI,
    R_AL,R_CL,R_DL,R_BL,R_AH,R_CH,R_BH,R_DH,
    R_CS,R_DS,R_ES,R_SS,R_FS,R_GS,
    R_ST,R_ST0,R_ST1,R_ST2,R_ST3,R_ST4,R_ST5,R_ST6,R_ST7,
    R_DR0,R_DR1,R_DR2,R_DR3,R_DR6,R_DR7,
    R_CR0,R_CR2,R_CR3,R_CR4,
    R_TR3,R_TR4,R_TR5,R_TR6,R_TR7,
    R_MM0,R_MM1,R_MM2,R_MM3,R_MM4,R_MM5,R_MM6,R_MM7,
    R_XMM0,R_XMM1,R_XMM2,R_XMM3,R_XMM4,R_XMM5,R_XMM6,R_XMM7
  );

  {# Set type definition for registers }
  tregisterset = set of tregister;

  {# Type definition for the array of string of register nnames }
  reg2strtable = array[tregister] of string[6];

const
  {# First register in the tregister enumeration }
  firstreg = low(tregister);
  {# Last register in the tregister enumeration }
  lastreg  = high(tregister);

  firstsreg = R_CS;
  lastsreg  = R_GS;

  regset8bit  : tregisterset = [R_AL..R_DH];
  regset16bit : tregisterset = [R_AX..R_DI,R_CS..R_SS];
  regset32bit : tregisterset = [R_EAX..R_EDI];

  { Convert reg to opsize }
  reg2opsize : array[firstreg..lastreg] of topsize = (S_NO,
    S_L,S_L,S_L,S_L,S_L,S_L,S_L,S_L,
    S_W,S_W,S_W,S_W,S_W,S_W,S_W,S_W,
    S_B,S_B,S_B,S_B,S_B,S_B,S_B,S_B,
    S_W,S_W,S_W,S_W,S_W,S_W,
    S_FL,S_FL,S_FL,S_FL,S_FL,S_FL,S_FL,S_FL,S_FL,
    S_L,S_L,S_L,S_L,S_L,S_L,
    S_L,S_L,S_L,S_L,
    S_L,S_L,S_L,S_L,S_L,
    S_D,S_D,S_D,S_D,S_D,S_D,S_D,S_D,
    S_D,S_D,S_D,S_D,S_D,S_D,S_D,S_D
  );

  { Convert reg to operand type }
  reg2type : array[firstreg..lastreg] of longint = (OT_NONE,
    OT_REG_EAX,OT_REG_ECX,OT_REG32,OT_REG32,OT_REG32,OT_REG32,OT_REG32,OT_REG32,
    OT_REG_AX,OT_REG_CX,OT_REG_DX,OT_REG16,OT_REG16,OT_REG16,OT_REG16,OT_REG16,
    OT_REG_AL,OT_REG_CL,OT_REG8,OT_REG8,OT_REG8,OT_REG8,OT_REG8,OT_REG8,
    OT_REG_CS,OT_REG_DESS,OT_REG_DESS,OT_REG_DESS,OT_REG_FSGS,OT_REG_FSGS,
    OT_FPU0,OT_FPU0,OT_FPUREG,OT_FPUREG,OT_FPUREG,OT_FPUREG,OT_FPUREG,OT_FPUREG,OT_FPUREG,
    OT_REG_DREG,OT_REG_DREG,OT_REG_DREG,OT_REG_DREG,OT_REG_DREG,OT_REG_DREG,
    OT_REG_CREG,OT_REG_CREG,OT_REG_CREG,OT_REG_CR4,
    OT_REG_TREG,OT_REG_TREG,OT_REG_TREG,OT_REG_TREG,OT_REG_TREG,
    OT_MMXREG,OT_MMXREG,OT_MMXREG,OT_MMXREG,OT_MMXREG,OT_MMXREG,OT_MMXREG,OT_MMXREG,
    OT_XMMREG,OT_XMMREG,OT_XMMREG,OT_XMMREG,OT_XMMREG,OT_XMMREG,OT_XMMREG,OT_XMMREG
  );

  {# Standard opcode string table (for each tasmop enumeration). The
     opcode strings should conform to the names as defined by the
     processor manufacturer.
  }
  std_op2str:op2strtable={$i i386int.inc}

  {# Standard register table (for each tregister enumeration). The
     register strings should conform to the the names as defined
     by the processor manufacturer
  }
  std_reg2str : reg2strtable = ('',
    'eax','ecx','edx','ebx','esp','ebp','esi','edi',
    'ax','cx','dx','bx','sp','bp','si','di',
    'al','cl','dl','bl','ah','ch','bh','dh',
    'cs','ds','es','ss','fs','gs',
    'st','st(0)','st(1)','st(2)','st(3)','st(4)','st(5)','st(6)','st(7)',
    'dr0','dr1','dr2','dr3','dr6','dr7',
    'cr0','cr2','cr3','cr4',
    'tr3','tr4','tr5','tr6','tr7',
    'mm0','mm1','mm2','mm3','mm4','mm5','mm6','mm7',
    'xmm0','xmm1','xmm2','xmm3','xmm4','xmm5','xmm6','xmm7'
  );


{*****************************************************************************
                                   Flags
*****************************************************************************}

type
  TResFlags = (F_E,F_NE,F_G,F_L,F_GE,F_LE,F_C,F_NC,F_A,F_AE,F_B,F_BE);

{*****************************************************************************
                                Reference
*****************************************************************************}

type
  trefoptions=(ref_none,ref_parafixup,ref_localfixup,ref_selffixup);

type
  { immediate/reference record }
  poperreference = ^treference;
  treference = packed record
     segment,
     base,
     index       : tregister;
     scalefactor : byte;
     offset      : longint;
     symbol      : tasmsymbol;
     offsetfixup : longint;
     options     : trefoptions;
  end;

{*****************************************************************************
                                Operands
*****************************************************************************}

       { Types of operand }
        toptype=(top_none,top_reg,top_ref,top_const,top_symbol);

        toper=record
          ot  : longint;
          case typ : toptype of
           top_none   : ();
           top_reg    : (reg:tregister);
           top_ref    : (ref:poperreference);
           top_const  : (val:aword);
           top_symbol : (sym:tasmsymbol;symofs:longint);
        end;



{*****************************************************************************
                             Argument Classification
*****************************************************************************}


{*****************************************************************************
                               Generic Location
*****************************************************************************}

type
  TLoc=(
    LOC_INVALID,      { added for tracking problems}
    LOC_CONSTANT,     { constant value }
    LOC_JUMP,         { boolean results only, jump to false or true label }
    LOC_FLAGS,        { boolean results only, flags are set }
    LOC_CREFERENCE,   { in memory constant value reference (cannot change) }
    LOC_REFERENCE,    { in memory value }
    LOC_REGISTER,     { in a processor register }
    LOC_CREGISTER,    { Constant register which shouldn't be modified }
    LOC_FPUREGISTER,  { FPU stack }
    LOC_CFPUREGISTER, { if it is a FPU register variable on the fpu stack }
    LOC_MMXREGISTER,  { MMX register }
    LOC_CMMXREGISTER, { MMX register variable }
    LOC_SSEREGISTER,
    LOC_CSSEREGISTER
  );

  plocation = ^tlocation;
  tlocation = packed record
     loc  : TLoc;
     size : TCGSize;
     case TLoc of
        LOC_FLAGS : (resflags : tresflags);
        LOC_CONSTANT : (
          case longint of
            1 : (value : AWord);
            2 : (valuelow, valuehigh:AWord);
          );
        LOC_CREFERENCE,
        LOC_REFERENCE : (reference : treference);
        { segment in reference at the same place as in loc_register }
        LOC_REGISTER,LOC_CREGISTER : (
          case longint of
            1 : (register,segment,registerhigh : tregister);
            { overlay a registerlow }
            2 : (registerlow : tregister);
          );
        { it's only for better handling }
        LOC_MMXREGISTER,LOC_CMMXREGISTER : (mmxreg : tregister);
  end;

{*****************************************************************************
                                 Constants
*****************************************************************************}

const
  general_registers = [R_EAX,R_EBX,R_ECX,R_EDX];
  
  {# Table of registers which can be allocated by the code generator       
     internally, when generating the code.                             
  }   
  { legend:                                                                }
  { xxxregs = set of all possibly used registers of that type in the code  }
  {           generator                                                    }
  { usableregsxxx = set of all 32bit components of registers that can be   }
  {           possible allocated to a regvar or using getregisterxxx (this }
  {           excludes registers which can be only used for parameter      }
  {           passing on ABI's that define this)                           }
  { c_countusableregsxxx = amount of registers in the usableregsxxx set    }

  intregs = [R_EAX..R_BL];
  usableregsint = general_registers;
  c_countusableregsint = 4;

  fpuregs = [R_ST0..R_ST7];
  usableregsfpu = [];
  c_countusableregsfpu = 0;

  mmregs = [R_MM0..R_MM7];
  usableregsmm = [R_MM0..R_MM7];
  c_countusableregsmm  = 8;

  firstsaveintreg = R_EAX;
  lastsaveintreg = R_EBX;
  firstsavefpureg = R_NO;
  lastsavefpureg = R_NO;
  firstsavemmreg = R_MM0;
  lastsavemmreg = R_MM7;

  {# Constant defining possibly all registers which might require saving }
  ALL_REGISTERS = [firstreg..lastreg];

  lvaluelocations = [LOC_REFERENCE,LOC_CFPUREGISTER,
    LOC_CREGISTER,LOC_MMXREGISTER,LOC_CMMXREGISTER];

{*****************************************************************************
                          Default generic sizes 
*****************************************************************************}
   {# Defines the default address size for a processor, }
   OS_ADDR = OS_32;
   {# the natural int size for a processor,             }
   OS_INT = OS_32;
   {# the maximum float size for a processor,           }
   OS_FLOAT = OS_F80;
   {# the size of a vector register for a processor     }
   OS_VECTOR = OS_M64;

{*****************************************************************************
                          Generic Register names
*****************************************************************************}

  {# Stack pointer register }
  stack_pointer_reg = R_ESP;
  {# Frame pointer register }
  frame_pointer_reg = R_EBP;
  {# Self pointer register : contains the instance address of an 
     object or class. }
  self_pointer_reg  = R_ESI;
  {# Register for addressing absolute data in a position independant way, 
     such as in PIC code. The exact meaning is ABI specific }
  pic_offset_reg = R_EBX;
  {# Results are returned in this register (32-bit values) }
  accumulator   = R_EAX;
  {# Hi-Results are returned in this register (64-bit value high register) }
  accumulatorhigh = R_EDX;
  { WARNING: don't change to R_ST0!! See comments above implementation of }
  { a_loadfpu* methods in rgcpu (JM)                                      }
  fpuresultreg = R_ST;
  mmresultreg = R_MM0;

  {# Registers which are defined as scratch and no need to save across 
     routine calls or in assembler blocks.
  }
  scratch_regs : array[1..1] of tregister = (R_EDI);

  

{*****************************************************************************
                       GCC /ABI linking information
*****************************************************************************}

  {# Registers which must be saved when calling a routine declared as
     cppdecl, cdecl, stdcall, safecall, palmossyscall. The registers
     saved should be the ones as defined in the target ABI and / or GCC.
     
     This value can be deduced from the CALLED_USED_REGISTERS array in the
     GCC source.
  }
  std_saved_registers = [R_ESI,R_EDI,R_EBX];
  {# Required parameter alignment when calling a routine declared as
     stdcall and cdecl. The alignment value should be the one defined
     by GCC or the target ABI. 
     
     The value of this constant is equal to the constant 
     PARM_BOUNDARY / BITS_PER_UNIT in the GCC source.
  }     
  std_param_align = 4;
  




{*****************************************************************************
                                  Helpers
*****************************************************************************}

    const
       maxvarregs = 4;
       varregs : array[1..maxvarregs] of tregister =
         (R_EBX,R_EDX,R_ECX,R_EAX);

       maxfpuvarregs = 8;
       max_operands = 3;

       maxintregs = maxvarregs;
       maxfpuregs = maxfpuvarregs;


    function is_calljmp(o:tasmop):boolean;

    function flags_to_cond(const f: TResFlags) : TAsmCond;


implementation

  uses
{$ifdef heaptrc}
      ppheap,
{$endif heaptrc}
      verbose;


{*****************************************************************************
                                  Helpers
*****************************************************************************}


    function is_calljmp(o:tasmop):boolean;
      begin
        case o of
          A_CALL,
          A_JCXZ,
          A_JECXZ,
          A_JMP,
          A_LOOP,
          A_LOOPE,
          A_LOOPNE,
          A_LOOPNZ,
          A_LOOPZ,
          A_Jcc :
            is_calljmp:=true;
          else
            is_calljmp:=false;
        end;
      end;





    function flags_to_cond(const f: TResFlags) : TAsmCond;
      const
        flags_2_cond : array[TResFlags] of TAsmCond =
          (C_E,C_NE,C_G,C_L,C_GE,C_LE,C_C,C_NC,C_A,C_AE,C_B,C_BE);
      begin
        result := flags_2_cond[f];
      end;


end.
{
  $Log$
  Revision 1.18  2002-04-21 15:31:40  carl
  - removed some other stuff to their units

  Revision 1.17  2002/04/20 21:37:07  carl
  + generic FPC_CHECKPOINTER
  + first parameter offset in stack now portable
  * rename some constants
  + move some cpu stuff to other units
  - remove unused constents
  * fix stacksize for some targets
  * fix generic size problems which depend now on EXTEND_SIZE constant
  * removing frame pointer in routines is only available for : i386,m68k and vis targets

  Revision 1.16  2002/04/15 19:53:54  peter
    * fixed conflicts between the last 2 commits

  Revision 1.15  2002/04/15 19:44:20  peter
    * fixed stackcheck that would be called recursively when a stack
      error was found
    * generic changeregsize(reg,size) for i386 register resizing
    * removed some more routines from cga unit
    * fixed returnvalue handling
    * fixed default stacksize of linux and go32v2, 8kb was a bit small :-)

  Revision 1.14  2002/04/15 19:12:09  carl
  + target_info.size_of_pointer -> pointer_size
  + some cleanup of unused types/variables
  * move several constants from cpubase to their specific units
    (where they are used)
  + att_Reg2str -> gas_reg2str
  + int_reg2str -> std_reg2str

  Revision 1.13  2002/04/14 16:59:41  carl
  + att_reg2str -> gas_reg2str

  Revision 1.12  2002/04/02 17:11:34  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.11  2002/03/31 20:26:37  jonas
    + a_loadfpu_* and a_loadmm_* methods in tcg
    * register allocation is now handled by a class and is mostly processor
      independent (+rgobj.pas and i386/rgcpu.pas)
    * temp allocation is now handled by a class (+tgobj.pas, -i386\tgcpu.pas)
    * some small improvements and fixes to the optimizer
    * some register allocation fixes
    * some fpuvaroffset fixes in the unary minus node
    * push/popusedregisters is now called rg.save/restoreusedregisters and
      (for i386) uses temps instead of push/pop's when using -Op3 (that code is
      also better optimizable)
    * fixed and optimized register saving/restoring for new/dispose nodes
    * LOC_FPU locations now also require their "register" field to be set to
      R_ST, not R_ST0 (the latter is used for LOC_CFPUREGISTER locations only)
    - list field removed of the tnode class because it's not used currently
      and can cause hard-to-find bugs

  Revision 1.10  2002/03/04 19:10:12  peter
    * removed compiler warnings

  Revision 1.9  2001/12/30 17:24:46  jonas
    * range checking is now processor independent (part in cgobj,
      part in cg64f32) and should work correctly again (it needed
      some changes after the changes of the low and high of
      tordef's to int64)
    * maketojumpbool() is now processor independent (in ncgutil)
    * getregister32 is now called getregisterint

  Revision 1.8  2001/12/29 15:29:59  jonas
    * powerpc/cgcpu.pas compiles :)
    * several powerpc-related fixes
    * cpuasm unit is now based on common tainst unit
    + nppcmat unit for powerpc (almost complete)

  Revision 1.7  2001/12/06 17:57:40  florian
    + parasym to tparaitem added

  Revision 1.6  2001/09/28 20:39:33  jonas
    * changed all flow control structures (except for exception handling
      related things) to processor independent code (in new ncgflw unit)
    + generic cgobj unit which contains lots of code generator helpers with
      global "cg" class instance variable
    + cgcpu unit for i386 (implements processor specific routines of the above
      unit)
    * updated cgbase and cpubase for the new code generator units
    * include ncgflw unit in cpunode unit

  Revision 1.5  2001/05/18 23:01:13  peter
    * portable constants

  Revision 1.4  2001/04/13 01:22:18  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.3  2001/02/20 21:34:04  peter
    * iret, lret fixes

  Revision 1.2  2000/12/07 17:19:45  jonas
    * new constant handling: from now on, hex constants >$7fffffff are
      parsed as unsigned constants (otherwise, $80000000 got sign extended
      and became $ffffffff80000000), all constants in the longint range
      become longints, all constants >$7fffffff and <=cardinal($ffffffff)
      are cardinals and the rest are int64's.
    * added lots of longint typecast to prevent range check errors in the
      compiler and rtl
    * type casts of symbolic ordinal constants are now preserved
    * fixed bug where the original resulttype wasn't restored correctly
      after doing a 64bit rangecheck

  Revision 1.1  2000/10/15 09:39:37  peter
    * moved cpu*.pas to i386/
    * renamed n386 to common cpunode

  Revision 1.7  2000/09/26 20:06:13  florian
    * hmm, still a lot of work to get things compilable

  Revision 1.6  2000/09/24 15:06:14  peter
    * use defines.inc

  Revision 1.5  2000/08/27 16:11:50  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.4  2000/08/05 13:25:06  peter
    * packenum 1 fixes (merged)

  Revision 1.3  2000/07/14 05:11:48  michael
  + Patch to 1.1

  Revision 1.2  2000/07/13 11:32:39  michael
  + removed logs

}
