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
unit cpubase;

{$i defines.inc}

interface

uses
  globals,cutils,cclasses,aasm;

const
{ Size of the instruction table converted by nasmconv.pas }
  instabentries = {$i i386nop.inc}
  maxinfolen    = 8;

{ By default we want everything }
{$define ATTOP}
{$define ATTREG}
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
  TAttSuffix = (AttSufNONE,AttSufINT,AttSufFPU,AttSufFPUint);

  TAsmOp=
{$i i386op.inc}

  op2strtable=array[tasmop] of string[11];

  tstr2opentry = class(Tnamedindexitem)
    op: TAsmOp;
  end;

const
  firstop = low(tasmop);
  lastop  = high(tasmop);

  AsmPrefixes = 6;
  AsmPrefix : array[0..AsmPrefixes-1] of TasmOP =(
    A_LOCK,A_REP,A_REPE,A_REPNE,A_REPNZ,A_REPZ
  );

  AsmOverrides = 6;
  AsmOverride : array[0..AsmOverrides-1] of TasmOP =(
    A_SEGCS,A_SEGES,A_SEGDS,A_SEGFS,A_SEGGS,A_SEGSS
  );


{$ifdef INTELOP}
  int_op2str:op2strtable=
{$i i386int.inc}
{$endif INTELOP}

{$ifdef ATTOP}
  att_op2str:op2strtable=
{$i i386att.inc}
{$endif ATTOP}

{$ifdef ATTSUF}
  att_needsuffix:array[tasmop] of TAttSuffix=
{$i i386atts.inc}
{$endif ATTSUF}


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

{$ifdef ATTOP}
  att_opsize2str : array[topsize] of string[2] = ('',
    'b','w','l','bw','bl','wl',
    's','l','q',
    's','l','t','d','q','v',
    '','',''
  );
{$endif}


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

const
  CondAsmOps=3;
  CondAsmOp:array[0..CondAsmOps-1] of TasmOp=(
    A_CMOVcc, A_Jcc, A_SETcc
  );
  CondAsmOpStr:array[0..CondAsmOps-1] of string[4]=(
    'CMOV','J','SET'
  );


{*****************************************************************************
                                  Registers
*****************************************************************************}

type
  { enumeration for registers, don't change the order }
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

  tregisterset = set of tregister;

  reg2strtable = array[tregister] of string[6];

const
  firstreg = low(tregister);
  lastreg  = high(tregister);

  firstsreg = R_CS;
  lastsreg  = R_GS;

  regset8bit  : tregisterset = [R_AL..R_DH];
  regset16bit : tregisterset = [R_AX..R_DI,R_CS..R_SS];
  regset32bit : tregisterset = [R_EAX..R_EDI];

  { Convert reg to opsize }
  reg_2_opsize:array[firstreg..lastreg] of topsize = (S_NO,
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
  reg_2_type:array[firstreg..lastreg] of longint = (OT_NONE,
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

{$ifdef INTELOP}
  int_reg2str : reg2strtable = ('',
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

  int_nasmreg2str : reg2strtable = ('',
    'eax','ecx','edx','ebx','esp','ebp','esi','edi',
    'ax','cx','dx','bx','sp','bp','si','di',
    'al','cl','dl','bl','ah','ch','bh','dh',
    'cs','ds','es','ss','fs','gs',
    'st0','st0','st1','st2','st3','st4','st5','st6','st7',
    'dr0','dr1','dr2','dr3','dr6','dr7',
    'cr0','cr2','cr3','cr4',
    'tr3','tr4','tr5','tr6','tr7',
    'mm0','mm1','mm2','mm3','mm4','mm5','mm6','mm7',
    'xmm0','xmm1','xmm2','xmm3','xmm4','xmm5','xmm6','xmm7'
  );
{$endif}

{$ifdef ATTREG}
  att_reg2str : reg2strtable = ('',
    '%eax','%ecx','%edx','%ebx','%esp','%ebp','%esi','%edi',
    '%ax','%cx','%dx','%bx','%sp','%bp','%si','%di',
    '%al','%cl','%dl','%bl','%ah','%ch','%bh','%dh',
    '%cs','%ds','%es','%ss','%fs','%gs',
    '%st','%st(0)','%st(1)','%st(2)','%st(3)','%st(4)','%st(5)','%st(6)','%st(7)',
    '%dr0','%dr1','%dr2','%dr3','%dr6','%dr7',
    '%cr0','%cr2','%cr3','%cr4',
    '%tr3','%tr4','%tr5','%tr6','%tr7',
    '%mm0','%mm1','%mm2','%mm3','%mm4','%mm5','%mm6','%mm7',
    '%xmm0','%xmm1','%xmm2','%xmm3','%xmm4','%xmm5','%xmm6','%xmm7'
  );
{$endif ATTREG}


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

  { immediate/reference record }
  preference = ^treference;
  treference = packed record
     is_immediate : boolean; { is this used as reference or immediate }
     segment,
     base,
     index       : tregister;
     scalefactor : byte;
     offset      : longint;
     symbol      : tasmsymbol;
     offsetfixup : longint;
     options     : trefoptions;
{$ifdef newcg}
     alignment   : byte;
{$endif newcg}
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
           top_ref    : (ref:preference);
           top_const  : (val:longint);
           top_symbol : (sym:tasmsymbol;symofs:longint);
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
  TLoc=(
    LOC_INVALID,      { added for tracking problems}
    LOC_FPU,          { FPU stack }
    LOC_REGISTER,     { in a processor register }
    LOC_MEM,          { in memory }
    LOC_REFERENCE,    { like LOC_MEM, but lvalue }
    LOC_JUMP,         { boolean results only, jump to false or true label }
    LOC_FLAGS,        { boolean results only, flags are set }
    LOC_CREGISTER,    { Constant register which shouldn't be modified }
    LOC_MMXREGISTER,  { MMX register }
    LOC_CMMXREGISTER, { MMX register variable }
    LOC_CFPUREGISTER, { if it is a FPU register variable on the fpu stack }
    LOC_SSEREGISTER,
    LOC_CSSEREGISTER
  );

  plocation = ^tlocation;
  tlocation = packed record
     case loc : tloc of
        LOC_MEM,LOC_REFERENCE : (reference : treference);
        LOC_FPU : ();
        LOC_JUMP : ();
        LOC_FLAGS : (resflags : tresflags);
        LOC_INVALID : ();

        { it's only for better handling }
        LOC_MMXREGISTER : (mmxreg : tregister);
        { segment in reference at the same place as in loc_register }
        LOC_REGISTER,LOC_CREGISTER : (
        case longint of
          1 : (register,segment,registerhigh : tregister);
          { overlay a registerlow }
          2 : (registerlow : tregister);
        );
  end;

{*****************************************************************************
                                 Constants
*****************************************************************************}

const
  general_registers = [R_EAX,R_EBX,R_ECX,R_EDX];

  intregs = general_registers;
  fpuregs = [];
  mmregs = [R_MM0..R_MM7];

  lvaluelocations = [LOC_REFERENCE,LOC_CFPUREGISTER,
    LOC_CREGISTER,LOC_MMXREGISTER,LOC_CMMXREGISTER];

  registers_saved_on_cdecl = [R_ESI,R_EDI,R_EBX];

  { generic register names }
  stack_pointer = R_ESP;
  frame_pointer = R_EBP;
  self_pointer  = R_ESI;
  accumulator   = R_EAX;
  accumulatorhigh = R_EDX;
  { the register where the vmt offset is passed to the destructor }
  { helper routine                                                }
  vmt_offset_reg = R_EDI;

  scratch_regs : array[1..1] of tregister = (R_EDI);

{ low and high of the available maximum width integer general purpose }
{ registers                                                           }
  LoGPReg = R_EAX;
  HiGPReg = R_EDI;

{ low and high of every possible width general purpose register (same as }
{ above on most architctures apart from the 80x86)                       }
  LoReg = R_EAX;
  HiReg = R_BL;

  cpuflags = [];

  { sizes }
  pointersize   = 4;
  extended_size = 10;
  sizepostfix_pointer = S_L;


{*****************************************************************************
                              Instruction table
*****************************************************************************}

{$ifndef NOAG386BIN}
type
  tinsentry=packed record
    opcode  : tasmop;
    ops     : byte;
    optypes : array[0..2] of longint;
    code    : array[0..maxinfolen] of char;
    flags   : longint;
  end;
  pinsentry=^tinsentry;

  TInsTabCache=array[TasmOp] of longint;
  PInsTabCache=^TInsTabCache;

const
  InsTab:array[0..instabentries-1] of TInsEntry=
{$i i386tab.inc}

var
  InsTabCache : PInsTabCache;
{$endif NOAG386BIN}


{*****************************************************************************
                   Opcode propeties (needed for optimizer)
*****************************************************************************}

{$ifndef NOOPT}
Type
{What an instruction can change}
  TInsChange = (Ch_None,
     {Read from a register}
     Ch_REAX, Ch_RECX, Ch_REDX, Ch_REBX, Ch_RESP, Ch_REBP, Ch_RESI, Ch_REDI,
     {write from a register}
     Ch_WEAX, Ch_WECX, Ch_WEDX, Ch_WEBX, Ch_WESP, Ch_WEBP, Ch_WESI, Ch_WEDI,
     {read and write from/to a register}
     Ch_RWEAX, Ch_RWECX, Ch_RWEDX, Ch_RWEBX, Ch_RWESP, Ch_RWEBP, Ch_RWESI, Ch_RWEDI,
     {modify the contents of a register with the purpose of using
      this changed content afterwards (add/sub/..., but e.g. not rep
      or movsd)}
     Ch_MEAX, Ch_MECX, Ch_MEDX, Ch_MEBX, Ch_MESP, Ch_MEBP, Ch_MESI, Ch_MEDI,
     Ch_CDirFlag {clear direction flag}, Ch_SDirFlag {set dir flag},
     Ch_RFlags, Ch_WFlags, Ch_RWFlags, Ch_FPU,
     Ch_Rop1, Ch_Wop1, Ch_RWop1,Ch_Mop1,
     Ch_Rop2, Ch_Wop2, Ch_RWop2,Ch_Mop2,
     Ch_Rop3, Ch_WOp3, Ch_RWOp3,Ch_Mop3,
     Ch_WMemEDI,
     Ch_All
  );


const
  MaxCh = 3; { Max things a instruction can change }
type
  TInsProp = packed record
    Ch : Array[1..MaxCh] of TInsChange;
  end;

const
  InsProp : array[tasmop] of TInsProp =
{$i i386prop.inc}

{$endif NOOPT}


{*****************************************************************************
                                  Init/Done
*****************************************************************************}

  procedure InitCpu;
  procedure DoneCpu;

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

    function imm_2_type(l:longint):longint;

    { the following functions allow to convert registers }
    { for example reg8toreg32(R_AL) returns R_EAX        }
    { for example reg16toreg32(R_AL) gives an undefined  }
    { result                                             }
    { these functions expects that the turn of           }
    { tregister isn't changed                            }
    function reg8toreg16(reg : tregister) : tregister;
    function reg8toreg32(reg : tregister) : tregister;
    function reg16toreg8(reg : tregister) : tregister;
    function reg32toreg8(reg : tregister) : tregister;
    function reg32toreg16(reg : tregister) : tregister;
    function reg16toreg32(reg : tregister) : tregister;

    { these procedures must be defined by all target cpus }
    function regtoreg8(reg : tregister) : tregister;
    function regtoreg16(reg : tregister) : tregister;
    function regtoreg32(reg : tregister) : tregister;

    { can be ignored on 32 bit systems }
    function regtoreg64(reg : tregister) : tregister;

    { returns the operand prefix for a given register }
    function regsize(reg : tregister) : topsize;

    { resets all values of ref to defaults }
    procedure reset_reference(var ref : treference);
    { set mostly used values of a new reference }
    function new_reference(base : tregister;offset : longint) : preference;

    function newreference(const r : treference) : preference;
    procedure disposereference(var r : preference);

    function reg2str(r : tregister) : string;

    function is_calljmp(o:tasmop):boolean;

    procedure clear_location(var loc : tlocation);
    procedure set_location(var destloc,sourceloc : tlocation);
    procedure swap_location(var destloc,sourceloc : tlocation);

    procedure inverse_flags(var f: TResFlags);
    function flags_to_cond(const f: TResFlags) : TAsmCond;

implementation

{$ifdef heaptrc}
  uses
      ppheap;
{$endif heaptrc}

{*****************************************************************************
                                  Helpers
*****************************************************************************}

    function imm_2_type(l:longint):longint;
      begin
        if (l>=-128) and (l<=127) then
         imm_2_type:=OT_IMM8 or OT_SIGNED
        else
         if (l>=-255) and (l<=255) then
          imm_2_type:=OT_IMM8
        else
         if (l>=-32768) and (l<=32767) then
          imm_2_type:=OT_IMM16 or OT_SIGNED
        else
         if (l>=-65536) and (l<=65535) then
          imm_2_type:=OT_IMM16 or OT_SIGNED
         else
          imm_2_type:=OT_IMM32;
      end;

    function reg2str(r : tregister) : string;
      const
         a : array[R_NO..R_BL] of string[3] =
          ('','EAX','ECX','EDX','EBX','ESP','EBP','ESI','EDI',
           'AX','CX','DX','BX','SP','BP','SI','DI',
           'AL','CL','DL','BL');
      begin
         if r in [R_ST0..R_ST7] then
           reg2str:='ST('+tostr(longint(r)-longint(R_ST0))+')'
         else
           reg2str:=a[r];
      end;


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


    procedure disposereference(var r : preference);
      begin
         dispose(r);
         r:=nil;
      end;


    function newreference(const r : treference) : preference;
      var
         p : preference;
      begin
         new(p);
         p^:=r;
         newreference:=p;
      end;


    function reg8toreg16(reg : tregister) : tregister;

      begin
         reg8toreg16:=reg32toreg16(reg8toreg32(reg));
      end;

    function reg16toreg8(reg : tregister) : tregister;

      begin
         reg16toreg8:=reg32toreg8(reg16toreg32(reg));
      end;

    function reg16toreg32(reg : tregister) : tregister;

      begin
         reg16toreg32:=tregister(byte(reg)-byte(R_EDI));
      end;

    function reg32toreg16(reg : tregister) : tregister;

      begin
         reg32toreg16:=tregister(byte(reg)+byte(R_EDI));
      end;

    function reg32toreg8(reg : tregister) : tregister;

      begin
         reg32toreg8:=tregister(byte(reg)+byte(R_DI));
      end;

    function reg8toreg32(reg : tregister) : tregister;

      begin
         reg8toreg32:=tregister(byte(reg)-byte(R_DI));
      end;

    function regtoreg8(reg : tregister) : tregister;

     begin
        regtoreg8:=reg32toreg8(reg);
     end;

    function regtoreg16(reg : tregister) : tregister;

     begin
        regtoreg16:=reg32toreg16(reg);
     end;

    function regtoreg32(reg : tregister) : tregister;

     begin
        regtoreg32:=reg;
     end;

    function regtoreg64(reg : tregister) : tregister;

     begin
        { to avoid warning }
        regtoreg64:=R_NO;
     end;

function regsize(reg : tregister) : topsize;
begin
   if reg in regset8bit then
     regsize:=S_B
   else if reg in regset16bit then
     regsize:=S_W
   else if reg in regset32bit then
     regsize:=S_L;
end;


procedure reset_reference(var ref : treference);
begin
  FillChar(ref,sizeof(treference),0);
end;


function new_reference(base : tregister;offset : longint) : preference;
var
  r : preference;
begin
  new(r);
  FillChar(r^,sizeof(treference),0);
  r^.base:=base;
  r^.offset:=offset;
  new_reference:=r;
end;

    procedure clear_location(var loc : tlocation);

      begin
        loc.loc:=LOC_INVALID;
      end;

    {This is needed if you want to be able to delete the string with the nodes !!}
    procedure set_location(var destloc,sourceloc : tlocation);

      begin
        destloc:= sourceloc;
      end;

    procedure swap_location(var destloc,sourceloc : tlocation);

      var
         swapl : tlocation;

      begin
         swapl := destloc;
         destloc := sourceloc;
         sourceloc := swapl;
      end;


{*****************************************************************************
                              Instruction table
*****************************************************************************}

procedure DoneCpu;
begin
  {exitproc:=saveexit; }
{$ifndef NOAG386BIN}
  if assigned(instabcache) then
    dispose(instabcache);
{$endif NOAG386BIN}
end;


procedure BuildInsTabCache;
{$ifndef NOAG386BIN}
var
  i : longint;
{$endif}
begin
{$ifndef NOAG386BIN}
  new(instabcache);
  FillChar(instabcache^,sizeof(tinstabcache),$ff);
  i:=0;
  while (i<InsTabEntries) do
   begin
     if InsTabCache^[InsTab[i].OPcode]=-1 then
      InsTabCache^[InsTab[i].OPcode]:=i;
     inc(i);
   end;
{$endif NOAG386BIN}
end;

    procedure inverse_flags(var f: TResFlags);
      const
         flagsinvers : array[F_E..F_BE] of tresflags =
            (F_NE,F_E,F_LE,F_GE,F_L,F_G,F_NC,F_C,
             F_BE,F_B,F_AE,F_A);
      begin
        f := flagsinvers[f];
      end;

    function flags_to_cond(const f: TResFlags) : TAsmCond;
      const
        flags_2_cond : array[TResFlags] of TAsmCond =
          (C_E,C_NE,C_G,C_L,C_GE,C_LE,C_C,C_NC,C_A,C_AE,C_B,C_BE);
      begin
        result := flags_2_cond[f];
      end;

procedure InitCpu;
begin
{$ifndef NOAG386BIN}
  if not assigned(instabcache) then
    BuildInsTabCache;
{$endif NOAG386BIN}
end;

end.
{
  $Log$
  Revision 1.10  2002-03-04 19:10:12  peter
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
