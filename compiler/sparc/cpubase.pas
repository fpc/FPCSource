{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Contains the base types for the SPARC

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
{ This Unit contains the base types for the PowerPC
}
unit cpubase;

{$i fpcdefs.inc}

interface

uses
  strings,cutils,cclasses,aasmbase,cpuinfo,cginfo;


{*****************************************************************************
                                Assembler Opcodes
*****************************************************************************}

    type
{$WARNING CPU32 opcodes do not fully include the Ultra SPRAC instruction set.}
      { don't change the order of these opcodes! }
      TAsmOp=({$INCLUDE opcode.inc});

      {# This should define the array of instructions as string }
      op2strtable=array[tasmop] of string[11];

    Const
      {# First value of opcode enumeration }
      firstop = low(tasmop);
      {# Last value of opcode enumeration  }
      lastop  = high(tasmop);

      std_op2str:op2strtable=({$INCLUDE strinst.inc});

{*****************************************************************************
                                  Registers
*****************************************************************************}

    type
      TCpuRegister=(
        R_NO
        {General purpose global registers}
        ,R_G0{This register is usually set to zero and used as a scratch register}
        ,R_G1,R_G2,R_G3,R_G4,R_G5,R_G6,R_G7
        {General purpose out registers}
        ,R_O0,R_O1,R_O2,R_O3,R_O4,R_O5,R_O6
        ,R_O7{This register is used to save the address of the last CALL instruction}
        {General purpose local registers}
        ,R_L0
        ,R_L1{This register is used to save the Program Counter (PC) after a Trap}
        ,R_L2{This register is used to save the Program Counter (nPC) after a Trap}
        ,R_L3,R_L4,R_L5,R_L6,R_L7
        {General purpose in registers}
        ,R_I0,R_I1,R_I2,R_I3,R_I4,R_I5,R_I6,R_I7
        {Floating point registers}
        ,R_F0,R_F1,R_F2,R_F3,R_F4,R_F5,R_F6,R_F7
        ,R_F8,R_F9,R_F10,R_F11,R_F12,R_F13,R_F14,R_F15
        ,R_F16,R_F17,R_F18,R_F19,R_F20,R_F21,R_F22,R_F23
        ,R_F24,R_F25,R_F26,R_F27,R_F28,R_F29,R_F30,R_F31
        {Floating point status/"front of queue" registers}
        ,R_FSR,R_FQ
        {Coprocessor registers}
        ,R_C0,R_C1,R_C2,R_C3,R_C4,R_C5,R_C6,R_C7
        ,R_C8,R_C9,R_C10,R_C11,R_C12,R_C13,R_C14,R_C15
        ,R_C16,R_C17,R_C18,R_C19,R_C20,R_C21,R_C22,R_C23
        ,R_C24,R_C25,R_C26,R_C27,R_C28,R_C29,R_C30,R_C31
        {Coprocessor status/queue registers}
        ,R_CSR
        ,R_CQ
        {Integer Unit control & status registers}
        ,R_PSR{Processor Status Register : informs upon the program status}
        ,R_TBR{Trap Base Register : saves the Trap vactor base address}
        ,R_WIM{Window Invalid Mask : }
        ,R_Y{Multiply/Devide Register : }
        {Ancillary State Registers : these are implementation dependent registers and
        thus, are not specified by the SPARC Reference Manual. I did choose the SUN's
        implementation according to the Assembler Refernce Manual.(MN)}
        ,R_ASR0,R_ASR1,R_ASR2,R_ASR3,R_ASR4,R_ASR5,R_ASR6,R_ASR7
        ,R_ASR8,R_ASR9,R_ASR10,R_ASR11,R_ASR12,R_ASR13,R_ASR14,R_ASR15
        ,R_ASR16,R_ASR17,R_ASR18,R_ASR19,R_ASR20,R_ASR21,R_ASR22,R_ASR23
        ,R_ASR24,R_ASR25,R_ASR26,R_ASR27,R_ASR28,R_ASR29,R_ASR30,R_ASR31
        {The following registers are just used with the new register allocator}
        ,R_INTREGISTER,R_FLOATREGISTER,R_MMXREGISTER,R_KNIREGISTER
      );

      TOldRegister=TCpuRegister;

      Tnewregister=word;
      Tsuperregister=byte;
      Tsubregister=byte;

      Tregister=record
        enum:TCpuRegister;
        number:Tnewregister;
      end;

      {# Set type definition for registers }
      tregisterset = set of TCpuRegister;
      Tsupregset=set of Tsuperregister;

      { A type to store register locations for 64 Bit values. }
      tregister64 = packed record
        reglo,reghi : tregister;
      end;

      { alias for compact code }
      treg64 = tregister64;


    Const
      {# First register in the tregister enumeration }
      firstreg = low(TCpuRegister);
      {# Last register in the tregister enumeration }
      lastreg  = R_ASR31;
    type
      {# Type definition for the array of string of register nnames }
      treg2strtable = array[firstreg..lastreg] of string[7];

    const
      std_reg2str:treg2strtable=(
        '',
          {general purpose global registers}
        '%g0','%g1','%g2','%g3','%g4','%g5','%g6','%g7',
          {general purpose out registers}
        '%o0','%o1','%o2','%o3','%o4','%o5','%o6','%o7',
          {general purpose local registers}
        '%l0','%l1','%l2','%l3','%l4','%l5','%l6','%l7',
          {general purpose in registers}
        '%i0','%i1','%i2','%i3','%i4','%i5','%i6','%i7',
          {floating point registers}
        '%f0','%f1','%f2','%f3','%f4','%f5','%f6','%f7',
        '%f8','%f9','%f10','%f11','%f12','%f13','%f14','%f15',
        '%f16','%f17','%f18','%f19','%f20','%f21','%f22','%f23',
        '%f24','%f25','%f26','%f27','%f28','%f29','%f30','%f31',
          {floating point status/"front of queue" registers}
        '%fSR','%fQ',
          {coprocessor registers}
        '%c0','%c1','%c2','%c3','%c4','%c5','%c6','%c7',
        '%c8','%c9','%c10','%c11','%c12','%c13','%c14','%c15',
        '%c16','%c17','%c18','%c19','%c20','%c21','%c22','%c23',
        '%c24','%c25','%c26','%c27','%c28','%c29','%c30','%c31',
          {coprocessor status/queue registers}
        '%csr','%cq',
          {"Program status"/"Trap vactor base address register"/"Window invalid mask"/Y registers}
        '%psr','%tbr','%wim','%y',
          {Ancillary state registers}
        '%asr0','%asr1','%asr2','%asr3','%asr4','%asr5','%asr6','%asr7',
        '%asr8','%asr9','%asr10','%asr11','%asr12','%asr13','%asr14','%asr15',
        '%asr16','%asr17','%asr18','%asr19','%asr20','%asr21','%asr22','%asr23',
        '%asr24','%asr25','%asr26','%asr27','%asr28','%asr29','%asr30','%asr31'
      );

    {New register coding:}

    {Special registers:}
    const
      NR_NO=$0000;  {Invalid register}

    {Normal registers:}

    {General purpose registers:}
      NR_G0=$0100;
      NR_G1=$0200;
      NR_G2=$0300;
      NR_G3=$0400;
      NR_G4=$0500;
      NR_G5=$0600;
      NR_G6=$0700;
      NR_G7=$0800;
      NR_O0=$0900;
      NR_O1=$0a00;
      NR_O2=$0b00;
      NR_O3=$0c00;
      NR_O4=$0d00;
      NR_O5=$0e00;
      NR_O6=$0f00;
      NR_O7=$1000;
      NR_L0=$1100;
      NR_L1=$1200;
      NR_L2=$1300;
      NR_L3=$1400;
      NR_L4=$1500;
      NR_L5=$1600;
      NR_L6=$1700;
      NR_L7=$1800;
      NR_I0=$1900;
      NR_I1=$1A00;
      NR_I2=$1B00;
      NR_I3=$1C00;
      NR_I4=$1D00;
      NR_I5=$1E00;
      NR_I6=$1F00;
      NR_I7=$2000;
{$ifdef dummy}
    { Floating point }
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
    { Coprocessor point }
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
    { ASR }
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
    { Floating point status/"front of queue" registers }
      NR_FSR=$5000;
      NR_FQ=$5000;
      NR_CSR=$5000;
      NR_CQ=$5000;
      NR_PSR=$5000;
      NR_TBR=$5000;
      NR_WIM=$5000;
      NR_Y=$5000;
{$endif dummy}

    {Super registers:}
      RS_NO=$00;
      RS_G0=$01;
      RS_G1=$02;
      RS_G2=$03;
      RS_G3=$04;
      RS_G4=$05;
      RS_G5=$06;
      RS_G6=$07;
      RS_G7=$08;
      RS_O0=$09;
      RS_O1=$0a;
      RS_O2=$0b;
      RS_O3=$0c;
      RS_O4=$0d;
      RS_O5=$0e;
      RS_O6=$0f;
      RS_O7=$10;
      RS_L0=$11;
      RS_L1=$12;
      RS_L2=$13;
      RS_L3=$14;
      RS_L4=$15;
      RS_L5=$16;
      RS_L6=$17;
      RS_L7=$18;
      RS_I0=$19;
      RS_I1=$1a;
      RS_I2=$1b;
      RS_I3=$1c;
      RS_I4=$1d;
      RS_I5=$1e;
      RS_I6=$1f;
      RS_I7=$20;

      first_supreg = $01;
      last_supreg = $20;

      first_imreg = $21;
      last_imreg = $ff;

    { Subregisters, situation unknown!! }
      R_SUBWHOLE=$00;
      R_SUBL=$00;

{Conversion between TCpuRegister and NewRegisters}
  RegEnum2Number:array[TCpuRegister]of cardinal=(
    NR_NO,
    NR_G0,
    NR_G1,
    NR_G2,
    NR_G3,
    NR_G4,
    NR_G5,
    NR_G6,
    NR_G7,
    NR_O0,
    NR_O1,
    NR_O2,
    NR_O3,
    NR_O4,
    NR_O5,
    NR_O6,
    NR_O7,
    NR_L0,
    NR_L1,
    NR_L2,
    NR_L3,
    NR_L4,
    NR_L5,
    NR_L6,
    NR_L7,
    NR_I0,
    NR_I1,
    NR_I2,
    NR_I3,
    NR_I4,
    NR_I5,
    NR_I6,
    NR_I7,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO,
    NR_NO
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

    const
      CondAsmOps=3;
      CondAsmOp:array[0..CondAsmOps-1] of TAsmOp=(
        A_FCMPd, A_JMPL, A_FCMPs
      );
      CondAsmOpStr:array[0..CondAsmOps-1] of string[7]=(
        'FCMPd','JMPL','FCMPs'
      );

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
      TRefOptions=(ref_none,ref_parafixup,ref_localfixup,ref_selffixup);

      { since we have no full 32 bit offsets, we need to be able to specify the high
        and low bits of the address of a symbol                                      }
      trefsymaddr = (refs_no,refs_full,refs_hi,refs_lo);

      { reference record }
      preference = ^treference;
      treference = packed record
         { base register, R_NO if none }
         base,
         { index register, R_NO if none }
         index       : tregister;
         { offset, 0 if none }
         offset      : longint;
         { symbol this reference refers to, nil if none }
         symbol      : tasmsymbol;
         { used in conjunction with symbols and offsets: refs_full means }
         { means a full 32bit reference, refs_hi means the upper 16 bits }
         { and refs_lo the lower 16 bits of the address                   }
         symaddr     : trefsymaddr;
         { changed when inlining and possibly in other cases, don't }
         { set manually                                             }
         offsetfixup : longint;
         { used in conjunction with the previous field }
         options     : trefoptions;
         { alignment this reference is guaranteed to have }
         alignment   : byte;
      end;

      { reference record }
      pparareference = ^tparareference;
      tparareference = packed record
         index       : tregister;
         offset      : aword;
      end;

    const
      symaddr2str: array[trefsymaddr] of string[3] = ('','','%hi','%lo');


{*****************************************************************************
                                Operand
*****************************************************************************}

    type
      toptype=(top_none,top_reg,top_ref,top_const,top_symbol);
      toper=record
        ot:LongInt;
        case typ:toptype of
          top_none:();
          top_reg:(reg:tregister);
          top_ref:(ref:preference);
          top_const:(val:aword);
          top_symbol:(sym:tasmsymbol;symofs:LongInt);
      end;

{*****************************************************************************
                                Operand Sizes
*****************************************************************************}

{$ifdef dummy}
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
{$endif dummy}

{*****************************************************************************
                               Generic Location
*****************************************************************************}

    type
      { tparamlocation describes where a parameter for a procedure is stored.
        References are given from the caller's point of view. The usual
        TLocation isn't used, because contains a lot of unnessary fields.
      }
      tparalocation = packed record
         size : TCGSize;
         { The location type where the parameter is passed, usually
           LOC_REFERENCE,LOC_REGISTER or LOC_FPUREGISTER
         }
         loc  : TCGLoc;
         { The stack pointer must be decreased by this value before
           the parameter is copied to the given destination.
           This allows to "encode" pushes with tparalocation.
           On the PowerPC, this field is unsed but it is there
           because several generic code accesses it.
         }
         sp_fixup : longint;
         case TCGLoc of
            LOC_REFERENCE : (reference : tparareference);
            LOC_FPUREGISTER, LOC_CFPUREGISTER, LOC_MMREGISTER, LOC_CMMREGISTER,
              LOC_REGISTER,LOC_CREGISTER : (
              case longint of
                1 : (register,registerhigh : tregister);
                { overlay a registerlow }
                2 : (registerlow : tregister);
                { overlay a 64 Bit register type }
                3 : (reg64 : tregister64);
                4 : (register64 : tregister64);
            );
      end;

      treglocation = packed record
        case longint of
          1 : (register,registerhigh : tregister);
          { overlay a registerlow }
          2 : (registerlow : tregister);
          { overlay a 64 Bit register type }
          3 : (reg64 : tregister64);
          4 : (register64 : tregister64);
       end;


      tlocation = packed record
         size : TCGSize;
         loc : tcgloc;
         case tcgloc of
            LOC_CREFERENCE,LOC_REFERENCE : (reference : treference);
            LOC_CONSTANT : (
              case longint of
{$ifdef FPC_BIG_ENDIAN}
                1 : (_valuedummy,value : AWord);
{$else FPC_BIG_ENDIAN}
                1 : (value : AWord);
{$endif FPC_BIG_ENDIAN}
                { can't do this, this layout depends on the host cpu. Use }
                { lo(valueqword)/hi(valueqword) instead (JM)              }
                { 2 : (valuelow, valuehigh:AWord);                        }
                { overlay a complete 64 Bit value }
                3 : (valueqword : qword);
              );
            LOC_FPUREGISTER, LOC_CFPUREGISTER, LOC_MMREGISTER, LOC_CMMREGISTER,
              LOC_REGISTER,LOC_CREGISTER : (
                case longint of
                  1 : (registerlow,registerhigh : tregister);
                  2 : (register : tregister);
                  { overlay a 64 Bit register type }
                  3 : (reg64 : tregister64);
                  4 : (register64 : tregister64);
                );
            LOC_FLAGS : (resflags : tresflags);
      end;

{*****************************************************************************
                                 Constants
*****************************************************************************}

    const
      max_operands = 3;

      {# Constant defining possibly all registers which might require saving }
      ALL_REGISTERS = [R_G0..R_I7];

      general_registers = [R_G0..R_I7];
      general_superregisters = [RS_O0..RS_I7];

      {# low and high of the available maximum width integer general purpose }
      { registers                                                            }
      LoGPReg = R_G0;
      HiGPReg = R_I7;

      {# low and high of every possible width general purpose register (same as }
      { above on most architctures apart from the 80x86)                        }
      LoReg = R_G0;
      HiReg = R_I7;

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

      maxintregs = 8;
      intregs = [R_G0..R_I7];
      usableregsint = [RS_O0..RS_I7];
      c_countusableregsint = 24;

      maxfpuregs = 8;
      fpuregs=[R_F0..R_F31];
      usableregsfpu=[R_F0..R_F31];
      c_countusableregsfpu=32;

      mmregs     = [];
      usableregsmm  = [];
      c_countusableregsmm  = 0;

      { no distinction on this platform }
      maxaddrregs = 0;
      addrregs    = [];
      usableregsaddr = [];
      c_countusableregsaddr = 0;

      firstsaveintreg = RS_O0;
      lastsaveintreg = RS_I7;
      firstsavefpureg = R_F0;
      lastsavefpureg = R_F31;
      firstsavemmreg = R_NO;
      lastsavemmreg = R_NO;

      maxvarregs = 8;
      varregs : Array [1..maxvarregs] of Tnewregister =
                (RS_L0,RS_L1,RS_L2,RS_L3,RS_L4,RS_L5,RS_L6,RS_L7);

      maxfpuvarregs = 1;
      fpuvarregs : Array [1..maxfpuvarregs] of TCpuRegister =
                (R_F2);

      {
      max_param_regs_int = 6;
      param_regs_int: Array[1..max_param_regs_int] of TCpuRegister =
        (R_3,R_4,R_5,R_6,R_7,R_8,R_9,R_10);

      max_param_regs_fpu = 13;
      param_regs_fpu: Array[1..max_param_regs_fpu] of TCpuRegister =
        (R_F1,R_F2,R_F3,R_F4,R_F5,R_F6,R_F7,R_F8,R_F9,R_F10,R_F11,R_F12,R_F13);

      max_param_regs_mm = 13;
      param_regs_mm: Array[1..max_param_regs_mm] of TCpuRegister =
        (R_M1,R_M2,R_M3,R_M4,R_M5,R_M6,R_M7,R_M8,R_M9,R_M10,R_M11,R_M12,R_M13);
      }

      {# Registers which are defined as scratch and no need to save across
         routine calls or in assembler blocks.
      }
      max_scratch_regs = 3;
      scratch_regs: Array[1..max_scratch_regs] of Tsuperregister = (RS_O7,RS_G2,RS_G3);

{*****************************************************************************
                          Default generic sizes
*****************************************************************************}

      {# Defines the default address size for a processor, }
      OS_ADDR = OS_32;
      {# the natural int size for a processor,             }
      OS_INT = OS_32;
      {# the maximum float size for a processor,           }
      OS_FLOAT = OS_F64;
      {# the size of a vector register for a processor     }
      OS_VECTOR = OS_M64;

{*****************************************************************************
                               GDB Information
*****************************************************************************}

      {# Register indexes for stabs information, when some
         parameters or variables are stored in registers.

         Taken from rs6000.h (DBX_REGISTER_NUMBER)
         from GCC 3.x source code.
      }

      stab_regindex : array[firstreg..lastreg] of shortint =
      (
        0{R_NO}
        {General purpose global registers}
        ,1{R_G0}{This register is usually set to zero and used as a scratch register}
        ,2{R_G1},3{R_G2},4{R_G3},5{R_G4},6{R_G5},7{R_G6},8{R_G7}
        {General purpose out registers}
        ,9{R_O0},10{R_O1},11{R_O2},12{R_O3},13{R_O4},14{R_O5},15{R_O6}
        ,16{R_O7}{This register is used to save the address of the last CALL instruction}
        {General purpose local registers}
        ,16{R_L0}
        ,17{R_L1}{This register is used to save the Program Counter (PC) after a Trap}
        ,18{R_L2}{This register is used to save the Program Counter (nPC) after a Trap}
        ,19{R_L3},20{R_L4},21{R_L5},22{R_L6},23{R_L7}
        {General purpose in registers}
        ,24{R_I0},25{R_I1},26{R_I2},27{R_I3},28{R_I4},29{R_I5},30{R_I6},31{R_I7}
        {Floating point registers}
        ,32{R_F0},33{R_F1},34{R_F2},35{R_F3},36{R_F4},37{R_F5},38{R_F6},39{R_F7}
        ,40{R_F8},41{R_F9},42{R_F10},43{R_F11},44{R_F12},45{R_F13},46{R_F14},47{R_F15}
        ,48{R_F16},49{R_F17},50{R_F18},51{R_F19},52{R_F20},53{R_F21},54{R_F22},55{R_F23}
        ,56{R_F24},57{R_F25},58{R_F26},59{R_F27},60{R_F28},61{R_F29},62{R_F30},63{R_F31}
        {Floating point status/"front of queue" registers}
        ,64{R_FSR},65{R_FQ}
        {Coprocessor registers}
        ,66{R_C0},67{R_C1},68{R_C2},69{R_C3},70{R_C4},71{R_C5},72{R_C6},73{R_C7}
        ,74{R_C8},75{R_C9},76{R_C10},77{R_C11},78{R_C12},79{R_C13},80{R_C14},81{R_C15}
        ,82{R_C16},83{R_C17},84{R_C18},85{R_C19},86{R_C20},87{R_C21},88{R_C22},89{R_C23}
        ,90{R_C24},91{R_C25},92{R_C26},93{R_C27},94{R_C28},95{R_C29},96{R_C30},98{R_C31}
        {Coprocessor status/queue registers}
        ,99{R_CSR}
        ,100{R_CQ}
        {Integer Unit control & status registers}
        ,101{R_PSR}{Processor Status Register : informs upon the program status}
        ,102{R_TBR}{Trap Base Register : saves the Trap vactor base address}
        ,103{R_WIM}{Window Invalid Mask : }
        ,104{R_Y}{Multiply/Devide Register : }
        {Ancillary State Registers : these are implementation dependent registers and
        thus, are not specified by the SPARC Reference Manual. I did choose the SUN's
        implementation according to the Assembler Refernce Manual.(MN)}
        ,105{R_ASR0},106{R_ASR1},107{R_ASR2},108{R_ASR3},109{R_ASR4},110{R_ASR5},111{R_ASR6},112{R_ASR7}
        ,113{R_ASR8},114{R_ASR9},115{R_ASR10},116{R_ASR11},117{R_ASR12},118{R_ASR13},119{R_ASR14},120{R_ASR15}
        ,121{R_ASR16},122{R_ASR17},123{R_ASR18},124{R_ASR19},125{R_ASR20},126{R_ASR21},127{R_ASR22},127{R_ASR23}
        ,127{R_ASR24},127{R_ASR25},127{R_ASR26},127{R_ASR27},127{R_ASR28},127{R_ASR29},127{R_ASR30},127{R_ASR31}
      );


{*****************************************************************************
                          Generic Register names
*****************************************************************************}

      {# Stack pointer register }
      NR_STACK_POINTER_REG = NR_O6;
      RS_STACK_POINTER_REG = RS_O6;
      {# Frame pointer register }
      NR_FRAME_POINTER_REG = NR_I6;
      RS_FRAME_POINTER_REG = RS_I6;
      {# Register for addressing absolute data in a position independant way,
         such as in PIC code. The exact meaning is ABI specific. For
         further information look at GCC source : PIC_OFFSET_TABLE_REGNUM

         Taken from GCC rs6000.h
      }
{$warning As indicated in rs6000.h, but can't find it anywhere else!}
      {PIC_OFFSET_REG = R_30;}
      { the return_result_reg, is used inside the called function to store its return
      value when that is a scalar value otherwise a pointer to the address of the
      result is placed inside it }
      { Results are returned in this register (32-bit values) }
      NR_FUNCTION_RETURN_REG = NR_I0;
      RS_FUNCTION_RETURN_REG = RS_I0;
      { Low part of 64bit return value }
      NR_FUNCTION_RETURN64_LOW_REG = NR_I1;
      RS_FUNCTION_RETURN64_LOW_REG = RS_I1;
      { High part of 64bit return value }
      NR_FUNCTION_RETURN64_HIGH_REG = NR_I0;
      RS_FUNCTION_RETURN64_HIGH_REG = RS_I0;
      { The value returned from a function is available in this register }
      NR_FUNCTION_RESULT_REG = NR_O0;
      RS_FUNCTION_RESULT_REG = RS_O0;
      { The lowh part of 64bit value returned from a function }
      NR_FUNCTION_RESULT64_LOW_REG = NR_O1;
      RS_FUNCTION_RESULT64_LOW_REG = RS_O1;
      { The high part of 64bit value returned from a function }
      NR_FUNCTION_RESULT64_HIGH_REG = NR_O0;
      RS_FUNCTION_RESULT64_HIGH_REG = RS_O0;

      FPU_RESULT_REG = R_F0;
      mmresultreg = R_NO;

{*****************************************************************************
                       GCC /ABI linking information
*****************************************************************************}

      {# Registers which must be saved when calling a routine declared as
         cppdecl, cdecl, stdcall, safecall, palmossyscall. The registers
         saved should be the ones as defined in the target ABI and / or GCC.

         This value can be deduced from CALLED_USED_REGISTERS array in the
         GCC source.
      }
      std_saved_registers = [];

      {# Required parameter alignment when calling a routine declared as
         stdcall and cdecl. The alignment value should be the one defined
         by GCC or the target ABI.

         The value of this constant is equal to the constant
         PARM_BOUNDARY / BITS_PER_UNIT in the GCC source.
      }
      std_param_align = 4;  { for 32-bit version only }

{*****************************************************************************
                            CPU Dependent Constants
*****************************************************************************}

    const
      simm13lo=-4096;
      simm13hi=4095;

{*****************************************************************************
                                  Helpers
*****************************************************************************}

    function  is_calljmp(o:tasmop):boolean;

    procedure inverse_flags(var f: TResFlags);
    function  flags_to_cond(const f: TResFlags) : TAsmCond;
    procedure convert_register_to_enum(var r:Tregister);
    function cgsize2subreg(s:Tcgsize):Tsubregister;

implementation

    uses
      verbose;

{*****************************************************************************
                                  Helpers
*****************************************************************************}

    function is_calljmp(o:tasmop):boolean;
      const
        CallJmpOp=[A_JMPL..A_CBccc];
      begin
        is_calljmp:=(o in CallJmpOp);
      end;


    procedure inverse_flags(var f: TResFlags);
      const
        inv_flags: array[TResFlags] of TResFlags =
          (F_NE,F_E,F_LE,F_GE,F_L,F_G,F_NC,F_C,F_BE,F_B,F_AE,F_A);
      begin
        f:=inv_flags[f];
      end;


   function flags_to_cond(const f:TResFlags):TAsmCond;
      const
        flags_2_cond:array[TResFlags] of TAsmCond=
          (C_E,C_NE,C_G,C_L,C_GE,C_LE,C_C,C_NC,C_A,C_AE,C_B,C_BE);
      begin
        result:=flags_2_cond[f];
      end;


    procedure convert_register_to_enum(var r:Tregister);
      begin
        if (r.enum=R_INTREGISTER) then
          begin
            if r.number>NR_I7 then
              internalerror(200301082);
            r.enum:=TCpuRegister(r.number shr 8);
          end;
      end;


    function cgsize2subreg(s:Tcgsize):Tsubregister;
      begin
        cgsize2subreg:=R_SUBWHOLE;
      end;

end.
{
  $Log$
  Revision 1.41  2003-06-12 19:11:34  jonas
    - removed ALL_INTREGISTERS (only the one in rgobj is valid)

  Revision 1.40  2003/06/04 21:00:54  mazen
  - making TOldRegister only declared for compatibility and
    no more used in cpubase

  Revision 1.39  2003/06/01 21:38:06  peter
    * getregisterfpu size parameter added
    * op_const_reg size parameter added
    * sparc updates

  Revision 1.38  2003/06/01 01:04:35  peter
    * reference fixes

  Revision 1.37  2003/05/31 15:05:28  peter
    * FUNCTION_RESULT64_LOW/HIGH_REG added for int64 results

  Revision 1.36  2003/05/31 01:00:51  peter
    * register fixes

  Revision 1.35  2003/05/30 23:57:08  peter
    * more sparc cleanup
    * accumulator removed, splitted in function_return_reg (called) and
      function_result_reg (caller)

}
