{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl and Peter Vreman

    Contains the base types for the ARM

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

{$i fpcdefs.inc}

interface

uses
  cutils,cclasses,
  globals,
  cpuinfo,
  aasmbase,
  cginfo;


{*****************************************************************************
                                Assembler Opcodes
*****************************************************************************}

    type
      TAsmOp=(A_ADC,A_ADD,A_AND,A_N,A_BIC,A_BKPT,A_BL,A_BLX,A_BX,
              A_CDP,A_CDP2,A_CLZ,A_CMN,A_CMP,A_EOR,A_LDC,_A_LDC2,
              A_LDM,A_LDR,A_LDRB,A_LDRD,A_LDRBT,A_LDRH,A_LDRSB,
              A_LDRSH,A_LDRT,A_MCR,A_MCR2,A_MCRR,A_MLA,A_MOV,
              A_MRC,A_MRC2,A_MRRC,A_RS,A_MSR,A_MUL,A_MVN,
              A_ORR,A_PLD,A_QADD,A_QDADD,A_QDSUB,A_QSUB,A_RSB,A_RSC,
              A_SBC,A_SMLAL.A_SMLA,A_SMLAL,A_SMLAW,A_SMULL,A_SMUL,
              A_SMULW,A_STC,A_STC2,A_STM,A_STR,A_STRB,A_STRBT,A_STRD,
              A_STRH,A_STRT,A_SUB,A_SWI,A_SWP,A_SWPB,A_TEQ,A_TST.
              A_UMLAL,A_UMULL);

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

      { A type to store register locations for 64 Bit values. }
      tregister64 = packed record
        reglo,reghi : tregister;
      end;

      { alias for compact code }
      treg64 = tregister64;

      {# Set type definition for registers }
      tregisterset = set of tregister;

      {# Type definition for the array of string of register names }
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
                                   Flags
*****************************************************************************}

    type
      TResFlags = (F_E,F_NE,F_G,F_L,F_GE,F_LE,F_C,F_NC,F_A,F_AE,F_B,F_BE);

{*****************************************************************************
                                Reference
*****************************************************************************}

    type
      trefoptions=(ref_none,ref_parafixup,ref_localfixup,ref_selffixup);

      { reference record }
      preference = ^treference;
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

      { reference record }
      pparareference = ^tparareference;
      tparareference = packed record
         index       : tregister;
         offset      : longint;
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
         top_const  : (val:aword);
         top_symbol : (sym:tasmsymbol;symofs:longint);
      end;

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

      { tparamlocation describes where a parameter for a procedure is stored.
        References are given from the caller's point of view. The usual
        TLocation isn't used, because contains a lot of unnessary fields.
      }
      tparalocation = packed record
         size : TCGSize;
         loc  : TLoc;
         sp_fixup : longint;
         case TLoc of
            LOC_REFERENCE : (reference : tparareference);
            { segment in reference at the same place as in loc_register }
            LOC_REGISTER,LOC_CREGISTER : (
              case longint of
                1 : (register,registerhigh : tregister);
                { overlay a registerlow }
                2 : (registerlow : tregister);
                { overlay a 64 Bit register type }
                3 : (reg64 : tregister64);
                4 : (register64 : tregister64);
              );
            { it's only for better handling }
            LOC_MMXREGISTER,LOC_CMMXREGISTER : (mmxreg : tregister);
      end;

      tlocation = packed record
         loc  : TLoc;
         size : TCGSize;
         case TLoc of
            LOC_FLAGS : (resflags : tresflags);
            LOC_CONSTANT : (
              case longint of
                1 : (value : AWord);
                { can't do this, this layout depends on the host cpu. Use }
                { lo(valueqword)/hi(valueqword) instead (JM)              }
                { 2 : (valuelow, valuehigh:AWord);                        }
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
      { declare aliases }
      LOC_MMREGISTER = LOC_SSEREGISTER;
      LOC_CMMREGISTER = LOC_CSSEREGISTER;

      max_operands = 3;

      lvaluelocations = [LOC_REFERENCE,LOC_CFPUREGISTER,
        LOC_CREGISTER,LOC_MMXREGISTER,LOC_CMMXREGISTER];

      {# Constant defining possibly all registers which might require saving }
      ALL_REGISTERS = [firstreg..lastreg];

      general_registers = [R_EAX,R_EBX,R_ECX,R_EDX];

      {# low and high of the available maximum width integer general purpose }
      { registers                                                            }
      LoGPReg = R_EAX;
      HiGPReg = R_EDX;

      {# low and high of every possible width general purpose register (same as }
      { above on most architctures apart from the 80x86)                        }
      LoReg = R_EAX;
      HiReg = R_DH;

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

      maxintregs = 4;
      intregs = [R_EAX..R_BL];
      usableregsint = [R_EAX,R_EBX,R_ECX,R_EDX];
      c_countusableregsint = 4;

      maxfpuregs = 8;
      fpuregs = [R_ST0..R_ST7];
      usableregsfpu = [];
      c_countusableregsfpu = 0;

      mmregs = [R_MM0..R_MM7];
      usableregsmm = [R_MM0..R_MM7];
      c_countusableregsmm  = 8;

      firstsaveintreg = R_EAX;
      lastsaveintreg  = R_EBX;
      firstsavefpureg = R_NO;
      lastsavefpureg  = R_NO;
      firstsavemmreg  = R_MM0;
      lastsavemmreg   = R_MM7;

      maxvarregs = 4;
      varregs : array[1..maxvarregs] of tregister =
         (R_EBX,R_EDX,R_ECX,R_EAX);

      maxfpuvarregs = 8;

      {# Registers which are defined as scratch and no need to save across
         routine calls or in assembler blocks.
      }
      max_scratch_regs = 1;
      scratch_regs : array[1..max_scratch_regs] of tregister = (R_EDI);


{*****************************************************************************
                               GDB Information
*****************************************************************************}

      {# Register indexes for stabs information, when some
         parameters or variables are stored in registers.

         Taken from i386.c (dbx_register_map) and i386.h
          (FIXED_REGISTERS) from GCC 3.x source code

      }
          stab_regindex : array[tregister] of shortint =
          (-1,
          0,1,2,3,4,5,6,7,
          0,1,2,3,4,5,6,7,
          0,1,2,3,0,1,2,3,
          -1,-1,-1,-1,-1,-1,
          12,12,13,14,15,16,17,18,19,
          -1,-1,-1,-1,-1,-1,
          -1,-1,-1,-1,
          -1,-1,-1,-1,-1,
          29,30,31,32,33,34,35,36,
          21,22,23,24,25,26,27,28
        );

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
         such as in PIC code. The exact meaning is ABI specific. For
         further information look at GCC source : PIC_OFFSET_TABLE_REGNUM
      }
      pic_offset_reg = R_EBX;
      {# Results are returned in this register (32-bit values) }
      accumulator   = R_EAX;
      {# Hi-Results are returned in this register (64-bit value high register) }
      accumulatorhigh = R_EDX;
      { WARNING: don't change to R_ST0!! See comments above implementation of }
      { a_loadfpu* methods in rgcpu (JM)                                      }
      fpu_result_reg = R_ST;
      mmresultreg = R_MM0;

{*****************************************************************************
                       GCC /ABI linking information
*****************************************************************************}

    const
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
                            CPU Dependent Constants
*****************************************************************************}

{*****************************************************************************
                                  Helpers
*****************************************************************************}

    function is_calljmp(o:tasmop):boolean;

    function flags_to_cond(const f: TResFlags) : TAsmCond;


implementation

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
  Revision 1.1  2003-07-21 16:35:30  florian
    * very basic stuff for the arm
}
