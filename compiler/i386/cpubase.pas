{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl and Peter Vreman

    Contains the base types for the i386

    * This code was inspired by the NASM sources
      The Netwide Assembler is Copyright (c) 1996 Simon Tatham and
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

{$i fpcdefs.inc}

interface

uses
  cutils,cclasses,
  globals,
  cpuinfo,
  aasmbase,
  cginfo
{$ifdef delphi}
  ,dmisc
{$endif}
  ;


{*****************************************************************************
                                Assembler Opcodes
*****************************************************************************}

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
        S_FS,S_FL,S_FX,S_D,S_Q,S_FV,S_FXX,
        S_NEAR,S_FAR,S_SHORT
      );

{*****************************************************************************
                                  Registers
*****************************************************************************}

      {The new register coding:

       For now we'll use this, when the old register coding is away, we
       can change this into a cardinal or something so the amount of
       possible registers increases.

       High byte: Register number
       Low byte:  Subregister

       Example:

       $0100      AL
       $0101      AH
       $0102      AX
       $0103      EAX
       $0104      RAX
       $0201      BL
       $0203      EBX}

      {Super register numbers:}
      const       RS_SPECIAL    = $00;      {Special register}
                  RS_EAX        = $01;      {EAX}
                  RS_EBX        = $02;      {EBX}
                  RS_ECX        = $03;      {ECX}
                  RS_EDX        = $04;      {EDX}
                  RS_ESI        = $05;      {ESI}
                  RS_EDI        = $06;      {EDI}
                  RS_EBP        = $07;      {EBP}
                  RS_ESP        = $08;      {ESP}
                  RS_R8         = $09;      {R8}
                  RS_R9         = $0a;      {R9}
                  RS_R10        = $0b;      {R10}
                  RS_R11        = $0c;      {R11}
                  RS_R12        = $0d;      {R12}
                  RS_R13        = $0e;      {R13}
                  RS_R14        = $0f;      {R14}
                  RS_R15        = $10;      {R15}


                  {Number of first and last superregister.}
                  first_supreg    = $01;
                  last_supreg     = $10;
                  
     {Sub register numbers:}
                  R_SUBL        = $00;      {Like AL}
                  R_SUBH        = $01;      {Like AH}
                  R_SUBW        = $02;      {Like AX}
                  R_SUBD        = $03;      {Like EAX}
                  R_SUBQ        = $04;      {Like RAX}
                  
     {The subregister that specifies the entire register.}
                  R_SUBWHOLE    = R_SUBD;  {i386}
                  {R_SUBWHOLE    = R_SUBQ;} {Hammer}

     {Special registers:}
     const        NR_NO    = $0000;      {Invalid register}
                  NR_CS    = $0001;      {CS}
                  NR_DS    = $0002;      {DS}
                  NR_ES    = $0003;      {ES}
                  NR_SS    = $0004;      {SS}
                  NR_FS    = $0005;      {FS}
                  NR_GS    = $0006;      {GS}
                  NR_RIP   = $000F;      {RIP}
                  NR_DR0   = $0010;      {DR0}
                  NR_DR1   = $0011;      {DR1}
                  NR_DR2   = $0012;      {DR2}
                  NR_DR3   = $0013;      {DR3}
                  NR_DR6   = $0016;      {DR6}
                  NR_DR7   = $0017;      {DR7}
                  NR_CR0   = $0020;      {CR0}
                  NR_CR2   = $0021;      {CR1}
                  NR_CR3   = $0022;      {CR2}
                  NR_CR4   = $0023;      {CR3}
                  NR_TR3   = $0030;      {R_TR3}
                  NR_TR4   = $0031;      {R_TR4}
                  NR_TR5   = $0032;      {R_TR5}
                  NR_TR6   = $0033;      {R_TR6}
                  NR_TR7   = $0034;      {R_TR7}

      {Normal registers.}
      const       NR_AL    = $0100;      {AL}
                  NR_AH    = $0101;      {AH}
                  NR_AX    = $0102;      {AX}
                  NR_EAX   = $0103;      {EAX}
                  NR_RAX   = $0104;      {RAX}
                  NR_BL    = $0200;      {BL}
                  NR_BH    = $0201;      {BH}
                  NR_BX    = $0202;      {BX}
                  NR_EBX   = $0203;      {EBX}
                  NR_RBX   = $0204;      {RBX}
                  NR_CL    = $0300;      {CL}
                  NR_CH    = $0301;      {CH}
                  NR_CX    = $0302;      {CX}
                  NR_ECX   = $0303;      {ECX}
                  NR_RCX   = $0304;      {RCX}
                  NR_DL    = $0400;      {DL}
                  NR_DH    = $0401;      {DH}
                  NR_DX    = $0402;      {DX}
                  NR_EDX   = $0403;      {EDX}
                  NR_RDX   = $0404;      {RDX}
                  NR_SIL   = $0500;      {SIL}
                  NR_SI    = $0502;      {SI}
                  NR_ESI   = $0503;      {ESI}
                  NR_RSI   = $0504;      {RSI}
                  NR_DIL   = $0600;      {DIL}
                  NR_DI    = $0602;      {DI}
                  NR_EDI   = $0603;      {EDI}
                  NR_RDI   = $0604;      {RDI}
                  NR_BPL   = $0700;      {BPL}
                  NR_BP    = $0702;      {BP}
                  NR_EBP   = $0703;      {EBP}
                  NR_RBP   = $0704;      {RBP}
                  NR_SPL   = $0800;      {SPL}
                  NR_SP    = $0802;      {SP}
                  NR_ESP   = $0803;      {ESP}
                  NR_RSP   = $0804;      {RSP}
                  NR_R8L   = $0900;      {R8L}
                  NR_R8W   = $0902;      {R8W}
                  NR_R8D   = $0903;      {R8D}
                  NR_R9L   = $0a00;      {R9D}
                  NR_R9W   = $0a02;      {R9W}
                  NR_R9D   = $0a03;      {R9D}
                  NR_R10L  = $0b00;      {R10L}
                  NR_R10W  = $0b02;      {R10W}
                  NR_R10D  = $0b03;      {R10D}
                  NR_R11L  = $0c00;      {R11L}
                  NR_R11W  = $0c02;      {R11W}
                  NR_R11D  = $0c03;      {R11D}
                  NR_R12L  = $0d00;      {R12L}
                  NR_R12W  = $0d02;      {R12W}
                  NR_R12D  = $0d03;      {R12D}
                  NR_R13L  = $0e00;      {R13L}
                  NR_R13W  = $0e02;      {R13W}
                  NR_R13D  = $0e03;      {R13D}
                  NR_R14L  = $0f00;      {R14L}
                  NR_R14W  = $0f02;      {R14W}
                  NR_R14D  = $0f03;      {R14D}
                  NR_R15L  = $1000;      {R15L}
                  NR_R15W  = $1002;      {R15W}
                  NR_R15D  = $1003;      {R15D}

    type
      {# Enumeration for all possible registers for cpu. It
        is to note that all registers of the same type
        (for example all FPU registers), should be grouped
        together.
      }
      { don't change the order }
      { it's used by the register size conversions        }
      {$packenum 1}
      Toldregister = (R_NO,
        R_EAX,R_ECX,R_EDX,R_EBX,R_ESP,R_EBP,R_ESI,R_EDI,
        R_AX,R_CX,R_DX,R_BX,R_SP,R_BP,R_SI,R_DI,
        R_AL,R_CL,R_DL,R_BL,R_AH,R_CH,R_BH,R_DH,
        R_CS,R_DS,R_ES,R_SS,R_FS,R_GS,
        R_ST,R_ST0,R_ST1,R_ST2,R_ST3,R_ST4,R_ST5,R_ST6,R_ST7,
        R_DR0,R_DR1,R_DR2,R_DR3,R_DR6,R_DR7,
        R_CR0,R_CR2,R_CR3,R_CR4,
        R_TR3,R_TR4,R_TR5,R_TR6,R_TR7,
        R_MM0,R_MM1,R_MM2,R_MM3,R_MM4,R_MM5,R_MM6,R_MM7,
        R_XMM0,R_XMM1,R_XMM2,R_XMM3,R_XMM4,R_XMM5,R_XMM6,R_XMM7,
        R_INTREGISTER,R_FLOATREGISTER,R_MMXREGISTER,R_KNIREGISTER
      );
      
      type  Tnewregister=word;

            Tregister = packed record
              enum:Toldregister;
              number:Tnewregister;  {This is a word for now, change to cardinal
                                     when the old register coding is away.}
            end;
            
            Tsuperregister=byte;
            Tsubregister=byte;
      {$packenum normal}

      { A type to store register locations for 64 Bit values. }
      tregister64 = packed record
        reglo,reghi : tregister;
      end;

      { alias for compact code }
      treg64 = tregister64;

      {# Set type definition for registers }
      tregisterset = set of Toldregister;
      Tsupregset = set of Tsuperregister;


    const
      {# First register in the tregister enumeration }
      firstreg = low(Toldregister);
      {# Last register in the tregister enumeration }
      lastreg  = R_XMM7;

      firstsreg = R_CS;
      lastsreg  = R_GS;

      nfirstsreg = NR_CS;
      nlastsreg  = NR_GS;

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
      
      {Converts subregister number to opsize}
      subreg2opsize:array[0..4] of Topsize = (S_B,S_B,S_W,S_L,S_D);
      {Converts subregister number to cgsize}
{      subreg2cgsize:array[0..4] of Tcgsize = (OS_8,OS_8,OS_16,OS_32);}

      {# Standard opcode string table (for each tasmop enumeration). The
         opcode strings should conform to the names as defined by the
         processor manufacturer.
      }
      std_op2str:op2strtable={$i i386int.inc}

    type
      {# Type definition for the array of string of register names }
         reg2strtable = array[firstreg..lastreg] of string[6];
         regname2regnumrec = record
           name:string[6];
           number:Tnewregister;
         end;

    const
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
      ALL_INTREGISTERS = [1..255];

      general_registers = [R_EAX,R_EBX,R_ECX,R_EDX];
      general_superregisters = [RS_EAX,RS_EBX,RS_ECX,RS_EDX];

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
      usableregsint = [RS_EAX,RS_EBX,RS_ECX,RS_EDX];
      c_countusableregsint = 4;

      maxfpuregs = 8;
      fpuregs = [R_ST0..R_ST7];
      usableregsfpu = [];
      c_countusableregsfpu = 0;

      mmregs = [R_MM0..R_MM7];
      usableregsmm = [R_MM0..R_MM7];
      c_countusableregsmm  = 8;
      
      maxaddrregs = 0;
      addrregs    = [];
      usableregsaddr = [];
      c_countusableregsaddr = 0;
      

      firstsaveintreg = RS_EAX;
      lastsaveintreg  = RS_EDX;
      firstsavefpureg = R_NO;
      lastsavefpureg  = R_NO;
      firstsavemmreg  = R_MM0;
      lastsavemmreg   = R_MM7;

      maxvarregs = 4;
      varregs : array[1..maxvarregs] of Toldregister =
         (R_EBX,R_EDX,R_ECX,R_EAX);

      maxfpuvarregs = 8;

      {# Registers which are defined as scratch and no need to save across
         routine calls or in assembler blocks.
      }
      max_scratch_regs = 1;
      scratch_regs : array[1..max_scratch_regs] of Tsuperregister = (RS_EDI);


{*****************************************************************************
                               GDB Information
*****************************************************************************}

      {# Register indexes for stabs information, when some
         parameters or variables are stored in registers.

         Taken from i386.c (dbx_register_map) and i386.h
          (FIXED_REGISTERS) from GCC 3.x source code

      }
          stab_regindex : array[firstreg..lastreg] of shortint =
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
      NR_STACK_POINTER_REG = NR_ESP;
      {# Frame pointer register }
      frame_pointer_reg = R_EBP;
      NR_FRAME_POINTER_REG = NR_EBP;
      {# Self pointer register : contains the instance address of an
         object or class. }
      self_pointer_reg  = R_ESI;
      RS_SELF_POINTER_REG  = RS_ESI;
      NR_SELF_POINTER_REG  = NR_ESI;
      {# Register for addressing absolute data in a position independant way,
         such as in PIC code. The exact meaning is ABI specific. For
         further information look at GCC source : PIC_OFFSET_TABLE_REGNUM
      }
      pic_offset_reg = R_EBX;
      {# Results are returned in this register (32-bit values) }
      accumulator   = R_EAX;
      RS_ACCUMULATOR   = RS_EAX;
      NR_ACCUMULATOR   = NR_EAX;
      {the return_result_reg, is used inside the called function to store its return
      value when that is a scalar value otherwise a pointer to the address of the
      result is placed inside it}
    	return_result_reg		=	accumulator;
      RS_RETURN_RESULT_REG = RS_ACCUMULATOR;
      NR_RETURN_RESULT_REG = NR_ACCUMULATOR;

      {the function_result_reg contains the function result after a call to a scalar
      function othewise it contains a pointer to the returned result}
    	function_result_reg	=	accumulator;
      {# Hi-Results are returned in this register (64-bit value high register) }
      accumulatorhigh = R_EDX;
      RS_ACCUMULATORHIGH  = RS_EDX;
      NR_ACCUMULATORHIGH  = NR_EDX;
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

    procedure convert_register_to_enum(var r:Tregister);
    function cgsize2subreg(s:Tcgsize):Tsubregister;
    function is_calljmp(o:tasmop):boolean;
    function flags_to_cond(const f: TResFlags) : TAsmCond;


implementation

    uses  verbose;

{*****************************************************************************
                                  Helpers
*****************************************************************************}

    procedure convert_register_to_enum(var r:Tregister);
    
    begin
      if r.enum=R_INTREGISTER then
        case r.number of
          NR_NO:  r.enum:=R_NO;
          NR_EAX: r.enum:=R_EAX;        NR_EBX: r.enum:=R_EBX;
          NR_ECX: r.enum:=R_ECX;        NR_EDX: r.enum:=R_EDX;
          NR_ESI: r.enum:=R_ESI;        NR_EDI: r.enum:=R_EDI;
          NR_ESP: r.enum:=R_ESP;        NR_EBP: r.enum:=R_EBP;
          NR_AX:  r.enum:=R_AX;         NR_BX:  r.enum:=R_BX;
          NR_CX:  r.enum:=R_CX;         NR_DX:  r.enum:=R_DX;
          NR_SI:  r.enum:=R_SI;         NR_DI:  r.enum:=R_DI;
          NR_SP:  r.enum:=R_SP;         NR_BP:  r.enum:=R_BP;
          NR_AL:  r.enum:=R_AL;         NR_BL:  r.enum:=R_BL;
          NR_CL:  r.enum:=R_CL;         NR_DL:  r.enum:=R_DL;
          NR_AH:  r.enum:=R_AH;         NR_BH:  r.enum:=R_BH;
          NR_CH:  r.enum:=R_CH;         NR_DH:  r.enum:=R_DH;
          NR_CS:  r.enum:=R_CS;         NR_DS:  r.enum:=R_DS;
          NR_ES:  r.enum:=R_ES;         NR_FS:  r.enum:=R_FS;
          NR_GS:  r.enum:=R_GS;         NR_SS:  r.enum:=R_SS;
        else
          internalerror(200301082);
        end;
    end;
    
    function cgsize2subreg(s:Tcgsize):Tsubregister;
    
    begin
      case s of
        OS_8,OS_S8:
          cgsize2subreg:=R_SUBL;
        OS_16,OS_S16:
          cgsize2subreg:=R_SUBW;
        OS_32,OS_S32:
          cgsize2subreg:=R_SUBD;
        OS_64,OS_S64:
          cgsize2subreg:=R_SUBQ;
        else
          internalerror(200301231);
      end;
    end;
    
    function supreg_name(r:Tsuperregister):string;
    
    var s:string[4];
    
    const supreg_names:array[0..last_supreg] of string[4]=
          ('INV',
           'eax','ebx','ecx','edx','esi','edi','ebp','esp',
           'r8' ,'r9', 'r10','r11','r12','r13','r14','r15');
    
    begin
      if r in [0..last_supreg] then
        supreg_name:=supreg_names[r]
      else
        begin
          str(r,s);
          supreg_name:='reg'+s;
        end;
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
  Revision 1.42  2003-02-19 22:00:15  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.41  2003/02/02 19:25:54  carl
    * Several bugfixes for m68k target (register alloc., opcode emission)
    + VIS target
    + Generic add more complete (still not verified)

  Revision 1.40  2003/01/13 18:37:44  daniel
    * Work on register conversion

  Revision 1.39  2003/01/09 20:41:00  daniel
    * Converted some code in cgx86.pas to new register numbering

  Revision 1.38  2003/01/09 15:49:56  daniel
    * Added register conversion

  Revision 1.37  2003/01/08 22:32:36  daniel
    * Added register convesrion procedure

  Revision 1.36  2003/01/08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.35  2003/01/05 13:36:53  florian
    * x86-64 compiles
    + very basic support for float128 type (x86-64 only)

  Revision 1.34  2002/11/17 18:26:16  mazen
  * fixed a compilation bug accmulator-->accumulator, in definition of return_result_reg

  Revision 1.33  2002/11/17 17:49:08  mazen
  + return_result_reg and function_result_reg are now used, in all plateforms, to pass functions result between called function and its caller. See the explanation of each one

  Revision 1.32  2002/10/05 12:43:29  carl
    * fixes for Delphi 6 compilation
     (warning : Some features do not work under Delphi)

  Revision 1.31  2002/08/14 18:41:48  jonas
    - remove valuelow/valuehigh fields from tlocation, because they depend
      on the endianess of the host operating system -> difficult to get
      right. Use lo/hi(location.valueqword) instead (remember to use
      valueqword and not value!!)

  Revision 1.30  2002/08/13 21:40:58  florian
    * more fixes for ppc calling conventions

  Revision 1.29  2002/08/12 15:08:41  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.28  2002/08/06 20:55:23  florian
    * first part of ppc calling conventions fix

  Revision 1.27  2002/07/25 18:01:29  carl
    + FPURESULTREG -> FPU_RESULT_REG

  Revision 1.26  2002/07/07 09:52:33  florian
    * powerpc target fixed, very simple units can be compiled
    * some basic stuff for better callparanode handling, far from being finished

  Revision 1.25  2002/07/01 18:46:30  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.24  2002/07/01 16:23:55  peter
    * cg64 patch
    * basics for currency
    * asnode updates for class and interface (not finished)

  Revision 1.23  2002/05/18 13:34:22  peter
    * readded missing revisions

  Revision 1.22  2002/05/16 19:46:50  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.19  2002/05/12 16:53:16  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.18  2002/04/21 15:31:40  carl
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

}
