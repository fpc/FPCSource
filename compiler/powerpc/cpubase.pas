{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Contains the base types for the PowerPC

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
      TAsmOp=(A_None,
        { normal opcodes }
        a_add, a_add_, a_addo, a_addo_, a_addc, a_addc_, a_addco, a_addco_,
        a_adde, a_adde_, a_addeo, a_addeo_, a_addi, a_addic, a_addic_, a_addis,
        a_addme, a_addme_, a_addmeo, a_addmeo_, a_addze, a_addze_, a_addzeo,
        a_addzeo_, a_and, a_and_, a_andc, a_andc_, a_andi_, a_andis_, a_b,
        a_ba, a_bl, a_bla, a_bc, a_bca, a_bcl, a_bcla, a_bcctr, a_bcctrl, a_bclr,
        a_bclrl, a_cmp, a_cmpi, a_cmpl, a_cmpli, a_cntlzw, a_cntlzw_, a_crand,
        a_crandc, a_creqv, a_crnand, a_crnor, a_cror, a_crorc, a_crxor, a_dcba,
        a_dcbf, a_dcbi, a_dcbst, a_dcbt, a_divw, a_divw_, a_divwo, a_divwo_,
        a_divwu, a_divwu_, a_divwuo, a_divwuo_, a_eciwx, a_ecowx, a_eieio, a_eqv,
        a_eqv_, a_extsb, a_extsb_, a_extsh, a_extsh_, a_fabs, a_fabs_, a_fadd,
        a_fadd_, a_fadds, a_fadds_, a_fcmpo, a_fcmpu, a_fctiw, a_fctw_, a_fctwz,
        a_fctwz_, a_fdiv, a_fdiv_, a_fdivs, a_fdivs_, a_fmadd, a_fmadd_, a_fmadds,
        a_fmadds_, a_fmr, a_fmsub, a_fmsub_, a_fmsubs, a_fmsubs_, a_fmul, a_fmul_,
        a_fmuls, a_fmuls_, a_fnabs, a_fnabs_, a_fneg, a_fneg_, a_fnmadd,
        a_fnmadd_, a_fnmadds, a_fnmadds_, a_fnmsub, a_fnmsub_, a_fnmsubs,
        a_fnmsubs_, a_fres, a_fres_, a_frsp, a_frsp_, a_frsqrte, a_frsqrte_,
        a_fsel, a_fsel_, a_fsqrt, a_fsqrt_, a_fsqrts, a_fsqrts_, a_fsub, a_fsub_,
        a_fsubs, a_fsubs_, a_icbi, a_isync, a_lbz, a_lbzu, a_lbzux, a_lbzx,
        a_lfd, a_lfdu, a_lfdux, a_lfdx, a_lfs, a_lfsu, a_lfsux, a_lfsx, a_lha,
        a_lhau, a_lhaux, a_lhax, a_hbrx, a_lhz, a_lhzu, a_lhzux, a_lhzx, a_lmw,
        a_lswi, a_lswx, a_lwarx, a_lwbrx, a_lwz, a_lwzu, a_lwzux, a_lwzx, a_mcrf,
        a_mcrfs, a_mcrxr, a_lcrxe, a_mfcr, a_mffs, a_maffs_, a_mfmsr, a_mfspr, a_mfsr,
        a_mfsrin, a_mftb, a_mtfcrf, a_a_mtfd0, a_mtfsb1, a_mtfsf, a_mtfsf_,
        a_mtfsfi, a_mtfsfi_, a_mtmsr, a_mtspr, a_mtsr, a_mtsrin, a_mulhw,
        a_mulhw_, a_mulhwu, a_mulhwu_, a_mulli, a_mullw, a_mullw_, a_mullwo,
        a_mullwo_, a_nand, a_nand_, a_neg, a_neg_, a_nego, a_nego_, a_nor, a_nor_,
        a_or, a_or_, a_orc, a_orc_, a_ori, a_oris, a_rfi, a_rlwimi, a_rlwimi_,
        a_rlwinm, a_rlwinm_, a_rlwnm, a_sc, a_slw, a_slw_, a_sraw, a_sraw_,
        a_srawi, a_srawi_,a_srw, a_srw_, a_stb, a_stbu, a_stbux, a_stbx, a_stfd,
        a_stfdu, a_stfdux, a_stfdx, a_stfiwx, a_stfs, a_stfsu, a_stfsux, a_stfsx,
        a_sth, a_sthbrx, a_sthu, a_sthux, a_sthx, a_stmw, a_stswi, a_stswx, a_stw,
        a_stwbrx, a_stwx_, a_stwu, a_stwux, a_stwx, a_subf, a_subf_, a_subfo,
        a_subfo_, a_subfc, a_subfc_, a_subfco, a_subfco_, a_subfe, a_subfe_,
        a_subfeo, a_subfeo_, a_subfic, a_subfme, a_subfme_, a_subfmeo, a_subfmeo_,
        a_subfze, a_subfze_, a_subfzeo, a_subfzeo_, a_sync, a_tlbia, a_tlbie,
        a_tlbsync, a_tw, a_twi, a_xor, a_xor_, a_xori, a_xoris,
        { simplified mnemonics }
        a_subi, a_subis, a_subic, a_subic_, a_sub, a_sub_, a_subo, a_subo_,
        a_subc, a_subc_, a_subco, a_subco_, a_cmpwi, a_cmpw, a_cmplwi, a_cmplw,
        a_extlwi, a_extlwi_, a_extrwi, a_extrwi_, a_inslwi, a_inslwi_, a_insrwi,
        a_insrwi_, a_rotlwi, a_rotlwi_, a_rotlw, a_rotlw_, a_slwi, a_slwi_,
        a_srwi, a_srwi_, a_clrlwi, a_clrlwi_, a_clrrwi, a_clrrwi_, a_clrslwi,
        a_clrslwi_, a_blr, a_bctr, a_blrl, a_bctrl, a_crset, a_crclr, a_crmove,
        a_crnot, a_mt {move to special prupose reg}, a_mf {move from special purpose reg},
        a_nop, a_li, a_lis, a_la, a_mr, a_mr_, a_not, a_mtcr, a_mtlr, a_mflr,
        a_mtctr, a_mfctr);

      {# This should define the array of instructions as string }
      op2strtable=array[tasmop] of string[8];

    Const
      {# First value of opcode enumeration }
      firstop = low(tasmop);
      {# Last value of opcode enumeration  }
      lastop  = high(tasmop);

{*****************************************************************************
                                  Registers
*****************************************************************************}

    type
      Toldregister = (R_NO,
        R_0,R_1,R_2,R_3,R_4,R_5,R_6,R_7,R_8,R_9,R_10,R_11,R_12,R_13,R_14,R_15,R_16,
        R_17,R_18,R_19,R_20,R_21,R_22,R_23,R_24,R_25,R_26,R_27,R_28,R_29,R_30,R_31,
        R_F0,R_F1,R_F2,R_F3,R_F4,R_F5,R_F6,R_F7,R_F8,R_F9,R_F10,R_F11,R_F12,
        R_F13,R_F14,R_F15,R_F16,R_F17, R_F18,R_F19,R_F20,R_F21,R_F22, R_F23,R_F24,
        R_F25,R_F26,R_F27,R_F28,R_F29,R_F30,R_F31,
        R_M0,R_M1,R_M2,R_M3,R_M4,R_M5,R_M6,R_M7,R_M8,R_M9,R_M10,R_M11,R_M12,
        R_M13,R_M14,R_M15,R_M16,R_M17,R_M18,R_M19,R_M20,R_M21,R_M22, R_M23,R_M24,
        R_M25,R_M26,R_M27,R_M28,R_M29,R_M30,R_M31,
        R_CR,R_CR0,R_CR1,R_CR2,R_CR3,R_CR4,R_CR5,R_CR6,R_CR7,
        R_XER,R_LR,R_CTR,R_FPSCR,

        R_INTREGISTER {Only for use by the register allocator.}
      );

      Tnewregister=word;
      Tsuperregister=byte;
      Tsubregister=byte;

      Tregister=record
        enum:Toldregister;
        number:Tnewregister;
      end;

      {# Set type definition for registers }
      tregisterset = set of Toldregister;
      Tsupregset=set of Tsuperregister;

      { A type to store register locations for 64 Bit values. }
      tregister64 = packed record
        reglo,reghi : tregister;
      end;

      { alias for compact code }
      treg64 = tregister64;


    Const
      {# First register in the tregister enumeration }
      firstreg = low(Toldregister);
      {# Last register in the tregister enumeration }
      lastreg  = R_FPSCR;
    type
      {# Type definition for the array of string of register nnames }
      treg2strtable = array[firstreg..lastreg] of string[5];

    const

      R_SPR1 = R_XER;
      R_SPR8 = R_LR;
      R_SPR9 = R_CTR;
      R_TOC = R_2;
   {   CR0 = 0;
      CR1 = 4;
      CR2 = 8;
      CR3 = 12;
      CR4 = 16;
      CR5 = 20;
      CR6 = 24;
      CR7 = 28;
      LT = 0;
      GT = 1;
      EQ = 2;
      SO = 3;
      FX = 4;
      FEX = 5;
      VX = 6;
      OX = 7;}

      mot_reg2str : treg2strtable = ('',
        'r0','r1','r2','r3','r4','r5','r6','r7','r8','r9','r10','r11','r12','r13',
        'r14','r15','r16','r17','r18','r19','r20','r21','r22','r23','r24','r25',
        'r26','r27','r28','r29','r30','r31',
        'F0','F1','F2','F3','F4','F5','F6','F7', 'F8','F9','F10','F11','F12',
        'F13','F14','F15','F16','F17', 'F18','F19','F20','F21','F22', 'F23','F24',
        'F25','F26','F27','F28','F29','F30','F31',
        'M0','M1','M2','M3','M4','M5','M6','M7','M8','M9','M10','M11','M12',
        'M13','M14','M15','M16','M17','M18','M19','M20','M21','M22', 'M23','M24',
        'M25','M26','M27','M28','M29','M30','M31',
        'CR','CR0','CR1','CR2','CR3','CR4','CR5','CR6','CR7',
        'XER','LR','CTR','FPSCR'
      );

      std_reg2str : treg2strtable = ('',
        'r0','r1','r2','r3','r4','r5','r6','r7','r8','r9','r10','r11','r12','r13',
        'r14','r15','r16','r17','r18','r19','r20','r21','r22','r23','r24','r25',
        'r26','r27','r28','r29','r30','r31',
        'F0','F1','F2','F3','F4','F5','F6','F7', 'F8','F9','F10','F11','F12',
        'F13','F14','F15','F16','F17', 'F18','F19','F20','F21','F22', 'F23','F24',
        'F25','F26','F27','F28','F29','F30','F31',
        'M0','M1','M2','M3','M4','M5','M6','M7','M8','M9','M10','M11','M12',
        'M13','M14','M15','M16','M17','M18','M19','M20','M21','M22', 'M23','M24',
        'M25','M26','M27','M28','M29','M30','M31',
        'CR','CR0','CR1','CR2','CR3','CR4','CR5','CR6','CR7',
        'XER','LR','CTR','FPSCR'
      );

    {New register coding:}

    {Special registers:}
    const
      NR_NO = $0000;  {Invalid register}

    {Normal registers:}

    {General purpose registers:}
      NR_R0 = $0100; NR_R1 = $0200; NR_R2 = $0300;
      NR_R3 = $0400; NR_R4 = $0500; NR_R5 = $0600;
      NR_R6 = $0700; NR_R7 = $0800; NR_R8 = $0900;
      NR_R9 = $0A00; NR_R10 = $0B00; NR_R11 = $0C00;
      NR_R12 = $0D00; NR_R13 = $0E00; NR_R14 = $0F00;
      NR_R15 = $1000; NR_R16 = $1100; NR_R17 = $1200;
      NR_R18 = $1300; NR_R19 = $1400; NR_R20 = $1500;
      NR_R21 = $1600; NR_R22 = $1700; NR_R23 = $1800;
      NR_R24 = $1900; NR_R25 = $1A00; NR_R26 = $1B00;
      NR_R27 = $1C00; NR_R28 = $1D00; NR_R29 = $1E00;
      NR_R30 = $1F00; NR_R31 = $2000;

      NR_RTOC = NR_R2;

    {Super registers:}
      RS_R0 = $01; RS_R1 = $02; RS_R2 = $03;
      RS_R3 = $04; RS_R4 = $05; RS_R5 = $06;
      RS_R6 = $07; RS_R7 = $08; RS_R8 = $09;
      RS_R9 = $0A; RS_R10 = $0B; RS_R11 = $0C;
      RS_R12 = $0D; RS_R13 = $0E; RS_R14 = $0F;
      RS_R15 = $10; RS_R16 = $11; RS_R17 = $12;
      RS_R18 = $13; RS_R19 = $14; RS_R20 = $15;
      RS_R21 = $16; RS_R22 = $17; RS_R23 = $18;
      RS_R24 = $19; RS_R25 = $1A; RS_R26 = $1B;
      RS_R27 = $1C; RS_R28 = $1D; RS_R29 = $1E;
      RS_R30 = $1F; RS_R31 = $20;

      first_supreg = $00;
      last_supreg = $20;
      {Number of first and last imaginary register.}
      first_imreg     = $21;
      last_imreg      = $ff;

    {Subregisters, situation unknown!!.}
      R_SUBWHOLE=$00;
      R_SUBL=$00;


{*****************************************************************************
                                Conditions
*****************************************************************************}

    type
      TAsmCondFlag = (C_None { unconditional jumps },
        { conditions when not using ctr decrement etc }
        C_LT,C_LE,C_EQ,C_GE,C_GT,C_NL,C_NE,C_NG,C_SO,C_NS,C_UN,C_NU,
        { conditions when using ctr decrement etc }
        C_T,C_F,C_DNZ,C_DNZT,C_DNZF,C_DZ,C_DZT,C_DZF);

    const
      { these are in the XER, but when moved to CR_x they correspond with the }
      { bits below (still needs to be verified!!!)                            }
      C_OV = C_EQ;
      C_CA = C_GT;

    type
      TAsmCond = packed record
                   case simple: boolean of
                     false: (BO, BI: byte);
                     true: (
                       cond: TAsmCondFlag;
                       case byte of
                         0: ();
                         { specifies in which part of the cr the bit has to be }
                         { tested for blt,bgt,beq,..,bnu                       }
                         1: (cr: R_CR0..R_CR7);
                         { specifies the bit to test for bt,bf,bdz,..,bdzf }
                         2: (crbit: byte)
                       );
                 end;

    const
      AsmCondFlag2BO: Array[C_T..C_DZF] of Byte =
        (12,4,16,8,0,18,10,2);

      AsmCondFlag2BI: Array[C_LT..C_NU] of Byte =
        (0,1,2,0,1,0,2,1,3,3,3,3);

      AsmCondFlagTF: Array[TAsmCondFlag] of Boolean =
        (false,true,false,true,false,true,false,false,false,true,false,true,false,
         true,false,false,true,false,false,true,false);

      AsmCondFlag2Str: Array[TAsmCondFlag] of string[4] = ({cf_none}'',
        { conditions when not using ctr decrement etc}
        'lt','le','eq','ge','gt','nl','ne','ng','so','ns','un','nu',
        't','f','dnz','dzt','dnzf','dz','dzt','dzf');

    const
      CondAsmOps=3;
      CondAsmOp:array[0..CondAsmOps-1] of TasmOp=(
         A_BC, A_TW, A_TWI
      );

{*****************************************************************************
                                   Flags
*****************************************************************************}

    type
      TResFlagsEnum = (F_EQ,F_NE,F_LT,F_LE,F_GT,F_GE,F_SO,F_FX,F_FEX,F_VX,F_OX);
      TResFlags = record
        cr: R_CR0..R_CR7;
        flag: TResFlagsEnum;
      end;

    (*
    const
      { arrays for boolean location conversions }

      flag_2_cond : array[TResFlags] of TAsmCond =
         (C_E,C_NE,C_LT,C_LE,C_GT,C_GE,???????????????);
    *)

{*****************************************************************************
                                Reference
*****************************************************************************}

    type
      trefoptions=(ref_none,ref_parafixup,ref_localfixup,ref_selffixup);

      { since we have only 16 offsets, we need to be able to specify the high }
      { and low 16 bits of the address of a symbol                            }
      trefsymaddr = (refs_full,refs_ha,refs_l);

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
         { means a full 32bit reference, refs_ha means the upper 16 bits }
         { and refs_l the lower 16 bits of the address                   }
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
      symaddr2str: array[trefsymaddr] of string[3] = ('','@ha','@l');

    const
      { MacOS only. Whether the direct data area (TOC) directly contain
        global variables. Otherwise it contains pointers to global variables. }
      macos_direct_globals = false;

{*****************************************************************************
                                Operand
*****************************************************************************}

    type
      toptype=(top_none,top_reg,top_ref,top_const,top_symbol,top_bool);

      toper=record
        ot  : longint;
        case typ : toptype of
         top_none   : ();
         top_reg    : (reg:tregister);
         top_ref    : (ref:^treference);
         top_const  : (val:aword);
         top_symbol : (sym:tasmsymbol;symofs:longint);
         top_bool  :  (b: boolean);
      end;

{*****************************************************************************
                                Operand Sizes
*****************************************************************************}

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
                1 : (value : AWord);
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
      max_operands = 5;

      lvaluelocations = [LOC_REFERENCE, LOC_CREGISTER, LOC_CFPUREGISTER,
                         LOC_CMMREGISTER];

      {# Constant defining possibly all registers which might require saving }
{$warning FIX ME !!!!!!!!! }
      ALL_REGISTERS = [R_0..R_FPSCR];
      ALL_INTREGISTERS = [1..255];

      general_registers = [R_0..R_31];
      general_superregisters = [RS_R0..RS_R31];

      {# low and high of the available maximum width integer general purpose }
      { registers                                                            }
      LoGPReg = R_0;
      HiGPReg = R_31;

      {# low and high of every possible width general purpose register (same as }
      { above on most architctures apart from the 80x86)                        }
      LoReg = R_0;
      HiReg = R_31;

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

      maxintregs = 18;
      intregs    = [R_0..R_31];
      usableregsint = [RS_R13..RS_R27];
      c_countusableregsint = 18;

      maxfpuregs = 31-14+1;
      fpuregs    = [R_F0..R_F31];
      usableregsfpu = [R_F14..R_F31];
      c_countusableregsfpu = 31-14+1;

      mmregs     = [R_M0..R_M31];
      usableregsmm  = [R_M14..R_M31];
      c_countusableregsmm  = 31-14+1;

      { no distinction on this platform }
      maxaddrregs = 0;
      addrregs    = [];
      usableregsaddr = [];
      c_countusableregsaddr = 0;


      firstsaveintreg = RS_R13;
      lastsaveintreg  = RS_R27;
      firstsavefpureg = R_F14;
      lastsavefpureg  = R_F31;
      { no altivec support yet. Need to override tcgobj.a_loadmm_* first in tcgppc }
      firstsavemmreg  = R_NO;
      lastsavemmreg   = R_NO;

      maxvarregs = 17;
      varregs : Array [1..maxvarregs] of Toldregister =
                (R_14,R_15,R_16,R_17,R_18,R_19,R_20,R_21,R_22,R_23,R_24,R_25,
                 R_26,R_27,R_28,R_29,R_30);

      maxfpuvarregs = 31-14+1;
      fpuvarregs : Array [1..maxfpuvarregs] of Toldregister =
                (R_F14,R_F15,R_F16,R_F17,R_F18,R_F19,R_F20,R_F21,R_F22,R_F23,
                 R_F24,R_F25,R_F26,R_F27,R_F28,R_F29,R_F30,R_F31);

      max_param_regs_int = 8;
      param_regs_int: Array[1..max_param_regs_int] of Toldregister =
        (R_3,R_4,R_5,R_6,R_7,R_8,R_9,R_10);

      max_param_regs_fpu = 13;
      param_regs_fpu: Array[1..max_param_regs_fpu] of Toldregister =
        (R_F1,R_F2,R_F3,R_F4,R_F5,R_F6,R_F7,R_F8,R_F9,R_F10,R_F11,R_F12,R_F13);

      max_param_regs_mm = 13;
      param_regs_mm: Array[1..max_param_regs_mm] of Toldregister =
        (R_M1,R_M2,R_M3,R_M4,R_M5,R_M6,R_M7,R_M8,R_M9,R_M10,R_M11,R_M12,R_M13);

      {# Registers which are defined as scratch and no need to save across
         routine calls or in assembler blocks.
      }
      max_scratch_regs = 3;
      scratch_regs: Array[1..max_scratch_regs] of Tsuperregister = (RS_R28,RS_R29,RS_R30);

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
      OS_VECTOR = OS_M128;

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

          stab_regindex : array[firstreg..lastreg] of shortint =
          (
           { R_NO }
           -1,
           { R0..R7 }
           0,1,2,3,4,5,6,7,
           { R8..R15 }
           8,9,10,11,12,13,14,15,
           { R16..R23 }
           16,17,18,19,20,21,22,23,
           { R24..R32 }
           24,25,26,27,28,29,30,31,
           { F0..F7 }
           32,33,34,35,36,37,38,39,
           { F8..F15 }
           40,41,42,43,44,45,46,47,
           { F16..F23 }
           48,49,50,51,52,53,54,55,
           { F24..F31 }
           56,57,58,59,60,61,62,63,
           { M0..M7 Multimedia registers are not supported by GCC }
           -1,-1,-1,-1,-1,-1,-1,-1,
           { M8..M15 }
           -1,-1,-1,-1,-1,-1,-1,-1,
           { M16..M23 }
           -1,-1,-1,-1,-1,-1,-1,-1,
           { M24..M31 }
           -1,-1,-1,-1,-1,-1,-1,-1,
           { CR }
           -1,
           { CR0..CR7 }
           68,69,70,71,72,73,74,75,
           { XER }
           76,
           { LR }
           65,
           { CTR }
           66,
           { FPSCR }
           -1
        );


{*****************************************************************************
                          Generic Register names
*****************************************************************************}

      {# Stack pointer register }
      stack_pointer_reg = R_1;
      NR_STACK_POINTER_REG = NR_R1;
      RS_STACK_POINTER_REG = RS_R1;
      {# Frame pointer register }
      frame_pointer_reg = stack_pointer_reg;
      NR_FRAME_POINTER_REG = NR_STACK_POINTER_REG;
      RS_FRAME_POINTER_REG = RS_STACK_POINTER_REG;
      {# Self pointer register : contains the instance address of an
         object or class. }
      self_pointer_reg = R_9;
      NR_SELF_POINTER_REG = NR_R9;
      RS_SELF_POINTER_REG = RS_R9;
      {# Register for addressing absolute data in a position independant way,
         such as in PIC code. The exact meaning is ABI specific. For
         further information look at GCC source : PIC_OFFSET_TABLE_REGNUM

         Taken from GCC rs6000.h
      }
{$warning As indicated in rs6000.h, but can't find it anywhere else!}
      pic_offset_reg = R_30;
      {# Results are returned in this register (32-bit values) }
      accumulator   = R_3;
      NR_ACCUMULATOR = NR_R3;
      RS_ACCUMULATOR = RS_R3;
  {the return_result_reg, is used inside the called function to store its return
  value when that is a scalar value otherwise a pointer to the address of the
  result is placed inside it}
        return_result_reg               =       accumulator;
      NR_RETURN_RESULT_REG = NR_ACCUMULATOR;
      RS_RETURN_RESULT_REG = RS_ACCUMULATOR;

  {the function_result_reg contains the function result after a call to a scalar
  function othewise it contains a pointer to the returned result}
        function_result_reg     =       accumulator;
      {# Hi-Results are returned in this register (64-bit value high register) }
      accumulatorhigh = R_4;
      NR_ACCUMULATORHIGH = NR_R4;
      RS_ACCUMULATORHIGH = RS_R4;
      { WARNING: don't change to R_ST0!! See comments above implementation of }
      { a_loadfpu* methods in rgcpu (JM)                                      }
      fpu_result_reg = R_F1;
      mmresultreg = R_M0;

{*****************************************************************************
                       GCC /ABI linking information
*****************************************************************************}

      {# Registers which must be saved when calling a routine declared as
         cppdecl, cdecl, stdcall, safecall, palmossyscall. The registers
         saved should be the ones as defined in the target ABI and / or GCC.

         This value can be deduced from CALLED_USED_REGISTERS array in the
         GCC source.
      }
      std_saved_registers = [RS_R13..RS_R29];
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

      LinkageAreaSize = 24;
      { offset in the linkage area for the saved stack pointer }
      LA_SP = 0;
      { offset in the linkage area for the saved conditional register}
      LA_CR = 4;
      { offset in the linkage area for the saved link register}
      LA_LR = 8;
      { offset in the linkage area for the saved RTOC register}
      LA_RTOC = 20;

{*****************************************************************************
                                  Helpers
*****************************************************************************}

    function  is_calljmp(o:tasmop):boolean;

    procedure inverse_flags(var r : TResFlags);
    procedure inverse_cond(const c: TAsmCond;var r : TAsmCond);
    function  flags_to_cond(const f: TResFlags) : TAsmCond;
    procedure create_cond_imm(BO,BI:byte;var r : TAsmCond);
    procedure create_cond_norm(cond: TAsmCondFlag; cr: byte;var r : TasmCond);
    procedure convert_register_to_enum(var r:Tregister);
    function cgsize2subreg(s:Tcgsize):Tsubregister;

implementation

    uses
      verbose;

{*****************************************************************************
                                  Helpers
*****************************************************************************}

    function is_calljmp(o:tasmop):boolean;
      begin
       is_calljmp:=false;
        case o of
          A_B,A_BA,A_BL,A_BLA,A_BC,A_BCA,A_BCL,A_BCLA,A_BCCTR,A_BCCTRL,A_BCLR,
            A_BCLRL,A_TW,A_TWI: is_calljmp:=true;
        end;
      end;


    procedure inverse_flags(var r: TResFlags);
      const
        inv_flags: array[F_EQ..F_GE] of TResFlagsEnum =
          (F_NE,F_EQ,F_GE,F_GE,F_LE,F_LT);
      begin
        r.flag := inv_flags[r.flag];
      end;


    procedure inverse_cond(const c: TAsmCond;var r : TAsmCond);
      const
        inv_condflags:array[TAsmCondFlag] of TAsmCondFlag=(C_None,
          C_GE,C_GT,C_NE,C_LT,C_LE,C_LT,C_EQ,C_GT,C_NS,C_SO,C_NU,C_UN,
          C_F,C_T,C_DNZ,C_DNZF,C_DNZT,C_DZ,C_DZF,C_DZT);
      begin
        r := c;
        r.cond := inv_condflags[c.cond];
      end;


    function flags_to_cond(const f: TResFlags) : TAsmCond;
      const
        flag_2_cond: array[F_EQ..F_SO] of TAsmCondFlag =
          (C_EQ,C_NE,C_LT,C_LE,C_GT,C_GE,C_SO);
      begin
        if f.flag > high(flag_2_cond) then
          internalerror(200112301);
        result.simple := true;
        result.cr := f.cr;
        result.cond := flag_2_cond[f.flag];
      end;


    procedure create_cond_imm(BO,BI:byte;var r : TAsmCond);
      begin
        r.simple := false;
        r.bo := bo;
        r.bi := bi;
      end;


    procedure create_cond_norm(cond: TAsmCondFlag; cr: byte;var r : TasmCond);

      begin
        r.simple := true;
        r.cond := cond;
        case cond of
          C_NONE:;
          C_T..C_DZF: r.crbit := cr
          else r.cr := Toldregister(ord(R_CR0)+cr);
        end;
      end;

    procedure convert_register_to_enum(var r:Tregister);

    begin
      if r.enum = R_INTREGISTER then
        case r.number of
          NR_NO: r.enum:= R_NO;

          NR_R0: r.enum:= R_0;
          NR_R1: r.enum:= R_1;
          NR_R2: r.enum:= R_2;
          NR_R3: r.enum:= R_3;
          NR_R4: r.enum:= R_4;
          NR_R5: r.enum:= R_5;
          NR_R6: r.enum:= R_6;
          NR_R7: r.enum:= R_7;
          NR_R8: r.enum:= R_8;
          NR_R9: r.enum:= R_9;
          NR_R10: r.enum:= R_10;
          NR_R11: r.enum:= R_11;
          NR_R12: r.enum:= R_12;
          NR_R13: r.enum:= R_13;
          NR_R14: r.enum:= R_14;
          NR_R15: r.enum:= R_15;
          NR_R16: r.enum:= R_16;
          NR_R17: r.enum:= R_17;
          NR_R18: r.enum:= R_18;
          NR_R19: r.enum:= R_19;
          NR_R20: r.enum:= R_20;
          NR_R21: r.enum:= R_21;
          NR_R22: r.enum:= R_22;
          NR_R23: r.enum:= R_23;
          NR_R24: r.enum:= R_24;
          NR_R25: r.enum:= R_25;
          NR_R26: r.enum:= R_26;
          NR_R27: r.enum:= R_27;
          NR_R28: r.enum:= R_28;
          NR_R29: r.enum:= R_29;
          NR_R30: r.enum:= R_30;
          NR_R31: r.enum:= R_31;
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
  Revision 1.50  2003-05-15 22:14:43  florian
    * fixed last commit, changing lastsaveintreg to r31 caused some strange problems

  Revision 1.49  2003/05/15 21:37:00  florian
    * sysv entry code saves r13 now as well

  Revision 1.48  2003/04/23 12:35:35  florian
    * fixed several issues with powerpc
    + applied a patch from Jonas for nested function calls (PowerPC only)
    * ...

  Revision 1.47  2003/04/22 11:27:48  florian
    + added first_ and last_imreg

  Revision 1.46  2003/03/19 14:26:26  jonas
    * fixed R_TOC bugs introduced by new register allocator conversion

  Revision 1.45  2003/03/11 21:46:24  jonas
    * lots of new regallocator fixes, both in generic and ppc-specific code
      (ppc compiler still can't compile the linux system unit though)

  Revision 1.44  2003/02/19 22:00:16  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.43  2003/02/02 19:25:54  carl
    * Several bugfixes for m68k target (register alloc., opcode emission)
    + VIS target
    + Generic add more complete (still not verified)

  Revision 1.42  2003/01/16 11:31:28  olle
    + added new register constants
    + implemented register convertion proc

  Revision 1.41  2003/01/13 17:17:50  olle
    * changed global var access, TOC now contain pointers to globals
    * fixed handling of function pointers

  Revision 1.40  2003/01/09 15:49:56  daniel
    * Added register conversion

  Revision 1.39  2003/01/08 18:43:58  daniel
   * Tregister changed into a record

  Revision 1.38  2002/11/25 17:43:27  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.37  2002/11/24 14:28:56  jonas
    + some comments describing the fields of treference

  Revision 1.36  2002/11/17 18:26:16  mazen
  * fixed a compilation bug accmulator-->accumulator, in definition of return_result_reg

  Revision 1.35  2002/11/17 17:49:09  mazen
  + return_result_reg and function_result_reg are now used, in all plateforms, to pass functions result between called function and its caller. See the explanation of each one

  Revision 1.34  2002/09/17 18:54:06  jonas
    * a_load_reg_reg() now has two size parameters: source and dest. This
      allows some optimizations on architectures that don't encode the
      register size in the register name.

  Revision 1.33  2002/09/07 17:54:59  florian
    * first part of PowerPC fixes

  Revision 1.32  2002/09/07 15:25:14  peter
    * old logs removed and tabs fixed

  Revision 1.31  2002/09/01 21:04:49  florian
    * several powerpc related stuff fixed

  Revision 1.30  2002/08/18 22:16:15  florian
    + the ppc gas assembler writer adds now registers aliases
      to the assembler file

  Revision 1.29  2002/08/18 21:36:42  florian
    + handling of local variables in direct reader implemented

  Revision 1.28  2002/08/14 18:41:47  jonas
    - remove valuelow/valuehigh fields from tlocation, because they depend
      on the endianess of the host operating system -> difficult to get
      right. Use lo/hi(location.valueqword) instead (remember to use
      valueqword and not value!!)

  Revision 1.27  2002/08/13 21:40:58  florian
    * more fixes for ppc calling conventions

  Revision 1.26  2002/08/12 15:08:44  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.25  2002/08/10 17:15:06  jonas
    * endianess fix

  Revision 1.24  2002/08/06 20:55:24  florian
    * first part of ppc calling conventions fix

  Revision 1.23  2002/08/04 12:57:56  jonas
    * more misc. fixes, mostly constant-related

  Revision 1.22  2002/07/27 19:57:18  jonas
    * some typo corrections in the instruction tables
    * renamed the m* registers to v*

  Revision 1.21  2002/07/26 12:30:51  jonas
    * fixed typo in instruction table (_subco_ -> a_subco)

  Revision 1.20  2002/07/25 18:04:10  carl
    + FPURESULTREG -> FPU_RESULT_REG

  Revision 1.19  2002/07/13 19:38:44  florian
    * some more generic calling stuff fixed

  Revision 1.18  2002/07/11 14:41:34  florian
    * start of the new generic parameter handling

  Revision 1.17  2002/07/11 07:35:36  jonas
    * some available registers fixes

  Revision 1.16  2002/07/09 19:45:01  jonas
    * unarynminus and shlshr node fixed for 32bit and smaller ordinals
    * small fixes in the assembler writer
    * changed scratch registers, because they were used by the linker (r11
      and r12) and by the abi under linux (r31)

  Revision 1.15  2002/07/07 09:44:31  florian
    * powerpc target fixed, very simple units can be compiled

  Revision 1.14  2002/05/18 13:34:26  peter
    * readded missing revisions

  Revision 1.12  2002/05/14 19:35:01  peter
    * removed old logs and updated copyright year

  Revision 1.11  2002/05/14 17:28:10  peter
    * synchronized cpubase between powerpc and i386
    * moved more tables from cpubase to cpuasm
    * tai_align_abstract moved to tainst, cpuasm must define
      the tai_align class now, which may be empty

}
