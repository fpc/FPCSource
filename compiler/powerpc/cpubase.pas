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
  strings,cutils,cclasses,aasmbase,cpuinfo,cgbase;


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
      { Number of registers used for indexing in tables }
      tregisterindex=0..{$i rppcnor.inc}-1;
      totherregisterset = set of tregisterindex;

    const
      { Available Superregisters }
      {$i rppcsup.inc}

      { No Subregisters }
      R_SUBWHOLE=R_SUBNONE;

      { Available Registers }
      {$i rppccon.inc}

      { Integer Super registers first and last }
      first_int_supreg = RS_R0;
      last_int_supreg = RS_R31;

      first_int_imreg = $20;
      last_int_imreg = $fe;

      { Float Super register first and last }
      first_fpu_supreg    = $00;
      last_fpu_supreg     = $1f;

      first_fpu_imreg     = $20;
      last_fpu_imreg      = $fe;

      { MM Super register first and last }
      first_mmx_supreg    = RS_INVALID;
      last_mmx_supreg     = RS_INVALID;
      first_mmx_imreg     = RS_INVALID;
      last_mmx_imreg      = RS_INVALID;

{$warning TODO Calculate bsstart}
      regnumber_count_bsstart = 64;

      regnumber_table : array[tregisterindex] of tregister = (
        {$i rppcnum.inc}
      );

      regstabs_table : array[tregisterindex] of tregister = (
        {$i rppcstab.inc}
      );

      { registers which may be destroyed by calls }
      VOLATILE_INTREGISTERS = [RS_R3..RS_R12];
{$warning FIXME!!}
      { FIXME: only R_F1..R_F8 under the SYSV ABI -> has to become a }
      {   typed const (JM)                                                                                                                                                                       }
      VOLATILE_FPUREGISTERS = [RS_F3..RS_F13];


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
                         1: (cr: RS_CR0..RS_CR7);
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
        cr: RS_CR0..RS_CR7;
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
         {Word alignment on stack 4 --> 32 bit}
         Alignment:Byte;
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
      max_operands = 5;

      general_superregisters = [RS_R0..RS_R31];

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
      { to determine how many registers to use for regvars }
      maxintscratchregs = 3;
      usableregsint = [RS_R13..RS_R27];
      c_countusableregsint = 18;

      maxfpuregs = 31-14+1;
      usableregsfpu = [RS_F14..RS_F31];
      c_countusableregsfpu = 31-14+1;

      usableregsmm  = [RS_M14..RS_M31];
      c_countusableregsmm  = 31-14+1;

      { no distinction on this platform }
      maxaddrregs = 0;
      addrregs    = [];
      usableregsaddr = [];
      c_countusableregsaddr = 0;

      firstsaveintreg = RS_R13;
      lastsaveintreg  = RS_R31;
      firstsavefpureg = RS_F14;
      lastsavefpureg  = RS_F31;
      { no altivec support yet. Need to override tcgobj.a_loadmm_* first in tcgppc }
      firstsavemmreg  = RS_INVALID;
      lastsavemmreg   = RS_INVALID;

      maxvarregs = 15;
      varregs : Array [1..maxvarregs] of Tsuperregister =
                (RS_R14,RS_R15,RS_R16,RS_R17,RS_R18,RS_R19,RS_R20,RS_R21,
                 RS_R22,RS_R23,RS_R24,RS_R25,RS_R26,RS_R27,RS_R28);

      maxfpuvarregs = 31-14+1;
      fpuvarregs : Array [1..maxfpuvarregs] of Tsuperregister =
                (RS_F14,RS_F15,RS_F16,RS_F17,RS_F18,RS_F19,RS_F20,RS_F21,RS_F22,RS_F23,
                 RS_F24,RS_F25,RS_F26,RS_F27,RS_F28,RS_F29,RS_F30,RS_F31);

{
//      max_param_regs_int = 8;
//      param_regs_int: Array[1..max_param_regs_int] of Tsuperregister =
//        (R_3,R_4,R_5,R_6,R_7,R_8,R_9,R_10);

//      max_param_regs_fpu = 13;
//      param_regs_fpu: Array[1..max_param_regs_fpu] of Toldregister =
//        (RS_F1,RS_F2,RS_F3,RS_F4,RS_F5,RS_F6,RS_F7,RS_F8,RS_F9,RS_F10,RS_F11,RS_F12,RS_F13);

      max_param_regs_mm = 13;
      param_regs_mm: Array[1..max_param_regs_mm] of Toldregister =
        (R_M1,R_M2,R_M3,R_M4,R_M5,R_M6,R_M7,R_M8,R_M9,R_M10,R_M11,R_M12,R_M13);
}


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

      stab_regindex : array[tregisterindex] of shortint = (
        {$i rppcstab.inc}
      );


{*****************************************************************************
                          Generic Register names
*****************************************************************************}

      {# Stack pointer register }
      NR_STACK_POINTER_REG = NR_R1;
      RS_STACK_POINTER_REG = RS_R1;
      {# Frame pointer register }
      NR_FRAME_POINTER_REG = NR_STACK_POINTER_REG;
      RS_FRAME_POINTER_REG = RS_STACK_POINTER_REG;
      {# Register for addressing absolute data in a position independant way,
         such as in PIC code. The exact meaning is ABI specific. For
         further information look at GCC source : PIC_OFFSET_TABLE_REGNUM

         Taken from GCC rs6000.h
      }
{$warning As indicated in rs6000.h, but can't find it anywhere else!}
      NR_PIC_OFFSET_REG = NR_R30;
      { Results are returned in this register (32-bit values) }
      NR_FUNCTION_RETURN_REG = NR_R3;
      RS_FUNCTION_RETURN_REG = RS_R3;
      { Low part of 64bit return value }
      NR_FUNCTION_RETURN64_LOW_REG = NR_R4;
      RS_FUNCTION_RETURN64_LOW_REG = RS_R4;
      { High part of 64bit return value }
      NR_FUNCTION_RETURN64_HIGH_REG = NR_R3;
      RS_FUNCTION_RETURN64_HIGH_REG = RS_R3;
      { The value returned from a function is available in this register }
      NR_FUNCTION_RESULT_REG = NR_FUNCTION_RETURN_REG;
      RS_FUNCTION_RESULT_REG = RS_FUNCTION_RETURN_REG;
      { The lowh part of 64bit value returned from a function }
      NR_FUNCTION_RESULT64_LOW_REG = NR_FUNCTION_RETURN64_LOW_REG;
      RS_FUNCTION_RESULT64_LOW_REG = RS_FUNCTION_RETURN64_LOW_REG;
      { The high part of 64bit value returned from a function }
      NR_FUNCTION_RESULT64_HIGH_REG = NR_FUNCTION_RETURN64_HIGH_REG;
      RS_FUNCTION_RESULT64_HIGH_REG = RS_FUNCTION_RETURN64_HIGH_REG;

      NR_FPU_RESULT_REG = NR_F1;
      NR_MM_RESULT_REG = NR_M0;


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

      LinkageAreaSizeAIX = 24;
      LinkageAreaSizeSYSV = 8;
      { offset in the linkage area for the saved stack pointer }
      LA_SP = 0;
      { offset in the linkage area for the saved conditional register}
      LA_CR_AIX = 4;
      { offset in the linkage area for the saved link register}
      LA_LR_AIX = 8;
      LA_LR_SYSV = 4;
      { offset in the linkage area for the saved RTOC register}
      LA_RTOC_AIX = 20;

      PARENT_FRAMEPOINTER_OFFSET = 12;

      NR_RTOC = NR_R2;

{*****************************************************************************
                                  Helpers
*****************************************************************************}

    function  is_calljmp(o:tasmop):boolean;

    procedure inverse_flags(var r : TResFlags);
    procedure inverse_cond(const c: TAsmCond;var r : TAsmCond);
    function  flags_to_cond(const f: TResFlags) : TAsmCond;
    procedure create_cond_imm(BO,BI:byte;var r : TAsmCond);
    procedure create_cond_norm(cond: TAsmCondFlag; cr: byte;var r : TasmCond);
    function cgsize2subreg(s:Tcgsize):Tsubregister;
    function findreg_by_number(r:Tregister):tregisterindex;
    function std_regnum_search(const s:string):Tregister;
    function std_regname(r:Tregister):string;
    function gas_regname(r:Tregister):string;


implementation

    uses
      verbose;

    const
      std_regname_table : array[tregisterindex] of string[7] = (
        {$i rppcstd.inc}
      );

      gas_regname_table : array[tregisterindex] of string[7] = (
        {$i rppcgas.inc}
      );

      regnumber_index : array[tregisterindex] of tregisterindex = (
        {$i rppcrni.inc}
      );

      std_regname_index : array[tregisterindex] of tregisterindex = (
        {$i rppcsri.inc}
      );


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
          else r.cr := RS_CR0+cr;
        end;
      end;


    function cgsize2subreg(s:Tcgsize):Tsubregister;
      begin
        cgsize2subreg:=R_SUBWHOLE;
      end;


    function findreg_by_stdname(const s:string):byte;
      var
        i,p : tregisterindex;
      begin
        {Binary search.}
        p:=0;
        i:=regnumber_count_bsstart;
        repeat
          if (p+i<=high(tregisterindex)) and (std_regname_table[std_regname_index[p+i]]<=s) then
            p:=p+i;
          i:=i shr 1;
        until i=0;
        if std_regname_table[std_regname_index[p]]=s then
          result:=std_regname_index[p]
        else
          result:=0;
      end;


    function findreg_by_number(r:Tregister):tregisterindex;
      var
        i,p : tregisterindex;
      begin
        {Binary search.}
        p:=0;
        i:=regnumber_count_bsstart;
        repeat
          if (p+i<=high(tregisterindex)) and (regnumber_table[regnumber_index[p+i]]<=r) then
            p:=p+i;
          i:=i shr 1;
        until i=0;
        if regnumber_table[regnumber_index[p]]=r then
          result:=regnumber_index[p]
        else
          result:=0;
      end;


    function std_regnum_search(const s:string):Tregister;
      begin
        result:=regnumber_table[findreg_by_stdname(s)];
      end;


    function std_regname(r:Tregister):string;
      var
        p : tregisterindex;
      begin
        p:=findreg_by_number(r);
        if p<>0 then
          result:=std_regname_table[p]
        else
          result:=generic_regname(r);
      end;


    function gas_regname(r:Tregister):string;
      var
        p : tregisterindex;
      begin
        p:=findreg_by_number(r);
        if p<>0 then
          result:=gas_regname_table[p]
        else
          result:=generic_regname(r);
      end;

end.
{
  $Log$
  Revision 1.70  2003-10-08 14:11:36  mazen
  + Alignement field added to TParaLocation (=4 as 32 bits archs)

  Revision 1.69  2003/10/01 20:34:49  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.68  2003/09/14 16:37:20  jonas
    * fixed some ppc problems

  Revision 1.67  2003/09/03 21:04:14  peter
    * some fixes for ppc

  Revision 1.66  2003/09/03 19:35:24  peter
    * powerpc compiles again

  Revision 1.65  2003/09/03 11:18:37  florian
    * fixed arm concatcopy
    + arm support in the common compiler sources added
    * moved some generic cg code around
    + tfputype added
    * ...

  Revision 1.64  2003/08/17 16:59:20  jonas
    * fixed regvars so they work with newra (at least for ppc)
    * fixed some volatile register bugs
    + -dnotranslation option for -dnewra, which causes the registers not to
      be translated from virtual to normal registers. Requires support in
      the assembler writer as well, which is only implemented in aggas/
      agppcgas currently

  Revision 1.63  2003/08/08 15:51:16  olle
    * merged macos entry/exit code generation into the general one.

  Revision 1.62  2003/07/23 11:00:09  jonas
    * "lastsaveintreg" is RS_R31 instead of RS_R27 with -dnewra, because
      there are no scratch regs anymore

  Revision 1.61  2003/07/06 20:25:03  jonas
    * fixed ppc compiler

  Revision 1.60  2003/07/06 15:28:24  jonas
    * VOLATILE_REGISTERS was wrong (it was more or less the inverted set
      of what it had to be :/ )

  Revision 1.59  2003/06/17 16:34:44  jonas
    * lots of newra fixes (need getfuncretparaloc implementation for i386)!
    * renamed all_intregisters to volatile_intregisters and made it
      processor dependent

  Revision 1.58  2003/06/14 22:32:43  jonas
    * ppc compiles with -dnewra, haven't tried to compile anything with it
      yet though

  Revision 1.57  2003/06/13 17:44:44  jonas
    + added supreg_name function

  Revision 1.56  2003/06/12 19:11:34  jonas
    - removed ALL_INTREGISTERS (only the one in rgobj is valid)

  Revision 1.55  2003/05/31 15:05:28  peter
    * FUNCTION_RESULT64_LOW/HIGH_REG added for int64 results

  Revision 1.54  2003/05/30 23:57:08  peter
    * more sparc cleanup
    * accumulator removed, splitted in function_return_reg (called) and
      function_result_reg (caller)

  Revision 1.53  2003/05/30 18:49:59  jonas
    * changed scratchregs from r28-r30 to r29-r31
    * made sure the regvar registers don't overlap with the scratchregs
      anymore

  Revision 1.52  2003/05/24 16:02:01  jonas
    * fixed endian problem with tlocation.value/valueqword fields

  Revision 1.51  2003/05/16 16:26:05  jonas
    * adapted for Peter's regvar fixes

  Revision 1.50  2003/05/15 22:14:43  florian
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
  * fixed a compilation bug accmulator-->FUNCTION_RETURN_REG, in definition of return_result_reg

  Revision 1.35  2002/11/17 17:49:09  mazen
  + return_result_reg and FUNCTION_RESULT_REG are now used, in all plateforms, to pass functions result between called function and its caller. See the explanation of each one

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
