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
    {  warning: CPU32 opcodes are not fully compatible with the MC68020. }
       { 68000 only opcodes }
       tasmop = (a_abcd,
         a_add,a_adda,a_addi,a_addq,a_addx,a_and,a_andi,
         a_asl,a_asr,a_bcc,a_bcs,a_beq,a_bge,a_bgt,a_bhi,
         a_ble,a_bls,a_blt,a_bmi,a_bne,a_bpl,a_bvc,a_bvs,
         a_bchg,a_bclr,a_bra,a_bset,a_bsr,a_btst,a_chk,
         a_clr,a_cmp,a_cmpa,a_cmpi,a_cmpm,a_dbcc,a_dbcs,a_dbeq,a_dbge,
         a_dbgt,a_dbhi,a_dble,a_dbls,a_dblt,a_dbmi,a_dbne,a_dbra,
         a_dbpl,a_dbt,a_dbvc,a_dbvs,a_dbf,a_divs,a_divu,
         a_eor,a_eori,a_exg,a_illegal,a_ext,a_jmp,a_jsr,
         a_lea,a_link,a_lsl,a_lsr,a_move,a_movea,a_movei,a_moveq,
         a_movem,a_movep,a_muls,a_mulu,a_nbcd,a_neg,a_negx,
         a_nop,a_not,a_or,a_ori,a_pea,a_rol,a_ror,a_roxl,
         a_roxr,a_rtr,a_rts,a_sbcd,a_scc,a_scs,a_seq,a_sge,
         a_sgt,a_shi,a_sle,a_sls,a_slt,a_smi,a_sne,
         a_spl,a_st,a_svc,a_svs,a_sf,a_sub,a_suba,a_subi,a_subq,
         a_subx,a_swap,a_tas,a_trap,a_trapv,a_tst,a_unlk,
         a_rte,a_reset,a_stop,
         { mc68010 instructions }
         a_bkpt,a_movec,a_moves,a_rtd,
         { mc68020 instructions }
         a_bfchg,a_bfclr,a_bfexts,a_bfextu,a_bfffo,
         a_bfins,a_bfset,a_bftst,a_callm,a_cas,a_cas2,
         a_chk2,a_cmp2,a_divsl,a_divul,a_extb,a_pack,a_rtm,
         a_trapcc,a_tracs,a_trapeq,a_trapf,a_trapge,a_trapgt,
         a_traphi,a_traple,a_trapls,a_traplt,a_trapmi,a_trapne,
         a_trappl,a_trapt,a_trapvc,a_trapvs,a_unpk,
         { fpu processor instructions - directly supported only. }
         { ieee aware and misc. condition codes not supported   }
         a_fabs,a_fadd,
         a_fbeq,a_fbne,a_fbngt,a_fbgt,a_fbge,a_fbnge,
         a_fblt,a_fbnlt,a_fble,a_fbgl,a_fbngl,a_fbgle,a_fbngle,
         a_fdbeq,a_fdbne,a_fdbgt,a_fdbngt,a_fdbge,a_fdbnge,
         a_fdblt,a_fdbnlt,a_fdble,a_fdbgl,a_fdbngl,a_fdbgle,a_fbdngle,
         a_fseq,a_fsne,a_fsgt,a_fsngt,a_fsge,a_fsnge,
         a_fslt,a_fsnlt,a_fsle,a_fsgl,a_fsngl,a_fsgle,a_fsngle,
         a_fcmp,a_fdiv,a_fmove,a_fmovem,
         a_fmul,a_fneg,a_fnop,a_fsqrt,a_fsub,a_fsgldiv,
         a_fsflmul,a_ftst,
         a_ftrapeq,a_ftrapne,a_ftrapgt,a_ftrapngt,a_ftrapge,a_ftrapnge,
         a_ftraplt,a_ftrapnlt,a_ftraple,a_ftrapgl,a_ftrapngl,a_ftrapgle,a_ftrapngle,
         { protected instructions }
         a_cprestore,a_cpsave,
         { fpu unit protected instructions                    }
         { and 68030/68851 common mmu instructions            }
         { (this may include 68040 mmu instructions)          }
         a_frestore,a_fsave,a_pflush,a_pflusha,a_pload,a_pmove,a_ptest,
         { useful for assembly langage output }
         a_label,a_none,a_dbxx,a_sxx,a_bxx,a_fbxx);

      {# This should define the array of instructions as string }
      op2strtable=array[tasmop] of string[11];

    Const
      {# First value of opcode enumeration }
      firstop = low(tasmop);
      {# Last value of opcode enumeration  }
      lastop  = high(tasmop);

{*****************************************************************************
                                  Registers
*****************************************************************************}

    type
       tregister = (
         R_NO,R_D0,R_D1,R_D2,R_D3,R_D4,R_D5,R_D6,R_D7,
         R_A0,R_A1,R_A2,R_A3,R_A4,R_A5,R_A6,R_SP,
         { PUSH/PULL- quick and dirty hack }
         R_SPPUSH,R_SPPULL,
         { misc. }
         R_CCR,R_FP0,R_FP1,R_FP2,R_FP3,R_FP4,R_FP5,R_FP6,
         R_FP7,R_FPCR,R_SR,R_SSP,R_DFC,R_SFC,R_VBR,R_FPSR);

      {# Set type definition for registers }
      tregisterset = set of tregister;

      { A type to store register locations for 64 Bit values. }
      tregister64 = packed record
        reglo,reghi : tregister;
      end;

      { alias for compact code }
      treg64 = tregister64;

      {# Type definition for the array of string of register nnames }
      reg2strtable = array[tregister] of string[7];

    Const
      {# First register in the tregister enumeration }
      firstreg = low(tregister);
      {# Last register in the tregister enumeration }
      lastreg  = high(tregister);

     std_reg2str : reg2strtable =
      ('', 'd0','d1','d2','d3','d4','d5','d6','d7',
       'a0','a1','a2','a3','a4','a5','a6','sp',
       '-(sp)','(sp)+',
       'ccr','fp0','fp1','fp2','fp3','fp4','fp5',
       'fp6','fp7','fpcr','sr','ssp','dfc',
       'sfc','vbr','fpsr');

{*****************************************************************************
                                Conditions
*****************************************************************************}

{*****************************************************************************
                                Conditions
*****************************************************************************}

    type
      TAsmCond=(C_None,
         C_CC,C_LS,C_CS,C_LT,C_EQ,C_MI,C_F,C_NE,
         C_GE,C_PL,C_GT,C_T,C_HI,C_VC,C_LE,C_VS
      );


    const
      cond2str:array[TAsmCond] of string[3]=('',
        'cc','ls','cs','lt','eq','mi','f','ne',
        'ge','pl','gt','t','hi','vc','le','vs'
      );


{*****************************************************************************
                                   Flags
*****************************************************************************}

    type
      TResFlags = (
          F_E,F_NE,
          F_G,F_L,F_GE,F_LE,F_C,F_NC,F_A,F_AE,F_B,F_BE);

{*****************************************************************************
                                Reference
*****************************************************************************}

    type
      trefoptions=(ref_none,ref_parafixup,ref_localfixup,ref_selffixup);

      { direction of address register :      }
      {              (An)     (An)+   -(An)  }
      tdirection = (dir_none,dir_inc,dir_dec);

      { reference record }
      preference = ^treference;
      treference = packed record
         base,
         index       : tregister;
         scalefactor : byte;
         offset      : longint;
         symbol      : tasmsymbol;
         offsetfixup : longint;
         options     : trefoptions;
         { indexed increment and decrement mode }
         { (An)+ and -(An)                      }
         direction : tdirection;
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
      toptype=(top_none,top_reg,top_ref,top_const,top_symbol,top_reglist);

      tregisterlist = set of tregister;

      toper=record
        ot  : longint;
        case typ : toptype of
         top_none   : ();
         top_reg    : (reg:tregister);
         top_ref    : (ref:preference);
         top_const  : (val:aword);
         top_symbol : (sym:tasmsymbol;symofs:longint);
         { used for pushing/popping multiple registers }
         top_reglist : (registerlist : tregisterlist);
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
        LOC_CFPUREGISTER  { if it is a FPU register variable on the fpu stack }
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
      end;


{*****************************************************************************
                                Operand Sizes
*****************************************************************************}

       { S_NO = No Size of operand   }
       { S_B  = 8-bit size operand   }
       { S_W  = 16-bit size operand  }
       { S_L  = 32-bit size operand  }
       { Floating point types        }
       { S_FS  = single type (32 bit) }
       { S_FD  = double/64bit integer }
       { S_FX  = Extended type      }
       topsize = (S_NO,S_B,S_W,S_L,S_FS,S_FD,S_FX,S_IQ);

{*****************************************************************************
                                 Constants
*****************************************************************************}

    const
      {# maximum number of operands in assembler instruction }
      max_operands = 4;

      lvaluelocations = [LOC_REFERENCE,LOC_CFPUREGISTER,LOC_CREGISTER];

      {# Constant defining possibly all registers which might require saving }
      ALL_REGISTERS = [R_D1..R_FPCR];

      general_registers = [R_D0..R_D7];

      {# low and high of the available maximum width integer general purpose }
      { registers                                                            }
      LoGPReg = R_D0;
      HiGPReg = R_D7;

      {# low and high of every possible width general purpose register (same as }
      { above on most architctures apart from the 80x86)                        }
      LoReg = LoGPReg;
      HiReg = HiGPReg;

      { Table of registers which can be allocated by the code generator
         internally, when generating the code.

       legend:
        xxxregs = set of all possibly used registers of that type in the code
                 generator
        usableregsxxx = set of all 32bit components of registers that can be
                 possible allocated to a regvar or using getregisterxxx (this
                 excludes registers which can be only used for parameter
                 passing on ABI's that define this)
       c_countusableregsxxx = amount of registers in the usableregsxxx set    }

      maxintregs = 8;
      intregs    = [R_D0..R_D7];
      usableregsint = [R_D2..R_D7];
      c_countusableregsint = 6;

      maxfpuregs = 8;
      fpuregs    = [R_FP0..R_FP7];
      usableregsfpu = [R_FP2..R_FP7];
      c_countusableregsfpu = 6;

      mmregs     = [];
      usableregsmm  = [];
      c_countusableregsmm  = 0;

      maxaddrregs = 8;
      addrregs    = [R_A0..R_SP];
      usableregsaddr = [R_A2..R_A4];
      c_countusableregsaddr = 3;


      { The first register in the usableregsint array }
      firstsaveintreg = R_D2;
      { The last register in the usableregsint array }
      lastsaveintreg  = R_D7;
      { The first register in the usableregsfpu array }
      firstsavefpureg = R_FP2;
      { The last  register in the usableregsfpu array }
      lastsavefpureg  = R_FP7;

      { these constants are m68k specific              }
      { The first register in the usableregsaddr array }
      firstsaveaddrreg = R_A2;
      { The last  register in the usableregsaddr array }
      lastsaveaddrreg  = R_A4;

      firstsavemmreg  = R_NO;
      lastsavemmreg   = R_NO;

      {
       Defines the maxinum number of integer registers which can be used as variable registers
      }
      maxvarregs = 6;
      { Array of integer registers which can be used as variable registers }
      varregs : Array [1..maxvarregs] of Tregister =
                (R_D2,R_D3,R_D4,R_D5,R_D6,R_D7);

      {
       Defines the maxinum number of float registers which can be used as variable registers
      }
      maxfpuvarregs = 6;
      { Array of float registers which can be used as variable registers }
      fpuvarregs : Array [1..maxfpuvarregs] of Tregister =
                (R_FP2,R_FP3,R_FP4,R_FP5,R_FP6,R_FP7);

      {
       Defines the number of integer registers which are used in the ABI to pass parameters
       (might be empty on systems which use the stack to pass parameters)
      }
      max_param_regs_int = 0;
      {param_regs_int: Array[1..max_param_regs_int] of tregister =
        (R_3,R_4,R_5,R_6,R_7,R_8,R_9,R_10);}

      {
       Defines the number of float registers which are used in the ABI to pass parameters
       (might be empty on systems which use the stack to pass parameters)
      }
      max_param_regs_fpu = 0;
      {param_regs_fpu: Array[1..max_param_regs_fpu] of tregister =
        (R_F1,R_F2,R_F3,R_F4,R_F5,R_F6,R_F7,R_F8,R_F9,R_F10,R_F11,R_F12,R_F13);}

      {
       Defines the number of mmx registers which are used in the ABI to pass parameters
       (might be empty on systems which use the stack to pass parameters)
      }
      max_param_regs_mm = 0;
      {param_regs_mm: Array[1..max_param_regs_mm] of tregister =
        (R_M1,R_M2,R_M3,R_M4,R_M5,R_M6,R_M7,R_M8,R_M9,R_M10,R_M11,R_M12,R_M13);}

      {# Registers which are defined as scratch integer and no need to save across
         routine calls or in assembler blocks.
      }
      max_scratch_regs = 2;
      scratch_regs: Array[1..max_scratch_regs] of TRegister = (R_D0,R_D1);

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

         Taken from m68kelf.h (DBX_REGISTER_NUMBER)
         from GCC 3.x source code.

         This is not compatible with the m68k-sun
         implementation.
      }
          stab_regindex : array[tregister] of shortint =
        (-1,                 { R_NO }
          0,1,2,3,4,5,6,7,   { R_D0..R_D7 }
          8,9,10,11,12,13,14,15,  { R_A0..R_A7 }
          -1,-1,-1,                { R_SPPUSH, R_SPPULL, R_CCR }
          18,19,20,21,22,23,24,25, { R_FP0..R_FP7    }
          -1,-1,-1,-1,-1,-1,-1);

{*****************************************************************************
                          Generic Register names
*****************************************************************************}

      {# Stack pointer register }
      stack_pointer_reg = R_SP;
      {# Frame pointer register }
      frame_pointer_reg = R_A6;
      {# Self pointer register : contains the instance address of an
         object or class. }
      self_pointer_reg  = R_A5;
      {# Register for addressing absolute data in a position independant way,
         such as in PIC code. The exact meaning is ABI specific. For
         further information look at GCC source : PIC_OFFSET_TABLE_REGNUM
      }
      pic_offset_reg = R_A5;
      {# Results are returned in this register (32-bit values) }
      accumulator   = R_D0;
      {# Hi-Results are returned in this register (64-bit value high register) }
      accumulatorhigh = R_D1;
      {# Floating point results will be placed into this register }
      FPU_RESULT_REG = R_FP0;
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
      std_saved_registers = [R_D2..R_D7,R_A2..R_A5];
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


{*****************************************************************************
                                  Helpers
*****************************************************************************}

    function  is_calljmp(o:tasmop):boolean;

    procedure inverse_flags(var r : TResFlags);
    function  flags_to_cond(const f: TResFlags) : TAsmCond;


implementation

    uses
      verbose;

{*****************************************************************************
                                  Helpers
*****************************************************************************}

    function is_calljmp(o:tasmop):boolean;
      begin
        is_calljmp := false;
        if o in [A_BXX,A_FBXX,A_DBXX,A_BCC..A_BVS,A_DBCC..A_DBVS,A_FBEQ..A_FSNGLE,
          A_JSR,A_BSR,A_JMP] then
           is_calljmp := true;
      end;

    procedure inverse_flags(var r: TResFlags);
      begin
      end;



    function flags_to_cond(const f: TResFlags) : TAsmCond;
      begin
      end;

end.
{
  $Log$
  Revision 1.8  2002-08-14 18:41:47  jonas
    - remove valuelow/valuehigh fields from tlocation, because they depend
      on the endianess of the host operating system -> difficult to get
      right. Use lo/hi(location.valueqword) instead (remember to use
      valueqword and not value!!)

  Revision 1.7  2002/08/13 21:40:58  florian
    * more fixes for ppc calling conventions

  Revision 1.6  2002/08/13 18:58:54  carl
    + m68k problems with cvs fixed?()!

  Revision 1.4  2002/08/12 15:08:44  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.3  2002/07/29 17:51:32  carl
    + restart m68k support


}
