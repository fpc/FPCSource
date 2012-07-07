{
    Copyright (c) 1998-2002 by the Free Pascal dev. team

    Contains the base types for the virtual instruction set

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
{ This Unit contains the base types for the Virtual Instruction machine
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
      TAsmOp=(a_none,a_beqs,a_bges,a_bgts,a_bles,a_blts,a_bnes,
              a_bras,a_rets,a_bccs,a_bcss,a_bvcs,a_bvss,a_bbss,
              a_bass,a_bats,a_bbts,a_beql,a_bgel,a_bgtl,a_blel,
              a_bltl,a_bnel,a_bral,a_bsrl,a_bbsl,a_basl,a_batl,
              a_bbtl,a_add,a_addc,a_and,a_asr,a_lsl,a_lsr,a_cmp,
              a_sub,a_subb,a_divs,a_divu,a_mod,a_move,a_muls,a_mulu,
              a_neg,a_not,a_or,a_xor,a_fadd,a_fcmp,a_fdiv,a_fmove,
              a_fmul,a_fneg,a_fsub,a_fldd,a_flds,a_lbzx,a_lbsx,a_llsx,
              a_llzx,a_lwsx,a_lwzx,a_fstd,a_fsts,a_stb,a_stl,a_stw,
              a_syscall,a_nop,a_lims,a_orhi,a_lilo,a_call,a_popl,
              a_pushl,
       { these are simplified mnemonics }
              a_lea,a_limm,a_bxx
              );

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
      toldregister = (R_NO,R_R0,R_R1,R_R2,R_R3,
                   R_R4,R_R5,R_R6,R_R7,
                   R_R8,R_R9,R_R10,R_R11,
                   R_CCR,R_SP,R_FP,R_PC,
                   R_FP0,R_FP1,R_FP2,R_FP3,
                   R_FP4,R_FP5,R_FP6,R_FP7,
                   R_FP8,R_FP9,R_FP10,R_FP11,
                   R_FP12,R_FP13,R_FP14,R_FP15,
                   R_INTREGISTER,R_FPUREGISTER
      );

      {# Set type definition for registers }
      tregisterset = set of Toldregister;
      Tnewregister=word;

      tregister=record
        enum:toldregister;
        number:Tnewregister;
      end;

      { A type to store register locations for 64 Bit values. }
      tregister64 = packed record
        reglo,reghi : tregister;
      end;

      Tsuperregister=byte;
      Tsubregister=byte;

      Tsupregset=set of Tsuperregister;

      { alias for compact code }
      treg64 = tregister64;

      {# Type definition for the array of string of register nnames }
      treg2strtable = array[toldregister] of string[5];

    Const

    {Special registers:}
      NR_NO = $0000;  {Invalid register}

    {Normal registers:}

    {General purpose registers:}
      NR_R0 = $0100; NR_R1 = $0200; NR_R2 = $0300;
      NR_R3 = $0400; NR_R4 = $0500; NR_R5 = $0600;
      NR_R6 = $0700; NR_R7 = $0800; NR_R8 = $0900;
      NR_R9 = $0A00; NR_R10 = $0B00; NR_R11 = $0C00;
      NR_SP = $0D00; NR_FP = $0E00;

    {Super registers:}
      RS_R0 = $01; RS_R1 = $02; RS_R2 = $03;
      RS_R3 = $04; RS_R4 = $05; RS_R5 = $06;
      RS_R6 = $07; RS_R7 = $08; RS_R8 = $09;
      RS_R9 = $0A; RS_R10 = $0B; RS_R11 = $0C;
      RS_SP = $0D; RS_FP = $0E;

    {Subregisters:}
      R_SUBL = $00;
      R_SUBW = $01;
      R_SUBD = $02;

      {# First register in the tregister enumeration }
      firstreg = low(toldregister);
      {# Last register in the tregister enumeration }
      lastreg  = high(toldregister);

      first_supreg = $01;
      last_supreg = $0c;


      std_reg2str : treg2strtable = ('',
        'r0','r1','r2','r3','r4','r5','r6','r7','r8','r9','r10','r11','ccr',
        'sp','fp','pc','fp0','fp1','fp2','fp3','fp4','fp5','fp6','fp7',
        'fp8','fp9','fp10','fp11','fp12','fp13','fp14','fp15','',''
      );


{*****************************************************************************
                                Conditions
*****************************************************************************}

    type
      TAsmCond=(C_None,
         C_EQ,                  { equal }
         C_NE,                  { not equal }
         C_GE,                  { greater or equal (signed) }
         C_GT,                  { greater than (signed)     }
         C_LE,                  { less or equal (signed)    }
         C_LT,                  { less than (signed)        }
         C_LS,                  { lower or same (unordered) }
         C_AS,                  { above or same (unordered) }
         C_AT,                  { above than (unordered)    }
         C_BT,                  { below than (unordered)    }
         C_CC,                  { carry clear               }
         C_CS                   { carry set                 }
      );


    const
      cond2str:array[TAsmCond] of string[3]=('',
        'eq','ne','ge','gt','le','lt','ls','as',
        'at','bt','cc','cs');


{*****************************************************************************
                                   Flags
*****************************************************************************}

    type
      TResFlags = (
          F_E,            { zero flag = equal      }
          F_NE,           { !zero_flag = not equal }
          F_G,            { greater (signed)       }
          F_L,            { less (signed)          }
          F_GE,
          F_LE,
          F_C,            { carry flag             }
          F_NC,           { !carry flag            }
          F_A,            { greater (unsigned)     }
          F_AE,
          F_B,            { less (unsigned)        }
          F_BE
         );

{*****************************************************************************
                                Reference
*****************************************************************************}

    type
      trefoptions=(ref_none,ref_parafixup,ref_localfixup,ref_selffixup);

      { reference record }
      preference = ^treference;
      treference = packed record
         base,
         index       : tregister;
         offset      : longint;
         symbol      : tasmsymbol;
         offsetfixup : longint;
         options     : trefoptions;
         alignment   : byte;
      end;

      { reference record }
      pparareference = ^tparareference;
      tparareference = packed record
         index       : tregister;
         offset      : aword;
      end;


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
                               Generic Location
*****************************************************************************}

    type
      TLoc=(
        { added for tracking problems}
        LOC_INVALID,
        { ordinal constant }
        LOC_CONSTANT,
        { in a processor register }
        LOC_REGISTER,
        { Constant register which shouldn't be modified }
        LOC_CREGISTER,
        { FPU register}
        LOC_FPUREGISTER,
        { Constant FPU register which shouldn't be modified }
        LOC_CFPUREGISTER,
        { multimedia register }
        LOC_MMREGISTER,
        { Constant multimedia reg which shouldn't be modified }
        LOC_CMMREGISTER,
        { in memory }
        LOC_REFERENCE,
        { in memory (constant) }
        LOC_CREFERENCE,
        { boolean results only, jump to false or true label }
        LOC_JUMP,
        { boolean results only, flags are set }
        LOC_FLAGS
      );

      { tparamlocation describes where a parameter for a procedure is stored.
        References are given from the caller's point of view. The usual
        TLocation isn't used, because contains a lot of unnessary fields.
      }
      tparalocation = packed record
         size : TCGSize;
         { The location type where the parameter is passed, usually
           LOC_REFERENCE,LOC_REGISTER or LOC_FPUREGISTER
         }
         loc  : TLoc;
         { The stack pointer must be decreased by this value before
           the parameter is copied to the given destination.
           This allows to "encode" pushes with tparalocation.
           On the PowerPC, this field is unsed but it is there
           because several generic code accesses it.
         }
         sp_fixup : longint;
         case TLoc of
            LOC_REFERENCE : (reference : tparareference);
            LOC_FPUREGISTER, LOC_CFPUREGISTER, LOC_MMREGISTER, LOC_CMMREGISTER,
              LOC_REGISTER,LOC_CREGISTER : (
              case longint of
                1 : (register,register64.reghi : tregister);
                { overlay a register64.reglo }
                2 : (register64.reglo : tregister);
                { overlay a 64 Bit register type }
                3 : (reg64 : tregister64);
                4 : (register64 : tregister64);
            );
      end;

      treglocation = packed record
        case longint of
          1 : (register,register64.reghi : tregister);
          { overlay a register64.reglo }
          2 : (register64.reglo : tregister);
          { overlay a 64 Bit register type }
          3 : (reg64 : tregister64);
          4 : (register64 : tregister64);
       end;


      tlocation = packed record
         size : TCGSize;
         loc : tloc;
         case tloc of
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
                  1 : (register64.reglo,register64.reghi : tregister);
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
      max_operands = 2;

      lvaluelocations = [LOC_REFERENCE, LOC_CREGISTER, LOC_CFPUREGISTER,
                         LOC_CMMREGISTER];

      {# Constant defining possibly all registers which might require saving }
      ALL_REGISTERS = [R_FP0..R_FP15];

      general_registers = [R_R0..R_R11];

      {# low and high of the available maximum width integer general purpose }
      { registers                                                            }
      LoGPReg = R_R0;
      HiGPReg = R_R11;

      {# low and high of every possible width general purpose register (same as }
      { above on most architctures apart from the 80x86)                        }
      LoReg = R_R0;
      HiReg = R_R11;

      maxintregs = 12;
      maxfpuregs = 16;
      maxaddrregs = 0;

      maxvarregs = 10;
      varregs : Array [1..maxvarregs] of toldregister =
                (R_R2,R_R3,R_R4,R_R5,R_R6,R_R7,R_R8,R_R9,R_R10,R_R11);

      maxfpuvarregs = 15;
      fpuvarregs : Array [1..maxfpuvarregs] of toldregister =
                (R_FP1,R_FP2,R_FP3,
                 R_FP4,R_FP5,R_FP6,
                 R_FP7,R_FP8,R_FP9,
                 R_FP10,R_FP11,R_FP12,
                 R_FP13,R_FP14,R_FP15);


      max_param_regs_int = 0;

      max_param_regs_fpu = 0;

      max_param_regs_mm = 0;

      {# Registers which are defined as scratch and no need to save across
         routine calls or in assembler blocks.
      }
      max_scratch_regs = 2;
      scratch_regs: Array[1..max_scratch_regs] of Tsuperregister = (RS_R0,RS_R1);

{*****************************************************************************
                          Default generic sizes
*****************************************************************************}

      {# Defines the default address size for a processor, }
      OS_ADDR = OS_32;
      {# the natural int size for a processor,
         has to match osuinttype/ossinttype as initialized in psystem }
      OS_INT = OS_32;
      {# the maximum float size for a processor,           }
      OS_FLOAT = OS_F64;
      {# the size of a vector register for a processor     }
      OS_VECTOR = OS_NO;

{*****************************************************************************
                               GDB Information
*****************************************************************************}

      {# Register indexes for stabs information, when some
         parameters or variables are stored in registers.

         Currently unsupported by abstract machine
      }

          stab_regindex : array[toldregister] of shortint =
          (-1,
           { r0..r11 }
           -1,-1,-1,-1,-1,-1,
           -1,-1,-1,-1,-1,-1,
           { sp,fp,ccr,pc }
           -1,-1,-1,-1,
           { FP0..FP7 }
           -1,-1,-1,-1,-1,-1,-1,-1,
           { FP8..FP15 }
           -1,-1,-1,-1,-1,-1,-1,-1,
           { invalid }
           -1,-1
        );


{*****************************************************************************
                          Generic Register names
*****************************************************************************}

      {# Stack pointer register }
      stack_pointer_reg = R_SP;
      NR_STACK_POINTER_REG = NR_SP;
      RS_STACK_POINTER_REG = RS_SP;
      {# Frame pointer register }
      frame_pointer_reg = R_FP;
      NR_FRAME_POINTER_REG = NR_FP;
      RS_FRAME_POINTER_REG = RS_FP;
      {# Self pointer register : contains the instance address of an
         object or class. }
      self_pointer_reg  = R_R11;
      NR_SELF_POINTER_REG = NR_R11;
      RS_SELF_POINTER_REG = RS_R11;
      {# Register for addressing absolute data in a position independant way,
         such as in PIC code. The exact meaning is ABI specific.
      }
      pic_offset_reg = R_R10;
      {# Results are returned in this register (32-bit values) }
      accumulator   = R_R0;
      NR_ACCUMULATOR = NR_R0;
      RS_ACCUMULATOR = RS_R0;
  {the return_result_reg, is used inside the called function to store its return
  value when that is a scalar value otherwise a pointer to the address of the
  result is placed inside it}
    return_result_reg       =   accumulator;

  {the function_result_reg contains the function result after a call to a scalar
  function othewise it contains a pointer to the returned result}
    function_result_reg =   accumulator;
      {# Hi-Results are returned in this register (64-bit value high register) }
      accumulatorhigh = R_R1;
      NR_ACCUMULATORHIGH = NR_R1;
      RS_ACCUMULATORHIGH = RS_R1;
      fpu_result_reg = R_FP0;
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
      std_saved_registers = [RS_R0,RS_R1,RS_R10,RS_R11];
      {# Required parameter alignment when calling a routine declared as
         stdcall and cdecl. The alignment value should be the one defined
         by GCC or the target ABI.

         The value of this constant is equal to the constant
         PARM_BOUNDARY / BITS_PER_UNIT in the GCC source.
      }
      std_param_align = 4;  { for 32-bit version only }


{*****************************************************************************
                                  Helpers
*****************************************************************************}

    function  is_calljmp(o:tasmop):boolean;

    procedure inverse_flags(var r : TResFlags);
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
      begin
        is_calljmp := false;
        if o in [a_bxx,a_call,a_beqs..a_bbtl] then
           is_calljmp := true;
      end;

    procedure inverse_flags(var r: TResFlags);
      const flagsinvers : array[F_E..F_BE] of tresflags =
            (F_NE,F_E,
             F_LE,F_GE,
             F_L,F_G,
             F_NC,F_C,
             F_BE,F_B,
             F_AE,F_A);
      begin
         r:=flagsinvers[r];
      end;



    function flags_to_cond(const f: TResFlags) : TAsmCond;
        const flags2cond : array[tresflags] of tasmcond =
        (
         {F_E}  C_EQ,
         {F_NE} C_NE,
         {F_G } C_GT,
         {F_L } C_LT,
         {F_GE} C_GE,
         {F_LE} C_LE,
         {F_C}  C_CS,
         {F_NC} C_CC,
         {F_A}  C_AT,
         {F_AE} C_AS,
         {F_B}  C_BT,
         {F_BE} C_LS);
      begin
        flags_to_cond := flags2cond[f];
      end;


    procedure convert_register_to_enum(var r:Tregister);

    begin
      if r.enum = R_INTREGISTER then
        case r.number of
          NR_NO: r.enum:= R_NO;
          NR_R0: r.enum:= R_R0;
          NR_R1: r.enum:= R_R1;
          NR_R2: r.enum:= R_R2;
          NR_R3: r.enum:= R_R3;
          NR_R4: r.enum:= R_R4;
          NR_R5: r.enum:= R_R5;
          NR_R6: r.enum:= R_R6;
          NR_R7: r.enum:= R_R7;
          NR_R8: r.enum:= R_R8;
          NR_R9: r.enum:= R_R9;
          NR_R10: r.enum:= R_R10;
          NR_R11: r.enum:= R_R11;
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
        else
          internalerror(200301231);
      end;
    end;


end.
