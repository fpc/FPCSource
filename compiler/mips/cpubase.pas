{
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
      globtype,globals,
      cpuinfo,
      aasmbase,
      cgbase
      ;


{*****************************************************************************
                                Assembler Opcodes
*****************************************************************************}

    type
      TAsmOp=(A_ABS_D,A_ABS_S,A_ADD,A_ADD_D,A_ADD_S,A_ADDI,A_ADDIU,A_ADDU,
              A_AND,A_ANDI,A_BC1F,A_BC1FL,A_BC1T,A_BC1TL,A_BC2F,A_BC2FL,
              A_BC2T,A_BC2TL,A_BEQ,A_BEQL,A_BGEZ,A_BGEZAL,A_BGEZALL,A_BGEZL,
              A_BGTZ,A_BGTZL,A_BLEZ,A_BLEZL,A_BLTZ,A_BLTZAL,A_BLTZALL,A_BLTZL,
              A_BNE,A_BNEL,A_BREAK,A_C_cond_D,A_C_cond_S,A_CACHE,A_CEIL_W_D,A_CEIL_W_S,
              A_CFC1,A_CFC2,A_CLO,A_CLZ,A_COP2,A_CTC1,A_CTC2,A_CVT_D_S,
              A_CVT_D_W,A_CVT_S_D,A_CVT_S_W,A_CVT_W_D,A_CVT_W_S,A_DIV,A_DIV_D,A_DIV_S,
              A_DIVU,A_ERET,A_FLOOR_W_D,A_FLOOR_W_S,A_J,A_JAL,A_JALR,A_JR,
              A_LB,A_LBU,A_LDC1,A_LDC2,A_LH,A_LHU,A_LL,A_LUI,
              A_LW,A_LWC1,A_LWC2,A_LWL,A_LWR,A_MADD,A_MADDU,A_MFC0,
              A_MFC1,A_MFC2,A_MFHI,A_MFLO,A_MOV_D,A_MOV_S,A_MOVF,A_MOVF_D,
              A_MOVF_S,A_MOVN,A_MOVN_D,A_MOVN_S,A_MOVT,A_MOVT_D,A_MOVT_S,A_MOVZ,
              A_MOVZ_D,A_MOVZ_S,A_MSUB,A_MSUBU,A_MTC0,A_MTC1,A_MTC2,A_MTHI,
              A_MTLO,A_MUL,A_MUL_D,A_MUL_S,A_MULT,A_MULTU,A_NEG_D,A_NEG_S,
              A_NOR,A_OR,A_ORI,A_PREF,A_ROUND_W_D,A_ROUND_W_S,A_SB,A_SC,
              A_SDC1,A_SDC2,A_SH,A_SLL,A_SLLV,A_SLT,A_SLTI,A_SLTIU,
              A_SLTU,A_SQRT_D,A_SQRT_S,A_SRA,A_SRAV,A_SRL,A_SRLV,A_SSNOP,
              A_SUB,A_SUB_D,A_SUB_S,A_SUBU,A_SW,A_SWC1,A_SWC2,A_SWL,
              A_SWR,A_SYNC,A_SYSCALL,A_TEQ,A_TEQI,A_TGE,A_TGEI,A_TGEIU,
              A_TGEU,A_TLBP,A_TLBR,A_TLBWI,A_TLBWR,A_TLT,A_TLTI,A_TLTIU,
              A_TLTU,A_TNE,A_TNEI,A_TRUNC_W_D,A_TRUNC_W_S,A_WAIT,A_XOR,A_XORI
             );

      { This should define the array of instructions as string }
      op2strtable=array[tasmop] of string[11];

    const
      { First value of opcode enumeration }
      firstop = low(tasmop);
      { Last value of opcode enumeration  }
      lastop  = high(tasmop);

{*****************************************************************************
                                  Registers
*****************************************************************************}

    type
      { Number of registers used for indexing in tables }
      tregisterindex=0..{$i rmipsnor.inc}-1;

    const
      { Available Superregisters }
      {$i rmipssup.inc}

      { No Subregisters }
      R_SUBWHOLE = R_SUBNONE;

      { Available Registers }
      {$i rmipscon.inc}

      { Integer Super registers first and last }
      first_int_supreg = RS_R0;
      first_int_imreg = $10;

      { Float Super register first and last }
      first_fpu_supreg    = RS_F0;
      first_fpu_imreg     = $08;

      { MM Super register first and last }
      first_mm_supreg    = RS_NO;
      first_mm_imreg     = RS_NO;

{ TODO: Calculate bsstart}
      regnumber_count_bsstart = 64;

      regnumber_table : array[tregisterindex] of tregister = (
        {$i rmipsnum.inc}
      );

      regstabs_table : array[tregisterindex] of shortint = (
        {$i rmipssta.inc}
      );

      regdwarf_table : array[tregisterindex] of shortint = (
        {$i rmipsdwf.inc}
      );
      { registers which may be destroyed by calls }
      VOLATILE_INTREGISTERS = [RS_R0..RS_R3,RS_R12..RS_R15];
      VOLATILE_FPUREGISTERS = [RS_F0..RS_F3];

    type
      totherregisterset = set of tregisterindex;

{*****************************************************************************
                          Instruction post fixes
*****************************************************************************}
    type
      { ARM instructions load/store and arithmetic instructions
        can have several instruction post fixes which are collected
        in this enumeration
      }
      TOpPostfix = (PF_None,
        { update condition flags
          or floating point single }
        PF_S,
        { floating point size }
        PF_D,PF_E,PF_P,PF_EP,
        { load/store }
        PF_B,PF_SB,PF_BT,PF_H,PF_SH,PF_T,
        { multiple load/store address modes }
        PF_IA,PF_IB,PF_DA,PF_DB,PF_FD,PF_FA,PF_ED,PF_EA
      );

      TRoundingMode = (RM_None,RM_P,RM_M,RM_Z);

    const
      cgsize2fpuoppostfix : array[OS_NO..OS_F128] of toppostfix = (
        PF_E,
        PF_None,PF_None,PF_None,PF_None,PF_None,PF_None,PF_None,PF_None,PF_None,PF_None,
        PF_S,PF_D,PF_E,PF_None,PF_None);

      oppostfix2str : array[TOpPostfix] of string[2] = ('',
        's',
        'd','e','p','ep',
        'b','sb','bt','h','sh','t',
        'ia','ib','da','db','fd','fa','ed','ea');

      roundingmode2str : array[TRoundingMode] of string[1] = ('',
        'p','m','z');

{*****************************************************************************
                                Conditions
*****************************************************************************}

    type
      TAsmCond=(C_None,
        C_EQ,C_NE,C_CS,C_CC,C_MI,C_PL,C_VS,C_VC,C_HI,C_LS,
        C_GE,C_LT,C_GT,C_LE,C_AL,C_NV
      );

    const
      cond2str : array[TAsmCond] of string[2]=('',
        'eq','ne','cs','cc','mi','pl','vs','vc','hi','ls',
        'ge','lt','gt','le','al','nv'
      );

      uppercond2str : array[TAsmCond] of string[2]=('',
        'EQ','NE','CS','CC','MI','PL','VS','VC','HI','LS',
        'GE','LT','GT','LE','AL','NV'
      );

      inverse_cond : array[TAsmCond] of TAsmCond=(C_None,
        C_NE,C_EQ,C_CC,C_CS,C_PL,C_MI,C_VC,C_VS,C_LS,C_HI,
        C_LT,C_GE,C_LE,C_GT,C_None,C_None
      );

{*****************************************************************************
                                   Flags
*****************************************************************************}

    type
      TResFlags = (F_EQ,F_NE,F_CS,F_CC,F_MI,F_PL,F_VS,F_VC,F_HI,F_LS,
        F_GE,F_LT,F_GT,F_LE);

{*****************************************************************************
                                Operands
*****************************************************************************}

      taddressmode = (AM_OFFSET,AM_PREINDEXED,AM_POSTINDEXED);
      tshiftmode = (SM_None,SM_LSL,SM_LSR,SM_ASR,SM_ROR,SM_RRX);

      tupdatereg = (UR_None,UR_Update);

      pshifterop = ^tshifterop;

      tshifterop = record
        shiftmode : tshiftmode;
        rs : tregister;
        shiftimm : byte;
      end;

{*****************************************************************************
                                 Constants
*****************************************************************************}

    const
      max_operands = 4;

      { Constant defining possibly all registers which might require saving }
      ALL_OTHERREGISTERS = [];

      general_superregisters = [RS_R0..RS_PC];

      { Table of registers which can be allocated by the code generator
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

      maxintregs = 15;
      { to determine how many registers to use for regvars }
      maxintscratchregs = 3;
      usableregsint = [RS_R4..RS_R10];
      c_countusableregsint = 7;

      maxfpuregs = 8;
      fpuregs = [RS_F0..RS_F7];
      usableregsfpu = [RS_F4..RS_F7];
      c_countusableregsfpu = 4;

      mmregs = [RS_NO..RS_NO];
      usableregsmm = [RS_NO..RS_NO];
      c_countusableregsmm  = 0;

      maxaddrregs = 0;
      addrregs    = [];
      usableregsaddr = [];
      c_countusableregsaddr = 0;

{*****************************************************************************
                                Operand Sizes
*****************************************************************************}

    type
      topsize = (S_NO,
        S_B,S_W,S_L,S_BW,S_BL,S_WL,
        S_IS,S_IL,S_IQ,
        S_FS,S_FL,S_FX,S_D,S_Q,S_FV,S_FXX
      );

{*****************************************************************************
                                 Constants
*****************************************************************************}

    const
      maxvarregs = 7;
      varregs : Array [1..maxvarregs] of tsuperregister =
                (RS_R4,RS_R5,RS_R6,RS_R7,RS_R8,RS_R9,RS_R10);

      maxfpuvarregs = 4;
      fpuvarregs : Array [1..maxfpuvarregs] of tsuperregister =
                (RS_F4,RS_F5,RS_F6,RS_F7);

{*****************************************************************************
                          Default generic sizes
*****************************************************************************}

      { Defines the default address size for a processor, }
      OS_ADDR = OS_32;
      { the natural int size for a processor,             }
      OS_INT = OS_32;
      OS_SINT = OS_S32;
      { the maximum float size for a processor,           }
      OS_FLOAT = OS_F64;
      { the size of a vector register for a processor     }
      OS_VECTOR = OS_M32;

{*****************************************************************************
                          Generic Register names
*****************************************************************************}

      { Stack pointer register }
      NR_STACK_POINTER_REG = NR_R13;
      RS_STACK_POINTER_REG = RS_R13;
      { Frame pointer register }
      RS_FRAME_POINTER_REG = RS_R11;
      NR_FRAME_POINTER_REG = NR_R11;
      { Register for addressing absolute data in a position independant way,
        such as in PIC code. The exact meaning is ABI specific. For
        further information look at GCC source : PIC_OFFSET_TABLE_REGNUM
      }
      NR_PIC_OFFSET_REG = NR_R9;
      { Results are returned in this register (32-bit values) }
      NR_FUNCTION_RETURN_REG = NR_R0;
      RS_FUNCTION_RETURN_REG = RS_R0;
      { Low part of 64bit return value }
      NR_FUNCTION_RETURN64_LOW_REG = NR_R0;
      RS_FUNCTION_RETURN64_LOW_REG = RS_R0;
      { High part of 64bit return value }
      NR_FUNCTION_RETURN64_HIGH_REG = NR_R1;
      RS_FUNCTION_RETURN64_HIGH_REG = RS_R1;
      { The value returned from a function is available in this register }
      NR_FUNCTION_RESULT_REG = NR_FUNCTION_RETURN_REG;
      RS_FUNCTION_RESULT_REG = RS_FUNCTION_RETURN_REG;
      { The lowh part of 64bit value returned from a function }
      NR_FUNCTION_RESULT64_LOW_REG = NR_FUNCTION_RETURN64_LOW_REG;
      RS_FUNCTION_RESULT64_LOW_REG = RS_FUNCTION_RETURN64_LOW_REG;
      { The high part of 64bit value returned from a function }
      NR_FUNCTION_RESULT64_HIGH_REG = NR_FUNCTION_RETURN64_HIGH_REG;
      RS_FUNCTION_RESULT64_HIGH_REG = RS_FUNCTION_RETURN64_HIGH_REG;

      NR_FPU_RESULT_REG = NR_F0;

      NR_MM_RESULT_REG  = NR_NO;

      NR_RETURN_ADDRESS_REG = NR_FUNCTION_RETURN_REG;

      { Offset where the parent framepointer is pushed }
      PARENT_FRAMEPOINTER_OFFSET = 0;

{*****************************************************************************
                       GCC /ABI linking information
*****************************************************************************}

    const
      { Registers which must be saved when calling a routine declared as
        cppdecl, cdecl, stdcall, safecall, palmossyscall. The registers
        saved should be the ones as defined in the target ABI and / or GCC.

        This value can be deduced from the CALLED_USED_REGISTERS array in the
        GCC source.
      }
      saved_standard_registers : array[0..8] of tsuperregister =
        (RS_R16,RS_R17,RS_R18,RS_R19,RS_R20,RS_R21,RS_R22,RS_R23,RS_R30);
        
      { this is only for the generic code which is not used for this architecture }
      saved_mm_registers : array[0..0] of tsuperregister = (RS_NO);
      
      { Required parameter alignment when calling a routine declared as
        stdcall and cdecl. The alignment value should be the one defined
        by GCC or the target ABI.

        The value of this constant is equal to the constant
        PARM_BOUNDARY / BITS_PER_UNIT in the GCC source.
      }
      std_param_align = 4;


{*****************************************************************************
                                  Helpers
*****************************************************************************}

    { Returns the tcgsize corresponding with the size of reg.}
    function reg_cgsize(const reg: tregister) : tcgsize;
    function cgsize2subreg(s:Tcgsize):Tsubregister;
    function is_calljmp(o:tasmop):boolean;
    procedure inverse_flags(var f: TResFlags);
    function flags_to_cond(const f: TResFlags) : TAsmCond;
    function findreg_by_number(r:Tregister):tregisterindex;
    function std_regnum_search(const s:string):Tregister;
    function std_regname(r:Tregister):string;

    procedure shifterop_reset(var so : tshifterop);
    function is_pc(const r : tregister) : boolean;

  implementation

    uses
      rgBase,verbose;


    const
      std_regname_table : array[tregisterindex] of string[7] = (
        {$i rmipsstd.inc}
      );

      regnumber_index : array[tregisterindex] of tregisterindex = (
        {$i rmipsrni.inc}
      );

      std_regname_index : array[tregisterindex] of tregisterindex = (
        {$i rmipssri.inc}
      );


    function cgsize2subreg(s:Tcgsize):Tsubregister;
      begin
        cgsize2subreg:=R_SUBWHOLE;
      end;


    function reg_cgsize(const reg: tregister): tcgsize;
      const subreg2cgsize:array[Tsubregister] of Tcgsize =
            (OS_NO,OS_8,OS_8,OS_16,OS_32,OS_64,OS_NO,OS_NO,OS_NO);
      begin
        case getregtype(reg) of
          R_INTREGISTER :
            reg_cgsize:=OS_32;
          R_FPUREGISTER :
            reg_cgsize:=OS_F80;
          else
            internalerror(200303181);
          end;
        end;


    function is_calljmp(o:tasmop):boolean;
      begin
        { This isn't 100% perfect because the arm allows jumps also by writing to PC=R15.
          To overcome this problem we simply forbid that FPC generates jumps by loading R15 }
        is_calljmp:= o in [A_J,A_JAL,A_JALR,{ A_JALX, }A_JR,
          A_BEQ,A_BNE,A_BGEZ,A_BGEZAL,A_BGTZ,A_BLEZ,A_BLTZ,A_BLTZAL,
          A_BEQL,A_BGEZALL,A_BGEZL,A_BGTZL,A_BLEZL,A_BLTZALL,A_BLTZL,A_BNEL];
      end;


    procedure inverse_flags(var f: TResFlags);
      const
        inv_flags: array[TResFlags] of TResFlags =
          (F_NE,F_EQ,F_CC,F_CS,F_PL,F_MI,F_VC,F_VS,F_LS,F_HI,
          F_LT,F_GE,F_LE,F_GT);
      begin
        f:=inv_flags[f];
      end;


    function flags_to_cond(const f: TResFlags) : TAsmCond;
      const
        flag_2_cond: array[F_EQ..F_LE] of TAsmCond =
          (C_EQ,C_NE,C_CS,C_CC,C_MI,C_PL,C_VS,C_VC,C_HI,C_LS,
           C_GE,C_LT,C_GT,C_LE);
      begin
        if f>high(flag_2_cond) then
          internalerror(200112301);
        result:=flag_2_cond[f];
      end;


    function findreg_by_number(r:Tregister):tregisterindex;
      begin
        result:=rgBase.findreg_by_number_table(r,regnumber_index);
      end;


    function std_regnum_search(const s:string):Tregister;
      begin
        result:=regnumber_table[findreg_by_name_table(s,std_regname_table,std_regname_index)];
      end;


    function std_regname(r:Tregister):string;
      var
        p : tregisterindex;
      begin
        p:=findreg_by_number_table(r,regnumber_index);
        if p<>0 then
          result:=std_regname_table[p]
        else
          result:=generic_regname(r);
      end;


    procedure shifterop_reset(var so : tshifterop);
      begin
        FillChar(so,sizeof(so),0);
      end;


    function is_pc(const r : tregister) : boolean;
      begin
        is_pc:=(r=NR_R15);
      end;

end.
