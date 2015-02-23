{
    Copyright (c) 1998-2012 by Florian Klaempfl and Peter Vreman

    Contains the base types for ARM64

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
{ Base unit for processor information. This unit contains
  enumerations of registers, opcodes, sizes, and other
  such things which are processor specific.
}
unit cpubase;

{$define USEINLINE}

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
      TAsmOp= {$i a64op.inc}

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
      tregisterindex=0..{$i ra64nor.inc}-1;

    const
      { Available Superregisters }
      {$i ra64sup.inc}

      R_SUBWHOLE = R_SUBQ;

      { Available Registers }
      {$i ra64con.inc}

      { Integer Super registers first and last }
      first_int_supreg = RS_X0;
      { xzr and sp take up a separate super register because some instructions
        are ambiguous otherwise }
      first_int_imreg = $21;

      { Integer Super registers first and last }
      first_fpu_supreg = RS_S0;
      first_fpu_imreg = $20;

      { MM Super register first and last }
      first_mm_supreg    = RS_S0;
      first_mm_imreg     = $20;

      { Required parameter alignment when calling a routine declared as
        stdcall and cdecl. The alignment value should be the one defined
        by GCC or the target ABI.

        The value of this constant is equal to the constant
        PARM_BOUNDARY / BITS_PER_UNIT in the GCC source.
      }
      std_param_align = 4;

      { TODO: Calculate bsstart}
      regnumber_count_bsstart = 128;

      regnumber_table : array[tregisterindex] of tregister = (
        {$i ra64num.inc}
      );

      regstabs_table : array[tregisterindex] of shortint = (
        {$i ra64sta.inc}
      );

      regdwarf_table : array[tregisterindex] of shortint = (
        {$i ra64dwa.inc}
      );
      { registers which may be destroyed by calls }
      VOLATILE_INTREGISTERS = [RS_X0..RS_X18,RS_X30];
      VOLATILE_MMREGISTERS =  [RS_D0..RS_D7,RS_D16..RS_D31];

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
        { update condition flags }
        PF_S,
        { load/store sizes }
        PF_B,PF_SB,PF_H,PF_SH,PF_W,PF_SW
      );

      TOpPostfixes = set of TOpPostfix;

    const
      oppostfix2str: array[TOpPostfix] of string[2] = ('',
        's',
        'b','sb','h','sh','w','sw');

{*****************************************************************************
                                Conditions
*****************************************************************************}

    type
      TAsmCond=(C_None,
        C_EQ,C_NE,C_HS,C_LO,C_MI,C_PL,C_VS,C_VC,C_HI,C_LS,
        C_GE,C_LT,C_GT,C_LE,C_AL,C_NV
      );

      TAsmConds = set of TAsmCond;

    const
      cond2str : array[TAsmCond] of string[2]=('',
        'eq','ne','hs','lo','mi','pl','vs','vc','hi','ls',
        'ge','lt','gt','le','al','nv'
      );

      uppercond2str : array[TAsmCond] of string[2]=('',
        'EQ','NE','hs','LO','MI','PL','VS','VC','HI','LS',
        'GE','LT','GT','LE','AL','NV'
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
      tshiftmode = (SM_None,SM_LSL,SM_LSR,SM_ASR);

      tupdatereg = (UR_None,UR_Update);

      pshifterop = ^tshifterop;

      tshifterop = record
        shiftmode : tshiftmode;
        shiftimm : byte;
      end;

      tspecialregflag = (srC, srX, srS, srF);
      tspecialregflags = set of tspecialregflag;

{*****************************************************************************
                                 Constants
*****************************************************************************}

    const
      max_operands = 6;

      maxintregs = 32;
      maxfpuregs = 32;
      maxaddrregs = 0;

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
                          Default generic sizes
*****************************************************************************}

   const
      { Defines the default address size for a processor, }
      OS_ADDR = OS_64;
      { the natural int size for a processor,
        has to match osuinttype/ossinttype as initialized in psystem }
      OS_INT = OS_64;
      OS_SINT = OS_S64;
      { the maximum float size for a processor,           }
      OS_FLOAT = OS_F64;
      { the size of a vector register for a processor     }
      OS_VECTOR = OS_M128;

{*****************************************************************************
                          Generic Register names
*****************************************************************************}


      { Stack pointer register }
      NR_STACK_POINTER_REG = NR_SP;
      RS_STACK_POINTER_REG = RS_SP;
      { Frame pointer register }
      NR_FRAME_POINTER_REG = NR_X29;
      RS_FRAME_POINTER_REG = RS_X29;
      { Register for addressing absolute data in a position independant way,
        such as in PIC code. The exact meaning is ABI specific. For
        further information look at GCC source : PIC_OFFSET_TABLE_REGNUM
      }
      NR_PIC_OFFSET_REG = NR_X18;
      { Results are returned in this register (32-bit values) }
      NR_FUNCTION_RETURN_REG = NR_X0;
      RS_FUNCTION_RETURN_REG = RS_X0;
      { The value returned from a function is available in this register }
      NR_FUNCTION_RESULT_REG = NR_FUNCTION_RETURN_REG;
      RS_FUNCTION_RESULT_REG = RS_FUNCTION_RETURN_REG;

      NR_FPU_RESULT_REG = NR_NO;

      NR_MM_RESULT_REG  = NR_D0;

      NR_RETURN_ADDRESS_REG = NR_FUNCTION_RETURN_REG;

      { Offset where the parent framepointer is pushed }
      PARENT_FRAMEPOINTER_OFFSET = 0;

      NR_DEFAULTFLAGS = NR_NZCV;
      RS_DEFAULTFLAGS = RS_NZCV;

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
      saved_standard_registers : array[0..9] of tsuperregister =
        (RS_X19,RS_X20,RS_X21,RS_X22,RS_X23,RS_X24,RS_X25,RS_X26,RS_X27,RS_X28);
      saved_mm_registers : array[0..7] of tsuperregister = (RS_D8,RS_D9,RS_D10,RS_D11,RS_D12,RS_D13,RS_D14,RS_D15);

      { this is only for the generic code which is not used for this architecture }
      saved_address_registers : array[0..0] of tsuperregister = (RS_INVALID);

{*****************************************************************************
                                  Helpers
*****************************************************************************}

    { Returns the tcgsize corresponding with the size of reg.}
    function reg_cgsize(const reg: tregister) : tcgsize;
    function cgsize2subreg(regtype: tregistertype; s:Tcgsize):Tsubregister;
    function is_calljmp(o:tasmop):boolean;{$ifdef USEINLINE}inline;{$endif USEINLINE}
    procedure inverse_flags(var f: TResFlags);
    function flags_to_cond(const f: TResFlags) : TAsmCond;
    function findreg_by_number(r:Tregister):tregisterindex;
    function std_regnum_search(const s:string):Tregister;
    function std_regname(r:Tregister):string;

    function inverse_cond(const c: TAsmCond): TAsmCond; {$ifdef USEINLINE}inline;{$endif USEINLINE}
    function conditions_equal(const c1, c2: TAsmCond): boolean; {$ifdef USEINLINE}inline;{$endif USEINLINE}

    procedure shifterop_reset(var so : tshifterop); {$ifdef USEINLINE}inline;{$endif USEINLINE}

    function dwarf_reg(r:tregister):shortint;

  implementation

    uses
      systems,rgBase,verbose;

    const
      std_regname_table : TRegNameTable = (
        {$i ra64std.inc}
      );

      regnumber_index : array[tregisterindex] of tregisterindex = (
        {$i ra64rni.inc}
      );

      std_regname_index : array[tregisterindex] of tregisterindex = (
        {$i ra64sri.inc}
      );


    function cgsize2subreg(regtype: tregistertype; s:Tcgsize):Tsubregister;
      begin
        case regtype of
          R_INTREGISTER:
            begin
              case s of
                { there's only Wn and Xn }
                OS_32,
                OS_S32:
                  cgsize2subreg:=R_SUBD;
                else
                  cgsize2subreg:=R_SUBWHOLE;
                end;
            end;
          R_MMREGISTER:
            begin
              case s of
                OS_F32:
                  cgsize2subreg:=R_SUBFS;
                OS_F64:
                  cgsize2subreg:=R_SUBFD;
                else
                  internalerror(2009112701);
              end;
            end;
          else
            cgsize2subreg:=R_SUBWHOLE;
        end;
      end;


    function reg_cgsize(const reg: tregister): tcgsize;
      begin
        case getregtype(reg) of
          R_INTREGISTER:
            case getsubreg(reg) of
              R_SUBD:
                result:=OS_32
              else
                result:=OS_64;
            end;
          R_MMREGISTER :
            begin
              case getsubreg(reg) of
                R_SUBFD,
                R_SUBWHOLE:
                  result:=OS_F64;
                R_SUBFS:
                  result:=OS_F32;
                else
                  internalerror(2009112903);
              end;
            end;
          else
            internalerror(200303181);
          end;
        end;


    function is_calljmp(o:tasmop):boolean;{$ifdef USEINLINE}inline;{$endif USEINLINE}
      begin
        is_calljmp:= o in [A_B,A_BLR,A_RET];
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
          (C_EQ,C_NE,C_HI,C_LO,C_MI,C_PL,C_VS,C_VC,C_HI,C_LS,
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


    procedure shifterop_reset(var so : tshifterop);{$ifdef USEINLINE}inline;{$endif USEINLINE}
      begin
        FillChar(so,sizeof(so),0);
      end;


    function inverse_cond(const c: TAsmCond): TAsmCond; {$ifdef USEINLINE}inline;{$endif USEINLINE}
      const
        inverse: array[TAsmCond] of TAsmCond=(C_None,
          C_NE,C_EQ,C_LO,C_HI,C_PL,C_MI,C_VC,C_VS,C_LS,C_HI,
          C_LT,C_GE,C_LE,C_GT,C_None,C_None
        );
      begin
        result := inverse[c];
      end;


    function conditions_equal(const c1, c2: TAsmCond): boolean; {$ifdef USEINLINE}inline;{$endif USEINLINE}
      begin
        result := c1 = c2;
      end;


    function dwarf_reg(r:tregister):shortint;
      begin
        result:=regdwarf_table[findreg_by_number(r)];
        if result=-1 then
          internalerror(200603251);
      end;


end.
