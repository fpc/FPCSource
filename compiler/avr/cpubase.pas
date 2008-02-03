{
    Copyright (c) 2006 by Florian Klaempfl

    Contains the base types for the AVR

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
      TAsmOp=(A_None,
        A_ADD,A_ADC,A_ADIW,A_SUB,A_SUBI,A_SBC,A_SBCI,A_SBIW,A_AND,A_ANDI,
        A_OR,A_ORI,A_EOR,A_COM,A_NEG,A_SBR,A_CBR,A_INC,A_DEC,A_TST,A_CLR,
        A_SER,A_MUL,A_MULS,A_FMUL,A_FMULS,A_FMULSU,A_RJMP,A_IJMP,
        A_EIJMP,A_JMP,A_RCALL,A_ICALL,R_EICALL,A_CALL,A_RET,A_RETI,A_CPSE,
        A_CP,A_CPC,A_CPI,A_SBxx,A_BRxx,A_MOV,A_MOVW,A_LDI,A_LDS,A_LD,A_LDD,
        A_STS,A_ST,A_STD,A_LPM,A_ELPM,A_SPM,A_IN,A_OUT,A_PUSH,A_POP,
        A_LSL,A_LSR,A_ROL,A_ROR,A_ASR,A_SWAP,A_BSET,A_BCLR,A_SBI,A_CBI,
        A_BST,A_BLD,A_Sxx,A_Cxx,A_BRAK,A_NOP,A_SLEEP,A_WDR);


      { This should define the array of instructions as string }
      op2strtable=array[tasmop] of string[11];

    const
      { First value of opcode enumeration }
      firstop = low(tasmop);
      { Last value of opcode enumeration  }
      lastop  = high(tasmop);

      jmp_instructions = [A_BRxx,A_SBxx,A_JMP,A_RCALL,A_ICALL,A_EIJMP,A_RJMP,A_CALL,A_RET,A_RETI,A_CPSE,A_IJMP];

{*****************************************************************************
                                  Registers
*****************************************************************************}

    type
      { Number of registers used for indexing in tables }
      tregisterindex=0..{$i ravrnor.inc}-1;

    const
      { Available Superregisters }
      {$i ravrsup.inc}

      { No Subregisters }
      R_SUBWHOLE = R_SUBNONE;

      { Available Registers }
      {$i ravrcon.inc}

      NR_XLO = NR_R26;
      NR_XHI = NR_R27;
      NR_YLO = NR_R28;
      NR_YHI = NR_R29;
      NR_ZLO = NR_R30;
      NR_ZHI = NR_R31;

      { Integer Super registers first and last }
      first_int_supreg = RS_R0;
      first_int_imreg = $10;

      { Float Super register first and last }
      first_fpu_supreg    = RS_INVALID;
      first_fpu_imreg     = RS_INVALID;

      { MM Super register first and last }
      first_mm_supreg    = RS_INVALID;
      first_mm_imreg     = RS_INVALID;

{$warning TODO Calculate bsstart}
      regnumber_count_bsstart = 64;

      regnumber_table : array[tregisterindex] of tregister = (
        {$i ravrnum.inc}
      );

      regstabs_table : array[tregisterindex] of shortint = (
        {$i ravrsta.inc}
      );

      regdwarf_table : array[tregisterindex] of shortint = (
        {$i ravrdwa.inc}
      );
      { registers which may be destroyed by calls }
      VOLATILE_INTREGISTERS = [RS_R18..RS_R27,RS_R30..RS_R31];
      VOLATILE_FPUREGISTERS = [];

    type
      totherregisterset = set of tregisterindex;

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

      {# Constant defining possibly all registers which might require saving }
      ALL_OTHERREGISTERS = [];

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

      maxintregs = 15;
      { to determine how many registers to use for regvars }
      maxintscratchregs = 3;
      usableregsint = [RS_R4..RS_R10];
      c_countusableregsint = 7;

      maxfpuregs = 0;
      fpuregs = [];
      usableregsfpu = [];
      c_countusableregsfpu = 0;

      mmregs = [];
      usableregsmm = [];
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
      firstsaveintreg = RS_R4;
      lastsaveintreg  = RS_R10;
      firstsavefpureg = RS_INVALID;
      lastsavefpureg  = RS_INVALID;
      firstsavemmreg  = RS_INVALID;
      lastsavemmreg   = RS_INVALID;

      maxvarregs = 7;
      varregs : Array [1..maxvarregs] of tsuperregister =
                (RS_R4,RS_R5,RS_R6,RS_R7,RS_R8,RS_R9,RS_R10);

      maxfpuvarregs = 1;
      fpuvarregs : Array [1..maxfpuvarregs] of tsuperregister =
                (RS_INVALID);

{*****************************************************************************
                          Default generic sizes
*****************************************************************************}

      { Defines the default address size for a processor, }
      OS_ADDR = OS_16;
      { the natural int size for a processor,             }
      OS_INT = OS_16;
      OS_SINT = OS_S16;
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

      NR_FPU_RESULT_REG = NR_NO;

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
      saved_standard_registers : array[0..6] of tsuperregister =
        (RS_R4,RS_R5,RS_R6,RS_R7,RS_R8,RS_R9,RS_R10);
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
    procedure inverse_flags(var f: TResFlags);
    function flags_to_cond(const f: TResFlags) : TAsmCond;
    function findreg_by_number(r:Tregister):tregisterindex;
    function std_regnum_search(const s:string):Tregister;
    function std_regname(r:Tregister):string;

    function inverse_cond(const c: TAsmCond): TAsmCond; {$ifdef USEINLINE}inline;{$endif USEINLINE}
    function conditions_equal(const c1, c2: TAsmCond): boolean; {$ifdef USEINLINE}inline;{$endif USEINLINE}

    function is_pc(const r : tregister) : boolean;

    function dwarf_reg(r:tregister):byte;
    function GetHigh(const r : TRegister) : TRegister;

  implementation

    uses
      rgBase,verbose;


    const
      std_regname_table : array[tregisterindex] of string[7] = (
        {$i ravrstd.inc}
      );

      regnumber_index : array[tregisterindex] of tregisterindex = (
        {$i ravrrni.inc}
      );

      std_regname_index : array[tregisterindex] of tregisterindex = (
        {$i ravrsri.inc}
      );


    function cgsize2subreg(s:Tcgsize):Tsubregister;
      begin
        cgsize2subreg:=R_SUBWHOLE;
      end;


    function reg_cgsize(const reg: tregister): tcgsize;
      const subreg2cgsize:array[Tsubregister] of Tcgsize =
            (OS_NO,OS_8,OS_8,OS_16,OS_32,OS_64,OS_NO,OS_NO,OS_NO,OS_NO,OS_NO,OS_NO);
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


    function inverse_cond(const c: TAsmCond): TAsmCond; {$ifdef USEINLINE}inline;{$endif USEINLINE}
      const
        inverse: array[TAsmCond] of TAsmCond=(C_None,
          C_NE,C_EQ,C_CC,C_CS,C_PL,C_MI,C_VC,C_VS,C_LS,C_HI,
          C_LT,C_GE,C_LE,C_GT,C_None,C_None
        );
      begin
        result := inverse[c];
      end;


    function conditions_equal(const c1, c2: TAsmCond): boolean; {$ifdef USEINLINE}inline;{$endif USEINLINE}
      begin
        result := c1 = c2;
      end;


    function rotl(d : dword;b : byte) : dword;
      begin
         result:=(d shr (32-b)) or (d shl b);
      end;


    function dwarf_reg(r:tregister):byte;
      begin
        result:=regdwarf_table[findreg_by_number(r)];
        if result=-1 then
          internalerror(200603251);
      end;


    function GetHigh(const r : TRegister) : TRegister;
      begin
        result:=TRegister(longint(r)+1)
      end;

end.
