{
    Copyright (c) 1998-2002 by Florian Klaempfl and Peter Vreman

    Contains the base types for MIPS

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
      TAsmOp=({$i opcode.inc});

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
                                Conditions
*****************************************************************************}

    type
      TAsmCond=(C_None,
        C_EQ, C_NE, C_LT, C_LE, C_GT, C_GE, C_LTU, C_LEU, C_GTU, C_GEU,
        C_FEQ,  {Equal}
        C_FNE, {Not Equal}
        C_FGT,  {Greater}
        C_FLT,  {Less}
        C_FGE, {Greater or Equal}
        C_FLE  {Less or Equal}

      );

    const
      cond2str : array[TAsmCond] of string[3]=('',
        'eq','ne','lt','le','gt','ge','ltu','leu','gtu','geu',
        'feq','fne','fgt','flt','fge','fle'
      );

{*****************************************************************************
                                 Constants
*****************************************************************************}

    const
      max_operands = 4;

      { Constant defining possibly all registers which might require saving }
      ALL_OTHERREGISTERS = [];

      general_superregisters = [RS_R0..RS_R31];

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

      maxintregs = 31;
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

      STK2_PTR = NR_R23;
      NR_GP = NR_R28;
      NR_SP = NR_R29;
      NR_S8 = NR_R30;
      NR_FP = NR_R30;
      NR_RA = NR_R31;

      RS_GP = RS_R28;
      RS_SP = RS_R29;
      RS_S8 = RS_R30;
      RS_FP = RS_R30;
      RS_RA = RS_R31;

      {# Stack pointer register }
      NR_STACK_POINTER_REG = NR_SP;
      RS_STACK_POINTER_REG = RS_SP;
      {# Frame pointer register }
      NR_FRAME_POINTER_REG = NR_FP;
      RS_FRAME_POINTER_REG = RS_FP;

      NR_RETURN_ADDRESS_REG = NR_R7;
      { the return_result_reg, is used inside the called function to store its return
      value when that is a scalar value otherwise a pointer to the address of the
      result is placed inside it }

      { Results are returned in this register (32-bit values) }
      NR_FUNCTION_RETURN_REG = NR_R2;
      RS_FUNCTION_RETURN_REG = RS_R2;
      { Low part of 64bit return value }
      NR_FUNCTION_RETURN64_LOW_REG = NR_R2;
      RS_FUNCTION_RETURN64_LOW_REG = RS_R2;
      { High part of 64bit return value }
      NR_FUNCTION_RETURN64_HIGH_REG = NR_R3;
      RS_FUNCTION_RETURN64_HIGH_REG = RS_R3;
      { The value returned from a function is available in this register }
      NR_FUNCTION_RESULT_REG = NR_R2;
      RS_FUNCTION_RESULT_REG = RS_R2;
      { The lowh part of 64bit value returned from a function }
      NR_FUNCTION_RESULT64_LOW_REG = NR_R2;
      RS_FUNCTION_RESULT64_LOW_REG = RS_R2;
      { The high part of 64bit value returned from a function }
      NR_FUNCTION_RESULT64_HIGH_REG = NR_R3;
      RS_FUNCTION_RESULT64_HIGH_REG = RS_R3;

      NR_FPU_RESULT_REG = NR_F0;
      NR_MM_RESULT_REG  = NR_NO;

      NR_TCR0 = NR_R15;
      NR_TCR1 = NR_R3;

      NR_TCR10 = NR_R20;
      NR_TCR11 = NR_R21;
      NR_TCR12 = NR_R18;
      NR_TCR13 = NR_R19;


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
      saved_standard_registers : array[0..0] of tsuperregister =
        (RS_NO);

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
    function cgsize2subreg(regtype: tregistertype; s:tcgsize):tsubregister;
    function is_calljmp(o:tasmop):boolean;
    function findreg_by_number(r:Tregister):tregisterindex;
    function std_regnum_search(const s:string):Tregister;
    function std_regname(r:Tregister):string;

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


    function cgsize2subreg(regtype: tregistertype; s:tcgsize):tsubregister;
      begin
        if s in [OS_64,OS_S64] then
          cgsize2subreg:=R_SUBQ
        else
          cgsize2subreg:=R_SUBWHOLE;
      end;


    function reg_cgsize(const reg: tregister): tcgsize;
      begin
        case getregtype(reg) of
          R_INTREGISTER :
            reg_cgsize:=OS_32;
          R_FPUREGISTER :
            begin
              if getsubreg(reg)=R_SUBFD then
                result:=OS_F64
              else
                result:=OS_F32;
            end;
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


    function inverse_cond(const c: TAsmCond): TAsmCond; {$ifdef USEINLINE}inline;{$endif USEINLINE}
      const
        inverse: array[TAsmCond] of TAsmCond=(C_None,
        C_EQ, C_NE, C_LT, C_LE, C_GT, C_GE, C_LTU, C_LEU, C_GTU, C_GEU,
        C_FEQ,  {Equal}
        C_FNE, {Not Equal}
        C_FGT,  {Greater}
        C_FLT,  {Less}
        C_FGE, {Greater or Equal}
        C_FLE  {Less or Equal}

        );
      begin
        result := inverse[c];
      end;      function findreg_by_number(r:Tregister):tregisterindex;
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


end.
