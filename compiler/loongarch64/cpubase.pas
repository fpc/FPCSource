{
    Copyright (C) 2022 Loongson Technology Corporation Limited.

    Contains the base types for the LoongArch64.

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
unit cpubase;

{$i fpcdefs.inc}

interface

uses
  strings,globtype,
  cutils,cclasses,aasmbase,cpuinfo,cgbase;

{*****************************************************************************
                                Assembler Opcodes
*****************************************************************************}

    type
      TAsmOp= {$i loongarch64op.inc}

      {# This should define the array of instructions as string }
      op2strtable=array[tasmop] of string[16];

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
      tregisterindex=0..{$i rloongarch64nor.inc}-1;

    const
      maxvarregs = 32-6; { 32 int registers - r0 - stackpointer - r2 - 3 scratch registers }
      maxfpuvarregs = 28; { 32 fpuregisters - some scratch registers (minimally 2) }

      { Available Superregisters }
      {$i rloongarch64sup.inc}

      { No Subregisters }
      R_SUBWHOLE=R_SUBNONE;

      { Available Registers }
      {$i rloongarch64con.inc}

      { Integer Super registers first and last }
      first_int_supreg = RS_R0;
      first_int_imreg = $20;

      { Float Super register first and last }
      first_fpu_supreg    = RS_F0;
      first_fpu_imreg     = $20;

      { MM Super register first and last }
      first_mm_supreg    = 0;
      first_mm_imreg     = 1;

{ TODO: Calculate bsstart}
      regnumber_count_bsstart = 64;

      regnumber_table : array[tregisterindex] of tregister = (
        {$i rloongarch64num.inc}
      );

      regstabs_table : array[tregisterindex] of shortint = (
        {$i rloongarch64sta.inc}
      );

      regdwarf_table : array[tregisterindex] of shortint = (
        {$i rloongarch64dwa.inc}
      );

{*****************************************************************************
                                Conditions
*****************************************************************************}

    type
      TAsmCond = (C_NONE,C_EQ,C_GT,C_LT,C_GE,C_LE,C_NE,C_LEU,C_LTU,C_GEU,C_GTU,
                  C_EQZ,C_GTZ,C_LTZ,C_GEZ,C_LEZ,C_NEZ);
      TAsmConds = set of TAsmCond;
    const
      cond2str: Array[TAsmCond] of string[4] = ('',
                'eq','gt','lt','ge','le','ne','leu','ltu','geu','gtu',
                'eqz','gtz','ltz','gez','lez','nez');


{*****************************************************************************
                                   Flags
*****************************************************************************}

    type
      TResFlagsEnum = (F_EQ,F_NE,F_LT,F_LTU,F_GE,F_GEU);

{*****************************************************************************
                                Reference
*****************************************************************************}

{*****************************************************************************
                                Operands
*****************************************************************************}

{*****************************************************************************
                                 Constants
*****************************************************************************}

    const
      max_operands = 5;

{*****************************************************************************
                          Default generic sizes
*****************************************************************************}

  {# Defines the default address size for a processor, }
  OS_ADDR = OS_64;
  {# the natural int size for a processor,
     has to match osuinttype/ossinttype as initialized in psystem }
  OS_INT = OS_64;
  OS_SINT = OS_S64;
  {# the maximum float size for a processor,           }
  OS_FLOAT = OS_F64;
  {# the size of a vector register for a processor     }
  OS_VECTOR = OS_M128;

{*****************************************************************************
                               GDB Information
*****************************************************************************}

      stab_regindex : array[tregisterindex] of shortint = (
        {$i rloongarch64sta.inc}
      );

{*****************************************************************************
                          Generic Register names
*****************************************************************************}

      {# Stack pointer register }
      NR_STACK_POINTER_REG = NR_R3;
      RS_STACK_POINTER_REG = RS_R3;
      {# Frame pointer register }
      NR_FRAME_POINTER_REG = NR_R22;
      RS_FRAME_POINTER_REG = RS_R22;

      { Return address of a function }
      NR_RETURN_ADDRESS_REG = NR_R1;
      RS_RETURN_ADDRESS_REG = RS_R1;
      { Results are returned in this register (32-bit values) }
      NR_FUNCTION_RETURN_REG = NR_R4;
      RS_FUNCTION_RETURN_REG = RS_R4;
      { Low part of 64bit return value }
      NR_FUNCTION_RETURN64_LOW_REG = NR_R4;
      RS_FUNCTION_RETURN64_LOW_REG = RS_R4;
      { High part of 64bit return value }
      NR_FUNCTION_RETURN64_HIGH_REG = NR_R4;
      RS_FUNCTION_RETURN64_HIGH_REG = RS_R4;
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
      NR_MM_RESULT_REG = NR_NO;

      NR_DEFAULTFLAGS = NR_NO;
      RS_DEFAULTFLAGS = RS_NO;

      RS_FIRST_INT_PARAM_SUPREG = RS_R4;
      RS_FIRST_FLOAT_PARAM_SUPREG = RS_F0;
      RS_FIRST_MM_PARAM_SUPREG = RS_NO;


{*****************************************************************************
                       GCC /ABI linking information
*****************************************************************************}

      {# Registers which must be saved when calling a routine declared as
         cppdecl, cdecl, stdcall, safecall, palmossyscall. The registers
         saved should be the ones as defined in the target ABI and / or GCC.

         This value can be deduced from CALLED_USED_REGISTERS array in the
         GCC source.
      }
      saved_standard_registers : array[0..10] of tsuperregister = (
        RS_R3, RS_R22,
        RS_R23, RS_R24, RS_R25, RS_R26, RS_R27, RS_R28, RS_R29, RS_R30, RS_R31
      );

      { this is only for the generic code which is not used for this architecture }
      saved_address_registers : array[0..0] of tsuperregister = (RS_INVALID);
      saved_mm_registers : array[0..0] of tsuperregister = (RS_INVALID);

      {# Required parameter alignment when calling a routine declared as
         stdcall and cdecl. The alignment value should be the one defined
         by GCC or the target ABI.

         The value of this constant is equal to the constant
         PARM_BOUNDARY / BITS_PER_UNIT in the GCC source.
      }
      std_param_align = 8;

{*****************************************************************************
                            CPU Dependent Constants
*****************************************************************************}

      maxfpuregs = 8;

{*****************************************************************************
                                  Helpers
*****************************************************************************}

    function is_simm12(value: tcgint): boolean;
    function is_uimm12(value: tcgint): boolean;
    function is_simm16_and_quadruple(value: tcgint): boolean;

    function is_calljmp(o:tasmop):boolean;

    function cgsize2subreg(regtype: tregistertype; s:Tcgsize):Tsubregister;
    { Returns the tcgsize corresponding with the size of reg.}
    function reg_cgsize(const reg: tregister) : tcgsize;

    function findreg_by_number(r:Tregister):tregisterindex;
    function std_regnum_search(const s:string):Tregister;
    function std_regname(r:Tregister):string;

    function inverse_cond(const c: TAsmCond): Tasmcond; {$ifdef USEINLINE}inline;{$endif USEINLINE}
    function dwarf_reg(r:tregister):shortint;
    function dwarf_reg_no_error(r:tregister):shortint;
    function eh_return_data_regno(nr: longint): longint;

    function conditions_equal(const c1,c2: TAsmCond): boolean;

    { Checks if Subset is a subset of c (e.g. "less than" is a subset of "less than or equal" }
    function condition_in(const Subset, c: TAsmCond): Boolean;

    function is_extra_reg(const s : string) : tregister;

implementation

    uses
      rgbase,verbose;

    const
      std_regname_table : TRegNameTable = (
        {$i rloongarch64std.inc}
      );

      abi_regname_table : TRegNameTable = (
        {$i rloongarch64abi.inc}
      );

      regnumber_index : array[tregisterindex] of tregisterindex = (
        {$i rloongarch64rni.inc}
      );

      std_regname_index : array[tregisterindex] of tregisterindex = (
        {$i rloongarch64sri.inc}
      );

{*****************************************************************************
                                  Helpers
*****************************************************************************}

    function is_simm12(value: tcgint): boolean;
      begin
        result:=(value >= -2048) and (value <= 2047);
      end;

    function is_uimm12(value: tcgint): boolean;
      begin
        result:=(value >= 0) and (value <= 4095);
      end;

    function is_simm16_and_quadruple(value: tcgint): boolean;
      begin
        result:=(value >= -32768) and (value <= 32767) and ((value mod 4) = 0);
      end;

    function is_calljmp(o:tasmop):boolean;
      begin
        case o of
          { Most of time is call. }
          A_JIRL,A_BL: result:=true;
          { Most of time is jump. }
          A_JR,A_B,A_BEQ,A_BNE,A_BLT,A_BLTU,A_BGE,
          A_BGEU,A_BEQZ,A_BNEZ,A_BCEQZ,A_BCNEZ,
          A_BLTZ,A_BGTZ,A_BGEZ,A_BLEZ,A_BGT,A_BLE,
          A_BGTU,A_BLEU,A_BXX: result:=true;
        else
          result:=false;
        end;
      end;

    function inverse_cond(const c: TAsmCond): Tasmcond; {$ifdef USEINLINE}inline;{$endif USEINLINE}
      const
        inv_condflags:array[TAsmCond] of TAsmCond=(C_NONE,
          C_NE,C_LE,C_GE,C_LT,C_GT,C_EQ,C_GTU,C_GEU,C_LTU,C_LEU,
          C_NEZ,C_LEZ,C_GEZ,C_LTZ,C_GTZ,C_EQZ);
      begin
        result := inv_condflags[c];
      end;


    function reg_cgsize(const reg: tregister): tcgsize;
      begin
        case getregtype(reg) of
          R_INTREGISTER :
            result:=OS_INT;
          R_MMREGISTER:
            result:=OS_M128;
          R_FPUREGISTER:
            result:=OS_F64;
          else
            internalerror(2022111902);
        end;
      end;


    function cgsize2subreg(regtype: tregistertype; s:Tcgsize):Tsubregister;
      begin
        cgsize2subreg:=R_SUBWHOLE;
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


    function dwarf_reg(r:tregister):shortint;
      begin
        result:=regdwarf_table[findreg_by_number(r)];
        if result=-1 then
          internalerror(2022111903);
      end;

    function dwarf_reg_no_error(r:tregister):shortint;
      begin
        result:=regdwarf_table[findreg_by_number(r)];
      end;

    function eh_return_data_regno(nr: longint): longint;
      begin
        if (nr>=0) and (nr<4) then
          result:=nr+10
        else
          result:=-1;
      end;

    function conditions_equal(const c1, c2: TAsmCond): boolean;
      begin
        result:=c1=c2;
      end;


    { Checks if Subset is a subset of c (e.g. "less than" is a subset of "less than or equal" }
    function condition_in(const Subset, c: TAsmCond): Boolean;
      begin
        Result := (c = C_None) or conditions_equal(Subset, c);

        if not Result then
          case Subset of
            C_EQ:
              Result := (c in [C_GE,C_GEU,C_LE,C_LEU]);
            C_EQZ:
              Result := (c in [C_GEZ,C_LEZ]);
            else
              Result := False;
          end;
      end;


    function is_extra_reg(const s: string): tregister;
      const abiname2reg : array[tregisterindex] of tregister = (NR_NO,
            NR_R0,NR_R1,NR_R2,NR_R3,NR_R4,NR_R5,NR_R6,NR_R7,
            NR_R8,NR_R9,NR_R10,NR_R11,NR_R12,NR_R13,NR_R14,NR_R15,
            NR_R16,NR_R17,NR_R18,NR_R19,NR_R20,NR_R21,NR_R22,NR_R23,
            NR_R24,NR_R25,NR_R26,NR_R27,NR_R28,NR_R29,NR_R30,NR_R31,
            NR_F0,NR_F1,NR_F2,NR_F3,NR_F4,NR_F5,NR_F6,NR_F7,
            NR_F8,NR_F9,NR_F10,NR_F11,NR_F12,NR_F13,NR_F14,NR_F15,
            NR_F16,NR_F17,NR_F18,NR_F19,NR_F20,NR_F21,NR_F22,NR_F23,
            NR_F24,NR_F25,NR_F26,NR_F27,NR_F28,NR_F29,NR_F30,NR_F31,
            NR_FCC0,NR_FCC1,NR_FCC2,NR_FCC3,NR_FCC4,NR_FCC5,NR_FCC6,NR_FCC7);
      var
        i : longint;
      begin
        result:=NR_NO;
        { LoongArch registers start by '$' and abiname length <= 5 }
        if not(length(s) in [2..5]) and (s[1]<>'$') then
          exit;
        for i:=low(abi_regname_table) to high(abi_regname_table) do
          begin
            if s=abi_regname_table[i] then
              begin
                result:=abiname2reg[i];
                exit;
              end;
          end;
      end;

begin
end.
