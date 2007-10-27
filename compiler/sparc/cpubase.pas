{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Contains the base types for the SPARC

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
  globtype,strings,cutils,cclasses,aasmbase,cpuinfo,cgbase;


{*****************************************************************************
                                Assembler Opcodes
*****************************************************************************}

    type
{$WARNING CPU32 opcodes do not fully include the Ultra SPRAC instruction set.}
      { don't change the order of these opcodes! }
      TAsmOp=({$i opcode.inc});

      {# This should define the array of instructions as string }
      op2strtable=array[tasmop] of string[11];

    Const
      {# First value of opcode enumeration }
      firstop = low(tasmop);
      {# Last value of opcode enumeration  }
      lastop  = high(tasmop);

      std_op2str:op2strtable=({$i strinst.inc});

{*****************************************************************************
                                  Registers
*****************************************************************************}

    type
      { Number of registers used for indexing in tables }
      tregisterindex=0..{$i rspnor.inc}-1;
      totherregisterset = set of tregisterindex;

    const
      { Available Superregisters }
      {$i rspsup.inc}

      { No Subregisters }
      R_SUBWHOLE = R_SUBD;

      { Available Registers }
      {$i rspcon.inc}

      first_int_imreg = $20;
      first_fpu_imreg = $20;

      { MM Super register first and last }
      first_mm_supreg    = 0;
      first_mm_imreg     = 1;

{$warning TODO Calculate bsstart}
      regnumber_count_bsstart = 128;

      regnumber_table : array[tregisterindex] of tregister = (
        {$i rspnum.inc}
      );

      regstabs_table : array[tregisterindex] of ShortInt = (
        {$i rspstab.inc}
      );

      regdwarf_table : array[tregisterindex] of ShortInt = (
        {$i rspdwrf.inc}
      );


{*****************************************************************************
                                Conditions
*****************************************************************************}

    type
      TAsmCond=(C_None,
        C_A,C_AE,C_B,C_BE,C_C,C_E,C_G,C_GE,C_L,C_LE,C_NA,C_NAE,
        C_NB,C_NBE,C_NC,C_NE,C_NG,C_NGE,C_NL,C_NLE,C_NO,C_NP,
        C_NS,C_NZ,C_O,C_P,C_PE,C_PO,C_S,C_Z,
        C_FE,C_FG,C_FL,C_FGE,C_FLE,C_FNE
      );

    const
      cond2str:array[TAsmCond] of string[3]=('',
        'gu','cc','cs','leu','cs','e','g','ge','l','le','leu','cs',
        'cc','gu','cc','ne','le','l','ge','g','vc','XX',
        'pos','ne','vs','XX','XX','XX','vs','e',
        'e','g','l','ge','le','ne'
      );

    const
      CondAsmOps=2;
      CondAsmOp:array[0..CondAsmOps-1] of TAsmOp=(
        A_Bxx,A_FBxx
      );
      CondAsmOpStr:array[0..CondAsmOps-1] of string[7]=(
        'B','FB'
      );


{*****************************************************************************
                                   Flags
*****************************************************************************}

    type
      TResFlags=(
        { Integer results }
        F_E,  {Equal}
        F_NE, {Not Equal}
        F_G,  {Greater}
        F_L,  {Less}
        F_GE, {Greater or Equal}
        F_LE, {Less or Equal}
        F_A,  {Above}
        F_AE, {Above or Equal}
        F_B,  {Below}
        F_BE, {Below or Equal}
        F_C,  {Carry}
        F_NC, {Not Carry}
        { Floating point results }
        F_FE,  {Equal}
        F_FNE, {Not Equal}
        F_FG,  {Greater}
        F_FL,  {Less}
        F_FGE, {Greater or Equal}
        F_FLE  {Less or Equal}
      );

{*****************************************************************************
                                Operand Sizes
*****************************************************************************}


{*****************************************************************************
                                 Constants
*****************************************************************************}

    const
      max_operands = 3;

      {# Constant defining possibly all registers which might require saving }
      ALL_OTHERREGISTERS = [];

      general_superregisters = [RS_O0..RS_I7];

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

      maxintregs = 8;
      { to determine how many registers to use for regvars }
      maxintscratchregs = 3;
      usableregsint = [RS_L0..RS_L7];
      c_countusableregsint = 8;

      maxfpuregs = 8;
      usableregsfpu=[RS_F0..RS_F31];
      c_countusableregsfpu=32;

      mmregs     = [];
      usableregsmm  = [];
      c_countusableregsmm  = 0;

      { no distinction on this platform }
      maxaddrregs = 0;
      addrregs    = [];
      usableregsaddr = [];
      c_countusableregsaddr = 0;

{$warning firstsaveintreg shall be RS_NO}
      firstsaveintreg = RS_L0; { Temporary, having RS_NO is broken }
      lastsaveintreg = RS_L0; { L0..L7 are already saved, I0..O7 are parameter }
      firstsavefpureg = RS_F2; { F0..F1 is used for return value }
      lastsavefpureg = RS_F31;
      firstsavemmreg = RS_INVALID;
      lastsavemmreg = RS_INVALID;

      maxvarregs = 8;
      varregs : Array [1..maxvarregs] of Tsuperregister =
                (RS_L0,RS_L1,RS_L2,RS_L3,RS_L4,RS_L5,RS_L6,RS_L7);

      maxfpuvarregs = 1;
      fpuvarregs : Array [1..maxfpuvarregs] of TsuperRegister =
                (RS_F2);

      {
      max_param_regs_int = 6;
      param_regs_int: Array[1..max_param_regs_int] of TCpuRegister =
        (R_3,R_4,R_5,R_6,R_7,R_8,R_9,R_10);

      max_param_regs_fpu = 13;
      param_regs_fpu: Array[1..max_param_regs_fpu] of TCpuRegister =
        (R_F1,R_F2,R_F3,R_F4,R_F5,R_F6,R_F7,R_F8,R_F9,R_F10,R_F11,R_F12,R_F13);

      max_param_regs_mm = 13;
      param_regs_mm: Array[1..max_param_regs_mm] of TCpuRegister =
        (R_M1,R_M2,R_M3,R_M4,R_M5,R_M6,R_M7,R_M8,R_M9,R_M10,R_M11,R_M12,R_M13);
      }


{*****************************************************************************
                          Default generic sizes
*****************************************************************************}

      {# Defines the default address size for a processor, }
      OS_ADDR = OS_32;
      {# the natural int size for a processor,             }
      OS_INT = OS_32;
      OS_SINT = OS_S32;
      {# the maximum float size for a processor,           }
      OS_FLOAT = OS_F64;
      {# the size of a vector register for a processor     }
      OS_VECTOR = OS_M64;

{*****************************************************************************
                          Generic Register names
*****************************************************************************}

      {# Stack pointer register }
      NR_STACK_POINTER_REG = NR_O6;
      RS_STACK_POINTER_REG = RS_O6;
      {# Frame pointer register }
      NR_FRAME_POINTER_REG = NR_I6;
      RS_FRAME_POINTER_REG = RS_I6;
      {# Register for addressing absolute data in a position independant way,
         such as in PIC code. The exact meaning is ABI specific. For
         further information look at GCC source : PIC_OFFSET_TABLE_REGNUM

         Taken from GCC rs6000.h
      }
{$warning As indicated in rs6000.h, but can't find it anywhere else!}
      {PIC_OFFSET_REG = R_30;}
      { Return address for DWARF }
      NR_RETURN_ADDRESS_REG = NR_I7;
      { the return_result_reg, is used inside the called function to store its return
      value when that is a scalar value otherwise a pointer to the address of the
      result is placed inside it }
      { Results are returned in this register (32-bit values) }
      NR_FUNCTION_RETURN_REG = NR_I0;
      RS_FUNCTION_RETURN_REG = RS_I0;
      { Low part of 64bit return value }
      NR_FUNCTION_RETURN64_LOW_REG = NR_I1;
      RS_FUNCTION_RETURN64_LOW_REG = RS_I1;
      { High part of 64bit return value }
      NR_FUNCTION_RETURN64_HIGH_REG = NR_I0;
      RS_FUNCTION_RETURN64_HIGH_REG = RS_I0;
      { The value returned from a function is available in this register }
      NR_FUNCTION_RESULT_REG = NR_O0;
      RS_FUNCTION_RESULT_REG = RS_O0;
      { The lowh part of 64bit value returned from a function }
      NR_FUNCTION_RESULT64_LOW_REG = NR_O1;
      RS_FUNCTION_RESULT64_LOW_REG = RS_O1;
      { The high part of 64bit value returned from a function }
      NR_FUNCTION_RESULT64_HIGH_REG = NR_O0;
      RS_FUNCTION_RESULT64_HIGH_REG = RS_O0;

      NR_FPU_RESULT_REG = NR_F0;
      NR_MM_RESULT_REG  = NR_NO;

      PARENT_FRAMEPOINTER_OFFSET = 68; { o0 }


{*****************************************************************************
                       GCC /ABI linking information
*****************************************************************************}

      {# Registers which must be saved when calling a routine declared as
         cppdecl, cdecl, stdcall, safecall, palmossyscall. The registers
         saved should be the ones as defined in the target ABI and / or GCC.

         This value can be deduced from CALLED_USED_REGISTERS array in the
         GCC source.
      }
      saved_standard_registers : array[0..0] of tsuperregister = (RS_NO);

      { this is only for the generic code which is not used for this architecture }
      saved_mm_registers : array[0..0] of tsuperregister = (RS_NO);
      
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

    const
      simm13lo=-4096;
      simm13hi=4095;

{*****************************************************************************
                                  Helpers
*****************************************************************************}

    function  is_calljmp(o:tasmop):boolean;

    procedure inverse_flags(var f: TResFlags);
    function inverse_cond(const c: TAsmCond): TAsmCond; {$ifdef USEINLINE}inline;{$endif USEINLINE}
    function conditions_equal(const c1, c2: TAsmCond): boolean; {$ifdef USEINLINE}inline;{$endif USEINLINE}

    function  flags_to_cond(const f: TResFlags) : TAsmCond;
    function cgsize2subreg(s:Tcgsize):Tsubregister;
    function reg_cgsize(const reg: tregister): tcgsize;
    function std_regname(r:Tregister):string;
    function std_regnum_search(const s:string):Tregister;
    function findreg_by_number(r:Tregister):tregisterindex;
    function dwarf_reg(r:tregister):shortint;


implementation

    uses
      rgBase,verbose;

    const
      std_regname_table : TRegNameTAble = (
        {$i rspstd.inc}
      );

      regnumber_index : TRegisterIndexTable = (
        {$i rsprni.inc}
      );

      std_regname_index : TRegisterIndexTable = (
        {$i rspsri.inc}
      );


{*****************************************************************************
                                  Helpers
*****************************************************************************}

    function is_calljmp(o:tasmop):boolean;
      const
        CallJmpOp=[A_JMPL..A_CBccc];
      begin
        is_calljmp:=(o in CallJmpOp);
      end;


    procedure inverse_flags(var f: TResFlags);
      const
        inv_flags: array[TResFlags] of TResFlags =
          (F_NE,F_E,F_LE,F_GE,F_L,F_G,F_BE,F_B,F_AE,F_A,F_NC,F_C,
           F_FNE,F_FE,F_FLE,F_FGE,F_FL,F_FG);
      begin
        f:=inv_flags[f];
      end;


   function flags_to_cond(const f:TResFlags):TAsmCond;
      const
        flags_2_cond:array[TResFlags] of TAsmCond=
          (C_E,C_NE,C_G,C_L,C_GE,C_LE,C_A,C_AE,C_B,C_BE,C_C,C_NC,
           C_FE,C_FNE,C_FG,C_FL,C_FGE,C_FLE);
      begin
        result:=flags_2_cond[f];
      end;


    function cgsize2subreg(s:Tcgsize):Tsubregister;
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
            result:=OS_32;
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


    function findreg_by_number(r:Tregister):tregisterindex;
      begin
        result:=findreg_by_number_table(r,regnumber_index);
      end;


    function std_regname(r:Tregister):string;
      var
        p : tregisterindex;
      begin
        { For double floats show a pair like %f0:%f1 }
        if (getsubreg(r)=R_SUBFD) and
           (getsupreg(r)<first_fpu_imreg) then
          begin
            setsubreg(r,R_SUBFS);
            p:=findreg_by_number(r);
            if p<>0 then
              result:=std_regname_table[p]
            else
              result:=generic_regname(r);
            setsupreg(r,getsupreg(r)+1);
            p:=findreg_by_number(r);
            if p<>0 then
              result:=result+':'+std_regname_table[p]
            else
              result:=result+':'+generic_regname(r);
          end
        else
          begin
            p:=findreg_by_number(r);
            if p<>0 then
              result:=std_regname_table[p]
            else
              result:=generic_regname(r);
          end;
      end;


    function std_regnum_search(const s:string):Tregister;
      begin
        result:=regnumber_table[findreg_by_name_table(s,std_regname_table,std_regname_index)];
      end;


    function inverse_cond(const c: TAsmCond): TAsmCond; {$ifdef USEINLINE}inline;{$endif USEINLINE}
      const
        inverse: array[TAsmCond] of TAsmCond=(C_None,
          C_NA,C_NAE,C_NB,C_NBE,C_NC,C_NE,C_NG,C_NGE,C_NL,C_NLE,C_A,C_AE,
          C_B,C_BE,C_C,C_E,C_G,C_GE,C_L,C_LE,C_O,C_P,
          C_S,C_Z,C_NO,C_NP,C_NP,C_P,C_NS,C_NZ,
          C_FNE,C_FLE,C_FGE,C_FL,C_FG,C_FE
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
