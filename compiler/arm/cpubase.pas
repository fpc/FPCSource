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
      TAsmOp= {$i armop.inc}

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
      tregisterindex=0..{$i rarmnor.inc}-1;

    const
      { Available Superregisters }
      {$i rarmsup.inc}

      RS_PC = RS_R15;

      { No Subregisters }
      R_SUBWHOLE = R_SUBNONE;

      { Available Registers }
      {$i rarmcon.inc}

      { aliases }
      NR_PC = NR_R15;

      { Integer Super registers first and last }
      first_int_supreg = RS_R0;
      first_int_imreg = $10;

      { Float Super register first and last }
      first_fpu_supreg    = RS_F0;
      first_fpu_imreg     = $08;

      { MM Super register first and last }
      first_mm_supreg    = RS_S0;
      first_mm_imreg     = $20;

{ TODO: Calculate bsstart}
      regnumber_count_bsstart = 64;

      regnumber_table : array[tregisterindex] of tregister = (
        {$i rarmnum.inc}
      );

      regstabs_table : array[tregisterindex] of shortint = (
        {$i rarmsta.inc}
      );

      regdwarf_table : array[tregisterindex] of shortint = (
        {$i rarmdwa.inc}
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
        PF_None,
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

      general_superregisters = [RS_R0..RS_PC];

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

      maxfpuregs = 8;
      fpuregs = [RS_F0..RS_F7];
      usableregsfpu = [RS_F4..RS_F7];
      c_countusableregsfpu = 4;

      mmregs = [RS_D0..RS_D15];
      usableregsmm = [RS_D8..RS_D15];
      c_countusableregsmm  = 8;

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
      firstsavefpureg = RS_F4;
      lastsavefpureg  = RS_F7;
      firstsavemmreg  = RS_D8;
      lastsavemmreg   = RS_D15;

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
      { The value returned from a function is available in this register }
      NR_FUNCTION_RESULT_REG = NR_FUNCTION_RETURN_REG;
      RS_FUNCTION_RESULT_REG = RS_FUNCTION_RETURN_REG;

      NR_FPU_RESULT_REG = NR_F0;

      NR_MM_RESULT_REG  = NR_NO;

      NR_RETURN_ADDRESS_REG = NR_FUNCTION_RETURN_REG;

      { Offset where the parent framepointer is pushed }
      PARENT_FRAMEPOINTER_OFFSET = 0;

      { Low part of 64bit return value }
      function NR_FUNCTION_RESULT64_LOW_REG: tregister;
      function RS_FUNCTION_RESULT64_LOW_REG: shortint;
      { High part of 64bit return value }
      function NR_FUNCTION_RESULT64_HIGH_REG: tregister;
      function RS_FUNCTION_RESULT64_HIGH_REG: shortint;

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
    function cgsize2subreg(regtype: tregistertype; s:Tcgsize):Tsubregister;
    function is_calljmp(o:tasmop):boolean;
    procedure inverse_flags(var f: TResFlags);
    function flags_to_cond(const f: TResFlags) : TAsmCond;
    function findreg_by_number(r:Tregister):tregisterindex;
    function std_regnum_search(const s:string):Tregister;
    function std_regname(r:Tregister):string;

    function inverse_cond(const c: TAsmCond): TAsmCond; {$ifdef USEINLINE}inline;{$endif USEINLINE}
    function conditions_equal(const c1, c2: TAsmCond): boolean; {$ifdef USEINLINE}inline;{$endif USEINLINE}

    procedure shifterop_reset(var so : tshifterop);
    function is_pc(const r : tregister) : boolean;

    function is_shifter_const(d : aint;var imm_shift : byte) : boolean;
    function dwarf_reg(r:tregister):shortint;

  implementation

    uses
      systems,rgBase,verbose;


    const
      std_regname_table : array[tregisterindex] of string[7] = (
        {$i rarmstd.inc}
      );

      regnumber_index : array[tregisterindex] of tregisterindex = (
        {$i rarmrni.inc}
      );

      std_regname_index : array[tregisterindex] of tregisterindex = (
        {$i rarmsri.inc}
      );


    function cgsize2subreg(regtype: tregistertype; s:Tcgsize):Tsubregister;
      begin
        cgsize2subreg:=R_SUBWHOLE;
      end;


    function reg_cgsize(const reg: tregister): tcgsize;
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
        is_calljmp:= o in [A_B,A_BL,A_BX,A_BLX];
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


    function is_shifter_const(d : aint;var imm_shift : byte) : boolean;
      var
         i : longint;
      begin
        if current_settings.cputype in cpu_thumb2 then
          begin
            for i:=0 to 24 do
              begin
                 if (dword(d) and not($ff shl i))=0 then
                   begin
                     imm_shift:=i;
                     result:=true;
                     exit;
                   end;
              end;
          end
        else
          begin
            for i:=0 to 15 do
              begin
                 if (dword(d) and not(rotl($ff,i*2)))=0 then
                   begin
                      imm_shift:=i*2;
                      result:=true;
                      exit;
                   end;
              end;
          end;
        result:=false;
      end;


    function dwarf_reg(r:tregister):shortint;
      begin
        result:=regdwarf_table[findreg_by_number(r)];
        if result=-1 then
          internalerror(200603251);
      end;

      { Low part of 64bit return value }
    function NR_FUNCTION_RESULT64_LOW_REG: tregister;
    begin
      if target_info.endian=endian_little then
        result:=NR_R0
      else
        result:=NR_R1;
    end;

    function RS_FUNCTION_RESULT64_LOW_REG: shortint;
    begin
      if target_info.endian=endian_little then
        result:=RS_R0
      else
        result:=RS_R1;
    end;

      { High part of 64bit return value }
    function NR_FUNCTION_RESULT64_HIGH_REG: tregister;
    begin
      if target_info.endian=endian_little then
        result:=NR_R1
      else
        result:=NR_R0;
    end;

    function RS_FUNCTION_RESULT64_HIGH_REG: shortint;
    begin
      if target_info.endian=endian_little then
        result:=RS_R1
      else
        result:=RS_R0;
    end;

end.
