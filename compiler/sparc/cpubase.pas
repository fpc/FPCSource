{
    $Id$
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
{$WARNING CPU32 opcodes do not fully include the Ultra SPRAC instruction set.}
      { don't change the order of these opcodes! }
      TAsmOp=({$INCLUDE opcode.inc});

      {# This should define the array of instructions as string }
      op2strtable=array[tasmop] of string[11];

    Const
      {# First value of opcode enumeration }
      firstop = low(tasmop);
      {# Last value of opcode enumeration  }
      lastop  = high(tasmop);

      std_op2str:op2strtable=({$INCLUDE strinst.inc});

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
      R_SUBWHOLE=R_SUBNONE;

      { Available Registers }
      {$i rspcon.inc}

      { Integer Super registers first and last }
{$warning Supreg shall be $00-$1f}
      first_int_supreg = $00;
      last_int_supreg = $1f;

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
      regnumber_count_bsstart = 128;

      regnumber_table : array[tregisterindex] of tregister = (
        {$i rspnum.inc}
      );

      regstabs_table : array[tregisterindex] of tregister = (
        {$i rspstab.inc}
      );


{*****************************************************************************
                                Conditions
*****************************************************************************}

    type
      TAsmCond=(C_None,
        C_A,C_AE,C_B,C_BE,C_C,C_E,C_G,C_GE,C_L,C_LE,C_NA,C_NAE,
        C_NB,C_NBE,C_NC,C_NE,C_NG,C_NGE,C_NL,C_NLE,C_NO,C_NP,
        C_NS,C_NZ,C_O,C_P,C_PE,C_PO,C_S,C_Z
      );

    const
      cond2str:array[TAsmCond] of string[3]=('',
        'gu','cc','cs','leu','cs','e','g','ge','l','le','leu','cs',
        'cc','gu','cc','ne','le','l','ge','g','vc','XX',
        'pos','ne','vs','XX','XX','XX','vs','e'
      );

      inverse_cond:array[TAsmCond] of TAsmCond=(C_None,
        C_NA,C_NAE,C_NB,C_NBE,C_NC,C_NE,C_NG,C_NGE,C_NL,C_NLE,C_A,C_AE,
        C_B,C_BE,C_C,C_E,C_G,C_GE,C_L,C_LE,C_O,C_P,
        C_S,C_Z,C_NO,C_NP,C_NP,C_P,C_NS,C_NZ
      );

    const
      CondAsmOps=1;
      CondAsmOp:array[0..CondAsmOps-1] of TAsmOp=(
        A_Bxx
      );
      CondAsmOpStr:array[0..CondAsmOps-1] of string[7]=(
        'B'
      );

{*****************************************************************************
                                   Flags
*****************************************************************************}

    type
      TResFlags=(
        F_E,  {Equal}
        F_NE, {Not Equal}
        F_G,  {Greater}
        F_L,  {Less}
        F_GE, {Greater or Equal}
        F_LE, {Less or Equal}
        F_C,  {Carry}
        F_NC, {Not Carry}
        F_A,  {Above}
        F_AE, {Above or Equal}
        F_B,  {Below}
        F_BE  {Below or Equal}
      );

{*****************************************************************************
                                Reference
*****************************************************************************}

    type
      TRefOptions=(ref_none,ref_parafixup,ref_localfixup,ref_selffixup);

      { since we have no full 32 bit offsets, we need to be able to specify the high
        and low bits of the address of a symbol                                      }
      trefsymaddr = (refs_no,refs_full,refs_hi,refs_lo);

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
         { means a full 32bit reference, refs_hi means the upper 16 bits }
         { and refs_lo the lower 16 bits of the address                   }
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
      symaddr2str: array[trefsymaddr] of string[3] = ('','','%hi','%lo');


{*****************************************************************************
                                Operand Sizes
*****************************************************************************}

{$ifdef dummy}
{*****************************************************************************
                             Argument Classification
*****************************************************************************}
type
  TArgClass = (
     { the following classes should be defined by all processor implemnations }
     AC_NOCLASS,
     AC_MEMORY,
     AC_INTEGER,
     AC_FPU,
     { the following argument classes are i386 specific }
     AC_FPUUP,
     AC_SSE,
     AC_SSEUP);
{$endif dummy}

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
            LOC_REFERENCE : (reference : tparareference; low_in_reg: boolean; lowreg : tregister);
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
      std_saved_registers = [];

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
    function  flags_to_cond(const f: TResFlags) : TAsmCond;
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
        {$i rspstd.inc}
      );

      regnumber_index : array[tregisterindex] of tregisterindex = (
        {$i rsprni.inc}
      );

      std_regname_index : array[tregisterindex] of tregisterindex = (
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
          (F_NE,F_E,F_LE,F_GE,F_L,F_G,F_NC,F_C,F_BE,F_B,F_AE,F_A);
      begin
        f:=inv_flags[f];
      end;


   function flags_to_cond(const f:TResFlags):TAsmCond;
      const
        flags_2_cond:array[TResFlags] of TAsmCond=
          (C_E,C_NE,C_G,C_L,C_GE,C_LE,C_C,C_NC,C_A,C_AE,C_B,C_BE);
      begin
        result:=flags_2_cond[f];
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
          result:=std_regname_table[p]
        else
          result:=generic_regname(r);
      end;

end.
{
  $Log$
  Revision 1.53  2003-10-08 14:11:36  mazen
  + Alignement field added to TParaLocation (=4 as 32 bits archs)

  Revision 1.52  2003/10/01 20:34:50  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.51  2003/09/14 21:35:15  peter
    * new volatile registers proc

  Revision 1.50  2003/09/14 19:19:05  peter
    * updates for new ra

  Revision 1.49  2003/09/03 16:29:37  peter
    * superregisters also from .dat file

  Revision 1.48  2003/09/03 15:55:01  peter
    * NEWRA branch merged

  Revision 1.47.2.3  2003/09/02 17:49:17  peter
    * newra updates

  Revision 1.47.2.2  2003/09/01 21:02:55  peter
    * sparc updates for new tregister

  Revision 1.47.2.1  2003/08/31 21:08:16  peter
    * first batch of sparc fixes

  Revision 1.47  2003/08/19 13:22:51  mazen
  + implemented gas_regname based on convert_register_to_enum std_Reg2str

  Revision 1.46  2003/08/17 16:59:20  jonas
    * fixed regvars so they work with newra (at least for ppc)
    * fixed some volatile register bugs
    + -dnotranslation option for -dnewra, which causes the registers not to
      be translated from virtual to normal registers. Requires support in
      the assembler writer as well, which is only implemented in aggas/
      agppcgas currently

  Revision 1.45  2003/07/06 17:58:22  peter
    * framepointer fixes for sparc
    * parent framepointer code more generic

  Revision 1.44  2003/07/02 22:18:04  peter
    * paraloc splitted in callerparaloc,calleeparaloc
    * sparc calling convention updates

  Revision 1.43  2003/06/17 16:34:44  jonas
    * lots of newra fixes (need getfuncretparaloc implementation for i386)!
    * renamed all_intregisters to volatile_intregisters and made it
      processor dependent

  Revision 1.42  2003/06/13 21:08:30  peter
    * supreg_name added

  Revision 1.41  2003/06/12 19:11:34  jonas
    - removed ALL_INTREGISTERS (only the one in rgobj is valid)

  Revision 1.40  2003/06/04 21:00:54  mazen
  - making TOldRegister only declared for compatibility and
    no more used in cpubase

  Revision 1.39  2003/06/01 21:38:06  peter
    * getregisterfpu size parameter added
    * op_const_reg size parameter added
    * sparc updates

  Revision 1.38  2003/06/01 01:04:35  peter
    * reference fixes

  Revision 1.37  2003/05/31 15:05:28  peter
    * FUNCTION_RESULT64_LOW/HIGH_REG added for int64 results

  Revision 1.36  2003/05/31 01:00:51  peter
    * register fixes

  Revision 1.35  2003/05/30 23:57:08  peter
    * more sparc cleanup
    * accumulator removed, splitted in function_return_reg (called) and
      function_result_reg (caller)

}
