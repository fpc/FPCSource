{
    $Id$
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
      cginfo
    {$ifdef delphi}
      ,dmisc
    {$endif}
      ;


{*****************************************************************************
                                Assembler Opcodes
*****************************************************************************}

    type
      TAsmOp=(A_None,A_ADC,A_ADD,A_AND,A_N,A_BIC,A_BKPT,A_B,A_BL,A_BLX,A_BX,
              A_CDP,A_CDP2,A_CLZ,A_CMN,A_CMP,A_EOR,A_LDC,_A_LDC2,
              A_LDM,A_LDR,A_LDRB,A_LDRD,A_LDRBT,A_LDRH,A_LDRSB,
              A_LDRSH,A_LDRT,A_MCR,A_MCR2,A_MCRR,A_MLA,A_MOV,
              A_MRC,A_MRC2,A_MRRC,A_RS,A_MSR,A_MUL,A_MVN,
              A_ORR,A_PLD,A_QADD,A_QDADD,A_QDSUB,A_QSUB,A_RSB,A_RSC,
              A_SBC,A_SMLAL,A_SMULL,A_SMUL,
              A_SMULW,A_STC,A_STC2,A_STM,A_STR,A_STRB,A_STRBT,A_STRD,
              A_STRH,A_STRT,A_SUB,A_SWI,A_SWP,A_SWPB,A_TEQ,A_TST,
              A_UMLAL,A_UMULL,
              { FPA coprocessor instructions }
              A_LDF,A_STF,A_LFM,A_SFM,A_FLT,A_FIX,A_WFS,A_RFS,A_RFC,
              A_ADF,A_DVF,A_FDV,A_FML,A_FRD,A_MUF,A_POL,A_PW,A_RDF,
              A_RMF,A_RPW,A_RSF,A_SUF,A_ABS,A_ACS,A_ASN,A_ATN,A_COS,
              A_EXP,A_LOG,A_LGN,A_MVF,A_MNF,A_NRM,A_RND,A_SIN,A_SQT,A_TAN,A_URD,
              A_CMF,A_CNF
              { VPA coprocessor codes }
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
      tregisterindex=0..{$i rarmnor.inc}-1;

    const
      { Super registers: }
      RS_NONE=$00;
      RS_R0 = $01;  RS_R1 = $02; RS_R2 = $03;
      RS_R3 = $04;  RS_R4 = $05; RS_R5 = $06;
      RS_R6 = $07;  RS_R7 = $08; RS_R8 = $09;
      RS_R9 = $0A;  RS_R10 = $0B; RS_R11 = $0C;
      RS_R12 = $0D; RS_R13 = $0E; RS_R14 = $0F;
      RS_R15 = $10;
      RS_PC = RS_R15;

      RS_F0=$00;
      RS_F1=$01;
      RS_F2=$02;
      RS_F3=$03;
      RS_F4=$04;
      RS_F5=$05;
      RS_F6=$06;
      RS_F7=$07;

      RS_D0 = $01;  RS_D1 = $02; RS_D2 = $03;
      RS_D3 = $04;  RS_D4 = $05; RS_D5 = $06;
      RS_D6 = $07;  RS_D7 = $08; RS_D8 = $09;
      RS_D9 = $0A;  RS_D10 = $0B; RS_D11 = $0C;
      RS_D12 = $0D; RS_D13 = $0E; RS_D14 = $0F;
      RS_D15 = $10;

      { No Subregisters }
      R_SUBWHOLE = R_SUBNONE;

      { Available Registers }
      {$i rarmcon.inc}

      { Integer Super registers first and last }
{$warning Supreg shall be $00-$1f}
      first_int_supreg = RS_R3;
      last_int_supreg = RS_R15;

      first_int_imreg = $20;
      last_int_imreg = $fe;

      { Float Super register first and last }
      first_fpu_supreg    = $00;
      last_fpu_supreg     = $07;

      first_fpu_imreg     = $20;
      last_fpu_imreg      = $fe;

      { MM Super register first and last }
      first_mmx_supreg    = RS_INVALID;
      last_mmx_supreg     = RS_INVALID;
      first_mmx_imreg     = RS_INVALID;
      last_mmx_imreg      = RS_INVALID;

{$warning TODO Calculate bsstart}
      regnumber_count_bsstart = 64;

      regnumber_table : array[tregisterindex] of tregister = (
        {$i rarmnum.inc}
      );

      regstabs_table : array[tregisterindex] of tregister = (
        {$i rarmsta.inc}
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
        PF_D,PF_E,PF_P,FP_EP,
        { load/store }
        PF_B,PF_SB,PF_BT,PF_H,PF_SH,PF_T,
        { multiple load/store address modes }
        PF_IA,PF_IB,PF_DA,PF_DB,PF_FD,PF_FA,PF_ED,PF_EA
      );

      TRoundingMode = (RM_None,RM_P,RM_M,RM_Z);

    const
      cgsize2fpuoppostfix : array[OS_NO..OS_F128] of toppostfix = (
        PF_E,
        PF_None,PF_None,PF_None,PF_None,PF_None,PF_None,PF_None,PF_None,
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
                                Reference
*****************************************************************************}

    type
      trefoptions=(ref_none,ref_parafixup,ref_localfixup,ref_selffixup);

      taddressmode = (AM_OFFSET,AM_PREINDEXED,AM_POSTINDEXED);
      tshiftmode = (SM_None,SM_LSL,SM_LSR,SM_ASR,SM_ROR,SM_RRX);

      { reference record }
      preference = ^treference;
      treference = packed record
         base,
         index       : tregister;
         shiftimm    : byte;
         signindex   : shortint;
         offset      : longint;
         symbol      : tasmsymbol;
         offsetfixup : longint;
         options     : trefoptions;
         addressmode : taddressmode;
         shiftmode   : tshiftmode;
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
      toptype=(top_none,top_reg,top_ref,top_const,top_symbol,top_regset,top_shifterop);

      tupdatereg = (UR_None,UR_Update);

      pshifterop = ^tshifterop;

      tshifterop = record
        shiftmode : tshiftmode;
        rs : tregister;
        shiftimm : byte;
      end;

      toper = record
        case typ : toptype of
         top_none   : ();
         top_reg    : (reg:tregister;update:tupdatereg);
         top_ref    : (ref:preference);
         top_const  : (val:aword);
         top_symbol : (sym:tasmsymbol;symofs:longint);
         top_regset : (regset:tsuperregisterset);
         top_shifterop : (shifterop : pshifterop);
      end;

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
         loc  : TCGLoc;
         sp_fixup : longint;
         case TCGLoc of
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
            { it's only for better handling }
            LOC_MMXREGISTER,LOC_CMMXREGISTER : (mmxreg : tregister);
      end;

      tlocation = packed record
         loc  : TCGLoc;
         size : TCGSize;
         case TCGLoc of
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
            { it's only for better handling }
            LOC_MMXREGISTER,LOC_CMMXREGISTER : (mmxreg : tregister);
      end;

{*****************************************************************************
                                 Constants
*****************************************************************************}

    const
      { declare aliases }
      LOC_MMREGISTER = LOC_SSEREGISTER;
      LOC_CMMREGISTER = LOC_CSSEREGISTER;

      max_operands = 3;

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
      std_saved_registers = [RS_R4..RS_R10];
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

    function cgsize2subreg(s:Tcgsize):Tsubregister;
    function is_calljmp(o:tasmop):boolean;
    procedure inverse_flags(var f: TResFlags);
    function flags_to_cond(const f: TResFlags) : TAsmCond;
    function findreg_by_number(r:Tregister):tregisterindex;
    function findreg_by_stdname(const s:string):byte;
    function std_regnum_search(const s:string):Tregister;
    function std_regname(r:Tregister):string;

    procedure shifterop_reset(var so : tshifterop);
    function is_pc(const r : tregister) : boolean;

  implementation

    uses
      verbose;


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


    function cgsize2subreg(s:Tcgsize):Tsubregister;
      begin
      end;


    function is_calljmp(o:tasmop):boolean;
      begin
        { This isn't 100% perfect because the arm allows jumps also by writing to PC=R15.
          To overcome this problem we simply forbid that FPC generates jumps by loading R15 }
        is_calljmp:= o in [A_B,A_BL,A_BX,A_BLX];
      end;


    procedure inverse_flags(var f: TResFlags);
      begin
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


    procedure shifterop_reset(var so : tshifterop);
      begin
        FillChar(so,sizeof(so),0);
      end;


    function is_pc(const r : tregister) : boolean;
      begin
        is_pc:=(r=NR_R15);
      end;

end.
{
  $Log$
  Revision 1.12  2003-09-04 00:15:29  florian
    * first bunch of adaptions of arm compiler for new register type

  Revision 1.11  2003/09/03 19:10:30  florian
    * initial revision of new register naming

  Revision 1.10  2003/09/01 15:11:16  florian
    * fixed reference handling
    * fixed operand postfix for floating point instructions
    * fixed wrong shifter constant handling

  Revision 1.9  2003/08/29 21:36:28  florian
    * fixed procedure entry/exit code
    * started to fix reference handling

  Revision 1.8  2003/08/28 00:05:29  florian
    * today's arm patches

  Revision 1.7  2003/08/25 23:20:38  florian
    + started to implement FPU support for the ARM
    * fixed a lot of other things

  Revision 1.6  2003/08/24 12:27:26  florian
    * continued to work on the arm port

  Revision 1.5  2003/08/21 03:14:00  florian
    * arm compiler can be compiled; far from being working

  Revision 1.4  2003/08/20 15:50:13  florian
    * more arm stuff

  Revision 1.3  2003/08/16 13:23:01  florian
    * several arm related stuff fixed

  Revision 1.2  2003/07/26 00:55:57  florian
    * basic stuff fixed

  Revision 1.1  2003/07/21 16:35:30  florian
    * very basic stuff for the arm
}
