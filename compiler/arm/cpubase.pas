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
              A_UMLAL,A_UMULL
              { FPA coprocessor codes }
              { VPA coprocessor codes }
              );

      { This should define the array of instructions as string }
      op2strtable=array[tasmop] of string[11];

    Const
      { First value of opcode enumeration }
      firstop = low(tasmop);
      { Last value of opcode enumeration  }
      lastop  = high(tasmop);

{*****************************************************************************
                                  Registers
*****************************************************************************}

    type
      {# Enumeration for all possible registers for cpu. It
        is to note that all registers of the same type
        (for example all FPU registers), should be grouped
        together.
      }
      { don't change the order }
      { it's used by the register size conversions        }
      toldregister = (R_NO,
        R_R0,R_R1,R_R2,R_R3,R_R4,R_R5,R_R6,R_R7,
        R_R8,R_R9,R_R10,R_R11,R_R12,R_R13,R_R14,R_PC,
        R_CPSR,
        { FPA registers }
        R_F0,R_F1,R_F2,R_F3,R_F4,R_F5,R_F6,R_F7,
        R_F8,R_F9,R_F10,R_F11,R_F12,R_F13,R_F14,R_F15,
        { VPA registers }
        R_S0,R_S1,R_S2,R_S3,R_S4,R_S5,R_S6,R_S7,
        R_S8,R_S9,R_S10,R_S11,R_S12,R_S13,R_S14,R_S15,
        R_S16,R_S17,R_S18,R_S19,R_S20,R_S21,R_S22,R_S23,
        R_S24,R_S25,R_S26,R_S27,R_S28,R_S29,R_S30,R_S31,
        R_D0,R_D1,R_D2,R_D3,R_D4,R_D5,R_D6,R_D7,
        R_D8,R_D9,R_D10,R_D11,R_D12,R_D13,R_D14,R_D15,
        R_INTREGISTER,R_FLOATREGISTER,R_MMXREGISTER,R_KNIREGISTER
      );

    const
      { special registers }
      { Invalid register }
      NR_NO    = $0000;

      { Normal registers:}

      { General purpose registers }
      NR_R0 = $0100; NR_R1 = $0200; NR_R2 = $0300;
      NR_R3 = $0400; NR_R4 = $0500; NR_R5 = $0600;
      NR_R6 = $0700; NR_R7 = $0800; NR_R8 = $0900;
      NR_R9 = $0A00; NR_R10 = $0B00; NR_R11 = $0C00;
      NR_R12 = $0D00; NR_R13 = $0E00; NR_R14 = $0F00;
      NR_R15 = $1000;
      NR_PC  = NR_R15;

      { Super registers: }
      RS_NONE=$00;
      RS_R0 = $01;  RS_R1 = $02; RS_R2 = $03;
      RS_R3 = $04;  RS_R4 = $05; RS_R5 = $06;
      RS_R6 = $07;  RS_R7 = $08; RS_R8 = $09;
      RS_R9 = $0A;  RS_R10 = $0B; RS_R11 = $0C;
      RS_R12 = $0D; RS_R13 = $0E; RS_R14 = $0F;
      RS_R15 = $10;
      RS_PC = RS_R15;

      first_supreg = RS_R0;
      last_supreg = RS_R15;

      { registers which may be destroyed by calls }
      VOLATILE_INTREGISTERS = [RS_R0..RS_R3];
      VOLATILE_FPUREGISTERS = [R_F0..R_F3];

      { Number of first and last imaginary register. }
      first_imreg     = $21;
      last_imreg      = $ff;

      { Subregisters, situation unknown!!.}
      R_SUBWHOLE=$00;
      R_SUBL=$00;

    type
      tnewregister=word;

      Tregister = packed record
        enum : Toldregister;
        { This is a word for now, change to cardinal
          when the old register coding is away.}
        number : Tnewregister;
      end;

      Tsuperregister = byte;
      Tsubregister = byte;

      { A type to store register locations for 64 Bit values. }
      tregister64 = packed record
        reglo,reghi : tregister;
      end;

      { alias for compact code }
      treg64 = tregister64;

      { Set type definition for registers }
      tregisterset = set of toldregister;
      tsupregset = set of tsuperregister;

    const
      { First register in the tregister enumeration }
      firstreg = low(toldregister);
      { Last register in the tregister enumeration }
      lastreg  = R_D15;

    type
      { Type definition for the array of string of register names }
      reg2strtable = array[firstreg..lastreg] of string[6];
      regname2regnumrec = record
        name:string[6];
        number:Tnewregister;
      end;

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
        { load/store }
        PF_B,PF_SB,PF_BT,PF_H,PF_SH,PF_T,
        { multiple load/store address modes }
        PF_IA,PF_IB,PF_DA,PF_DB,PF_DF,PF_FA,PF_ED,PF_EA
      );
    const
      oppostfix2str : array[TOpPostfix] of string[2] = ('',
        's',
        'b','sb','bt','h','sh','t',
        'ia','ib','da','db','df','fa','ed','ea');

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
      tshiftmode = (SM_LSL,SM_LSR,SM_ASR,SM_ROR,SM_RRX);

      { reference record }
      preference = ^treference;
      treference = packed record
         base,
         index       : tregister;
         scalefactor : byte;
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


      tshiftertype = (SO_None,SO_ASR,SO_LSL,SO_LSR,SO_ROR,SO_RRX);

      pshifterop = ^tshifterop;

      tshifterop = record
        shiftertype : tshiftertype;
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
         top_regset : (regset:tsupregset);
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
      ALL_REGISTERS = [firstreg..lastreg];

      general_registers = [R_R0..R_PC];
      general_superregisters = [RS_R0..RS_PC];

      {# low and high of the available maximum width integer general purpose }
      { registers                                                            }
      LoGPReg = R_R0;
      HiGPReg = R_R14;

      {# low and high of every possible width general purpose register (same as }
      { above on most architctures apart from the 80x86)                        }
      LoReg = R_R0;
      HiReg = R_R14;

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
      maxintscratchregs = 2;
      intregs = [R_R0..R_R14];
      usableregsint = [RS_R4..RS_R10];
      c_countusableregsint = 7;

      maxfpuregs = 8;
      fpuregs = [R_F0..R_F7];
      usableregsfpu = [R_F4..R_F7];
      c_countusableregsfpu = 4;

      mmregs = [R_S0..R_D7];
      usableregsmm = [R_S16..R_S31];
      c_countusableregsmm  = 16;

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
                                Registers
*****************************************************************************}
    const
        { Standard opcode string table (for each tasmop enumeration). The
          opcode strings should conform to the names as defined by the
          processor manufacturer.
        }
        std_op2str : op2strtable = (
                '','adc','add','and','n','bic','bkpt','b','bl','blx','bx',
                'cdp','cdp2','clz','cmn','cmp','eor','ldc','ldc2',
                'ldm','ldr','ldrb','ldrd','ldrbt','ldrh','ldrsb',
                'ldrsh','ldrt','mcr','mcr2','mcrr','mla','mov',
                'mrc','mrc2','mrrc','rs','msr','mul','mvn',
                'orr','pld','qadd','qdadd','qdsub','qsub','rsb','rsc',
                'sbc','smlal','smull','smul',
                'smulw','stc','stc2','stm','str','strb','strbt','strd',
                'strh','strt','sub','swi','swp','swpb','teq','tst',
                'umlal','umull'
                { FPA coprocessor codes }
                { VPA coprocessor codes }
                );

        { Standard register table (for each tregister enumeration). The
          register strings should conform to the the names as defined
          by the processor manufacturer
        }
        std_reg2str : reg2strtable = ('',
          'r0','r1','r2','r3','r4','r5','r6','r7',
          'r8','r9','r10','r11','r12','r13','r14','pc',
          'cpsr',
          { FPA registers }
          'f0','f1','f2','f3','f4','f5','f6','f7',
          'f8','f9','f10','f11','f12','f13','f14','f15',
          { VPA registers }
          's0','s1','s2','s3','s4','s5','s6','s7',
          's8','s9','s10','s11','s12','s13','s14','s15',
          's16','s17','s18','s19','s20','s21','s22','s23',
          's24','s25','s26','s27','s28','s29','s30','s31',
          'd0','d1','d2','d3','d4','d5','d6','d7',
          'd8','d9','d10','d11','d12','d13','d14','d15'
        );

{*****************************************************************************
                                 Constants
*****************************************************************************}

      firstsaveintreg = R_R4;
      lastsaveintreg  = R_R10;
      firstsavefpureg = R_F4;
      lastsavefpureg  = R_F7;
      firstsavemmreg  = R_S16;
      lastsavemmreg   = R_S31;

//!!!      general_registers = [R_EAX,R_EBX,R_ECX,R_EDX];
//!!!      general_superregisters = [RS_EAX,RS_EBX,RS_ECX,RS_EDX];


//!!!      usableregsint = [first_imreg..last_imreg];
//!!!      c_countusableregsint = 4;

      maxaddrregs = 0;
      addrregs    = [];
      usableregsaddr = [];
      c_countusableregsaddr = 0;

      maxvarregs = 7;
      varregs : Array [1..maxvarregs] of Tnewregister =
                (RS_R4,RS_R5,RS_R6,RS_R7,RS_R8,RS_R9,RS_R10);

      maxfpuvarregs = 4;
      fpuvarregs : Array [1..maxfpuvarregs] of Toldregister =
                (R_F4,R_F5,R_F6,R_F7);

{*****************************************************************************
                               GDB Information
*****************************************************************************}

      {
        I don't know where I could get this information for the arm
      }
      stab_regindex : array[0..0] of shortint =
        (0
      );

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
      frame_pointer_reg = R_R11;
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

      fpu_result_reg = R_F0;
//!!!      mmresultreg = R_MM0;

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
      std_saved_registers = [R_R4..R_R10];
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

    procedure convert_register_to_enum(var r:Tregister);
    function cgsize2subreg(s:Tcgsize):Tsubregister;
    function reg2opsize(r:tregister):topsize;
    function is_calljmp(o:tasmop):boolean;
    procedure inverse_flags(var f: TResFlags);
    function flags_to_cond(const f: TResFlags) : TAsmCond;
    function supreg_name(r:Tsuperregister):string;

    procedure shifterop_reset(var so : tshifterop);

implementation

    procedure convert_register_to_enum(var r:Tregister);
      begin
      end;


    function cgsize2subreg(s:Tcgsize):Tsubregister;
      begin
      end;


    function reg2opsize(r:tregister):topsize;
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
      begin
      end;


    function supreg_name(r:Tsuperregister):string;
      begin
      end;

    procedure shifterop_reset(var so : tshifterop);
      begin
        FillChar(so,sizeof(so),0);
      end;


end.
{
  $Log$
  Revision 1.6  2003-08-24 12:27:26  florian
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
