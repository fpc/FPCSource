{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl and Peter Vreman

    Contains the basic declarations for the x86-64 architecture

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
{ This unit contains the basic declarations for the x86-64 architecture.
}
unit cpubase;

{$i fpcdefs.inc}

interface

uses
  globals,cutils,cclasses,
  aasmbase,
  cpuinfo,
  cginfo;

{*****************************************************************************
                                Assembler Opcodes
*****************************************************************************}

    type
      TAsmOp={$i x86_64op.inc}

      { This should define the array of instructions as string }
      op2strtable=array[tasmop] of string[11];

    Const
      { First value of opcode enumeration }
      firstop = low(tasmop);
      { Last value of opcode enumeration }
      lastop  = high(tasmop);


{*****************************************************************************
                                Operand Sizes
*****************************************************************************}

type
  topsize = (S_NO,
    S_B,S_W,S_L,S_BW,S_BL,S_WL,
    S_IS,S_IL,S_IQ,
    S_FS,S_FL,S_FX,S_D,S_Q,S_FV,
    S_NEAR,S_FAR,S_SHORT
  );

{*****************************************************************************
                                  Registers
*****************************************************************************}

type
  { don't change the order }
  { it's used by the register size conversions }
  { Enumeration of all registers of the CPU }
  tregister = (R_NO,
    R_RAX,R_RCX,R_RDX,R_RBX,R_RSP,R_RBP,R_RSI,R_RDI,
    R_R8,R_R9,R_R10,R_R11,R_R12,R_R13,R_R14,R_R15,R_RIP,
    R_EAX,R_ECX,R_EDX,R_EBX,R_ESP,R_EBP,R_ESI,R_EDI,
    R_R8D,R_R9D,R_R10D,R_R11D,R_R12D,R_R13D,R_R14D,R_R15D,
    R_AX,R_CX,R_DX,R_BX,R_SP,R_BP,R_SI,R_DI,
    R_R8W,R_R9W,R_R10W,R_R11W,R_R12W,R_R13W,R_R14W,R_R15W,
    R_AL,R_CL,R_DL,R_BL,R_SPL,R_BPL,R_SIL,R_DIL,
    R_R8B,R_R9B,R_R10B,R_R11B,R_R12B,R_R13B,R_R14B,R_R15B,
    R_AH,R_CH,R_BH,R_DH,
    R_CS,R_DS,R_ES,R_SS,R_FS,R_GS,
    R_ST,R_ST0,R_ST1,R_ST2,R_ST3,R_ST4,R_ST5,R_ST6,R_ST7,
    R_DR0,R_DR1,R_DR2,R_DR3,R_DR6,R_DR7,
    R_CR0,R_CR2,R_CR3,R_CR4,
    R_TR3,R_TR4,R_TR5,R_TR6,R_TR7,
    R_MM0,R_MM1,R_MM2,R_MM3,R_MM4,R_MM5,R_MM6,R_MM7,
    R_XMM0,R_XMM1,R_XMM2,R_XMM3,R_XMM4,R_XMM5,R_XMM6,R_XMM7,
    R_XMM8,R_XMM9,R_XMM10,R_XMM11,R_XMM12,R_XMM13,R_XMM14,R_XMM15
  );

   { A type to store register locations for 64 Bit values. }
      tregister64 = tregister;

   { alias for compact code }
   treg64 = tregister64;

   { Set type definition for registers }
    tregisterset = set of tregister;

   { Type definition for the array of string of register names }
   reg2strtable = array[tregister] of string[6];

const
  firstreg = low(tregister);
  lastreg  = high(tregister);

  firstsreg = R_CS;
  lastsreg  = R_GS;

  regset8bit  : tregisterset = [R_AL..R_DH];
  regset16bit : tregisterset = [R_AX..R_DI,R_CS..R_SS];
  regset32bit : tregisterset = [R_EAX..R_EDI];

  { Convert reg to opsize }
  reg2opsize:array[firstreg..lastreg] of topsize = (S_NO,
    S_Q,S_Q,S_Q,S_Q,S_Q,S_Q,S_Q,S_Q,
    S_Q,S_Q,S_Q,S_Q,S_Q,S_Q,S_Q,S_Q,S_Q,
    S_L,S_L,S_L,S_L,S_L,S_L,S_L,S_L,
    S_L,S_L,S_L,S_L,S_L,S_L,S_L,S_L,
    S_W,S_W,S_W,S_W,S_W,S_W,S_W,S_W,
    S_W,S_W,S_W,S_W,S_W,S_W,S_W,S_W,
    S_B,S_B,S_B,S_B,S_B,S_B,S_B,S_B,
    S_B,S_B,S_B,S_B,S_B,S_B,S_B,S_B,
    S_B,S_B,S_B,S_B,
    S_W,S_W,S_W,S_W,S_W,S_W,
    S_FL,S_FL,S_FL,S_FL,S_FL,S_FL,S_FL,S_FL,S_FL,
    S_L,S_L,S_L,S_L,S_L,S_L,
    S_L,S_L,S_L,S_L,
    S_L,S_L,S_L,S_L,S_L,
    S_D,S_D,S_D,S_D,S_D,S_D,S_D,S_D,
    S_D,S_D,S_D,S_D,S_D,S_D,S_D,S_D,
    S_D,S_D,S_D,S_D,S_D,S_D,S_D,S_D
  );

  { Standard opcode string table (for each tasmop enumeration). The
    opcode strings should conform to the names as defined by the
    processor manufacturer.
  }
  std_op2str:op2strtable={$i x86_64in.inc}

  { Standard register table (for each tregister enumeration). The
    register strings should conform to the the names as defined
    by the processor manufacturer
  }
  std_reg2str : reg2strtable = ('',
    'rax','rcx','rdx','rbx','rsp','rbp','rsi','rdi',
    'r8','r9','r10','r11','r12','r13','r14','r15','rip',
    'eax','ecx','edx','ebx','esp','ebp','esi','edi',
    'r8d','r9d','r10d','r11d','r12d','r13d','r14d','r15d',
    'ax','cx','dx','bx','sp','bp','si','di',
    'r8w','r9w','r10w','r11w','r12w','r13w','r14w','r15w',
    'al','cl','dl','bl','spl','bpl','sil','dil',
    'r8b','r9b','r10b','r11b','r12b','r13b','r14b','r15b',
    'ah','ch','bh','dh',
    'cs','ds','es','ss','fs','gs',
    'st','st(0)','st(1)','st(2)','st(3)','st(4)','st(5)','st(6)','st(7)',
    'dr0','dr1','dr2','dr3','dr6','dr7',
    'cr0','cr2','cr3','cr4',
    'tr3','tr4','tr5','tr6','tr7',
    'mm0','mm1','mm2','mm3','mm4','mm5','mm6','mm7',
    'xmm0','xmm1','xmm2','xmm3','xmm4','xmm5','xmm6','xmm7',
    'xmm8','xmm9','xmm10','xmm11','xmm12','xmm13','xmm14','xmm15');

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
    'a','ae','b','be','c','e','g','ge','l','le','na','nae',
    'nb','nbe','nc','ne','ng','nge','nl','nle','no','np',
    'ns','nz','o','p','pe','po','s','z'
  );
  inverse_cond:array[TAsmCond] of TAsmCond=(C_None,
    C_NA,C_NAE,C_NB,C_NBE,C_NC,C_NE,C_NG,C_NGE,C_NL,C_NLE,C_A,C_AE,
    C_B,C_BE,C_C,C_E,C_G,C_GE,C_L,C_LE,C_O,C_P,
    C_S,C_Z,C_NO,C_NP,C_NP,C_P,C_NS,C_NZ
  );


{*****************************************************************************
                                   Flags
*****************************************************************************}

    type
       TResFlags = (F_E,F_NE,F_G,F_L,F_GE,F_LE,F_C,F_NC,F_A,F_AE,F_B,F_BE);

{*****************************************************************************
                                Reference
*****************************************************************************}

    type
      trefoptions=(ref_none,ref_parafixup,ref_localfixup,ref_selffixup);

      { immediate/reference record }
      preference = ^treference;
      treference = packed record
         offset      : longint;
         symbol      : tasmsymbol;
         offsetfixup : longint;
         segment,
         base,
         index       : tregister;
         scalefactor : byte;
         options     : trefoptions;
         alignment   : byte;
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
        toptype=(top_none,top_reg,top_ref,top_const,top_symbol);

        toper=record
          ot  : longint;
          case typ : toptype of
           top_none   : ();
           top_reg    : (reg:tregister);
           top_ref    : (ref:preference);
           top_const  : (val:longint);
           top_symbol : (sym:tasmsymbol;symofs:longint);
        end;

{*****************************************************************************
                               Generic Location
*****************************************************************************}

    type
       TLoc=(
         { added for tracking problems}
         LOC_INVALID,
         { contant }
         LOC_CONSTANT,
         { in a processor register }
         LOC_REGISTER,
         { in memory }
         LOC_CREFERENCE,
         { like LOC_MEM, but lvalue }
         LOC_REFERENCE,
         { boolean results only, jump to false or true label }
         LOC_JUMP,
         { boolean results only, flags are set }
         LOC_FLAGS,
         { Constant register which shouldn't be modified }
         LOC_CREGISTER,
         { MMX register }
         LOC_MMXREGISTER,
         { Constant MMX register }
         LOC_CMMXREGISTER,
         { FPU stack }
         LOC_FPUREGISTER,
         { if it is a FPU register variable on the fpu stack }
         LOC_CFPUREGISTER,
         LOC_SSEREGISTER,
         LOC_CSSEREGISTER
       );

      { tparamlocation describes where a parameter for a procedure is stored.
        References are given from the caller's point of view. The usual
        TLocation isn't used, because contains a lot of unnessary fields.
      }
      tparalocation = packed record
         loc  : TLoc;
         sp_fixup : longint;
         case TLoc of
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

  plocation = ^tlocation;
  tlocation = packed record
     loc : tloc;
     size : TCGSize;
     case  TLoc of
        LOC_FLAGS : (resflags : tresflags);
        LOC_CONSTANT : (
          case longint of
             1 : (value : AWord);
             2 : (valuelow, valuehigh:AWord);
             { overlay a complete 64 Bit value }
             3 : (valueqword : qword);
        );
        LOC_CREFERENCE,LOC_REFERENCE : (reference : treference);
        { segment in reference at the same place as in loc_register }
        LOC_REGISTER,LOC_CREGISTER : (
        case longint of
          1 : (register,segment,registerhigh : tregister);
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
       max_operands = 3;

       lvaluelocations = [LOC_REFERENCE,LOC_CFPUREGISTER,
         LOC_CREGISTER,LOC_MMXREGISTER,LOC_CMMXREGISTER];

       ALL_REGISTERS = [R_EAX..R_XMM15];

       general_registers = [R_EAX,R_EBX,R_ECX,R_EDX];

       { low and high of the available maximum width integer general purpose }
       { registers                                                           }
       LoGPReg = R_EAX;
       HiGPReg = R_EDI;

     { low and high of every possible width general purpose register (same as }
     { above on most architctures apart from the 80x86)                       }
       LoReg = R_EAX;
       HiReg = R_BL;

       intregs = general_registers;

       maxvarregs = 4;
       varregs : array[1..maxvarregs] of tregister =
         (R_EBX,R_EDX,R_ECX,R_EAX);

       usableregsint = [R_EAX,R_EBX,R_ECX,R_EDX];
       c_countusableregsint = 4;

       maxfpuvarregs = 16;

       maxintregs = maxvarregs;
       maxfpuregs = maxfpuvarregs;

       fpuregs = [R_ST0..R_ST7];
       usableregsfpu = [];
       c_countusableregsfpu = 0;

       mmregs = [R_MM0..R_MM7];
       usableregsmm = [R_XMM0..R_XMM15];
       c_countusableregsmm  = 8;

       firstsaveintreg = R_EAX;
       lastsaveintreg  = R_R15;
       firstsavefpureg = R_NO;
       lastsavefpureg  = R_NO;
       firstsavemmreg  = R_XMM0;
       lastsavemmreg   = R_XMM15;

       registers_saved_on_cdecl = [R_ESI,R_EDI,R_EBX];

       scratch_regs : array[1..1] of tregister = (R_EDI);


{*****************************************************************************
                          Default generic sizes
*****************************************************************************}
      { Defines the default address size for a processor, }
      OS_ADDR = OS_32;
      { the natural int size for a processor,             }
      OS_INT = OS_32;
      { the maximum float size for a processor,           }
      OS_FLOAT = OS_F80;
      { the size of a vector register for a processor     }
      OS_VECTOR = OS_M64;

  cpuflags = [];

  { sizes }
  pointersize   = 8;
  extended_size = 10;
  sizepostfix_pointer = S_L;


{*****************************************************************************
                          Generic Register names
*****************************************************************************}

       { location of function results }

       stack_pointer_reg = R_ESP;
       frame_pointer_reg = R_EBP;
       self_pointer_reg  = R_ESI;
       accumulator   = R_EAX;
       accumulatorhigh = R_EDX;
       { the register where the vmt offset is passed to the destructor }
       { helper routine                                                }
       vmt_offset_reg = R_EDI;

       resultreg = R_RAX;
       resultreg64 = R_RAX;
       fpuresultreg = R_ST;

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
      std_saved_registers = [R_ESI,R_EDI,R_EBX];
      { Required parameter alignment when calling a routine declared as
        stdcall and cdecl. The alignment value should be the one defined
        by GCC or the target ABI.

         The value of this constant is equal to the constant
         PARM_BOUNDARY / BITS_PER_UNIT in the GCC source.
      }
      std_param_align = 8;

{*****************************************************************************
                                  Helpers
*****************************************************************************}

    function is_calljmp(o:tasmop):boolean;

    function flags_to_cond(const f: TResFlags) : TAsmCond;


implementation

{*****************************************************************************
                                  Helpers
*****************************************************************************}

    function is_calljmp(o:tasmop):boolean;
      begin
        case o of
          A_CALL,
          A_JCXZ,
          A_JECXZ,
          A_JMP,
          A_LOOP,
          A_LOOPE,
          A_LOOPNE,
          A_LOOPNZ,
          A_LOOPZ,
          A_Jcc :
            is_calljmp:=true;
          else
            is_calljmp:=false;
        end;
      end;


    function flags_to_cond(const f: TResFlags) : TAsmCond;
      const
        flags_2_cond : array[TResFlags] of TAsmCond =
          (C_E,C_NE,C_G,C_L,C_GE,C_LE,C_C,C_NC,C_A,C_AE,C_B,C_BE);
      begin
        result := flags_2_cond[f];
      end;


end.
{
  $Log$
  Revision 1.1  2002-07-24 22:38:15  florian
    + initial release of x86-64 target code

}
