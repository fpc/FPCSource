{
    $Id$
    Copyright (c) 1995-98 by Florian Klaempfl

    This unit implements an types and classes specific for the i386+

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
unit i386;

  interface

    uses
       strings,systems,cobjects,globals,aasm,files,verbose;

    const
      extended_size = 10;

    type
       tasmop = (
         A_MOV,A_MOVZX,A_MOVSX,A_LABEL,A_ADD,
         A_CALL,A_IDIV,A_IMUL,A_JMP,A_LEA,A_MUL,A_NEG,A_NOT,
         A_POP,A_POPAD,A_PUSH,A_PUSHAD,A_RET,A_SUB,A_XCHG,A_XOR,
         A_FILD,A_CMP,A_JZ,A_INC,A_DEC,A_SETE,A_SETNE,A_SETL,
         A_SETG,A_SETLE,A_SETGE,A_JE,A_JNE,A_JL,A_JG,A_JLE,A_JGE,
         A_OR,A_FLD,A_FADD,A_FMUL,A_FSUB,A_FDIV,A_FCHS,A_FLD1,
         A_FIDIV,A_CLTD,A_JNZ,A_FSTP,A_AND,A_JNO,A_NOTH,A_NONE,
         A_ENTER,A_LEAVE,A_CLD,A_MOVS,A_REP,A_SHL,A_SHR,A_BOUND,
         A_JNS,A_JS,A_JO,A_SAR,A_TEST,
         A_FCOM,A_FCOMP,A_FCOMPP,A_FXCH,A_FADDP,A_FMULP,A_FSUBP,A_FDIVP,
         A_FNSTS,A_SAHF,A_FDIVRP,A_FSUBRP,A_SETC,A_SETNC,A_JC,A_JNC,
         A_JA,A_JAE,A_JB,A_JBE,A_SETA,A_SETAE,A_SETB,A_SETBE,
         A_AAA,A_AAD,A_AAM,A_AAS,A_CBW,A_CDQ,A_CLC,A_CLI,
         A_CLTS,A_CMC,A_CWD,A_CWDE,A_DAA,A_DAS,A_HLT,A_IRET,A_LAHF,
         A_LODS,A_LOCK,A_NOP,A_PUSHA,A_PUSHF,A_PUSHFD,
         A_STC,A_STD,A_STI,A_STOS,A_WAIT,A_XLAT,A_XLATB,A_MOVSB,
         A_MOVSBL,A_MOVSBW,A_MOVSWL,A_MOVZB,A_MOVZWL,A_POPA,A_IN,
         A_OUT,A_LDS,A_LCS,A_LES,A_LFS,A_LGS,A_LSS,A_POPF,A_SBB,A_ADC,
         A_DIV,A_ROR,A_ROL,A_RCL,A_RCR,A_SAL,A_SHLD,A_SHRD,
         A_LCALL,A_LJMP,A_LRET,A_JNAE,A_JNB,A_JNA,A_JNBE,A_JP,A_JNP,
         A_JPE,A_JPO,A_JNGE,A_JNG,A_JNL,A_JNLE,A_JCXZ,A_JECXZ,
         A_LOOP,A_CMPS,A_INS,A_OUTS,A_SCAS,A_BSF,A_BSR,A_BT,A_BTC,A_BTR,A_BTS,A_INT,
         A_INT3,A_INTO,A_BOUNDL,A_BOUNDW,
         A_LOOPZ,A_LOOPE,A_LOOPNZ,A_LOOPNE,A_SETO,A_SETNO,A_SETNAE,A_SETNB,
         A_SETZ,A_SETNZ,A_SETNA,A_SETNBE,A_SETS,A_SETNS,A_SETP,A_SETPE,A_SETNP,
         A_SETPO,A_SETNGE,A_SETNL,A_SETNG,A_SETNLE,A_ARPL,A_LAR,A_LGDT,A_LIDT,
         A_LLDT,A_LMSW,A_LSL,A_LTR,A_SGDT,A_SIDT,A_SLDT,A_SMSW,A_STR,A_VERR,A_VERW,
         A_FABS,A_FBLD,A_FBSTP,A_FCLEX,A_FNCLEX,
         A_FCOS,A_FDECSTP,A_FDISI,A_FNDISI,
         A_FDIVR,A_FENI,A_FNENI,A_FFREE,A_FIADD,A_FICOM,A_FICOMP,
         A_FIDIVR,A_FIMUL,A_FINCSTP,A_FINIT,A_FNINIT,A_FIST,A_FISTP,A_FISUB,
         A_FISUBR,A_FLDCW,A_FLDENV,A_FLDLG2,A_FLDLN2,A_FLDL2E,
         A_FLDL2T,A_FLDPI,A_FLDS,A_FLDZ,A_FNOP,A_FPATAN,
         A_FPREM,A_FPREM1,A_FPTAN,A_FRNDINT,A_FRSTOR,A_FSAVE,A_FNSAVE,
         A_FSCALE,A_FSETPM,A_FSIN,A_FSINCOS,A_FSQRT,A_FST,A_FSTCW,A_FNSTCW,
         A_FSTENV,A_FNSTENV,A_FSTSW,A_FNSTSW,A_FTST,A_FUCOM,A_FUCOMP,
         A_FUCOMPP,A_FWAIT,A_FXAM,A_FXTRACT,A_FYL2X,A_FYL2XP1,A_F2XM1,
         A_FILDQ,A_FILDS,A_FILDL,A_FLDL,A_FLDT,A_FISTQ,A_FISTS,A_FISTL,A_FSTL,A_FSTS,
         A_FSTPS,A_FISTPL,A_FSTPL,A_FISTPS,A_FISTPQ,A_FSTPT,
         A_FCOMPS,A_FICOMPL,A_FCOMPL,A_FICOMPS,
         A_FCOMS,A_FICOML,A_FCOML,A_FICOMS,A_FIADDL,A_FADDL,A_FIADDS,
         A_FISUBL,A_FSUBL,A_FISUBS,A_FSUBS,A_FSUBR,A_FSUBRS,A_FISUBRL,
         A_FSUBRL,A_FISUBRS,A_FMULS,A_FIMULL,A_FMULL,A_FIMULS,A_FDIVS,A_FIDIVL,
         A_FDIVL,A_FIDIVS,A_FDIVRS,A_FIDIVRL,A_FDIVRL,A_FIDIVRS,
         A_REPE,A_REPNE,A_FADDS,A_POPFD,
         { MMX instructions: }
         A_EMMS,A_MOVD,A_MOVQ,A_PACKSSDW,A_PACKSSWB,A_PACKUSWB,
         A_PADDB,A_PADDD,A_PADDSB,A_PADDSW,A_PADDUSB,A_PADDUSW,
         A_PADDW,A_PAND,A_PANDN,A_PCMPEQB,A_PCMPEQD,A_PCMPEQW,
         A_PCMPGTB,A_PCMPGTD,A_PCMPGTW,A_PMADDWD,A_PMULHW,
         A_PMULLW,A_POR,A_PSLLD,A_PSLLQ,A_PSLLW,A_PSRAD,A_PSRAW,
         A_PSRLD,A_PSRLQ,A_PSRLW,A_PSUBB,A_PSUBD,A_PSUBSB,A_PSUBSW,
         A_PSUBUSB,A_PSUBUSW,A_PSUBW,A_PUNPCKHBW,A_PUNPCKHDQ,
         A_PUNPCKHWD,A_PUNPCKLBW,A_PUNPCKLDQ,A_PUNPCKLWD,A_PXOR);
    const
      firstop = A_MOV;
      lastop  = A_PXOR;

    type
       { enumeration for registers, don't change this }
       { it's used by the register size converstaions }
       tregister = (
         R_NO,R_EAX,R_ECX,R_EDX,R_EBX,R_ESP,R_EBP,R_ESI,R_EDI,
         R_AX,R_CX,R_DX,R_BX,R_SP,R_BP,R_SI,R_DI,
         R_AL,R_CL,R_DL,R_BL,R_AH,R_CH,R_BH,R_DH,
         { for an easier assembler generation }
         R_DEFAULT_SEG,R_CS,R_DS,R_ES,R_FS,R_GS,R_SS,
         R_ST,R_ST0,R_ST1,R_ST2,R_ST3,R_ST4,R_ST5,R_ST6,R_ST7,
         R_MM0,R_MM1,R_MM2,R_MM3,R_MM4,R_MM5,R_MM6,R_MM7);

       { S_NO = No Size of operand }
       { S_B  = Byte size operand  }
       { S_W  = Word size operand  }
       { S_L  = DWord size operand }
       { USED FOR conversions in x86}
       { S_BW = Byte to word       }
       { S_BL = Byte to long       }
       { S_WL = Word to long       }
       { Floating point types      }
       { S_FX  = Extended type      }
       { S_FL  = double/64bit integer }
       { S_FS  = single type (32 bit) }
       { S_IQ  = integer on 64 bits   }
       { S_IL  = integer on 32 bits   }
       { S_IS  = integer on 16 bits   }
       { S_D   = integer on  bits for MMX }
       topsize = (S_NO,S_B,S_W,S_L,S_BW,S_BL,S_WL,
                  S_IS,S_IL,S_IQ,S_FS,S_FL,S_FX,S_D);
       { S_FS and S_FL added
         S_X renamed to S_FX
         S_IL added
         S_S and S_Q renamed to S_IQ and S_IS
         S_F? means a real load or store or read
         added to distinguish between longint l suffix like 'movl'
         and double l suffix 'fldl'
         distinction needed for intel output !! }

       plocation = ^tlocation;

       { information about the location of an operand }
       { LOC_FPUSTACK    FPU stack }
       { LOC_REGISTER    in a processor register }
       { LOC_MEM         in the memory }
       { LOC_REFERENCE   like LOC_MEM, but lvalue }
       { LOC_JUMP        nur bool'sche Resultate, Sprung zu false- oder }
       {                 truelabel }
       { LOC_FLAGS       nur bool'sche Rsultate, Flags sind gesetzt }
       { LOC_CREGISTER   register which shouldn't be modified }
       { LOC_INVALID     added for tracking problems}

       tloc = (LOC_INVALID,LOC_FPU,LOC_REGISTER,LOC_MEM,LOC_REFERENCE,LOC_JUMP,
               LOC_FLAGS,LOC_CREGISTER,LOC_MMXREGISTER,LOC_CMMXREGISTER);

       tresflags = (F_E,F_NE,F_G,F_L,F_GE,F_LE,F_C,F_NC,
                    F_A,F_AE,F_B,F_BE);

       preference = ^treference;

       treference = record
          base,segment,index : tregister;
          offset : longint;
          symbol : pstring;
          { a constant is also a treference, this makes the code generator }
          { easier                                                         }
          isintvalue : boolean;
          scalefactor : byte;
       end;

       tlocation = record
          case loc : tloc of
             { segment in reference at the same place as in loc_register }
             LOC_REGISTER,LOC_CREGISTER : (register,segment : tregister);
             LOC_MEM,LOC_REFERENCE : (reference : treference);
             LOC_FPU : ();
             LOC_JUMP : ();
             LOC_FLAGS : (resflags : tresflags);
             LOC_INVALID : ();

             { it's only for better handling }
             LOC_MMXREGISTER : (mmxreg : tregister);
       end;

       pcsymbol = ^tcsymbol;

       tcsymbol = record
          symbol : pchar;
          offset : longint;
       end;

    const
       { arrays for boolean location conversions }
       flag_2_jmp : array[F_E..F_BE] of tasmop =
          (A_JE,A_JNE,A_JG,A_JL,A_JGE,A_JLE,A_JC,A_JNC,
           A_JA,A_JAE,A_JB,A_JBE);

       flag_2_set : array[F_E..F_BE] of tasmop =        { v-- the GAS didn't know setc }
          (A_SETE,A_SETNE,A_SETG,A_SETL,A_SETGE,A_SETLE,A_SETB,A_SETAE,
           A_SETA,A_SETAE,A_SETB,A_SETBE);

       { operand types }
       top_none = 0;
       top_reg = 1;
       top_ref = 2;

       { a constant can be also written as treference }
       top_const = 3;

       { this is for calls }
       top_symbol = 4;

       stack_pointer = R_ESP;

       frame_pointer = R_EBP;

       {This constant is an alias for the accumulator, as it's name may
        differ from processor to processor.}
       accumulator = R_EAX;

    type

       pai_labeled = ^tai_labeled;

       tai_labeled = object(tai)
          _operator : tasmop;
          lab : plabel;
          constructor init(op : tasmop; l : plabel);
          destructor done;virtual;
       end;

{$ifdef REGALLOC}

       pairegalloc = ^tairegalloc;

       tairegalloc = object(tai)
          reg : tregister;
          constructor init(r : tregister);
       end;

       pairegdealloc = ^tairegdealloc;

       tairegdealloc = object(tai)
          reg : tregister;
          constructor init(r : tregister);
       end;

{$endif REGALLOC}

       pai386 = ^tai386;

       tai386 = object(tai)
          { this isn't a proper style, but not very memory expensive }
          op1,op2: pointer;
          _operator : tasmop;
          opxt:word;
          size:topsize;
          constructor op_none(op : tasmop;_size : topsize);

          constructor op_reg(op : tasmop;_size : topsize;_op1 : tregister);
          constructor op_const(op : tasmop;_size : topsize;_op1 : longint);
          constructor op_ref(op : tasmop;_size : topsize;_op1 : preference);
          constructor op_loc(op : tasmop;_size : topsize;_op1 : tlocation);

          constructor op_reg_reg(op : tasmop;_size : topsize;_op1,_op2 : tregister);
          constructor op_reg_ref(op : tasmop;_size : topsize;_op1 : tregister;_op2 : preference);
          constructor op_reg_loc(op : tasmop;_size : topsize;_op1 : tregister;_op2 : tlocation);
          constructor op_loc_reg(op : tasmop;_size : topsize;_op1 : tlocation;_op2 : tregister);

          constructor op_const_reg(op : tasmop;_size : topsize;_op1 : longint;_op2 : tregister);
          { this combination is needed by ENTER }
          constructor op_const_const(op : tasmop;_size : topsize;_op1,_op2 : longint);
          constructor op_const_ref(op : tasmop;_size : topsize;_op1 : longint;_op2 : preference);
          constructor op_const_loc(op : tasmop;_size : topsize;_op1 : longint;_op2 : tlocation);

          constructor op_ref_reg(op : tasmop;_size : topsize;_op1 : preference;_op2 : tregister);
          { this is only allowed if _op1 is an int value (_op1^.isintvalue=true) }
          constructor op_ref_ref(op : tasmop;_size : topsize;_op1,_op2 : preference);
          {
          constructor op_ref_loc(op : tasmop;_size : topsize;_op1 : preference;_op2 : tlcation);}

          constructor op_const_reg_reg(op : tasmop;_size : topsize;_op1 : longint;_op2 : tregister;_op3 : tregister);

          { this is for CALL etc.                            }
          { symbol is replaced by the address of symbol      }
          { so op_csymbol(A_PUSH,S_L,strnew('P')); generates }
          { an instruction which pushes the address of P     }
          { to the stack                                     }
          constructor op_csymbol(op : tasmop;_size : topsize;_op1 : pcsymbol);
          constructor op_csymbol_reg(op : tasmop;_size : topsize;_op1 : pcsymbol;_op2 : tregister);
          constructor op_csymbol_ref(op : tasmop;_size : topsize;_op1 : pcsymbol;_op2 : preference);
          constructor op_csymbol_loc(op : tasmop;_size : topsize;_op1 : pcsymbol;_op2 : tlocation);
          { OUT immediate8  }
          constructor op_reg_const(op:tasmop; _size: topsize; _op1: tregister; _op2: longint);
          function op1t:byte;
          function op2t:byte;
          function op3t:byte;
          destructor done;virtual;
       end;



    const
       maxvarregs = 4;

       varregs : array[1..maxvarregs] of tregister =
         (R_EBX,R_EDX,R_ECX,R_EAX);

       nextlabelnr : longint = 1;

    { the following functions allow to convert registers }
    { for example reg8toreg32(R_AL) returns R_EAX        }
    { for example reg16toreg32(R_AL) gives an undefined  }
    { result                                             }
    { these functions expects that the turn of           }
    { tregister isn't changed                            }
    function reg8toreg16(reg : tregister) : tregister;
    function reg8toreg32(reg : tregister) : tregister;
    function reg16toreg8(reg : tregister) : tregister;
    function reg32toreg8(reg : tregister) : tregister;
    function reg32toreg16(reg : tregister) : tregister;
    function reg16toreg32(reg : tregister) : tregister;

    { resets all values of ref to defaults }
    procedure reset_reference(var ref : treference);
    { mostly set value of a reference }
    function new_reference(base : tregister;offset : longint) : preference;
    { same as reset_reference, but symbol is disposed }
    { use this only for already used references       }
    procedure clear_reference(var ref : treference);

    { make l as a new label }
    procedure getlabel(var l : plabel);
    { frees the label if unused }
    procedure freelabel(var l : plabel);
    { make a new zero label }
    procedure getzerolabel(var l : plabel);
    { reset a label to a zero label }
    procedure setzerolabel(var l : plabel);
    {just get a label number }
    procedure getlabelnr(var l : longint);

    function newreference(const r : treference) : preference;

    function reg2str(r : tregister) : string;

    { generates an help record for constants }
    function newcsymbol(const s : string;l : longint) : pcsymbol;

    function lab2str(l : plabel) : string;

    const
       ao_unknown = $0;
       { 8 bit reg }
       ao_reg8 = $1;
       { 16 bit reg }
       ao_reg16 = $2;
       { 32 bit reg }
       ao_reg32 = $4;
       ao_reg = (ao_reg8 or ao_reg16 or ao_reg32);

       { for  push/pop operands }
       ao_wordreg = (ao_reg16 or ao_reg32);
       ao_imm8 = $8;        { 8 bit immediate }
       ao_imm8S   = $10;        { 8 bit immediate sign extended }
       ao_imm16   = $20;        { 16 bit immediate }
       ao_imm32   = $40;        { 32 bit immediate }
       ao_imm1    = $80;        { 1 bit immediate }

       { for  unknown expressions }
       ao_immunknown = ao_imm32;

       { gen'l immediate }
       ao_imm = (ao_imm8 or ao_imm8S or ao_imm16 or ao_imm32);
       ao_disp8   = $200;       { 8 bit displacement (for  jumps) }
       ao_disp16  = $400;       { 16 bit displacement }
       ao_disp32  = $800;       { 32 bit displacement }

       { general displacement }
       ao_disp    = (ao_disp8 or ao_disp16 or ao_disp32);

       { for  unknown size displacements }
       ao_dispunknown = ao_disp32;
       ao_mem8    = $1000;
       ao_mem16   = $2000;
       ao_mem32   = $4000;
       ao_baseindex = $8000;

       { general mem }
       ao_mem     = (ao_disp or ao_mem8 or ao_mem16 or ao_mem32 or ao_baseindex);
       ao_wordmem = (ao_mem16 or ao_mem32 or ao_disp or ao_baseindex);
       ao_bytemem = (ao_mem8 or ao_disp or ao_baseindex);

       { register to hold in/out port addr = dx }
       ao_inoutportreg = $10000;
       { register to hold shift cound = cl }
       ao_shiftcount = $20000;
       ao_control = $40000; { Control register }
       ao_debug   = $80000; { Debug register }
       ao_test    = $100000;    { Test register }

       { suggestion from PM }
       { st0 is also a float reg }

       {ao_floatreg = $200000;  }{ Float register }
       ao_otherfloatreg = $200000;  { Float register different from st0 }
       ao_floatacc = $400000;   { Float stack top %st(0) }
       ao_floatreg = ao_otherfloatreg or ao_floatacc; { all float regs }

       { Florian correct this if it is wrong
         but it seems necessary for ratti386 to accept the code
         in i386/math.inc !! }

       { 2 bit segment register }
       ao_sreg2   = $800000;

       { 3 bit segment register }
       ao_sreg3   = $1000000;

       { Accumulat or  %al  or  %ax  or  %eax }
       ao_acc  = $2000000;
       ao_implicitregister = (ao_inoutportreg or ao_shiftcount or ao_acc or ao_floatacc);
       ao_jumpabsolute = $4000000;
       ao_abs8 = $08000000;
       ao_abs16 = $10000000;
       ao_abs32 = $20000000;
       ao_abs = (ao_abs8 or ao_abs16 or ao_abs32);

       ao_none = $ff;


       { this is for the code generator }
       { set if operands are words or dwords }
       af_w       = $1;
       { D = 0 if Reg --> Regmem; D = 1 if Regmem --> Reg }
       af_d        = $2;
       { direction flag for floating insns:  MUST BE = $400 }
       af_floatd = $400;
       { shorthand }
       af_dw = (af_d or af_w);
       { register is in low 3 bits of opcode }
       shortform = $10;
       { shortform and w-bit is=$8 }
       Shortformw = $20;
       seg2shortform = $40; { encoding of load segment reg insns }
       seg3shortform = $80; { fs/gs segment register insns. }
       jump = $100;     { special case for jump insns. }
       jumpintersegment = $200; { special case for intersegment leaps/calls }
       dont_use = $400;
       noModrm = $800;
       modrm = $1000;
       imulkludge = $2000;
       Jumpbyte = $4000;
       Jumpdword = $8000;
       af_ReverseRegRegmem = $10000;

    type
       ttemplate = record
          i : tasmop;
          ops : byte;
          oc : longint;
          eb : byte;
          m : longint;
          o1,o2,o3 : longint;
       end;

       tins_cache = array[A_MOV..A_POPFD] of longint;

    var
       ins_cache : tins_cache;
       exprasmlist : paasmoutput;

    const
       it : array[0..440] of ttemplate = (
         (i : A_MOV;ops : 2;oc : $a0;eb : ao_none;m : af_dw or NoModrm;o1 : ao_disp32;o2 : ao_acc;o3 : 0 ),
         (i : A_MOV;ops : 2;oc : $88;eb : ao_none;m : af_dw or Modrm;o1 : ao_reg;o2 : ao_reg or ao_mem;o3 : 0 ),
         (i : A_MOV;ops : 2;oc : $b0;eb : ao_none;m : ShortFormW;o1 : ao_imm;o2 : ao_reg;o3 : 0 ),
         (i : A_MOV;ops : 2;oc : $c6;eb : ao_none;m : af_w or Modrm;o1 : ao_imm;o2 : ao_reg or ao_mem;o3 : 0 ),
         (i : A_MOV;ops : 2;oc : $8c;eb : ao_none;m : af_d or Modrm;o1 : ao_sreg3 or ao_sreg2;o2 : ao_reg16 or
           ao_mem16;o3 : 0 ),
         (i : A_MOV;ops : 2;oc : $0f20;eb : ao_none;m : af_d or Modrm;o1 : ao_control;o2 : ao_reg32;o3 : 0),
         (i : A_MOV;ops : 2;oc : $0f21;eb : ao_none;m : af_d or Modrm;o1 : ao_debug;o2 : ao_reg32;o3 : 0),
         (i : A_MOV;ops : 2;oc : $0f24;eb : ao_none;m : af_d or Modrm;o1 : ao_test;o2 : ao_reg32;o3 : 0),
         (i : A_MOVSB;ops : 2;oc : $0fbe;eb : ao_none;m : af_reverseregregmem or Modrm;o1 : ao_reg8 or ao_mem;o2 : ao_reg16
           or ao_reg32;o3 : 0),
         (i : A_MOVSBL;ops : 2;oc : $0fbe;eb : ao_none;m : af_reverseregregmem or Modrm;o1 : ao_reg8 or ao_mem;
           o2 : ao_reg32;o3 : 0),
         (i : A_MOVSBW;ops : 2;oc : $660fbe;eb : ao_none;m : af_reverseregregmem or Modrm;o1 : ao_reg8 or ao_mem;
           o2 : ao_reg16;o3 : 0),
         (i : A_MOVSWL;ops : 2;oc : $0fbf;eb : ao_none;m : af_reverseregregmem or Modrm;o1 : ao_reg16 or ao_mem;
           o2 : ao_reg32;o3 : 0),
         (i : A_MOVZB;ops : 2;oc : $0fb6;eb : ao_none;m : af_reverseregregmem or Modrm;o1 : ao_reg8 or ao_mem;
           o2 : ao_reg16 or ao_reg32;o3 : 0),
         (i : A_MOVZWL;ops : 2;oc : $0fb7;eb : ao_none;m : af_reverseregregmem or Modrm;o1 : ao_reg16 or ao_mem;
           o2 : ao_reg32;o3 : 0),
         (i : A_PUSH;ops : 1;oc : $50;eb : ao_none;m : ShortForm;o1 : ao_wordreg;o2 : 0;o3 : 0 ),
         (i : A_PUSH;ops : 1;oc : $ff;eb : $6;m : Modrm;o1 : ao_wordreg or ao_wordMem;o2 : 0;o3 : 0 ),
         (i : A_PUSH;ops : 1;oc : $6a;eb : ao_none;m : NoModrm;o1 : ao_imm8S;o2 : 0;o3 : 0),
         (i : A_PUSH;ops : 1;oc : $68;eb : ao_none;m : NoModrm;o1 : ao_imm32 or ao_imm16;o2 : 0;o3 : 0),
         (i : A_PUSH;ops : 1;oc : $06;eb : ao_none;m : Seg2ShortForm;o1 : ao_sreg2;o2 : 0;o3 : 0 ),
         (i : A_PUSH;ops : 1;oc : $0fa0;eb : ao_none;m : Seg3ShortForm;o1 : ao_sreg3;o2 : 0;o3 : 0 ),
         (i : A_PUSHA;ops : 0;oc : $60;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0 ),
         (i : A_PUSHAD; ops: 0; oc: $6660;eb: ao_none;m: NoModRm;o1:   0;o2:  0;o3:  0 ),
         (i : A_POP;ops : 1;oc : $58;eb : ao_none;m : ShortForm;o1 : ao_wordreg;o2 : 0;o3 : 0 ),
         (i : A_POP;ops : 1;oc : $8f;eb : $0;m : Modrm;o1 : ao_wordreg or ao_wordmem;o2 : 0;o3 : 0 ),
         (i : A_POP;ops : 1;oc : $07;eb : ao_none;m : Seg2ShortForm;o1 : ao_sreg2;o2 : 0;o3 : 0 ),
         (i : A_POP;ops : 1;oc : $0fa1;eb : ao_none;m : Seg3ShortForm;o1 : ao_sreg3;o2 : 0;o3 : 0 ),
         (i : A_POPA;ops : 0;oc : $61;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0 ),
         (i : A_POPAD; ops: 0; oc: $6661;eb: ao_none;m : NoModRm;o1 : 0;o2 : 0;o3: 0),
         (i : A_XCHG;ops : 2;oc : $90;eb : ao_none;m : ShortForm;o1 : ao_wordreg;o2 : ao_acc;o3 : 0 ),
         (i : A_XCHG;ops : 2;oc : $90;eb : ao_none;m : ShortForm;o1 : ao_acc;o2 : ao_wordreg;o3 : 0 ),
         (i : A_XCHG;ops : 2;oc : $86;eb : ao_none;m : af_w or Modrm;o1 : ao_reg;o2 : ao_reg or ao_mem;o3 : 0 ),
         (i : A_XCHG;ops : 2;oc : $86;eb : ao_none;m : af_w or Modrm;o1 : ao_reg or ao_mem;o2 : ao_reg;o3 : 0 ),
         (i : A_IN;ops : 2;oc : $e4;eb : ao_none;m : af_w or NoModrm;o1 : ao_imm8;o2 : ao_acc;o3 : 0 ),
         (i : A_IN;ops : 2;oc : $ec;eb : ao_none;m : af_w or NoModrm;o1 : ao_inoutportreg;o2 : ao_acc;o3 : 0 ),
         (i : A_OUT;ops : 2;oc : $e6;eb : ao_none;m : af_w or NoModrm;o1 : ao_acc;o2 : ao_imm8;o3 : 0 ),
         (i : A_OUT;ops : 2;oc : $ee;eb : ao_none;m : af_w or NoModrm;o1 : ao_acc;o2 : ao_inoutportreg;o3 : 0 ),
         (i : A_LEA;ops : 2;oc : $8d;eb : ao_none;m : Modrm;o1 : ao_wordmem;o2 : ao_wordreg;o3 : 0 ),
         (i : A_LDS;ops : 2;oc : $c5;eb : ao_none;m : Modrm;o1 : ao_mem;o2 : ao_reg32;o3 : 0),
         (i : A_LES;ops : 2;oc : $c4;eb : ao_none;m : Modrm;o1 : ao_mem;o2 : ao_reg32;o3 : 0),
         (i : A_LFS;ops : 2;oc : $0fb4;eb : ao_none;m : Modrm;o1 : ao_mem;o2 : ao_reg32;o3 : 0),
         (i : A_LGS;ops : 2;oc : $0fb5;eb : ao_none;m : Modrm;o1 : ao_mem;o2 : ao_reg32;o3 : 0),
         (i : A_LSS;ops : 2;oc : $0fb2;eb : ao_none;m : Modrm;o1 : ao_mem;o2 : ao_reg32;o3 : 0),
         (i : A_CLC;ops : 0;oc : $f8;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_CLD;ops : 0;oc : $fc;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_CLI;ops : 0;oc : $fa;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_CLTS;ops : 0;oc : $0f06;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_CMC;ops : 0;oc : $f5;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_LAHF;ops : 0;oc : $9f;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_SAHF;ops : 0;oc : $9e;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_PUSHF;ops : 0;oc : $9c;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_PUSHFD; ops: 0; oc: $669c; eb: ao_none; m: NoModRm; o1: 0;o2: 0;o3: 0),
         (i : A_POPF;ops : 0;oc : $9d;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_POPFD;ops: 0;oc:  $669d;eb : ao_none;m : NoModRm;o1:  0;o2 : 0;o3 : 0),
         (i : A_STC;ops : 0;oc : $f9;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_STD;ops : 0;oc : $fd;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_STI;ops : 0;oc : $fb;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_ADD;ops : 2;oc : $0;eb : ao_none;m : af_dw or Modrm;o1 : ao_reg;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_ADD;ops : 2;oc : $83;eb : 0;m : Modrm;o1 : ao_imm8s;o2 : ao_wordreg or ao_wordmem;o3 : 0),
         (i : A_ADD;ops : 2;oc : $4;eb : ao_none;m : af_w or NoModrm;o1 : ao_imm;o2 : ao_acc;o3 : 0),
         (i : A_ADD;ops : 2;oc : $80;eb : 0;m : af_w or Modrm;o1 : ao_imm;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_INC;ops : 1;oc : $40;eb : ao_none;m : ShortForm;o1 : ao_wordreg;o2 : 0;o3 : 0),
         (i : A_INC;ops : 1;oc : $fe;eb : 0;m : af_w or Modrm;o1 : ao_reg or ao_mem;o2 : 0;o3 : 0),
         (i : A_SUB;ops : 2;oc : $28;eb : ao_none;m : af_dw or Modrm;o1 : ao_reg;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_SUB;ops : 2;oc : $83;eb : 5;m : Modrm;o1 : ao_imm8s;o2 : ao_wordreg or ao_wordmem;o3 : 0),
         (i : A_SUB;ops : 2;oc : $2c;eb : ao_none;m : af_w or NoModrm;o1 : ao_imm;o2 : ao_acc;o3 : 0),
         (i : A_SUB;ops : 2;oc : $80;eb : 5;m : af_w or Modrm;o1 : ao_imm;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_DEC;ops : 1;oc : $48;eb : ao_none;m : ShortForm;o1 : ao_wordreg;o2 : 0;o3 : 0),
         (i : A_DEC;ops : 1;oc : $fe;eb : 1;m : af_w or Modrm;o1 : ao_reg or ao_mem;o2 : 0;o3 : 0),
         (i : A_SBB;ops : 2;oc : $18;eb : ao_none;m : af_dw or Modrm;o1 : ao_reg;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_SBB;ops : 2;oc : $83;eb : 3;m : Modrm;o1 : ao_imm8s;o2 : ao_wordreg or ao_wordmem;o3 : 0),
         (i : A_SBB;ops : 2;oc : $1c;eb : ao_none;m : af_w or NoModrm;o1 : ao_imm;o2 : ao_acc;o3 : 0),
         (i : A_SBB;ops : 2;oc : $80;eb : 3;m : af_w or Modrm;o1 : ao_imm;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_CMP;ops : 2;oc : $38;eb : ao_none;m : af_dw or Modrm;o1 : ao_reg;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_CMP;ops : 2;oc : $83;eb : 7;m : Modrm;o1 : ao_imm8s;o2 : ao_wordreg or ao_wordmem;o3 : 0),
         (i : A_CMP;ops : 2;oc : $3c;eb : ao_none;m : af_w or NoModrm;o1 : ao_imm;o2 : ao_acc;o3 : 0),
         (i : A_CMP;ops : 2;oc : $80;eb : 7;m : af_w or Modrm;o1 : ao_imm;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_TEST;ops : 2;oc : $84;eb : ao_none;m : af_w or Modrm;o1 : ao_reg or ao_mem;o2 : ao_reg;o3 : 0),
         (i : A_TEST;ops : 2;oc : $84;eb : ao_none;m : af_w or Modrm;o1 : ao_reg;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_TEST;ops : 2;oc : $a8;eb : ao_none;m : af_w or NoModrm;o1 : ao_imm;o2 : ao_acc;o3 : 0),
         (i : A_TEST;ops : 2;oc : $f6;eb : 0;m : af_w or Modrm;o1 : ao_imm;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_AND;ops : 2;oc : $20;eb : ao_none;m : af_dw or Modrm;o1 : ao_reg;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_AND;ops : 2;oc : $83;eb : 4;m : Modrm;o1 : ao_imm8s;o2 : ao_wordreg or ao_wordmem;o3 : 0),
         (i : A_AND;ops : 2;oc : $24;eb : ao_none;m : af_w or NoModrm;o1 : ao_imm;o2 : ao_acc;o3 : 0),
         (i : A_AND;ops : 2;oc : $80;eb : 4;m : af_w or Modrm;o1 : ao_imm;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_OR;ops : 2;oc : $08;eb : ao_none;m : af_dw or Modrm;o1 : ao_reg;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_OR;ops : 2;oc : $83;eb : 1;m : Modrm;o1 : ao_imm8s;o2 : ao_wordreg or ao_wordmem;o3 : 0),
         (i : A_OR;ops : 2;oc : $0c;eb : ao_none;m : af_w or NoModrm;o1 : ao_imm;o2 : ao_acc;o3 : 0),
         (i : A_OR;ops : 2;oc : $80;eb : 1;m : af_w or Modrm;o1 : ao_imm;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_XOR;ops : 2;oc : $30;eb : ao_none;m : af_dw or Modrm;o1 : ao_reg;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_XOR;ops : 2;oc : $83;eb : 6;m : Modrm;o1 : ao_imm8s;o2 : ao_wordreg or ao_wordmem;o3 : 0),
         (i : A_XOR;ops : 2;oc : $34;eb : ao_none;m : af_w or NoModrm;o1 : ao_imm;o2 : ao_acc;o3 : 0),
         (i : A_XOR;ops : 2;oc : $80;eb : 6;m : af_w or Modrm;o1 : ao_imm;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_ADC;ops : 2;oc : $10;eb : ao_none;m : af_dw or Modrm;o1 : ao_reg;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_ADC;ops : 2;oc : $83;eb : 2;m : Modrm;o1 : ao_imm8s;o2 : ao_wordreg or ao_wordmem;o3 : 0),
         (i : A_ADC;ops : 2;oc : $14;eb : ao_none;m : af_w or NoModrm;o1 : ao_imm;o2 : ao_acc;o3 : 0),
         (i : A_ADC;ops : 2;oc : $80;eb : 2;m : af_w or Modrm;o1 : ao_imm;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_NEG;ops : 1;oc : $f6;eb : 3;m : af_w or Modrm;o1 : ao_reg or ao_mem;o2 : 0;o3 : 0),
         (i : A_NOT;ops : 1;oc : $f6;eb : 2;m : af_w or Modrm;o1 : ao_reg or ao_mem;o2 : 0;o3 : 0),
         (i : A_AAA;ops : 0;oc : $37;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_AAS;ops : 0;oc : $3f;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_DAA;ops : 0;oc : $27;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_DAS;ops : 0;oc : $2f;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_AAD;ops : 0;oc : $d50a;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_AAM;ops : 0;oc : $d40a;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_CBW;ops : 0;oc : $6698;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_CWD;ops : 0;oc : $6699;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_CWDE;ops : 0;oc : $98;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_CDQ;ops : 0;oc : $99;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_MUL;ops : 1;oc : $f6;eb : 4;m : af_w or Modrm;o1 : ao_reg or ao_mem;o2 : 0;o3 : 0),
         (i : A_IMUL;ops : 1;oc : $f6;eb : 5;m : af_w or Modrm;o1 : ao_reg or ao_mem;o2 : 0;o3 : 0),
         (i : A_IMUL;ops : 2;oc : $0faf;eb : ao_none;m : Modrm or af_reverseregregmem;o1 : ao_wordreg or ao_mem;
           o2 : ao_wordreg;o3 : 0),
         (i : A_IMUL;ops : 3;oc : $6b;eb : ao_none;m : Modrm or af_reverseregregmem;o1 : ao_imm8s;
           o2 : ao_wordreg or ao_mem;o3 : ao_wordreg),
         (i : A_IMUL;ops : 3;oc : $69;eb : ao_none;m : Modrm or af_reverseregregmem;o1 : ao_imm16 or ao_imm32;
           o2 : ao_wordreg or ao_mem;o3 : ao_wordreg),
         (i : A_IMUL;ops : 2;oc : $6b;eb : ao_none;m : Modrm or imulKludge;o1 : ao_imm8s;o2 : ao_wordreg;o3 : 0),
         (i : A_IMUL;ops : 2;oc : $69;eb : ao_none;m : Modrm or imulKludge;o1 : ao_imm16 or ao_imm32;o2 : ao_wordreg;o3 : 0),
         (i : A_DIV;ops : 1;oc : $f6;eb : 6;m : af_w or Modrm;o1 : ao_reg or ao_mem;o2 : 0;o3 : 0),
         (i : A_DIV;ops : 2;oc : $f6;eb : 6;m : af_w or Modrm;o1 : ao_reg or ao_mem;o2 : ao_acc;o3 : 0),
         (i : A_IDIV;ops : 1;oc : $f6;eb : 7;m : af_w or Modrm;o1 : ao_reg or ao_mem;o2 : 0;o3 : 0),
         (i : A_IDIV;ops : 2;oc : $f6;eb : 7;m : af_w or Modrm;o1 : ao_reg or ao_mem;o2 : ao_acc;o3 : 0),
         (i : A_ROL;ops : 2;oc : $d0;eb : 0;m : af_w or Modrm;o1 : ao_imm1;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_ROL;ops : 2;oc : $c0;eb : 0;m : af_w or Modrm;o1 : ao_imm8;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_ROL;ops : 2;oc : $d2;eb : 0;m : af_w or Modrm;o1 : ao_shiftcount;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_ROL;ops : 1;oc : $d0;eb : 0;m : af_w or Modrm;o1 : ao_reg or ao_mem;o2 : 0;o3 : 0),
         (i : A_ROR;ops : 2;oc : $d0;eb : 1;m : af_w or Modrm;o1 : ao_imm1;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_ROR;ops : 2;oc : $c0;eb : 1;m : af_w or Modrm;o1 : ao_imm8;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_ROR;ops : 2;oc : $d2;eb : 1;m : af_w or Modrm;o1 : ao_shiftcount;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_ROR;ops : 1;oc : $d0;eb : 1;m : af_w or Modrm;o1 : ao_reg or ao_mem;o2 : 0;o3 : 0),
         (i : A_RCL;ops : 2;oc : $d0;eb : 2;m : af_w or Modrm;o1 : ao_imm1;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_RCL;ops : 2;oc : $c0;eb : 2;m : af_w or Modrm;o1 : ao_imm8;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_RCL;ops : 2;oc : $d2;eb : 2;m : af_w or Modrm;o1 : ao_shiftcount;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_RCL;ops : 1;oc : $d0;eb : 2;m : af_w or Modrm;o1 : ao_reg or ao_mem;o2 : 0;o3 : 0),
         (i : A_RCR;ops : 2;oc : $d0;eb : 3;m : af_w or Modrm;o1 : ao_imm1;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_RCR;ops : 2;oc : $c0;eb : 3;m : af_w or Modrm;o1 : ao_imm8;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_RCR;ops : 2;oc : $d2;eb : 3;m : af_w or Modrm;o1 : ao_shiftcount;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_RCR;ops : 1;oc : $d0;eb : 3;m : af_w or Modrm;o1 : ao_reg or ao_mem;o2 : 0;o3 : 0),
         (i : A_SAL;ops : 2;oc : $d0;eb : 4;m : af_w or Modrm;o1 : ao_imm1;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_SAL;ops : 2;oc : $c0;eb : 4;m : af_w or Modrm;o1 : ao_imm8;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_SAL;ops : 2;oc : $d2;eb : 4;m : af_w or Modrm;o1 : ao_shiftcount;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_SAL;ops : 1;oc : $d0;eb : 4;m : af_w or Modrm;o1 : ao_reg or ao_mem;o2 : 0;o3 : 0),
         (i : A_SHL;ops : 2;oc : $d0;eb : 4;m : af_w or Modrm;o1 : ao_imm1;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_SHL;ops : 2;oc : $c0;eb : 4;m : af_w or Modrm;o1 : ao_imm8;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_SHL;ops : 2;oc : $d2;eb : 4;m : af_w or Modrm;o1 : ao_shiftcount;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_SHL;ops : 1;oc : $d0;eb : 4;m : af_w or Modrm;o1 : ao_reg or ao_mem;o2 : 0;o3 : 0),
         (i : A_SHLD;ops : 3;oc : $0fa4;eb : ao_none;m : Modrm;o1 : ao_imm8;o2 : ao_wordreg;o3 : ao_wordreg or ao_mem),
         (i : A_SHLD;ops : 3;oc : $0fa5;eb : ao_none;m : Modrm;o1 : ao_shiftcount;o2 : ao_wordreg;o3 : ao_wordreg or ao_mem),
         (i : A_SHR;ops : 2;oc : $d0;eb : 5;m : af_w or Modrm;o1 : ao_imm1;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_SHR;ops : 2;oc : $c0;eb : 5;m : af_w or Modrm;o1 : ao_imm8;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_SHR;ops : 2;oc : $d2;eb : 5;m : af_w or Modrm;o1 : ao_shiftcount;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_SHR;ops : 1;oc : $d0;eb : 5;m : af_w or Modrm;o1 : ao_reg or ao_mem;o2 : 0;o3 : 0),
         (i : A_SHRD;ops : 3;oc : $0fac;eb : ao_none;m : Modrm;o1 : ao_imm8;o2 : ao_wordreg;o3 : ao_wordreg or ao_mem),
         (i : A_SHRD;ops : 3;oc : $0fad;eb : ao_none;m : Modrm;o1 : ao_shiftcount;o2 : ao_wordreg;o3 : ao_wordreg or ao_mem),
         (i : A_SAR;ops : 2;oc : $d0;eb : 7;m : af_w or Modrm;o1 : ao_imm1;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_SAR;ops : 2;oc : $c0;eb : 7;m : af_w or Modrm;o1 : ao_imm8;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_SAR;ops : 2;oc : $d2;eb : 7;m : af_w or Modrm;o1 : ao_shiftcount;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_SAR;ops : 1;oc : $d0;eb : 7;m : af_w or Modrm;o1 : ao_reg or ao_mem;o2 : 0;o3 : 0),
         (i : A_CALL;ops : 1;oc : $e8;eb : ao_none;m : jumpdword;o1 : ao_disp32;o2 : 0;o3 : 0),
         (i : A_CALL;ops : 1;oc : $ff;eb : 2;m : Modrm;o1 : ao_reg or ao_mem or ao_jumpabsolute;o2 : 0;o3 : 0),
         (i : A_LCALL;ops : 2;oc : $9a;eb : ao_none;m : JumpInterSegment;o1 : ao_imm16;o2 : ao_abs32;o3 : 0),
         (i : A_LCALL;ops : 1;oc : $ff;eb : 3;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_JMP;ops : 1;oc : $eb;eb : ao_none;m : Jump;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_JMP;ops : 1;oc : $ff;eb : 4;m : Modrm;o1 : ao_reg32 or ao_mem or ao_jumpabsolute;o2 : 0;o3 : 0),
         (i : A_LJMP;ops : 2;oc : $ea;eb : ao_none;m : JumpInterSegment;o1 : ao_imm16;o2 : ao_imm32;o3 : 0),
         (i : A_LJMP;ops : 1;oc : $ff;eb : 5;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_RET;ops : 0;oc : $c3;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_RET;ops : 1;oc : $c2;eb : ao_none;m : NoModrm;o1 : ao_imm16;o2 : 0;o3 : 0),
         (i : A_LRET;ops : 0;oc : $cb;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_LRET;ops : 1;oc : $ca;eb : ao_none;m : NoModrm;o1 : ao_imm16;o2 : 0;o3 : 0),
         (i : A_ENTER;ops : 2;oc : $c8;eb : ao_none;m : NoModrm;o1 : ao_imm16;o2 : ao_imm8;o3 : 0),
         (i : A_LEAVE;ops : 0;oc : $c9;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_JO;ops : 1;oc : $70;eb : ao_none;m : Jump;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_JNO;ops : 1;oc : $71;eb : ao_none;m : Jump;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_JB;ops : 1;oc : $72;eb : ao_none;m : Jump;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_JC;ops : 1;oc : $72;eb : ao_none;m : Jump;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_JNAE;ops : 1;oc : $72;eb : ao_none;m : Jump;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_JNB;ops : 1;oc : $73;eb : ao_none;m : Jump;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_JNC;ops : 1;oc : $73;eb : ao_none;m : Jump;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_JAE;ops : 1;oc : $73;eb : ao_none;m : Jump;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_JE;ops : 1;oc : $74;eb : ao_none;m : Jump;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_JZ;ops : 1;oc : $74;eb : ao_none;m : Jump;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_JNE;ops : 1;oc : $75;eb : ao_none;m : Jump;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_JNZ;ops : 1;oc : $75;eb : ao_none;m : Jump;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_JBE;ops : 1;oc : $76;eb : ao_none;m : Jump;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_JNA;ops : 1;oc : $76;eb : ao_none;m : Jump;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_JNBE;ops : 1;oc : $77;eb : ao_none;m : Jump;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_JA;ops : 1;oc : $77;eb : ao_none;m : Jump;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_JS;ops : 1;oc : $78;eb : ao_none;m : Jump;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_JNS;ops : 1;oc : $79;eb : ao_none;m : Jump;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_JP;ops : 1;oc : $7a;eb : ao_none;m : Jump;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_JPE;ops : 1;oc : $7a;eb : ao_none;m : Jump;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_JNP;ops : 1;oc : $7b;eb : ao_none;m : Jump;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_JPO;ops : 1;oc : $7b;eb : ao_none;m : Jump;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_JL;ops : 1;oc : $7c;eb : ao_none;m : Jump;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_JNGE;ops : 1;oc : $7c;eb : ao_none;m : Jump;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_JNL;ops : 1;oc : $7d;eb : ao_none;m : Jump;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_JGE;ops : 1;oc : $7d;eb : ao_none;m : Jump;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_JLE;ops : 1;oc : $7e;eb : ao_none;m : Jump;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_JNG;ops : 1;oc : $7e;eb : ao_none;m : Jump;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_JNLE;ops : 1;oc : $7f;eb : ao_none;m : Jump;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_JG;ops : 1;oc : $7f;eb : ao_none;m : Jump;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_JCXZ;ops : 1;oc : $67e3;eb : ao_none;m : JumpByte;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_JECXZ;ops : 1;oc : $e3;eb : ao_none;m : JumpByte;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_LOOP;ops : 1;oc : $e2;eb : ao_none;m : JumpByte;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_LOOPZ;ops : 1;oc : $e1;eb : ao_none;m : JumpByte;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_LOOPE;ops : 1;oc : $e1;eb : ao_none;m : JumpByte;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_LOOPNZ;ops : 1;oc : $e0;eb : ao_none;m : JumpByte;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_LOOPNE;ops : 1;oc : $e0;eb : ao_none;m : JumpByte;o1 : ao_disp;o2 : 0;o3 : 0),
         (i : A_SETO;ops : 1;oc : $0f90;eb : 0;m : Modrm;o1 : ao_reg8 or ao_mem;o2 : 0;o3 : 0),
         (i : A_SETNO;ops : 1;oc : $0f91;eb : 0;m : Modrm;o1 : ao_reg8 or ao_mem;o2 : 0;o3 : 0),
         (i : A_SETB;ops : 1;oc : $0f92;eb : 0;m : Modrm;o1 : ao_reg8 or ao_mem;o2 : 0;o3 : 0),
         (i : A_SETNAE;ops : 1;oc : $0f92;eb : 0;m : Modrm;o1 : ao_reg8 or ao_mem;o2 : 0;o3 : 0),
         (i : A_SETNB;ops : 1;oc : $0f93;eb : 0;m : Modrm;o1 : ao_reg8 or ao_mem;o2 : 0;o3 : 0),
         (i : A_SETAE;ops : 1;oc : $0f93;eb : 0;m : Modrm;o1 : ao_reg8 or ao_mem;o2 : 0;o3 : 0),
         (i : A_SETE;ops : 1;oc : $0f94;eb : 0;m : Modrm;o1 : ao_reg8 or ao_mem;o2 : 0;o3 : 0),
         (i : A_SETZ;ops : 1;oc : $0f94;eb : 0;m : Modrm;o1 : ao_reg8 or ao_mem;o2 : 0;o3 : 0),
         (i : A_SETNE;ops : 1;oc : $0f95;eb : 0;m : Modrm;o1 : ao_reg8 or ao_mem;o2 : 0;o3 : 0),
         (i : A_SETNZ;ops : 1;oc : $0f95;eb : 0;m : Modrm;o1 : ao_reg8 or ao_mem;o2 : 0;o3 : 0),
         (i : A_SETBE;ops : 1;oc : $0f96;eb : 0;m : Modrm;o1 : ao_reg8 or ao_mem;o2 : 0;o3 : 0),
         (i : A_SETNA;ops : 1;oc : $0f96;eb : 0;m : Modrm;o1 : ao_reg8 or ao_mem;o2 : 0;o3 : 0),
         (i : A_SETNBE;ops : 1;oc : $0f97;eb : 0;m : Modrm;o1 : ao_reg8 or ao_mem;o2 : 0;o3 : 0),
         (i : A_SETA;ops : 1;oc : $0f97;eb : 0;m : Modrm;o1 : ao_reg8 or ao_mem;o2 : 0;o3 : 0),
         (i : A_SETS;ops : 1;oc : $0f98;eb : 0;m : Modrm;o1 : ao_reg8 or ao_mem;o2 : 0;o3 : 0),
         (i : A_SETNS;ops : 1;oc : $0f99;eb : 0;m : Modrm;o1 : ao_reg8 or ao_mem;o2 : 0;o3 : 0),
         (i : A_SETP;ops : 1;oc : $0f9a;eb : 0;m : Modrm;o1 : ao_reg8 or ao_mem;o2 : 0;o3 : 0),
         (i : A_SETC;ops : 1; oc: $0f92;eb : 0;m : Modrm;o1 : ao_reg8 or ao_mem;o2 : 0;o3 : 0),
         (i : A_SETNC;ops : 1;oc: $0f93;eb : 0;m : Modrm;o1 : ao_reg8 or ao_mem;o2 : 0;o3 : 0),
         (i : A_SETPE;ops : 1;oc : $0f9a;eb : 0;m : Modrm;o1 : ao_reg8 or ao_mem;o2 : 0;o3 : 0),
         (i : A_SETNP;ops : 1;oc : $0f9b;eb : 0;m : Modrm;o1 : ao_reg8 or ao_mem;o2 : 0;o3 : 0),
         (i : A_SETPO;ops : 1;oc : $0f9b;eb : 0;m : Modrm;o1 : ao_reg8 or ao_mem;o2 : 0;o3 : 0),
         (i : A_SETL;ops : 1;oc : $0f9c;eb : 0;m : Modrm;o1 : ao_reg8 or ao_mem;o2 : 0;o3 : 0),
         (i : A_SETNGE;ops : 1;oc : $0f9c;eb : 0;m : Modrm;o1 : ao_reg8 or ao_mem;o2 : 0;o3 : 0),
         (i : A_SETNL;ops : 1;oc : $0f9d;eb : 0;m : Modrm;o1 : ao_reg8 or ao_mem;o2 : 0;o3 : 0),
         (i : A_SETGE;ops : 1;oc : $0f9d;eb : 0;m : Modrm;o1 : ao_reg8 or ao_mem;o2 : 0;o3 : 0),
         (i : A_SETLE;ops : 1;oc : $0f9e;eb : 0;m : Modrm;o1 : ao_reg8 or ao_mem;o2 : 0;o3 : 0),
         (i : A_SETNG;ops : 1;oc : $0f9e;eb : 0;m : Modrm;o1 : ao_reg8 or ao_mem;o2 : 0;o3 : 0),
         (i : A_SETNLE;ops : 1;oc : $0f9f;eb : 0;m : Modrm;o1 : ao_reg8 or ao_mem;o2 : 0;o3 : 0),
         (i : A_SETG;ops : 1;oc : $0f9f;eb : 0;m : Modrm;o1 : ao_reg8 or ao_mem;o2 : 0;o3 : 0),
         (i : A_CMPS;ops : 0;oc : $a6;eb : ao_none;m : af_w or NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_INS;ops : 0;oc : $6c;eb : ao_none;m : af_w or NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_OUTS;ops : 0;oc : $6e;eb : ao_none;m : af_w or NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_LODS;ops : 0;oc : $ac;eb : ao_none;m : af_w or NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_MOVS;ops : 0;oc : $a4;eb : ao_none;m : af_w or NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_SCAS;ops : 0;oc : $ae;eb : ao_none;m : af_w or NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_STOS;ops : 0;oc : $aa;eb : ao_none;m : af_w or NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_XLAT;ops : 0;oc : $d7;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_BSF;ops : 2;oc : $0fbc;eb : ao_none;m : Modrm or af_reverseregregmem;o1 : ao_reg or ao_mem;o2 : ao_reg;o3 : 0),
         (i : A_BSR;ops : 2;oc : $0fbd;eb : ao_none;m : Modrm or af_reverseregregmem;o1 : ao_reg or ao_mem;o2 : ao_reg;o3 : 0),
         (i : A_BT;ops : 2;oc : $0fa3;eb : ao_none;m : Modrm;o1 : ao_reg;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_BT;ops : 2;oc : $0fba;eb : 4;m : Modrm;o1 : ao_imm8;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_BTC;ops : 2;oc : $0fbb;eb : ao_none;m : Modrm;o1 : ao_reg;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_BTC;ops : 2;oc : $0fba;eb : 7;m : Modrm;o1 : ao_imm8;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_BTR;ops : 2;oc : $0fb3;eb : ao_none;m : Modrm;o1 : ao_reg;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_BTR;ops : 2;oc : $0fba;eb : 6;m : Modrm;o1 : ao_imm8;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_BTS;ops : 2;oc : $0fab;eb : ao_none;m : Modrm;o1 : ao_reg;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_BTS;ops : 2;oc : $0fba;eb : 5;m : Modrm;o1 : ao_imm8;o2 : ao_reg or ao_mem;o3 : 0),
         (i : A_INT;ops : 1;oc : $cd;eb : ao_none;m : NoModrm;o1 : ao_imm8;o2 : 0;o3 : 0),
         (i : A_INT3;ops : 0;oc : $cc;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_INTO;ops : 0;oc : $ce;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_IRET;ops : 0;oc : $cf;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_BOUNDL;ops : 2;oc : $62;eb : ao_none;m : Modrm;o1 : ao_reg32;o2 : ao_mem;o3 : 0),
         (i : A_BOUNDW;ops : 2;oc : $6662;eb : ao_none;m : Modrm;o1 : ao_reg16;o2 : ao_mem;o3 : 0),
         (i : A_HLT;ops : 0;oc : $f4;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_wAIT;ops : 0;oc : $9b;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_NOP;ops : 0;oc : $90;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_ARPL;ops : 2;oc : $63;eb : ao_none;m : Modrm;o1 : ao_reg16;o2 : ao_reg16 or ao_mem;o3 : 0),
         (i : A_LAR;ops : 2;oc : $0f02;eb : ao_none;m : Modrm or af_reverseregregmem;o1 : ao_wordreg or ao_mem;
           o2 : ao_wordreg;o3 : 0),
         (i : A_LGDT;ops : 1;oc : $0f01;eb : 2;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_LIDT;ops : 1;oc : $0f01;eb : 3;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_LLDT;ops : 1;oc : $0f00;eb : 2;m : Modrm;o1 : ao_wordreg or ao_mem;o2 : 0;o3 : 0),
         (i : A_LMSW;ops : 1;oc : $0f01;eb : 6;m : Modrm;o1 : ao_wordreg or ao_mem;o2 : 0;o3 : 0),
         (i : A_LSL;ops : 2;oc : $0f03;eb : ao_none;m : Modrm or af_reverseregregmem;o1 : ao_wordreg or ao_mem;
           o2 : ao_wordreg;o3 : 0),
         (i : A_LTR;ops : 1;oc : $0f00;eb : 3;m : Modrm;o1 : ao_wordreg or ao_mem;o2 : 0;o3 : 0),
         (i : A_SGDT;ops : 1;oc : $0f01;eb : 0;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_SIDT;ops : 1;oc : $0f01;eb : 1;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_SLDT;ops : 1;oc : $0f00;eb : 0;m : Modrm;o1 : ao_wordreg or ao_mem;o2 : 0;o3 : 0),
         (i : A_SMSW;ops : 1;oc : $0f01;eb : 4;m : Modrm;o1 : ao_wordreg or ao_mem;o2 : 0;o3 : 0),
         (i : A_STR;ops : 1;oc : $0f00;eb : 1;m : Modrm;o1 : ao_reg16 or ao_mem;o2 : 0;o3 : 0),
         (i : A_VERR;ops : 1;oc : $0f00;eb : 4;m : Modrm;o1 : ao_wordreg or ao_mem;o2 : 0;o3 : 0),
         (i : A_VERW;ops : 1;oc : $0f00;eb : 5;m : Modrm;o1 : ao_wordreg or ao_mem;o2 : 0;o3 : 0),
         (i : A_FLD;ops : 1;oc : $d9c0;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : 0;o3 : 0),
         (i : A_FLDS;ops : 1;oc : $d9;eb : 0;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FILDL;ops : 1;oc : $db;eb : 0;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FLDL;ops : 1;oc : $dd;eb : 0;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FLDL;ops : 1;oc : $d9c0;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : 0;o3 : 0),
         (i : A_FILDS;ops : 1;oc : $df;eb : 0;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FILDQ;ops : 1;oc : $df;eb : 5;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FLDT;ops : 1;oc : $db;eb : 5;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FBLD;ops : 1;oc : $df;eb : 4;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FST;ops : 1;oc : $ddd0;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : 0;o3 : 0),
         (i : A_FSTS;ops : 1;oc : $d9;eb : 2;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FISTL;ops : 1;oc : $db;eb : 2;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FSTL;ops : 1;oc : $dd;eb : 2;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FSTL;ops : 1;oc : $ddd0;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : 0;o3 : 0),
         (i : A_FISTS;ops : 1;oc : $df;eb : 2;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FSTP;ops : 1;oc : $ddd8;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : 0;o3 : 0),
         (i : A_FSTPS;ops : 1;oc : $d9;eb : 3;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FISTPL;ops : 1;oc : $db;eb : 3;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FSTPL;ops : 1;oc : $dd;eb : 3;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FSTPL;ops : 1;oc : $ddd8;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : 0;o3 : 0),
         (i : A_FISTPS;ops : 1;oc : $df;eb : 3;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FISTPQ;ops : 1;oc : $df;eb : 7;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FSTPT;ops : 1;oc : $db;eb : 7;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FBSTP;ops : 1;oc : $df;eb : 6;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FXCH;ops : 1;oc : $d9c8;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : 0;o3 : 0),
         (i : A_FCOM;ops : 1;oc : $d8d0;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : 0;o3 : 0),
         (i : A_FCOMS;ops : 1;oc : $d8;eb : 2;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FICOML;ops : 1;oc : $da;eb : 2;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FCOML;ops : 1;oc : $dc;eb : 2;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FCOML;ops : 1;oc : $d8d0;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : 0;o3 : 0),
         (i : A_FICOMS;ops : 1;oc : $de;eb : 2;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FCOMP;ops : 1;oc : $d8d8;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : 0;o3 : 0),
         (i : A_FCOMPS;ops : 1;oc : $d8;eb : 3;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FICOMPL;ops : 1;oc : $da;eb : 3;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FCOMPL;ops : 1;oc : $dc;eb : 3;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FCOMPL;ops : 1;oc : $d8d8;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : 0;o3 : 0),
         (i : A_FICOMPS;ops : 1;oc : $de;eb : 3;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FCOMPP;ops : 0;oc : $ded9;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FUCOM;ops : 1;oc : $dde0;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : 0;o3 : 0),
         (i : A_FUCOMP;ops : 1;oc : $dde8;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : 0;o3 : 0),
         (i : A_FUCOMPP;ops : 0;oc : $dae9;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FTST;ops : 0;oc : $d9e4;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FXAM;ops : 0;oc : $d9e5;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FLD1;ops : 0;oc : $d9e8;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FLDL2T;ops : 0;oc : $d9e9;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FLDL2E;ops : 0;oc : $d9ea;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FLDPI;ops : 0;oc : $d9eb;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FLDLG2;ops : 0;oc : $d9ec;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FLDLN2;ops : 0;oc : $d9ed;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FLDZ;ops : 0;oc : $d9ee;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FADD;ops : 1;oc : $d8c0;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : 0;o3 : 0),
         (i : A_FADD;ops : 2;oc : $d8c0;eb : ao_none;m : ShortForm or af_floatd;o1 : ao_floatreg;o2 : ao_floatacc;o3 : 0),
         (i : A_FADD;ops : 0;oc : $dcc1;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FADDP;ops : 1;oc : $dac0;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : 0;o3 : 0),
         (i : A_FADDP;ops : 2;oc : $dac0;eb : ao_none;m : ShortForm or af_floatd;o1 : ao_floatreg;o2 : ao_floatacc;o3 : 0),
         (i : A_FADDP;ops : 0;oc : $dec1;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FADDS;ops : 1;oc : $d8;eb : 0;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FIADDL;ops : 1;oc : $da;eb : 0;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FADDL;ops : 1;oc : $dc;eb : 0;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FIADDS;ops : 1;oc : $de;eb : 0;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FSUB;ops : 1;oc : $d8e0;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : 0;o3 : 0),
         (i : A_FSUB;ops : 2;oc : $d8e0;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : ao_floatacc;o3 : 0),
         (i : A_FSUB;ops : 2;oc : $dce8;eb : ao_none;m : ShortForm;o1 : ao_floatacc;o2 : ao_floatreg;o3 : 0),
         (i : A_FSUB;ops : 0;oc : $dce1;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FSUBP;ops : 1;oc : $dae0;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : 0;o3 : 0),
         (i : A_FSUBP;ops : 2;oc : $dae0;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : ao_floatacc;o3 : 0),
         (i : A_FSUBP;ops : 2;oc : $dee0;eb : ao_none;m : ShortForm;o1 : ao_floatacc;o2 : ao_floatreg;o3 : 0),
         (i : A_FSUBP;ops : 0;oc : $dee1;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FSUBS;ops : 1;oc : $d8;eb : 4;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FISUBL;ops : 1;oc : $da;eb : 4;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FSUBL;ops : 1;oc : $dc;eb : 4;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FISUBS;ops : 1;oc : $de;eb : 4;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FSUBR;ops : 1;oc : $d8e8;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : 0;o3 : 0),
         (i : A_FSUBR;ops : 2;oc : $d8e8;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : ao_floatacc;o3 : 0),
         (i : A_FSUBR;ops : 2;oc : $dce8;eb : ao_none;m : ShortForm;o1 : ao_floatacc;o2 : ao_floatreg;o3 : 0),
         (i : A_FSUBR;ops : 0;oc : $dce9;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FSUBRP;ops : 1;oc : $dae8;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : 0;o3 : 0),
         (i : A_FSUBRP;ops : 2;oc : $dae8;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : ao_floatacc;o3 : 0),
         (i : A_FSUBRP;ops : 2;oc : $dee8;eb : ao_none;m : ShortForm;o1 : ao_floatacc;o2 : ao_floatreg;o3 : 0),
         (i : A_FSUBRP;ops : 0;oc : $dee9;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FSUBRS;ops : 1;oc : $d8;eb : 5;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FISUBRL;ops : 1;oc : $da;eb : 5;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FSUBRL;ops : 1;oc : $dc;eb : 5;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FISUBRS;ops : 1;oc : $de;eb : 5;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FMUL;ops : 1;oc : $d8c8;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : 0;o3 : 0),
         (i : A_FMUL;ops : 2;oc : $d8c8;eb : ao_none;m : ShortForm or af_floatd;o1 : ao_floatreg;o2 : ao_floatacc;o3 : 0),
         (i : A_FMUL;ops : 0;oc : $dcc9;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FMULP;ops : 1;oc : $dac8;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : 0;o3 : 0),
         (i : A_FMULP;ops : 2;oc : $dac8;eb : ao_none;m : ShortForm or af_floatd;o1 : ao_floatreg;o2 : ao_floatacc;o3 : 0),
         (i : A_FMULP;ops : 0;oc : $dec9;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FMULS;ops : 1;oc : $d8;eb : 1;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FIMULL;ops : 1;oc : $da;eb : 1;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FMULL;ops : 1;oc : $dc;eb : 1;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FIMULS;ops : 1;oc : $de;eb : 1;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FDIV;ops : 1;oc : $d8f0;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : 0;o3 : 0),
         (i : A_FDIV;ops : 2;oc : $d8f0;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : ao_floatacc;o3 : 0),
         (i : A_FDIV;ops : 2;oc : $dcf0;eb : ao_none;m : ShortForm;o1 : ao_floatacc;o2 : ao_floatreg;o3 : 0),
         (i : A_FDIV;ops : 0;oc : $dcf1;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FDIVP;ops : 1;oc : $daf0;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : 0;o3 : 0),
         (i : A_FDIVP;ops : 2;oc : $daf0;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : ao_floatacc;o3 : 0),
         (i : A_FDIVP;ops : 2;oc : $def0;eb : ao_none;m : ShortForm;o1 : ao_floatacc;o2 : ao_floatreg;o3 : 0),
         (i : A_FDIVP;ops : 0;oc : $def1;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FDIVS;ops : 1;oc : $d8;eb : 6;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FIDIVL;ops : 1;oc : $da;eb : 6;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FDIVL;ops : 1;oc : $dc;eb : 6;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FIDIVS;ops : 1;oc : $de;eb : 6;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FDIVR;ops : 1;oc : $d8f8;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : 0;o3 : 0),
         (i : A_FDIVR;ops : 2;oc : $d8f8;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : ao_floatacc;o3 : 0),
         (i : A_FDIVR;ops : 2;oc : $dcf8;eb : ao_none;m : ShortForm;o1 : ao_floatacc;o2 : ao_floatreg;o3 : 0),
         (i : A_FDIVR;ops : 0;oc : $dcf9;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FDIVRP;ops : 1;oc : $daf8;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : 0;o3 : 0),
         (i : A_FDIVRP;ops : 2;oc : $daf8;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : ao_floatacc;o3 : 0),
         (i : A_FDIVRP;ops : 2;oc : $def8;eb : ao_none;m : ShortForm;o1 : ao_floatacc;o2 : ao_floatreg;o3 : 0),
         (i : A_FDIVRP;ops : 0;oc : $def9;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FDIVRS;ops : 1;oc : $d8;eb : 7;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FIDIVRL;ops : 1;oc : $da;eb : 7;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FDIVRL;ops : 1;oc : $dc;eb : 7;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FIDIVRS;ops : 1;oc : $de;eb : 7;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_F2XM1;ops : 0;oc : $d9f0;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FYL2X;ops : 0;oc : $d9f1;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FPTAN;ops : 0;oc : $d9f2;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FPATAN;ops : 0;oc : $d9f3;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FXTRACT;ops : 0;oc : $d9f4;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FPREM1;ops : 0;oc : $d9f5;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FDECSTP;ops : 0;oc : $d9f6;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FINCSTP;ops : 0;oc : $d9f7;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FPREM;ops : 0;oc : $d9f8;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FYL2XP1;ops : 0;oc : $d9f9;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FSQRT;ops : 0;oc : $d9fa;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FSINCOS;ops : 0;oc : $d9fb;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FRNDINT;ops : 0;oc : $d9fc;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FSCALE;ops : 0;oc : $d9fd;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FSIN;ops : 0;oc : $d9fe;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FCOS;ops : 0;oc : $d9ff;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FCHS;ops : 0;oc : $d9e0;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FABS;ops : 0;oc : $d9e1;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FNINIT;ops : 0;oc : $dbe3;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FINIT;ops : 0;oc : $dbe3;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FLDCW;ops : 1;oc : $d9;eb : 5;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FNSTCW;ops : 1;oc : $d9;eb : 7;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FSTCW;ops : 1;oc : $d9;eb : 7;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FNSTSW;ops : 1;oc : $dfe0;eb : ao_none;m : NoModrm;o1 : ao_acc;o2 : 0;o3 : 0),
         (i : A_FNSTSW;ops : 1;oc : $dd;eb : 7;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FNSTSW;ops : 0;oc : $dfe0;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FSTSW;ops : 1;oc : $dfe0;eb : ao_none;m : NoModrm;o1 : ao_acc;o2 : 0;o3 : 0),
         (i : A_FSTSW;ops : 1;oc : $dd;eb : 7;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FSTSW;ops : 0;oc : $dfe0;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FNCLEX;ops : 0;oc : $dbe2;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FCLEX;ops : 0;oc : $dbe2;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FNSTENV;ops : 1;oc : $d9;eb : 6;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FSTENV;ops : 1;oc : $d9;eb : 6;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FLDENV;ops : 1;oc : $d9;eb : 4;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FNSAVE;ops : 1;oc : $dd;eb : 6;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FSAVE;ops : 1;oc : $dd;eb : 6;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FRSTOR;ops : 1;oc : $dd;eb : 4;m : Modrm;o1 : ao_mem;o2 : 0;o3 : 0),
         (i : A_FFREE;ops : 1;oc : $ddc0;eb : ao_none;m : ShortForm;o1 : ao_floatreg;o2 : 0;o3 : 0),
         (i : A_FNOP;ops : 0;oc : $d9d0;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FWAIT;ops : 0;oc : $9b;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
{         (i : A_ADDRaf_wORD;ops : 0;oc : $67;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0), }
{         (i : A_WORD;ops : 0;oc : $66;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0), }
         (i : A_LOCK;ops : 0;oc : $f0;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
{         (i : A_CS;ops : 0;oc : $2e;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_DS;ops : 0;oc : $3e;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_ES;ops : 0;oc : $26;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_FS;ops : 0;oc : $64;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_GS;ops : 0;oc : $65;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_SS;ops : 0;oc : $36;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0), }
         (i : A_REP;ops : 0;oc : $f3;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_REPE;ops : 0;oc : $f3;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_REPNE;ops : 0;oc : $f2;eb : ao_none;m : NoModrm;o1 : 0;o2 : 0;o3 : 0),
         (i : A_NONE));

{****************************************************************************
                            Assembler Mnemoics
****************************************************************************}

     att_op2str : array[firstop..lastop] of string[7] =
       ('mov','movz','movs','','add',
        'call','idiv','imul','jmp','lea','mul','neg','not',
        'pop','popal','push','pushal','ret','sub','xchg','xor',
        'fild','cmp','jz','inc','dec','sete','setne','setl',
        'setg','setle','setge','je','jne','jl','jg','jle','jge',
        'or','fld','fadd','fmul','fsub','fdiv','fchs','fld1',
        'fidiv','cltd','jnz','fstp','and','jno','','',
        'enter','leave','cld','movs','rep','shl','shr','bound',
        'jns','js','jo','sar','test',
        'fcom','fcomp','fcompp','fxch','faddp','fmulp','fsubp','fdivp',
        'fnsts','sahf','fdivrp','fsubrp','setc','setnc','jc','jnc',
        'ja','jae','jb','jbe','seta','setae','setb','setbe',
        'aaa','aad','aam','aas','cbw','cdq','clc','cli',
        'clts','cmc','cwd','cwde','daa','das','hlt','iret','lahf',
        'lods','lock','nop','pusha','pushf','pushfl',
        'stc','std','sti','stos','wait','xlat','xlatb','movsb',
        'movsbl','movsbw','movswl','movsb','movzwl','popa','in',
        'out','lds','lcs','les','lfs','lgs','lss','popf','sbb','adc',
        'div','ror','rol','rcl','rcr','sal','shld','shrd',
        'lcall','ljmp','lret','jnae','jnb','jna','jnbe','jb','jnp',
        'jpe','jpo','jnge','jng','jnl','jnle','jcxz','jecxz',
        'loop','cmps','ins','outs','scas','bsf','bsr','bt','btc',
        'btr','bts','int','int3','into','boundl','boundw',
        'loopz','loope','loopnz','loopne','seto','setno','setnae',
        'setnb','setz','setnz','setna','setnbe','sets','setns','setp',
        'setpe','setnp','setpo','setnge','setnl','setng','setnle',
        'arpl','lar','lgdt','lidt','lldt','lmsw','lsl','ltr','sgdt',
        'sidt','sldt','smsw','str','verr','verw','fabs','fbld','fbstp',
        'fclex','fnclex','fcos','fdecstp','fdisi','fndisi','fdivr',
        'feni','fneni','ffree','fiadd','ficom','ficomp','fidivr',
        'fimul','fincstp','finit','fninit','fist','fistp','fisub',
        'fisubr','fldcw','fldenv','fldlg2','fldln2','fldl2e','fldl2t',
        'fldpi','flds','fldz','fnop','fpatan','fprem','fprem1','fptan',
        'frndint','frstor','fsave','fnsave','fscale','fsetpm','fsin',
        'fsincos','fsqrt','fst','fstcw','fnstcw','fstenv','fnstenv',
        'fstsw','fnstsw','ftst','fucom','fucomp','fucompp','fwait',
        'fxam','fxtract','fyl2x','fyl2xp1','f2xm1','fildq','filds',
        'fildl','fldl','fldt','fistq','fists','fistl','fstl','fsts',
        'fstps','fistpl','fstpl','fistps','fistpq','fstpt','fcomps',
        'ficompl','fcompl','ficomps','fcoms','ficoml','fcoml','ficoms',
        'fiaddl','faddl','fiadds','fisubl','fsubl','fisubs','fsubs',
        'fsubr','fsubrs','fisubrl','fsubrl','fisubrs','fmuls','fimull',
        'fmull','fimuls','fdivs','fidivl','fdivl','fidivs','fdivrs',
        'fidivrl','fdivrl','fidivrs','repe','repne','fadds','popfl',
        { mmx instructions supported by GNU AS v281 }
        'emms','movd','movq','packssdw','packsswb','packuswb',
        'paddb','paddd','paddsb','paddsw','paddusb','paddusw',
        'paddw','pand','pandn','pcmpeqb','pcmpeqd','pcmpeqw',
        'pcmpgtb','pcmpgtd','pcmpgtw','pmaddwd','pmulhw',
        'pmullw','por','pslld','psllq','psllw','psrad','psraw',
        'psrld','psrlq','psrlw','psubb','psubd','psubsb','psubsw',
        'psubusb','psubusw','psubw','punpckhbw','punpckhdq',
        'punpckhwd','punpcklbw','punpckldq','punpcklwd','pxor');

     {  topsize = (S_NO,S_B,S_W,S_L,S_BW,S_BL,S_WL,
                  S_IS,S_IL,S_IQ,S_FS,S_FL,S_FX,S_D); }
     att_opsize2str : array[topsize] of string[2] =
       ('','b','w','l','bw','bl','wl',
        's','l','q','s','l','t','d');

     att_reg2str : array[tregister] of string[6] =
       ('','%eax','%ecx','%edx','%ebx','%esp','%ebp','%esi','%edi',
        '%ax','%cx','%dx','%bx','%sp','%bp','%si','%di',
        '%al','%cl','%dl','%bl','%ah','%ch','%bh','%dh',
        '','%cs','%ds','%es','%fs','%gs','%ss',
        '%st','%st(0)','%st(1)','%st(2)','%st(3)','%st(4)',
        '%st(5)','%st(6)','%st(7)',
        '%mm0','%mm1','%mm2','%mm3',
        '%mm4','%mm5','%mm6','%mm7');

      int_op2str : array[firstop..lastop] of string[9] =
       ('mov','movzx','movsx','','add',
        'call','idiv','imul','jmp','lea','mul','neg','not',
        'pop','popad','push','pushad','ret','sub','xchg','xor',
        'fild','cmp','jz','inc','dec','sete','setne','setl',
        'setg','setle','setge','je','jne','jl','jg','jle','jge',
        'or','fld','fadd','fmul','fsub','fdiv','fchs','fld1',
        'fidiv','cdq','jnz','fstp','and','jno','','',
        'enter','leave','cld','movs','rep','shl','shr','bound',
        'jns','js','jo','sar','test',
        'fcom','fcomp','fcompp','fxch','faddp','fmulp','fsubrp','fdivp',
        'fnsts','sahf','fdivp','fsubp','setc','setnc','jc','jnc',
        'ja','jae','jb','jbe','seta','setae','setb','setbe',
        'aaa','aad','aam','aas','cbw','cdq','clc','cli',
        'clts','cmc','cwd','cwde','daa','das','hlt','iret','lahf',
        'lods','lock','nop','pusha','pushf','pushfd',
        'stc','std','sti','stos','wait','xlat','xlatb','movsx',
        'movsx','movsx','movsx','movsx','movzx','popa','in',
        'out','lds','lcs','les','lfs','lgs','lss','popf','sbb','adc',
        'div','ror','rol','rcl','rcr','sal','shld','shrd',
        'call','jmp','ret','jnae','jnb','jna','jnbe','jb','jnp',
        'jpe','jpo','jnge','jng','jnl','jnle','jcxz','jecxz',
        'loop','cmps','ins','outs','scas','bsf','bsr','bt','btc',
        'btr','bts','int','int3','into','bound','bound',
        'loopz','loope','loopnz','loopne','seto','setno','setnae',
        'setnb','setz','setnz','setna','setnbe','sets','setns','setp',
        'setpe','setnp','setpo','setnge','setnl','setng','setnle',
        'arpl','lar','lgdt','lidt','lldt','lmsw','lsl','ltr','sgdt',
        'sidt','sldt','smsw','str','verr','verw','fabs','fbld','fbstp',
        'fclex','fnclex','fcos','fdecstp','fdisi','fndisi','fdivr',
        'feni','fneni','ffree','fiadd','ficom','ficomp','fidivr',
        'fimul','fincstp','finit','fninit','fist','fistp','fisub',
        'fisubr','fldcw','fldenv','fldlg2','fldln2','fldl2e','fldl2t',
        'fldpi','flds','fldz','fnop','fpatan','fprem','fprem1','fptan',
        'frndint','frstor','fsave','fnsave','fscale','fsetpm','fsin',
        'fsincos','fsqrt','fst','fstcw','fnstcw','fstenv','fnstenv',
        'fstsw','fnstsw','ftst','fucom','fucomp','fucompp','fwait',
        'fxam','fxtract','fyl2x','fyl2xp1','f2xm1','fildq','filds',
        'fildl','fldl','fldt','fistq','fists','fistl','fstl','fsts',
        'fstps','fistpl','fstpl','fistps','fistpq','fstpt','fcomps',
        'ficompl','fcompl','ficomps','fcoms','ficoml','fcoml','ficoms',
        'fiadd','fadd','fiadd','fisub','fsub','fisub','fsub',
        'fsubr','fsubr','fisubr','fsubr','fisubr','fmul','fimul',
        'fmul','fimul','fdiv','fidiv','fdiv','fidiv','fdivr',
        'fidivr','fdivr','fidivr','repe','repne','fadd','popfd',
        { mmx instructions }
        'emms','movd','movq','packssdw','packsswb','packuswb',
        'paddb','paddd','paddsb','paddsw','paddusb','paddusw',
        'paddw','pand','pandn','pcmpeqb','pcmpeqd','pcmpeqw',
        'pcmpgtb','pcmpgtd','pcmpgtw','pmaddwd','pmulhw',
        'pmullw','por','pslld','psllq','psllw','psrad','psraw',
        'psrld','psrlq','psrlw','psubb','psubd','psubsb','psubsw',
        'psubusb','psubusw','psubw','punpckhbw','punpckhdq',
        'punpckhwd','punpcklbw','punpckldq','punpcklwd','pxor');

     int_reg2str : array[tregister] of string[5] =
       ('','eax','ecx','edx','ebx','esp','ebp','esi','edi',
        'ax','cx','dx','bx','sp','bp','si','di',
        'al','cl','dl','bl','ah','ch','bh','dh',
        '','cs','ds','es','fs','gs','ss',
        'st','st(0)','st(1)','st(2)','st(3)','st(4)','st(5)','st(6)','st(7)',
        'mm0','mm1','mm2','mm3','mm4','mm5','mm6','mm7');

     int_nasmreg2str : array[tregister] of string[5] =
       ('','eax','ecx','edx','ebx','esp','ebp','esi','edi',
        'ax','cx','dx','bx','sp','bp','si','di',
        'al','cl','dl','bl','ah','ch','bh','dh',
        '','cs','ds','es','fs','gs','ss',
        'st0','st0','st1','st2','st3','st4','st5','st6','st7',
        'mm0','mm1','mm2','mm3','mm4','mm5','mm6','mm7');


  implementation

    function reg2str(r : tregister) : string;

      const
         a : array[R_NO..R_BL] of string[3] =
          ('','EAX','ECX','EDX','EBX','ESP','EBP','ESI','EDI',
           'AX','CX','DX','BX','SP','BP','SI','DI',
           'AL','CL','DL','BL');

      begin
         reg2str:=a[r];
      end;

    function newreference(const r : treference) : preference;

      var
         p : preference;

      begin
         new(p);
         p^:=r;
         if assigned(r.symbol) then
           p^.symbol:=stringdup(r.symbol^);
         newreference:=p;
      end;

    function lab2str(l : plabel) : string;

      begin
         if (l=nil) or (l^.nb=0) then
{$ifdef EXTDEBUG}
           lab2str:='ILLEGAL'
         else
         begin
            lab2str:=target_asm.labelprefix+tostr(l^.nb);
         end;
{$else EXTDEBUG}
           internalerror(2000);
           lab2str:=target_asm.labelprefix+tostr(l^.nb);
{$endif EXTDEBUG}
         { was missed: }
         inc(l^.refcount);
         l^.is_used:=true;
      end;

    function reg8toreg16(reg : tregister) : tregister;

      begin
         reg8toreg16:=reg32toreg16(reg8toreg32(reg));
      end;

    function reg16toreg8(reg : tregister) : tregister;

      begin
         reg16toreg8:=reg32toreg8(reg16toreg32(reg));
      end;

    function reg16toreg32(reg : tregister) : tregister;

      begin
         reg16toreg32:=tregister(byte(reg)-byte(R_EDI));
      end;

    function reg32toreg16(reg : tregister) : tregister;

      begin
         reg32toreg16:=tregister(byte(reg)+byte(R_EDI));
      end;

    function reg32toreg8(reg : tregister) : tregister;

      begin
         reg32toreg8:=tregister(byte(reg)+byte(R_DI));
      end;

    function reg8toreg32(reg : tregister) : tregister;

      begin
         reg8toreg32:=tregister(byte(reg)-byte(R_DI));
      end;

    procedure reset_reference(var ref : treference);

      begin
{$ifdef ver0_6}
         ref.index:=R_NO;
         ref.base:=R_NO;
         ref.segment:=R_DEFAULT_SEG;
         ref.offset:=0;
         ref.scalefactor:=1;
         ref.isintvalue:=false;
         ref.symbol:=nil;
{$else}
         with ref do
           begin
              index:=R_NO;
              base:=R_NO;
              segment:=R_DEFAULT_SEG;
              offset:=0;
              scalefactor:=1;
              isintvalue:=false;
              symbol:=nil;
           end;
{$endif}
      end;

      function new_reference(base : tregister;offset : longint) : preference;

        var
           r : preference;
        begin
           new(r);
           reset_reference(r^);
           r^.base:=base;
           r^.offset:=offset;
           new_reference:=r;
        end;

      procedure clear_reference(var ref : treference);

      begin
         stringdispose(ref.symbol);
         reset_reference(ref);
      end;

    procedure getlabel(var l : plabel);

      begin
         new(l);
         l^.nb:=nextlabelnr;
         l^.is_used:=false;
         l^.is_set:=false;
         l^.refcount:=0;
         inc(nextlabelnr);
      end;

    procedure freelabel(var l : plabel);

      begin
         if (l<>nil) and (not l^.is_set) and (not l^.is_used) then
           dispose(l);
         l:=nil;
      end;

    procedure setzerolabel(var l : plabel);

      begin
         l^.nb:=0;
         l^.is_used:=false;
         l^.is_set:=false;
         l^.refcount:=0;
      end;

    procedure getzerolabel(var l : plabel);

      begin
         new(l);
         l^.nb:=0;
         l^.is_used:=false;
         l^.is_set:=false;
         l^.refcount:=0;
      end;

    procedure getlabelnr(var l : longint);

      begin
         l:=nextlabelnr;
         inc(nextlabelnr);
      end;

    function newcsymbol(const s : string;l : longint) : pcsymbol;

      var
         p : pcsymbol;

      begin
         new(p);
         p^.symbol:=strpnew(s);
         p^.offset:=l;
         newcsymbol:=p;
      end;

    procedure disposecsymbol(p : pcsymbol);

      begin
      strdispose(p^.symbol);
      dispose(p);
      end;
{****************************************************************************
                       objects for register de/allocation
 ****************************************************************************}

{$ifdef REGALLOC}

    constructor tairegalloc.init(r : tregister);

      begin
         inherited init;
         typ:=ait_regalloc;
         reg:=r;
      end;

    constructor tairegdealloc.init(r : tregister);

      begin
         inherited init;
         typ:=ait_regdealloc;
         reg:=r;
      end;

{$endif REGALLOC}

{****************************************************************************
                             TAI386
 ****************************************************************************}

    constructor tai386.op_none(op : tasmop;_size : topsize);

      begin
         inherited init;
         typ:=ait_instruction;
         _operator:=op;
         opxt:=0;
         size:=_size;

         { the following isn't required ! }
         op1:=nil;
         op2:=nil;
      end;

    constructor tai386.op_reg(op : tasmop;_size : topsize;_op1 : tregister);

      begin
         inherited init;
         typ:=ait_instruction;
         _operator:=op;
         opxt:=Top_reg;
         size:=_size;
         op1:=pointer(_op1);

         op2:=nil;
      end;

    constructor tai386.op_const(op : tasmop;_size : topsize;_op1 : longint);

      begin
         inherited init;
         typ:=ait_instruction;
         _operator:=op;
         opxt:=Top_const;
         size:=_size;
         op1:=pointer(_op1);

         op2:=nil;
      end;

    constructor tai386.op_ref(op : tasmop;_size : topsize;_op1 : preference);

      begin
         inherited init;
         typ:=ait_instruction;
         _operator:=op;
         size:=_size;
         if _op1^.isintvalue then
           begin
              opxt:=top_const;
              op1:=pointer(_op1^.offset);
           end
         else
           begin
              opxt:=top_ref;
              op1:=pointer(_op1);
           end;

         op2:=nil;
      end;

    constructor tai386.op_loc(op : tasmop;_size : topsize;_op1 : tlocation);

      begin
         inherited init;
         typ:=ait_instruction;
         _operator:=op;
         size:=_size;
         if (_op1.loc=loc_register) or (_op1.loc=loc_cregister)  then
           begin
             opxt:=top_reg;
             op1:=pointer(_op1.register);
           end
         else
         if _op1.reference.isintvalue then
           begin
              opxt:=top_const;
              op1:=pointer(_op1.reference.offset);
           end
         else
           begin
              opxt:=top_ref;
              op1:=pointer(newreference(_op1.reference));
           end;

         op2:=nil;
      end;

    constructor tai386.op_reg_reg(op : tasmop;_size : topsize;_op1,_op2 : tregister);

      begin
         inherited init;
         typ:=ait_instruction;
         _operator:=op;
         opxt:=Top_reg shl 4+Top_reg;
         size:=_size;
         op1:=pointer(_op1);
         op2:=pointer(_op2);

      end;

    constructor tai386.op_reg_ref(op : tasmop;_size : topsize;_op1 : tregister;_op2 : preference);

      begin
         inherited init;
         typ:=ait_instruction;
         _operator:=op;
         opxt:=top_reg;
         size:=_size;
         op1:=pointer(_op1);

         if _op2^.isintvalue then
           begin
              opxt:=opxt+top_const shl 4;
              op2:=pointer(_op2^.offset);
           end
         else
           begin
              opxt:=opxt+top_ref shl 4;
              op2:=pointer(_op2);
           end;

      end;

    constructor tai386.op_reg_loc(op : tasmop;_size : topsize;_op1 : tregister;_op2 : tlocation);

      begin
         inherited init;
         typ:=ait_instruction;
         _operator:=op;
         opxt:=top_reg;
         size:=_size;
         op1:=pointer(_op1);

         if (_op2.loc=loc_register) or (_op2.loc=loc_cregister)  then
           begin
             opxt:=opxt+top_reg shl 4;
             op2:=pointer(_op2.register);
           end
         else
         if _op2.reference.isintvalue then
           begin
              opxt:=opxt+top_const shl 4;
              op2:=pointer(_op2.reference.offset);
           end
         else
           begin
              opxt:=opxt+Top_ref shl 4;
              op2:=pointer(newreference(_op2.reference));
           end;

      end;

    constructor tai386.op_loc_reg(op : tasmop;_size : topsize;_op1 : tlocation;_op2 : tregister);

      begin
         inherited init;
         typ:=ait_instruction;
         _operator:=op;
         opxt:=top_reg shl 4;
         size:=_size;
         op2:=pointer(_op2);

         if (_op1.loc=loc_register) or (_op1.loc=loc_cregister)  then
           begin
             opxt:=opxt+top_reg;
             op1:=pointer(_op1.register);
           end
         else
         if _op1.reference.isintvalue then
           begin
              opxt:=opxt+top_const;
              op1:=pointer(_op1.reference.offset);
           end
         else
           begin
              opxt:=opxt+top_ref;
              op1:=pointer(newreference(_op1.reference));
           end;

      end;

    constructor tai386.op_const_reg_reg(op : tasmop;_size : topsize;_op1 : longint;_op2 : tregister;_op3 : tregister);

    type    twowords=record
                word1,word2:word;
            end;

      begin
         inherited init;
         typ:=ait_instruction;
         _operator:=op;
         opxt:=Top_const+Top_reg shl 4+Top_reg shl 8;
         size:=_size;
         op1:=pointer(_op1);
         twowords(op2).word1:=word(_op2);
         twowords(op2).word2:=word(_op3);
      end;

    constructor tai386.op_const_reg(op : tasmop;_size : topsize;_op1 : longint;_op2 : tregister);

      begin
         inherited init;
         typ:=ait_instruction;
         _operator:=op;
         if ((op=A_CMP) or (op=A_AND) or (op=A_ADD) or
            (op=A_ADC) or (op=A_SUB) or (op=A_SBB)) and
            ((_size=S_B) or (_size=S_BW) or (_size=S_BL)) and
            ((_op2<R_AL) or (_op2>R_DH)) and
            (_op1>127) then
           begin
{$ifdef extdebug}
              comment(v_warning,'wrong size for instruction due to implicit size extension !!');
{$endif extdebug}
              if _size=S_BW then
                _size:=S_W
              else
                _size:=S_L;
           end;
         opxt:=Top_const+Top_reg shl 4;
         size:=_size;
         op1:=pointer(_op1);
         op2:=pointer(_op2);

      end;

    constructor tai386.op_const_const(op : tasmop;_size : topsize;_op1,_op2 : longint);

      begin
         inherited init;
         typ:=ait_instruction;
         _operator:=op;
         opxt:=Top_const+Top_const shl 4;
         size:=_size;
         op1:=pointer(_op1);
         op2:=pointer(_op2);

      end;

    constructor tai386.op_const_ref(op : tasmop;_size : topsize;_op1 : longint;_op2 : preference);

      begin
         inherited init;
         typ:=ait_instruction;
         _operator:=op;
         opxt:=top_const;
         size:=_size;
         op1:=pointer(_op1);

         if _op2^.isintvalue then
           begin
              opxt:=opxt+top_const shl 4;
              op2:=pointer(_op2^.offset);
           end
         else
           begin
              opxt:=opxt+top_ref shl 4;
              op2:=pointer(_op2);
           end;

      end;

    constructor tai386.op_const_loc(op : tasmop;_size : topsize;_op1 : longint;_op2 : tlocation);

      begin
         inherited init;
         typ:=ait_instruction;
         _operator:=op;
         opxt:=top_const;
         size:=_size;
         op1:=pointer(_op1);

         if (_op2.loc=loc_register) or (_op2.loc=loc_cregister)  then
           begin
             opxt:=opxt+Top_reg shl 4;
             op2:=pointer(_op2.register);
           end
         else
         if _op2.reference.isintvalue then
           begin
              opxt:=opxt+top_const shl 4;
              op2:=pointer(_op2.reference.offset);
           end
         else
           begin
              opxt:=opxt+top_ref shl 4;
              op2:=pointer(newreference(_op2.reference));
           end;

      end;

    constructor tai386.op_ref_reg(op : tasmop;_size : topsize;_op1 : preference;_op2 : tregister);

      begin
         inherited init;
         typ:=ait_instruction;
         _operator:=op;
         opxt:=top_reg shl 4;
         size:=_size;
         op2:=pointer(_op2);

         if _op1^.isintvalue then
           begin
              opxt:=opxt+top_const;
              op1:=pointer(_op1^.offset);
           end
         else
           begin
              opxt:=opxt+top_ref;
              op1:=pointer(_op1);
           end;

      end;

    constructor tai386.op_ref_ref(op : tasmop;_size : topsize;_op1,_op2 : preference);

      begin
         inherited init;
         typ:=ait_instruction;
         _operator:=op;
         size:=_size;

         if _op1^.isintvalue then
           begin
              opxt:=top_const;
              op1:=pointer(_op1^.offset);
           end
         else
           begin
              opxt:=top_ref;
              op1:=pointer(_op1);
           end;

         if _op2^.isintvalue then
           begin
              opxt:=opxt+top_const shl 4;
              op2:=pointer(_op2^.offset);
           end
         else
           begin
              opxt:=opxt+top_ref shl 4;
              op2:=pointer(_op2);
           end;

      end;

    constructor tai386.op_csymbol(op : tasmop;_size : topsize;_op1 : pcsymbol);

      begin
         inherited init;
         typ:=ait_instruction;
         _operator:=op;
         if (op=A_CALL) and (use_esp_stackframe) then
          Message(cg_e_stackframe_with_esp);
         opxt:=top_symbol;
         size:=_size;
         op1:=pointer(_op1);
         op2:=nil;
      end;

    constructor tai386.op_csymbol_reg(op : tasmop;_size : topsize;_op1 : pcsymbol;_op2 : tregister);

      begin
         inherited init;
         typ:=ait_instruction;
         _operator:=op;
         opxt:=Top_symbol+Top_reg shl 4;
         size:=_size;
         op1:=pointer(_op1);
         op2:=pointer(_op2);

      end;

    constructor tai386.op_csymbol_ref(op : tasmop;_size : topsize;_op1 : pcsymbol;_op2 : preference);

      begin
         inherited init;
         typ:=ait_instruction;
         _operator:=op;
         opxt:=top_symbol;
         size:=_size;
         op1:=pointer(_op1);

         if _op2^.isintvalue then
           begin
              opxt:=opxt+top_const shl 4;
              op2:=pointer(_op2^.offset);
           end
         else
           begin
              opxt:=opxt+top_ref shl 4;
              op2:=pointer(_op2);
           end;

      end;

    constructor tai386.op_csymbol_loc(op : tasmop;_size : topsize;_op1 : pcsymbol;_op2 : tlocation);

      begin
         inherited init;
         typ:=ait_instruction;
         _operator:=op;
         opxt:=top_symbol;
         size:=_size;
         op1:=pointer(_op1);

         if (_op2.loc=loc_register) or (_op2.loc=loc_cregister)  then
           begin
             opxt:=top_reg shl 4;
             op2:=pointer(_op2.register);
           end
         else
         if _op2.reference.isintvalue then
           begin
              opxt:=opxt+top_const shl 4;
              op2:=pointer(_op2.reference.offset);
           end
         else
           begin
              opxt:=opxt+top_ref shl 4;
              op2:=pointer(newreference(_op2.reference));
           end;

      end;

    constructor tai386.op_reg_const(op:tasmop; _size: topsize; _op1: tregister; _op2: longint);
    begin
         inherited init;
         typ:=ait_instruction;
         _operator:=op;
         opxt:=top_reg+top_const shl 4;
         size:=_size;
         op1:=pointer(_op1);
         op2:=pointer(_op2);
    end;

   function Tai386.op1t:byte;

    begin
        op1t:=opxt and 15;
    end;

   function Tai386.op2t:byte;

    begin
        op2t:=(opxt shr 4) and 15;
    end;

   function Tai386.op3t:byte;

    begin
        op3t:=(opxt shr 8) and 15;
    end;

   destructor tai386.done;

     begin
        if op1t=top_symbol then
          disposecsymbol(pcsymbol(op1))
        else if op1t=top_ref then
          begin
             clear_reference(preference(op1)^);
             dispose(preference(op1));
          end;
        if op2t=top_symbol then
          disposecsymbol(pcsymbol(op2))
        else if op2t=top_ref then
          begin
             clear_reference(preference(op2)^);
             dispose(preference(op2));
          end;
     end;

{****************************************************************************
                             TAI_LABELED
 ****************************************************************************}

    constructor tai_labeled.init(op : tasmop; l : plabel);

      begin
         inherited init;
         typ:=ait_labeled_instruction;
         _operator:=op;
         lab:=l;
         lab^.is_used:=true;
         inc(lab^.refcount);
      end;

    destructor tai_labeled.done;

      begin
         dec(lab^.refcount);
         if lab^.refcount=0 then
           Begin
             lab^.is_used := False;
             If Not(lab^.is_set) Then
               Dispose(lab);
           End;
      end;

end.
{
  $Log$
  Revision 1.7  1998-05-20 09:42:34  pierre
    + UseTokenInfo now default
    * unit in interface uses and implementation uses gives error now
    * only one error for unknown symbol (uses lastsymknown boolean)
      the problem came from the label code !
    + first inlined procedures and function work
      (warning there might be allowed cases were the result is still wrong !!)
    * UseBrower updated gives a global list of all position of all used symbols
      with switch -gb

  Revision 1.6  1998/05/04 17:54:25  peter
    + smartlinking works (only case jumptable left todo)
    * redesign of systems.pas to support assemblers and linkers
    + Unitname is now also in the PPU-file, increased version to 14

  Revision 1.5  1998/04/29 10:33:53  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.4  1998/04/09 15:46:38  florian
    + register allocation tracing stuff added

  Revision 1.3  1998/04/08 16:58:02  pierre
    * several bugfixes
      ADD ADC and AND are also sign extended
      nasm output OK (program still crashes at end
      and creates wrong assembler files !!)
      procsym types sym in tdef removed !!

  Revision 1.2  1998/04/04 05:29:57  carl
    * bugfix of crash with ins_cache and popfd
    * bugfix of pushfd typo mistake in att output
    + added setc, and setnc
}
