{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl, Carl Eric Codere

    This unit implements an types and classes specific for the
    MC68000/MC68020

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

  interface

    uses
       cobjects,aasm,globtype;

    const
      { if real fpu is used }
      { otherwise maps to   }
      { s32real.            }
      extended_size = 12;

    type
    {  warning: CPU32 opcodes are not fully compatible with the MC68020. }
       { 68000 only opcodes }
       tasmop = (A_ABCD,
         A_ADD,A_ADDA,A_ADDI,A_ADDQ,A_ADDX,A_AND,A_ANDI,
         A_ASL,A_ASR,A_BCC,A_BCS,A_BEQ,A_BGE,A_BGT,A_BHI,
         A_BLE,A_BLS,A_BLT,A_BMI,A_BNE,A_BPL,A_BVC,A_BVS,
         A_BCHG,A_BCLR,A_BRA,A_BSET,A_BSR,A_BTST,A_CHK,
         A_CLR,A_CMP,A_CMPA,A_CMPI,A_CMPM,A_DBCC,A_DBCS,A_DBEQ,A_DBGE,
         A_DBGT,A_DBHI,A_DBLE,A_DBLS,A_DBLT,A_DBMI,A_DBNE,A_DBRA,
         A_DBPL,A_DBT,A_DBVC,A_DBVS,A_DBF,A_DIVS,A_DIVU,
         A_EOR,A_EORI,A_EXG,A_ILLEGAL,A_EXT,A_JMP,A_JSR,
         A_LEA,A_LINK,A_LSL,A_LSR,A_MOVE,A_MOVEA,A_MOVEI,A_MOVEQ,
         A_MOVEM,A_MOVEP,A_MULS,A_MULU,A_NBCD,A_NEG,A_NEGX,
         A_NOP,A_NOT,A_OR,A_ORI,A_PEA,A_ROL,A_ROR,A_ROXL,
         A_ROXR,A_RTR,A_RTS,A_SBCD,A_SCC,A_SCS,A_SEQ,A_SGE,
         A_SGT,A_SHI,A_SLE,A_SLS,A_SLT,A_SMI,A_SNE,
         A_SPL,A_ST,A_SVC,A_SVS,A_SF,A_SUB,A_SUBA,A_SUBI,A_SUBQ,
         A_SUBX,A_SWAP,A_TAS,A_TRAP,A_TRAPV,A_TST,A_UNLK,
         A_RTE,A_RESET,A_STOP,
         { MC68010 instructions }
         A_BKPT,A_MOVEC,A_MOVES,A_RTD,
         { MC68020 instructions }
         A_BFCHG,A_BFCLR,A_BFEXTS,A_BFEXTU,A_BFFFO,
         A_BFINS,A_BFSET,A_BFTST,A_CALLM,A_CAS,A_CAS2,
         A_CHK2,A_CMP2,A_DIVSL,A_DIVUL,A_EXTB,A_PACK,A_RTM,
         A_TRAPCC,A_TRACS,A_TRAPEQ,A_TRAPF,A_TRAPGE,A_TRAPGT,
         A_TRAPHI,A_TRAPLE,A_TRAPLS,A_TRAPLT,A_TRAPMI,A_TRAPNE,
         A_TRAPPL,A_TRAPT,A_TRAPVC,A_TRAPVS,A_UNPK,
         { FPU Processor instructions - directly supported only. }
         { IEEE aware and misc. condition codes not supported   }
         A_FABS,A_FADD,
         A_FBEQ,A_FBNE,A_FBNGT,A_FBGT,A_FBGE,A_FBNGE,
         A_FBLT,A_FBNLT,A_FBLE,A_FBGL,A_FBNGL,A_FBGLE,A_FBNGLE,
         A_FDBEQ,A_FDBNE,A_FDBGT,A_FDBNGT,A_FDBGE,A_FDBNGE,
         A_FDBLT,A_FDBNLT,A_FDBLE,A_FDBGL,A_FDBNGL,A_FDBGLE,A_FBDNGLE,
         A_FSEQ,A_FSNE,A_FSGT,A_FSNGT,A_FSGE,A_FSNGE,
         A_FSLT,A_FSNLT,A_FSLE,A_FSGL,A_FSNGL,A_FSGLE,A_FSNGLE,
         A_FCMP,A_FDIV,A_FMOVE,A_FMOVEM,
         A_FMUL,A_FNEG,A_FNOP,A_FSQRT,A_FSUB,A_FSGLDIV,
         A_FSFLMUL,A_FTST,
         A_FTRAPEQ,A_FTRAPNE,A_FTRAPGT,A_FTRAPNGT,A_FTRAPGE,A_FTRAPNGE,
         A_FTRAPLT,A_FTRAPNLT,A_FTRAPLE,A_FTRAPGL,A_FTRAPNGL,A_FTRAPGLE,A_FTRAPNGLE,
         { Protected instructions }
         A_CPRESTORE,A_CPSAVE,
         { FPU Unit protected instructions                    }
         { and 68030/68851 common MMU instructions            }
         { (this may include 68040 MMU instructions)          }
         A_FRESTORE,A_FSAVE,A_PFLUSH,A_PFLUSHA,A_PLOAD,A_PMOVE,A_PTEST,
         { Useful for assembly langage output }
         A_LABEL,A_NONE);

       { enumeration for registers, don't change the }
       { order of this table                         }
       { Registers which can and will be used by the compiler }
       tregister = (
         R_NO,R_D0,R_D1,R_D2,R_D3,R_D4,R_D5,R_D6,R_D7,
         R_A0,R_A1,R_A2,R_A3,R_A4,R_A5,R_A6,R_SP,
         { PUSH/PULL- quick and dirty hack }
         R_SPPUSH,R_SPPULL,
         { misc. }
         R_CCR,R_FP0,R_FP1,R_FP2,R_FP3,R_FP4,R_FP5,R_FP6,
         R_FP7,R_FPCR,R_SR,R_SSP,R_DFC,R_SFC,R_VBR,R_FPSR,
         { other - not used in reg2str }
         R_DEFAULT_SEG);

       { S_NO = No Size of operand }
       { S_B  = Byte size operand  }
       { S_W  = Word size operand  }
       { S_L  = DWord size operand }
       { USED FOR conversions in x86}
       { S_BW = Byte to word       }
       { S_BL = Byte to long       }
       { S_WL = Word to long       }
       { Floating point types      }
       { S_FS  = single type (32 bit) }
       { S_FL  = double/64bit integer }
       { S_FX  = Extended type      }
       { S_IS  = integer on 16 bits   }
       { S_IL  = integer on 32 bits   }
       { S_IQ  = integer on 64 bits   }
       topsize = (S_NO,S_B,S_W,S_L,S_BW,S_BL,S_WL,
                  S_FS,S_FL,S_FX,S_IS,S_IL,S_IQ);

       plocation = ^tlocation;

       { information about the location of an operand }
       { LOC_FPU         FPU registers = Dn if emulation }
       { LOC_REGISTER    in a processor register }
       { LOC_MEM         in the memory }
       { LOC_REFERENCE   like LOC_MEM, but lvalue }
       { LOC_JUMP        nur bool'sche Resultate, Sprung zu false- oder }
       {                 truelabel }
       { LOC_FLAGS       nur bool'sche Rsultate, Flags sind gesetzt }
       { LOC_CREGISTER   register which shouldn't be modified }
       { LOC_INVALID     added for tracking problems}

       tloc = (LOC_INVALID,LOC_FPU,LOC_REGISTER,LOC_MEM,LOC_REFERENCE,LOC_JUMP,
           LOC_FLAGS,LOC_CREGISTER);

       tregisterlist = set of tregister;

 { F_E = Equal
   F_NE = Not Equal
   F_G = Greater then
   F_L = Less then
   F_GE = Greater or equal then
   F_LE = Less or equal then
   F_C = Carry
   F_NC = Not Carry
   F_A = Above
   F_AE = Above or Equal
   F_B = Below
   F_BE = Below or Equal
   other flags:
   FL_xxx = floating type flags .

 }
       tresflags = (F_E,F_NE,F_G,F_L,F_GE,F_LE,F_C,F_NC,
          F_A,F_AE,F_B,F_BE);
          { floating type flags used by FBCC are auotmatically converted }
          { to standard condition codes                                  }
{          FL_E,FL_NE,FL_A,FL_AE,FL_B,FL_BE);}

       preference = ^treference;

      { direction of address register : }
      {              (An)     (An)+   -(An)  }
      tdirection = (dir_none,dir_inc,dir_dec);

       treference = record
      base,segment,index : tregister;
      offset : longint;
      symbol : pstring;
      { indexed increment and decrement mode }
      { (An)+ and -(An)                      }
      direction : tdirection;
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
         LOC_FPU : (fpureg:tregister);
         LOC_JUMP : ();
         LOC_FLAGS : (resflags : tresflags);
         LOC_INVALID : ();
       end;

       pcsymbol = ^tcsymbol;

       tcsymbol = record
      symbol : pchar;
         offset : longint;
       end;

    const
 {----------------------------------------------------------------------}
 { F_E = Equal                                                          }
 { F_NE = Not Equal                                                     }
 { F_G = Greater then                                                   }
 { F_L = Less then                                                      }
 { F_GE = Greater or equal then                                         }
 { F_LE = Less or equal then                                            }
 { F_C = Carry                            = C                           }
 { F_NC = Not Carry                       = not C                       }
 { F_A = Above                            = not C and not Z             }
 { F_AE = Above or Equal                  = not C                       }
 { F_B = Below                            = C                           }
 { F_BE = Below or Equal                  = C or Z                      }
 { FL_E = Floating point equal            = Z                           }
 { FL_NE = Floating point Not equal       = not Z                       }
 { FL_A  = Floating point above           =                             }
 { FL_AE = Floating point above or equal  =                             }
 { FL_B  = Floating point below           =                             }
 { FL_BE = Floating point below or equal  =                             }

 { THE ORDER OF THIS TABLE SHOULD NOT BE CHANGED! }
 flag_2_jmp: array[F_E..F_BE] of tasmop =
 (A_BEQ,A_BNE,A_BGT,A_BLT,A_BGE,A_BLE,A_BCS,A_BCC,
  A_BHI,A_BCC,A_BCS,A_BLS);
  { floating point jumps - CURRENTLY NOT USED }
{  A_FBEQ,A_FBNE,A_FBGT,A_FBGE,A_FBLT,A_FBLE); }

 { don't change the order of this table, it is related to }
 { the flags table.                                       }

 flag_2_set: array[F_E..F_BE] of tasmop =
 (A_SEQ,A_SNE,A_SGT,A_SLT,A_SGE,A_SLE,A_SCS,A_SCC,
  A_SHI,A_SCC,A_SCS,A_SLS);


       { operand types }
       top_none = 0;
       top_reg = 1;
       top_ref = 2;
       top_reglist = 5;

       { a constant can be also written as treference }
       top_const = 3;

       { this is for calls }
       top_symbol = 4;

       {This constant is an alias for the stack pointer, as it's name may
        differ from processor to processor.}
       stack_pointer_reg = R_SP;

       frame_pointer_reg = R_A6;

       {This constant is an alias for the accumulator, as it's name may
        differ from processor to processor.}
       accumulator = R_D0;

    type

       pai_labeled = ^tai_labeled;

       tai_labeled = object(tai)
          _operator : tasmop;
          _op1: tregister;
          lab : pasmlabel;
          constructor init(op : tasmop; l : pasmlabel);
          constructor init_reg(op: tasmop; l : pasmlabel; reg: tregister);
          destructor done;virtual;
       end;

       paicpu = ^taicpu;

       taicpu = object(tai)
      { this isn't a proper style, but not very memory expensive }
      op1,op2,op3 : pointer;
      _operator : tasmop;
      op1t,op2t,op3t : byte;
      size : topsize;
     reglist: set of tregister;
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

      { used by MC68020 mul/div }
      constructor op_reg_reg_Reg(op: tasmop;_size: topsize;_op1: tregister; _op2: tregister; _op3: tregister);

     { used by link }
     constructor op_reg_const(op: tasmop; _size: topsize; _op1: tregister; _op2: longint);
      { this is for CALL etc.                            }
      { symbol is replaced by the address of symbol      }
      { so op_csymbol(A_PUSH,S_L,strnew('P')); generates }
      { an instruction which pushes the address of P     }
      { to the stack                                     }
      constructor op_csymbol(op : tasmop;_size : topsize;_op1 : pcsymbol);
      constructor op_csymbol_reg(op : tasmop;_size : topsize;_op1 : pcsymbol;_op2 : tregister);
      constructor op_csymbol_ref(op : tasmop;_size : topsize;_op1 : pcsymbol;_op2 : preference);
      constructor op_csymbol_loc(op : tasmop;_size : topsize;_op1 : pcsymbol;_op2 : tlocation);

     constructor op_ref_reglist(op: tasmop; _size : topsize; _op1: preference;_op2: tregisterlist);
     constructor op_reglist_ref(op: tasmop; _size : topsize; _op1: tregisterlist; _op2: preference);

      destructor done;virtual;
       end;

    const
       maxvarregs = 5;
       maxfpuvarregs = 8;
       varregs : array[1..maxvarregs] of tregister =
        (R_D2,R_D3,R_D4,R_D5,R_D7);
       fpuvarregs : array[1..maxfpuvarregs] of tregister =
        (R_FP0,R_FP1,R_FP2,R_FP3,R_FP4,R_FP5,R_FP6,
         R_FP7);


    { resets all values of ref to defaults }
    procedure reset_reference(var ref : treference);

    { set mostly used values of a new reference }
    function new_reference(base : tregister;offset : longint) : preference;

    { same as reset_reference, but symbol is disposed }
    { use this only for already used references       }
    procedure clear_reference(var ref : treference);

    procedure disposereference(var r : preference);

    function newreference(const r : treference) : preference;

    { generates an help record for constants }
    function newcsymbol(const s : string;l : longint) : pcsymbol;

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
       ao_imm8 = $8;            { 8 bit immediate }
       ao_imm8S   = $10;                { 8 bit immediate sign extended }
       ao_imm16   = $20;                { 16 bit immediate }
       ao_imm32   = $40;                { 32 bit immediate }
       ao_imm1    = $80;        { 1 bit immediate }

       { for  unknown expressions }
       ao_immunknown = ao_imm32;

       { gen'l immediate }
       ao_imm = (ao_imm8 or ao_imm8S or ao_imm16 or ao_imm32);
       ao_disp8   = $200;               { 8 bit displacement (for  jumps) }
       ao_disp16  = $400;               { 16 bit displacement }
       ao_disp32  = $800;               { 32 bit displacement }

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

       ao_control = $40000;     { Control register }
       ao_debug   = $80000;     { Debug register }
       ao_test    = $100000;    { Test register }
       ao_floatreg = $200000;   { Float register }


       ao_jumpabsolute = $4000000;
       ao_abs8 = $08000000;
       ao_abs16 = $10000000;
       ao_abs32 = $20000000;
       ao_abs = (ao_abs8 or ao_abs16 or ao_abs32);

       ao_none = $ff;

  const
     AB_DN     =  1;
     AB_AN     =  2;
     AB_INDAN  =  3;
     AB_INDPP  =  4;
     AB_MMIND  =  5;
     AB_OFFAN  =  6;
     AB_OFFIDX =  7;
     AB_ABSW   =  8;
     AB_ABSL   =  9;
     AB_OFFPC  =  10;
     AB_OFFIDXPC =11;
     AB_IMM      =12;
     AB_REGS     =13;       {*  movem       *}
     AB_BBRANCH  =14;
     AB_WBRANCH  =15;
     AB_CCR      =16;
     AB_SR       =17;
     AB_USP      =18;
     AB_MULDREGS =19;
     AB_MULDREGU =20;

     AF_DN      =(1 SHL AB_DN);
     AF_AN      =(1 SHL AB_AN);
     AF_INDAN   = (1 SHL AB_INDAN);
     AF_INDPP   = (1 SHL AB_INDPP);
     AF_MMIND   = (1 SHL AB_MMIND);
     AF_OFFAN   = (1 SHL AB_OFFAN);
     AF_OFFIDX  = (1 SHL AB_OFFIDX);
     AF_ABSW    = (1 SHL AB_ABSW);
     AF_ABSL    = (1 SHL AB_ABSL);
     AF_OFFPC   = (1 SHL AB_OFFPC);
     AF_OFFIDXPC= (1 SHL AB_OFFIDXPC);
     AF_IMM      =(1 SHL AB_IMM);
     AF_REGS    = (1 SHL AB_REGS);
     AF_BBRANCH = (1 SHL AB_BBRANCH);
     AF_WBRANCH = (1 SHL AB_WBRANCH);
     AF_CCR     =(1 SHL AB_CCR);
     AF_SR      =(1 SHL AB_SR);
     AF_USP     =(1 SHL AB_USP);
     AF_MULDREGS= (1 SHL AB_MULDREGS);
     AF_MULDREGU= (1 SHL AB_MULDREGU);

      AF_ALL     = AF_DN OR AF_AN OR AF_INDAN OR AF_INDPP OR AF_MMIND OR AF_OFFAN OR AF_OFFIDX OR AF_ABSW OR
                  AF_ABSL OR AF_OFFPC OR AF_OFFIDXPC OR AF_IMM;
      AF_ALLNA  = AF_DN OR AF_INDAN OR AF_INDPP OR AF_MMIND OR AF_OFFAN OR AF_OFFIDX OR AF_ABSW OR AF_ABSL
                  OR AF_OFFPC OR AF_OFFIDXPC OR AF_IMM;

      AF_ALT      = AF_DN OR AF_AN OR AF_INDAN OR AF_INDPP OR AF_MMIND OR AF_OFFAN OR
                   AF_OFFIDX OR AF_ABSW OR AF_ABSL;

      AF_ALTNA    = AF_DN OR AF_INDAN OR AF_INDPP OR AF_MMIND OR AF_OFFAN OR AF_OFFIDX OR AF_ABSW OR AF_ABSL;
      AF_ALTM     = AF_INDAN OR AF_INDPP OR AF_MMIND OR AF_OFFAN OR AF_OFFIDX OR AF_ABSW OR AF_ABSL;
      AF_CTL       = AF_INDAN OR AF_OFFAN OR AF_OFFIDX OR AF_ABSW OR AF_ABSL OR AF_OFFPC OR AF_OFFIDXPC;
      AF_CTLNPC   = AF_INDAN OR AF_OFFAN OR AF_OFFIDX OR AF_ABSW OR AF_ABSL OR AF_OFFIDXPC;


{ S_WL  (S_W|S_L)
 S_BW   (S_B|S_W)}
     const
       S_ALL = [S_B] + [S_W] + [S_L];
{#define S_ALL  (S_B|S_W|S_L)}


    type
     ttemplate = record
        i : tasmop;
        oc : longint;
        ops : byte;
       size: set of topsize;
        o1,o2: longint;
    end;

       tins_cache = array[A_ABCD..A_UNLK] of longint;

    var
       ins_cache : tins_cache;

    const
       it : array[0..188] of ttemplate = (

    (   i:A_ABCD; oc: $C100; ops:2;size: [S_B];  o1:AF_DN;      o2:AF_DN           ),
    (   i:A_ABCD; oc: $C108; ops:2;size: [S_B];  o1:AF_MMIND;   o2:AF_MMIND        ),
    (   i:A_ADD;  oc: $D000; ops:2;size: S_ALL;  o1:AF_ALL;     o2:AF_DN           ),
    (   i:A_ADD;  oc: $D100; ops:2;size: S_ALL;  o1:AF_DN;      o2:AF_ALTM         ),
    (   i:A_ADD;  oc: $D0C0; ops:2;size: [S_W];  o1:AF_ALL;     o2:AF_AN           ),
    (   i:A_ADD;  oc: $D1C0; ops:2;size: [S_L];  o1:AF_ALL;     o2:AF_AN           ),
    (   i:A_ADD;  oc: $0600; ops:2;size: S_ALL;  o1:AF_IMM;     o2:AF_ALTNA        ),
    (   i:A_ADDQ; oc: $5000; ops:2;size: S_ALL;  o1:AF_IMM;     o2:AF_ALT        ),
    (   i:A_ADDX; oc: $D100; ops:2;size: S_ALL;  o1:AF_DN;      o2:AF_DN           ),
    (   i:A_ADDX; oc: $D108; ops:2;size: S_ALL;  o1:AF_MMIND;   o2:AF_MMIND        ),
    (   i:A_AND;  oc: $C000; ops:2;size: S_ALL;  o1:AF_ALLNA;   o2:AF_DN           ),
    (   i:A_AND;  oc: $C100; ops:2;size: S_ALL;  o1:AF_DN;      o2:AF_ALTM         ),
    (   i:A_AND;  oc: $0200; ops:2;size: S_ALL;  o1:AF_IMM;     o2:AF_ALTNA        ),
    (   i:A_AND;  oc: $023C; ops:2;size: [S_B];  o1:AF_IMM;     o2:AF_CCR          ),
    (   i:A_AND;  oc: $027C; ops:2;size: [S_W];  o1:AF_IMM;     o2:AF_SR           ),
    (   i:A_ASL;  oc: $E120; ops:2;size: S_ALL;  o1:AF_DN;      o2:AF_DN           ),
    (   i:A_ASL;  oc: $E100; ops:2;size: S_ALL;  o1:AF_IMM;     o2:AF_DN         ),
    (   i:A_ASL;  oc: $E1C0; ops:1;size: [S_W];  o1:0;          o2:AF_ALTM         ),
    (   i:A_ASR;  oc: $E020; ops:2;size: S_ALL;  o1:AF_DN;      o2:AF_DN           ),
    (   i:A_ASR;  oc: $E000; ops:2;size: S_ALL;  o1:AF_IMM;     o2:AF_DN           ),
    (   i:A_ASR;  oc: $E0C0; ops:1;size: [S_W];  o1:0;          o2:AF_ALTM         ),

    (   i:A_BCC;  oc: $6400; ops:1;size: [S_NO]; o1:AF_WBRANCH; o2:0               ),
    (   i:A_BCS;  oc: $6500; ops:1;size: [S_NO]; o1:AF_WBRANCH; o2:0             ),
    (   i:A_BEQ;  oc: $6700; ops:1;size: [S_NO]; o1:AF_WBRANCH; o2:0             ),
    (   i:A_BGE;  oc: $6C00; ops:1;size: [S_NO]; o1:AF_WBRANCH; o2:0             ),
    (   i:A_BGT;  oc: $6E00; ops:1;size: [S_NO]; o1:AF_WBRANCH; o2:0             ),
    (   i:A_BHI;  oc: $6200; ops:1;size: [S_NO]; o1:AF_WBRANCH; o2:0             ),
    (   i:A_BLE;  oc: $6F00; ops:1;size: [S_NO]; o1:AF_WBRANCH; o2:0               ),
    (   i:A_BLS;  oc: $6300; ops:1;size: [S_NO]; o1:AF_WBRANCH; o2:0               ),
    (   i:A_BLT;  oc: $6D00; ops:1;size: [S_NO]; o1:AF_WBRANCH; o2:0               ),
    (   i:A_BMI;  oc: $6B00; ops:1;size: [S_NO]; o1:AF_WBRANCH; o2:0             ),
    (   i:A_BNE;  oc: $6600; ops:1;size: [S_NO]; o1:AF_WBRANCH; o2:0             ),
    (   i:A_BPL;  oc: $6A00; ops:1;size: [S_NO]; o1:AF_WBRANCH; o2:0             ),
    (   i:A_BVC;  oc: $6800; ops:1;size: [S_NO]; o1:AF_WBRANCH; o2:0             ),
    (   i:A_BVS;  oc: $6900; ops:1;size: [S_NO]; o1:AF_WBRANCH; o2:0             ),

    {*      opcode   Temp   Rs  EAs Rd  EAd Siz Sizes  SModes   DModes    Spec# *}

    (   i:A_BCHG; oc: $0140;  ops:2;size: [S_B];    o1:AF_DN;      o2:AF_ALTM         ),
    (   i:A_BCHG; oc: $0140;  ops:2;size: [S_L];    o1:AF_DN;      o2:AF_DN           ),
    (   i:A_BCHG; oc: $0840;  ops:2;size: [S_B];    o1:AF_IMM;     o2:AF_ALTM         ),
    (   i:A_BCHG; oc: $0840;  ops:2;size: [S_L];    o1:AF_IMM;     o2:AF_DN           ),
    (   i:A_BCLR; oc: $0180;  ops:2;size: [S_B];    o1:AF_DN;      o2:AF_ALTM         ),
    (   i:A_BCLR; oc: $0180;  ops:2;size: [S_L];    o1:AF_DN;      o2:AF_DN           ),
    (   i:A_BCLR; oc: $0880;  ops:2;size: [S_B];    o1:AF_IMM;     o2:AF_ALTM         ),
    (   i:A_BCLR; oc: $0880;  ops:2;size: [S_L];    o1:AF_IMM;     o2:AF_DN           ),

    (   i:A_BRA;  oc: $6000;  ops:1;size: [S_NO];    o1:AF_WBRANCH;o2:0              ),

    (   i:A_BSET; oc: $01C0;  ops:2;size: [S_B];    o1:AF_DN;      o2:AF_ALTM         ),
    (   i:A_BSET; oc: $01C0;  ops:2;size: [S_L];    o1:AF_DN;      o2:AF_DN           ),
    (   i:A_BSET; oc: $08C0;  ops:2;size: [S_B];    o1:AF_IMM;     o2:AF_ALTM         ),
    (   i:A_BSET; oc: $08C0;  ops:2;size: [S_L];    o1:AF_IMM;     o2:AF_DN           ),
    (   i:A_BTST; oc: $0100;  ops:2;size: [S_B];    o1:AF_DN;      o2:AF_ALTM         ),
    (   i:A_BTST; oc: $0100;  ops:2;size: [S_L];    o1:AF_DN;      o2:AF_DN           ),
    (   i:A_BTST; oc: $0800;  ops:2;size: [S_B];    o1:AF_IMM;     o2:AF_ALTM         ),
    (   i:A_BTST; oc: $0800;  ops:2;size: [S_L];    o1:AF_IMM;     o2:AF_DN           ),

    {*      opcode   Temp   Rs  EAs Rd  EAd Siz Sizes  SModes   DModes    Spec# *}

    (   i:A_CHK;  oc: $4180;  ops:2;size: [S_W];  o1:AF_ALLNA;   o2:AF_DN           ),
    (   i:A_CLR;  oc: $4200;  ops:1;size: S_ALL;  o1:AF_ALTNA;   o2:0  ),
    (   i:A_CMP;  oc: $B000;  ops:2;size: S_ALL;  o1:AF_ALLNA;   o2:AF_DN           ),
    (   i:A_CMP;  oc: $B000;  ops:2;size: [S_WL]; o1:AF_AN;      o2:AF_DN           ),
    (   i:A_CMP;  oc: $B0C0;  ops:2;size: [S_W];  o1:AF_ALL;     o2:AF_AN           ),
    (   i:A_CMP;  oc: $B1C0;  ops:2;size: [S_L];  o1:AF_ALL;     o2:AF_AN           ),
    ( i:A_CMP;  oc: $0C00;  ops:2;size: S_ALL;  o1:AF_IMM;     o2:AF_ALTNA        ),
    (   i:A_CMP;  oc: $B108;  ops:2;size: S_ALL;  o1:AF_INDPP;   o2:AF_INDPP        ),

    ( i:A_DBCC; oc: $54C8;  ops:2;size: [S_NO];    o1:AF_DN;   o2:AF_WBRANCH       ),
    (   i:A_DBCS; oc: $55C8;  ops:2;size: [S_NO];    o1:AF_DN;   o2:AF_WBRANCH       ),
    (   i:A_DBEQ; oc: $57C8;  ops:2;size: [S_NO];    o1:AF_DN;   o2:AF_WBRANCH       ),
    (   i:A_DBF; oc: $51C8;   ops:2;size: [S_NO];    o1:AF_DN;   o2:AF_WBRANCH       ),
    (   i:A_DBGE; oc: $5CC8;  ops:2;size: [S_NO];    o1:AF_DN;   o2:AF_WBRANCH       ),
    (   i:A_DBGT; oc: $5EC8;  ops:2;size: [S_NO];    o1:AF_DN;   o2:AF_WBRANCH       ),
    (   i:A_DBHI; oc: $52C8;  ops:2;size: [S_NO];    o1:AF_DN;   o2:AF_WBRANCH       ),
    (   i:A_DBLE; oc: $5FC8;  ops:2;size: [S_NO];    o1:AF_DN;   o2:AF_WBRANCH       ),
    (   i:A_DBLS; oc: $53C8;  ops:2;size: [S_NO];    o1:AF_DN;   o2:AF_WBRANCH       ),
    (   i:A_DBLT; oc: $5DC8;  ops:2;size: [S_NO];    o1:AF_DN;   o2:AF_WBRANCH       ),
    (   i:A_DBMI; oc: $5BC8;  ops:2;size: [S_NO];    o1:AF_DN;   o2:AF_WBRANCH       ),
    (   i:A_DBNE; oc: $56C8;  ops:2;size: [S_NO];    o1:AF_DN;   o2:AF_WBRANCH       ),
    (   i:A_DBPL; oc: $5AC8;  ops:2;size: [S_NO];    o1:AF_DN;   o2:AF_WBRANCH       ),
    (   i:A_DBT; oc: $50C8;   ops:2;size: [S_NO];    o1:AF_DN;   o2:AF_WBRANCH       ),
    (   i:A_DBVC; oc: $58C8;  ops:2;size: [S_NO];    o1:AF_DN;   o2:AF_WBRANCH       ),
    (   i:A_DBVS; oc: $59C8;  ops:2;size: [S_NO];    o1:AF_DN;   o2:AF_WBRANCH       ),

    {*      opcode   Temp   Rs  EAs Rd  EAd Siz Sizes  SModes   DModes    Spec# *}

    (   i:A_DIVS; oc: $81C0;  ops:2;size: [S_W];    o1:AF_ALLNA;   o2:AF_DN           ),
    (   i:A_DIVS; oc: $4C40;  ops:2;size: [S_L];    o1:AF_ALLNA;   o2:AF_DN OR AF_MULDREGS),  {*  020 *}
    (   i:A_DIVU; oc: $80C0;  ops:2;size: [S_W];    o1:AF_ALLNA;   o2:AF_DN           ),
    (   i:A_DIVU; oc: $4C40;  ops:2;size: [S_L];    o1:AF_ALLNA;   o2:AF_DN OR AF_MULDREGU),  {*  020 *}


    (   i:A_EOR;   oc: $B100; ops:2;size: S_ALL;  o1:AF_DN;      o2:AF_ALTNA        ),
    (   i:A_EOR;  oc: $0A00;  ops:2;size: S_ALL;  o1:AF_IMM;     o2:AF_ALTNA        ),
    (   i:A_EOR;  oc: $0A3C;  ops:2;size: [S_B];  o1:AF_IMM;     o2:AF_CCR          ),
    (   i:A_EOR;  oc: $0A7C;  ops:2;size: [S_W];  o1:AF_IMM;     o2:AF_SR           ),
    (   i:A_EXG;  oc: $C140;  ops:2;size: [S_L];  o1:AF_DN;      o2:AF_DN           ),
    (   i:A_EXG;  oc: $C148;  ops:2;size: [S_L];  o1:AF_AN;      o2:AF_AN           ),
    (   i:A_EXG;  oc: $C188;  ops:2;size: [S_L];  o1:AF_DN;      o2:AF_AN           ),
    (   i:A_EXG;  oc: $C188;  ops:2;size: [S_L];  o1:AF_AN;      o2:AF_DN           ),
    (   i:A_EXT;  oc: $4880;  ops:1;size: [S_W];  o1:AF_DN;      o2:0    ),
    (   i:A_EXT;  oc: $48C0;  ops:1;size: [S_L];  o1:AF_DN;      o2:0    ),
    { MC68020 }
    (   i:A_EXTB; oc: $49C0;  ops:1;size: [S_L];  o1:AF_DN;      o2:0    ),
    (   i:A_ILLEGAL;oc: $4AFC;ops:0;size: [S_NO];   o1:0;             o2:0    ),


    {*
     *  note: BSR/BSR/JSR ordering must remain as it is (passc.c optimizations)
     *}
    (   i:A_JMP;  oc: $4EC0;  ops:1;size: [S_NO];      o1:AF_CTL;     o2:0               ),
    (   i:A_BSR;  oc: $6100;  ops:1;size: [S_NO];      o1:AF_WBRANCH; o2: 0              ),
    (   i:A_JSR;  oc: $4E80;  ops:1;size: [S_NO];      o1:AF_CTL;     o2:0               ),

    (   i:A_LEA;  oc: $41C0;  ops:2;size: [S_L];    o1:AF_CTL;     o2:AF_AN           ),
    (   i:A_LINK; oc: $4E50;  ops:2;size: [S_W];    o1:AF_AN;      o2:AF_IMM          ),

    {*      opcode   Temp   Rs  EAs Rd  EAd Siz Sizes  SModes   DModes    Spec# *}

    (   i:A_LSL;  oc: $E128;  ops:2;size: S_ALL;  o1:AF_DN;      o2:AF_DN           ),
    (   i:A_LSL;  oc: $E108;  ops:2;size: S_ALL;  o1:AF_IMM;     o2:AF_DN           ),
    (   i:A_LSL;  oc: $E3C0;  ops:1;size: [S_W];  o1:0;          o2:AF_ALTM         ),
    (   i:A_LSR;  oc: $E028;  ops:2;size: S_ALL;  o1:AF_DN;      o2:AF_DN           ),
    (   i:A_LSR;  oc: $E008;  ops:2;size: S_ALL;  o1:AF_IMM;     o2:AF_DN           ),
    (   i:A_LSR;  oc: $E2C0;  ops:1;size: [S_W];  o1:0;          o2:AF_ALTM         ),

    (   i:A_MOVE; oc: $2000;  ops:2;size: [S_L];    o1:AF_ALLNA;   o2:AF_ALTNA        ),
    (   i:A_MOVE; oc: $3000;  ops:2;size: [S_W];    o1:AF_ALLNA;   o2:AF_ALTNA        ),
    (   i:A_MOVE; oc: $1000;  ops:2;size: [S_B];    o1:AF_ALLNA;   o2:AF_ALTNA        ),
    (   i:A_MOVE; oc: $2000;  ops:2;size: [S_L];    o1:AF_AN;      o2:AF_ALTNA        ),
    (   i:A_MOVE; oc: $3000;  ops:2;size: [S_W];    o1:AF_AN;      o2:AF_ALTNA        ),

  {* 68010
   *(   'MOVE'; i:A_MOVE; oc: $42C0; -1; -1;  0;  3; -1; size: [S_W];    o1:AF_CCR;     o1:AF_ALTNA        ),
   *}

    (   i:A_MOVE; oc: $44C0;  ops:2;size: [S_W];    o1:AF_ALLNA;   o2:AF_CCR          ),
    (   i:A_MOVE; oc: $46C0;  ops:2;size: [S_W];    o1:AF_ALLNA;   o2:AF_SR           ),
    (   i:A_MOVE; oc: $40C0;  ops:2;size: [S_W];    o1:AF_SR;      o2:AF_ALTNA        ),
    (   i:A_MOVE; oc: $3040;  ops:2;size: [S_W];    o1:AF_ALL;     o2:AF_AN           ),
    (   i:A_MOVE; oc: $2040;  ops:2;size: [S_L];    o1:AF_ALL;     o2:AF_AN           ),
    (   i:A_MOVE; oc: $4E68;  ops:2;size: [S_L];    o1:AF_USP;     o2:AF_AN           ),
    (   i:A_MOVE; oc: $4E60;  ops:2;size: [S_L];    o1:AF_AN;      o2:AF_USP          ),
    {* MOVEC 68010  *}
    (   i:A_MOVEM;oc: $48C0; ops:8;size: [S_L];    o1:AF_REGS;    o2:AF_CTL OR AF_MMIND ),
    (   i:A_MOVEM;oc: $4880; ops:8;size: [S_W];    o1:AF_REGS;    o2:AF_CTL OR AF_MMIND ),
    (   i:A_MOVEM;oc: $4CC0; ops:8;size: [S_L];    o1:AF_CTL OR AF_INDPP;    o2:AF_REGS ),
    (   i:A_MOVEM;oc: $4C80; ops:8;size: [S_W];    o1:AF_CTL OR AF_INDPP;    o2:AF_REGS ),
    (   i:A_MOVEP;oc: $0188; ops:2;size: [S_W];    o1:AF_DN;      o2:AF_OFFAN        ),
    (   i:A_MOVEP;oc: $01C8; ops:2;size: [S_L];    o1:AF_DN;      o2:AF_OFFAN        ),
    (   i:A_MOVEP;oc: $0108; ops:2;size: [S_W];    o1:AF_OFFAN;   o2:AF_DN           ),
    (   i:A_MOVEP;oc: $0148; ops:2;size: [S_L];    o1:AF_OFFAN;   o2:AF_DN           ),
    {*  MOVES   68010   *}
    (   i:A_MOVEQ;oc: $7000; ops:2;size: [S_L];    o1:AF_IMM;    o2:AF_DN           ),

    {*      opcode   Temp   Rs  EAs Rd  EAd Siz Sizes  SModes   DModes    Spec# *}

    (   i:A_MULS; oc: $C1C0;  ops:2;size: [S_W];    o1:AF_ALLNA;   o2:AF_DN           ),
    (   i:A_MULS; oc: $4C00;  ops:2;size: [S_L];    o1:AF_ALLNA;   o2:AF_DN OR AF_MULDREGS),  {*  020 *}
    (   i:A_MULU; oc: $C0C0;  ops:2;size: [S_W];    o1:AF_ALLNA;   o2:AF_DN           ),
    (   i:A_MULU; oc: $4C00;  ops:2;size: [S_L];    o1:AF_ALLNA;   o2:AF_DN OR AF_MULDREGU),  {*  020 *}
    (   i:A_NBCD; oc: $4800;  ops:1;size: [S_B];    o1:AF_ALTNA;   o2:0    ),
    (   i:A_NEG;  oc: $4400;  ops:1;size: S_ALL;    o1:AF_ALTNA;   o2:0    ),
    (   i:A_NEGX; oc: $4000;  ops:1;size: S_ALL;    o1:AF_ALTNA;   o2:0    ),
    (   i:A_NOP;  oc: $4E71;  ops:0;size: [S_NO];   o1:0;          o2:0    ),
    (   i:A_NOT;  oc: $4600;  ops:1;size: S_ALL;    o1:AF_ALTNA;   o2:0    ),

    (   i:A_OR;   oc: $8000;  ops:2;size: S_ALL;    o1:AF_ALLNA;   o2:AF_DN           ),
    (   i:A_OR;   oc: $8100;  ops:2;size: S_ALL;    o1:AF_DN;      o2:AF_ALTNA        ),
    ( i:A_OR;   oc: $0000;  ops:2;size: S_ALL;    o1:AF_IMM;     o2:AF_ALTNA        ),
    (   i:A_OR;   oc: $003C;  ops:2;size: [S_B];    o1:AF_IMM;     o2:AF_CCR          ),
    (   i:A_OR;   oc: $007C;  ops:2;size: [S_W];    o1:AF_IMM;     o2:AF_SR           ),
    (   i:A_PEA;  oc: $4840;  ops:1;size: [S_L];    o1:AF_CTL;     o2:0               ),
    (   i:A_RESET;oc: $4E70;  ops:0;size: [S_NO];   o1:0;          o2:0               ),

    {*      opcode   Temp   Rs  EAs Rd  EAd Siz Sizes  SModes   DModes    Spec# *}

    (   i:A_ROL;  oc: $E138;  ops:2;size: S_ALL;  o1:AF_DN;      o2:AF_DN           ),
    (   i:A_ROL;  oc: $E118;  ops:2;size: S_ALL;  o1:AF_IMM;     o2:AF_DN           ),
    (   i:A_ROL;  oc: $E7C0;  ops:1;size: [S_W];  o1:AF_ALTM;    o2:0               ),
    (   i:A_ROR;  oc: $E038;  ops:2;size: S_ALL;  o1:AF_DN;      o2:AF_DN           ),
    (   i:A_ROR;  oc: $E018;  ops:2;size: S_ALL;  o1:AF_IMM;     o2:AF_DN           ),
    (   i:A_ROR;  oc: $E6C0;  ops:1;size: [S_W];  o1:AF_ALTM;    o2:0               ),

    (   i:A_ROXL; oc: $E130;  ops:2;size: S_ALL;  o1:AF_DN;      o2:AF_DN           ),
    (   i:A_ROXL; oc: $E110;  ops:2;size: S_ALL;  o1:AF_IMM;     o2:AF_DN           ),
    (   i:A_ROXL; oc: $E5C0;  ops:1;size: [S_W];  o1:AF_ALTM;    o2:0               ),
    (   i:A_ROXR; oc: $E030;  ops:2;size: S_ALL;  o1:AF_DN;      o2:AF_DN           ),
    (   i:A_ROXR; oc: $E010;  ops:2;size: S_ALL;  o1:AF_IMM;     o2:AF_DN           ),
    (   i:A_ROXR; oc: $E4C0;  ops:1;size: [S_W];  o1:AF_ALTM;    o2:0               ),

    {*  RTD 68010   *}
    (   i:A_RTE;  oc: $4E73;  ops:0;size: [S_NO];   o1:0;          o2:0               ),
    (   i:A_RTR;  oc: $4E77;  ops:0;size: [S_NO];   o1:0;          o2:0               ),
    (   i:A_RTS;  oc: $4E75;  ops:0;size: [S_NO];   o1:0;          o2:0               ),
    (   i:A_SBCD; oc: $8100;  ops:2;size: [S_B];    o1:AF_DN;      o2:AF_DN           ),
    (   i:A_SBCD; oc: $8108;  ops:2;size: [S_B];    o1:AF_MMIND;   o2:AF_MMIND        ),

    {*      opcode   Temp   Rs  EAs Rd  EAd Siz Sizes  SModes   DModes    Spec# *}

    {* SCC note; even though they are in the same group since all have the
     * same note if one isn't accepted none of the others will be either
     *}

    (   i:A_SCC;  oc: $54C0;  ops:1;size: [S_B]; o1:AF_ALTNA; o2: 0        ),
    (   i:A_SCS;  oc: $55C0;  ops:1;size: [S_B]; o1:AF_ALTNA; o2: 0        ),
    (   i:A_SEQ;  oc: $57C0;  ops:1;size: [S_B]; o1:AF_ALTNA; o2: 0        ),
    (   i:A_SF;   oc: $51C0;  ops:1;size: [S_B]; o1:AF_ALTNA; o2: 0        ),
    (   i:A_SGE;  oc: $5CC0;  ops:1;size: [S_B]; o1:AF_ALTNA; o2: 0        ),
    (   i:A_SGT;  oc: $5EC0;  ops:1;size: [S_B]; o1:AF_ALTNA; o2: 0        ),
    (   i:A_SHI;  oc: $52C0;  ops:1;size: [S_B]; o1:AF_ALTNA; o2: 0        ),
    (   i:A_SLE;  oc: $5FC0;  ops:1;size: [S_B]; o1:AF_ALTNA; o2: 0        ),
    (   i:A_SLS;  oc: $53C0;  ops:1;size: [S_B]; o1:AF_ALTNA; o2: 0        ),
    (   i:A_SLT;  oc: $5DC0;  ops:1;size: [S_B]; o1:AF_ALTNA; o2: 0        ),
    (   i:A_SMI;  oc: $5BC0;  ops:1;size: [S_B]; o1:AF_ALTNA; o2: 0        ),
    (   i:A_SNE;  oc: $56C0;  ops:1;size: [S_B]; o1:AF_ALTNA; o2: 0        ),
    (   i:A_SPL;  oc: $5AC0;  ops:1;size: [S_B]; o1:AF_ALTNA; o2: 0        ),
    (   i:A_ST;   oc: $50C0;  ops:1;size: [S_B]; o1:AF_ALTNA; o2: 0        ),
    (   i:A_SVC;  oc: $58C0;  ops:1;size: [S_B]; o1:AF_ALTNA; o2: 0        ),
    (   i:A_SVS;  oc: $59C0;  ops:1;size: [S_B]; o1:AF_ALTNA; o2: 0        ),

    {*      opcode   Temp   Rs  EAs Rd  EAd Siz Sizes  SModes   DModes    Spec# *}

    (   i:A_STOP; oc: $4E72; ops:0; size: [S_W]; o1:AF_IMM;  o2:   0               ),

    (   i:A_SUB;  oc: $9000; ops:2;size: S_ALL;  o1:AF_ALL;     o2:AF_DN           ),
    (   i:A_SUB;  oc: $9100; ops:2;size: S_ALL;  o1:AF_DN;      o2:AF_ALTM         ),
    (   i:A_SUB;  oc: $90C0; ops:2;size: [S_W];  o1:AF_ALL;     o2:AF_AN           ),
    (   i:A_SUB;  oc: $91C0; ops:2;size: [S_L];  o1:AF_ALL;     o2:AF_AN           ),
    (   i:A_SUB;  oc: $0400; ops:2;size: S_ALL;  o1:AF_IMM;     o2:AF_ALTNA        ),
    (   i:A_SUBQ; oc: $5100; ops:2;size: S_ALL;  o1:AF_IMM;     o2:AF_ALT          ),
    (   i:A_SUBX; oc: $9100; ops:2;size: S_ALL;  o1:AF_DN;      o2:AF_DN           ),
    (   i:A_SUBX; oc: $9108; ops:2;size: S_ALL;  o1:AF_MMIND;   o2:AF_MMIND        ),

    (   i:A_SWAP; oc: $4840; ops:1;size: [S_W];  o1:AF_DN;      o2:0    ),
    (   i:A_TAS;  oc: $4AC0; ops:1;size: [S_B];  o1:AF_ALTNA;   o2:0    ),
    (   i:A_TRAP; oc: $4E40; ops:1;size: [S_NO]; o1:AF_IMM;     o2:0               ),
    (   i:A_TRAPV;oc: $4E76; ops:0;size: [S_NO]; o1:0;          o2:0               ),
    (   i:A_TST;  oc: $4A00; ops:1;size: S_ALL;  o1:AF_ALTNA;   o2:0               ),
    (   i:A_UNLK; oc: $4E58; ops:1;size: [S_NO]; o1:AF_AN;      o2:0               ),
    ( i:A_NONE)
    );

{****************************************************************************
                            Assembler Mnemoics
****************************************************************************}

   const
     firstop = A_ABCD;
     lastop = A_LABEL;

     mot_op2str : array[firstop..lastop] of string[10] =
       { 68000 only instructions }
       ('abcd','add', 'adda','addi','addq','addx','and','andi',
       'asl','asr','bcc','bcs','beq','bge','bgt','bhi',
       'ble','bls','blt','bmi','bne','bpl','bvc','bvs',
       'bchg','bclr','bra','bset','bsr','btst','chk',
       'clr','cmp','cmpa','cmpi','cmpm','dbcc','dbcs','dbeq','dbge',
       'dbgt','dbhi','dble','dbls','dblt','dbmi','dbne','dbra',
       'dbpl','dbt','dbvc','dbvs','dbf','divs','divu',
       'eor','eori','exg','illegal','ext','jmp','jsr',
       'lea','link','lsl','lsr','move','movea','movei','moveq',
       'movem','movep','muls','mulu','nbcd','neg','negx',
       'nop','not','or','ori','pea','rol','ror','roxl',
       'roxr','rtr','rts','sbcd','scc','scs','seq','sge',
       'sgt','shi','sle','sls','slt','smi','sne',
       'spl','st','svc','svs','sf','sub','suba','subi','subq',
       'subx','swap','tas','trap','trapv','tst','unlk',
       'rte','reset','stop',
       { MC68010 instructions }
       'bkpt','movec','moves','rtd',
       { MC68020 instructions }
       'bfchg','bfclr','bfexts','bfextu','bfffo',
       'bfins','bfset','bftst','callm','cas','cas2',
       'chk2','cmp2','divsl','divul','extb','pack','rtm',
       'trapcc','tracs','trapeq','trapf','trapge','trapgt',
       'traphi','traple','trapls','traplt','trapmi','trapne',
       'trappl','trapt','trapvc','trapvs','unpk',
       { FPU Processor instructions - directly supported only. }
       { IEEE aware and misc. condition codes not supported   }
       'fabs','fadd',
       'fbeq','fbne','fbngt','fbgt','fbge','fbnge',
       'fblt','fbnlt','fble','fbgl','fbngl','fbgle','fbngle',
       'fdbeq','fdbne','fdbgt','fdbngt','fdbge','fdnbge',
       'fdblt','fdbnlt','fdble','fdbgl','fdbngl','fdbgle','fbdngle',
       'fseq','fsne','fsgt','fsngt','fsge','fsnge',
       'fslt','fsnlt','fsle','fsgl','fsngl','fsgle','fsngle',
       'fcmp','fdiv','fmove','fmovem',
       'fmul','fneg','fnop','fsqrt','fsub','fsgldiv',
       'fsflmul','ftst',
       'ftrapeq','ftrapne','ftrapgt','ftrapngt','ftrapge','ftrapnge',
       'ftraplt','ftrapnlt','ftraple','ftrapgl','ftrapngl','ftrapgle',
       'ftrapngle',
       { Useful for assembly langage output }
       { Protected instructions }
       'cprestore','cpsave',
       { FPU Unit protected instructions                    }
       { and 68030/68851 common MMU instructions            }
       { (this may include 68040 MMU instructions)          }
       'frestore','fsave','pflush','pflusha','pload','pmove','ptest',
       { Useful for assembly langage output }
       '');

     mot_opsize2str : array[topsize] of string[2] =
      ('','.b','.w','.l','.b','.b','.w',
       '.s','.d','.x','.s','.l','.q');
       { I don't know about S_IS, S_IL and S_IQ for m68k
         so I guessed, I am not even sure it can happen !!
         (PM) }

     mot_reg2str : array[R_NO..R_FPSR] of string[6] =
      ('', 'd0','d1','d2','d3','d4','d5','d6','d7',
       'a0','a1','a2','a3','a4','a5','a6','sp',
       '-(sp)','(sp)+',
       'ccr','fp0','fp1','fp2','fp3','fp4','fp5',
       'fp6','fp7','fpcr','sr','ssp','dfc',
       'sfc','vbr','fpsr');

     gas_opsize2str : array[topsize] of string[2] =
      ('','.b','.w','.l','.b','.b','.w',
       '.s','.d','.x','.s','.l','.q');

     gas_reg2str : array[R_NO..R_FPSR] of string[6] =
      ('', 'd0','d1','d2','d3','d4','d5','d6','d7',
       'a0','a1','a2','a3','a4','a5','a6','sp',
       '-(sp)','(sp)+',
       'ccr','fp0','fp1','fp2','fp3','fp4','fp5',
       'fp6','fp7','fpcr','sr','ssp','dfc',
       'sfc','vbr','fpsr');

     mit_opsize2str : array[topsize] of string[2] =
      ('','b','w','l','b','b','w',
       's','d','x','s','l','q');

     mit_reg2str : array[R_NO..R_FPSR] of string[6] =
      ('', 'd0','d1','d2','d3','d4','d5','d6','d7',
       'a0','a1','a2','a3','a4','a5','a6','sp',
       'sp@-','sp@+',
       'ccr','fp0','fp1','fp2','fp3','fp4','fp5',
       'fp6','fp7','fpcr','sr','ssp','dfc',
       'sfc','vbr','fpsr');

     gasPalmOS_reg2str : array[R_NO..R_FPSR] of string[6] =
      ('', '%d0','%d1','%d2','%d3','%d4','%d5','%d6','%d7',
       '%a0','%a1','%a2','%a3','%a4','%a5','%a6','%sp',
       '-(%sp)','(%sp)+',
       '%ccr','%fp0','%fp1','%fp2','%fp3','%fp4','%fp5',
       '%fp6','%fp7','%fpcr','%sr','%ssp','%dfc',
       '%sfc','%vbr','%fpsr');


{*****************************************************************************
                                  Init/Done
*****************************************************************************}

  procedure InitCpu;
  procedure DoneCpu;
  
  implementation

    uses
      strings,globals,verbose;


    procedure disposereference(var r : preference);

      begin
         if assigned(r^.symbol) then
           stringdispose(r^.symbol);
         dispose(r);
         r:=nil;
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

    procedure reset_reference(var ref : treference);

      begin
         with ref do
        begin
          index:=R_NO;
          base:=R_NO;
          segment:=R_DEFAULT_SEG;
          offset:=0;
          scalefactor:=1;
          isintvalue:=false;
          symbol:=nil;
          direction := dir_none;
        end;
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
                 Taicpu
 ****************************************************************************}

    constructor taicpu.op_none(op : tasmop;_size : topsize);

      begin
     inherited init;
     typ:=ait_instruction;
     _operator:=op;
     op1t:=top_none;
     op2t:=top_none;
     op3t:=top_none;
     size:=_size;

     { the following isn't required ! }
     op1:=nil;
     op2:=nil;
     op3:=nil;
      end;

    constructor taicpu.op_reg(op : tasmop;_size : topsize;_op1 : tregister);

      begin
     inherited init;
     typ:=ait_instruction;
     _operator:=op;
     op1t:=top_reg;
     op2t:=top_none;
     op3t:=top_none;
     size:=_size;
     op1:=pointer(_op1);

     op2:=nil;
     op3:=nil;
      end;

    constructor taicpu.op_const(op : tasmop;_size : topsize;_op1 : longint);

 begin
     inherited init;
     typ:=ait_instruction;
     _operator:=op;
     op1t:=top_const;
     op2t:=top_none;
     op3t:=top_none;
     size:=_size;
     op1:=pointer(_op1);

     op2:=nil;
     op3:=nil;
  end;



    constructor taicpu.op_ref(op : tasmop;_size : topsize;_op1 : preference);

      begin
     inherited init;
     typ:=ait_instruction;
     _operator:=op;
     op2t:=top_none;
     op3t:=top_none;
     size:=_size;
     if _op1^.isintvalue then
       begin
          op1t:=top_const;
          op1:=pointer(_op1^.offset);
          disposereference(_op1);
       end
     else
       begin
          op1t:=top_ref;
          op1:=pointer(_op1);
       end;

     op2:=nil;
     op3:=nil;
      end;

    constructor taicpu.op_loc(op : tasmop;_size : topsize;_op1 : tlocation);

      begin
     inherited init;
     typ:=ait_instruction;
     _operator:=op;
     op2t:=top_none;
     op3t:=top_none;
     size:=_size;
     if (_op1.loc=loc_register) or (_op1.loc=loc_cregister)  then
       begin
         op1t:=top_reg;
         op1:=pointer(_op1.register);
       end
     else
     if _op1.reference.isintvalue then
       begin
          op1t:=top_const;
          op1:=pointer(_op1.reference.offset);
       end
     else
       begin
          op1t:=top_ref;
          op1:=pointer(newreference(_op1.reference));
       end;

     op2:=nil;
     op3:=nil;
      end;

    constructor taicpu.op_reg_reg(op : tasmop;_size : topsize;_op1,_op2 : tregister);

      begin
     inherited init;
     typ:=ait_instruction;
     _operator:=op;
     op1t:=top_reg;
     op2t:=top_reg;
     op3t:=top_none;
     size:=_size;
     op1:=pointer(_op1);
     op2:=pointer(_op2);

     op3:=nil;
      end;

    constructor taicpu.op_reg_ref(op : tasmop;_size : topsize;_op1 : tregister;_op2 : preference);

      begin
     inherited init;
     typ:=ait_instruction;
     _operator:=op;
     op1t:=top_reg;
     op3t:=top_none;
     size:=_size;
     op1:=pointer(_op1);

     if _op2^.isintvalue then
       begin
          op2t:=top_const;
          op2:=pointer(_op2^.offset);
          disposereference(_op2);
       end
     else
       begin
          op2t:=top_ref;
          op2:=pointer(_op2);
       end;

     op3:=nil;
      end;

    constructor taicpu.op_reg_loc(op : tasmop;_size : topsize;_op1 : tregister;_op2 : tlocation);

      begin
     inherited init;
     typ:=ait_instruction;
     _operator:=op;
     op1t:=top_reg;
     op3t:=top_none;
     size:=_size;
     op1:=pointer(_op1);

     if (_op2.loc=loc_register) or (_op2.loc=loc_cregister)  then
       begin
         op2t:=top_reg;
         op2:=pointer(_op2.register);
       end
     else
     if _op2.reference.isintvalue then
       begin
          op2t:=top_const;
          op2:=pointer(_op2.reference.offset);
       end
     else
       begin
          op2t:=top_ref;
          op2:=pointer(newreference(_op2.reference));
       end;

     op3:=nil;
      end;

    constructor taicpu.op_loc_reg(op : tasmop;_size : topsize;_op1 : tlocation;_op2 : tregister);

      begin
     inherited init;
     typ:=ait_instruction;
     _operator:=op;
     op2t:=top_reg;
     op3t:=top_none;
     size:=_size;
     op2:=pointer(_op2);

     if (_op1.loc=loc_register) or (_op1.loc=loc_cregister)  then
       begin
         op1t:=top_reg;
         op1:=pointer(_op1.register);
       end
     else
     if _op1.reference.isintvalue then
       begin
          op1t:=top_const;
          op1:=pointer(_op1.reference.offset);
       end
     else
       begin
          op1t:=top_ref;
          op1:=pointer(newreference(_op1.reference));
       end;

     op3:=nil;
      end;

    constructor taicpu.op_const_reg_reg(op : tasmop;_size : topsize;_op1 : longint;_op2 : tregister;_op3 : tregister);

      begin
     inherited init;
     typ:=ait_instruction;
     _operator:=op;
     op1t:=top_const;
     op2t:=top_reg;
     op3t:=top_reg;
     size:=_size;
     op1:=pointer(_op1);
     op2:=pointer(_op2);
     op3:=pointer(_op3);
      end;

  constructor taicpu.op_reg_const(op: tasmop; _size: topsize; _op1: tregister; _op2: longint);
   begin
    inherited init;
    typ := ait_instruction;
    _operator := op;
    op1t := top_reg;
    op2t := top_const;
    op3t := top_none;
    op1 := pointer(_op1);
    op2 := pointer(_op2);
    size := _size;
   end;


    constructor taicpu.op_reg_reg_reg(op : tasmop;_size : topsize;_op1 : tregister;_op2 : tregister;_op3 : tregister);

      begin
     inherited init;
     typ:=ait_instruction;
     _operator:=op;
     op1t:=top_reg;
     op2t:=top_reg;
     op3t:=top_reg;
     size:=_size;
     op1:=pointer(_op1);
     op2:=pointer(_op2);
     op3:=pointer(_op3);
      end;

    constructor taicpu.op_const_reg(op : tasmop;_size : topsize;_op1 : longint;_op2 : tregister);

      begin
     inherited init;
     typ:=ait_instruction;
     _operator:=op;
     op1t:=top_const;
     op2t:=top_reg;
     op3t:=top_none;
     size:=_size;
     op1:=pointer(_op1);
     op2:=pointer(_op2);

     op3:=nil;
      end;

    constructor taicpu.op_const_const(op : tasmop;_size : topsize;_op1,_op2 : longint);

      begin
     inherited init;
     typ:=ait_instruction;
     _operator:=op;
     op1t:=top_const;
     op2t:=top_const;
     op3t:=top_none;
     size:=_size;
     op1:=pointer(_op1);
     op2:=pointer(_op2);

     op3:=nil;
      end;

    constructor taicpu.op_const_ref(op : tasmop;_size : topsize;_op1 : longint;_op2 : preference);

      begin
     inherited init;
     typ:=ait_instruction;
     _operator:=op;
     op1t:=top_const;
     op3t:=top_none;
     size:=_size;
     op1:=pointer(_op1);

     if _op2^.isintvalue then
       begin
          op2t:=top_const;
          op2:=pointer(_op2^.offset);
          disposereference(_op2);
       end
     else
       begin
          op2t:=top_ref;
          op2:=pointer(_op2);
       end;

     op3:=nil;
      end;

    constructor taicpu.op_const_loc(op : tasmop;_size : topsize;_op1 : longint;_op2 : tlocation);

      begin
     inherited init;
     typ:=ait_instruction;
     _operator:=op;
     op1t:=top_const;
     op3t:=top_none;
     size:=_size;
     op1:=pointer(_op1);

     if (_op2.loc=loc_register) or (_op2.loc=loc_cregister)  then
       begin
         op2t:=top_reg;
         op2:=pointer(_op2.register);
       end
     else
     if _op2.reference.isintvalue then
       begin
          op2t:=top_const;
          op2:=pointer(_op2.reference.offset);
       end
     else
       begin
          op2t:=top_ref;
          op2:=pointer(newreference(_op2.reference));
       end;

     op3:=nil;
      end;

    constructor taicpu.op_ref_reg(op : tasmop;_size : topsize;_op1 : preference;_op2 : tregister);

      begin
     inherited init;
     typ:=ait_instruction;
     _operator:=op;
     op2t:=top_reg;
     op3t:=top_none;
     size:=_size;
     op2:=pointer(_op2);

     if _op1^.isintvalue then
       begin
          op1t:=top_const;
          op1:=pointer(_op1^.offset);
          disposereference(_op1);
       end
     else
       begin
          op1t:=top_ref;
          op1:=pointer(_op1);
       end;

     op3:=nil;
      end;

    constructor taicpu.op_ref_ref(op : tasmop;_size : topsize;_op1,_op2 : preference);

      begin
     inherited init;
     typ:=ait_instruction;
     _operator:=op;
     op3t:=top_none;
     size:=_size;

     if _op1^.isintvalue then
       begin
          op1t:=top_const;
          op1:=pointer(_op1^.offset);
          disposereference(_op1);
       end
     else
       begin
          op1t:=top_ref;
          op1:=pointer(_op1);
       end;

     if _op2^.isintvalue then
       begin
          op2t:=top_const;
          op2:=pointer(_op2^.offset);
          disposereference(_op2);
       end
     else
       begin
          op2t:=top_ref;
          op2:=pointer(_op2);
       end;

     op3:=nil;
      end;

    constructor taicpu.op_csymbol(op : tasmop;_size : topsize;_op1 : pcsymbol);

      begin
     inherited init;
     typ:=ait_instruction;
     _operator:=op;
     if (op=A_JSR) and
        (use_esp_stackframe) then
      Message(cg_e_stackframe_with_esp);
     op1t:=top_symbol;
     op2t:=top_none;
     op3t:=top_none;
     size:=_size;
     op1:=pointer(_op1);

     op2:=nil;
     op3:=nil;
      end;

    constructor taicpu.op_csymbol_reg(op : tasmop;_size : topsize;_op1 : pcsymbol;_op2 : tregister);

      begin
     inherited init;
     typ:=ait_instruction;
     _operator:=op;
     op1t:=top_symbol;
     op2t:=top_reg;
      op3t:=top_none;
     size:=_size;
     op1:=pointer(_op1);
     op2:=pointer(_op2);

     op3:=nil;
      end;

    constructor taicpu.op_csymbol_ref(op : tasmop;_size : topsize;_op1 : pcsymbol;_op2 : preference);

      begin
     inherited init;
     typ:=ait_instruction;
     _operator:=op;
     op1t:=top_symbol;
     op3t:=top_none;
     size:=_size;
     op1:=pointer(_op1);

     if _op2^.isintvalue then
       begin
          op2t:=top_const;
          op2:=pointer(_op2^.offset);
          disposereference(_op2);
       end
     else
       begin
          op2t:=top_ref;
          op2:=pointer(_op2);
       end;

     op3:=nil;
      end;

    constructor taicpu.op_csymbol_loc(op : tasmop;_size : topsize;_op1 : pcsymbol;_op2 : tlocation);

      begin
     inherited init;
     typ:=ait_instruction;
     _operator:=op;
     op1t:=top_symbol;
     op3t:=top_none;
     size:=_size;
     op1:=pointer(_op1);

     if (_op2.loc=loc_register) or (_op2.loc=loc_cregister)  then
       begin
         op2t:=top_reg;
         op2:=pointer(_op2.register);
       end
     else
     if _op2.reference.isintvalue then
       begin
          op2t:=top_const;
          op2:=pointer(_op2.reference.offset);
       end
     else
       begin
          op2t:=top_ref;
          op2:=pointer(newreference(_op2.reference));
       end;

     op3:=nil;
      end;

   destructor taicpu.done;

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
    if op3t=top_symbol then
      disposecsymbol(pcsymbol(op3))
    else if op3t = top_ref then
      begin
         clear_reference(preference(op3)^);
         dispose(preference(op3));
      end;
    inherited done;
     end;


   constructor taicpu.op_ref_reglist(op: tasmop; _size : topsize; _op1: preference;_op2: tregisterlist);
   Begin
     Inherited Init;
      typ:=ait_instruction;
      _operator:=op;
      op2t:=top_reglist;
      op3t:=top_none;
      size:=_size;
     reglist := _op2;

      if _op1^.isintvalue then
        begin
           op1t:=top_const;
           op1:=pointer(_op1^.offset);
           disposereference(_op1);
        end
      else
        begin
           op1t:=top_ref;
           op1:=pointer(_op1);
        end;

      op3:=nil;
   end;


   constructor taicpu.op_reglist_ref(op: tasmop; _size : topsize; _op1: tregisterlist; _op2: preference);
   Begin
     Inherited Init;
      typ:=ait_instruction;

      _operator:=op;
      reglist:=_op1;

     op1t:=top_reglist;
      op3t:=top_none;
      size:=_size;

      if _op2^.isintvalue then
        begin
           op2t:=top_const;
           op2:=pointer(_op2^.offset);
           disposereference(_op2);
        end
      else
        begin
           op2t:=top_ref;
           op2:=pointer(_op2);
        end;
      op3:=nil;
   end;



{****************************************************************************
                              TAI_LABELED
 ****************************************************************************}

    constructor tai_labeled.init(op : tasmop; l : pasmlabel);

      begin
         inherited init;
         typ:=ait_labeled_instruction;
         _operator:=op;
         _op1:=R_NO;
         lab:=l;
         inc(lab^.refs);
      end;

    constructor tai_labeled.init_reg(op : tasmop; l : pasmlabel; reg: tregister);

      begin
         inherited init;
         typ:=ait_labeled_instruction;
         _op1:=reg;
         _operator:=op;
         lab:=l;
         inc(lab^.refs);
      end;

    destructor tai_labeled.done;

      begin
         dec(lab^.refs);
         inherited done;
      end;
{*****************************************************************************
                                  Init/Done
*****************************************************************************}

  procedure InitCpu;
    begin
    end;
    
  procedure DoneCpu;
    begin
    end;
  
end.
{
  $Log$
  Revision 1.2  2002-04-20 21:40:48  carl
  * renamed some constants

  Revision 1.1  2000/07/13 06:30:05  michael
  + Initial import

  Revision 1.2  2000/01/07 01:14:50  peter
    * updated copyright to 2000

  Revision 1.1  1999/09/16 23:05:57  florian
    * m68k compiler is again compilable (only gas writer, no assembler reader)

  Revision 1.12  1999/08/19 13:02:08  pierre
    + label faillabel added for _FAIL support

  Revision 1.11  1999/06/22 16:24:42  pierre
   * local browser stuff corrected

  Revision 1.10  1998/10/29 11:35:45  florian
    * some dll support for win32
    * fixed assembler writing for PalmOS

  Revision 1.9  1998/10/14 08:47:18  pierre
    * bugs in secondfuncret for result in subprocedures removed

  Revision 1.8  1998/10/13 16:50:15  pierre
    * undid some changes of Peter that made the compiler wrong
      for m68k (I had to reinsert some ifdefs)
    * removed several memory leaks under m68k
    * removed the meory leaks for assembler readers
    * cross compiling shoud work again better
      ( crosscompiling sysamiga works
       but as68k still complain about some code !)

  Revision 1.7  1998/08/31 12:26:27  peter
    * m68k and palmos updates from surebugfixes

  Revision 1.6  1998/08/21 14:08:44  pierre
    + TEST_FUNCRET now default (old code removed)
      works also for m68k (at least compiles)

  Revision 1.5  1998/06/04 23:51:45  peter
    * m68k compiles
    + .def file creation moved to gendef.pas so it could also be used
      for win32

  Revision 1.4  1998/05/23 01:21:10  peter
    + aktasmmode, aktoptprocessor, aktoutputformat
    + smartlink per module $SMARTLINK-/+ (like MMX) and moved to aktswitches
    + $LIBNAME to set the library name where the unit will be put in
    * splitted cgi386 a bit (codeseg to large for bp7)
    * nasm, tasm works again. nasm moved to ag386nsm.pas

  Revision 1.3  1998/05/11 13:07:54  peter
    + $ifdef NEWPPU for the new ppuformat
    + $define GDB not longer required
    * removed all warnings and stripped some log comments
    * no findfirst/findnext anymore to remove smartlink *.o files

  Revision 1.2  1998/04/29 10:33:54  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions
}
