{
    $Id$
    Copyright (c) 1999 by Florian Klaempfl and Peter Vreman

    Contains the base types for the i386

    * This code was inspired by the NASM sources
      The Netwide Assembler is copyright (C) 1996 Simon Tatham and
      Julian Hall. All rights reserved.

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
{$ifdef TP}
  {$L-,Y-}
{$endif}

uses
  globals,strings,cobjects,aasm;

const
{ Size of the instruction table converted by nasmconv.pas }
  instabentries = 1292;
  maxinfolen    = 8;

{ By default we want everything }
{$define ATTOP}
{$define ATTREG}
{$define INTELOP}
{$define ITTABLE}

{ For TP we can't use asmdebug due the table sizes }
{$ifndef TP}
  {$define ASMDEBUG}
{$endif}

{ We Don't need the intel style opcodes if we don't have a intel
  reader or generator }
{$ifndef ASMDEBUG}
{$ifdef NORA386INT}
  {$ifdef NOAG386NSM}
    {$ifdef NOAG386INT}
      {$undef INTELOP}
    {$endif}
  {$endif}
{$endif}
{$endif}

{ We Don't need the AT&T style opcodes if we don't have a AT&T
  reader or generator }
{$ifdef NORA386ATT}
  {$ifdef NOAG386ATT}
    {$undef ATTOP}
    {$ifdef NOAG386DIR}
       {$undef ATTREG}
    {$endif}
  {$endif}
{$endif}

const
{ Operand types }
  OT_NONE      = $00000000;

  OT_BITS8     = $00000001;  { size, and other attributes, of the operand  }
  OT_BITS16    = $00000002;
  OT_BITS32    = $00000004;
  OT_BITS64    = $00000008;  { FPU only  }
  OT_BITS80    = $00000010;
  OT_FAR       = $00000020;  { this means 16:16 or 16:32, like in CALL/JMP }
  OT_NEAR      = $00000040;
  OT_SHORT     = $00000080;

  OT_SIZE_MASK = $000000FF;  { all the size attributes  }
  OT_NON_SIZE  = not OT_SIZE_MASK;

  OT_SIGNED    = $00000100;  { the operand need to be signed -128-127 }

  OT_TO        = $00000200;  { operand is followed by a colon  }
                             { reverse effect in FADD, FSUB &c  }
  OT_COLON     = $00000400;

  OT_REGISTER  = $00001000;
  OT_IMMEDIATE = $00002000;
  OT_IMM8      = $00002001;
  OT_IMM16     = $00002002;
  OT_IMM32     = $00002004;
  OT_IMM64     = $00002008;
  OT_IMM80     = $00002010;
  OT_REGMEM    = $00200000;  { for r/m, ie EA, operands  }
  OT_REGNORM   = $00201000;  { 'normal' reg, qualifies as EA  }
  OT_REG8      = $00201001;
  OT_REG16     = $00201002;
  OT_REG32     = $00201004;
  OT_MMXREG    = $00201008;  { MMX registers  }
  OT_XMMREG    = $00201010;  { Katmai registers  }
  OT_MEMORY    = $00204000;  { register number in 'basereg'  }
  OT_MEM8      = $00204001;
  OT_MEM16     = $00204002;
  OT_MEM32     = $00204004;
  OT_MEM64     = $00204008;
  OT_MEM80     = $00204010;
  OT_FPUREG    = $01000000;  { floating point stack registers  }
  OT_FPU0      = $01000800;  { FPU stack register zero  }
  OT_REG_SMASK = $00070000;  { special register operands: these may be treated differently  }
                             { a mask for the following  }
  OT_REG_ACCUM = $00211000;  { accumulator: AL, AX or EAX  }
  OT_REG_AL    = $00211001;    { REG_ACCUM | BITSxx  }
  OT_REG_AX    = $00211002;    { ditto  }
  OT_REG_EAX   = $00211004;    { and again  }
  OT_REG_COUNT = $00221000;  { counter: CL, CX or ECX  }
  OT_REG_CL    = $00221001;    { REG_COUNT | BITSxx  }
  OT_REG_CX    = $00221002;    { ditto  }
  OT_REG_ECX   = $00221004;    { another one  }
  OT_REG_DX    = $00241002;

  OT_REG_SREG  = $00081002;  { any segment register  }
  OT_REG_CS    = $01081002;  { CS  }
  OT_REG_DESS  = $02081002;  { DS, ES, SS (non-CS 86 registers)  }
  OT_REG_FSGS  = $04081002;  { FS, GS (386 extended registers)  }

  OT_REG_CDT   = $00101004;  { CRn, DRn and TRn  }
  OT_REG_CREG  = $08101004;  { CRn  }
  OT_REG_CR4   = $08101404;  { CR4 (Pentium only)  }
  OT_REG_DREG  = $10101004;  { DRn  }
  OT_REG_TREG  = $20101004;  { TRn  }

  OT_MEM_OFFS  = $00604000;  { special type of EA  }
                             { simple [address] offset  }
  OT_ONENESS   = $00800000;  { special type of immediate operand  }
                             { so UNITY == IMMEDIATE | ONENESS  }
  OT_UNITY     = $00802000;  { for shift/rotate instructions  }

{Instruction flags }
  IF_SM     = $00000001;        { size match first two operands  }
  IF_SM2    = $00000002;
  IF_SB     = $00000004;  { unsized operands can't be non-byte  }
  IF_SW     = $00000008;  { unsized operands can't be non-word  }
  IF_SD     = $00000010;  { unsized operands can't be nondword  }
  IF_AR0    = $00000020;  { SB, SW, SD applies to argument 0  }
  IF_AR1    = $00000040;  { SB, SW, SD applies to argument 1  }
  IF_AR2    = $00000060;  { SB, SW, SD applies to argument 2  }
  IF_ARMASK = $00000060;  { mask for unsized argument spec  }
  IF_PRIV   = $00000100;  { it's a privileged instruction  }
  IF_SMM    = $00000200;  { it's only valid in SMM  }
  IF_PROT   = $00000400;  { it's protected mode only  }
  IF_UNDOC  = $00001000;  { it's an undocumented instruction  }
  IF_FPU    = $00002000;  { it's an FPU instruction  }
  IF_MMX    = $00004000;  { it's an MMX instruction  }
  IF_3DNOW  = $00008000;  { it's a 3DNow! instruction  }
  IF_SSE    = $00010000;  { it's a SSE (KNI, MMX2) instruction  }
  IF_PMASK  = $FF000000;  { the mask for processor types  }
  IF_PFMASK = $F001FF00;  { the mask for disassembly "prefer"  }
  IF_8086   = $00000000;  { 8086 instruction  }
  IF_186    = $01000000;  { 186+ instruction  }
  IF_286    = $02000000;  { 286+ instruction  }
  IF_386    = $03000000;  { 386+ instruction  }
  IF_486    = $04000000;  { 486+ instruction  }
  IF_PENT   = $05000000;  { Pentium instruction  }
  IF_P6     = $06000000;  { P6 instruction  }
  IF_KATMAI = $07000000;  { Katmai instructions  }
  IF_CYRIX  = $10000000;  { Cyrix-specific instruction  }
  IF_AMD    = $20000000;  { AMD-specific instruction  }
  { added flags }
  IF_PRE    = $40000000;  { it's a prefix instruction }
  IF_PASS2  = $80000000;  { if the instruction can change in a second pass }

type
  TAsmOp=(A_None,
    { prefixes }
    A_LOCK,A_REP,A_REPE,A_REPNE,A_REPNZ,A_REPZ,
    A_CS,A_ES,A_DS,A_FS,A_GS,A_SS,
    { normal }
    A_AAA, A_AAD, A_AAM, A_AAS, A_ADC, A_ADD, A_AND, A_ARPL,
    A_BOUND, A_BSF, A_BSR, A_BSWAP, A_BT, A_BTC, A_BTR, A_BTS,
    A_CALL, A_CBW, A_CDQ, A_CLC, A_CLD, A_CLI, A_CLTS, A_CMC, A_CMP,
    A_CMPSB, A_CMPSD, A_CMPSW, A_CMPXCHG, A_CMPXCHG486, A_CMPXCHG8B,
    A_CPUID, A_CWD, A_CWDE, A_DAA, A_DAS, A_DEC, A_DIV,
    A_EMMS, A_ENTER, A_EQU, A_F2XM1, A_FABS,
    A_FADD, A_FADDP, A_FBLD, A_FBSTP, A_FCHS, A_FCLEX, A_FCMOVB,
    A_FCMOVBE, A_FCMOVE, A_FCMOVNB, A_FCMOVNBE, A_FCMOVNE,
    A_FCMOVNU, A_FCMOVU, A_FCOM, A_FCOMI, A_FCOMIP, A_FCOMP,
    A_FCOMPP, A_FCOS, A_FDECSTP, A_FDISI, A_FDIV, A_FDIVP, A_FDIVR,
    A_FDIVRP, A_FEMMS,
    A_FENI, A_FFREE, A_FIADD, A_FICOM, A_FICOMP, A_FIDIV,
    A_FIDIVR, A_FILD, A_FIMUL, A_FINCSTP, A_FINIT, A_FIST, A_FISTP,
    A_FISUB, A_FISUBR, A_FLD, A_FLD1, A_FLDCW, A_FLDENV, A_FLDL2E,
    A_FLDL2T, A_FLDLG2, A_FLDLN2, A_FLDPI, A_FLDZ, A_FMUL, A_FMULP,
    A_FNCLEX, A_FNDISI, A_FNENI, A_FNINIT, A_FNOP, A_FNSAVE,
    A_FNSTCW, A_FNSTENV, A_FNSTSW, A_FPATAN, A_FPREM, A_FPREM1,
    A_FPTAN, A_FRNDINT, A_FRSTOR, A_FSAVE, A_FSCALE, A_FSETPM,
    A_FSIN, A_FSINCOS, A_FSQRT, A_FST, A_FSTCW, A_FSTENV, A_FSTP,
    A_FSTSW, A_FSUB, A_FSUBP, A_FSUBR, A_FSUBRP, A_FTST, A_FUCOM,
    A_FUCOMI, A_FUCOMIP, A_FUCOMP, A_FUCOMPP, A_FWAIT,A_FXAM, A_FXCH,
    A_FXTRACT, A_FYL2X, A_FYL2XP1, A_HLT, A_IBTS, A_ICEBP, A_IDIV,
    A_IMUL, A_IN, A_INC, A_INSB, A_INSD, A_INSW, A_INT,
    A_INT01, A_INT1, A_INT03, A_INT3, A_INTO, A_INVD, A_INVLPG, A_IRET,
    A_IRETD, A_IRETW, A_JCXZ, A_JECXZ, A_JMP, A_LAHF, A_LAR, A_LDS,
    A_LEA, A_LEAVE, A_LES, A_LFS, A_LGDT, A_LGS, A_LIDT, A_LLDT,
    A_LMSW, A_LOADALL, A_LOADALL286, A_LODSB, A_LODSD, A_LODSW,
    A_LOOP, A_LOOPE, A_LOOPNE, A_LOOPNZ, A_LOOPZ, A_LSL, A_LSS,
    A_LTR, A_MOV, A_MOVD, A_MOVQ, A_MOVSB, A_MOVSD, A_MOVSW,
    A_MOVSX, A_MOVZX, A_MUL, A_NEG, A_NOP, A_NOT, A_OR, A_OUT,
    A_OUTSB, A_OUTSD, A_OUTSW, A_PACKSSDW, A_PACKSSWB, A_PACKUSWB,
    A_PADDB, A_PADDD, A_PADDSB, A_PADDSIW, A_PADDSW, A_PADDUSB,
    A_PADDUSW, A_PADDW, A_PAND, A_PANDN, A_PAVEB,
    A_PAVGUSB, A_PCMPEQB, A_PCMPEQD, A_PCMPEQW, A_PCMPGTB, A_PCMPGTD,
    A_PCMPGTW, A_PDISTIB,
    A_PF2ID, A_PFACC, A_PFADD, A_PFCMPEQ, A_PFCMPGE, A_PFCMPGT,
    A_PFMAX, A_PFMIN, A_PFMUL, A_PFRCP, A_PFRCPIT1, A_PFRCPIT2,
    A_PFRSQIT1, A_PFRSQRT, A_PFSUB, A_PFSUBR, A_PI2FD,
    A_PMACHRIW, A_PMADDWD, A_PMAGW,  A_PMULHRIW, A_PMULHRWA,
    A_PMULHRWC, A_PMULHW, A_PMULLW, A_PMVGEZB, A_PMVLZB, A_PMVNZB,
    A_PMVZB, A_POP, A_POPA, A_POPAD, A_POPAW, A_POPF, A_POPFD,
    A_POPFW, A_POR, A_PREFETCH, A_PREFETCHW,
    A_PSLLD, A_PSLLQ, A_PSLLW, A_PSRAD, A_PSRAW,
    A_PSRLD, A_PSRLQ, A_PSRLW, A_PSUBB, A_PSUBD, A_PSUBSB,
    A_PSUBSIW, A_PSUBSW, A_PSUBUSB, A_PSUBUSW, A_PSUBW, A_PUNPCKHBW,
    A_PUNPCKHDQ, A_PUNPCKHWD, A_PUNPCKLBW, A_PUNPCKLDQ, A_PUNPCKLWD,
    A_PUSH, A_PUSHA, A_PUSHAD, A_PUSHAW, A_PUSHF, A_PUSHFD,
    A_PUSHFW, A_PXOR, A_RCL, A_RCR, A_RDSHR, A_RDMSR, A_RDPMC, A_RDTSC,
    A_RESB, A_RET, A_RETF, A_RETN,
    A_ROL, A_ROR, A_RSDC, A_RSLDT, A_RSM, A_SAHF, A_SAL, A_SALC, A_SAR, A_SBB,
    A_SCASB, A_SCASD, A_SCASW, A_SGDT, A_SHL, A_SHLD, A_SHR, A_SHRD,
    A_SIDT, A_SLDT, A_SMI, A_SMINT, A_SMINTOLD, A_SMSW, A_STC, A_STD, A_STI, A_STOSB,
    A_STOSD, A_STOSW, A_STR, A_SUB, A_SVDC, A_SVLDT, A_SVTS, A_SYSCALL, A_SYSENTER,
    A_SYSEXIT, A_SYSRET, A_TEST, A_UD1, A_UD2, A_UMOV, A_VERR, A_VERW,
    A_WAIT, A_WBINVD, A_WRSHR, A_WRMSR, A_XADD, A_XBTS, A_XCHG, A_XLAT, A_XLATB,
    A_XOR, A_CMOVcc, A_Jcc, A_SETcc,
    A_ADDPS, A_ADDSS, A_ANDNPS, A_ANDPS, A_CMPEQPS, A_CMPEQSS, A_CMPLEPS,
    A_CMPLESS, A_CMPLTPS, A_CMPLTSS, A_CMPNEQPS, A_CMPNEQSS, A_CMPNLEPS,
    A_CMPNLESS, A_CMPNLTPS, A_CMPNLTSS, A_CMPORDPS, A_CMPORDSS, A_CMPUNORDPS, A_CMPUNORDSS,
    A_CMPPS, A_CMPSS, A_COMISS, A_CVTPI2PS, A_CVTPS2PI, A_CVTSI2SS, A_CVTSS2SI,
    A_CVTTPS2PI, A_CVTTSS2SI, A_DIVPS, A_DIVSS, A_LDMXCSR, A_MAXPS, A_MAXSS, A_MINPS,
    A_MINSS, A_MOVAPS, A_MOVHPS, A_MOVLHPS, A_MOVLPS, A_MOVHLPS, A_MOVMSKPS,
    A_MOVNTPS, A_MOVSS, A_MOVUPS, A_MULPS, A_MULSS, A_ORPS, A_RCPPS, A_RCPSS,
    A_RSQRTPS, A_RSQRTSS, A_SHUFPS, A_SQRTPS, A_SQRTSS, A_STMXCSR, A_SUBPS, A_SUBSS,
    A_UCOMISS, A_UNPCKHPS, A_UNPCKLPS, A_XORPS, A_FXRSTOR, A_FXSAVE, A_PREFETCHNTA,
    A_PREFETCHT0, A_PREFETCHT1,A_PREFETCHT2,
    A_SFENCE, A_MASKMOVQ, A_MOVNTQ, A_PAVGB, A_PAVGW, A_PEXTRW, A_PINSRW, A_PMAXSW,
    A_PMAXUB, A_PMINSW, A_PMINUB, A_PMOVMSKB, A_PMULHUW, A_PSADBW, A_PSHUFW
  );

  op2strtable=array[tasmop] of string[10];

const
  firstop = low(tasmop);
  lastop  = high(tasmop);

  AsmPrefixes = 6;
  AsmPrefix : array[0..AsmPrefixes-1] of TasmOP =(
    A_LOCK,A_REP,A_REPE,A_REPNE,A_REPNZ,A_REPZ
  );

  AsmOverrides = 6;
  AsmOverride : array[0..AsmOverrides-1] of TasmOP =(
    A_CS,A_ES,A_DS,A_FS,A_GS,A_SS
  );


{$ifdef INTELOP}
  int_op2str:op2strtable=('<none>',
    { prefixes }
    'lock','rep','repe','repne','repnz','repz',
    'segcs','seges','segds','segfs','seggs','segss',
    { normal }
    'aaa','aad','aam','aas','adc','add','and','arpl',
    'bound','bsf','bsr','bswap','bt','btc','btr','bts',
    'call','cbw','cdq','clc','cld','cli','clts','cmc','cmp',
    'cmpsb','cmpsd','cmpsw','cmpxchg','cmpxchg486','cmpxchg8b',
    'cpuid','cwd','cwde','daa','das','dec','div','emms',
    'enter','equ','f2xm1','fabs',
    'fadd','faddp','fbld','fbstp','fchs','fclex','fcmovb',
    'fcmovbe','fcmove','fcmovnb','fcmovnbe','fcmovne',
    'fcmovnu','fcmovu','fcom','fcomi','fcomip','fcomp',
    'fcompp','fcos','fdecstp','fdisi','fdiv','fdivp','fdivr',
    'fdivrp',
    'femms',
    'feni','ffree','fiadd','ficom','ficomp','fidiv',
    'fidivr','fild','fimul','fincstp','finit','fist','fistp',
    'fisub','fisubr','fld','fld1','fldcw','fldenv','fldl2e',
    'fldl2t','fldlg2','fldln2','fldpi','fldz','fmul','fmulp',
    'fnclex','fndisi','fneni','fninit','fnop','fnsave',
    'fnstcw','fnstenv','fnstsw','fpatan','fprem','fprem1',
    'fptan','frndint','frstor','fsave','fscale','fsetpm',
    'fsin','fsincos','fsqrt','fst','fstcw','fstenv','fstp',
    'fstsw','fsub','fsubp','fsubr','fsubrp','ftst','fucom',
    'fucomi','fucomip','fucomp','fucompp','fwait','fxam','fxch',
    'fxtract','fyl2x','fyl2xp1','hlt','ibts','icebp','idiv',
    'imul','in','inc','insb','insd','insw','int',
    'int01','int1','int03','int3','into','invd','invlpg','iret',
    'iretd','iretw','jcxz','jecxz','jmp','lahf','lar','lds',
    'lea','leave','les','lfs','lgdt','lgs','lidt','lldt',
    'lmsw','loadall','loadall286','lodsb','lodsd','lodsw',
    'loop','loope','loopne','loopnz','loopz','lsl','lss',
    'ltr','mov','movd','movq','movsb','movsd','movsw',
    'movsx','movzx','mul','neg','nop','not','or','out',
    'outsb','outsd','outsw','packssdw','packsswb','packuswb',
    'paddb','paddd','paddsb','paddsiw','paddsw','paddusb',
    'paddusw','paddw','pand','pandn','paveb',
    'pavgusb','pcmpeqb',
    'pcmpeqd','pcmpeqw','pcmpgtb','pcmpgtd','pcmpgtw',
    'pdistib',
    'pf2id','pfacc','pfadd','pfcmpeq','pfcmpge','pfcmpgt',
    'pfmax','pfmin','pfmul','pfrcp','pfrcpit1','pfrcpit2',
    'pfrsqit1','pfrsqrt','pfsub','pfsubr','pi2fd',
    'pmachriw','pmaddwd','pmagw','pmulhriw','pmulhrwa','pmulhrwc',
    'pmulhw','pmullw','pmvgezb','pmvlzb','pmvnzb',
    'pmvzb','pop','popa','popad','popaw','popf','popfd',
    'popfw','por',
    'prefetch','prefetchw','pslld','psllq','psllw','psrad','psraw',
    'psrld','psrlq','psrlw','psubb','psubd','psubsb',
    'psubsiw','psubsw','psubusb','psubusw','psubw','punpckhbw',
    'punpckhdq','punpckhwd','punpcklbw','punpckldq','punpcklwd',
    'push','pusha','pushad','pushaw','pushf','pushfd',
    'pushfw','pxor','rcl','rcr','rdshr','rdmsr','rdpmc','rdtsc',
    'resb','ret','retf','retn',
    'rol','ror','rsdc','rsldt','rsm','sahf','sal','salc','sar','sbb',
    'scasb','scasd','scasw','sgdt','shl','shld','shr','shrd',
    'sidt','sldt','smi','smint','smintold','smsw','stc','std','sti','stosb',
    'stosd','stosw','str','sub','svdc','svldt','svts','syscall','sysenter',
    'sysexit','sysret','test','ud1','ud2','umov','verr','verw',
    'wait','wbinvd','wrshr','wrmsr','xadd','xbts','xchg','xlat','xlatb',
    'xor','cmov','j','set',
    'addps','addss','andnps','andps','cmpeqps','cmpeqss','cmpleps','cmpless','cmpltps',
    'cmpltss','cmpneqps','cmpneqss','cmpnleps','cmpnless','cmpnltps','cmpnltss',
    'cmpordps','cmpordss','cmpunordps','cmpunordss','cmpps','cmpss','comiss','cvtpi2ps','cvtps2pi',
    'cvtsi2ss','cvtss2si','cvttps2pi','cvttss2si','divps','divss','ldmxcsr','maxps',
    'maxss','minps','minss','movaps','movhps','movlhps','movlps','movhlps','movmskps',
    'movntps','movss','movups','mulps','mulss','orps','rcpps','rcpss','rsqrtps','rsqrtss',
    'shufps','sqrtps','sqrtss','stmxcsr','subps','subss','ucomiss','unpckhps','unpcklps',
    'xorps','fxrstor','fxsave','prefetchnta','prefetcht0','prefetcht1','prefetcht2',
    'sfence','maskmovq','movntq','pavgb','pavgw','pextrw','pinsrw','pmaxsw','pmaxub',
    'pminsw','pminub','pmovmskb','pmulhuw','psadbw','pshufw'
  );
{$endif INTELOP}

{$ifdef ATTOP}
  att_op2str:op2strtable=('<none>',
    { prefixes }
    'lock','rep','repe','repne','repnz','repz',
    'cs','es','ds','fs','gs','ss',
    { normal }
    'aaa','aad','aam','aas','adc','add','and','arpl',
    'bound','bsf','bsr','bswap','bt','btc','btr','bts',
    'call','cbtw','cltd','clc','cld','cli','clts','cmc','cmp',
    'cmpsb','cmpsl','cmpsw','cmpxchg','cmpxchg486','cmpxchg8b',
    'cpuid','cwtd','cwtl','daa','das','dec','div',
    'emms','enter','equ','f2xm1','fabs',
    'fadd','faddp','fbld','fbstp','fchs','fclex','fcmovb',
    'fcmovbe','fcmove','fcmovnb','fcmovnbe','fcmovne',
    'fcmovnu','fcmovu','fcom','fcomi','fcomip','fcomp',
    'fcompp','fcos','fdecstp','fdisi','fdiv','fdivp','fdivr',
    'fdivrp','femms',
    'feni','ffree','fiadd','ficom','ficomp','fidiv',
    'fidivr','fild','fimul','fincstp','finit','fist','fistp',
    'fisub','fisubr','fld','fld1','fldcw','fldenv','fldl2e',
    'fldl2t','fldlg2','fldln2','fldpi','fldz','fmul','fmulp',
    'fnclex','fndisi','fneni','fninit','fnop','fnsave',
    'fnstcw','fnstenv','fnstsw','fpatan','fprem','fprem1',
    'fptan','frndint','frstor','fsave','fscale','fsetpm',
    'fsin','fsincos','fsqrt','fst','fstcw','fstenv','fstp',
    'fstsw','fsub','fsubp','fsubr','fsubrp','ftst','fucom',
    'fucomi','fucomip','fucomp','fucompp','fwait','fxam','fxch',
    'fxtract','fyl2x','fyl2xp1','hlt','ibts','icebp','idiv',
    'imul','in','inc','insb','insl','insw','int',
    'int01','int1','int03','int3','into','invd','invlpg','iret',
    'iretd','iretw','jcxz','jecxz','jmp','lahf','lar','lds',
    'lea','leave','les','lfs','lgdt','lgs','lidt','lldt',
    'lmsw','loadall','loadall286','lodsb','lodsl','lodsw',
    'loop','loope','loopne','loopnz','loopz','lsl','lss',
    'ltr','mov','movd','movq','movsb','movsl','movsw',
    'movs','movz','mul','neg','nop','not','or','out',
    'outsb','outsl','outsw','packssd','packssw','packusw',
    'paddb','paddd','paddsb','paddsiw','paddsw','paddusb',
    'paddusw','paddw','pand','pandn','paveb',
    'pavgusb','pcmpeqb',
    'pcmpeqd','pcmpeqw','pcmpgtb','pcmpgtd','pcmpgtw',
    'pdistib',
    'pf2id','pfacc','pfadd','pfcmpeq','pfcmpge','pfcmpgt',
    'pfmax','pfmin','pfmul','pfrcp','pfrcpit1','pfrcpit2',
    'pfrsqit1','pfrsqrt','pfsub','pfsubr','pi2fd',
    'pmachriw','pmaddwd','pmagw','pmulhriw','pmulhrwa','pmulhrwc',
    'pmulhw','pmullw','pmvgezb','pmvlzb','pmvnzb',
    'pmvzb','pop','popa','popal','popaw','popf','popfl',
    'popfw','por',
    'prefetch','prefetchw','pslld','psllq','psllw','psrad','psraw',
    'psrld','psrlq','psrlw','psubb','psubd','psubsb',
    'psubsiw','psubsw','psubusb','psubusw','psubw','punpckhbw',
    'punpckhdq','punpckhwd','punpcklbw','punpckldq','punpcklwd',
    'push','pusha','pushal','pushaw','pushf','pushfl',
    'pushfw','pxor','rcl','rcr','rdshr','rdmsr','rdpmc','rdtsc',
    'resb','ret','retf','retn',
    'rol','ror','rsdc','rsldt','rsm','sahf','sal','salc','sar','sbb',
    'scasb','scasl','scasw','sgdt','shl','shld','shr','shrd',
    'sidt','sldt','smi','smint','smintold','smsw','stc','std','sti','stosb',
    'stosl','stosw','str','sub','svdc','svldt','svts','syscall','sysenter',
    'sysexit','sysret','test','ud1','ud2','umov','verr','verw',
    'wait','wbinvd','wrshr','wrmsr','xadd','xbts','xchg','xlat','xlatb',
    'xor','cmov','j','set',
    'addps','addss','andnps','andps','cmpeqps','cmpeqss','cmpleps','cmpless','cmpltps',
    'cmpltss','cmpneqps','cmpneqss','cmpnleps','cmpnless','cmpnltps','cmpnltss',
    'cmpordps','cmpordss','cmpunordps','cmpunordss','cmpps','cmpss','comiss','cvtpi2ps','cvtps2pi',
    'cvtsi2ss','cvtss2si','cvttps2pi','cvttss2si','divps','divss','ldmxcsr','maxps',
    'maxss','minps','minss','movaps','movhps','movlhps','movlps','movhlps','movmskps',
    'movntps','movss','movups','mulps','mulss','orps','rcpps','rcpss','rsqrtps','rsqrtss',
    'shufps','sqrtps','sqrtss','stmxcsr','subps','subss','ucomiss','unpckhps','unpcklps',
    'xorps','fxrstor','fxsave','prefetchnta','prefetcht0','prefetcht1','prefetcht2',
    'sfence','maskmovq','movntq','pavgb','pavgw','pextrw','pinsrw','pmaxsw','pmaxub',
    'pminsw','pminub','pmovmskb','pmulhuw','psadbw','pshufw'
  );

  att_nosuffix:array[tasmop] of boolean=(
    { 0 }
    false,false,false,false,false,false,false,false,false,false,
    false,false,false,false,false,false,false,false,false,false,
    false,false,false,false,false,false,false,false,false,false,
    false,false,false,false,false,false,false,false,false,false,
    false,false,false,false,false,false,false,false,false,false,
    false,false,false,false,false,false,false,false,false,false,
    false,false,false,false,false,false,false,false,false,false,
    false,false,false,false,false,false,false,false,false,false,
    false,false,false,false,false,false,false,false,false,false,
    false,false,false,false,false,false,false,false,false,false,
    { 100 }
    false,false,false,false,false,false,false,false,false,false,
    false,false,false,false,false,false,false,false,false,false,
    false,false,false,false,false,false,false,false,false,false,
    false,false,false,false,false,false,false,false,false,false,
    false,false,false,false,false,false,false,false,false,false,
    false,false,false,false,false,false,false,false,false,false,
    false,false,false,false,false,false,false,false,false,false,
    false,false,false,false,false,false,false,false,false,false,
    false,false,false,false,false,false,false,false,false,false,
    false,false,false,false,false,false,false,false,false,false,
    { 200 }
    false,false,true,true,true,true,true,false,false,false,
    false,false,false,false,false,false,false,false,true,true,
    true,true,true,true,true,true,true,true,true,true,
    true,true,true,true,true,true,true,true,true,true,
    true,true,true,true,true,true,true,true,true,true,
    true,true,true,true,true,true,true,true,true,true,
    true,true,true,true,true,true,true,true,false,false,
    false,false,false,false,false,false,true,true,true,true,
    true,true,true,true,true,true,true,true,true,true,
    true,true,true,true,true,true,true,true,true,
    { 300 }
    true,false,false,true,true,false,true,true,true,false,false,
    false,false,false,false,false,false,false,false,false,false,
    false,false,false,false,false,false,false,false,false,false,
    false,false,false,false,false,false,false,false,false,false,
    false,false,false,false,false,false,false,false,false,false,
    false,false,false,false,false,false,false,false,false,false,
    false,false,false,false,false,false,false,false,false,false,
    false,false,false,false,false,false,false,true,true,true,
    true,true,true,true,true,true,true,true,true,true,
    true,true,true,true,true,true,true,true,true,true,
    { 400 }
    true,true,true,true,true,true,true,true,true,true,
    true,true,true,true,true,true,true,true,true,true,
    true,true,true,true,true,true,true,true,true,true,
    true,true,true,true,true,true,true,true,true,true,
    true,true,true,true,true,true,true,true,true,true,
    true,true,true,true,true,true,true,true,true,true
  );

{$endif ATTOP}


{*****************************************************************************
                                Operand Sizes
*****************************************************************************}

type
  topsize = (S_NO,
    S_B,S_W,S_L,S_BW,S_BL,S_WL,
    S_IS,S_IL,S_IQ,
    S_FS,S_FL,S_FX,S_D,S_Q,S_FV
  );

const
  { Intel style operands ! }
  opsize_2_type:array[0..2,topsize] of longint=(
    (OT_NONE,
     OT_BITS8,OT_BITS16,OT_BITS32,OT_BITS16,OT_BITS32,OT_BITS32,
     OT_BITS16,OT_BITS32,OT_BITS64,
     OT_BITS32,OT_BITS64,OT_BITS80,OT_BITS64,OT_BITS64,OT_BITS64
    ),
    (OT_NONE,
     OT_BITS8,OT_BITS16,OT_BITS32,OT_BITS8,OT_BITS8,OT_BITS16,
     OT_BITS16,OT_BITS32,OT_BITS64,
     OT_BITS32,OT_BITS64,OT_BITS80,OT_BITS64,OT_BITS64,OT_BITS64
    ),
    (OT_NONE,
     OT_BITS8,OT_BITS16,OT_BITS32,OT_NONE,OT_NONE,OT_NONE,
     OT_BITS16,OT_BITS32,OT_BITS64,
     OT_BITS32,OT_BITS64,OT_BITS80,OT_BITS64,OT_BITS64,OT_BITS64
    )
  );

{$ifdef ATTOP}
  att_opsize2str : array[topsize] of string[2] = ('',
    'b','w','l','bw','bl','wl',
    's','l','q',
    's','l','t','d','q','v'
  );
{$endif}


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

const
  CondAsmOps=3;
  CondAsmOp:array[0..CondAsmOps-1] of TasmOp=(
    A_CMOVcc, A_Jcc, A_SETcc
  );
  CondAsmOpStr:array[0..CondAsmOps-1] of string[4]=(
    'CMOV','J','SET'
  );


{*****************************************************************************
                                  Registers
*****************************************************************************}

type
  { enumeration for registers, don't change the order }
  { it's used by the register size conversions        }
  tregister = (R_NO,
    R_EAX,R_ECX,R_EDX,R_EBX,R_ESP,R_EBP,R_ESI,R_EDI,
    R_AX,R_CX,R_DX,R_BX,R_SP,R_BP,R_SI,R_DI,
    R_AL,R_CL,R_DL,R_BL,R_AH,R_CH,R_BH,R_DH,
    R_CS,R_DS,R_ES,R_SS,R_FS,R_GS,
    R_ST,R_ST0,R_ST1,R_ST2,R_ST3,R_ST4,R_ST5,R_ST6,R_ST7,
    R_DR0,R_DR1,R_DR2,R_DR3,R_DR6,R_DR7,
    R_CR0,R_CR2,R_CR3,R_CR4,
    R_TR3,R_TR4,R_TR5,R_TR6,R_TR7,
    R_MM0,R_MM1,R_MM2,R_MM3,R_MM4,R_MM5,R_MM6,R_MM7,
    R_XMM0,R_XMM1,R_XMM2,R_XMM3,R_XMM4,R_XMM5,R_XMM6,R_XMM7
  );

  tregisterset = set of tregister;

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
  reg_2_opsize:array[firstreg..lastreg] of topsize = (S_NO,
    S_L,S_L,S_L,S_L,S_L,S_L,S_L,S_L,
    S_W,S_W,S_W,S_W,S_W,S_W,S_W,S_W,
    S_B,S_B,S_B,S_B,S_B,S_B,S_B,S_B,
    S_W,S_W,S_W,S_W,S_W,S_W,
    S_FL,S_FL,S_FL,S_FL,S_FL,S_FL,S_FL,S_FL,S_FL,
    S_L,S_L,S_L,S_L,S_L,S_L,
    S_L,S_L,S_L,S_L,
    S_L,S_L,S_L,S_L,S_L,
    S_D,S_D,S_D,S_D,S_D,S_D,S_D,S_D,
    S_D,S_D,S_D,S_D,S_D,S_D,S_D,S_D
  );

  { Convert reg to operand type }
  reg_2_type:array[firstreg..lastreg] of longint = (OT_NONE,
    OT_REG_EAX,OT_REG_ECX,OT_REG32,OT_REG32,OT_REG32,OT_REG32,OT_REG32,OT_REG32,
    OT_REG_AX,OT_REG_CX,OT_REG_DX,OT_REG16,OT_REG16,OT_REG16,OT_REG16,OT_REG16,
    OT_REG_AL,OT_REG_CL,OT_REG8,OT_REG8,OT_REG8,OT_REG8,OT_REG8,OT_REG8,
    OT_REG_CS,OT_REG_DESS,OT_REG_DESS,OT_REG_DESS,OT_REG_FSGS,OT_REG_FSGS,
    OT_FPU0,OT_FPU0,OT_FPUREG,OT_FPUREG,OT_FPUREG,OT_FPUREG,OT_FPUREG,OT_FPUREG,OT_FPUREG,
    OT_REG_DREG,OT_REG_DREG,OT_REG_DREG,OT_REG_DREG,OT_REG_DREG,OT_REG_DREG,
    OT_REG_CREG,OT_REG_CREG,OT_REG_CREG,OT_REG_CR4,
    OT_REG_TREG,OT_REG_TREG,OT_REG_TREG,OT_REG_TREG,OT_REG_TREG,
    OT_MMXREG,OT_MMXREG,OT_MMXREG,OT_MMXREG,OT_MMXREG,OT_MMXREG,OT_MMXREG,OT_MMXREG,
    OT_XMMREG,OT_XMMREG,OT_XMMREG,OT_XMMREG,OT_XMMREG,OT_XMMREG,OT_XMMREG,OT_XMMREG
  );

{$ifdef INTELOP}
  int_reg2str : reg2strtable = ('',
    'eax','ecx','edx','ebx','esp','ebp','esi','edi',
    'ax','cx','dx','bx','sp','bp','si','di',
    'al','cl','dl','bl','ah','ch','bh','dh',
    'cs','ds','es','ss','fs','gs',
    'st','st(0)','st(1)','st(2)','st(3)','st(4)','st(5)','st(6)','st(7)',
    'dr0','dr1','dr2','dr3','dr6','dr7',
    'cr0','cr2','cr3','cr4',
    'tr3','tr4','tr5','tr6','tr7',
    'mm0','mm1','mm2','mm3','mm4','mm5','mm6','mm7',
    'xmm0','xmm1','xmm2','xmm3','xmm4','xmm5','xmm6','xmm7'
  );

  int_nasmreg2str : reg2strtable = ('',
    'eax','ecx','edx','ebx','esp','ebp','esi','edi',
    'ax','cx','dx','bx','sp','bp','si','di',
    'al','cl','dl','bl','ah','ch','bh','dh',
    'cs','ds','es','ss','fs','gs',
    'st0','st0','st1','st2','st3','st4','st5','st6','st7',
    'dr0','dr1','dr2','dr3','dr6','dr7',
    'cr0','cr2','cr3','cr4',
    'tr3','tr4','tr5','tr6','tr7',
    'mm0','mm1','mm2','mm3','mm4','mm5','mm6','mm7',
    'xmm0','xmm1','xmm2','xmm3','xmm4','xmm5','xmm6','xmm7'
  );
{$endif}

{$ifdef ATTREG}
  att_reg2str : reg2strtable = ('',
    '%eax','%ecx','%edx','%ebx','%esp','%ebp','%esi','%edi',
    '%ax','%cx','%dx','%bx','%sp','%bp','%si','%di',
    '%al','%cl','%dl','%bl','%ah','%ch','%bh','%dh',
    '%cs','%ds','%es','%ss','%fs','%gs',
    '%st','%st(0)','%st(1)','%st(2)','%st(3)','%st(4)','%st(5)','%st(6)','%st(7)',
    '%dr0','%dr1','%dr2','%dr3','%dr6','%dr7',
    '%cr0','%cr2','%cr3','%cr4',
    '%tr3','%tr4','%tr5','%tr6','%tr7',
    '%mm0','%mm1','%mm2','%mm3','%mm4','%mm5','%mm6','%mm7',
    '%xmm0','%xmm1','%xmm2','%xmm3','%xmm4','%xmm5','%xmm6','%xmm7'
  );
{$endif ATTREG}


{*****************************************************************************
                                   Flags
*****************************************************************************}

type
  TResFlags = (F_E,F_NE,F_G,F_L,F_GE,F_LE,F_C,F_NC,F_A,F_AE,F_B,F_BE);

const
  { arrays for boolean location conversions }
  flag_2_cond : array[TResFlags] of TAsmCond =
     (C_E,C_NE,C_G,C_L,C_GE,C_LE,C_C,C_NC,C_A,C_AE,C_B,C_BE);


{*****************************************************************************
                                Reference
*****************************************************************************}

type
  trefoptions=(ref_none,ref_parafixup,ref_localfixup);

  { immediate/reference record }
  preference = ^treference;
  treference = packed record
     is_immediate : boolean; { is this used as reference or immediate }
     segment,
     base,
     index       : tregister;
     scalefactor : byte;
     offset      : longint;
     symbol      : pasmsymbol;
     offsetfixup : longint;
     options     : trefoptions;
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
           top_symbol : (sym:pasmsymbol;symofs:longint);
        end;

{*****************************************************************************
                               Generic Location
*****************************************************************************}

type
  TLoc=(
    LOC_INVALID,     { added for tracking problems}
    LOC_FPU,         { FPU stack }
    LOC_REGISTER,    { in a processor register }
    LOC_MEM,         { in memory }
    LOC_REFERENCE,   { like LOC_MEM, but lvalue }
    LOC_JUMP,        { boolean results only, jump to false or true label }
    LOC_FLAGS,       { boolean results only, flags are set }
    LOC_CREGISTER,   { Constant register which shouldn't be modified }
    LOC_MMXREGISTER, { MMX register }
    LOC_CMMXREGISTER,{ Constant MMX register }
    LOC_CFPUREGISTER { if it is a FPU register variable on the fpu stack }
  );

  plocation = ^tlocation;
  tlocation = packed record
     case loc : tloc of
        LOC_MEM,LOC_REFERENCE : (reference : treference);
        LOC_FPU : ();
        LOC_JUMP : ();
        LOC_FLAGS : (resflags : tresflags);
        LOC_INVALID : ();

        { it's only for better handling }
        LOC_MMXREGISTER : (mmxreg : tregister);
        { segment in reference at the same place as in loc_register }
        LOC_REGISTER,LOC_CREGISTER : (
        case longint of
          1 : (register,segment,registerhigh : tregister);
          { overlay a registerlow }
          2 : (registerlow : tregister);
        );
  end;

{*****************************************************************************
                                 Constants
*****************************************************************************}

const
  general_registers = [R_EAX,R_EBX,R_ECX,R_EDX];

  intregs = general_registers;
  fpuregs = [];
  mmregs = [R_MM0..R_MM7];

  registers_saved_on_cdecl = [R_ESI,R_EDI,R_EBX];

  { generic register names }
  stack_pointer = R_ESP;
  frame_pointer = R_EBP;
  self_pointer  = R_ESI;
  accumulator   = R_EAX;
  scratch_register = R_EDI;

  cpuflags = [];

  { sizes }
  pointersize   = 4;
  extended_size = 10;
  sizepostfix_pointer = S_L;


{*****************************************************************************
                              Instruction table
*****************************************************************************}

{$ifndef NOAG386BIN}
type
  tinsentry=packed record
    opcode  : tasmop;
    ops     : byte;
    optypes : array[0..2] of longint;
    code    : array[0..maxinfolen] of char;
    flags   : longint;
  end;
  pinsentry=^tinsentry;

  TInsTabCache=array[TasmOp] of longint;
  PInsTabCache=^TInsTabCache;

const
  InsTab:array[0..instabentries-1] of TInsEntry=
{$i i386tab.inc}

var
  InsTabCache : PInsTabCache;
{$endif NOAG386BIN}


{*****************************************************************************
                                  Helpers
*****************************************************************************}

    const
       maxvarregs = 4;
       varregs : array[1..maxvarregs] of tregister =
         (R_EBX,R_EDX,R_ECX,R_EAX);

       maxfpuvarregs = 8;

    function imm_2_type(l:longint):longint;

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

    { these procedures must be defined by all target cpus }
    function regtoreg8(reg : tregister) : tregister;
    function regtoreg16(reg : tregister) : tregister;
    function regtoreg32(reg : tregister) : tregister;

    { can be ignored on 32 bit systems }
    function regtoreg64(reg : tregister) : tregister;

    { returns the operand prefix for a given register }
    function regsize(reg : tregister) : topsize;

    { resets all values of ref to defaults }
    procedure reset_reference(var ref : treference);
    { set mostly used values of a new reference }
    function new_reference(base : tregister;offset : longint) : preference;

    function newreference(const r : treference) : preference;
    procedure disposereference(var r : preference);

    function reg2str(r : tregister) : string;

    function is_calljmp(o:tasmop):boolean;


implementation

{$ifdef heaptrc}
  uses
      ppheap;
{$endif heaptrc}

{*****************************************************************************
                                  Helpers
*****************************************************************************}

    function imm_2_type(l:longint):longint;
      begin
        if (l>=-128) and (l<=127) then
         imm_2_type:=OT_IMM8 or OT_SIGNED
        else
         if (l>=-255) and (l<=255) then
          imm_2_type:=OT_IMM8
        else
         if (l>=-32768) and (l<=32767) then
          imm_2_type:=OT_IMM16 or OT_SIGNED
        else
         if (l>=-65536) and (l<=65535) then
          imm_2_type:=OT_IMM16 or OT_SIGNED
         else
          imm_2_type:=OT_IMM32;
      end;

    function reg2str(r : tregister) : string;
      const
         a : array[R_NO..R_BL] of string[3] =
          ('','EAX','ECX','EDX','EBX','ESP','EBP','ESI','EDI',
           'AX','CX','DX','BX','SP','BP','SI','DI',
           'AL','CL','DL','BL');
      begin
         if r in [R_ST0..R_ST7] then
           reg2str:='ST('+tostr(longint(r)-longint(R_ST0))+')'
         else
           reg2str:=a[r];
      end;


    function is_calljmp(o:tasmop):boolean;
      begin
        case o of
          A_CALL,
          A_JCXZ,
          A_JECXZ,
          A_JMP,
          A_LOOP,
          A_Jcc :
            is_calljmp:=true;
          else
            is_calljmp:=false;
        end;
      end;


    procedure disposereference(var r : preference);
      begin
         dispose(r);
         r:=nil;
      end;


    function newreference(const r : treference) : preference;
      var
         p : preference;
      begin
         new(p);
         p^:=r;
         newreference:=p;
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

    function regtoreg8(reg : tregister) : tregister;

     begin
        regtoreg8:=reg32toreg8(reg);
     end;

    function regtoreg16(reg : tregister) : tregister;

     begin
        regtoreg16:=reg32toreg16(reg);
     end;

    function regtoreg32(reg : tregister) : tregister;

     begin
        regtoreg32:=reg;
     end;

    function regtoreg64(reg : tregister) : tregister;

     begin
        { to avoid warning }
        regtoreg64:=R_NO;
     end;

function regsize(reg : tregister) : topsize;
begin
   if reg in regset8bit then
     regsize:=S_B
   else if reg in regset16bit then
     regsize:=S_W
   else if reg in regset32bit then
     regsize:=S_L;
end;


procedure reset_reference(var ref : treference);
begin
  FillChar(ref,sizeof(treference),0);
end;


function new_reference(base : tregister;offset : longint) : preference;
var
  r : preference;
begin
  new(r);
  FillChar(r^,sizeof(treference),0);
  r^.base:=base;
  r^.offset:=offset;
  new_reference:=r;
end;


{*****************************************************************************
                              Instruction table
*****************************************************************************}

var
  saveexit : pointer;

procedure FreeInsTabCache;{$ifndef FPC}far;{$endif}
begin
  exitproc:=saveexit;
{$ifndef NOAG386BIN}
  dispose(instabcache);
{$endif NOAG386BIN}
end;


procedure BuildInsTabCache;
{$ifndef NOAG386BIN}
var
  i : longint;
{$endif}
begin
{$ifndef NOAG386BIN}
  new(instabcache);
  FillChar(instabcache^,sizeof(tinstabcache),$ff);
  i:=0;
  while (i<InsTabEntries) do
   begin
     if InsTabCache^[InsTab[i].OPcode]=-1 then
      InsTabCache^[InsTab[i].OPcode]:=i;
     inc(i);
   end;
{$endif NOAG386BIN}
  saveexit:=exitproc;
  exitproc:=@FreeInsTabCache;
end;


begin
  BuildInsTabCache;
end.
{
  $Log$
  Revision 1.6  1999-08-13 15:36:30  peter
    * fixed suffix writing for a_setcc

  Revision 1.5  1999/08/12 14:36:02  peter
    + KNI instructions

  Revision 1.4  1999/08/07 14:20:58  florian
    * some small problems fixed

  Revision 1.3  1999/08/05 14:58:09  florian
    * some fixes for the floating point registers
    * more things for the new code generator

  Revision 1.2  1999/08/04 13:45:25  florian
    + floating point register variables !!
    * pairegalloc is now generated for register variables

  Revision 1.1  1999/08/04 00:22:58  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.10  1999/08/02 21:28:58  florian
    * the main branch psub.pas is now used for
      newcg compiler

  Revision 1.9  1999/08/02 21:01:45  michael
  * Moved toperand type back =(

  Revision 1.8  1999/08/02 20:45:49  michael
  * Moved toperand type to aasm

  Revision 1.7  1999/08/02 17:17:09  florian
    * small changes for the new code generator

  Revision 1.6  1999/06/06 15:53:15  peter
    * suffix adding can be turned of for some tasmops in att_nosuffix array

  Revision 1.5  1999/05/27 19:44:34  peter
    * removed oldasm
    * plabel -> pasmlabel
    * -a switches to source writing automaticly
    * assembler readers OOPed
    * asmsymbol automaticly external
    * jumptables and other label fixes for asm readers

  Revision 1.4  1999/05/17 14:33:50  pierre
   uses heaptrc need for extrainfo with heaptrc

  Revision 1.3  1999/05/12 00:19:51  peter
    * removed R_DEFAULT_SEG
    * uniform float names

  Revision 1.2  1999/05/11 16:30:00  peter
    * more noag386bin defines, so tp7 can compile at least

  Revision 1.1  1999/05/01 13:24:23  peter
    * merged nasm compiler
    * old asm moved to oldasm/

  Revision 1.13  1999/04/14 09:07:43  peter
    * asm reader improvements

  Revision 1.12  1999/04/10 16:14:09  peter
    * fixed optimizer

  Revision 1.11  1999/03/31 13:55:33  peter
    * assembler inlining working for ag386bin

  Revision 1.10  1999/03/29 16:05:50  peter
    * optimizer working for ag386bin

  Revision 1.9  1999/03/26 00:01:14  peter
    * first things for optimizer (compiles but cycle crashes)

  Revision 1.8  1999/03/06 17:24:21  peter
    * rewritten intel parser a lot, especially reference reading
    * size checking added for asm parsers

  Revision 1.7  1999/03/02 02:56:20  peter
    + stabs support for binary writers
    * more fixes and missing updates from the previous commit :(

  Revision 1.6  1999/03/01 15:46:22  peter
    * ag386bin finally make cycles correct
    * prefixes are now also normal opcodes

  Revision 1.5  1999/02/26 00:48:29  peter
    * assembler writers fixed for ag386bin

  Revision 1.4  1999/02/25 21:03:04  peter
    * ag386bin updates
    + coff writer

  Revision 1.3  1999/02/22 02:44:18  peter
    * ag386bin doesn't use i386.pas anymore

  Revision 1.2  1999/02/22 02:16:03  peter
    * updates for ag386bin

  Revision 1.1  1999/02/16 17:59:38  peter
    + initial files

}
