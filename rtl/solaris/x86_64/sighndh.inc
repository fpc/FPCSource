{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2009 by Pierre Muller,
    member of the Free Pascal development team.

    Sigcontext and Sigaction for amd64/i386 CPUs

    Adapted from
    http://cvs.opensolaris.org/source/xref/onnv/onnv-gate/usr/src/uts/intel/sys/regset.h

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$packrecords C}

{$packrecords C}

const
{ i386/amd64 definition }

{
    #if defined(__amd64)
    #define	_NGREG	28
    #else
    #define	_NGREG	19
    #endif
}
{$ifdef x86_64 }
  _NGREG = 28;
{$else i386 }
  _NGREG = 19;
{$endif i386 }
  _NGREG32 = 19;
  _NGREG64 = 28;

{$ifdef x86_64}
(* AMD64 layout
      #define REG_GSBASE      27
      #define	REG_FSBASE	26
      #define	REG_DS		25
      #define	REG_ES		24

      #define	REG_GS		23
      #define	REG_FS		22
      #define	REG_SS		21
      #define	REG_RSP		20
      #define	REG_RFL		19
      #define	REG_CS		18
      #define	REG_RIP		17
      #define	REG_ERR		16
      #define	REG_TRAPNO	15
      #define	REG_RAX		14
      #define	REG_RCX		13
      #define	REG_RDX		12
      #define	REG_RBX		11
      #define	REG_RBP		10
      #define	REG_RSI		9
      #define	REG_RDI		8
      #define	REG_R8		7
      #define	REG_R9		6
      #define	REG_R10		5
      #define	REG_R11		4
      #define	REG_R12		3
      #define	REG_R13		2
      #define	REG_R14		1
      #define	REG_R15		0
*)
  REG_R15    = 0;
  REG_R14    = 1;
  REG_R13    = 2;
  REG_R12    = 3;
  REG_R11    = 4;
  REG_R10    = 5;
  REG_R9     = 6;
  REG_R8     = 7;
  REG_RDI    = 8;
  REG_RSI    = 9;
  REG_RBP    = 10;
  REG_RBX    = 11;
  REG_RDX    = 12;
  REG_RCX    = 13;
  REG_RAX    = 14;
  REG_TRAPNO = 15;
  REG_ERR    = 16;
  REG_RIP    = 17;
  REG_CS     = 18;
  REG_RFL    = 19;
  REG_RSP    = 20;
  REG_SS     = 21;
  REG_FS     = 22;
  REG_GS     = 23;
  REG_ES     = 24;
  REG_DS     = 25;
  REG_FSBASE = 26;
  REG_GSBASE = 27;

{$else i386}
(* I386 layout
    #define     SS              18	/* only stored on a privilege transition */
    #define	UESP		17	/* only stored on a privilege transition */
    #define	EFL		16
    #define	CS		15
    #define	EIP		14
    #define	ERR		13
    #define	TRAPNO		12
    #define	EAX		11
    #define	ECX		10
    #define	EDX		9
    #define	EBX		8
    #define	ESP		7
    #define	EBP		6
    #define	ESI		5
    #define	EDI		4
    #define	DS		3
    #define	ES		2
    #define	FS		1
    #define	GS		0
*)
  REG_GS     = 0;
  REG_FS     = 1;
  REG_ES     = 2;
  REG_DS     = 3;
  REG_EDI    = 4;
  REG_ESI    = 5;
  REG_EBP    = 6;
  REG_ESP    = 7;
  REG_EBX    = 8;
  REG_EDX    = 9;
  REG_ECX    = 10;
  REG_EAX    = 11;
  REG_TRAPNO = 12;
  REG_ERR    = 13;
  REG_EIP    = 14;
  REG_CS     = 15;
  REG_EFL    = 16;
  REG_UESP   = 17; (* only stored on a privilege transition *)
  REG_SS     = 18;   (* only stored on a privilege transition *)
{$endif i386}


{$ifdef x86_64 }
  REG_PC  = REG_RIP;
  REG_FP  = REG_RBP;
  REG_SP  = REG_RSP;
  REG_PS  = REG_RFL;
  REG_R0  = REG_RAX;
  REG_R1  = REG_RDX;
{$else	/* __i386 */ }
  REG_PC  = REG_EIP;
  REG_FP  = REG_EBP;
  REG_SP  = REG_UESP;
  REG_PS  = REG_EFL;
  REG_R0  = REG_EAX;
  REG_R1  = REG_EDX;
{$endif }

type
{$ifdef x86_64}
  TGReg = cint64;
{$else}
  TGReg = cint32;
{$endif}
  TGReg32 = cint32;
  TGReg64 = cint64;

  TGRegSet = array[0.._NGREG-1] of TGReg;
  TGRegSet32 = array[0.._NGREG32-1] of TGReg32;
  TGRegSet64 = array[0.._NGREG64-1] of TGReg64;



type
  FPU_SAVE_TYPE = (fnsave_type, fxsave_type);

  TFPURegs = record
    case longint of
      0: ( fpuregs: array[0..31] of cardinal);
      1: ( fpudregs: array[0..15] of double);
  end;

  PFQ = ^TFQ;
  TFQ = record
    fpq_addr : ^cuint;
    fpq_instr : cuint;
  end;

(*  struct fpchip_state {
    uint32_t state[27];	/* 287/387 saved state */
    uint32_t status;	/* saved at exception */
    uint32_t mxcsr;		/* SSE control and status */
    uint32_t xstatus;	/* SSE mxcsr at exception */
    uint32_t __pad[2];	/* align to 128-bits */
    upad128_t xmm[8];	/* %xmm0-%xmm7 */
    } fpchip_state;
*)
  TUpad128 = record
   case longint of
   0: (_q : extended;);
   1: (_l : array [0..4-1] of cuint32;);
  end;

  TFPChip_State = record
    state   : array [0..27-1] of cuint32;
    status  : cuint32;
    mxcsr   : cuint32;
    xstatus : cuint32;
    __pad   : array [0..1] of cuint32;
    xmm   : array [0..8-1] of TUpad128;
  end;

  TFP_emul_space = record
    fp_emul : array [0..248-1] of cuint8;
    fp_epad : array [0..1] of cuint8;
  end;

  TFPU = record
  case longint of
   0: (fpchip_state : TFPChip_state;);
   1: (fp_emul_space : TFP_emul_space;);
   2: (f_fpregs : Array[0..130-1] of cuint32;);
  end;

  TFPRegSet = TFPU;

  TMContext = record
    gregs    : TGRegSet;
    fpregs   : TFPRegSet;
  end;

  TStack = record
    ss_sp     : pointer;
    ss_size  : size_t;
    ss_flags : cint;
  end;

  PSigContext = ^TSigContext;
  TSigContext = record
    uc_flags    : cuint;
    uc_link     : PSigContext;
    uc_sigmask  : sigset_t;
    uc_stack    : TStack;
    uc_mcontext : TMContext;
    __uc_filler : array[0..5-1] of clong;
  end;

