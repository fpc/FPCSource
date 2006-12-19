{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Jonas Maebe,
    member of the Free Pascal development team.

    Sigcontext and Sigaction

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$packrecords C}

{ sparc v8 definition }

const
  SPARC_MAXREGWINDOW = 31;
  _NGREG = 19;

type
{$ifdef cpu64}
  TGReg = clong;
{$else}
  TGReg = cint;
{$endif}

  TGRegSet = array[0.._NGREG-1] of TGReg;

  PSPBuf = ^TSPBuf;
  TSPBuf = array[0..SPARC_MAXREGWINDOW-1] of TGReg;

  TRWindow = record
    rw_local : array[0..7] of TGReg;
    rw_in    : array[0..7] of TGReg;
  end;

  PGWindows = ^TGWindows;
  TGWindows = record
    wbcnt   : cint;
    spbuf   : PSPBuf;
    rwindow : array[0..SPARC_MAXREGWINDOW-1] of TRWindow;
  end;

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

  TFPU = record
    fpu_fr : TFPURegs;
    fq: PFQ;
    fpu_fsr: cardinal;
    fpu_qcnt : byte;
    fpu_q_entrysize : byte;
    fpu_e : byte;
  end;
  TFPRegSet = TFPU;

  TXRS = record
    xrs_id  : cuint;
    xrs_ptr : pointer;
  end;

  TMContext = record
    gregs    : TGRegSet;
    gwins    : PGWindows;
    fpregs   : TFPRegSet;
    xrs      : TXRS;
    __filler : array[0..19-1] of clong;
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
    __uc_filler : array[0..23-1] of clong;
  end;
