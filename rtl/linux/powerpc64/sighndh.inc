{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Jonas Maebe,
    member of the Free Pascal development team.

    TSigContext

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$packrecords C}

type
  TPPC_Reg = QWord;
  { from include/ppc/ptrace.h }
  pptregs = ^tptregs;
  tptregs = record
    gpr: array[0..31] of TPPC_Reg;
    nip: TPPC_Reg;
    msr: TPPC_Reg;
    orig_gpr3: TPPC_Reg; { Used for restarting system calls }
    ctr: TPPC_Reg;
    link: TPPC_Reg;
    xer: TPPC_Reg;
    ccr: TPPC_Reg;
    mq: TPPC_Reg;        { 601 only (not used at present)  }
                         { Used on APUS to hold IPL value. }
    trap: TPPC_Reg;      { Reason for being here }
    dar: TPPC_Reg;       { Fault registers }
    dsisr: TPPC_Reg;
    result: TPPC_Reg;    { Result of a system call }
  end;

  { from include/asm-ppc/signal.h }
  stack_t = record
    ss_sp: pointer;
    ss_flags: longint;
    ss_size: size_t;
  end;

  { from include/asm-ppc/sigcontext.h }
  tsigcontext_struct = record
    _unused: array[0..3] of PtrUInt;
    signal: longint;
    handler: PtrInt;
    oldmask: PtrInt;
    pt_regs: pptregs;
  end;

  { from include/asm-ppc/ucontext.h }
  pucontext = ^tucontext;
  tucontext = record
    uc_flags : dword;
    uc_link : pucontext;
    uc_stack : stack_t;
    uc_mcontext : tsigcontext_struct;
    uc_sigmask : sigset_t;
  end;


  { from arch/ppc/kernel/signal.c, the type of the actual parameter passed }
  { to the sigaction handler                                               }
  t_rt_sigframe = record
    _unused: array[0..1] of cardinal;
    pinfo: psiginfo;
    puc: pointer;
    siginfo: tsiginfo;
    uc: tucontext;
  end;

  PSigContext = ^TSigContext;
  TSigContext= tsigcontext_struct;
