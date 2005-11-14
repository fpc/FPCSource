{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Jonas Maebe,
    member of the Free Pascal development team.

    TSigContext and related records. Translated from headers from
    2.6.11 kernel sources.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$packrecords C}

type
  gpr_reg = cULong;

  { from include/ppc/ptrace.h }
  pptregs = ^tptregs;
  tptregs = record
    gpr: array[0..31] of gpr_reg;
    nip: gpr_reg;
    msr: gpr_reg;
    orig_gpr3: gpr_reg; { Used for restarting system calls }
    ctr: gpr_reg;
    link: gpr_reg;
    xer: gpr_reg;
    ccr: gpr_reg;
    mq: gpr_reg;        { 601 only (not used at present)  }
                        { Used on APUS to hold IPL value. }
    trap: gpr_reg;      { Reason for being here }
    dar: gpr_reg;       { Fault registers }
    dsisr: gpr_reg;
    result: gpr_reg;    { Result of a system call }
  end;

  { from include/asm-ppc/signal.h }
  stack_t = record
    ss_sp: pointer;
    ss_flags: cInt;
    ss_size: size_t;
  end;

  { from include/asm-ppc/sigcontext.h }
  tsigcontext_struct = record
    _unused: array[0..3] of cULong;
    signal: cInt;
    handler: cULong;
    oldmask: cULong;
    pt_regs: pptregs;
  end;

  { from include/asm-ppc/ucontext.h }
  pucontext = ^tucontext;
  tucontext = record
    uc_flags : cULong;
    uc_link : pucontext;
    uc_stack : stack_t;
    uc_mcontext : tsigcontext_struct;
    uc_sigmask : sigset_t;
  end;

  { from arch/ppc/kernel/signal.c, the type of the actual parameter passed }
  { to the sigaction handler                                               }
  PSigContext = ^TSigContext;
  TSigContext= tucontext;
