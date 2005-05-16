{
    $Id: sighndh.inc,v 1.2 2005/02/14 17:13:30 peter Exp $
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
  { from include/ppc/ptrace.h }
  pptregs = ^tptregs;
  tptregs = record
    gpr: array[0..31] of cardinal;
    nip: cardinal;
    msr: cardinal;
    orig_gpr3: cardinal; { Used for restarting system calls }
    ctr: cardinal;
    link: cardinal;
    xer: cardinal;
    ccr: cardinal;
    mq: cardinal;        { 601 only (not used at present)  }
                         { Used on APUS to hold IPL value. }
    trap: cardinal;      { Reason for being here }
    dar: cardinal;       { Fault registers }
    dsisr: cardinal;
    result: cardinal;    { Result of a system call }
  end;

  { from include/asm-ppc/signal.h }
  stack_t = record
    ss_sp: pointer;
    ss_flags: longint;
    ss_size: size_t;
  end;

  { from include/asm-ppc/sigcontext.h }
  tsigcontext_struct = record
    _unused: array[0..3] of dword;
    signal: longint;
    handler: dword;
    oldmask: dword;
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

{
  $Log: sighndh.inc,v $
  Revision 1.2  2005/02/14 17:13:30  peter
    * truncate log

  Revision 1.1  2005/01/30 18:01:15  peter
    * signal cleanup for linux
    * sigactionhandler instead of tsigaction for bsds
    * sigcontext moved to cpu dir

}

