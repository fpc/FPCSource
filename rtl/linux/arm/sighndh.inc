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
  PSigContext = ^TSigContext;
  TSigContext = record
    trap_no : dword;
    error_code : dword;
    oldmask : dword;
    arm_r0 : dword;
    arm_r1 : dword;
    arm_r2 : dword;
    arm_r3 : dword;
    arm_r4 : dword;
    arm_r5 : dword;
    arm_r6 : dword;
    arm_r7 : dword;
    arm_r8 : dword;
    arm_r9 : dword;
    arm_r10 : dword;
    arm_fp : dword;
    arm_ip : dword;
    arm_sp : dword;
    arm_lr : dword;
    arm_pc : dword;
    arm_cpsr : dword;
    fault_address : dword;
  end;

  { from include/asm-ppc/signal.h }
  stack_t = record
    ss_sp: pointer;
    ss_flags: longint;
    ss_size: size_t;
  end;

  { from include/asm-arm/ucontext.h }
  pucontext = ^tucontext;
  tucontext = record
    uc_flags : dword;
    uc_link : pucontext;
    uc_stack : stack_t;
    uc_mcontext : TSigContext;
    uc_sigmask : sigset_t;
  end;



