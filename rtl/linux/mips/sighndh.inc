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

const
  __SUNOS_MAXWIN = 31;

type
  twbuf = record
    locals : array[0..7] of longint;
    ins    : array[0..7] of longint;
  end;

(* MIPS OABI32 structure 
struct sigcontext {
  unsigned int sc_regmask;
  unsigned int sc_status;
  unsigned long long sc_pc;
  unsigned long long sc_regs[32];
  unsigned long long sc_fpregs[32];
  unsigned int sc_ownedfp;
  unsigned int sc_fpc_csr;
  unsigned int sc_fpc_eir;
  unsigned int sc_used_math;
  unsigned int sc_dsp;
  unsigned long long sc_mdhi;
  unsigned long long sc_mdlo;
  unsigned long sc_hi1;
  unsigned long sc_lo1;
  unsigned long sc_hi2;
  unsigned long sc_lo2;
  unsigned long sc_hi3;
  unsigned long sc_lo3;
}; *)
  PSigContext = ^TSigContext;
  TSigContext = record
    sigc_regmask,                   
    sigc_status: cuint; 
    sigc_pc : culonglong;    
    sigc_regs : array[0..31] of culonglong;
    sigc_fpregs : array[0..31] of culonglong; 
    sigc_fpc_csr, sigc_fpc_eir : cuint;
    sigc_used_math : cuint;
    sigc_dsp : cuint;
    sigc_mdhi, sigc_mdlo : culonglong;
    sigc_hi1,sigc_lo1,
    sigc_hi2,sigc_lo2,
    sigc_hi3,sigc_lo3 : culong;
  end;

