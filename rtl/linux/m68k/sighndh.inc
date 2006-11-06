{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Pierre Muller,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

type
  { REMARK: floating point regs are defined as arrays of
    3 longints; I don't know if C does align this to
    16 byte boundaries for each element of the array PM }
  { If C does we might need to define this as
    array from 0 to 3 }

  tfpreg = array[0..2] of longint;

  pfpstate = ^tfpstate;
  tfpstate = record
           pcr,psr,fpiaddr : longint;
	   fpreg : array [0..7] of tfpreg;
  end;

  { as defined in asm_m68k/signal.h }
  Stack_T = Record
    ss_sp : pointer;
    ss_flags : longint;
    ss_size : size_t;
  end;

  { SigContextRec corresponds to the ucontext record
    in linux asm-m68k/ucontext.h include file }
  PSigContext = ^TSigContext;
  TSigContext = record
    uc_flags : cardinal;
    uc_link : pSigContext;
    uc_stack : stack_t; { what's that ?? }
    { fields from 'version' to 'pc'
      correspond to the mcontext struct in asm-m68k/ucontext.h file }
    version : longint; { SigContext version check }
    { 18 general registers }
    d0,d1,d2,d3,d4,d5,d6,d7 : cardinal;
    a0,a1,a2,a3,a4,a5 : cardinal;
    fp,sp,ps,pc : cardinal;
    { fields from 'pcr' to 'fpreg'
      are floating point part }
    pcr,psr,fpiaddr : longint;
    fpreg : array[0..7] of tfpreg; { how is this aligned ?? }
    filler : array[0..79] of cardinal;
    sigmask : TSigSet;
  end;
