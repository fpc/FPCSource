{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Jonas Maebe,
    member of the Free Pascal development team.

    TSigcontext

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$packrecords C}

type
  Pfpstate = ^Tfpstate;
  Tfpstate = record
    cwd,
    swd,
    twd,    // Note this is not the same as the 32bit/x87/FSAVE twd
    fop : word;
    rip,
    rdp : qword;
    mxcsr,
    mxcsr_mask : dword;
    st_space : array[0..31] of dword;  // 8*16 bytes for each FP-reg
    xmm_space : array[0..63] of dword; // 16*16 bytes for each XMM-reg
    reserved2 : array[0..23] of dword;
  end;

  PSigContext = ^TSigContext;
  TSigContext = record
    __pad00 : array[0..4] of qword;
    r8,
    r9,
    r10,
    r11,
    r12,
    r13,
    r14,
    r15,
    rdi,
    rsi,
    rbp,
    rbx,
    rdx,
    rax,
    rcx,
    rsp,
    rip,
    eflags : qword;
    cs,
    gs,
    fs,
    __pad0 : word;
    err,
    trapno,
    oldmask,
    cr2 : qword;
    fpstate : Pfpstate;       // zero when no FPU context */
    reserved1 : array[0..7] of qword;
  end;


