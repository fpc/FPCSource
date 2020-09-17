{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Jonas Maebe,
    member of the Free Pascal development team.

    TSigContext and associated structures.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$packrecords C}

type
  TAarch64_ctx = record
    magic,
    size : DWord
  end;

  TFPSIMD_Context = record
    head : TAarch64_ctx;
    fpsr,
    fpcr : DWord;
    vregs : array[0..31] of array[0..7] of Byte;
  end;

  PSigContext = ^TSigContext;
  TSigContext = record
    fault_address : cULong;
    regs : array[0..30] of cULong;
    sp : cULong;
    pc : cULong;
    pstate : cULong;
    __pad : cULong;
    { The following fields should be 16-byte-aligned. Currently the
      directive for specifying alignment is buggy, so the preceding
      field was added so that the record has the right size. }
    case Byte of
      1: (__reserved : array[0..4095] of cUChar);
      2: (FPSIMD_Context : TFPSIMD_Context);
  end;

  stack_t = record
    ss_sp : pointer;
    ss_flags : cInt;
    ss_size : size_t;
  end;

  PUContext = ^TUContext;
  TUContext = record
    uc_flags : cULong;
    uc_link : PUContext;
    uc_stack : stack_t;
    uc_sigmask : sigset_t;
    __padding : array[1..128 - sizeof(sigset_t)] of byte;
    __pad : cULong;
    { The following field should be 16-byte-aligned. Currently the
      directive for specifying alignment is buggy, so the preceding
      field was added so that the record has the right size. }
    uc_mcontext : TSigContext;
  end;
