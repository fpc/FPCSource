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
  PContext_Info = ^TContext_Info;
  TContext_Info = record
    magic,
    size : DWord;
    __padding : cULong;
  end;

  PFPU_Context = ^TFPU_Context;
  TFPU_Context = record
    regs : array[0..31] of cULong;
    fcc : cULong;
    fcsr : cUInt;
  end;

  PLSX_Context = ^TLSX_Context;
  TLSX_Context = record
    regs : array[0..31] of array[0..15] of Byte;
    fcc : cULong;
    fcsr : cUInt;
  end;

  PLASX_Context = ^TLASX_Context;
  TLASX_Context = record
    regs : array[0..31] of array[0..31] of Byte;
    fcc : cULong;
    fcsr : cUInt;
  end;

  PSigContext = ^TSigContext;
  TSigContext = record
    pc : cULong;
    regs : array[0..31] of cULong;
    flags : cUInt;
    { __pad : cULong; }
    { The following fields should be 16-byte-aligned. Currently the
      directive for specifying alignment is buggy, so the preceding
      field was added so that the record has the right size.
      It is noted by aarch64 and we drop here at present. }
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
    {$if sizeof(sigset_t) < 128 }
    __padding : array[1..128 - sizeof(sigset_t)] of byte;
    {$endif}
    __pad : cULong;
    { The following field should be 16-byte-aligned. Currently the
      directive for specifying alignment is buggy, so the preceding
      field was added so that the record has the right size.
      It is noted by aarch64 and we keep here at present. }
    uc_mcontext : TSigContext;
  end;
