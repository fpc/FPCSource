{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by the Free Pascal development team.

    Processor dependent implementation for the system unit for
    RiscV32

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ Common RiscV stuff }
{$I ../riscv/riscv.inc}

{****************************************************************************
                       stack frame related stuff
****************************************************************************}

{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:pointer;addr:pointer=nil):pointer;assembler;
  asm
    lw a0, -4*1(a0)
  end;


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer;addr:pointer=nil):pointer;assembler;
  asm
    lw a0, -4*2(a0)
  end;

{****************************************************************************
                       atomic operations
****************************************************************************}


{ while some of the functions below could be shared with RiscV 64, this makes no sense imo
  to scatter those functions around }

{$ifdef VER3_2}
function InterLockedDecrement (var Target: longint) : longint; assembler; nostackframe;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_DEC_32}
function fpc_atomic_dec_32 (var Target: longint) : longint; assembler; nostackframe;
{$endif VER3_2}
  asm
{$ifdef CPURV_HAS_ATOMIC}
    addi a1, x0, -1
    amoadd.w a0, a1, (a0)
    add a0, a0, a1
{$else CPURV_HAS_ATOMIC}
    lw a1, 0(a0)
    addi a1, a1, -1
    sw a1, 0(a0)
    addi a0, a1, 0
{$endif CPURV_HAS_ATOMIC}
  end;


{$ifdef VER3_2}
function InterLockedIncrement (var Target: longint) : longint; assembler; nostackframe;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_INC_32}
function fpc_atomic_inc_32 (var Target: longint) : longint; assembler; nostackframe;
{$endif VER3_2}
  asm
{$ifdef CPURV_HAS_ATOMIC}
    addi a1, x0, 1
    amoadd.w a0, a1, (a0)
    add a0, a0, a1
{$else CPURV_HAS_ATOMIC}
    lw a1, 0(a0)
    addi a1, a1, 1
    sw a1, 0(a0)
    addi a0, a1, 0
{$endif CPURV_HAS_ATOMIC}
  end;


{$ifdef VER3_2}
function InterLockedExchange (var Target: longint;Source : longint) : longint; assembler; nostackframe;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_XCHG_32}
function fpc_atomic_xchg_32 (var Target: longint;Source : longint) : longint; assembler; nostackframe;
{$endif VER3_2}
  asm
{$ifdef CPURV_HAS_ATOMIC}
    amoswap.w a0, a1, (a0)
{$else CPURV_HAS_ATOMIC}
    lw a2, 0(a0)
    sw a1, 0(a0)
    addi a0, a2, 0
{$endif CPURV_HAS_ATOMIC}
  end;


{$ifdef VER3_2}
function InterlockedCompareExchange(var Target: longint; NewValue: longint; Comperand: longint): longint; assembler; nostackframe;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_CMP_XCHG_32}
function fpc_atomic_cmp_xchg_32 (var Target: longint; NewValue: longint; Comparand: longint) : longint; [public,alias:'FPC_ATOMIC_CMP_XCHG_32']; assembler; nostackframe;
{$endif VER3_2}
  asm
{$ifdef CPURV_HAS_ATOMIC}
  .LLoop:
    lr.w a3, 0(a0)
    bne a3, a2, .LFail
    sc.w a4, a1, 0(a0)
    bne a4, x0, .LLoop
  .LFail:
    addi a0, a3, 0
{$else CPURV_HAS_ATOMIC}
    lw a3, 0(a0)
    bne a3, a2, .LFail
    sw a1, 0(a0)
  .LFail:
    addi a0, a3, 0
{$endif CPURV_HAS_ATOMIC}
  end;


{$ifdef VER3_2}
function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint; assembler; nostackframe;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_ADD_32}
function fpc_atomic_add_32 (var Target: longint;Value: longint) : longint; assembler; nostackframe;
{$endif VER3_2}
  asm
{$ifdef CPURV_HAS_ATOMIC}
    amoadd.w a0, a1, (a0)
{$else CPURV_HAS_ATOMIC}
    lw a2, 0(a0)
    add a2, a2, a1
    sw a2, 0(a0)
    addi a0, a2, 0
{$endif CPURV_HAS_ATOMIC}
  end;
