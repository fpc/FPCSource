{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2014 by Jonas Maebe, member of
    the Free Pascal development team.

    Processor dependent implementation for the system unit for
    AArch64

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF LINUX}
    {$DEFINE USE_DCBZ}
{$ENDIF LINUX}

{****************************************************************************
                           AArch64 specific stuff
****************************************************************************}
const
  fpu_ioe = 1 shl 8;
  fpu_dze = 1 shl 9;
  fpu_ofe = 1 shl 10;
  fpu_ufe = 1 shl 11;
  fpu_ixe = 1 shl 12;
  fpu_ide = 1 shl 15;
  fpu_exception_mask = fpu_ioe or fpu_dze or fpu_ofe or fpu_ufe or fpu_ixe or fpu_ide;
  fpu_exception_mask_to_status_mask_shift = 8;

function getfpcr: dword; nostackframe; assembler;
  asm
    mrs x0,fpcr
  end;


procedure setfpcr(val: dword); nostackframe; assembler;
  asm
    msr fpcr,x0
  end;


function getfpsr: dword; nostackframe; assembler;
  asm
    mrs x0,fpsr
  end;


procedure setfpsr(val: dword); nostackframe; assembler;
  asm
    msr fpsr, x0
  end;


procedure fpc_enable_fpu_exceptions;
  begin
    { clear all "exception happened" flags we care about}
    setfpsr(getfpsr and not(fpu_exception_mask shr fpu_exception_mask_to_status_mask_shift));
    { enable invalid operations and division by zero exceptions. }
    setfpcr(getfpcr or fpu_exception_mask);
  end;

procedure fpc_cpuinit;
  begin
    { don't let libraries influence the FPU cw set by the host program }
    if not IsLibrary then
      fpc_enable_fpu_exceptions;
  end;


{****************************************************************************
                                Move / Fill
****************************************************************************}


{****************************************************************************
                                 String
****************************************************************************}

{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:pointer;addr:pointer=nil):pointer;assembler; nostackframe;
  asm
    cbz x0, .Lcaller_addr_invalid
    ldur x0, [x0]
    cbz x0, .Lcaller_addr_invalid
    ldur x0, [x0, #8]
   .Lcaller_addr_invalid:
  end;


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer;addr:pointer=nil):pointer;assembler; nostackframe;
  asm
    cbz x0, .Lcaller_addr_invalid
    ldur x0, [x0]
   .Lcaller_addr_invalid:
  end;


{$define FPC_SYSTEM_HAS_SPTR}
Function Sptr : Pointer;assembler; nostackframe;
  asm
    mov x0, sp
  end;


{****************************************************************************
                                 Str()
****************************************************************************}

{ int_str: generic implementation is used for now }


{****************************************************************************
                             Multithreading
****************************************************************************}

{ perform a thread-safe inc/dec }

{$define FPC_SYSTEM_HAS_DECLOCKED_LONGINT}
function declocked(var l : longint) : boolean;assembler;nostackframe;
  { input:  address of l in x0                                      }
  { output: boolean indicating whether l is zero after decrementing }
  asm
  .LDecLockedLoop:
    ldxr   w1,[x0]
    sub    w1,w1,#1
    stxr   w2,w1,[x0]
    cbnz   w2,.LDecLockedLoop
    cset   w0, eq
  end;


{$define FPC_SYSTEM_HAS_INCLOCKED_LONGINT}
procedure inclocked(var l : longint);assembler;nostackframe;
  asm
  .LIncLockedLoop:
    ldxr   w1,[x0]
    add    w1,w1,#1
    stxr   w2,w1,[x0]
    cbnz   w2,.LIncLockedLoop
  end;


{$define FPC_SYSTEM_HAS_DECLOCKED_INT64}
function declocked(var l : int64) : boolean;assembler;nostackframe;
  { input:  address of l in x0                                      }
  { output: boolean indicating whether l is zero after decrementing }
  asm
  .LDecLockedLoop:
    ldxr   x1,[x0]
    subs   x1,x1,#1
    stxr   w2,x1,[x0]
    cbnz   w2,.LDecLockedLoop
    cset   w0, eq
  end;


{$define FPC_SYSTEM_HAS_INCLOCKED_INT64}
procedure inclocked(var l : int64);assembler;nostackframe;
  asm
  .LIncLockedLoop:
    ldxr   x1,[x0]
    add    x1,x1,#1
    stxr   w2,x1,[x0]
    cbnz   w2,.LIncLockedLoop
  end;


function InterLockedDecrement (var Target: longint) : longint; assembler; nostackframe;
  { input:  address of target in x0 }
  { output: target-1 in x0          }
  { side-effect: target := target-1 }
  asm
  .LInterDecLockedLoop:
    ldxr   w1,[x0]
    sub    w1,w1,#1
    stxr   w2,w1,[x0]
    cbnz   w2,.LInterDecLockedLoop
    mov    w0,w1
  end;


function InterLockedIncrement (var Target: longint) : longint; assembler; nostackframe;
  { input:  address of target in x0 }
  { output: target+1 in x0          }
  { side-effect: target := target+1 }
  asm
  .LInterIncLockedLoop:
    ldxr   w1,[x0]
    add    w1,w1,#1
    stxr   w2,w1,[x0]
    cbnz   w2,.LInterIncLockedLoop
    mov    w0,w1
  end;


function InterLockedExchange (var Target: longint;Source : longint) : longint; assembler; nostackframe;
  { input:  address of target in x0, source in w1 }
  { output: target in x0                          }
  { side-effect: target := source                 }
  asm
  .LInterLockedXchgLoop:
    ldxr   w2,[x0]
    stxr   w3,w1,[x0]
    cbnz   w3,.LInterLockedXchgLoop
    mov    w0,w2
  end;


function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint; assembler; nostackframe;
  asm
  .LInterLockedXchgAddLoop:
    ldxr   w2,[x0]
    add    w4,w2,w1
    stxr   w3,w4,[x0]
    cbnz   w3,.LInterLockedXchgAddLoop
    mov    w0,w2
  end;


function InterlockedCompareExchange(var Target: longint; NewValue: longint; Comperand: longint): longint; assembler; nostackframe;
  { input:  address of target in x0, newvalue in w1, comparand in w2 }
  { output: value stored in target before entry of the function      }
  { side-effect: NewValue stored in target if (target = comparand)   }
  asm
  .LInterlockedCompareExchangeLoop:
    ldxr   w3,[x0]
    cmp    w3,w2
    csel   w4,w1,w3,eq
    stxr   w5,w4,[x0]
    cbnz   w5,.LInterlockedCompareExchangeLoop
    mov    w0,w3
  end;


function InterLockedDecrement64 (var Target: int64) : int64; assembler; nostackframe;
  asm
  .LInterDecLockedLoop:
    ldxr   x1,[x0]
    sub    x1,x1,#1
    stxr   w2,x1,[x0]
    cbnz   w2,.LInterDecLockedLoop
    mov    x0,x1
  end;


function InterLockedIncrement64 (var Target: int64) : int64; assembler; nostackframe;
  asm
  .LInterIncLockedLoop:
    ldxr   x1,[x0]
    add    x1,x1,#1
    stxr   w2,x1,[x0]
    cbnz   w2,.LInterIncLockedLoop
    mov    x0,x1
  end;


function InterLockedExchange64 (var Target: int64;Source : int64) : int64; assembler; nostackframe;
  asm
  .LInterLockedXchgLoop:
    ldxr   x2,[x0]
    stxr   w3,x1,[x0]
    cbnz   w3,.LInterLockedXchgLoop
    mov    x0,x2
  end;


function InterLockedExchangeAdd64 (var Target: int64;Source : int64) : int64; assembler; nostackframe;
  asm
  .LInterLockedXchgAddLoop:
    ldxr   x2,[x0]
    add    x4,x2,x1
    stxr   w3,x4,[x0]
    cbnz   w3,.LInterLockedXchgAddLoop
    mov    x0,x2
  end;


function InterLockedCompareExchange64(var Target: int64; NewValue, Comperand : int64): int64; assembler; nostackframe;
  asm
  .LInterlockedCompareExchangeLoop:
    ldxr   x3,[x0]
    cmp    x3,x2
    csel   x4,x1,x3,eq
    stxr   w5,x4,[x0]
    cbnz   w5,.LInterlockedCompareExchangeLoop
    mov    x0,x3
  end;


{$ifndef FPC_SYSTEM_HAS_MEM_BARRIER}
{$define FPC_SYSTEM_HAS_MEM_BARRIER}
procedure ReadBarrier;assembler;nostackframe;{$ifdef SYSTEMINLINE}inline;{$endif}
  asm
    { dmb ishld }
    dmb #9
  end;

procedure ReadDependencyBarrier;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  { reads imply barrier on earlier reads depended on }
end;

procedure ReadWriteBarrier;assembler;nostackframe;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
  { dmb ish }
  dmb #11
end;

procedure WriteBarrier;assembler;nostackframe;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
  { dmb ishst }
  dmb #10
end;

{$endif}
