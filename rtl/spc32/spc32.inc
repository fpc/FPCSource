{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by the Free Pascal development team.

    Processor dependent implementation for the system unit for
    AVR

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$if not(defined(FPUSOFT))}
Procedure SysInitFPU;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
  end;
{$endif}


procedure fpc_cpuinit;
  begin
    SysInitFPU;
  end;



{$IFNDEF INTERNAL_BACKTRACE}
{$define FPC_SYSTEM_HAS_GET_FRAME}
function get_frame:pointer;assembler;nostackframe;
  asm
    ld r5
    st r0
  end;
{$ENDIF not INTERNAL_BACKTRACE}


{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:pointer;addr:pointer=nil):pointer;assembler;
  asm
  end;


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer;addr:pointer=nil):pointer;assembler;
  asm
  end;


{$define FPC_SYSTEM_HAS_SPTR}
Function Sptr : pointer;assembler;
  asm
  end;


function InterLockedDecrement (var Target: longint) : longint; assembler; nostackframe;
  asm
    ld -1
    st r1
    ld r0
    inc r1
    st r0
  end;


function InterLockedIncrement (var Target: longint) : longint; assembler; nostackframe;
  asm
    ld 1
    st r1
    ld r0
    inc r1
    st r0
  end;


function InterLockedExchange (var Target: longint;Source : longint) : longint; assembler; nostackframe;
  asm
    ld r0
    xchg r1
    st r0
  end;


function InterlockedCompareExchange(var Target: longint; NewValue: longint; Comperand: longint): longint; assembler; nostackframe;
  asm
    ld r0
    cas r2, r1
    st r0
  end;


function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint; assembler; nostackframe;
  asm
    ld r0
    inc r1
    sub r1
    st r0
  end;

