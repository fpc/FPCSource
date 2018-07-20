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

procedure fpc_cpuinit;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
  end;


{$IFNDEF INTERNAL_BACKTRACE}
{$define FPC_SYSTEM_HAS_GET_FRAME}
function get_frame:pointer;assembler;nostackframe;
  asm
    addi a0, fp, 0
  end;
{$ENDIF not INTERNAL_BACKTRACE}


{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:pointer;addr:pointer=nil):pointer;assembler;
  asm
    ld a0, -8*2(a0)
  end;


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer;addr:pointer=nil):pointer;assembler;
  asm
    ld a0, -8*1(a0)
  end;


{$define FPC_SYSTEM_HAS_SPTR}
Function Sptr : pointer;assembler;
  asm
    addi a0, sp, 0
  end;


function InterLockedDecrement (var Target: longint) : longint;
  begin
    dec(Target);
    Result:=Target;
  end;


function InterLockedIncrement (var Target: longint) : longint;
  begin
    inc(Target);
    Result:=Target;
  end;


function InterLockedExchange (var Target: longint;Source : longint) : longint;
  begin
    Result:=Target;
    Target:=Source;
  end;


function InterlockedCompareExchange(var Target: longint; NewValue: longint; Comperand: longint): longint;
  begin
    Result:=Target;
    if Target=Comperand then
      Target:=NewValue;
  end;


function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint;
  begin
    Result:=Target;
    inc(Target,Source);
  end;



function InterLockedDecrement64 (var Target: int64) : int64;
  begin
    dec(Target);
    Result:=Target;
  end;


function InterLockedIncrement64 (var Target: int64) : int64;
  begin
    inc(Target);
    Result:=Target;
  end;


function InterLockedExchange64 (var Target: int64;Source : int64) : int64;
  begin
    Result:=Target;
    Target:=Source;
  end;


function InterlockedCompareExchange64(var Target: int64; NewValue: int64; Comperand: int64): int64;
  begin
    Result:=Target;
    if Target=Comperand then
      Target:=NewValue;
  end;


function InterLockedExchangeAdd64 (var Target: int64;Source : int64) : int64;
  begin
    Result:=Target;
    inc(Target,Source);
  end;

