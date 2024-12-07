{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2013 by the Free Pascal development team.

    Processor dependent implementation for the system unit for
    ARM Thumb

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$define FPC_SYSTEM_HAS_SYSINITFPU}
Procedure SysInitFPU;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
{$ifndef FPUNONE}
    softfloat_exception_mask:=[float_flag_underflow,float_flag_inexact,float_flag_denormal];
    softfloat_exception_flags:=[];
{$endif FPUNONE}
end;


procedure fpc_cpucodeinit;
begin
end;


{$define FPC_SYSTEM_HAS_SYSRESETFPU}
Procedure SysResetFPU;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
{$ifndef FPUNONE}
  softfloat_exception_flags:=[];
{$endif FPUNONE}
end;


{$IFNDEF INTERNAL_BACKTRACE}
{$define FPC_SYSTEM_HAS_GET_FRAME}
function get_frame:pointer;assembler;nostackframe;
  asm
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


{$ifndef VER3_2}
{$define FPC_SYSTEM_INTERLOCKED_USE_INTRIN}
{$endif}


{$ifdef VER3_2}
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


function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint;
  begin
    Result:=Target;
    inc(Target,Source);
  end;
{$endif VER3_2}


{$ifdef VER3_2}
function InterlockedCompareExchange(var Target: longint; NewValue: longint; Comperand: longint): longint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_CMP_XCHG_32}
function fpc_atomic_cmp_xchg_32(var Target: longint; NewValue; longint; Comparand: longint): longint; [public,alias:'FPC_ATOMIC_CMP_XCHG_32'];
{$endif VER3_2}
  begin
    Result:=Target;
    if Target={$ifdef VER3_2}Comperand{$else}Comparand{$endif} then
      Target:=NewValue;
  end;

