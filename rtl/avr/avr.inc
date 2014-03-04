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

{$asmmode gas}

Procedure SysInitFPU;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
  end;


procedure fpc_cpuinit;
  begin
    SysInitFPU;
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


function InterLockedDecrement (var Target: longint) : longint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    asm
      in r0,0x3f
      std temp_sreg,r0
      cli
    end;

    dec(Target);
    Result:=Target;

    { release interrupts }
    asm
      ldd r0,temp_sreg
      out 0x3f,r0
    end;
  end;


function InterLockedIncrement (var Target: longint) : longint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    asm
      in r0,0x3f
      std temp_sreg,r0
      cli
    end;

    inc(Target);
    Result:=Target;

    { release interrupts }
    asm
      ldd r0,temp_sreg
      out 0x3f,r0
    end;
  end;


function InterLockedExchange (var Target: longint;Source : longint) : longint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    asm
      in r0,0x3f
      std temp_sreg,r0
      cli
    end;

    Result:=Target;
    Target:=Source;

    { release interrupts }
    asm
      ldd r0,temp_sreg
      out 0x3f,r0
    end;
  end;


function InterlockedCompareExchange(var Target: longint; NewValue: longint; Comperand: longint): longint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    asm
      in r0,0x3f
      std temp_sreg,r0
      cli
    end;

    Result:=Target;
    if Target=Comperand then
      Target:=NewValue;

    { release interrupts }
    asm
      ldd r0,temp_sreg
      out 0x3f,r0
    end;
  end;


function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    asm
      in r0,0x3f
      std temp_sreg,r0
      cli
    end;

    Result:=Target;
    inc(Target,Source);

    { release interrupts }
    asm
      ldd r0,temp_sreg
      out 0x3f,r0
    end;
  end;

