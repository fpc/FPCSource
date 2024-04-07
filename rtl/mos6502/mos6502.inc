{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Processor dependent implementation for the system unit for
    MOS Technology 6502

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF INTERNAL_BACKTRACE}
{$define FPC_SYSTEM_HAS_GET_FRAME}
function get_frame:pointer;
  begin
    get_frame:=nil;
  end;
{$ENDIF not INTERNAL_BACKTRACE}


{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:pointer;addr:pointer=nil):pointer;
  begin
    get_caller_addr:=nil;
  end;


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer;addr:pointer=nil):pointer;
  begin
    get_caller_frame:=nil;
  end;


{$define FPC_SYSTEM_HAS_SPTR}
Function Sptr : pointer;
  begin
    Sptr:=nil;
  end;


function InterLockedDecrement (var Target: longint) : longint;
  var
    temp_sreg : byte;
  begin
{$ifdef FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
    { block interrupts }
    asm
      sei
    end;
{$endif FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}

    dec(Target);
    Result:=Target;

{$ifdef FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
    { release interrupts }
    asm
      cli
    end;
{$endif FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
  end;


function InterLockedIncrement (var Target: longint) : longint;
  var
    temp_sreg : byte;
  begin
{$ifdef FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
    { block interrupts }
    asm
      sei
    end;
{$endif FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}

    inc(Target);
    Result:=Target;

{$ifdef FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
    { release interrupts }
    asm
      cli
    end;
{$endif FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
  end;


function InterLockedExchange (var Target: longint;Source : longint) : longint;
  var
    temp_sreg : byte;
  begin
{$ifdef FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
    { block interrupts }
    asm
      sei
    end;
{$endif FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}

    Result:=Target;
    Target:=Source;

{$ifdef FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
    { release interrupts }
    asm
      cli
    end;
{$endif FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
  end;


function InterlockedCompareExchange(var Target: longint; NewValue: longint; Comperand: longint): longint;
  var
    temp_sreg : byte;
  begin
{$ifdef FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
    { block interrupts }
    asm
      sei
    end;
{$endif FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}

    Result:=Target;
    if Target=Comperand then
      Target:=NewValue;

{$ifdef FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
    { release interrupts }
    asm
      cli
    end;
{$endif FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
  end;


function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint;
  var
    temp_sreg : byte;
  begin
{$ifdef FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
    { block interrupts }
    asm
      sei
    end;
{$endif FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}

    Result:=Target;
    inc(Target,Source);

{$ifdef FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
    { release interrupts }
    asm
      cli
    end;
{$endif FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
  end;


function InterLockedDecrement (var Target: smallint) : smallint;
  var
    temp_sreg : byte;
  begin
{$ifdef FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
    { block interrupts }
    asm
      sei
    end;
{$endif FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}

    dec(Target);
    Result:=Target;

{$ifdef FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
    { release interrupts }
    asm
      cli
    end;
{$endif FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
  end;


function InterLockedIncrement (var Target: smallint) : smallint;
  var
    temp_sreg : byte;
  begin
{$ifdef FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
    { block interrupts }
    asm
      sei
    end;
{$endif FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}

    inc(Target);
    Result:=Target;

{$ifdef FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
    { release interrupts }
    asm
      cli
    end;
{$endif FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
  end;


function InterLockedExchange (var Target: smallint;Source : smallint) : smallint;
  var
    temp_sreg : byte;
  begin
{$ifdef FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
    { block interrupts }
    asm
      sei
    end;
{$endif FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}

    Result:=Target;
    Target:=Source;

{$ifdef FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
    { release interrupts }
    asm
      cli
    end;
{$endif FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
  end;


function InterlockedCompareExchange(var Target: smallint; NewValue: smallint; Comperand: smallint): smallint;
  var
    temp_sreg : byte;
  begin
{$ifdef FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
    { block interrupts }
    asm
      sei
    end;
{$endif FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}

    Result:=Target;
    if Target=Comperand then
      Target:=NewValue;

{$ifdef FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
    { release interrupts }
    asm
      cli
    end;
{$endif FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
  end;


function InterLockedExchangeAdd (var Target: smallint;Source : smallint) : smallint;
  var
    temp_sreg : byte;
  begin
{$ifdef FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
    { block interrupts }
    asm
      sei
    end;
{$endif FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}

    Result:=Target;
    inc(Target,Source);

{$ifdef FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
    { release interrupts }
    asm
      cli
    end;
{$endif FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
  end;
