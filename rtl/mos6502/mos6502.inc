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


{$ifdef VER3_2}
function InterLockedDecrement (var Target: longint) : longint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_DEC_32}
function fpc_atomic_dec_32 (var Target: longint): longint;
{$endif VER3_2}
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


{$ifdef VER3_2}
function InterLockedIncrement (var Target: longint) : longint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_INC_32}
function fpc_atomic_inc_32 (var Target: longint): longint;
{$endif VER3_2}
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


{$ifdef VER3_2}
function InterLockedExchange (var Target: longint;Source : longint) : longint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_XCHG_32}
function fpc_atomic_xchg_32 (var Target: longint;Source : longint): longint;
{$endif VER3_2}
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


{$ifdef VER3_2}
function InterlockedCompareExchange(var Target: longint; NewValue: longint; Comperand: longint): longint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_CMP_XCHG_32}
function fpc_atomic_cmp_xchg_32 (var Target: longint; NewValue: longint; Comparand: longint): longint;
{$endif VER3_2}
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
    if Target={$ifdef VER3_2}Comperand{$else}Comparand{$endif} then
      Target:=NewValue;

{$ifdef FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
    { release interrupts }
    asm
      cli
    end;
{$endif FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
  end;


{$ifdef VER3_2}
function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_ADD_32}
function fpc_atomic_add_32 (var Target: longint;Value : longint): longint;
{$endif VER3_2}
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
    inc(Target,{$ifdef VER3_2}Source{$else}Value{$endif});

{$ifdef FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
    { release interrupts }
    asm
      cli
    end;
{$endif FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
  end;


{$ifdef VER3_2}
function InterLockedDecrement (var Target: smallint) : smallint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_DEC_16}
function fpc_atomic_dec_16 (var Target: smallint): smallint;
{$endif VER3_2}
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


{$ifdef VER3_2}
function InterLockedIncrement (var Target: smallint) : smallint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_INC_16}
function fpc_atomic_inc_16 (var Target: smallint): smallint;
{$endif VER3_2}
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


{$ifdef VER3_2}
function InterLockedExchange (var Target: smallint;Source : smallint) : smallint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_XCHG_16}
function fpc_atomic_xchg_16 (var Target: smallint;Source : smallint): smallint;
{$endif VER3_2}
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


{$ifdef VER3_2}
function InterlockedCompareExchange(var Target: smallint; NewValue: smallint; Comperand: smallint): smallint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_CMP_XCHG_16}
function fpc_atomic_cmp_xchg_16 (var Target: smallint; NewValue: smallint; Comparand: smallint): smallint; [public,alias:'FPC_ATOMIC_CMP_XCHG_16'];
{$endif VER3_2}
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
    if Target={$ifdef VER3_2}Comperand{$else}Comparand{$endif} then
      Target:=NewValue;

{$ifdef FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
    { release interrupts }
    asm
      cli
    end;
{$endif FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
  end;


{$ifdef VER3_2}
function InterLockedExchangeAdd (var Target: smallint;Source : smallint) : smallint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_ADD_16}
function fpc_atomic_add_16 (var Target: smallint;Value : smallint): smallint;
{$endif VER3_2}
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
    inc(Target,{$ifdef VER3_2}Source{$else}Value{$endif});

{$ifdef FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
    { release interrupts }
    asm
      cli
    end;
{$endif FPC_MOS6502_INTERLOCKED_OPERATIONS_BLOCK_INTERRUPTS}
  end;
