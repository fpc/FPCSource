{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2017 by the Free Pascal development team.

    Processor dependent implementation for the system unit for
    Z80

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


var
  z80_save_hl: Word; public name 'FPC_Z80_SAVE_HL';

{$define FPC_SYSTEM_HAS_FPC_CPUINIT}
procedure fpc_cpuinit;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
  end;


{$define FPC_SYSTEM_HAS_MOVE}
procedure Move(const source;var dest;count:SizeInt);assembler;[public, alias: 'FPC_MOVE'];
label
  skip, forward_move;
asm
  ld c, (count)
  ld b, (count+1)
  bit 7, b
  jp NZ, skip
  ld a, b
  or a, c
  jp Z, skip

  ld l, (source)
  ld h, (source+1)
  ld e, (dest)
  ld d, (dest+1)

  ld a, d
  cp a, h
  jp C, forward_move
  ld a, e
  cp a, l
  jp C, forward_move

  { backward move }
  add hl, bc
  dec hl
  ex de, hl
  add hl, bc
  dec hl
  ex de, hl
  lddr
  jp skip

forward_move:
  ldir

skip:
end;


{$define FPC_SYSTEM_HAS_FILLCHAR}
Procedure FillChar(var x;count:SizeInt;value:byte);assembler;
label
  skip, loop;
asm
  ld c, (count)
  ld b, (count+1)
  bit 7, b
  jp NZ, skip

  ld a, b
  or a, c
  jp Z, skip

  ld e, (value)
  ld a, 0
  ld l, (x)
  ld h, (x+1)
loop:
  ld (hl), e
  inc hl
  dec bc
  cp a, c
  jp NZ, loop
  cp a, b
  jp NZ, loop
skip:
end;


{$IFNDEF INTERNAL_BACKTRACE}
{$define FPC_SYSTEM_HAS_GET_FRAME}
function get_frame:pointer;assembler;nostackframe;
  asm
    push ix
    pop hl
  end;
{$ENDIF not INTERNAL_BACKTRACE}


{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:pointer;addr:pointer=nil):pointer;assembler;
  label
    framebp_null;
  asm
    ld l, (framebp)
    ld h, (framebp+1)

    ld a, l
    or a, h
    jp Z, framebp_null

    inc hl
    inc hl

    ld e, (hl)
    inc hl
    ld d, (hl)
    ex de, hl
framebp_null:
  end;


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer;addr:pointer=nil):pointer;assembler;
  label
    framebp_null;
  asm
    ld l, (framebp)
    ld h, (framebp+1)

    ld a, l
    or a, h
    jp Z, framebp_null

    ld e, (hl)
    inc hl
    ld d, (hl)
    ex de, hl
framebp_null:
  end;


{$define FPC_SYSTEM_HAS_SPTR}
Function Sptr : pointer;assembler;nostackframe;
  asm
    ld hl, 0
    add hl, sp
  end;


{$ifdef VER3_2}
function InterLockedDecrement (var Target: longint) : longint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_DEC_32}
function fpc_atomic_dec_32 (var Target: longint): longint;
{$endif VER3_2}
  begin
    { block interrupts }
    asm
      di
    end;

    dec(Target);
    Result:=Target;

    { release interrupts }
    asm
      ei
    end;
  end;


{$ifdef VER3_2}
function InterLockedIncrement (var Target: longint) : longint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_INC_32}
function fpc_atomic_inc_32 (var Target: longint): longint;
{$endif VER3_2}
  begin
    { block interrupts }
    asm
      di
    end;

    inc(Target);
    Result:=Target;

    { release interrupts }
    asm
      ei
    end;
  end;


{$ifdef VER3_2}
function InterLockedExchange (var Target: longint;Source : longint) : longint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_XCHG_32}
function fpc_atomic_xchg_32 (var Target: longint;Source : longint): longint;
{$endif VER3_2}
  begin
    { block interrupts }
    asm
      di
    end;

    Result:=Target;
    Target:=Source;

    { release interrupts }
    asm
      ei
    end;
  end;


{$ifdef VER3_2}
function InterlockedCompareExchange(var Target: longint; NewValue: longint; Comperand: longint): longint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_CMP_XCHG_32}
function fpc_atomic_cmp_xchg_32 (var Target: longint; NewValue: longint; Comparand: longint): longint;
{$endif VER3_2}
  begin
    { block interrupts }
    asm
      di
    end;

    Result:=Target;
    if Target={$ifdef VER3_2}Comperand{$else}Comparand{$endif} then
      Target:=NewValue;

    { release interrupts }
    asm
      ei
    end;
  end;


{$ifdef VER3_2}
function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_ADD_32}
function fpc_atomic_add_32 (var Target: longint;Value : longint): longint;
{$endif VER3_2}
  begin
    { block interrupts }
    asm
      di
    end;

    Result:=Target;
    inc(Target,{$ifdef VER3_2}Source{$else}Value{$endif});

    { release interrupts }
    asm
      ei
    end;
  end;


{$ifdef VER3_2}
function InterLockedDecrement (var Target: smallint) : smallint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_DEC_16}
function fpc_atomic_dec_16 (var Target: smallint): smallint;
{$endif VER3_2}
  begin
    { block interrupts }
    asm
      di
    end;

    dec(Target);
    Result:=Target;

    { release interrupts }
    asm
      ei
    end;
  end;


{$ifdef VER3_2}
function InterLockedIncrement (var Target: smallint) : smallint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_INC_16}
function fpc_atomic_inc_16 (var Target: smallint): smallint;
{$endif VER3_2}
  begin
    { block interrupts }
    asm
      di
    end;

    inc(Target);
    Result:=Target;

    { release interrupts }
    asm
      ei
    end;
  end;


{$ifdef VER3_2}
function InterLockedExchange (var Target: smallint;Source : smallint) : smallint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_XCHG_16}
function fpc_atomic_xchg_16 (var Target: smallint;Source : smallint): smallint;
{$endif VER3_2}
  begin
    { block interrupts }
    asm
      di
    end;

    Result:=Target;
    Target:=Source;

    { release interrupts }
    asm
      ei
    end;
  end;


{$ifdef VER3_2}
function InterlockedCompareExchange(var Target: smallint; NewValue: smallint; Comperand: smallint): smallint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_CMP_XCHG_16}
function fpc_atomic_cmp_xchg_16 (var Target: smallint; NewValue: smallint; Comparand: smallint): smallint; [public,alias:'FPC_ATOMIC_CMP_XCHG_16'];
{$endif VER3_2}
  begin
    { block interrupts }
    asm
      di
    end;

    Result:=Target;
    if Target={$ifdef VER3_2}Comperand{$else}Comparand{$endif} then
      Target:=NewValue;

    { release interrupts }
    asm
      ei
    end;
  end;


{$ifdef VER3_2}
function InterLockedExchangeAdd (var Target: smallint;Source : smallint) : smallint;
{$else VER3_2}
{$define FPC_SYSTEM_HAS_ATOMIC_ADD_16}
function fpc_atomic_add_16 (var Target: smallint;Value : smallint): smallint;
{$endif VER3_2}
  begin
    { block interrupts }
    asm
      di
    end;

    Result:=Target;
    inc(Target,{$ifdef VER3_2}Source{$else}Value{$endif});

    { release interrupts }
    asm
      ei
    end;
  end;
