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

procedure fpc_cpuinit;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
  end;


{$define FPC_SYSTEM_HAS_MOVE}
procedure Move(const source;var dest;count:SizeInt);[public, alias: 'FPC_MOVE'];
{var
  pdest,psrc,pend : pbyte;}
begin
(*  if (@dest=@source) or (count<=0) then
    exit;
  if (@dest<@source) or (@source+count<@dest) then
    begin
      { Forward Move }
      psrc:=@source;
      pdest:=@dest;
      pend:=psrc+count;
      while psrc<pend do
        begin
          pdest^:=psrc^;
          inc(pdest);
          inc(psrc);
        end;
    end
  else
    begin
      { Backward Move }
      psrc:=@source+count;
      pdest:=@dest+count;
      while psrc>@source do
        begin
          dec(pdest);
          dec(psrc);
          pdest^:=psrc^;
        end;
    end;*)
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
  asm
  end;


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer;addr:pointer=nil):pointer;assembler;
  asm
  end;


{$define FPC_SYSTEM_HAS_SPTR}
Function Sptr : pointer;assembler;nostackframe;
  asm
    ld hl, 0
    add hl, sp
  end;


function InterLockedDecrement (var Target: longint) : longint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    asm
    end;

    dec(Target);
    Result:=Target;

    { release interrupts }
    asm
    end;
  end;


function InterLockedIncrement (var Target: longint) : longint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    asm
    end;

    inc(Target);
    Result:=Target;

    { release interrupts }
    asm
    end;
  end;


function InterLockedExchange (var Target: longint;Source : longint) : longint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    asm
    end;

    Result:=Target;
    Target:=Source;

    { release interrupts }
    asm
    end;
  end;


function InterlockedCompareExchange(var Target: longint; NewValue: longint; Comperand: longint): longint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    asm
    end;

    Result:=Target;
//    if Target=Comperand then
//      Target:=NewValue;

    { release interrupts }
    asm
    end;
  end;


function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    asm
    end;

    Result:=Target;
    inc(Target,Source);

    { release interrupts }
    asm
    end;
  end;


function InterLockedDecrement (var Target: smallint) : smallint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    asm
    end;

    dec(Target);
    Result:=Target;

    { release interrupts }
    asm
    end;
  end;


function InterLockedIncrement (var Target: smallint) : smallint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    asm
    end;

    inc(Target);
    Result:=Target;

    { release interrupts }
    asm
    end;
  end;


function InterLockedExchange (var Target: smallint;Source : smallint) : smallint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    asm
    end;

    Result:=Target;
    Target:=Source;

    { release interrupts }
    asm
    end;
  end;


function InterlockedCompareExchange(var Target: smallint; NewValue: smallint; Comperand: smallint): smallint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    asm
    end;

    Result:=Target;
//    if Target=Comperand then
//      Target:=NewValue;

    { release interrupts }
    asm
    end;
  end;


function InterLockedExchangeAdd (var Target: smallint;Source : smallint) : smallint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    asm
    end;

    Result:=Target;
    inc(Target,Source);

    { release interrupts }
    asm
    end;
  end;
