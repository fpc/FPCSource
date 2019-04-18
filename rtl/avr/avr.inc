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

const
{$i cpuinnr.inc}

{ Reads SREG and then disables interrupts, returns contents of SREG }
function avr_save: byte;[INTERNPROC: in_avr_save];
{ Restores SREG }
procedure avr_restore(old_sreg: byte); [INTERNPROC: in_avr_restore];


procedure fpc_cpuinit;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
  end;


{$define FPC_SYSTEM_HAS_MOVE}
procedure Move(const source;var dest;count:SizeInt);[public, alias: 'FPC_MOVE'];
var
  pdest,psrc,pend : pbyte;
begin
  if (@dest=@source) or (count<=0) then
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
    end;
end;


{$define FPC_SYSTEM_HAS_FILLCHAR}
Procedure FillChar(var x;count:SizeInt;value:byte);
var
  pdest,pend : pbyte;
  v : ptruint;
begin
  if count <= 0 then
    exit;
  pdest:=@x;
  pend:=pdest+count;
  while pdest<pend do
    begin
      pdest^:=value;
      inc(pdest);
    end;
end;


{$IFNDEF INTERNAL_BACKTRACE}
{$define FPC_SYSTEM_HAS_GET_FRAME}
function get_frame:pointer;assembler;nostackframe;
  asm
  end;
{$ENDIF not INTERNAL_BACKTRACE}


{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:pointer;addr:pointer=nil):pointer;assembler;nostackframe;
  asm
  end;


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer;addr:pointer=nil):pointer;assembler;nostackframe;
  asm
  end;


{$define FPC_SYSTEM_HAS_SPTR}
Function Sptr : pointer;assembler;nostackframe;
  asm
  end;


function InterLockedDecrement (var Target: longint) : longint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    temp_sreg:=avr_save();

    Result:=Target-1;
    Target:=Result;

    { release interrupts }
    avr_restore(temp_sreg);
  end;


function InterLockedIncrement (var Target: longint) : longint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    temp_sreg:=avr_save();

    Result:=Target+1;
    Target:=Result;

    { release interrupts }
    avr_restore(temp_sreg);
  end;


function InterLockedExchange (var Target: longint;Source : longint) : longint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    temp_sreg:=avr_save();

    Result:=Target;
    Target:=Source;

    { release interrupts }
    avr_restore(temp_sreg);
  end;


function InterlockedCompareExchange(var Target: longint; NewValue: longint; Comperand: longint): longint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    temp_sreg:=avr_save();

    Result:=Target;
    if Result=Comperand then
      Target:=NewValue;

    { release interrupts }
    avr_restore(temp_sreg);
  end;


function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    temp_sreg:=avr_save();

    Result:=Target;
    Target:=Result+Source;

    { release interrupts }
    avr_restore(temp_sreg);
  end;


function InterLockedDecrement (var Target: smallint) : smallint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    temp_sreg:=avr_save();

    Result:=Target-1;
    Target:=Result;

    { release interrupts }
    avr_restore(temp_sreg);
  end;


function InterLockedIncrement (var Target: smallint) : smallint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    temp_sreg:=avr_save();

    Result:=Target+1;
    Target:=Result;

    { release interrupts }
    avr_restore(temp_sreg);
  end;


function InterLockedExchange (var Target: smallint;Source : smallint) : smallint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    temp_sreg:=avr_save();

    Result:=Target;
    Target:=Source;

    { release interrupts }
    avr_restore(temp_sreg);
  end;


function InterlockedCompareExchange(var Target: smallint; NewValue: smallint; Comperand: smallint): smallint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    temp_sreg:=avr_save();

    Result:=Target;
    if Result=Comperand then
      Target:=NewValue;

    { release interrupts }
    avr_restore(temp_sreg);
  end;


function InterLockedExchangeAdd (var Target: smallint;Source : smallint) : smallint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    temp_sreg:=avr_save();

    Result:=Target;
    Target:=Result+Source;

    { release interrupts }
    avr_restore(temp_sreg);
  end;

