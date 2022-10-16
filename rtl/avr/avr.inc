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

{$define FPC_SYSTEM_HAS_FPC_CPUINIT}
procedure fpc_cpuinit;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
  end;


{$if not(defined(CPUAVR_16_REGS)) and defined(CPUAVR_HAS_MOVW)}
{$define FPC_SYSTEM_HAS_MOVE}
procedure Move(const source;var dest;count:SizeInt);[public, alias: 'FPC_MOVE']; assembler; nostackframe;
asm
  push r28
  push r29

  movw r26, r24         // Src=X
  movw r28, r22         // Dest=Y
  movw r30, r20         // Count=Z
  cp r1, r30
  cpc r1, r31
  brge .Lexit           // if 0 >= Count
  cp  r28, r26
  cpc r29, r27
  breq .Lexit           // if dest = source
  brlo .LForwardMove    // if dest < source

  // Add count to both pointers
  add r26, r30
  adc r27, r31
  add r28, r30
  adc r29, r31
.LBackwardMove:
  ld r18, -X
  st -Y, r18
  sbiw r30, 1
  brne .LBackwardMove
  rjmp .Lexit

.LForwardMove:
  ld r18, X+
  st Y+, r18
  sbiw r30, 1
  brne .LForwardMove
.Lexit:

  pop r29
  pop r28
end;
{$endif not(defined(CPUAVR_16_REGS)) and defined(CPUAVR_HAS_MOVW)}

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
{ this is never going to work on avr properly this way, so inline and return nil so the compiler
  can optimize it }
function get_frame:pointer;inline;
  begin
    result:=nil;
  end;
{$ENDIF not INTERNAL_BACKTRACE}


{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
{ this is never going to work on avr properly this way, so inline and return nil so the compiler
  can optimize it }
function get_caller_addr(framebp:pointer;addr:pointer=nil):pointer;inline;
  begin
    result:=nil;
  end;


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
{ this is never going to work on avr properly this way, so inline and return nil so the compiler
  can optimize it }
function get_caller_frame(framebp:pointer;addr:pointer=nil):pointer;inline;
  begin
    result:=nil;
  end;


{$define FPC_SYSTEM_HAS_SPTR}
Function Sptr : pointer;assembler;nostackframe;
  asm
    in r24, 0x3d
    in r25, 0x3e
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

{$define FPC_HAS_SHORTSTR_SHORTSTR_INTERN_CHARMOVE}
procedure fpc_shortstr_shortstr_intern_charmove(const src: shortstring; const srcindex: byte; var dst: shortstring; const dstindex, len: byte); {$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    move(src[srcindex],dst[dstindex],len);
  end;

{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_CONCAT_MULTI}
procedure fpc_shortstr_concat_multi(var dests:shortstring;const sarr:array of pshortstring);compilerproc;
  var
    s2l, tmpindex : byte;
    i,
    Len : ObjpasInt;
    relocatedstringindex: byte;
    p : pshortstring;
  begin
    if high(sarr)=0 then
      begin
        DestS:='';
        exit;
      end;
    { for s := s1 + s + ..., relocate s to the correct first position in dests
      and remember this position for possible subsequent occurences of s }
    Len:=1;
    i:=low(sarr);
    while (i<=high(sarr)) and (@dests<>sarr[i]) and (Len<=high(dests)) do
      begin
        Len:=Len+length(sarr[i]^);
        inc(i);
      end;

    if Len<=high(dests) then
      begin
        relocatedstringindex:=Len;
        s2l:=length(dests);
        if uint16(s2l)+uint16(relocatedstringindex) > high(dests) then
          s2l:=high(dests)-relocatedstringindex+1;
        fpc_shortstr_shortstr_intern_charmove(dests,1,dests,relocatedstringindex,s2l);
      end;

    Len:=0;
    for i:=low(sarr) to high(sarr) do
      begin
        p:=sarr[i];
        if assigned(p) then
          begin
            s2l:=length(p^);
            if Len+s2l>high(dests) then
              s2l:=high(dests)-Len;

            { Use relocated string position if src = dests }
            if (p=@dests) then
              tmpindex:=relocatedstringindex
            else
              tmpindex:=1;

            fpc_shortstr_shortstr_intern_charmove(p^,tmpindex,dests,Len+1,s2l);
            inc(Len,s2l);
          end;
      end;
    dests[0]:=Chr(Len);
  end;

{include hand-optimized assembler code}
{$i math.inc}
