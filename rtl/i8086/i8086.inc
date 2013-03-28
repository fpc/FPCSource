{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2013 by the Free Pascal development team.

    Processor dependent implementation for the system unit for
    intel i8086+

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure fpc_cpuinit;
  begin
  end;

{$define FPC_SYSTEM_HAS_SPTR}
Function Sptr : Pointer;assembler;nostackframe;
asm
  mov ax, sp
end;

{$define FPC_SYSTEM_HAS_CSEG}
function CSeg: Word;{$ifdef SYSTEMINLINE}inline;{$endif}assembler;nostackframe;
asm
  mov ax, cs
end;

{$define FPC_SYSTEM_HAS_DSEG}
function DSeg: Word;{$ifdef SYSTEMINLINE}inline;{$endif}assembler;nostackframe;
asm
  mov ax, ds
end;

{$define FPC_SYSTEM_HAS_SSEG}
function SSeg: Word;{$ifdef SYSTEMINLINE}inline;{$endif}assembler;nostackframe;
asm
  mov ax, ss
end;

{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:pointer;addr:pointer=nil):pointer;nostackframe;assembler;
asm
  or ax, ax
  jz @@Lg_a_null
  xchg ax, bx
  mov bx, [bx+2]
  xchg ax, bx
@@Lg_a_null:
end;

{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer;addr:pointer=nil):pointer;nostackframe;assembler;
asm
  or ax, ax
  jz @@Lgnf_null
  xchg ax, bx
  mov bx, [bx]
  xchg ax, bx
@@Lgnf_null:
end;

{TODO: fix, use smallint?}
function InterLockedDecrement (var Target: longint) : longint; assembler;
asm
  push bx
  mov bx, ax
  sub word [bx], 1
  sbb word [bx+2], 0
  mov ax, [bx]
  mov dx, [bx+2]
  pop bx
end;

{TODO: fix, use smallint?}
function InterLockedIncrement (var Target: longint) : longint; assembler;
asm
  push bx
  mov bx, ax
  add word [bx], 1
  adc word [bx+2], 0
  mov ax, [bx]
  mov dx, [bx+2]
  pop bx
end;

{TODO: fix, use smallint?}
function InterLockedExchange (var Target: longint;Source : longint) : longint;
begin
  InterLockedExchange := Target;
  Target := Source;
end;

{TODO: implement}
function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint;
begin
  runerror(304);
end;

{TODO: implement}
function InterlockedCompareExchange(var Target: longint; NewValue: longint; Comperand: longint): longint;
begin
  runerror(304);
end;

