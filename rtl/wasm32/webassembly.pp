{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2022 by Nikolay Nikolov

    This unit contains some WebAssembly-specific routines

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit WebAssembly;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}

interface

const
  { Special values for the TimeoutNanoseconds parameter of AtomicWait }
  awtInfiniteTimeout = -1;
  { AtomicWait result values }
  awrOk = 0;       { woken by another agent in the cluster }
  awrNotEqual = 1; { the loaded value did not match the expected value }
  awrTimedOut = 2; { not woken before timeout expired }

procedure AtomicFence; inline;

function AtomicLoad(constref Mem: Int8): Int8; inline;
function AtomicLoad(constref Mem: UInt8): UInt8; inline;
function AtomicLoad(constref Mem: Int16): Int16; inline;
function AtomicLoad(constref Mem: UInt16): UInt16; inline;
function AtomicLoad(constref Mem: Int32): Int32; inline;
function AtomicLoad(constref Mem: UInt32): UInt32; inline;
function AtomicLoad(constref Mem: Int64): Int64; inline;
function AtomicLoad(constref Mem: UInt64): UInt64; inline;

procedure AtomicStore(out Mem: Int8; Data: Int8); inline;
procedure AtomicStore(out Mem: UInt8; Data: UInt8); inline;
procedure AtomicStore(out Mem: Int16; Data: Int16); inline;
procedure AtomicStore(out Mem: UInt16; Data: UInt16); inline;
procedure AtomicStore(out Mem: Int32; Data: Int32); inline;
procedure AtomicStore(out Mem: UInt32; Data: UInt32); inline;
procedure AtomicStore(out Mem: Int64; Data: Int64); inline;
procedure AtomicStore(out Mem: UInt64; Data: UInt64); inline;

function AtomicAdd(var Mem: Int8; Data: Int8): Int8; inline;
function AtomicAdd(var Mem: UInt8; Data: UInt8): UInt8; inline;
function AtomicAdd(var Mem: Int16; Data: Int16): Int16; inline;
function AtomicAdd(var Mem: UInt16; Data: UInt16): UInt16; inline;
function AtomicAdd(var Mem: Int32; Data: Int32): Int32; inline;
function AtomicAdd(var Mem: UInt32; Data: UInt32): UInt32; inline;
function AtomicAdd(var Mem: Int64; Data: Int64): Int64; inline;
function AtomicAdd(var Mem: UInt64; Data: UInt64): UInt64; inline;

function AtomicSub(var Mem: Int8; Data: Int8): Int8; inline;
function AtomicSub(var Mem: UInt8; Data: UInt8): UInt8; inline;
function AtomicSub(var Mem: Int16; Data: Int16): Int16; inline;
function AtomicSub(var Mem: UInt16; Data: UInt16): UInt16; inline;
function AtomicSub(var Mem: Int32; Data: Int32): Int32; inline;
function AtomicSub(var Mem: UInt32; Data: UInt32): UInt32; inline;
function AtomicSub(var Mem: Int64; Data: Int64): Int64; inline;
function AtomicSub(var Mem: UInt64; Data: UInt64): UInt64; inline;

function AtomicAnd(var Mem: Int8; Data: Int8): Int8; inline;
function AtomicAnd(var Mem: UInt8; Data: UInt8): UInt8; inline;
function AtomicAnd(var Mem: Int16; Data: Int16): Int16; inline;
function AtomicAnd(var Mem: UInt16; Data: UInt16): UInt16; inline;
function AtomicAnd(var Mem: Int32; Data: Int32): Int32; inline;
function AtomicAnd(var Mem: UInt32; Data: UInt32): UInt32; inline;
function AtomicAnd(var Mem: Int64; Data: Int64): Int64; inline;
function AtomicAnd(var Mem: UInt64; Data: UInt64): UInt64; inline;

function AtomicOr(var Mem: Int8; Data: Int8): Int8; inline;
function AtomicOr(var Mem: UInt8; Data: UInt8): UInt8; inline;
function AtomicOr(var Mem: Int16; Data: Int16): Int16; inline;
function AtomicOr(var Mem: UInt16; Data: UInt16): UInt16; inline;
function AtomicOr(var Mem: Int32; Data: Int32): Int32; inline;
function AtomicOr(var Mem: UInt32; Data: UInt32): UInt32; inline;
function AtomicOr(var Mem: Int64; Data: Int64): Int64; inline;
function AtomicOr(var Mem: UInt64; Data: UInt64): UInt64; inline;

function AtomicXor(var Mem: Int8; Data: Int8): Int8; inline;
function AtomicXor(var Mem: UInt8; Data: UInt8): UInt8; inline;
function AtomicXor(var Mem: Int16; Data: Int16): Int16; inline;
function AtomicXor(var Mem: UInt16; Data: UInt16): UInt16; inline;
function AtomicXor(var Mem: Int32; Data: Int32): Int32; inline;
function AtomicXor(var Mem: UInt32; Data: UInt32): UInt32; inline;
function AtomicXor(var Mem: Int64; Data: Int64): Int64; inline;
function AtomicXor(var Mem: UInt64; Data: UInt64): UInt64; inline;

function AtomicExchange(var Mem: Int8; Data: Int8): Int8; inline;
function AtomicExchange(var Mem: UInt8; Data: UInt8): UInt8; inline;
function AtomicExchange(var Mem: Int16; Data: Int16): Int16; inline;
function AtomicExchange(var Mem: UInt16; Data: UInt16): UInt16; inline;
function AtomicExchange(var Mem: Int32; Data: Int32): Int32; inline;
function AtomicExchange(var Mem: UInt32; Data: UInt32): UInt32; inline;
function AtomicExchange(var Mem: Int64; Data: Int64): Int64; inline;
function AtomicExchange(var Mem: UInt64; Data: UInt64): UInt64; inline;

function AtomicCompareExchange(var Mem: Int8; Compare, Data: Int8): Int8; inline;
function AtomicCompareExchange(var Mem: UInt8; Compare, Data: UInt8): UInt8; inline;
function AtomicCompareExchange(var Mem: Int16; Compare, Data: Int16): Int16; inline;
function AtomicCompareExchange(var Mem: UInt16; Compare, Data: UInt16): UInt16; inline;
function AtomicCompareExchange(var Mem: Int32; Compare, Data: Int32): Int32; inline;
function AtomicCompareExchange(var Mem: UInt32; Compare, Data: UInt32): UInt32; inline;
function AtomicCompareExchange(var Mem: Int64; Compare, Data: Int64): Int64; inline;
function AtomicCompareExchange(var Mem: UInt64; Compare, Data: UInt64): UInt64; inline;

function AtomicWait(constref Mem: Int32; Compare: Int32; TimeoutNanoseconds: Int64): Int32; inline;
function AtomicWait(constref Mem: UInt32; Compare: UInt32; TimeoutNanoseconds: Int64): Int32; inline;
function AtomicWait(constref Mem: Int64; Compare: Int64; TimeoutNanoseconds: Int64): Int32; inline;
function AtomicWait(constref Mem: UInt64; Compare: UInt64; TimeoutNanoseconds: Int64): Int32; inline;

function AtomicNotify(constref Mem: Int32; Count: UInt32): UInt32; inline;
function AtomicNotify(constref Mem: UInt32; Count: UInt32): UInt32; inline;
function AtomicNotify(constref Mem: Int64; Count: UInt32): UInt32; inline;
function AtomicNotify(constref Mem: UInt64; Count: UInt32): UInt32; inline;

implementation

{$I cpuh.inc}

procedure AtomicFence; inline;
begin
{$ifdef FPC_WASM_THREADS}
  fpc_wasm32_atomic_fence;
{$endif FPC_WASM_THREADS}
end;

function AtomicLoad(constref Mem: Int8): Int8; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicLoad:=Int8(fpc_wasm32_i32_atomic_load8_u(@Mem));
{$else FPC_WASM_THREADS}
  AtomicLoad:=Mem;
{$endif FPC_WASM_THREADS}
end;

function AtomicLoad(constref Mem: UInt8): UInt8; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicLoad:=UInt8(fpc_wasm32_i32_atomic_load8_u(@Mem));
{$else FPC_WASM_THREADS}
  AtomicLoad:=Mem;
{$endif FPC_WASM_THREADS}
end;

function AtomicLoad(constref Mem: Int16): Int16; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicLoad:=Int16(fpc_wasm32_i32_atomic_load16_u(@Mem));
{$else FPC_WASM_THREADS}
  AtomicLoad:=Mem;
{$endif FPC_WASM_THREADS}
end;

function AtomicLoad(constref Mem: UInt16): UInt16; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicLoad:=UInt16(fpc_wasm32_i32_atomic_load16_u(@Mem));
{$else FPC_WASM_THREADS}
  AtomicLoad:=Mem;
{$endif FPC_WASM_THREADS}
end;

function AtomicLoad(constref Mem: Int32): Int32; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicLoad:=Int32(fpc_wasm32_i32_atomic_load(@Mem));
{$else FPC_WASM_THREADS}
  AtomicLoad:=Mem;
{$endif FPC_WASM_THREADS}
end;

function AtomicLoad(constref Mem: UInt32): UInt32; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicLoad:=UInt32(fpc_wasm32_i32_atomic_load(@Mem));
{$else FPC_WASM_THREADS}
  AtomicLoad:=Mem;
{$endif FPC_WASM_THREADS}
end;

function AtomicLoad(constref Mem: Int64): Int64; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicLoad:=Int64(fpc_wasm32_i64_atomic_load(@Mem));
{$else FPC_WASM_THREADS}
  AtomicLoad:=Mem;
{$endif FPC_WASM_THREADS}
end;

function AtomicLoad(constref Mem: UInt64): UInt64; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicLoad:=UInt64(fpc_wasm32_i64_atomic_load(@Mem));
{$else FPC_WASM_THREADS}
  AtomicLoad:=Mem;
{$endif FPC_WASM_THREADS}
end;

procedure AtomicStore(out Mem: Int8; Data: Int8); inline;
begin
{$ifdef FPC_WASM_THREADS}
  fpc_wasm32_i32_atomic_store8(@Mem,Byte(Data));
{$else FPC_WASM_THREADS}
  Mem:=Data;
{$endif FPC_WASM_THREADS}
end;

procedure AtomicStore(out Mem: UInt8; Data: UInt8); inline;
begin
{$ifdef FPC_WASM_THREADS}
  fpc_wasm32_i32_atomic_store8(@Mem,Data);
{$else FPC_WASM_THREADS}
  Mem:=Data;
{$endif FPC_WASM_THREADS}
end;

procedure AtomicStore(out Mem: Int16; Data: Int16); inline;
begin
{$ifdef FPC_WASM_THREADS}
  fpc_wasm32_i32_atomic_store16(@Mem,Word(Data));
{$else FPC_WASM_THREADS}
  Mem:=Data;
{$endif FPC_WASM_THREADS}
end;

procedure AtomicStore(out Mem: UInt16; Data: UInt16); inline;
begin
{$ifdef FPC_WASM_THREADS}
  fpc_wasm32_i32_atomic_store16(@Mem,Data);
{$else FPC_WASM_THREADS}
  Mem:=Data;
{$endif FPC_WASM_THREADS}
end;

procedure AtomicStore(out Mem: Int32; Data: Int32); inline;
begin
{$ifdef FPC_WASM_THREADS}
  fpc_wasm32_i32_atomic_store(@Mem,LongWord(Data));
{$else FPC_WASM_THREADS}
  Mem:=Data;
{$endif FPC_WASM_THREADS}
end;

procedure AtomicStore(out Mem: UInt32; Data: UInt32); inline;
begin
{$ifdef FPC_WASM_THREADS}
  fpc_wasm32_i32_atomic_store(@Mem,Data);
{$else FPC_WASM_THREADS}
  Mem:=Data;
{$endif FPC_WASM_THREADS}
end;

procedure AtomicStore(out Mem: Int64; Data: Int64); inline;
begin
{$ifdef FPC_WASM_THREADS}
  fpc_wasm32_i64_atomic_store(@Mem,QWord(Data));
{$else FPC_WASM_THREADS}
  Mem:=Data;
{$endif FPC_WASM_THREADS}
end;

procedure AtomicStore(out Mem: UInt64; Data: UInt64); inline;
begin
{$ifdef FPC_WASM_THREADS}
  fpc_wasm32_i64_atomic_store(@Mem,Data);
{$else FPC_WASM_THREADS}
  Mem:=Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicAdd(var Mem: Int8; Data: Int8): Int8; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicAdd:=Int8(fpc_wasm32_i32_atomic_rmw8_add_u(@Mem,Byte(Data)));
{$else FPC_WASM_THREADS}
  {$push}{$Q-,R-}
  AtomicAdd:=Mem;
  Inc(Mem,Data);
  {$pop}
{$endif FPC_WASM_THREADS}
end;

function AtomicAdd(var Mem: UInt8; Data: UInt8): UInt8; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicAdd:=UInt8(fpc_wasm32_i32_atomic_rmw8_add_u(@Mem,Data));
{$else FPC_WASM_THREADS}
  {$push}{$Q-,R-}
  AtomicAdd:=Mem;
  Inc(Mem,Data);
  {$pop}
{$endif FPC_WASM_THREADS}
end;

function AtomicAdd(var Mem: Int16; Data: Int16): Int16; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicAdd:=Int16(fpc_wasm32_i32_atomic_rmw16_add_u(@Mem,Word(Data)));
{$else FPC_WASM_THREADS}
  {$push}{$Q-,R-}
  AtomicAdd:=Mem;
  Inc(Mem,Data);
  {$pop}
{$endif FPC_WASM_THREADS}
end;

function AtomicAdd(var Mem: UInt16; Data: UInt16): UInt16; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicAdd:=UInt16(fpc_wasm32_i32_atomic_rmw16_add_u(@Mem,Data));
{$else FPC_WASM_THREADS}
  {$push}{$Q-,R-}
  AtomicAdd:=Mem;
  Inc(Mem,Data);
  {$pop}
{$endif FPC_WASM_THREADS}
end;

function AtomicAdd(var Mem: Int32; Data: Int32): Int32; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicAdd:=Int32(fpc_wasm32_i32_atomic_rmw_add(@Mem,LongWord(Data)));
{$else FPC_WASM_THREADS}
  {$push}{$Q-,R-}
  AtomicAdd:=Mem;
  Inc(Mem,Data);
  {$pop}
{$endif FPC_WASM_THREADS}
end;

function AtomicAdd(var Mem: UInt32; Data: UInt32): UInt32; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicAdd:=fpc_wasm32_i32_atomic_rmw_add(@Mem,Data);
{$else FPC_WASM_THREADS}
  {$push}{$Q-,R-}
  AtomicAdd:=Mem;
  Inc(Mem,Data);
  {$pop}
{$endif FPC_WASM_THREADS}
end;

function AtomicAdd(var Mem: Int64; Data: Int64): Int64; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicAdd:=Int64(fpc_wasm32_i64_atomic_rmw_add(@Mem,QWord(Data)));
{$else FPC_WASM_THREADS}
  {$push}{$Q-,R-}
  AtomicAdd:=Mem;
  Inc(Mem,Data);
  {$pop}
{$endif FPC_WASM_THREADS}
end;

function AtomicAdd(var Mem: UInt64; Data: UInt64): UInt64; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicAdd:=fpc_wasm32_i64_atomic_rmw_add(@Mem,Data);
{$else FPC_WASM_THREADS}
  {$push}{$Q-,R-}
  AtomicAdd:=Mem;
  Inc(Mem,Data);
  {$pop}
{$endif FPC_WASM_THREADS}
end;

function AtomicSub(var Mem: Int8; Data: Int8): Int8; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicSub:=Int8(fpc_wasm32_i32_atomic_rmw8_sub_u(@Mem,Byte(Data)));
{$else FPC_WASM_THREADS}
  {$push}{$Q-,R-}
  AtomicSub:=Mem;
  Dec(Mem,Data);
  {$pop}
{$endif FPC_WASM_THREADS}
end;

function AtomicSub(var Mem: UInt8; Data: UInt8): UInt8; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicSub:=UInt8(fpc_wasm32_i32_atomic_rmw8_sub_u(@Mem,Data));
{$else FPC_WASM_THREADS}
  {$push}{$Q-,R-}
  AtomicSub:=Mem;
  Dec(Mem,Data);
  {$pop}
{$endif FPC_WASM_THREADS}
end;

function AtomicSub(var Mem: Int16; Data: Int16): Int16; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicSub:=Int16(fpc_wasm32_i32_atomic_rmw16_sub_u(@Mem,Word(Data)));
{$else FPC_WASM_THREADS}
  {$push}{$Q-,R-}
  AtomicSub:=Mem;
  Dec(Mem,Data);
  {$pop}
{$endif FPC_WASM_THREADS}
end;

function AtomicSub(var Mem: UInt16; Data: UInt16): UInt16; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicSub:=UInt16(fpc_wasm32_i32_atomic_rmw16_sub_u(@Mem,Data));
{$else FPC_WASM_THREADS}
  {$push}{$Q-,R-}
  AtomicSub:=Mem;
  Dec(Mem,Data);
  {$pop}
{$endif FPC_WASM_THREADS}
end;

function AtomicSub(var Mem: Int32; Data: Int32): Int32; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicSub:=Int32(fpc_wasm32_i32_atomic_rmw_sub(@Mem,LongWord(Data)));
{$else FPC_WASM_THREADS}
  {$push}{$Q-,R-}
  AtomicSub:=Mem;
  Dec(Mem,Data);
  {$pop}
{$endif FPC_WASM_THREADS}
end;

function AtomicSub(var Mem: UInt32; Data: UInt32): UInt32; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicSub:=fpc_wasm32_i32_atomic_rmw_sub(@Mem,Data);
{$else FPC_WASM_THREADS}
  {$push}{$Q-,R-}
  AtomicSub:=Mem;
  Dec(Mem,Data);
  {$pop}
{$endif FPC_WASM_THREADS}
end;

function AtomicSub(var Mem: Int64; Data: Int64): Int64; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicSub:=Int64(fpc_wasm32_i64_atomic_rmw_sub(@Mem,QWord(Data)));
{$else FPC_WASM_THREADS}
  {$push}{$Q-,R-}
  AtomicSub:=Mem;
  Dec(Mem,Data);
  {$pop}
{$endif FPC_WASM_THREADS}
end;

function AtomicSub(var Mem: UInt64; Data: UInt64): UInt64; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicSub:=fpc_wasm32_i64_atomic_rmw_sub(@Mem,Data);
{$else FPC_WASM_THREADS}
  {$push}{$Q-,R-}
  AtomicSub:=Mem;
  Dec(Mem,Data);
  {$pop}
{$endif FPC_WASM_THREADS}
end;

function AtomicAnd(var Mem: Int8; Data: Int8): Int8; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicAnd:=Int8(fpc_wasm32_i32_atomic_rmw8_and_u(@Mem,Byte(Data)));
{$else FPC_WASM_THREADS}
  AtomicAnd:=Mem;
  Mem:=Mem and Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicAnd(var Mem: UInt8; Data: UInt8): UInt8; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicAnd:=UInt8(fpc_wasm32_i32_atomic_rmw8_and_u(@Mem,Data));
{$else FPC_WASM_THREADS}
  AtomicAnd:=Mem;
  Mem:=Mem and Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicAnd(var Mem: Int16; Data: Int16): Int16; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicAnd:=Int16(fpc_wasm32_i32_atomic_rmw16_and_u(@Mem,Word(Data)));
{$else FPC_WASM_THREADS}
  AtomicAnd:=Mem;
  Mem:=Mem and Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicAnd(var Mem: UInt16; Data: UInt16): UInt16; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicAnd:=UInt16(fpc_wasm32_i32_atomic_rmw16_and_u(@Mem,Data));
{$else FPC_WASM_THREADS}
  AtomicAnd:=Mem;
  Mem:=Mem and Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicAnd(var Mem: Int32; Data: Int32): Int32; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicAnd:=Int32(fpc_wasm32_i32_atomic_rmw_and(@Mem,LongWord(Data)));
{$else FPC_WASM_THREADS}
  AtomicAnd:=Mem;
  Mem:=Mem and Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicAnd(var Mem: UInt32; Data: UInt32): UInt32; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicAnd:=fpc_wasm32_i32_atomic_rmw_and(@Mem,Data);
{$else FPC_WASM_THREADS}
  AtomicAnd:=Mem;
  Mem:=Mem and Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicAnd(var Mem: Int64; Data: Int64): Int64; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicAnd:=Int64(fpc_wasm32_i64_atomic_rmw_and(@Mem,QWord(Data)));
{$else FPC_WASM_THREADS}
  AtomicAnd:=Mem;
  Mem:=Mem and Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicAnd(var Mem: UInt64; Data: UInt64): UInt64; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicAnd:=fpc_wasm32_i64_atomic_rmw_and(@Mem,Data);
{$else FPC_WASM_THREADS}
  AtomicAnd:=Mem;
  Mem:=Mem and Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicOr(var Mem: Int8; Data: Int8): Int8; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicOr:=Int8(fpc_wasm32_i32_atomic_rmw8_or_u(@Mem,Byte(Data)));
{$else FPC_WASM_THREADS}
  AtomicOr:=Mem;
  Mem:=Mem or Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicOr(var Mem: UInt8; Data: UInt8): UInt8; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicOr:=UInt8(fpc_wasm32_i32_atomic_rmw8_or_u(@Mem,Data));
{$else FPC_WASM_THREADS}
  AtomicOr:=Mem;
  Mem:=Mem or Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicOr(var Mem: Int16; Data: Int16): Int16; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicOr:=Int16(fpc_wasm32_i32_atomic_rmw16_or_u(@Mem,Word(Data)));
{$else FPC_WASM_THREADS}
  AtomicOr:=Mem;
  Mem:=Mem or Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicOr(var Mem: UInt16; Data: UInt16): UInt16; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicOr:=UInt16(fpc_wasm32_i32_atomic_rmw16_or_u(@Mem,Data));
{$else FPC_WASM_THREADS}
  AtomicOr:=Mem;
  Mem:=Mem or Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicOr(var Mem: Int32; Data: Int32): Int32; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicOr:=Int32(fpc_wasm32_i32_atomic_rmw_or(@Mem,LongWord(Data)));
{$else FPC_WASM_THREADS}
  AtomicOr:=Mem;
  Mem:=Mem or Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicOr(var Mem: UInt32; Data: UInt32): UInt32; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicOr:=fpc_wasm32_i32_atomic_rmw_or(@Mem,Data);
{$else FPC_WASM_THREADS}
  AtomicOr:=Mem;
  Mem:=Mem or Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicOr(var Mem: Int64; Data: Int64): Int64; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicOr:=Int64(fpc_wasm32_i64_atomic_rmw_or(@Mem,QWord(Data)));
{$else FPC_WASM_THREADS}
  AtomicOr:=Mem;
  Mem:=Mem or Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicOr(var Mem: UInt64; Data: UInt64): UInt64; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicOr:=fpc_wasm32_i64_atomic_rmw_or(@Mem,Data);
{$else FPC_WASM_THREADS}
  AtomicOr:=Mem;
  Mem:=Mem or Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicXor(var Mem: Int8; Data: Int8): Int8; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicXor:=Int8(fpc_wasm32_i32_atomic_rmw8_xor_u(@Mem,Byte(Data)));
{$else FPC_WASM_THREADS}
  AtomicXor:=Mem;
  Mem:=Mem xor Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicXor(var Mem: UInt8; Data: UInt8): UInt8; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicXor:=UInt8(fpc_wasm32_i32_atomic_rmw8_xor_u(@Mem,Data));
{$else FPC_WASM_THREADS}
  AtomicXor:=Mem;
  Mem:=Mem xor Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicXor(var Mem: Int16; Data: Int16): Int16; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicXor:=Int16(fpc_wasm32_i32_atomic_rmw16_xor_u(@Mem,Word(Data)));
{$else FPC_WASM_THREADS}
  AtomicXor:=Mem;
  Mem:=Mem xor Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicXor(var Mem: UInt16; Data: UInt16): UInt16; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicXor:=UInt16(fpc_wasm32_i32_atomic_rmw16_xor_u(@Mem,Data));
{$else FPC_WASM_THREADS}
  AtomicXor:=Mem;
  Mem:=Mem xor Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicXor(var Mem: Int32; Data: Int32): Int32; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicXor:=Int32(fpc_wasm32_i32_atomic_rmw_xor(@Mem,LongWord(Data)));
{$else FPC_WASM_THREADS}
  AtomicXor:=Mem;
  Mem:=Mem xor Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicXor(var Mem: UInt32; Data: UInt32): UInt32; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicXor:=fpc_wasm32_i32_atomic_rmw_xor(@Mem,Data);
{$else FPC_WASM_THREADS}
  AtomicXor:=Mem;
  Mem:=Mem xor Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicXor(var Mem: Int64; Data: Int64): Int64; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicXor:=Int64(fpc_wasm32_i64_atomic_rmw_xor(@Mem,QWord(Data)));
{$else FPC_WASM_THREADS}
  AtomicXor:=Mem;
  Mem:=Mem xor Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicXor(var Mem: UInt64; Data: UInt64): UInt64; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicXor:=fpc_wasm32_i64_atomic_rmw_xor(@Mem,Data);
{$else FPC_WASM_THREADS}
  AtomicXor:=Mem;
  Mem:=Mem xor Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicExchange(var Mem: Int8; Data: Int8): Int8; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicExchange:=Int8(fpc_wasm32_i32_atomic_rmw8_xchg_u(@Mem,Byte(Data)));
{$else FPC_WASM_THREADS}
  AtomicExchange:=Mem;
  Mem:=Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicExchange(var Mem: UInt8; Data: UInt8): UInt8; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicExchange:=UInt8(fpc_wasm32_i32_atomic_rmw8_xchg_u(@Mem,Data));
{$else FPC_WASM_THREADS}
  AtomicExchange:=Mem;
  Mem:=Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicExchange(var Mem: Int16; Data: Int16): Int16; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicExchange:=Int16(fpc_wasm32_i32_atomic_rmw16_xchg_u(@Mem,Word(Data)));
{$else FPC_WASM_THREADS}
  AtomicExchange:=Mem;
  Mem:=Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicExchange(var Mem: UInt16; Data: UInt16): UInt16; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicExchange:=UInt16(fpc_wasm32_i32_atomic_rmw16_xchg_u(@Mem,Data));
{$else FPC_WASM_THREADS}
  AtomicExchange:=Mem;
  Mem:=Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicExchange(var Mem: Int32; Data: Int32): Int32; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicExchange:=Int32(fpc_wasm32_i32_atomic_rmw_xchg(@Mem,LongWord(Data)));
{$else FPC_WASM_THREADS}
  AtomicExchange:=Mem;
  Mem:=Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicExchange(var Mem: UInt32; Data: UInt32): UInt32; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicExchange:=fpc_wasm32_i32_atomic_rmw_xchg(@Mem,Data);
{$else FPC_WASM_THREADS}
  AtomicExchange:=Mem;
  Mem:=Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicExchange(var Mem: Int64; Data: Int64): Int64; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicExchange:=Int64(fpc_wasm32_i64_atomic_rmw_xchg(@Mem,QWord(Data)));
{$else FPC_WASM_THREADS}
  AtomicExchange:=Mem;
  Mem:=Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicExchange(var Mem: UInt64; Data: UInt64): UInt64; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicExchange:=fpc_wasm32_i64_atomic_rmw_xchg(@Mem,Data);
{$else FPC_WASM_THREADS}
  AtomicExchange:=Mem;
  Mem:=Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicCompareExchange(var Mem: Int8; Compare, Data: Int8): Int8; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicCompareExchange:=Int8(fpc_wasm32_i32_atomic_rmw8_cmpxchg_u(@Mem,Byte(Compare),Byte(Data)));
{$else FPC_WASM_THREADS}
  AtomicCompareExchange:=Mem;
  if Mem=Compare then
    Mem:=Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicCompareExchange(var Mem: UInt8; Compare, Data: UInt8): UInt8; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicCompareExchange:=UInt8(fpc_wasm32_i32_atomic_rmw8_cmpxchg_u(@Mem,Compare,Data));
{$else FPC_WASM_THREADS}
  AtomicCompareExchange:=Mem;
  if Mem=Compare then
    Mem:=Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicCompareExchange(var Mem: Int16; Compare, Data: Int16): Int16; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicCompareExchange:=Int16(fpc_wasm32_i32_atomic_rmw16_cmpxchg_u(@Mem,Word(Compare),Word(Data)));
{$else FPC_WASM_THREADS}
  AtomicCompareExchange:=Mem;
  if Mem=Compare then
    Mem:=Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicCompareExchange(var Mem: UInt16; Compare, Data: UInt16): UInt16; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicCompareExchange:=UInt16(fpc_wasm32_i32_atomic_rmw16_cmpxchg_u(@Mem,Compare,Data));
{$else FPC_WASM_THREADS}
  AtomicCompareExchange:=Mem;
  if Mem=Compare then
    Mem:=Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicCompareExchange(var Mem: Int32; Compare, Data: Int32): Int32; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicCompareExchange:=Int32(fpc_wasm32_i32_atomic_rmw_cmpxchg_u(@Mem,LongWord(Compare),LongWord(Data)));
{$else FPC_WASM_THREADS}
  AtomicCompareExchange:=Mem;
  if Mem=Compare then
    Mem:=Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicCompareExchange(var Mem: UInt32; Compare, Data: UInt32): UInt32; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicCompareExchange:=fpc_wasm32_i32_atomic_rmw_cmpxchg_u(@Mem,Compare,Data);
{$else FPC_WASM_THREADS}
  AtomicCompareExchange:=Mem;
  if Mem=Compare then
    Mem:=Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicCompareExchange(var Mem: Int64; Compare, Data: Int64): Int64; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicCompareExchange:=Int64(fpc_wasm32_i64_atomic_rmw_cmpxchg_u(@Mem,QWord(Compare),QWord(Data)));
{$else FPC_WASM_THREADS}
  AtomicCompareExchange:=Mem;
  if Mem=Compare then
    Mem:=Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicCompareExchange(var Mem: UInt64; Compare, Data: UInt64): UInt64; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicCompareExchange:=fpc_wasm32_i64_atomic_rmw_cmpxchg_u(@Mem,Compare,Data);
{$else FPC_WASM_THREADS}
  AtomicCompareExchange:=Mem;
  if Mem=Compare then
    Mem:=Data;
{$endif FPC_WASM_THREADS}
end;

function AtomicWait(constref Mem: Int32; Compare: Int32; TimeoutNanoseconds: Int64): Int32; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicWait:=fpc_wasm32_memory_atomic_wait32(@Mem,LongWord(Compare),TimeoutNanoseconds);
{$else FPC_WASM_THREADS}
  if Mem<>Compare then
    AtomicWait:=awrNotEqual
  else
    AtomicWait:=awrTimedOut;
{$endif FPC_WASM_THREADS}
end;

function AtomicWait(constref Mem: UInt32; Compare: UInt32; TimeoutNanoseconds: Int64): Int32; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicWait:=fpc_wasm32_memory_atomic_wait32(@Mem,Compare,TimeoutNanoseconds);
{$else FPC_WASM_THREADS}
  if Mem<>Compare then
    AtomicWait:=awrNotEqual
  else
    AtomicWait:=awrTimedOut;
{$endif FPC_WASM_THREADS}
end;

function AtomicWait(constref Mem: Int64; Compare: Int64; TimeoutNanoseconds: Int64): Int32; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicWait:=fpc_wasm32_memory_atomic_wait64(@Mem,QWord(Compare),TimeoutNanoseconds);
{$else FPC_WASM_THREADS}
  if Mem<>Compare then
    AtomicWait:=awrNotEqual
  else
    AtomicWait:=awrTimedOut;
{$endif FPC_WASM_THREADS}
end;

function AtomicWait(constref Mem: UInt64; Compare: UInt64; TimeoutNanoseconds: Int64): Int32; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicWait:=fpc_wasm32_memory_atomic_wait64(@Mem,Compare,TimeoutNanoseconds);
{$else FPC_WASM_THREADS}
  if Mem<>Compare then
    AtomicWait:=awrNotEqual
  else
    AtomicWait:=awrTimedOut;
{$endif FPC_WASM_THREADS}
end;

function AtomicNotify(constref Mem: Int32; Count: UInt32): UInt32; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicNotify:=fpc_wasm32_memory_atomic_notify(@Mem,Count);
{$else FPC_WASM_THREADS}
  AtomicNotify:=0;
{$endif FPC_WASM_THREADS}
end;

function AtomicNotify(constref Mem: UInt32; Count: UInt32): UInt32; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicNotify:=fpc_wasm32_memory_atomic_notify(@Mem,Count);
{$else FPC_WASM_THREADS}
  AtomicNotify:=0;
{$endif FPC_WASM_THREADS}
end;

function AtomicNotify(constref Mem: Int64; Count: UInt32): UInt32; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicNotify:=fpc_wasm32_memory_atomic_notify(@Mem,Count);
{$else FPC_WASM_THREADS}
  AtomicNotify:=0;
{$endif FPC_WASM_THREADS}
end;

function AtomicNotify(constref Mem: UInt64; Count: UInt32): UInt32; inline;
begin
{$ifdef FPC_WASM_THREADS}
  AtomicNotify:=fpc_wasm32_memory_atomic_notify(@Mem,Count);
{$else FPC_WASM_THREADS}
  AtomicNotify:=0;
{$endif FPC_WASM_THREADS}
end;

end.
