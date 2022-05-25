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

unit WebAssembly;

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
  fpc_wasm32_atomic_fence;
end;

function AtomicLoad(constref Mem: Int8): Int8; inline;
begin
  AtomicLoad:=Int8(fpc_wasm32_i32_atomic_load8_u(@Mem));
end;

function AtomicLoad(constref Mem: UInt8): UInt8; inline;
begin
  AtomicLoad:=UInt8(fpc_wasm32_i32_atomic_load8_u(@Mem));
end;

function AtomicLoad(constref Mem: Int16): Int16; inline;
begin
  AtomicLoad:=Int16(fpc_wasm32_i32_atomic_load16_u(@Mem));
end;

function AtomicLoad(constref Mem: UInt16): UInt16; inline;
begin
  AtomicLoad:=UInt16(fpc_wasm32_i32_atomic_load16_u(@Mem));
end;

function AtomicLoad(constref Mem: Int32): Int32; inline;
begin
  AtomicLoad:=Int32(fpc_wasm32_i32_atomic_load(@Mem));
end;

function AtomicLoad(constref Mem: UInt32): UInt32; inline;
begin
  AtomicLoad:=UInt32(fpc_wasm32_i32_atomic_load(@Mem));
end;

function AtomicLoad(constref Mem: Int64): Int64; inline;
begin
  AtomicLoad:=Int64(fpc_wasm32_i64_atomic_load(@Mem));
end;

function AtomicLoad(constref Mem: UInt64): UInt64; inline;
begin
  AtomicLoad:=UInt64(fpc_wasm32_i64_atomic_load(@Mem));
end;

procedure AtomicStore(out Mem: Int8; Data: Int8); inline;
begin
  fpc_wasm32_i32_atomic_store8(@Mem,Byte(Data));
end;

procedure AtomicStore(out Mem: UInt8; Data: UInt8); inline;
begin
  fpc_wasm32_i32_atomic_store8(@Mem,Data);
end;

procedure AtomicStore(out Mem: Int16; Data: Int16); inline;
begin
  fpc_wasm32_i32_atomic_store16(@Mem,Word(Data));
end;

procedure AtomicStore(out Mem: UInt16; Data: UInt16); inline;
begin
  fpc_wasm32_i32_atomic_store16(@Mem,Data);
end;

procedure AtomicStore(out Mem: Int32; Data: Int32); inline;
begin
  fpc_wasm32_i32_atomic_store(@Mem,LongWord(Data));
end;

procedure AtomicStore(out Mem: UInt32; Data: UInt32); inline;
begin
  fpc_wasm32_i32_atomic_store(@Mem,Data);
end;

procedure AtomicStore(out Mem: Int64; Data: Int64); inline;
begin
  fpc_wasm32_i64_atomic_store(@Mem,QWord(Data));
end;

procedure AtomicStore(out Mem: UInt64; Data: UInt64); inline;
begin
  fpc_wasm32_i64_atomic_store(@Mem,Data);
end;

function AtomicAdd(var Mem: Int8; Data: Int8): Int8; inline;
begin
  AtomicAdd:=Int8(fpc_wasm32_i32_atomic_rmw8_add_u(@Mem,Byte(Data)));
end;

function AtomicAdd(var Mem: UInt8; Data: UInt8): UInt8; inline;
begin
  AtomicAdd:=UInt8(fpc_wasm32_i32_atomic_rmw8_add_u(@Mem,Data));
end;

function AtomicAdd(var Mem: Int16; Data: Int16): Int16; inline;
begin
  AtomicAdd:=Int16(fpc_wasm32_i32_atomic_rmw16_add_u(@Mem,Word(Data)));
end;

function AtomicAdd(var Mem: UInt16; Data: UInt16): UInt16; inline;
begin
  AtomicAdd:=UInt16(fpc_wasm32_i32_atomic_rmw16_add_u(@Mem,Data));
end;

function AtomicAdd(var Mem: Int32; Data: Int32): Int32; inline;
begin
  AtomicAdd:=Int32(fpc_wasm32_i32_atomic_rmw_add(@Mem,LongWord(Data)));
end;

function AtomicAdd(var Mem: UInt32; Data: UInt32): UInt32; inline;
begin
  AtomicAdd:=fpc_wasm32_i32_atomic_rmw_add(@Mem,Data);
end;

function AtomicAdd(var Mem: Int64; Data: Int64): Int64; inline;
begin
  AtomicAdd:=Int64(fpc_wasm32_i64_atomic_rmw_add(@Mem,QWord(Data)));
end;

function AtomicAdd(var Mem: UInt64; Data: UInt64): UInt64; inline;
begin
  AtomicAdd:=fpc_wasm32_i64_atomic_rmw_add(@Mem,Data);
end;

function AtomicSub(var Mem: Int8; Data: Int8): Int8; inline;
begin
  AtomicSub:=Int8(fpc_wasm32_i32_atomic_rmw8_sub_u(@Mem,Byte(Data)));
end;

function AtomicSub(var Mem: UInt8; Data: UInt8): UInt8; inline;
begin
  AtomicSub:=UInt8(fpc_wasm32_i32_atomic_rmw8_sub_u(@Mem,Data));
end;

function AtomicSub(var Mem: Int16; Data: Int16): Int16; inline;
begin
  AtomicSub:=Int16(fpc_wasm32_i32_atomic_rmw16_sub_u(@Mem,Word(Data)));
end;

function AtomicSub(var Mem: UInt16; Data: UInt16): UInt16; inline;
begin
  AtomicSub:=UInt16(fpc_wasm32_i32_atomic_rmw16_sub_u(@Mem,Data));
end;

function AtomicSub(var Mem: Int32; Data: Int32): Int32; inline;
begin
  AtomicSub:=Int32(fpc_wasm32_i32_atomic_rmw_sub(@Mem,LongWord(Data)));
end;

function AtomicSub(var Mem: UInt32; Data: UInt32): UInt32; inline;
begin
  AtomicSub:=fpc_wasm32_i32_atomic_rmw_sub(@Mem,Data);
end;

function AtomicSub(var Mem: Int64; Data: Int64): Int64; inline;
begin
  AtomicSub:=Int64(fpc_wasm32_i64_atomic_rmw_sub(@Mem,QWord(Data)));
end;

function AtomicSub(var Mem: UInt64; Data: UInt64): UInt64; inline;
begin
  AtomicSub:=fpc_wasm32_i64_atomic_rmw_sub(@Mem,Data);
end;

function AtomicAnd(var Mem: Int8; Data: Int8): Int8; inline;
begin
  AtomicAnd:=Int8(fpc_wasm32_i32_atomic_rmw8_and_u(@Mem,Byte(Data)));
end;

function AtomicAnd(var Mem: UInt8; Data: UInt8): UInt8; inline;
begin
  AtomicAnd:=UInt8(fpc_wasm32_i32_atomic_rmw8_and_u(@Mem,Data));
end;

function AtomicAnd(var Mem: Int16; Data: Int16): Int16; inline;
begin
  AtomicAnd:=Int16(fpc_wasm32_i32_atomic_rmw16_and_u(@Mem,Word(Data)));
end;

function AtomicAnd(var Mem: UInt16; Data: UInt16): UInt16; inline;
begin
  AtomicAnd:=UInt16(fpc_wasm32_i32_atomic_rmw16_and_u(@Mem,Data));
end;

function AtomicAnd(var Mem: Int32; Data: Int32): Int32; inline;
begin
  AtomicAnd:=Int32(fpc_wasm32_i32_atomic_rmw_and(@Mem,LongWord(Data)));
end;

function AtomicAnd(var Mem: UInt32; Data: UInt32): UInt32; inline;
begin
  AtomicAnd:=fpc_wasm32_i32_atomic_rmw_and(@Mem,Data);
end;

function AtomicAnd(var Mem: Int64; Data: Int64): Int64; inline;
begin
  AtomicAnd:=Int64(fpc_wasm32_i64_atomic_rmw_and(@Mem,QWord(Data)));
end;

function AtomicAnd(var Mem: UInt64; Data: UInt64): UInt64; inline;
begin
  AtomicAnd:=fpc_wasm32_i64_atomic_rmw_and(@Mem,Data);
end;

function AtomicOr(var Mem: Int8; Data: Int8): Int8; inline;
begin
  AtomicOr:=Int8(fpc_wasm32_i32_atomic_rmw8_or_u(@Mem,Byte(Data)));
end;

function AtomicOr(var Mem: UInt8; Data: UInt8): UInt8; inline;
begin
  AtomicOr:=UInt8(fpc_wasm32_i32_atomic_rmw8_or_u(@Mem,Data));
end;

function AtomicOr(var Mem: Int16; Data: Int16): Int16; inline;
begin
  AtomicOr:=Int16(fpc_wasm32_i32_atomic_rmw16_or_u(@Mem,Word(Data)));
end;

function AtomicOr(var Mem: UInt16; Data: UInt16): UInt16; inline;
begin
  AtomicOr:=UInt16(fpc_wasm32_i32_atomic_rmw16_or_u(@Mem,Data));
end;

function AtomicOr(var Mem: Int32; Data: Int32): Int32; inline;
begin
  AtomicOr:=Int32(fpc_wasm32_i32_atomic_rmw_or(@Mem,LongWord(Data)));
end;

function AtomicOr(var Mem: UInt32; Data: UInt32): UInt32; inline;
begin
  AtomicOr:=fpc_wasm32_i32_atomic_rmw_or(@Mem,Data);
end;

function AtomicOr(var Mem: Int64; Data: Int64): Int64; inline;
begin
  AtomicOr:=Int64(fpc_wasm32_i64_atomic_rmw_or(@Mem,QWord(Data)));
end;

function AtomicOr(var Mem: UInt64; Data: UInt64): UInt64; inline;
begin
  AtomicOr:=fpc_wasm32_i64_atomic_rmw_or(@Mem,Data);
end;

function AtomicXor(var Mem: Int8; Data: Int8): Int8; inline;
begin
  AtomicXor:=Int8(fpc_wasm32_i32_atomic_rmw8_xor_u(@Mem,Byte(Data)));
end;

function AtomicXor(var Mem: UInt8; Data: UInt8): UInt8; inline;
begin
  AtomicXor:=UInt8(fpc_wasm32_i32_atomic_rmw8_xor_u(@Mem,Data));
end;

function AtomicXor(var Mem: Int16; Data: Int16): Int16; inline;
begin
  AtomicXor:=Int16(fpc_wasm32_i32_atomic_rmw16_xor_u(@Mem,Word(Data)));
end;

function AtomicXor(var Mem: UInt16; Data: UInt16): UInt16; inline;
begin
  AtomicXor:=UInt16(fpc_wasm32_i32_atomic_rmw16_xor_u(@Mem,Data));
end;

function AtomicXor(var Mem: Int32; Data: Int32): Int32; inline;
begin
  AtomicXor:=Int32(fpc_wasm32_i32_atomic_rmw_xor(@Mem,LongWord(Data)));
end;

function AtomicXor(var Mem: UInt32; Data: UInt32): UInt32; inline;
begin
  AtomicXor:=fpc_wasm32_i32_atomic_rmw_xor(@Mem,Data);
end;

function AtomicXor(var Mem: Int64; Data: Int64): Int64; inline;
begin
  AtomicXor:=Int64(fpc_wasm32_i64_atomic_rmw_xor(@Mem,QWord(Data)));
end;

function AtomicXor(var Mem: UInt64; Data: UInt64): UInt64; inline;
begin
  AtomicXor:=fpc_wasm32_i64_atomic_rmw_xor(@Mem,Data);
end;

function AtomicExchange(var Mem: Int8; Data: Int8): Int8; inline;
begin
  AtomicExchange:=Int8(fpc_wasm32_i32_atomic_rmw8_xchg_u(@Mem,Byte(Data)));
end;

function AtomicExchange(var Mem: UInt8; Data: UInt8): UInt8; inline;
begin
  AtomicExchange:=UInt8(fpc_wasm32_i32_atomic_rmw8_xchg_u(@Mem,Data));
end;

function AtomicExchange(var Mem: Int16; Data: Int16): Int16; inline;
begin
  AtomicExchange:=Int16(fpc_wasm32_i32_atomic_rmw16_xchg_u(@Mem,Word(Data)));
end;

function AtomicExchange(var Mem: UInt16; Data: UInt16): UInt16; inline;
begin
  AtomicExchange:=UInt16(fpc_wasm32_i32_atomic_rmw16_xchg_u(@Mem,Data));
end;

function AtomicExchange(var Mem: Int32; Data: Int32): Int32; inline;
begin
  AtomicExchange:=Int32(fpc_wasm32_i32_atomic_rmw_xchg(@Mem,LongWord(Data)));
end;

function AtomicExchange(var Mem: UInt32; Data: UInt32): UInt32; inline;
begin
  AtomicExchange:=fpc_wasm32_i32_atomic_rmw_xchg(@Mem,Data);
end;

function AtomicExchange(var Mem: Int64; Data: Int64): Int64; inline;
begin
  AtomicExchange:=Int64(fpc_wasm32_i64_atomic_rmw_xchg(@Mem,QWord(Data)));
end;

function AtomicExchange(var Mem: UInt64; Data: UInt64): UInt64; inline;
begin
  AtomicExchange:=fpc_wasm32_i64_atomic_rmw_xchg(@Mem,Data);
end;

function AtomicCompareExchange(var Mem: Int8; Compare, Data: Int8): Int8; inline;
begin
  AtomicCompareExchange:=Int8(fpc_wasm32_i32_atomic_rmw8_cmpxchg_u(@Mem,Byte(Compare),Byte(Data)));
end;

function AtomicCompareExchange(var Mem: UInt8; Compare, Data: UInt8): UInt8; inline;
begin
  AtomicCompareExchange:=UInt8(fpc_wasm32_i32_atomic_rmw8_cmpxchg_u(@Mem,Compare,Data));
end;

function AtomicCompareExchange(var Mem: Int16; Compare, Data: Int16): Int16; inline;
begin
  AtomicCompareExchange:=Int16(fpc_wasm32_i32_atomic_rmw16_cmpxchg_u(@Mem,Word(Compare),Word(Data)));
end;

function AtomicCompareExchange(var Mem: UInt16; Compare, Data: UInt16): UInt16; inline;
begin
  AtomicCompareExchange:=UInt16(fpc_wasm32_i32_atomic_rmw16_cmpxchg_u(@Mem,Compare,Data));
end;

function AtomicCompareExchange(var Mem: Int32; Compare, Data: Int32): Int32; inline;
begin
  AtomicCompareExchange:=Int32(fpc_wasm32_i32_atomic_rmw_cmpxchg_u(@Mem,LongWord(Compare),LongWord(Data)));
end;

function AtomicCompareExchange(var Mem: UInt32; Compare, Data: UInt32): UInt32; inline;
begin
  AtomicCompareExchange:=fpc_wasm32_i32_atomic_rmw_cmpxchg_u(@Mem,Compare,Data);
end;

function AtomicCompareExchange(var Mem: Int64; Compare, Data: Int64): Int64; inline;
begin
  AtomicCompareExchange:=Int64(fpc_wasm32_i64_atomic_rmw_cmpxchg_u(@Mem,QWord(Compare),QWord(Data)));
end;

function AtomicCompareExchange(var Mem: UInt64; Compare, Data: UInt64): UInt64; inline;
begin
  AtomicCompareExchange:=fpc_wasm32_i64_atomic_rmw_cmpxchg_u(@Mem,Compare,Data);
end;

function AtomicWait(constref Mem: Int32; Compare: Int32; TimeoutNanoseconds: Int64): Int32; inline;
begin
  AtomicWait:=fpc_wasm32_memory_atomic_wait32(@Mem,LongWord(Compare),TimeoutNanoseconds);
end;

function AtomicWait(constref Mem: UInt32; Compare: UInt32; TimeoutNanoseconds: Int64): Int32; inline;
begin
  AtomicWait:=fpc_wasm32_memory_atomic_wait32(@Mem,Compare,TimeoutNanoseconds);
end;

function AtomicWait(constref Mem: Int64; Compare: Int64; TimeoutNanoseconds: Int64): Int32; inline;
begin
  AtomicWait:=fpc_wasm32_memory_atomic_wait64(@Mem,QWord(Compare),TimeoutNanoseconds);
end;

function AtomicWait(constref Mem: UInt64; Compare: UInt64; TimeoutNanoseconds: Int64): Int32; inline;
begin
  AtomicWait:=fpc_wasm32_memory_atomic_wait64(@Mem,Compare,TimeoutNanoseconds);
end;

function AtomicNotify(constref Mem: Int32; Count: UInt32): UInt32; inline;
begin
  AtomicNotify:=fpc_wasm32_memory_atomic_notify(@Mem,Count);
end;

function AtomicNotify(constref Mem: UInt32; Count: UInt32): UInt32; inline;
begin
  AtomicNotify:=fpc_wasm32_memory_atomic_notify(@Mem,Count);
end;

function AtomicNotify(constref Mem: Int64; Count: UInt32): UInt32; inline;
begin
  AtomicNotify:=fpc_wasm32_memory_atomic_notify(@Mem,Count);
end;

function AtomicNotify(constref Mem: UInt64; Count: UInt32): UInt32; inline;
begin
  AtomicNotify:=fpc_wasm32_memory_atomic_notify(@Mem,Count);
end;

end.
