{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2021 by the Free Pascal development team.

    CPU specific system unit header file

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$define FPC_SYSTEM_FPUCW_IMMUTABLE}

const
  {$I cpuinnr.inc}

function fpc_wasm32_memory_size: longword;[internproc:fpc_in_wasm32_memory_size];
function fpc_wasm32_memory_grow(n: longword): longword;[internproc:fpc_in_wasm32_memory_grow];
procedure fpc_wasm32_unreachable;[internproc:fpc_in_wasm32_unreachable];
procedure fpc_wasm32_throw_fpcexception;[internproc:fpc_in_wasm32_throw_fpcexception];
procedure fpc_wasm32_memory_fill(d,val,n: longword);[internproc:fpc_in_wasm32_memory_fill];
procedure fpc_wasm32_memory_copy(d,s,n: longword);[internproc:fpc_in_wasm32_memory_copy];
procedure fpc_wasm32_atomic_fence;[internproc:fpc_in_wasm32_atomic_fence];

function fpc_wasm32_i32_atomic_rmw8_add_u(Dest: PByte; x: LongWord): longword;[internproc:fpc_in_wasm32_i32_atomic_rmw8_add_u];
function fpc_wasm32_i32_atomic_rmw16_add_u(Dest: PWord; x: LongWord): longword;[internproc:fpc_in_wasm32_i32_atomic_rmw16_add_u];
function fpc_wasm32_i32_atomic_rmw_add(Dest: PLongWord; x: LongWord): longword;[internproc:fpc_in_wasm32_i32_atomic_rmw_add];
function fpc_wasm32_i64_atomic_rmw8_add_u(Dest: PByte; x: QWord): qword;[internproc:fpc_in_wasm32_i64_atomic_rmw8_add_u];
function fpc_wasm32_i64_atomic_rmw16_add_u(Dest: PWord; x: QWord): qword;[internproc:fpc_in_wasm32_i64_atomic_rmw16_add_u];
function fpc_wasm32_i64_atomic_rmw32_add_u(Dest: PLongWord; x: QWord): qword;[internproc:fpc_in_wasm32_i64_atomic_rmw32_add_u];
function fpc_wasm32_i64_atomic_rmw_add(Dest: PQWord; x: QWord): QWord;[internproc:fpc_in_wasm32_i64_atomic_rmw_add];

function fpc_wasm32_i32_atomic_rmw8_sub_u(Dest: PByte; x: LongWord): longword;[internproc:fpc_in_wasm32_i32_atomic_rmw8_sub_u];
function fpc_wasm32_i32_atomic_rmw16_sub_u(Dest: PWord; x: LongWord): longword;[internproc:fpc_in_wasm32_i32_atomic_rmw16_sub_u];
function fpc_wasm32_i32_atomic_rmw_sub(Dest: PLongWord; x: LongWord): longword;[internproc:fpc_in_wasm32_i32_atomic_rmw_sub];
function fpc_wasm32_i64_atomic_rmw8_sub_u(Dest: PByte; x: QWord): qword;[internproc:fpc_in_wasm32_i64_atomic_rmw8_sub_u];
function fpc_wasm32_i64_atomic_rmw16_sub_u(Dest: PWord; x: QWord): qword;[internproc:fpc_in_wasm32_i64_atomic_rmw16_sub_u];
function fpc_wasm32_i64_atomic_rmw32_sub_u(Dest: PLongWord; x: QWord): qword;[internproc:fpc_in_wasm32_i64_atomic_rmw32_sub_u];
function fpc_wasm32_i64_atomic_rmw_sub(Dest: PQWord; x: QWord): QWord;[internproc:fpc_in_wasm32_i64_atomic_rmw_sub];

function fpc_wasm32_i32_atomic_rmw8_and_u(Dest: PByte; x: LongWord): longword;[internproc:fpc_in_wasm32_i32_atomic_rmw8_and_u];
function fpc_wasm32_i32_atomic_rmw16_and_u(Dest: PWord; x: LongWord): longword;[internproc:fpc_in_wasm32_i32_atomic_rmw16_and_u];
function fpc_wasm32_i32_atomic_rmw_and(Dest: PLongWord; x: LongWord): longword;[internproc:fpc_in_wasm32_i32_atomic_rmw_and];
function fpc_wasm32_i64_atomic_rmw8_and_u(Dest: PByte; x: QWord): qword;[internproc:fpc_in_wasm32_i64_atomic_rmw8_and_u];
function fpc_wasm32_i64_atomic_rmw16_and_u(Dest: PWord; x: QWord): qword;[internproc:fpc_in_wasm32_i64_atomic_rmw16_and_u];
function fpc_wasm32_i64_atomic_rmw32_and_u(Dest: PLongWord; x: QWord): qword;[internproc:fpc_in_wasm32_i64_atomic_rmw32_and_u];
function fpc_wasm32_i64_atomic_rmw_and(Dest: PQWord; x: QWord): QWord;[internproc:fpc_in_wasm32_i64_atomic_rmw_and];

function fpc_wasm32_i32_atomic_rmw8_or_u(Dest: PByte; x: LongWord): longword;[internproc:fpc_in_wasm32_i32_atomic_rmw8_or_u];
function fpc_wasm32_i32_atomic_rmw16_or_u(Dest: PWord; x: LongWord): longword;[internproc:fpc_in_wasm32_i32_atomic_rmw16_or_u];
function fpc_wasm32_i32_atomic_rmw_or(Dest: PLongWord; x: LongWord): longword;[internproc:fpc_in_wasm32_i32_atomic_rmw_or];
function fpc_wasm32_i64_atomic_rmw8_or_u(Dest: PByte; x: QWord): qword;[internproc:fpc_in_wasm32_i64_atomic_rmw8_or_u];
function fpc_wasm32_i64_atomic_rmw16_or_u(Dest: PWord; x: QWord): qword;[internproc:fpc_in_wasm32_i64_atomic_rmw16_or_u];
function fpc_wasm32_i64_atomic_rmw32_or_u(Dest: PLongWord; x: QWord): qword;[internproc:fpc_in_wasm32_i64_atomic_rmw32_or_u];
function fpc_wasm32_i64_atomic_rmw_or(Dest: PQWord; x: QWord): QWord;[internproc:fpc_in_wasm32_i64_atomic_rmw_or];

function fpc_wasm32_i32_atomic_rmw8_xor_u(Dest: PByte; x: LongWord): longword;[internproc:fpc_in_wasm32_i32_atomic_rmw8_xor_u];
function fpc_wasm32_i32_atomic_rmw16_xor_u(Dest: PWord; x: LongWord): longword;[internproc:fpc_in_wasm32_i32_atomic_rmw16_xor_u];
function fpc_wasm32_i32_atomic_rmw_xor(Dest: PLongWord; x: LongWord): longword;[internproc:fpc_in_wasm32_i32_atomic_rmw_xor];
function fpc_wasm32_i64_atomic_rmw8_xor_u(Dest: PByte; x: QWord): qword;[internproc:fpc_in_wasm32_i64_atomic_rmw8_xor_u];
function fpc_wasm32_i64_atomic_rmw16_xor_u(Dest: PWord; x: QWord): qword;[internproc:fpc_in_wasm32_i64_atomic_rmw16_xor_u];
function fpc_wasm32_i64_atomic_rmw32_xor_u(Dest: PLongWord; x: QWord): qword;[internproc:fpc_in_wasm32_i64_atomic_rmw32_xor_u];
function fpc_wasm32_i64_atomic_rmw_xor(Dest: PQWord; x: QWord): QWord;[internproc:fpc_in_wasm32_i64_atomic_rmw_xor];

function fpc_wasm32_i32_atomic_rmw8_xchg_u(Dest: PByte; x: LongWord): longword;[internproc:fpc_in_wasm32_i32_atomic_rmw8_xchg_u];
function fpc_wasm32_i32_atomic_rmw16_xchg_u(Dest: PWord; x: LongWord): longword;[internproc:fpc_in_wasm32_i32_atomic_rmw16_xchg_u];
function fpc_wasm32_i32_atomic_rmw_xchg(Dest: PLongWord; x: LongWord): longword;[internproc:fpc_in_wasm32_i32_atomic_rmw_xchg];
function fpc_wasm32_i64_atomic_rmw8_xchg_u(Dest: PByte; x: QWord): qword;[internproc:fpc_in_wasm32_i64_atomic_rmw8_xchg_u];
function fpc_wasm32_i64_atomic_rmw16_xchg_u(Dest: PWord; x: QWord): qword;[internproc:fpc_in_wasm32_i64_atomic_rmw16_xchg_u];
function fpc_wasm32_i64_atomic_rmw32_xchg_u(Dest: PLongWord; x: QWord): qword;[internproc:fpc_in_wasm32_i64_atomic_rmw32_xchg_u];
function fpc_wasm32_i64_atomic_rmw_xchg(Dest: PQWord; x: QWord): QWord;[internproc:fpc_in_wasm32_i64_atomic_rmw_xchg];

function fpc_wasm32_i32_atomic_rmw8_cmpxchg_u(Dest: PByte; Expected, Replacement: LongWord): LongWord;[internproc:fpc_in_wasm32_i32_atomic_rmw8_cmpxchg_u];
function fpc_wasm32_i32_atomic_rmw16_cmpxchg_u(Dest: PWord; Expected, Replacement: LongWord): LongWord;[internproc:fpc_in_wasm32_i32_atomic_rmw16_cmpxchg_u];
function fpc_wasm32_i32_atomic_rmw_cmpxchg_u(Dest: PLongWord; Expected, Replacement: LongWord): LongWord;[internproc:fpc_in_wasm32_i32_atomic_rmw_cmpxchg];
function fpc_wasm32_i64_atomic_rmw8_cmpxchg_u(Dest: PByte; Expected, Replacement: QWord): QWord;[internproc:fpc_in_wasm32_i64_atomic_rmw8_cmpxchg_u];
function fpc_wasm32_i64_atomic_rmw16_cmpxchg_u(Dest: PWord; Expected, Replacement: QWord): QWord;[internproc:fpc_in_wasm32_i64_atomic_rmw16_cmpxchg_u];
function fpc_wasm32_i64_atomic_rmw32_cmpxchg_u(Dest: PWord; Expected, Replacement: QWord): QWord;[internproc:fpc_in_wasm32_i64_atomic_rmw32_cmpxchg_u];
function fpc_wasm32_i64_atomic_rmw_cmpxchg_u(Dest: PLongWord; Expected, Replacement: QWord): QWord;[internproc:fpc_in_wasm32_i64_atomic_rmw_cmpxchg];

function fpc_wasm32_memory_atomic_wait32(Dest: PLongWord; Expected: LongWord; Timeout: Int64): LongInt;[internproc:fpc_in_wasm32_memory_atomic_wait32];
function fpc_wasm32_memory_atomic_wait64(Dest: PQWord; Expected: QWord; Timeout: Int64): LongInt;[internproc:fpc_in_wasm32_memory_atomic_wait64];
function fpc_wasm32_memory_atomic_notify(Address: Pointer; Count: LongWord): LongWord;[internproc:fpc_in_wasm32_memory_atomic_notify];

function fpc_wasm32_i32_atomic_load8_u(Address: Pointer): LongWord;[internproc:fpc_in_i32_atomic_load8_u];
function fpc_wasm32_i32_atomic_load16_u(Address: Pointer): LongWord;[internproc:fpc_in_i32_atomic_load16_u];
function fpc_wasm32_i32_atomic_load(Address: Pointer): LongWord;[internproc:fpc_in_i32_atomic_load];
function fpc_wasm32_i64_atomic_load8_u(Address: Pointer): QWord;[internproc:fpc_in_i64_atomic_load8_u];
function fpc_wasm32_i64_atomic_load16_u(Address: Pointer): QWord;[internproc:fpc_in_i64_atomic_load16_u];
function fpc_wasm32_i64_atomic_load32_u(Address: Pointer): QWord;[internproc:fpc_in_i64_atomic_load32_u];
function fpc_wasm32_i64_atomic_load(Address: Pointer): QWord;[internproc:fpc_in_i64_atomic_load];

procedure fpc_wasm32_i32_atomic_store8(Dest: PByte; x: LongWord);[internproc:fpc_in_i32_atomic_store8];
procedure fpc_wasm32_i32_atomic_store16(Dest: PWord; x: LongWord);[internproc:fpc_in_i32_atomic_store16];
procedure fpc_wasm32_i32_atomic_store(Dest: PLongWord; x: LongWord);[internproc:fpc_in_i32_atomic_store];
procedure fpc_wasm32_i64_atomic_store8(Dest: PByte; x: QWord);[internproc:fpc_in_i64_atomic_store8];
procedure fpc_wasm32_i64_atomic_store16(Dest: PWord; x: QWord);[internproc:fpc_in_i64_atomic_store16];
procedure fpc_wasm32_i64_atomic_store32(Dest: PLongWord; x: QWord);[internproc:fpc_in_i64_atomic_store32];
procedure fpc_wasm32_i64_atomic_store(Dest: PQWord; x: QWord);[internproc:fpc_in_i64_atomic_store];

function fpc_wasm32_tls_size: SizeUInt;[internproc:fpc_in_wasm32_tls_size];
function fpc_wasm32_tls_align: SizeUInt;[internproc:fpc_in_wasm32_tls_align];
function fpc_wasm32_tls_base: Pointer;[internproc:fpc_in_wasm32_tls_base];

