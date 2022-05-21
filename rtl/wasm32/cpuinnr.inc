{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2021 by the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

  fpc_in_wasm32_memory_size = fpc_in_cpu_first;
  fpc_in_wasm32_memory_grow = fpc_in_cpu_first+1;
  fpc_in_wasm32_unreachable = fpc_in_cpu_first+2;
  fpc_in_wasm32_throw_fpcexception = fpc_in_cpu_first+3;
  fpc_in_wasm32_memory_copy = fpc_in_cpu_first+4;
  fpc_in_wasm32_memory_fill = fpc_in_cpu_first+5;
  fpc_in_wasm32_atomic_fence = fpc_in_cpu_first+6;

  fpc_in_wasm32_i32_atomic_rmw8_add_u  = fpc_in_cpu_first+7;
  fpc_in_wasm32_i32_atomic_rmw16_add_u = fpc_in_cpu_first+8;
  fpc_in_wasm32_i32_atomic_rmw_add     = fpc_in_cpu_first+9;
  fpc_in_wasm32_i64_atomic_rmw8_add_u  = fpc_in_cpu_first+10;
  fpc_in_wasm32_i64_atomic_rmw16_add_u = fpc_in_cpu_first+11;
  fpc_in_wasm32_i64_atomic_rmw32_add_u = fpc_in_cpu_first+12;
  fpc_in_wasm32_i64_atomic_rmw_add     = fpc_in_cpu_first+13;

  fpc_in_wasm32_i32_atomic_rmw8_sub_u  = fpc_in_cpu_first+14;
  fpc_in_wasm32_i32_atomic_rmw16_sub_u = fpc_in_cpu_first+15;
  fpc_in_wasm32_i32_atomic_rmw_sub     = fpc_in_cpu_first+16;
  fpc_in_wasm32_i64_atomic_rmw8_sub_u  = fpc_in_cpu_first+17;
  fpc_in_wasm32_i64_atomic_rmw16_sub_u = fpc_in_cpu_first+18;
  fpc_in_wasm32_i64_atomic_rmw32_sub_u = fpc_in_cpu_first+19;
  fpc_in_wasm32_i64_atomic_rmw_sub     = fpc_in_cpu_first+20;

  fpc_in_wasm32_i32_atomic_rmw8_and_u  = fpc_in_cpu_first+21;
  fpc_in_wasm32_i32_atomic_rmw16_and_u = fpc_in_cpu_first+22;
  fpc_in_wasm32_i32_atomic_rmw_and     = fpc_in_cpu_first+23;
  fpc_in_wasm32_i64_atomic_rmw8_and_u  = fpc_in_cpu_first+24;
  fpc_in_wasm32_i64_atomic_rmw16_and_u = fpc_in_cpu_first+25;
  fpc_in_wasm32_i64_atomic_rmw32_and_u = fpc_in_cpu_first+26;
  fpc_in_wasm32_i64_atomic_rmw_and     = fpc_in_cpu_first+27;

  fpc_in_wasm32_i32_atomic_rmw8_or_u  = fpc_in_cpu_first+28;
  fpc_in_wasm32_i32_atomic_rmw16_or_u = fpc_in_cpu_first+29;
  fpc_in_wasm32_i32_atomic_rmw_or     = fpc_in_cpu_first+30;
  fpc_in_wasm32_i64_atomic_rmw8_or_u  = fpc_in_cpu_first+31;
  fpc_in_wasm32_i64_atomic_rmw16_or_u = fpc_in_cpu_first+32;
  fpc_in_wasm32_i64_atomic_rmw32_or_u = fpc_in_cpu_first+33;
  fpc_in_wasm32_i64_atomic_rmw_or     = fpc_in_cpu_first+34;

  fpc_in_wasm32_i32_atomic_rmw8_xor_u  = fpc_in_cpu_first+35;
  fpc_in_wasm32_i32_atomic_rmw16_xor_u = fpc_in_cpu_first+36;
  fpc_in_wasm32_i32_atomic_rmw_xor     = fpc_in_cpu_first+37;
  fpc_in_wasm32_i64_atomic_rmw8_xor_u  = fpc_in_cpu_first+38;
  fpc_in_wasm32_i64_atomic_rmw16_xor_u = fpc_in_cpu_first+39;
  fpc_in_wasm32_i64_atomic_rmw32_xor_u = fpc_in_cpu_first+40;
  fpc_in_wasm32_i64_atomic_rmw_xor     = fpc_in_cpu_first+41;

  fpc_in_wasm32_i32_atomic_rmw8_xchg_u  = fpc_in_cpu_first+42;
  fpc_in_wasm32_i32_atomic_rmw16_xchg_u = fpc_in_cpu_first+43;
  fpc_in_wasm32_i32_atomic_rmw_xchg     = fpc_in_cpu_first+44;
  fpc_in_wasm32_i64_atomic_rmw8_xchg_u  = fpc_in_cpu_first+45;
  fpc_in_wasm32_i64_atomic_rmw16_xchg_u = fpc_in_cpu_first+46;
  fpc_in_wasm32_i64_atomic_rmw32_xchg_u = fpc_in_cpu_first+47;
  fpc_in_wasm32_i64_atomic_rmw_xchg     = fpc_in_cpu_first+48;

  fpc_in_wasm32_i32_atomic_rmw8_cmpxchg_u  = fpc_in_cpu_first+49;
  fpc_in_wasm32_i32_atomic_rmw16_cmpxchg_u = fpc_in_cpu_first+50;
  fpc_in_wasm32_i32_atomic_rmw_cmpxchg     = fpc_in_cpu_first+51;
  fpc_in_wasm32_i64_atomic_rmw8_cmpxchg_u  = fpc_in_cpu_first+52;
  fpc_in_wasm32_i64_atomic_rmw16_cmpxchg_u = fpc_in_cpu_first+53;
  fpc_in_wasm32_i64_atomic_rmw32_cmpxchg_u = fpc_in_cpu_first+54;
  fpc_in_wasm32_i64_atomic_rmw_cmpxchg     = fpc_in_cpu_first+55;

  fpc_in_wasm32_memory_atomic_wait32 = fpc_in_cpu_first+56;
  fpc_in_wasm32_memory_atomic_wait64 = fpc_in_cpu_first+57;
  fpc_in_wasm32_memory_atomic_notify = fpc_in_cpu_first+58;

