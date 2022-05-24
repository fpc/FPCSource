{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2021 by the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

  in_wasm32_memory_size = in_cpu_first,
  in_wasm32_memory_grow = in_cpu_first+1,
  in_wasm32_unreachable = in_cpu_first+2,
  in_wasm32_throw_fpcexception = in_cpu_first+3,
  in_wasm32_memory_copy = in_cpu_first+4,
  in_wasm32_memory_fill = in_cpu_first+5,
  in_wasm32_atomic_fence = in_cpu_first+6,

  in_wasm32_i32_atomic_rmw8_add_u  = in_cpu_first+7,
  in_wasm32_i32_atomic_rmw16_add_u = in_cpu_first+8,
  in_wasm32_i32_atomic_rmw_add     = in_cpu_first+9,
  in_wasm32_i64_atomic_rmw8_add_u  = in_cpu_first+10,
  in_wasm32_i64_atomic_rmw16_add_u = in_cpu_first+11,
  in_wasm32_i64_atomic_rmw32_add_u = in_cpu_first+12,
  in_wasm32_i64_atomic_rmw_add     = in_cpu_first+13,

  in_wasm32_i32_atomic_rmw8_sub_u  = in_cpu_first+14,
  in_wasm32_i32_atomic_rmw16_sub_u = in_cpu_first+15,
  in_wasm32_i32_atomic_rmw_sub     = in_cpu_first+16,
  in_wasm32_i64_atomic_rmw8_sub_u  = in_cpu_first+17,
  in_wasm32_i64_atomic_rmw16_sub_u = in_cpu_first+18,
  in_wasm32_i64_atomic_rmw32_sub_u = in_cpu_first+19,
  in_wasm32_i64_atomic_rmw_sub     = in_cpu_first+20,

  in_wasm32_i32_atomic_rmw8_and_u  = in_cpu_first+21,
  in_wasm32_i32_atomic_rmw16_and_u = in_cpu_first+22,
  in_wasm32_i32_atomic_rmw_and     = in_cpu_first+23,
  in_wasm32_i64_atomic_rmw8_and_u  = in_cpu_first+24,
  in_wasm32_i64_atomic_rmw16_and_u = in_cpu_first+25,
  in_wasm32_i64_atomic_rmw32_and_u = in_cpu_first+26,
  in_wasm32_i64_atomic_rmw_and     = in_cpu_first+27,

  in_wasm32_i32_atomic_rmw8_or_u  = in_cpu_first+28,
  in_wasm32_i32_atomic_rmw16_or_u = in_cpu_first+29,
  in_wasm32_i32_atomic_rmw_or     = in_cpu_first+30,
  in_wasm32_i64_atomic_rmw8_or_u  = in_cpu_first+31,
  in_wasm32_i64_atomic_rmw16_or_u = in_cpu_first+32,
  in_wasm32_i64_atomic_rmw32_or_u = in_cpu_first+33,
  in_wasm32_i64_atomic_rmw_or     = in_cpu_first+34,

  in_wasm32_i32_atomic_rmw8_xor_u  = in_cpu_first+35,
  in_wasm32_i32_atomic_rmw16_xor_u = in_cpu_first+36,
  in_wasm32_i32_atomic_rmw_xor     = in_cpu_first+37,
  in_wasm32_i64_atomic_rmw8_xor_u  = in_cpu_first+38,
  in_wasm32_i64_atomic_rmw16_xor_u = in_cpu_first+39,
  in_wasm32_i64_atomic_rmw32_xor_u = in_cpu_first+40,
  in_wasm32_i64_atomic_rmw_xor     = in_cpu_first+41,

  in_wasm32_i32_atomic_rmw8_xchg_u  = in_cpu_first+42,
  in_wasm32_i32_atomic_rmw16_xchg_u = in_cpu_first+43,
  in_wasm32_i32_atomic_rmw_xchg     = in_cpu_first+44,
  in_wasm32_i64_atomic_rmw8_xchg_u  = in_cpu_first+45,
  in_wasm32_i64_atomic_rmw16_xchg_u = in_cpu_first+46,
  in_wasm32_i64_atomic_rmw32_xchg_u = in_cpu_first+47,
  in_wasm32_i64_atomic_rmw_xchg     = in_cpu_first+48,

  in_wasm32_i32_atomic_rmw8_cmpxchg_u  = in_cpu_first+49,
  in_wasm32_i32_atomic_rmw16_cmpxchg_u = in_cpu_first+50,
  in_wasm32_i32_atomic_rmw_cmpxchg     = in_cpu_first+51,
  in_wasm32_i64_atomic_rmw8_cmpxchg_u  = in_cpu_first+52,
  in_wasm32_i64_atomic_rmw16_cmpxchg_u = in_cpu_first+53,
  in_wasm32_i64_atomic_rmw32_cmpxchg_u = in_cpu_first+54,
  in_wasm32_i64_atomic_rmw_cmpxchg     = in_cpu_first+55,

  in_wasm32_memory_atomic_wait32 = in_cpu_first+56,
  in_wasm32_memory_atomic_wait64 = in_cpu_first+57,
  in_wasm32_memory_atomic_notify = in_cpu_first+58,

  in_i32_atomic_load8_u  = in_cpu_first+59,
  in_i32_atomic_load16_u = in_cpu_first+60,
  in_i32_atomic_load     = in_cpu_first+61,
  in_i64_atomic_load8_u  = in_cpu_first+62,
  in_i64_atomic_load16_u = in_cpu_first+63,
  in_i64_atomic_load32_u = in_cpu_first+64,
  in_i64_atomic_load     = in_cpu_first+65,
  in_i32_atomic_store8   = in_cpu_first+66,
  in_i32_atomic_store16  = in_cpu_first+67,
  in_i32_atomic_store    = in_cpu_first+68,
  in_i64_atomic_store8   = in_cpu_first+69,
  in_i64_atomic_store16  = in_cpu_first+70,
  in_i64_atomic_store32  = in_cpu_first+71,
  in_i64_atomic_store    = in_cpu_first+72

