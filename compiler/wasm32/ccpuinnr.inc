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
  in_wasm32_atomic_fence = in_cpu_first+6

