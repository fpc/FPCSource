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
