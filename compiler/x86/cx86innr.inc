{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2018 by the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

  in_x86_inportb = fpc_in_cpu_first,
  in_x86_inportw = fpc_in_cpu_first+1,
  in_x86_inportl = fpc_in_cpu_first+2,
  in_x86_outportb = fpc_in_cpu_first+3,
  in_x86_outportw = fpc_in_cpu_first+4,
  in_x86_outportl = fpc_in_cpu_first+5,
  in_x86_cli      = fpc_in_cpu_first+6,
  in_x86_sti      = fpc_in_cpu_first+7,
  in_x86_get_cs   = fpc_in_cpu_first+8,
  in_x86_get_ss   = fpc_in_cpu_first+9,
  in_x86_get_ds   = fpc_in_cpu_first+10,
  in_x86_get_es   = fpc_in_cpu_first+11,
  in_x86_get_fs   = fpc_in_cpu_first+12,
  in_x86_get_gs   = fpc_in_cpu_first+13
