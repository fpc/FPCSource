{
    Copyright (c) 2007-2008 by Jonas Maebe

    Contains the base types for LLVM

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
{ This Unit contains the base types for LLVM
}
unit llvmbase;

{$i fpcdefs.inc}

interface

uses
  strings,globtype,
  cutils,cclasses,aasmbase,cpuinfo,cgbase;


{*****************************************************************************
                                Assembler Opcodes
*****************************************************************************}

    type
      tllvmop = (la_none,
        { terminator instructions }
        la_ret, la_br, la_switch, la_invoke, la_unwind, la_unreachable,
        { binary operations }
        la_add, la_sub, la_mul, la_div, la_urem, la_srem, la_frem, 
        { bitwise binary operations }
        la_shl, la_lshr, la_ashr, la_and, la_or, la_xor,
        { vector operations }
        la_extractelement, la_insertelement, la_shufflevector,
        { memory access and memory addressing operations }
        la_malloc, la_free, la_alloca,
        la_load, la_store, la_getelementptr,
        { conversion operations }
        la_trunc, la_zext, la_sext, la_fptrunc, la_fpext,
        la_fptoui, la_fptosi, la_uitofp, la_sitofp,
        la_ptrtoint, la_inttoptr,
        la_bitcast,
        { other operations }
        la_icmp, la_fcmp,
        la_phi, la_select, la_call, la_va_arg, la_getresult,
        la_type);

      {# This should define the array of instructions as string }
      llvmop2strtable=array[tllvmop] of string[8];

  implementation

end.
