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
        la_ret, la_br, la_switch, la_indirectbr,
        la_invoke, la_unwind,
        la_unreachable,
        { binary operations }
        la_add, la_fadd, la_sub, la_fsub, la_mul, la_fmul,
        la_udiv,la_sdiv, la_fdiv, la_urem, la_srem, la_frem,
        { bitwise binary operations }
        la_shl, la_lshr, la_ashr, la_and, la_or, la_xor,
        { vector operations }
        la_extractelement, la_insertelement, la_shufflevector,
        { memory access and memory addressing operations }
        la_alloca,
        la_load, la_store, la_getelementptr,
        { conversion operations }
        la_trunc, la_zext, la_sext, la_fptrunc, la_fpext,
        la_fptoui, la_fptosi, la_uitofp, la_sitofp,
        la_ptrtoint, la_inttoptr,
        la_bitcast,
        { other operations }
        la_icmp, la_fcmp,
        la_phi, la_select, la_call, la_va_arg,
        la_type);

    type
      tllvmopsize = (SL_NO,
        { i1, i8, ... }
        SL_I1, SL_I8, SL_I16, SL_I32, SL_I64, SL_I128,
        { pointer to i1, ... }
        SL_I1P, SL_I8P, SL_I16P, SL_I32P, SL_I64P, SL_I128P,
        { single, double, extended, 128 bit float, 128 bit float composed of two doubles }
        SL_FLOAT, SL_DOUBLE, SL_X86_FP80, SL_FP128, SL_PPC_FP128,
        SL_FLOATP, SL_DOUBLEP, SL_X86_FP80P, SL_FP128P, SL_PPC_FP128P,
        { vectors (number = elementsize; total size = native vector register size) }
        SL_M8,SL_M16,SL_M32,SL_M64,SL_M128,
        SL_M8P,SL_M16P,SL_M32P,SL_M64P,SL_M128P
        );


      {# This should define the array of instructions as string }
      llvmop2strtable=array[tllvmop] of string[8];

    const
{$if defined(powerpc) or defined(powerpc64) or defined(sparc)}
      LLVMFLOAT128=SL_PPC_FP128;
      LLVMFLOAT128P=SL_PPC_FP128P;
{$else}
      LLVMFLOAT128=SL_FP128;
      LLVMFLOAT128P=SL_FP128P;
{$endif}
      cgsize2llvmopsize: array[tcgsize] of tllvmopsize = (SL_NO,
        { integer registers }
        SL_I8,SL_I16,SL_I32,SL_I64,SL_I128,SL_I8,SL_I16,SL_I32,SL_I64,SL_I128,
        { single,double,extended,comp,float128 }
        SL_FLOAT,SL_DOUBLE,SL_X86_FP80,SL_NO,LLVMFLOAT128,
        { multi-media sizes: split in byte, word, dword, ... }
        SL_M8,SL_M16,SL_M32,SL_M64,SL_M128,
        SL_M8,SL_M16,SL_M32,SL_M64,SL_M128);

      cgsize2llvmptropsize: array[tcgsize] of tllvmopsize = (SL_NO,
        { integer registers }
        SL_I8P,SL_I16P,SL_I32P,SL_I64P,SL_I128P,SL_I8P,SL_I16P,SL_I32P,SL_I64P,SL_I128P,
        { single,double,extended,comp,float128 }
        SL_FLOATP,SL_DOUBLEP,SL_X86_FP80P,SL_NO,LLVMFLOAT128P,
        { multi-media sizes: split in byte, word, dword, ... }
        SL_M8P,SL_M16P,SL_M32P,SL_M64P,SL_M128P,
        SL_M8P,SL_M16P,SL_M32P,SL_M64P,SL_M128P);

      llvmpsize2ptropsize: array[tllvmopsize] of tllvmopsize = (SL_NO,
        SL_I1P,SL_I8P,SL_I16P,SL_I32P,SL_I64P,SL_I128P,
        SL_NO,SL_NO,SL_NO,SL_NO,SL_NO,SL_NO,
        SL_FLOATP, SL_DOUBLEP, SL_X86_FP80P, SL_FP128P, SL_PPC_FP128P,
        SL_NO,SL_NO,SL_NO,SL_NO,SL_NO,
        SL_M8P,SL_M16P,SL_M32P,SL_M64P,SL_M128P,
        SL_NO,SL_NO,SL_NO,SL_NO,SL_NO);

(*
  need to resolve conflicts with native tresflags: in case llvm is used, we
  still use parts of the original code generator as well (e.g., to generate
  the entry/exit code for pure assembler routines), so we can't just replace its
  resflags
    type
      tresflags = type tregister;
*)

implementation

end.
