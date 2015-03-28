{
    Copyright (c) 2013 by Jonas Maebe

    This unit contains the LLVM instruction tables

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
unit itllvm;

{$i fpcdefs.inc}

interface

    uses
      llvmbase, cgbase;

    const
      llvm_op2str : llvmop2strtable = ('',
        { terminator instructions }
        'ret', 'br', 'switch', 'indirectbr',
        'invoke', 'resume',
        'unreachable',
        { binary operations }
        'add', 'fadd', 'sub', 'fsub', 'mul', 'fmul',
        'udiv','sdiv', 'fdiv', 'urem', 'srem', 'frem',
        { bitwise binary operations }
        'shl', 'lshr', 'ashr', 'and', 'or', 'xor',
        { vector operations }
        'extractelement', 'insertelement', 'shufflevector',
        { aggregate operations }
        'extractvalue', 'insertvalue',
        { memory access and memory addressing operations }
        'alloca',
        'load', 'store',
        'fence', 'cmpxchg', 'atomicrmw',
        'getelementptr',
        { conversion operations }
        'trunc', 'zext', 'sext', 'fptrunc', 'fpext',
        'fptoui', 'fptosi', 'uitofp', 'sitofp',
        'ptrtoint', 'inttoptr',
        'bitcast',
        { other operations }
        'icmp', 'fcmp',
        'phi', 'select', 'call',
        'va_arg', 'landingpad',
        { fpc pseudo opcodes }
        'type', { type definition }
        'invalid1', { la_x_to_inttoptr }
        'invalid2'  { la_ptrtoint_to_x }
      );

      llvm_cond2str : array[topcmp] of ansistring = ('',
        'eq',
        'sgt',
        'slt',
        'sge',
        'sle',
        'ne',
        'ule',
        'ult',
        'uge',
        'ugt'
      );

      llvm_fpcond2str: array[tllvmfpcmp] of ansistring = (
      'invalid',
      'false',
      'oeq', 'ogt', 'oge', 'olt', 'ole', 'one', 'ord',
      'ueq', 'ugt', 'uge', 'ult', 'ule', 'une', 'uno',
      'true');


implementation

end.
