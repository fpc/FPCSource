{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2016 by the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{**********************************************************************
          Declarations for SetJmp/LongJmp
 **********************************************************************}

{$if defined(fpu68881)}
type
  Tsizefpureg = packed array[0..11] of byte;
{$elseif defined(fpucoldfire)}
type
  Tsizefpureg = double;
{$endif}

Type
  jmp_buf = packed record
    fp : dword; { offset  0}   { frame pointer  (also a6)    }
    sp : dword; { offset  4}   { stack pointer  (also a7)    }
    pc : dword; { offset  8}   { program counter   }

    { data registers (d2, d3, d4, d5, d6, d7) }
    { offsets:        12, 16, 20, 24, 28, 32 }
    dregs : array[2..7] of dword;
    { address registers (a2, a3, a4, a5), a6 and a7 are fp and sp respectively }
    { offsets:           36, 40, 44, 48}
    aregs : array[2..5] of dword;
{$if defined(fpu68881) or defined(fpucoldfire)}
    { offset: 52, size: 48 or 72 bytes, depending on FPU register size }
    fregs : array[2..7] of tsizefpureg;
{$endif}
    { total size: 52, 100 or 124 bytes }
  end;

  PJmp_buf = ^jmp_buf;

Function Setjmp (Var S : Jmp_buf) : longint;[external name 'FPC_SETJMP'];
Procedure longjmp (Var S : Jmp_buf; value : longint);[external name 'FPC_LONGJMP'];
