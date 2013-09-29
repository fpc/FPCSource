{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by xxxx
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{**********************************************************************
          Declarations for SetJmp/LongJmp
 **********************************************************************}

Type
  jmp_buf = packed record
    fp : dword; { offset  0}   { frame pointer  (also a6)    }
    sp : dword; { offset  4}   { stack pointer  (also a7)    }
    pc : dword; { offset  8}   { program counter   }
    { There is no point in saving d0, as this is the register used to
      return the vlaue of a function, which must be either zero
      if called from SetJmp or value if called from LongJmp }
    { data registers (d1, d2, d3, d4, d5, d6, d7) }
    { offsets:        12, 16, 20, 24, 28, 32, 36 }
    dregs : array[1..7] of dword;
    { address registers (a0, a1, a2, a3, a4, a5), a6 and a7 are fp and sp respectively }
    { offsets:           40, 44, 48, 52, 56, 60 }
    aregs : array[0..5] of dword;
    {Total size 64 bytes }
  end;

  PJmp_buf = ^jmp_buf;

Function Setjmp (Var S : Jmp_buf) : longint;[external name 'FPC_SETJMP'];
Procedure longjmp (Var S : Jmp_buf; value : longint);[external name 'FPC_LONGJMP'];

