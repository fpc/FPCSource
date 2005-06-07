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
    fp : longint;                    { frame pointer     }
    sp : longint;                    { stack pointer     }
    pc : longint;                    { program counter   }
    aregs : array[0..3] of dword;    { address registers (a2,a3,a4,a5) }
  end;
  PJmp_buf = ^jmp_buf;

Function Setjmp (Var S : Jmp_buf) : longint;
Procedure longjmp (Var S : Jmp_buf; value : longint);

