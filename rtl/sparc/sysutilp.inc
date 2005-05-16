{
    $Id: sysutilp.inc,v 1.5 2005/03/12 18:45:15 florian Exp $
    This file is part of the Free Pascal run time library.

    Copyright (c) 2003 by Peter Vreman,
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ ---------------------------------------------------------------------
  This include contains cpu-specific routines
  ---------------------------------------------------------------------}

var
  fpc_system_lock : byte;external name 'fpc_system_lock';

function InterLockedDecrement (var Target: longint) : longint; assembler; nostackframe;
asm
  { usually, we shouldn't lock here so saving the stack frame for these extra intructions is
    worse the effort, especially while waiting :)
  }
.LInterLockedDecrement1:
  sethi %hi(fpc_system_lock), %g1
  or %g1,%lo(fpc_system_lock), %g1
  ldstub [%g1],%g1
  cmp %g1,0
  bne .LInterLockedDecrement1
  nop

  ld [%o0],%g1
  sub %g1,1,%g1
  st %g1,[%o0]

  mov %g1,%o0

  { unlock }
  sethi %hi(fpc_system_lock), %g1
  or %g1,%lo(fpc_system_lock), %g1
  stb %g0,[%g1]
end;

function InterLockedIncrement (var Target: longint) : longint; assembler; nostackframe;
asm
  { usually, we shouldn't lock here so saving the stack frame for these extra intructions is
    worse the effort, especially while waiting :)
  }
.LInterLockedIncrement1:
  sethi %hi(fpc_system_lock), %g1
  or %g1,%lo(fpc_system_lock), %g1
  ldstub [%g1],%g1
  cmp %g1,0
  bne .LInterLockedIncrement1
  nop

  ld [%o0],%g1
  add %g1,1,%g1
  st %g1,[%o0]

  mov %g1,%o0

  { unlock }
  sethi %hi(fpc_system_lock), %g1
  or %g1,%lo(fpc_system_lock), %g1
  stb %g0,[%g1]
end;


function InterLockedExchange (var Target: longint;Source : longint) : longint; assembler; nostackframe;
asm
  { usually, we shouldn't lock here so saving the stack frame for these extra intructions is
    worse the effort, especially while waiting :)
  }
.LInterLockedExchange1:
  sethi %hi(fpc_system_lock), %g1
  or %g1,%lo(fpc_system_lock), %g1
  ldstub [%g1],%g1
  cmp %g1,0
  bne .LInterLockedExchange1
  nop

  ld [%o0],%g1
  st %o1,[%o0]

  mov %g1,%o0

  { unlock }
  sethi %hi(fpc_system_lock), %g1
  or %g1,%lo(fpc_system_lock), %g1
  stb %g0,[%g1]
end;


function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint; assembler; nostackframe;
asm
  { usually, we shouldn't lock here so saving the stack frame for these extra intructions is
    worse the effort, especially while waiting :)
  }
.LInterLockedExchangeAdd1:
  sethi %hi(fpc_system_lock), %g1
  or %g1,%lo(fpc_system_lock), %g1
  ldstub [%g1],%g1
  cmp %g1,0
  bne .LInterLockedExchangeAdd1
  nop

  ld [%o0],%g1
  add %g1,%o1,%o1
  st %o1,[%o0]

  mov %g1,%o0

  { unlock }
  sethi %hi(fpc_system_lock), %g1
  or %g1,%lo(fpc_system_lock), %g1
  stb %g0,[%g1]
end;


{
  $Log: sysutilp.inc,v $
  Revision 1.5  2005/03/12 18:45:15  florian
    * InterlockedDecrement fixed

  Revision 1.4  2005/02/14 17:13:31  peter
    * truncate log

}
