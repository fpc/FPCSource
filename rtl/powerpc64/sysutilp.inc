{
    This file is part of the Free Pascal run time library.

    Copyright (c) 2001 by Jonas Maebe,
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

function InterLockedDecrement (var Target: longint) : longint; assembler;
{ input:  address of target in r3 }
{ output: target-1 in r3          }
{ side-effect: target := target-1 }
asm
.LInterLockedDecLoop:
        lwarx   r10,0,r3
        subi    r10,r10,1
        stwcx.  r10,0,r3
        bne     .LInterLockedDecLoop
        mr      r3,r10
end;


function InterLockedIncrement (var Target: longint) : longint; assembler;
{ input:  address of target in r3 }
{ output: target+1 in r3          }
{ side-effect: target := target+1 }
asm
.LInterLockedIncLoop:
        lwarx   r10,0,r3
        addi    r10,r10,1
        stwcx.  r10,0,r3
        bne     .LInterLockedIncLoop
        mr      r3,r10
end;


function InterLockedExchange (var Target: longint;Source : longint) : longint; assembler;
{ input:  address of target in r3, source in r4 }
{ output: target in r3                          }
{ side-effect: target := source                 }
asm
.LInterLockedXchgLoop:
        lwarx   r10,0,r3
        stwcx.  r4,0,r3
        bne     .LInterLockedXchgLoop
        mr      r3,r10
end;


function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint; assembler;
{ input:  address of target in r3, source in r4 }
{ output: target in r3                          }
{ side-effect: target := target+source          }
asm
.LInterLockedXchgAddLoop:
        lwarx   r10,0,r3
        add     r10,r10,r4
        stwcx.  r10,0,r3
        bne     .LInterLockedXchgAddLoop
        sub     r3,r10,r4
end;

