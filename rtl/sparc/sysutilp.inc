{
    $Id$
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

function InterLockedDecrement (var Target: integer) : Integer; assembler;
asm
{$warning FIXME}
end;
(*
{ input:  address of target in r3 }
{ output: target-1 in r3          }
{ side-effect: target := target-1 }
asm
InterLockedDecLoop:
        lwarx   r0,r0,r3
        subi    r0,r0,1
        stwcx.  r0,r0,r3
        bne     InterLockedDecLoop
        mr      r3,r0
end;
*)


function InterLockedIncrement (var Target: integer) : Integer; assembler;
asm
{$warning FIXME}
end;
(*
{ input:  address of target in r3 }
{ output: target+1 in r3          }
{ side-effect: target := target+1 }
asm
InterLockedIncLoop:
        lwarx   r0,r0,r3
        addi    r0,r0,1
        stwcx.  r0,r0,r3
        bne     InterLockedIncLoop
        mr      r3,r0
end;
*)

function InterLockedExchange (var Target: integer;Source : integer) : Integer; assembler;
asm
{$warning FIXME}
end;
(*
{ input:  address of target in r3, source in r4 }
{ output: target in r3                          }
{ side-effect: target := source                 }
asm
InterLockedXchgLoop:
        lwarx   r0,r0,r3
        stwcx.  r4,r0,r3
        bne     InterLockedXchgLoop
        mr      r3,r0
end;
*)


function InterLockedExchangeAdd (var Target: integer;Source : integer) : Integer; assembler;
asm
{$warning FIXME}
end;
(*
{ input:  address of target in r3, source in r4 }
{ output: target in r3                          }
{ side-effect: target := target+source          }
asm
InterLockedXchgAddLoop:
        lwarx   r0,r0,r3
        add     r0,r0,r4
        stwcx.  r0,r0,r3
        bne     InterLockedXchgAddLoop
        sub     r3,r0,r4
end;
*)

{
  $Log$
  Revision 1.1  2003-09-01 20:46:32  peter
    * new dummies

}
