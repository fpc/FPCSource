{
    $Id$
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

function InterLockedDecrement (var Target: integer) : Integer; assembler;
{ input:  address of target in r3 }
{ output: target-1 in r3          }
{ side-effect: target := target-1 }
asm
.LInterLockedDecLoop:
        lwarx   r10,r0,r3
        subi    r10,r10,1
        stwcx.  r10,r0,r3
        bne     .LInterLockedDecLoop
        mr      r3,r10
end;


function InterLockedIncrement (var Target: integer) : Integer; assembler;
{ input:  address of target in r3 }
{ output: target+1 in r3          }
{ side-effect: target := target+1 }
asm
.LInterLockedIncLoop:
        lwarx   r10,r0,r3
        addi    r10,r10,1
        stwcx.  r10,r0,r3
        bne     .LInterLockedIncLoop
        mr      r3,r10
end;


function InterLockedExchange (var Target: integer;Source : integer) : Integer; assembler;
{ input:  address of target in r3, source in r4 }
{ output: target in r3                          }
{ side-effect: target := source                 }
asm
.LInterLockedXchgLoop:
        lwarx   r10,r0,r3
        stwcx.  r4,r0,r3
        bne     .LInterLockedXchgLoop
        mr      r3,r10
end;


function InterLockedExchangeAdd (var Target: integer;Source : integer) : Integer; assembler;
{ input:  address of target in r3, source in r4 }
{ output: target in r3                          }
{ side-effect: target := target+source          }
asm
.LInterLockedXchgAddLoop:
        lwarx   r10,r0,r3
        add     r10,r10,r4
        stwcx.  r10,r0,r3
        bne     .LInterLockedXchgAddLoop
        sub     r3,r10,r4
end;


{
  $Log$
  Revision 1.6  2003-12-28 20:55:57  jonas
    * fixed *locked* routines

  Revision 1.5  2003/11/29 16:27:19  jonas
    * fixed several ppc assembler reader related problems
    * local vars in assembler procedures now start at offset 4
    * fixed second_int_to_bool (apparently an integer can be in  LOC_JUMP??)

  Revision 1.4  2003/08/24 20:50:11  olle
    * changed used scratchreg from r0 to r10

  Revision 1.3  2003/04/24 12:13:23  florian
    * fixed assembler errors

  Revision 1.2  2002/09/07 16:01:26  peter
    * old logs removed and tabs fixed

}
