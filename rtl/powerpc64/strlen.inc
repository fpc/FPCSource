{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Processor specific implementation of strlen

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ in: p in r3                }
{ out: result (length) in r3 }
asm
        {  load the begin of the string in the data cache }
        dcbt    0,r3
        { empty/invalid string? }
        cmpldi  cr0,r3,0
        { if yes, do nothing }
        beq     .LStrLenDone
        subi    r9,r3,1
.LStrLenLoop:
        lbzu    r10,1(r9)
        cmpldi  cr0,r10,0
        bne     .LStrLenLoop
        sub     r3,r9,r3
.LStrLenDone:
end;

