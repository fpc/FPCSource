{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Jonas Maebe, member of the
    Free Pascal development team

    Processor dependent part of strings.pp, not shared with
    sysutils unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

function strpas(p : pchar) : string; assembler;
asm
        {  load the begin of the string in the data cache }
        dcbt    r0,r3
        { load result address in r9 }
        li      r29,__RESULT@l
        addis   r29,__RESULT@ha
        { maxlength }
        li      r30,255
        mtctr   r30
        lwz     r29,0(r29)
        { save address for at the end }
        mr      r3,r29
        { no "subi r29,r29,1" because the first byte = length byte }
        subi    r4,r4,1
LStrPasLoop:
        lbzu    r30,1(r4)
        cmpli   r30,0
        stbu    r30,1(r29)
        bdnzf   cr0*4+eq, LStrPasLoop

        { get remaining count for length }
        mfctr   r30
        { if we stopped because of a terminating #0, decrease the length by 1 }
        mfcr    r4
        subfic  r30,r30,255
        { put "equal" condition bit of cr0 in bit position 31 (= rightmost) }
        { and clear other bits                                              }
        rlwinm  r4,r4,cr0*4+eq+1,31,31
        sub     r30,r30,r4

        { store length }
        stb     r30,0(r3)
end ['r3','r4','r29','r30','cr0','ctr'];

function strpcopy(d : pchar;const s : string) : pchar;assembler;
asm
        { get length  }
        lbz     r30,0(r4)
        { put in counter }
        cmpli   r30,0
        mtctr   r30
        subi    r29,r3,1
        beq     LStrPCopyEmpty
LStrPCopyLoop:
        { copy everything }
        lbzu    r30,1(r4)
        stbu    r30,1(r29)
        bdnz    LStrPCopyLoop
        { add terminating #0 }
        li      r30,0
LStrPCopyEmpty:
        stb     r30,1(r29)
end ['r4','r29','r30','cr0','ctr'];

{
  $Log$
  Revision 1.3  2001-07-07 12:46:12  jonas
    * some small bugfixes and cache optimizations

  Revision 1.2  2001/02/11 12:15:03  jonas
    * some small optimizations and bugfixes

  Revision 1.1  2001/02/10 16:10:32  jonas
    * initial implementation: everything implemented, nothing tested


}
