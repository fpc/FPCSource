{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Processor specific implementation of strpas

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

asm
        {  load the begin of the string in the data cache }
        dcbt    r0,r3
        { load result address in r9 }
        li      r0,__RESULT@l
        addis   r0,__RESULT@ha
        { maxlength }
        li      r10,255
        mtctr   r10
        lwz     r0,0(r0)
        { save address for at the end }
        mr      r3,r0
        { no "subi r0,r0,1" because the first byte = length byte }
        subi    r4,r4,1
LStrPasLoop:
        lbzu    r10,1(r4)
        cmpli   r10,0
        stbu    r10,1(r0)
        bdnzf   cr0*4+eq, LStrPasLoop

        { get remaining count for length }
        mfctr   r10
        { if we stopped because of a terminating #0, decrease the length by 1 }
        mfcr    r4
        subfic  r10,r10,255
        { put "equal" condition bit of cr0 in bit position 31 (= rightmost) }
        { and clear other bits                                              }
        rlwinm  r4,r4,cr0*4+eq+1,31,31
        sub     r10,r10,r4

        { store length }
        stb     r10,0(r3)
end ['r0','r3','r4','r10','cr0','ctr'];

{
  $Log$
  Revision 1.1  2001-09-27 15:30:29  jonas
    * conversion to compilerproc and to structure used by i386 rtl
    * some bugfixes
    * powerpc.inc is almost complete (only fillchar/word/dword, get_frame etc
      and the class helpers are still needed
    - removed unnecessary register saving in set.inc (thanks to compilerproc)
    * use registers reserved for parameters as much as possible instead of
      those reserved for local vars (since those have to be saved by the
      called anyway, while the ones for local vars have to be saved by the
      callee)

}