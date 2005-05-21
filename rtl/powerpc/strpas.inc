{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Processor specific implementation of strpas

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
   r3: result address
   r4: src
}
asm
        { nil? }
        cmplwi   r4, 0
        {  load the begin of the string in the data cache }
        dcbt    0,r4
        { maxlength }
        li      r10,255
        mtctr   r10
        { at LStrPasDone, we set the length of the result to 255 - r10 - r4 }
        { = 255 - 255 - 0 if the soure = nil -> perfect :)                  }
        beq     .LStrPasDone
        { save address for at the end  and use r5 in loop }
        mr      r5,r3
        { no "subi r5,r5,1" because the first byte = length byte }
        subi    r4,r4,1
.LStrPasLoop:
        lbzu    r10,1(r4)
        cmplwi  cr0,r10,0
        stbu    r10,1(r5)
        bdnzf   cr0*4+eq, .LStrPasLoop

        { if we stopped because of a terminating #0, decrease the length by 1 }
        cntlzw  r4,r10
        { get remaining count for length }
        mfctr   r10
        { if r10 was zero (-> stopped because of zero byte), then r4 will be 32 }
        { (32 leading zero bits) -> shr 5 = 1, otherwise this will be zero      }
        srwi    r4,r4,5
.LStrPasDone:
        subfic  r10,r10,255
        sub     r10,r10,r4

        { store length }
        stb     r10,0(r3)
end;

{
  $Log: strpas.inc,v $
  Revision 1.12  2005/02/14 17:13:31  peter
    * truncate log

}
