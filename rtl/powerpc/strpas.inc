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
        beq     LStrPasDone
        { save address for at the end  and use r5 in loop }
        mr      r5,r3
        { no "subi r5,r5,1" because the first byte = length byte }
        subi    r4,r4,1
LStrPasLoop:
        lbzu    r10,1(r4)
        cmplwi  cr0,r10,0
        stbu    r10,1(r5)
        bdnzf   cr0*4+eq, LStrPasLoop

        { if we stopped because of a terminating #0, decrease the length by 1 }
        cntlzw  r4,r10
        { get remaining count for length }
        mfctr   r10
        { if r10 was zero (-> stopped because of zero byte), then r4 will be 32 }
        { (32 leading zero bits) -> shr 5 = 1, otherwise this will be zero      }
        srwi    r4,r4,5
LStrPasDone:
        subfic  r10,r10,255
        sub     r10,r10,r4

        { store length }
        stb     r10,0(r3)
end;

{
  $Log$
  Revision 1.10  2003-06-14 12:41:09  jonas
    * fixed compilation problems (removed unnecessary modified registers
      lists from procedures)

  Revision 1.9  2003/04/27 16:25:08  jonas
    * support nil as parameter and some other fixes

  Revision 1.8  2002/10/17 10:14:46  jonas
    * fixed srwi's after cntlzw instructions (should be 5 instead of 31)

  Revision 1.7  2002/09/11 07:49:40  jonas
    * fixed assembler errors

  Revision 1.6  2002/09/07 16:01:26  peter
    * old logs removed and tabs fixed

  Revision 1.5  2002/08/18 22:11:10  florian
    * fixed remaining assembler errors

  Revision 1.4  2002/08/18 21:37:48  florian
    * several errors in inline assembler fixed

  Revision 1.3  2002/08/10 17:14:36  jonas
    * various fixes, mostly changing the names of the modifies registers to
      upper case since that seems to be required by the compiler

}
