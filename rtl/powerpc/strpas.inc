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
        {  load the begin of the string in the data cache }
        dcbt    0,r4
        { maxlength }
        li      r10,255
        mtctr   r10
        lwz     r0,0(r3)
        { save address for at the end }
        mr      r3,r0
        { no "subi r0,r0,1" because the first byte = length byte }
        subi    r4,r4,1
LStrPasLoop:
        lbzu    r10,1(r4)
        cmpli   cr0,r10,0
        stbu    r10,1(r0)
        bdnzf   cr0*4+eq, LStrPasLoop

        { if we stopped because of a terminating #0, decrease the length by 1 }
        cntlzw  r4,r10
        { get remaining count for length }
        mfctr   r10
        { if r10 was zero (-> stopped because of zero byte), then r4 will be 32 }
        { (32 leading zero bits) -> shr 31 = 1, otherwise this will be zero     }
        srwi    r4,r4,31
        subfic  r10,r10,255
        sub     r10,r10,r4

        { store length }
        stb     r10,0(r3)
end ['R0','R3','R4','R10','CR0','CTR'];

{
  $Log$
  Revision 1.4  2002-08-18 21:37:48  florian
    * several errors in inline assembler fixed

  Revision 1.3  2002/08/10 17:14:36  jonas
    * various fixes, mostly changing the names of the modifies registers to
      upper case since that seems to be required by the compiler

  Revision 1.2  2001/09/28 13:23:44  jonas
    * small optimization

  Revision 1.1  2001/09/27 15:30:29  jonas
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
