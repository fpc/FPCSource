{
    $Id$
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
        cmplwi  cr0,r3,0
        { if yes, do nothing }
        beq     LStrLenDone
        subi    r29,r3,1
LStrLenLoop:
        lbzu    r30,1(r29)
        cmplwi  cr0,r30,0
        bne     LStrLenLoop
        sub     r3,r29,r3
LStrLenDone:
end;

{
  $Log$
  Revision 1.6  2003-06-14 12:41:09  jonas
    * fixed compilation problems (removed unnecessary modified registers
      lists from procedures)

  Revision 1.5  2002/09/11 07:49:40  jonas
    * fixed assembler errors

  Revision 1.4  2002/09/07 16:01:26  peter
    * old logs removed and tabs fixed

  Revision 1.3  2002/08/18 21:37:48  florian
    * several errors in inline assembler fixed

  Revision 1.2  2002/08/10 17:14:36  jonas
    * various fixes, mostly changing the names of the modifies registers to
      upper case since that seems to be required by the compiler

}
