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
        cmpli   r3,0
        { if yes, do nothing }
        beq     LStrLenDone
        subi    r29,r3,1
LStrLenLoop:
        lbzu    r30,1(r29)
        cmpli   r30,0
        bne     LStrLenLoop
        sub     r3,r29,r3
LStrLenDone:
end ['R3','R4','R29','R30','CR0'];

{
  $Log$
  Revision 1.2  2002-08-10 17:14:36  jonas
    * various fixes, mostly changing the names of the modifies registers to
      upper case since that seems to be required by the compiler

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