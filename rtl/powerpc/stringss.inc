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
{$i strpas.inc}

function strpcopy(d : pchar;const s : string) : pchar;assembler;
asm
        { get length  }
        lbz     r0,0(r4)
        { put in counter }
        cmplwi  r0,0
        mtctr   r0
        subi    r10,r3,1
        beq     LStrPCopyEmpty
LStrPCopyLoop:
        { copy everything }
        lbzu    r0,1(r4)
        stbu    r0,1(r10)
        bdnz    LStrPCopyLoop
        { add terminating #0 }
        li      r0,0
LStrPCopyEmpty:
        stb     r0,1(r10)
end ['R0','R4','R10','CR0','CTR'];

{
  $Log$
  Revision 1.8  2002-09-11 07:49:40  jonas
    * fixed assembler errors

  Revision 1.7  2002/09/07 16:01:26  peter
    * old logs removed and tabs fixed

  Revision 1.6  2002/09/06 16:58:43  jonas
    * fixed wrong references (used r0 as base register)

  Revision 1.5  2002/08/10 17:14:36  jonas
    * various fixes, mostly changing the names of the modifies registers to
      upper case since that seems to be required by the compiler

}
