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
        cmpli   r0,0
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
  Revision 1.6  2002-09-06 16:58:43  jonas
    * fixed wrong references (used r0 as base register)

  Revision 1.5  2002/08/10 17:14:36  jonas
    * various fixes, mostly changing the names of the modifies registers to
      upper case since that seems to be required by the compiler

  Revision 1.4  2001/09/27 15:30:29  jonas
    * conversion to compilerproc and to structure used by i386 rtl
    * some bugfixes
    * powerpc.inc is almost complete (only fillchar/word/dword, get_frame etc
      and the class helpers are still needed
    - removed unnecessary register saving in set.inc (thanks to compilerproc)
    * use registers reserved for parameters as much as possible instead of
      those reserved for local vars (since those have to be saved by the
      called anyway, while the ones for local vars have to be saved by the
      callee)

  Revision 1.3  2001/07/07 12:46:12  jonas
    * some small bugfixes and cache optimizations

  Revision 1.2  2001/02/11 12:15:03  jonas
    * some small optimizations and bugfixes

  Revision 1.1  2001/02/10 16:10:32  jonas
    * initial implementation: everything implemented, nothing tested


}
