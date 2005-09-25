{
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

{$ifndef FPC_UNIT_HAS_STRPCOPY}
{$define FPC_UNIT_HAS_STRPCOPY}
function strpcopy(d : pchar;const s : string) : pchar;assembler;
asm
        { get length  }
        lbz     r0,0(r4)
        { put in counter }
        cmpldi  r0,0
        mtctr   r0
        subi    r10,r3,1
        beq     .LStrPCopyEmpty
.LStrPCopyLoop:
        { copy everything }
        lbzu    r0,1(r4)
        stbu    r0,1(r10)
        bdnz    .LStrPCopyLoop
        { add terminating #0 }
        li      r0,0
.LStrPCopyEmpty:
        stb     r0,1(r10)
end;
{$endif FPC_UNIT_HAS_STRPCOPY}

