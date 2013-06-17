{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2000 by Jonas Maebe, member of the
    Free Pascal development team

    Processor dependent part of strings.pp, that can be shared with
    sysutils unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifndef CPUTHUMB}
{$ifndef FPC_UNIT_HAS_STRUPPER}
{$define FPC_UNIT_HAS_STRUPPER}
function strupper(p : pchar) : pchar;assembler;nostackframe;
asm
        mov     ip, r0   // Don't change r0, because thats our return value

        ldrb    r1, [ip] // First loop does not postindex
.LByteLoop:
        cmp     r1, #0
{$if defined(cputhumb2)}
        it eq
{$endif}
{$if defined(cpuarmv3) or defined(cpuarmv4)}
        moveq   pc, lr
{$else}
        bxeq    lr
{$endif}

        sub     r2, r1, #97   // Normalize to zero
        cmp     r2, #25       // temp >= 0 and temp <=25
{$if defined(cputhumb2)}
        itt ls
{$endif}
        subls   r1, r1, #32   // is lowercase, make uppercase
        strlsb  r1, [ip]      // Store only on change
        ldrb    r1, [ip, #1]! // Loading here utilizes a load delay slot
        b       .LByteLoop
end;
{$endif FPC_UNIT_HAS_STRUPPER}

{$ifndef FPC_UNIT_HAS_STRLOWER}
{$define FPC_UNIT_HAS_STRLOWER}
function strlower(p : pchar) : pchar;assembler;nostackframe;
asm
        mov     ip, r0   // Don't change r0, because thats our return value

        ldrb    r1, [ip] // First loop does not postindex
.LByteLoop:
        cmp     r1, #0
{$if defined(cputhumb2)}
        it eq
{$endif}
{$if defined(cpuarmv3) or defined(cpuarmv4)}
        moveq   pc, lr
{$else}
        bxeq    lr
{$endif}

        sub     r2, r1, #65   // Normalize to zero
        cmp     r2, #25       // temp >= 0 and temp <=25
{$if defined(cputhumb2)}
        itt ls
{$endif}
        addls   r1, r1, #32   // Is uppercase, make lowercase
        strlsb  r1, [ip]      // Store only on change
        ldrb    r1, [ip, #1]! // Loading here utilizes a load delay slot
        b       .LByteLoop
end;
{$endif FPC_UNIT_HAS_STRLOWER}
{$endif CPUTHUMB}
