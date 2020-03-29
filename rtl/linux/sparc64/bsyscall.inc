{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 by Michael Van Canneyt,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$define FPC_BASEUNIX_HAS_FPPIPE}
Function fppipe(var fildes : tfildes):cint;assembler;
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
        mov     42,%g1
        mov     %i0,%o0
        ta      0x6d
        bcc     %xcc,.LSyscOK
        nop
        call    fpseterrno
        nop
        b       .Lend
        { use delay slot }
        mov     -1,%o0
.LSyscOK:
        st      %o0,[%i0]
        st      %o1,[%i0+4]
        mov	0,%i0
.Lend:
end;

