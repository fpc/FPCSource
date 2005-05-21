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
        ta      0x10
        bcc     .LSyscOK
        nop
        call    fpseterrno
        nop
        mov     -1,%i0
        b	.Lend
        nop
.LSyscOK:
        st      %o0,[%i0]
        st      %o1,[%i0+4]
        mov	0,%i0
.Lend:
end;

{
  $Log: bsyscall.inc,v $
  Revision 1.2  2005/03/03 22:02:59  florian
    * final fppipe fix hopefully

  Revision 1.1  2005/03/03 20:58:38  florian
    + routines in baseunix can be overriden by processor specifics in bsyscall.inc
}
