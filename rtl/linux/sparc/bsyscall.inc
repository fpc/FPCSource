{
    $Id$
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
        mov     %o0,%l0
        sethi   %hi(fpc_threadvar_relocate_proc),%o2
        or      %o2,%lo(fpc_threadvar_relocate_proc),%o2
        ld      [%o2],%o3
        subcc   %o3,%g0,%g0
        bne     .LThread
        nop
        sethi   %hi(Errno+4),%o0
        ba      .LNoThread
        or      %o0,%lo(Errno+4),%o0
.LThread:
        sethi   %hi(Errno),%o0
        or      %o0,%lo(Errno),%o0
        call    %o3
        ld      [%o0],%o0
.LNoThread:
        st      %l0,[%o0]
        mov     -1,%o0
.LSyscOK:
        st      [%i0],%o0
        st      [%i0+4],%o1
end;

{
  $Log$
  Revision 1.1  2005-03-03 20:58:38  florian
    + routines in baseunix can be overriden by processor specifics in bsyscall.inc
}