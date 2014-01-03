{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2013 by the Free Pascal development team

    This file contains some helper routines for longint and dword

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$define FPC_SYSTEM_HAS_MUL_DWORD}
function fpc_mul_dword( f1, f2: dword; checkoverflow: boolean ): dword; [public,alias: 'FPC_MUL_DWORD']; compilerproc;
begin
{ routine contributed by Max Nazhalov

  32-bit multiplications summary:
  f1 = A1*$10000+A0
  f2 = B1*$10000+B0
  (A1:A0*B1:B0) = (A1*B1)<<32 + (A1*B0)<<16 + (A0*B1)<<16 + (A0*B0)

  A1*B1 [only needed for overflow checking; overflow if <>0]
  A1*B0
  A0*B1
  A0:B0
}
  asm
    mov     cx,word[f1]
    mov     ax,word[f1+2]
    mov     di,word[f2]
    mov     si,word[f2+2]
    cmp     checkoverflow,0
    jne     @@checked
    mul     di
    xchg    ax,si
    mul     cx
    add     si,ax
    mov     ax,di
    mul     cx
    add     dx,si
    jmp     @@done
@@checked:
    test    ax,ax
    jz      @@skip
    test    si,si
    jnz     @@done
    mul     di
    test    dx,dx
    jnz     @@done
@@skip:
    xchg    ax,si
    mul     cx
    test    dx,dx
    jnz     @@done
    add     si,ax
    jc      @@done
    mov     ax,di
    mul     cx
    add     dx,si
    jc      @@done
    // checked and succeed
    mov     checkoverflow,0
@@done:
    mov     word[result],ax
    mov     word[result+2],dx
  end [ 'ax','cx','dx','si','di' ];
  if checkoverflow then
    HandleErrorAddrFrameInd(215,get_pc_addr,get_frame);
end;

