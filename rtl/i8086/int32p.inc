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


{$define FPC_SYSTEM_HAS_DIV_DWORD}
function fpc_div_dword( n, z: dword ): dword; [public, alias:'FPC_DIV_DWORD']; compilerproc;
begin
{ routine contributed by Max Nazhalov }
  result := 0;
  if n=0 then
    HandleErrorAddrFrameInd(200,get_pc_addr,get_frame);
  asm
      mov    ax,word [z]
      mov    dx,word [z+2]
      mov    bx,word [n]
      mov    cx,word [n+2]
      // check for underflow: z<n
      mov    si,dx
      cmp    ax,bx
      sbb    si,cx
      jc     @@3
      // select one of 3 trivial cases
      test   cx,cx
      jnz    @@1
      cmp    dx,bx
      jnc    @@0
      // (i) single division: n<=0xFFFF, z<=(n<<16)-1
      div    bx
      mov    word [result],ax
      jmp    @@3
@@0:  // (ii) two divisions: n<=0xFFFF, z>(n<<16)-1
      //  q1 := [0:z1] div n; r := [0:z1] mod n;
      //  q0 := [r:z0] div n;
      xchg   ax,cx
      xchg   ax,dx
    { dx=0, ax=z1, cx=z0 }
      div    bx
      xchg   ax,cx
    { dx=r, ax=z0, cx=q1 }
      div    bx
      mov    word [result],ax
      mov    word [result+2],cx
      jmp    @@3
@@1:  // (iii) long divisor: n>=0x10000 (hence q<=0xFFFF)
      // Special case of the generic "schoolbook" division [see e.g. Knuth]:
      //  1. normalize divisor: [n1:n0] := n<<m, so that 0x8000<=n1<=0xFFFF
      //     n>=0x10000 -> m<=15
      //  2. adjust divident accordingly: [z2:z1:z0] := z<<m
      //     m<=15 -> z2<=0x7FFF
      // implementation: instead do >> dropping n0 and z0
      mov    si,bx // save n0
      mov    di,cx // save n1
      test   ch,ch
      jz     @@2
      mov    bl,bh
      mov    bh,cl
      mov    cl,ch
      mov    al,ah
      mov    ah,dl
      mov    dl,dh
      xor    dh,dh
@@2:  // repeat >> 1..8 times resulting in [dx:ax]=[z2:z1] and bx=n1
      shr    cl,1
      rcr    bx,1
      shr    dx,1
      rcr    ax,1
      test   cl,cl
      jnz    @@2
      //  3. estimate quotient: q_hat := [z2:z1]/n1
      //     Division never overflows since z2<=0x7FFF and n1>0x7FFF
      div    bx
      //  4. multiply & subtract calculating remainder:
      //     r := z-n*q_hat (z and n are original)
      //  5. adjust quotient: while (r<0) do { q_hat-=1; r+=n };
      //     theoretically, 0..2 iterations are required [see e.g. Knuth];
      //     in practice, with such initial data, at most one iteration
      //     is needed (no disproof has been found yet; and if it will
      //     ever be found -- it also should raise doubts about the i386
      //     fpc_div_qword helper again; see FPC mantis #23963)
      mov    cx,ax // save q_hat
      mul    si
      mov    bx,ax
      mov    si,dx
      mov    ax,cx
      mul    di
      xor    di,di
      add    ax,si
      adc    dx,di // [dx:ax:bx] := n*q_hat; di=0
      mov    si,word [z]
      sub    si,bx
      mov    si,word [z+2]
      sbb    si,ax
      sbb    di,dx
      sbb    cx,0
      //  6. done: q := [0:cx]
      mov    word [result],cx
@@3:
  end;
end;


{$define FPC_SYSTEM_HAS_MOD_DWORD}
function fpc_mod_dword( n, z: dword ): dword; [public, alias:'FPC_MOD_DWORD']; compilerproc;
begin
{ routine contributed by Max Nazhalov }
  result := z;
  if n=0 then
    HandleErrorAddrFrameInd(200,get_pc_addr,get_frame);
  asm
      mov    ax,word [z]
      mov    dx,word [z+2]
      mov    bx,word [n]
      mov    cx,word [n+2]
      // check for underflow: z<n
      mov    si,dx
      cmp    ax,bx
      sbb    si,cx
      jc     @@4
      // select one of 3 trivial cases
      test   cx,cx
      jnz    @@1
      cmp    dx,bx
      jnc    @@0
      // (i) single division: n<=0xFFFF, z<=(n<<16)-1
      div    bx
      jmp    @@3 // r=cx:dx (cx=0)
@@0:  // (ii) two divisions: n<=0xFFFF, z>(n<<16)-1
      //  q1 := [0:z1] div n; r := [0:z1] mod n;
      //  q0 := [r:z0] div n; r := [r:z0] mod n;
      xchg   ax,cx
      xchg   ax,dx
    { dx=0, ax=z1, cx=z0 }
      div    bx
      mov    ax,cx
      xor    cx,cx
    { dx=r, ax=z0, cx=0 }
      div    bx
      jmp    @@3 // r=cx:dx (cx=0)
@@1:  // (iii) long divisor: n>=0x10000 (hence q<=0xFFFF)
      // Special case of the generic "schoolbook" division [see e.g. Knuth]:
      //  1. normalize divisor: [n1:n0] := n<<m, so that 0x8000<=n1<=0xFFFF
      //     n>=0x10000 -> m<=15
      //  2. adjust divident accordingly: [z2:z1:z0] := z<<m
      //     m<=15 -> z2<=0x7FFF
      // implementation: instead do >> dropping n0 and z0
      mov    si,bx // save n0
      mov    di,cx // save n1
      test   ch,ch
      jz     @@2
      mov    bl,bh
      mov    bh,cl
      mov    cl,ch
      mov    al,ah
      mov    ah,dl
      mov    dl,dh
      xor    dh,dh
@@2:  // repeat >> 1..8 times resulting in [dx:ax]=[z2:z1] and bx=n1
      shr    cl,1
      rcr    bx,1
      shr    dx,1
      rcr    ax,1
      test   cl,cl
      jnz    @@2
      //  3. estimate quotient: q_hat := [z2:z1]/n1
      //     Division never overflows since z2<=0x7FFF and n1>0x7FFF
      div    bx
      //  4. multiply & subtract calculating remainder:
      //     r := z-n*q_hat (z and n are original)
      //  5. adjust quotient: while (r<0) do { q_hat-=1; r+=n };
      //     theoretically, 0..2 iterations are required [see e.g. Knuth];
      //     in practice, with such initial data, at most one iteration
      //     is needed (no disproof has been found yet; and if it will
      //     ever be found -- it also should raise doubts about the i386
      //     fpc_div_qword helper again; see FPC mantis #23963)
      mov    cx,ax // save q_hat
      mul    si
      mov    bx,ax
      mov    si,dx
      mov    ax,cx
      mul    di
      xor    di,di
      add    ax,si
      adc    dx,di // [dx:ax:bx] := n*q_hat; di=0
      mov    si,word [z]
      mov    cx,word [z+2]
      sub    si,bx
      sbb    cx,ax
      sbb    di,dx
      mov    dx,si
      jnc    @@3
      add    dx,word [n]
      adc    cx,word [n+2]
@@3:  // done: r=cx:dx
      mov    word [result],dx
      mov    word [result+2],cx
@@4:
  end;
end;
