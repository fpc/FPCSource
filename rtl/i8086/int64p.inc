{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2013 by the Free Pascal development team

    This file contains some helper routines for int64 and qword

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$define FPC_SYSTEM_HAS_MUL_QWORD}
function fpc_mul_qword( f1, f2: qword; checkoverflow: longbool ): qword; [public,alias: 'FPC_MUL_QWORD']; compilerproc;
begin
{ routine contributed by Max Nazhalov

64-bit multiplication via 16-bit digits: (A3:A2:A1:A0)*(B3:B2:B1:B0)

//////// STEP 1; break-down to 32-bit multiplications, each of them generates 64-bit result:
  (A3:A2*B3:B2)<<64 + (A3:A2*B1:B0)<<32 + (A1:A0*B3:B2)<<32 + (A1:A0*B1:B0)

(A1:A0*B1:B0) = (A1*B1)<<32 + (A1*B0)<<16 + (A0*B1)<<16 + (A0:B0)
 -- never overflows, forms the base of the final result, name it as "R64"

(A3:A2*B3:B2) is not required for the 64-bit result if overflow is not checked, since it is completely beyond the resulting width.
 -- always overflows if "<>0", so can be checked as "((A2|A3)<>0)&&(B2|B3)<>0)"

(A3:A2*B1:B0) and (A1:A0*B3:B2) are partially required for the final result
 -- to be calculated on steps 2 and 3 as a correction for the "R64"

//////// STEP 2; calculate "R64+=(A3:A2*B1:B0)<<32" (16-bit multiplications, each of them generates 32-bit result):
  (A3*B1)<<32 + (A3*B0)<<16 + (A2*B1)<<16 + (A2*B0)

((A3*B1)<<32)<<32 is not required for the 64-bit result if overflow is not checked, since it is completely beyond the resulting width.
 -- always overflows if "<>0", so can be checked as "(A3<>0)&&(B1<>0)"

((A3*B0)<<16)<<32: only low word of "A3*B0" contributes to the final result if overflow is not checked.
 -- overflows if the hi_word "<>0"
 -- overflows if R64+(lo_word<<48) produces C-flag

((A2*B1)<<16)<<32: only low word of "A2*B1" contributes to the final result if overflow is not checked.
 -- overflows if the hi_word "<>0"
 -- overflows if R64+(lo_word<<48) produces C-flag

(A2*B0)<<32: the whole dword is significand, name it as "X"
 -- overflows if R64+(X<<32) produces C-flag

//////// STEP 3; calculate "R64+=(A1:A0*B3:B2)<<32" (16-bit multiplications, each of them generates 32-bit result):
  (A1*B3)<<32 + (A1*B2)<<16 + (A0*B3)<<16 + (A0*B2)

((A1*B3)<<32)<<32 is not required for the 64-bit result if overflow is not checked, since it is completely beyond the resulting width.
 -- always overflows if "<>0", so can be checked as "(A1<>0)&&(B3<>0)"

((A1*B2)<<16)<<32: only low word of "A1*B2" contributes to the final result if overflow is not checked.
 -- overflows if the hi_word "<>0"
 -- overflows if R64+(lo_word<<48) produces C-flag

((A0*B3)<<16)<<32: only low word "A0*B3" contributes to the final result if overflow is not checked.
 -- overflows if the hi_word "<>0"
 -- overflows if R64+(lo_word<<48) produces C-flag

(A0*B2)<<32: the whole dword is significand, name it as "Y"
 -- overflows if R64+(Y<<32) produces C-flag
}
  asm
    mov     di,word[f1]
    mov     bx,word[f1+2]
    mov     si,word[f2]
    mov     ax,word[f2+2]
    push    bp
    mov     cx,ax
    mul     bx
    xchg    ax,bx
    mov     bp,dx
    mul     si
    xchg    ax,cx
    add     bx,dx
    adc     bp,0
    mul     di
    add     cx,ax
    adc     bx,dx
    adc     bp,0
    mov     ax,di
    mul     si
    add     cx,dx
    adc     bx,0
    adc     bp,0
    mov     dx,bp
    pop     bp
    mov     word[result],ax
    mov     word[result+2],cx
    mov     word[result+4],bx
    mov     word[result+6],dx
    mov     si,word[f1+4]
    mov     ax,word[f1+6]
    mov     bx,word[checkoverflow]
    or      bx,word[checkoverflow+2]
    jnz     @@checked
    mov     di,word[f2]
    mul     di
    mov     cx,ax
    mov     ax,word[f2+2]
    mul     si
    add     cx,ax
    mov     ax,di
    mul     si
    mov     bx,ax
    add     cx,dx
    mov     si,word[f2+4]
    mov     ax,word[f2+6]
    mov     di,word[f1]
    mul     di
    add     cx,ax
    mov     ax,word[f1+2]
    mul     si
    add     cx,ax
    mov     ax,di
    mul     si
    add     bx,ax
    adc     cx,dx
    add     word[result+4],bx
    adc     word[result+6],cx
    jmp     @@done
@@checked:
    mov     bx,word[f2+6]
    mov     cx,ax
    or      cx,si
    jz      @@nover1
    mov     cx,word[f2+4]
    or      cx,bx
    jnz     @@done
@@nover1:
    test    bx,bx
    jz      @@nover2
    mov     bx,word[f1+2]
    test    bx,bx
    jnz     @@done
@@nover2:
    test    ax,ax
    jz      @@nover3
    or      bx,word[f2+2]
    jnz     @@done
@@nover3:
    mov     di,word[f2]
    mul     di
    test    dx,dx
    jnz     @@done
    mov     cx,ax
    mov     ax,word[f2+2]
    mul     si
    test    dx,dx
    jnz     @@done
    add     cx,ax
    jc      @@done
    mov     ax,di
    mul     si
    mov     bx,ax
    add     cx,dx
    jc      @@done
    mov     si,word[f2+4]
    mov     ax,word[f2+6]
    mov     di,word[f1]
    mul     di
    test    dx,dx
    jnz     @@done
    add     cx,ax
    jc      @@done
    mov     ax,word[f1+2]
    mul     si
    test    dx,dx
    jnz     @@done
    add     cx,ax
    jc      @@done
    mov     ax,di
    mul     si
    add     bx,ax
    adc     cx,dx
    jc      @@done
    add     word[result+4],bx
    adc     word[result+6],cx
    jc      @@done
    // checked and succeed
    xor     ax,ax
    mov     word[checkoverflow],ax
    mov     word[checkoverflow+2],ax
@@done:
  end [ 'ax','bx','cx','dx','si','di' ];
  if checkoverflow then
    HandleErrorAddrFrameInd(215,get_pc_addr,get_frame);
end;


{$define FPC_SYSTEM_HAS_MUL_DWORD_TO_QWORD}
function fpc_mul_dword_to_qword(f1,f2 : dword) : qword;[public,alias: 'FPC_MUL_DWORD_TO_QWORD']; compilerproc; assembler; nostackframe;
asm
  push    bp
  mov     bp, sp
  mov     di,word[bp +  8 + extra_param_offset]  // word[f1]
  mov     bx,word[bp + 10 + extra_param_offset]  // word[f1+2]
  mov     si,word[bp +  4 + extra_param_offset]  // word[f2]
  mov     ax,word[bp +  6 + extra_param_offset]  // word[f2+2]
  mov     cx,ax
  mul     bx
  xchg    ax,bx
  mov     bp,dx
  mul     si
  xchg    ax,cx
  add     bx,dx
  adc     bp,0
  mul     di
  add     cx,ax
  adc     bx,dx
  adc     bp,0
  mov     ax,di
  mul     si
  add     cx,dx
  adc     bx,0
  adc     bp,0
  mov     dx,ax
  mov     ax,bp
  pop     bp
end;


{$define FPC_SYSTEM_HAS_DIV_QWORD}
function fpc_div_qword( n, z: qword ): qword; [public, alias:'FPC_DIV_QWORD']; compilerproc;
// Generic "schoolbook" division algorithm
// see [D.Knuth, TAOCP, vol.2, sect.4.3.1] for explanation
var
  dig: byte;
  u: array [0..6] of word;
begin
  asm
      mov    dig,3 // quotient contains 3 digits for "long" division path
      // Check parameters
      mov    dx,word [n]
      mov    cx,word [n+2]
      mov    bx,word [n+4]
      mov    ax,word [n+6]
      mov    di,ax
      or     di,bx
      or     di,cx
      jnz    @@s1
      or     di,dx
      jz     @@q // div by 0
      // Short division
      mov    dig,al
      mov    dx,word [z+6]
      cmp    dx,di
      jc     @@s0
      xchg   ax,dx
      div    di
@@s0: mov    word [result+6],ax
      mov    ax,word [z+4]
      div    di
      mov    word [result+4],ax
      mov    ax,word [z+2]
      div    di
      mov    word [result+2],ax
      mov    ax,word [z]
      div    di
      mov    word [result],ax
      jmp    @@q
@@s1: // Long division
      xor    si,si
      cmp    word [z],dx
      mov    di,word [z+2]
      sbb    di,cx
      mov    di,word [z+4]
      sbb    di,bx
      mov    di,word [z+6]
      sbb    di,ax
      jnc    @@n0
      // underflow: return 0
      mov    dig,0
      mov    word [result],si
      mov    word [result+2],si
      mov    word [result+4],si
      mov    word [result+6],si
      jmp    @@q
@@n0: // D1. Normalize divisor:
      //   n := n shl lzv, so that 2^63<=n<2^64
      // Note: n>=0x10000 leads to lzv<=47 and q3=0
      mov    word [result+6],si // q3:=0
      mov    di,si
      test   ax,ax
      jnz    @@n2
@@n1: add    si,16
      or     ax,bx
      mov    bx,cx
      mov    cx,dx
      mov    dx,di
      jz     @@n1
@@n2: test   ah,ah
      jnz    @@n4
      add    si,8
      or     ah,al
      mov    al,bh
      mov    bh,bl
      mov    bl,ch
      mov    ch,cl
      mov    cl,dh
      mov    dh,dl
      mov    dl,0
      js     @@n5
@@n3: inc    si
      shl    dx,1
      rcl    cx,1
      rcl    bx,1
      adc    ax,ax
@@n4: jns    @@n3
@@n5: mov    word [n],dx 
      mov    word [n+2],cx
      mov    word [n+4],bx
      mov    word [n+6],ax
      // Adjust divident accordingly:
      //   u := uint128(z) shl lzv; lzv=si=0..47; di=0
      mov    dx,word [z]
      mov    cx,word [z+2]
      mov    bx,word [z+4]
      mov    ax,word [z+6]
      push   bp
      mov    bp,si // save lzv
      test   si,8
      jz     @@m0
      // << by odd-8
      xchg   al,ah
      mov    di,ax
      and    di,0FFh
      mov    al,bh
      mov    bh,bl
      mov    bl,ch
      mov    ch,cl
      mov    cl,dh
      mov    dh,dl
      xor    dl,dl
@@m0: and    si,7
      jz     @@m2
      // << 1..7
@@m1: shl    dx,1
      rcl    cx,1
      rcl    bx,1
      rcl    ax,1
      rcl    di,1
      dec    si
      jnz    @@m1
@@m2: // si=0, bp=lzv
      // di:ax:bx:cx:dx shifted by 0..15; 0|16|32 shifts remain
      sub    bp,16
      jc     @@m5
      sub    bp,16
      jc     @@m4
      // << 32
      pop    bp
      mov    word [u],si
      mov    word [u+2],si
      mov    word [u+4],dx
      mov    word [u+6],cx
      mov    word [u+8],bx
      mov    word [u+10],ax
      mov    word [u+12],di
      jmp    @@m6
@@m4: // << 16
      pop    bp
      mov    word [u],si
      mov    word [u+2],dx
      mov    word [u+4],cx
      mov    word [u+6],bx
      mov    word [u+8],ax
      mov    word [u+10],di
      mov    word [u+12],si
      jmp    @@m6
@@m5: // << 0
      pop    bp
      mov    word [u],dx
      mov    word [u+2],cx
      mov    word [u+4],bx
      mov    word [u+6],ax
      mov    word [u+8],di
      mov    word [u+10],si
      mov    word [u+12],si
@@m6: // D2. Start from j:=2 (since u7=0 and u6<n3), si:=@u[j], bx:=@q[j]
      lea    si,word [u+4]
      lea    bx,word [result+4]
@@d0: push   bx
      // D3. Estimate the next quotient digit:
      //   q_hat := [u(j+4):u(j+3)]/[n3]
      //   use max.possible q_hat if division overflows
      mov    ax,-1
      mov    dx,ss:[si+8]
      mov    di,word [n+6]
      cmp    dx,di
      jnc    @@d1
      mov    ax,ss:[si+6]
      div    di
@@d1: // D4. Multiply & subtract calculating partial reminder:
      //   r := [u(j+4):u(j+3):u(j+2):u(j+1):u(j)]-q_hat*[n3:n2:n1:n0]
      push   ax // q_hat
      push   si // @u[j]
      mov    si,ax
      mul    word [n]
      mov    bx,ax
      mov    cx,dx
      mov    ax,word [n+2]
      mul    si
      add    cx,ax
      adc    dx,0
      mov    di,dx
      mov    ax,word [n+4]
      mul    si
      add    di,ax
      adc    dx,0
      xchg   dx,si
      mov    ax,word [n+6]
      mul    dx
      add    ax,si
      pop    si // @u[j]
      adc    dx,0
      sub    ss:[si],bx
      sbb    ss:[si+2],cx
      sbb    ss:[si+4],di
      sbb    ss:[si+6],ax
      sbb    ss:[si+8],dx
      pop    di // q_hat
      // D5. Test reminder
      jnc    @@d3 // 0<=r<n
      // D6. Add back once or twice correcting the quotient and remainder:
      //   while (r<0) do { q_hat--; r+=n; }
      mov    dx,word [n]
      mov    cx,word [n+2]
      mov    bx,word [n+4]
      mov    ax,word [n+6] 
@@d2: dec    di
      add    ss:[si],dx
      adc    ss:[si+2],cx
      adc    ss:[si+4],bx
      adc    ss:[si+6],ax
      adc    word ss:[si+8],0
      jnc    @@d2
@@d3: // D7. Store q[j], loop on j--
      pop    bx // @q[j]
      dec    si
      dec    si
      mov    ss:[bx],di
      dec    bx
      dec    bx
      dec    dig
      jnz    @@d0
@@q:
  end;
  if dig<>0 then
    HandleErrorAddrFrameInd(200,get_pc_addr,get_frame);
end;


{$define FPC_SYSTEM_HAS_MOD_QWORD}
function fpc_mod_qword( n, z: qword ): qword; [public, alias:'FPC_MOD_QWORD']; compilerproc;
// Generic "schoolbook" division algorithm
// see [D.Knuth, TAOCP, vol.2, sect.4.3.1] for explanation
var
  dig: byte;
  lzv: word;
  u: array [0..6] of word;
begin
  asm
      mov    dig,3 // quotient contains 3 digist for "long" division path
      // Check parameters
      mov    dx,word [n]
      mov    cx,word [n+2]
      mov    bx,word [n+4]
      mov    ax,word [n+6]
      mov    di,ax
      or     di,bx
      or     di,cx
      jnz    @@s1
      or     di,dx
      jz     @@q // div by 0
      // Short division
      mov    dig,al
      mov    dx,word [z+6]
      cmp    dx,di
      jc     @@s0
      xchg   ax,dx
      div    di
@@s0: mov    ax,word [z+4]
      div    di
      mov    ax,word [z+2]
      div    di
      mov    ax,word [z]
      div    di
      mov    word [result],dx
      mov    word [result+2],cx
      mov    word [result+4],cx
      mov    word [result+6],cx
      jmp    @@q
@@s1: // Long division
      xor    si,si
      cmp    word [z],dx
      mov    di,word [z+2]
      sbb    di,cx
      mov    di,word [z+4]
      sbb    di,bx
      mov    di,word [z+6]
      sbb    di,ax
      jnc    @@n0
      // underflow: return z
      mov    dig,0
      mov    dx,word [z]
      mov    cx,word [z+2]
      mov    bx,word [z+4]
      mov    ax,word [z+6]
      jmp    @@r6
@@n0: // D1. Normalize divisor:
      //   n := n shl lzv, so that 2^63<=n<2^64
      // Note: n>=0x10000 leads to lzv<=47
      mov    di,si
      test   ax,ax
      jnz    @@n2
@@n1: add    si,16
      or     ax,bx
      mov    bx,cx
      mov    cx,dx
      mov    dx,di
      jz     @@n1
@@n2: test   ah,ah
      jnz    @@n4
      add    si,8
      or     ah,al
      mov    al,bh
      mov    bh,bl
      mov    bl,ch
      mov    ch,cl
      mov    cl,dh
      mov    dh,dl
      mov    dl,0
      js     @@n5
@@n3: inc    si
      shl    dx,1
      rcl    cx,1
      rcl    bx,1
      adc    ax,ax
@@n4: jns    @@n3
@@n5: mov    word [n],dx 
      mov    word [n+2],cx
      mov    word [n+4],bx
      mov    word [n+6],ax
      mov    lzv,si
      // Adjust divident accordingly:
      //   u := uint128(z) shl lzv; lzv=si=0..47; di=0
      mov    dx,word [z]
      mov    cx,word [z+2]
      mov    bx,word [z+4]
      mov    ax,word [z+6]
      push   bp
      mov    bp,si // save lzv
      test   si,8
      jz     @@m0
      // << by odd-8
      xchg   al,ah
      mov    di,ax
      and    di,0FFh
      mov    al,bh
      mov    bh,bl
      mov    bl,ch
      mov    ch,cl
      mov    cl,dh
      mov    dh,dl
      xor    dl,dl
@@m0: and    si,7
      jz     @@m2
      // << 1..7
@@m1: shl    dx,1
      rcl    cx,1
      rcl    bx,1
      rcl    ax,1
      rcl    di,1
      dec    si
      jnz    @@m1
@@m2: // si=0, bp=lzv
      // di:ax:bx:cx:dx shifted by 0..15; 0|16|32 shifts remain
      sub    bp,16
      jc     @@m5
      sub    bp,16
      jc     @@m4
      // << 32
      pop    bp
      mov    word [u],si
      mov    word [u+2],si
      mov    word [u+4],dx
      mov    word [u+6],cx
      mov    word [u+8],bx
      mov    word [u+10],ax
      mov    word [u+12],di
      jmp    @@m6
@@m4: // << 16
      pop    bp
      mov    word [u],si
      mov    word [u+2],dx
      mov    word [u+4],cx
      mov    word [u+6],bx
      mov    word [u+8],ax
      mov    word [u+10],di
      mov    word [u+12],si
      jmp    @@m6
@@m5: // << 0
      pop    bp
      mov    word [u],dx
      mov    word [u+2],cx
      mov    word [u+4],bx
      mov    word [u+6],ax
      mov    word [u+8],di
      mov    word [u+10],si
      mov    word [u+12],si
@@m6: // D2. Start from j:=2 (since u7=0 and u6<n3), si:=@u[j]
      lea    si,word [u+4]
@@d0: // D3. Estimate the next quotient digit:
      //   q_hat := [u(j+4):u(j+3)]/[n3]
      //   use max.possible q_hat if division overflows
      mov    ax,-1
      mov    dx,ss:[si+8]
      mov    di,word [n+6]
      cmp    dx,di
      jnc    @@d1
      mov    ax,ss:[si+6]
      div    di
@@d1: // D4. Multiply & subtract calculating partial reminder:
      //   r := [u(j+4):u(j+3):u(j+2):u(j+1):u(j)]-q_hat*[n3:n2:n1:n0]
      push   si    // @u[j]
      mov    si,ax // q_hat
      mul    word [n]
      mov    bx,ax
      mov    cx,dx
      mov    ax,word [n+2]
      mul    si
      add    cx,ax
      adc    dx,0
      mov    di,dx
      mov    ax,word [n+4]
      mul    si
      add    di,ax
      adc    dx,0
      xchg   dx,si
      mov    ax,word [n+6]
      mul    dx
      add    ax,si
      pop    si // @u[j]
      adc    dx,0
      sub    ss:[si],bx
      sbb    ss:[si+2],cx
      sbb    ss:[si+4],di
      sbb    ss:[si+6],ax
      mov    di,ss:[si+8]
      sbb    di,dx
      // D5. Test reminder
      jnc    @@d3 // 0<=r<n
      // D6. Add back once or twice correcting the remainder:
      //   while (r<0) do { r+=n; }
      mov    dx,word [n]
      mov    cx,word [n+2]
      mov    bx,word [n+4]
      mov    ax,word [n+6]
@@d2: add    ss:[si],dx
      adc    ss:[si+2],cx
      adc    ss:[si+4],bx
      adc    ss:[si+6],ax
      adc    di,0
      jnc    @@d2
@@d3: // D7. Loop on j--
      dec    si
      dec    si
      dec    dig
      jnz    @@d0
      // D8. "Unnormalize" and return reminder:
      //   result := [u3:u2:u1:u0] shr lzv
      xor    ax,ax
      mov    si,lzv
      sub    si,16
      jc     @@r2
      sub    si,16
      jc     @@r1
      // >> 32..47
      mov    bx,ax
      mov    cx,word [u+6]
      mov    dx,word [u+4]
      jmp    @@r3
@@r1: // >> 16..31
      mov    bx,word [u+6]
      mov    cx,word [u+4]
      mov    dx,word [u+2]
      jmp    @@r3
@@r2: // >> 0..15
      mov    ax,word [u+6]
      mov    bx,word [u+4]
      mov    cx,word [u+2]
      mov    dx,word [u]
@@r3: and    si,15
      sub    si,8
      jc     @@r4
      // >> 8..15
      mov    dl,dh
      mov    dh,cl
      mov    cl,ch
      mov    ch,bl
      mov    bl,bh
      mov    bh,al
      mov    al,ah
      xor    ah,ah
@@r4: and    si,7
      jz     @@r6
      // >> 1..7
@@r5: shr    ax,1
      rcr    bx,1
      rcr    cx,1
      rcr    dx,1
      dec    si
      jnz    @@r5
@@r6: mov    word [result],dx
      mov    word [result+2],cx
      mov    word [result+4],bx
      mov    word [result+6],ax
@@q:
  end;
  if dig<>0 then
    HandleErrorAddrFrameInd(200,get_pc_addr,get_frame);
end;

