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

