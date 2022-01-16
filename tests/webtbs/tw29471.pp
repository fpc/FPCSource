{ %cpu=x86_64 }
{ %norun }
program avxtest;

 {$ASMMODE ATT}

 type
   TSIMDWord = packed array[0..3] of Double;

 procedure SIMDAddWord(var A, B, ARes);assembler;
 asm
         vmovdqu (%rdi), %ymm2 // <- error 1
         vmovdqu (%rsi), %ymm1
         vaddpd %ymm1, %ymm2, %ymm0 // <- error 2
         vmovdqu %ymm0, (%rdx)
 end;

 var
    V1, V2, Res : TSIMDWord;

 begin
    SIMDAddWord(V1, V2, Res);
 end.
 