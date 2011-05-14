{ %CPU=x86_64 }
{ %norun }

// (Almost) every of these instructions use a high register and thus generate REX.
{$asmmode att}
procedure test; assembler; nostackframe;
asm
    addsubpd  0x12345678(%rip),%xmm8
    addsubps  0x12345678(%rip),%xmm8
    comisd    0x12345678(%rip),%xmm8
    comiss    0x12345678(%rip),%xmm8
    cvtdq2pd  0x12345678(%rip),%xmm8
    cvtpd2dq  0x12345678(%rip),%xmm8
    cvtps2pd  0x12345678(%rip),%xmm8
    cvttps2dq 0x12345678(%rip),%xmm8
    cvtsi2ss  %eax, %xmm8
    cvtsi2ssl %eax, %xmm8
    cvtsi2ss  %rax, %xmm8
    cvtsi2ssq %rax, %xmm8

    cvtsi2ss  (%rax), %xmm8
    cvtsi2ssl (%rax), %xmm8
    cvtsi2ssq (%rax), %xmm8

    cvtsi2sd  %eax, %xmm8
    cvtsi2sdl %eax, %xmm8
    cvtsi2sd  %rax, %xmm8
    cvtsi2sdq %rax, %xmm8
    cvtsi2sd  (%rax), %xmm8
    cvtsi2sdl (%rax), %xmm8
    cvtsi2sdq (%rax), %xmm8

    movdqu    %xmm8,0x12345678(%rip)
    movdqu    0x12345678(%rip),%xmm8
    movdqa    %xmm8,0x12345678(%rip)
    movdqa    0x12345678(%rip),%xmm8

    haddps    0x12345678(%rip),%xmm8
    haddpd    0x12345678(%rip),%xmm8
    movshdup  0x12345678(%rip),%xmm8
    movsldup  0x12345678(%rip),%xmm8

    movhpd    %xmm8,0x12345678(%rip)
    movhpd    0x12345678(%rip),%xmm8
    movhps    %xmm8,0x12345678(%rip)
    movhps    0x12345678(%rip),%xmm8
    movlpd    %xmm8,0x12345678(%rip)
    movlpd    0x12345678(%rip),%xmm8
    movlps    %xmm8,0x12345678(%rip)
    movlps    0x12345678(%rip),%xmm8

    pshufhw   $0x90,0x12345678(%rip),%xmm1      { Fixed: unable to determine size }
    pshuflw   $0x90,0x12345678(%rip),%xmm1
    punpcklbw 0x12345678(%rip),%mm1
    punpckldq 0x12345678(%rip),%mm1
    punpcklwd 0x12345678(%rip),%mm1
    punpcklbw 0x12345678(%rip),%xmm8
    punpckldq 0x12345678(%rip),%xmm8
    punpcklwd 0x12345678(%rip),%xmm8
    punpcklqdq 0x12345678(%rip),%xmm8

    ucomisd   0x12345678(%rip),%xmm8
    ucomiss   0x12345678(%rip),%xmm8

    cmpeqsd (%rax),%xmm8
    cmpeqss (%rax),%xmm8

    cvtpi2pd %mm0,%xmm8
    cvtpi2pd (%rax),%xmm8

    cvtpi2ps %mm0,%xmm8
    cvtpi2ps (%rax),%xmm8

    cvtps2pi (%r8),%mm0

    cvtsd2si (%r8),%eax
    cvtsd2siq (%r8),%rax

    cvttsd2si (%r8),%eax
    cvttsd2siq (%r8),%rax

    cvtsd2ss (%r8),%xmm0
    cvtss2sd (%r8),%xmm0

    cvtss2si (%r8),%eax
    cvtss2siq (%r8),%rax

    cvttss2si (%r8),%eax
    cvttss2siq (%r8),%rax

    divsd (%rax),%xmm8
    divss (%rax),%xmm8
    maxsd (%rax),%xmm8
    maxss (%rax),%xmm8
    minss (%rax),%xmm8
    minss (%rax),%xmm8
    movntsd %xmm8,(%rax)
    movntss %xmm8,(%rax)
    movss (%rax),%xmm8
    movss %xmm8,(%rax)
    mulsd (%rax),%xmm8
    mulss (%rax),%xmm8
    rcpss (%rax),%xmm8
    rcpps (%rax),%xmm8
    roundsd $0,(%rax),%xmm8             { (Fixed) unable to determine size }
    roundss $0,(%rax),%xmm8
    rsqrtss (%rax),%xmm8
    rsqrtps (%rax),%xmm8
    sqrtsd (%rax),%xmm8
    sqrtss (%rax),%xmm8
    sqrtpd (%rax),%xmm8
    sqrtps (%rax),%xmm8
    subsd (%rax),%xmm8
    subss (%rax),%xmm8

    pmovsxbw (%rax),%xmm8
    pmovsxbd (%rax),%xmm8
    pmovsxbq (%rax),%xmm8
    pmovsxwd (%rax),%xmm8
    pmovsxwq (%rax),%xmm8
    pmovsxdq (%rax),%xmm8
    pmovzxbw (%rax),%xmm8
    pmovzxbd (%rax),%xmm8
    pmovzxbq (%rax),%xmm8
    pmovzxwd (%rax),%xmm8
    pmovzxwq (%rax),%xmm8
    pmovzxdq (%rax),%xmm8
    insertps $0x0,(%rax),%xmm8          { (Fixed) unable to determine size }

    unpckhpd (%rax),%xmm8               { (Fixed) invalid combination of opcode and operands }
    unpckhps (%rax),%xmm8
    unpcklpd (%rax),%xmm8
    unpcklps (%rax),%xmm8

    cmpss   $0x10,%xmm7,%xmm6
    cmpss   $0x10,(%rax),%xmm7          { (Fixed) unable to determine size }

    paddq   %mm1,%mm0
    paddq   (%rax),%mm0
    paddq   %xmm1,%xmm0
    paddq   (%rax),%xmm0

    psubq   %mm1,%mm0
    psubq   (%rax),%mm0
    psubq   %xmm1,%xmm0
    psubq   (%rax),%xmm0

    pmuludq  %mm1,%mm0
    pmuludq  (%rax),%mm0
    pmuludq  %xmm1,%xmm0
    pmuludq  (%rax),%xmm0

    addps   (%r8),%xmm0
    addss   (%r8),%xmm0

    pmulhuw (%r8),%xmm0
    psadbw (%r8), %xmm0
end;

// Here are some more tough nuts to crack...
procedure test2; assembler; nostackframe;
asm
//    movq      %xmm1,0x12345678(%rip)    { converted to mov, invalid combination of opcode and operands }
//    movq      0x12345678(%rip),%xmm1
//    cmpsd     $0x10,%xmm7,%xmm6         { unrecognized opcode }
//    cmpsd     $0x10,(%rax),%xmm7
//    movsd     (%rax),%xmm8              { unrecognized opcode }
//    movsd     %xmm8,(%rax)
end;

begin
end.
