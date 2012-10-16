{ %CPU=x86_64 }

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

procedure tmovd; assembler; nostackframe;
asm
    movd  0x12345678(%rip), %xmm0
    movd  %xmm0, 0x12345678(%rip)
    movd  %eax, %xmm0
    movd  %xmm0, %eax

{ same for MMX registers }
    movd  0x12345678(%rip), %mm0
    movd  %mm0, 0x12345678(%rip)
    movd  %eax, %mm0
    movd  %mm0, %eax
end;

procedure tmovq; assembler; nostackframe;
asm
    movq  0x12345678(%rip), %xmm0
    movq  %xmm0, 0x12345678(%rip)
    movq  %xmm1, %xmm0
    movq  %rax, %xmm0
    movq  %xmm0, %rax

{ same for MMX registers }
    movq  0x12345678(%rip), %mm0
    movq  %mm0, 0x12345678(%rip)
    movq  %mm1, %mm0
    movq  %rax, %mm0
    movq  %mm0, %rax
end;

// Here are some more tough nuts to crack...
procedure test2; assembler; nostackframe;
asm
//    cmpsd     $0x10,%xmm7,%xmm6         { unrecognized opcode }
//    cmpsd     $0x10,(%rax),%xmm7
//    movsd     (%rax),%xmm8              { unrecognized opcode }
//    movsd     %xmm8,(%rax)
end;

procedure tcalljump; assembler; nostackframe;
asm
    call   *0x12345678(%rip)
    call   *(%rax)
    jmp    *0x12345678(%rip)
    jmp    *(%rax)
    lcall  *0x12345678(%rip)
    lcall  *(%rax)
    ljmp   *0x12345678(%rip)
    ljmp   *(%rax)
end;

const
  test_expected : array[0..745] of byte = (
    $66,$44,$0F,$D0,$05,$78,$56,$34,$12, // addsubpd
    $F2,$44,$0F,$D0,$05,$78,$56,$34,$12, // addsubps
    $66,$44,$0F,$2F,$05,$78,$56,$34,$12, // comisd
    $44,$0F,$2F,$05,$78,$56,$34,$12,     // comiss
    $F3,$44,$0F,$E6,$05,$78,$56,$34,$12, // cvtdq2pd
    $F2,$44,$0F,$E6,$05,$78,$56,$34,$12, // cvtpd2dq
    $44,$0F,$5A,$05,$78,$56,$34,$12,     // cvtps2pd
    $F3,$44,$0F,$5B,$05,$78,$56,$34,$12, // cvttps2dq
    $F3,$44,$0F,$2A,$C0,                 // cvtsi2ss
    $F3,$44,$0F,$2A,$C0,                 // cvtsi2ss
    $F3,$4C,$0F,$2A,$C0,                 // cvtsi2ss
    $F3,$4C,$0F,$2A,$C0,                 // cvtsi2ss
    $F3,$44,$0F,$2A,$00,                 // cvtsi2ss  (%rax),%xmm8
    $F3,$44,$0F,$2A,$00,                 // cvtsi2ssl (%rax),%xmm8
    $F3,$4C,$0F,$2A,$00,                 // cvtsi2ssq (%rax),%xmm8
    $F2,$44,$0F,$2A,$C0,$F2,$44,$0F,
    $2A,$C0,$F2,$4C,$0F,$2A,$C0,$F2,$4C,$0F,$2A,$C0,$F2,$44,$0F,
    $2A,$00,$F2,$44,$0F,$2A,$00,$F2,$4C,$0F,$2A,$00,$F3,$44,$0F,
    $7F,$05,$78,$56,$34,$12,$F3,$44,$0F,$6F,$05,$78,$56,$34,$12,
    $66,$44,$0F,$7F,$05,$78,$56,$34,$12,$66,$44,$0F,$6F,$05,$78,
    $56,$34,$12,$F2,$44,$0F,$7C,$05,$78,$56,$34,$12,$66,$44,$0F,
    $7C,$05,$78,$56,$34,$12,$F3,$44,$0F,$16,$05,$78,$56,$34,$12,
    $F3,$44,$0F,$12,$05,$78,$56,$34,$12,$66,$44,$0F,$17,$05,$78,
    $56,$34,$12,$66,$44,$0F,$16,$05,$78,$56,$34,$12,$44,$0F,$17,
    $05,$78,$56,$34,$12,$44,$0F,$16,$05,$78,$56,$34,$12,$66,$44,
    $0F,$13,$05,$78,$56,$34,$12,$66,$44,$0F,$12,$05,$78,$56,$34,
    $12,$44,$0F,$13,$05,$78,$56,$34,$12,$44,$0F,$12,$05,$78,$56,
    $34,$12,$F3,$0F,$70,$0D,$78,$56,$34,$12,$90,$F2,$0F,$70,$0D,
    $78,$56,$34,$12,$90,$0F,$60,$0D,$78,$56,$34,$12,$0F,$62,$0D,
    $78,$56,$34,$12,$0F,$61,$0D,$78,$56,$34,$12,$66,$44,$0F,$60,
    $05,$78,$56,$34,$12,$66,$44,$0F,$62,$05,$78,$56,$34,$12,$66,
    $44,$0F,$61,$05,$78,$56,$34,$12,$66,$44,$0F,$6C,$05,$78,$56,
    $34,$12,$66,$44,$0F,$2E,$05,$78,$56,$34,$12,$44,$0F,$2E,$05,
    $78,$56,$34,$12,$F2,$44,$0F,$C2,$00,$00,$F3,$44,$0F,$C2,$00,
    $00,$66,$44,$0F,$2A,$C0,$66,$44,$0F,$2A,$00,$44,$0F,$2A,$C0,
    $44,$0F,$2A,$00,$41,$0F,$2D,$00,$F2,$41,$0F,$2D,$00,$F2,$49,
    $0F,$2D,$00,$F2,$41,$0F,$2C,$00,$F2,$49,$0F,$2C,$00,$F2,$41,
    $0F,$5A,$00,$F3,$41,$0F,$5A,$00,$F3,$41,$0F,$2D,$00,$F3,$49,
    $0F,$2D,$00,$F3,$41,$0F,$2C,$00,$F3,$49,$0F,$2C,$00,$F2,$44,
    $0F,$5E,$00,$F3,$44,$0F,$5E,$00,$F2,$44,$0F,$5F,$00,$F3,$44,
    $0F,$5F,$00,$F3,$44,$0F,$5D,$00,$F3,$44,$0F,$5D,$00,$F2,$44,
    $0F,$2B,$00,$F3,$44,$0F,$2B,$00,$F3,$44,$0F,$10,$00,$F3,$44,
    $0F,$11,$00,$F2,$44,$0F,$59,$00,$F3,$44,$0F,$59,$00,$F3,$44,
    $0F,$53,$00,$44,$0F,$53,$00,$66,$44,$0F,$3A,$0B,$00,$00,$66,
    $44,$0F,$3A,$0A,$00,$00,$F3,$44,$0F,$52,$00,$44,$0F,$52,$00,
    $F2,$44,$0F,$51,$00,$F3,$44,$0F,$51,$00,$66,$44,$0F,$51,$00,
    $44,$0F,$51,$00,$F2,$44,$0F,$5C,$00,$F3,$44,$0F,$5C,$00,$66,
    $44,$0F,$38,$20,$00,$66,$44,$0F,$38,$21,$00,$66,$44,$0F,$38,
    $22,$00,$66,$44,$0F,$38,$23,$00,$66,$44,$0F,$38,$24,$00,$66,
    $44,$0F,$38,$25,$00,$66,$44,$0F,$38,$30,$00,$66,$44,$0F,$38,
    $31,$00,$66,$44,$0F,$38,$32,$00,$66,$44,$0F,$38,$33,$00,$66,
    $44,$0F,$38,$34,$00,$66,$44,$0F,$38,$35,$00,$66,$44,$0F,$3A,
    $21,$00,$00,$66,$44,$0F,$15,$00,$44,$0F,$15,$00,$66,$44,$0F,
    $14,$00,$44,$0F,$14,$00,$F3,$0F,$C2,$F7,$10,$F3,$0F,$C2,$38,
    $10,$0F,$D4,$C1,$0F,$D4,$00,$66,$0F,$D4,$C1,$66,$0F,$D4,$00,
    $0F,$FB,$C1,$0F,$FB,$00,$66,$0F,$FB,$C1,$66,$0F,$FB,$00,$0F,
    $F4,$C1,$0F,$F4,$00,$66,$0F,$F4,$C1,$66,$0F,$F4,$00,$41,$0F,
    $58,$00,$F3,$41,$0F,$58,$00,$66,$41,$0F,$E4,$00,$66,$41,$0F,
    $F6,$00,$C3);

 tmovq_expected: array[0..54] of byte = (
    $F3,$0F,$7E,$05,$78,$56,$34,$12,
    $66,$0F,$D6,$05,$78,$56,$34,$12,
    $F3,$0F,$7E,$C1,
    $66,$48,$0F,$6E,$C0,
    $66,$48,$0F,$7E,$C0,
    $0F,$6F,$05,$78,$56,$34,$12,
    $0F,$7F,$05,$78,$56,$34,$12,
    $0F,$6F,$C1,
    $48,$0F,$6E,$C0,
    $48,$0F,$7E,$C0
  );

  tmovd_expected: array[0..43] of byte = (
    $66,$0F,$6E,$05,$78,$56,$34,$12,
    $66,$0F,$7E,$05,$78,$56,$34,$12,
    $66,$0F,$6E,$C0,
    $66,$0F,$7E,$C0,
    $0F,$6E,$05,$78,$56,$34,$12,
    $0F,$7E,$05,$78,$56,$34,$12,
    $0F,$6E,$C0,
    $0F,$7E,$C0
  );

  tcalljump_expected: array[0..31] of byte = (
    $ff,$15,$78,$56,$34,$12,
    $ff,$10,
    $ff,$25,$78,$56,$34,$12,
    $ff,$20,
    $ff,$1d,$78,$56,$34,$12,
    $ff,$18,
    $ff,$2d,$78,$56,$34,$12,
    $ff,$28
  );

procedure check(const id: string; const expected: array of byte; p: pointer);
var
  i : longint;
begin
  for i:=0 to high(expected) do
    if expected[i]<>pbyte(p)[i] then
      begin
        writeln(id, ' mismatch at offset $',hexstr(i,4), ', expected=$',hexstr(expected[i],2),' actual=$',hexstr(pbyte(p)[i],2));
        halt(1);
      end;
end;

begin
  check('generic', test_expected, @test);
  check('movq', tmovq_expected, @tmovq);
  check('movd', tmovd_expected, @tmovd);
  check('calljmp', tcalljump_expected, @tcalljump);
  writeln('ok');
end.
