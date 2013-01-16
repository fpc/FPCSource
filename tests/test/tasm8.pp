{ %CPU=x86_64 }

// (Almost) every of these instructions use a high register and thus generate REX.
{$asmmode intel}
procedure avxtest; assembler; nostackframe;
asm
    VADDPD         XMM0, XMM1, [RAX] + $100
    VADDPD         YMM2, YMM3, [RAX] + $100

    VBLENDPD       XMM2, XMM5, XMM7, $02
    VBLENDPD       YMM2, YMM5, YMM7, $0F

    VBLENDVPS      XMM2, XMM5, XMM7, XMM4
    VBLENDVPS      YMM0, YMM1, YMM2, YMM5

    VBROADCASTSD   YMM0, [RAX]

    VPEXTRB         EAX, XMM0, $00

    VPINSRD        XMM7, XMM1, EAX, $03

    VZEROALL

end;


const
  avxtest_expected : array[0..59] of byte = (
  $C5,$F1,$58,$80,$00,$01,$00,$00,
  $C5,$E5,$58,$90,$00,$01,$00,$00,
  $C4,$E3,$51,$0D,$D7,$02,
  $C4,$E3,$55,$0D,$D7,$0F,
  $C4,$E3,$51,$4A,$D7,$40,
  $C4,$E3,$75,$4A,$C2,$50,
  $C4,$E2,$7D,$19,$00,
  $C4,$E3,$79,$14,$C0,$00,
  $C4,$E3,$71,$22,$F8,$03,
  $C5,$FC,$77);



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
  check('generic', avxtest_expected, @avxtest);
  writeln('ok');
end.
