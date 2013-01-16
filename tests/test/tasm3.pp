{ %CPU=x86_64 }
{ %opt=-vw -Sew }

// Basic test for 3-operand SIMD instructions with rip-relative operand

{$codealign varmin=16}
{$codealign constmin=16}
type
  tvec=array[0..3] of longword;

var
  t: tvec = ($00010203, $04050607, $08090a0b, $0c0d0e0f);

{$asmmode att}
function test: word; assembler; nostackframe;
asm
     pshufd   $0b11100100,t(%rip),%xmm0       // direct copy
     pcmpeqd  t(%rip),%xmm0
     pmovmskb %xmm0,%eax
end;

begin
  if test<>65535 then
    Halt(1)
  else
    writeln('ok');
end.
