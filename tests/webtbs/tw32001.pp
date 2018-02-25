{ %CPU=i386,x86_64 }

{$asmmode intel}
procedure test;assembler;
asm
  CMPEQPS xmm1, xmm2
  CMPLTPS xmm1, xmm2
  CMPLEPS xmm1, xmm2
  CMPUNORDPS xmm1, xmm2
  CMPNEQPS xmm1, xmm2
  CMPNLTPS xmm1, xmm2
  CMPNLEPS xmm1, xmm2
  CMPORDPS xmm1, xmm2
end;

begin
end.
