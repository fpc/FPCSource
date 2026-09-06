{ %norun }
{ %cpu=i386,x86_64 }

{$asmmode intel}

procedure LoadStore(Buffer: Pointer); assembler;
asm
  movss xmm2, [Buffer]
  movss [Buffer], xmm2
  vmovss xmm2, [Buffer]
  vmovss [Buffer], xmm2
  movsd xmm2, [Buffer]
  movsd [Buffer], xmm2
  vmovsd xmm2, [Buffer]
  vmovsd [Buffer], xmm2
end;

begin
end.
