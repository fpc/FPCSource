{ %cpu=i386,x86_64 }

{$asmmode att}
{$ifdef cpui386}
{$warn 7104 off} //"Using -offset(%ebp) is not recommended"
procedure test_i386; assembler; nostackframe;
asm
  vmovd  %xmm0, -16(%ebp)
  vmovd  -16(%ebp), %xmm0
  
  vmovq  %xmm6, -16(%ebp)
  vmovq  -16(%ebp), %xmm6
  vmovq  %xmm0, %xmm2
end;

const
  expected_i386: array[0..23] of byte = (
    $C5,$F9,$7E,$45,$F0,
    $C5,$F9,$6E,$45,$F0,
    $C5,$F9,$D6,$75,$F0,
    $C5,$FA,$7E,$75,$F0,
    $C5,$FA,$7E,$D0
  );
{$endif}

{$ifdef cpux86_64}  
procedure test_x86_64; assembler; nostackframe;
asm
  vmovq  0x12345678(%rip), %xmm0
  vmovq  %xmm0, 0x12345678(%rip)
  vmovq  %xmm1, %xmm0
  vmovq  %rax, %xmm0
  vmovq  %xmm0, %rax
end;

const
  expected_x86_64: array[0..29] of byte = (
    $C5,$FA,$7E,$05,$78,$56,$34,$12,
    $C5,$F9,$D6,$05,$78,$56,$34,$12,
    $C5,$FA,$7E,$C1,
    $C4,$E1,$F9,$6E,$C0,
    $C4,$E1,$F9,$7E,$C0
  );
{$endif}


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
{$ifdef cpux86_64}
  check('x86_64',expected_x86_64,@test_x86_64);
{$endif}
{$ifdef cpui386}
  check('i386',expected_i386,@test_i386);
{$endif}  
  writeln('ok');
end.
