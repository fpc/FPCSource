{ %cpu=i386 }
procedure p1;assembler;nostackframe;
  asm
    INSERTQ $1,$3,%xmm0,%xmm1
  end;

{$asmmode intel}
procedure p2;assembler;nostackframe;
  asm
    INSERTQ xmm1,xmm0,3,1
  end;


const
  test_expected : array[0..5] of byte = (
    $F2,$0F,$78,$C8,$03,$01);

var
  i : longint;


begin
  for i:=0 to high(test_expected) do
    if test_expected[i]<>pbyte(@p1)[i] then
      begin
        writeln('mismatch at offset $',hexstr(i,4), ', expected=$',
          hexstr(test_expected[i],2),' actual=$',hexstr(pbyte(@p1)[i],2));
        halt(1);
      end;
  for i:=0 to high(test_expected) do
    if test_expected[i]<>pbyte(@p2)[i] then
      begin
        writeln('mismatch at offset $',hexstr(i,4), ', expected=$',
          hexstr(test_expected[i],2),' actual=$',hexstr(pbyte(@p2)[i],2));
        halt(1);
      end;
  writeln('ok');
end.

