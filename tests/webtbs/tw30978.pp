{ %cpu=x86_64 }

procedure p1;assembler;nostackframe;
  asm
    cltq
  end;

const
  test_expected : array[0..1] of byte = (
    $48,$98);

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
  writeln('ok');
end.

