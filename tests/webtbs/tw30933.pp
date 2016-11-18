{ %cpu=x86_64,i386 }

procedure p1;assembler;nostackframe;
  asm
{$ifdef cpux86_64}
    prefetchwt1 (%rax)
{$else cpux86_64}
    prefetchwt1 (%eax)
{$endif cpux86_64}
  end;

const
  test_expected : array[0..2] of byte = (
    $0f,$0d,$10);

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

