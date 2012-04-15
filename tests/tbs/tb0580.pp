{ %cpu=i386,x86_64 }
procedure p;assembler;nostackframe;
asm
  iret
  iretw
{$ifdef cpux86_64}
  iretq
{$endif cpux86_64}
end;


const
  test_expected : array[0..2{$ifdef cpux86_64}+2{$endif cpux86_64}] of byte = (
    $CF,$66,$CF{$ifdef cpux86_64},$48,$cf{$endif cpux86_64});

var
  i : longint;


begin
  for i:=0 to high(test_expected) do
    if test_expected[i]<>pbyte(@p)[i] then
      begin
        writeln('mismatch at offset $',hexstr(i,4), ', expected=$',
          hexstr(test_expected[i],2),' actual=$',hexstr(pbyte(@p)[i],2));
        halt(1);
      end;
  writeln('ok');
end.
