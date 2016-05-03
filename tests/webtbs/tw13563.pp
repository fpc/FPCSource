{ %cpu=i386 }
{ %opt=-Cg- }

program bug;
type
  br=bitpacked record	//Note! "bitpacked"
    l:longword;
    m:longword;
    h:longword;
  end;

var
  l,
  m,
  moffs,
  h: longword;

test:br;

{$asmmode att}
begin
  with test do
  begin
    l:=4;
    m:=8;
    h:=$f
  end;
  asm
    movl br.m,%eax	//eax should be 4,but it is 32. Eight times. error!
    movl %eax, moffs
    movl test.m,%eax  //eax should be 8,but it is, a strange number. error!
    movl %eax, m
    movl test.l,%eax
    movl %eax, l
    movl test.h,%eax
    movl %eax, h
  end;

  if (moffs<>4) then
    halt(1);
  if (m<>8) then
    halt(2);
  if (l<>4) then
    halt(3);
  if (h<>$f) then
    halt(4);
end.

