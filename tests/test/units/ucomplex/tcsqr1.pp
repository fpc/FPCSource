uses
  ucomplex;

var
  c1,c2,c3 : complex;

begin
  c1:=cinit(1,1);
  c2:=csqr(c1);
  if c2.re<>0 then
    halt(1);
  if c2.im<>2 then
   halt(1);

  c3:=csqrt(c2);

  if not csamevalue(c1,c3) then
    halt(1);

  writeln('ok');
end.
