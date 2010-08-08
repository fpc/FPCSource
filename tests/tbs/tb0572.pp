{ test copy optimization of dyn. arrays }
uses
  Sysutils;
var
  a,b,c : array of ansistring;
  i : longint;

begin
  SetLength(a,1000);
  SetLength(c,1000);
  for i:=low(a) to high(a) do
    a[i]:=IntToStr(random(10000));
  b:=copy(a);
  c:=copy(a);
  a:=nil;
  for i:=low(b) to high(b) do
    if b[i]<>c[i] then
      halt(1);
  writeln('ok');
end.
