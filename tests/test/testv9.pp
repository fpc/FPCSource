{$mode objfpc}
uses
  Variants;

var
  a : array of longint;
  a2 : array of array of longint;
  v : variant;
  i : longint;
begin
  setlength(a,1000);
  for i:=0 to high(a) do
    a[i]:=i;
  v:=a;
  for i:=0 to high(a) do
    if v[i]<>i then
      begin
        writeln('v[',i,']=',v[i]);
        halt(1);
      end;
  writeln('simple test ok');

  setlength(a2,10,30);
end.
