{$mode objfpc}
type
  ta1 = array[0..10] of longint;
var
  a1 : array[0..10] of longint;
  a2 : array of longint;
  i : longint;

begin
  for i:=low(a1) to high(a2) do
    a1[i]:=i*i;
  a2:=a1;
  if length(a2)<>length(a1) then
    halt(1);
  for i:=low(a1) to high(a2) do
    if a2[i]<>a1[i] then
      halt(1);
  writeln('ok');
end.
