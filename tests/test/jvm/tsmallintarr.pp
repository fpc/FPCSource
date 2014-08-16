program tsmallintarr;

procedure test;
var
  a: smallint;
  l: longint;
  b: array[smallint] of byte;
begin
  for a:=low(b) to high(b) do
    b[a]:=a and 255;
  for l:=low(b) to high(b) do
    if b[l]<>(l and 255) then
      halt(1);
end;

begin
  test
end.
