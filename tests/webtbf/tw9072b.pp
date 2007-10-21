{ %fail }

var
  a: array[1..2] of longint;
  l: longint;
begin
  l:=0;
  for a[1]:=1 to 10 do
    inc(l);
  if (l<>10) then
    halt(1);
end.
