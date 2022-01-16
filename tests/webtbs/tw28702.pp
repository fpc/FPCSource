var
  l: longint;
begin
  l:=5;
  { dummy random to prevent constant propagation }
  if ((l+random(1))div -1)<>-5 then
    halt(1);
end.
