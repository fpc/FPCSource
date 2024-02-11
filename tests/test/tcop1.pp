{ test c style assignment operators }

{$COPERATORS ON}
var
  i : Single;
begin
  i:=1234;
  i += 1;
  if i<>1235 then
    halt(1);
  i -= 1;
  if i<>1234 then
    halt(2);
  i *= 2;
  if i<>2468 then
    halt(3);
  i /= 2;
  if i<>1234 then
    halt(4);
end.
