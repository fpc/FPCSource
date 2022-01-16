var
  b1, b2, b3: longbool;
begin
  b1:=longbool(1);
  b2:=longbool(2);
  b3:=b1 and b2;
  if not b3 then
    halt(1);
  b3:=b1 xor b2;
  if b3 then
    halt(2);
{$b+}
  b3:=b1 and b2;
  if not b3 then
    halt(3);
  b3:=b1 xor b2;
  if b3 then
    halt(4);
end.
