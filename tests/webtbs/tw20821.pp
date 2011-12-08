program bug;

var
  b : byte;
begin
  b:=170;
  if b<>Byte(#%10101010) then
    halt(1);
  if b<>Byte(%10101010) then
    halt(1);
  if b<>Byte(%10101010) then
    halt(1);
  WriteLn('Ok');
end.
