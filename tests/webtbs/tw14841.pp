{$r+,o+}
var
b: byte;
s: shortint;
begin
  b:=57;
  s:=-1;
  inc(b, s);
  if b<>56 then
    halt(1);
end.

