{$r-}
var
  l : longint;
begin
  l:=$ff;
  l:=RorByte(l);
  if l<>$ff then
    halt(1);
  l:=RolByte(l);
  if l<>$ff then
    halt(1);
  writeln('ok');
end.
