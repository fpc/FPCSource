{$mode macpas}
program unary;
var
  i : longint;
begin
  if -2 shr 1<>-1 then
    halt(1);
  if -2-2<>-4 then
    halt(1);
  i:=2;
  if -i shr 1<>-1 then
    halt(1);
  if -i-i<>-4 then
    halt(1);
  writeln('ok');
end.
