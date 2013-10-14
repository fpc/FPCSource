{ %opt=-O2 }
procedure test;
var
  a, b: cardinal;
begin
  a:=$ffffffff;
  b:=(a shr 24) and $3f;
  if b<>$3f then
    halt(1);
end;

begin
  test;
end.

