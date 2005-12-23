{$mode macpas}
function f: longint;
begin
  return 3;
end;

begin
  if f() <> 3 then
    halt(1);
end.

