{$mode objfpc}
type
  tc = class sealed
  end;

var
  c : tc;

function f : tc;
  begin
    result:=tc.create;
  end;


begin
  c:=tc.create;
  if not(c is tc) then
    halt(1);
  if not(f is tc) then
    halt(1);
  writeln('ok');;
end.

