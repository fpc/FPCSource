{ %VERSION=1.1 }

program tinivar;

procedure p1;
var
  a : integer = 1;
begin
  writeln(a);
  if a<>1 then
    halt(1);
  inc(a);
end;

begin
  p1;
  p1;
end.
