{ %recompile }

uses ub0506;

var
  c : c2;
  i : integer;
begin
  c:=c2.create;
  c.value:=1;
  i:=c.value;
  if i<>2 then
    halt(1);
end.
