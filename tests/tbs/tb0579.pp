{ %opt=-O3 }
var
  i : longint;

function f : real;
  begin
    inc(i);
    f:=2;
  end;

begin
  i:=0;
  if f*f<>4 then
    halt(1);
  if i<>2 then
    halt(1);
  writeln('ok');
end.
