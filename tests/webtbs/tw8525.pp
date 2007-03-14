{$mode objfpc}{$H+}

var
  x, y: integer;
begin
  x:=1;
  y := Round(x);
  if y<>1 then
    halt(1);
  writeln('ok');
end.