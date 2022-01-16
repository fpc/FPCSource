program and_problem;
{$mode objfpc}{$H+}

var
  a : extended;
  d : longint;
begin
  a := -1;
  d := (round(a*512) div 180) and 1023;
  writeln(d);
  if d<>1022 then
    halt(1);

  d := (round(a*512) div 180);
  while (d<0) do d := d+1024;
  writeln(d);
  if d<>1022 then
    halt(1);
end.
