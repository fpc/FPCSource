{ %OPT=-O2 -Oofastmath }
{ the reciprocal transform must not be applied to a Currency division }
program tw41865d;

{$mode objfpc}

var
  c: currency;
begin
  c := 10;
  if c / 2 <> 5 then
    Halt(1);
  WriteLn('ok');
end.
