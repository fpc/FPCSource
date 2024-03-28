{ %OPT=-O1 }
program tw40643;
var
  a: integer = 1;
  b: integer = 1;
begin
  if (a < 1) or ((a = 1) and (a = b)) then Exit;

  WriteLn('Fail');
  Halt(1);
end.
