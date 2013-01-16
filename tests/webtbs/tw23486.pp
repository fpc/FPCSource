{ %OPT=-O3 -Oocse }
program project1;

var
  S: Single;
  I: Integer;
begin
  S := 400;
  I := 600;
  writeln(Round((I / 2) - (S / 2)));
end.
