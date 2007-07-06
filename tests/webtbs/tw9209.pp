{%OPT=-O2}

procedure Proc1(i: integer);
begin
  if i = 1 then
    Inc(i);
end;

procedure Proc2;
begin
  writeln('Test failed!');
  Halt(1);
end;

var
  i, j: integer;

begin
  if (i = 0) and (j = 0) then
    Proc1(i)
  else
    Proc2;
  writeln('Test OK.');
end.
