{$mode objfpc}

procedure pext(a:array of extended);
begin
end;

procedure p(a:array of const);
begin
end;

begin
  p([0.0]);
  p([pi]);
  pext([0.0]);
end.
