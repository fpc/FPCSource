{ %fail }
program app;
{$mode objfpc}

procedure test(a: array of const);
begin
  a := [1, 2, 3];
end;

begin
end.
