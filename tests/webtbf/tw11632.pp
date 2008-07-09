{ %fail }
{$mode objfpc}
program crash1;
procedure a(b: array of const); begin end;
begin
  a([0..1]);
end.
