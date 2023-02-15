{ %opt=-CO -Seh }
{ %norun }
program test;

procedure foo;
var
  i: integer;
  a: array of integer;
begin
  a := NIL;
  for i in a do
  begin
  end;
end;

begin
  foo;
end.
