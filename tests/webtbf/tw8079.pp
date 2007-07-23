{ %opt=-vw -Sew -Oodfa }
{ %fail }
program test;

function foo(x: integer): integer;
begin
  if x > 10 then
    exit(10);
end;

begin
  foo(4);
  foo(12);
end.