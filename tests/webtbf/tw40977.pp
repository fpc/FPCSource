{ %fail }
program Project1;

procedure foo2;
begin
end;

procedure foo(bar: integer);
var x: integer absolute foo(1);
begin
  foo2;
end;

begin
  foo(1);
end.
