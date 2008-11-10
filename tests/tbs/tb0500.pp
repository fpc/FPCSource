{ %OPT=-Seh -vh }
program test;
uses math;

{ compiler claims that math isn't used, while the ** operator is used from it }

function f(r: real; i: integer): int64;
begin
  f:= trunc(r**i);
end;

begin
  writeln(f(2.0,2));
end.

