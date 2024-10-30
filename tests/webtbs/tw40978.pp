program Project1;

var
  aa: integer public name 'aa';

procedure foo(bar: integer);
var x: integer absolute 'aa';
begin
  if x<>1234 then
    halt(1);
  x := 2;
  x := bar;
  writeln(x);
end;

begin
  aa:=1234;
  foo(1);
  if aa<>1 then
    halt(1);
end.
