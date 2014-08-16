{$r+}
program project1;
{$t+}
var
  a : array of integer;
  i : integer;

procedure Foo(var c: array of integer);
begin
  writeln( (@c)^[1] );
  i:=(@c)^[1];
end;

begin
  i:=$1234;
  SetLength(a,5);
  a[0]:= 100;
  a[1]:= 101;
  foo(a);
  if i<>101 then
    halt(1);
  writeln('ok');
end.
