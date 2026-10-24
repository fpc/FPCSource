{ %FAIL }
{ "type of x(b)" with a variable x is a type cast, which is a value and
  not a type, so it can not be used where a type is expected. }
program ttypeinquiry13;

{$mode objfpc}

var
  x: word;
  b: byte;
  v: type of x(b);
begin
  v:=1;
end.
