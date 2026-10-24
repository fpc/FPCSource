{ %FAIL }
{ "type of" binds tighter than the binary operators, so "type of a+b" is
  "(type of a)+b", which is not a valid expression. }
program ttypeinquiry14;

{$mode objfpc}

var
  a: byte;
  b: byte;
  c: byte;
begin
  a:=1;
  b:=2;
  c:=type of a+b;
end.
