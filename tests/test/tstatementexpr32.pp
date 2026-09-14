{%FAIL}
{ statement expressions are disabled by default in mode objfpc }
{$Mode ObjFPC}
var
  s: String;
begin
  s := if 0 < 1 then 'Foo' else 'Bar';
  Halt(1);
end.
