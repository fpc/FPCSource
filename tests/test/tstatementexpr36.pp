{%FAIL}
{ if-expression requires else }
{$ModeSwitch StatementExpressions}
var
  s: String;
begin
  s := if 0 < 1 then 'Foo';
  Halt(1);
end.
