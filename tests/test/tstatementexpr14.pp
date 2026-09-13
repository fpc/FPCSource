{%FAIL}
{$ModeSwitch StatementExpressions}
var
  s: String;
begin
  s := if 0 < 1 then 'Foo' else 32; // cant be combined
  Halt(1);
end.
