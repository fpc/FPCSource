{%FAIL}
{ case-expression must end with end }
{$ModeSwitch StatementExpressions}
var
  s: String;
begin
  s := case 5 of
    0: 'Foo';
    else 'Bar';
  Halt(1);
end.
