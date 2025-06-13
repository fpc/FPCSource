{%FAIL}
{$ModeSwitch StatementExpressions}
var
  s: String;
begin
  s := case 5 of
    0: 'Foo';
    5: 32;
    else 'FooBar';
  Halt(1);
end.
