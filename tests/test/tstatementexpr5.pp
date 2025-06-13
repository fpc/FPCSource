{%FAIL}
{$ModeSwitch StatementExpressions}
type TMyEnum = (meFirst, meSecond, meLast);

var
  s: String;
begin
  s := case meSecond of
    meFirst: 'Foo';
    meSecond: 'Bar';
  end;
  Halt(1);
end.
