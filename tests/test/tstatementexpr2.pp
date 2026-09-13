{$ModeSwitch StatementExpressions}
type TMyEnum = (meFirst, meSecond, meLast);

var
  s: String;
begin
  s := case meSecond of
    meFirst: 'Foo';
    meSecond: 'Bar';
    meLast: 'FooBar';
  end;

  WriteLn(s);
  if (s<>'Bar') then
    Halt(1);
end.
