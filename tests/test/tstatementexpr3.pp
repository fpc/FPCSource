{$ModeSwitch StatementExpressions}
var
  s: String;
begin
  s := case 5 of
    0: 'Foo';
    1..9: 'Bar';
    else 'FooBar';

  WriteLn(s);
  if (s<>'Bar') then
    Halt(1);
end.
