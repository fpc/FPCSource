{$Mode ObjFPC}{$H+}
{$ModeSwitch StatementExpressions}
var
  s, c: String;
begin
  c:='Bar';
  s := case c of
    'Foo': 'Foo';
    'Bar': 'Bar';
    else 'FooBar';

  WriteLn(s);
  if (s<>'Bar') then
    Halt(1);
end.
