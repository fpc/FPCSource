{%FAIL}
{$Mode ObjFPC}{$H+}
{$ModeSwitch StatementExpressions}
var
  s, c: String;
begin
  c:='Bar';
  s := case c of
    'Foo': 'Foo';
    'Bar': 'Bar';
    'FooBar': 'FooBar';
    // not exhaustive
  end;

  WriteLn(s);
  if (s<>'Bar') then
    Halt(1);
end.
