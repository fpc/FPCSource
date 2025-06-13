{$ModeSwitch StatementExpressions}
var
  s: String;
begin
  s := if 0 < 1 then 'Foo' else 'Bar' + 'Baz';
  WriteLn(s);
  if (s<>'Foo') then
    Halt(1);
end.
