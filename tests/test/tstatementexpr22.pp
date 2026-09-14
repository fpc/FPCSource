{$ModeSwitch StatementExpressions}
var
  v: Variant;
begin
  v := if 0 < 1 then 'Foo' else Variant('Bar');
  WriteLn(v);
  if (v<>'Foo') then
    Halt(1);
end.
