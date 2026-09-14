{$ModeSwitch StatementExpressions}
var
  sz: SizeInt;
begin
  sz := sizeOf((if 0<1 then 'Foo' else widestring('Bar'))[1]);
  WriteLn(sz);
  if (sz<>2) then Halt(1);
end.
