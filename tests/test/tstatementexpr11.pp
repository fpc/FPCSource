{$ModeSwitch StatementExpressions}
var
  sz: SizeInt;
begin
  sz := sizeOf((
    case 5 of
    0: 'Foo';
    5: widestring('Bar');
    else 'FooBar'
    end
  )[1]);
  WriteLn(sz);
  if (sz<>2) then Halt(1);
end.
