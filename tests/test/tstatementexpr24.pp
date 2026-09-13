{$ModeSwitch StatementExpressions}
var
  sz: sizeint;
begin
  sz := SizeOf(if 0<1 then #65 else WideChar('A'));
  WriteLn(sz);
  if (sz<>SizeOf(WideChar)) then Halt(1);
end.
