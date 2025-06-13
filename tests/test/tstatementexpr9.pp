{$Mode EXTENDEDPASCAL}
{$ModeSwitch StatementExpressions}
var
  i: Integer;
begin
  i := case 5 of
    0: 3;
    1..9: 42;
    otherwise 0;

  WriteLn(i);
  if (i<>42) then
    Halt(1);
end.
