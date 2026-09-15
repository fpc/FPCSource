{$Mode ISO}
{$ModeSwitch StatementExpressions}
program tstatementexpr41(output);
var
  i: Integer;
begin
  i := case 5 of
    0: 3;
    1: 4;
    otherwise 42
  end;

  WriteLn(i);
  if (i<>42) then
    Halt(1);
end.
