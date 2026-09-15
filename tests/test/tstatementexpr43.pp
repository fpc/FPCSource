{ %FAIL }
{ ISO Pascal: else is not allowed in a case expression }
{$Mode ISO}
{$ModeSwitch StatementExpressions}
program tstatementexpr43(output);
var
  i: Integer;
begin
  i := case 5 of
    0: 3;
    else 42
  end;
end.
