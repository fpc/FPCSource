{ %FAIL }
{ Extended Pascal: else is not allowed in a case expression }
{$Mode EXTENDEDPASCAL}
{$ModeSwitch StatementExpressions}
program tstatementexpr42(output);
var
  i: Integer;
begin
  i := case 5 of
    0: 3;
    else 42
  end;
end.
