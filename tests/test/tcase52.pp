{ %FAIL }
{ ISO Pascal: else is not allowed in a case statement }
{$mode iso}
program tcase52(output);

var
  i: integer;
begin
  i:=5;
  case i of
    1: writeln(1);
  else
    writeln(2);
  end;
end.
