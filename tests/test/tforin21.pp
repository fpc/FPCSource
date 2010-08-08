{ %FAIL}
{$mode objfpc}
{$apptype console}

operator enumerator(a: Integer): Integer;
begin
  Result := 0;
end;

var
  i: Integer;
begin
  for i in 1 do Writeln(i);
end.
