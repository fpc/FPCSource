{ %OPT=-Sew -Oodfa }
{ %NORUN }
{$mode objfpc}
function Test(a: integer): boolean;
var
  i: Integer;
begin
  for i:=2 to 1 do
    Result:=true;
  Result:=true;
end;

begin
end.
