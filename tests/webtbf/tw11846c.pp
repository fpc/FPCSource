{ %OPT=-Sew -Oodfa }
{ %NORUN }
{ %fail }
{$mode objfpc}
function Test(a: integer): boolean;
var
  i: Integer;
begin
  for i:=2 to 1 do
    Result:=true;
end;

begin
end.
