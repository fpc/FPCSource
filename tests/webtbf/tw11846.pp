{ %OPT=-Sew -Oodfa }
{ %NORUN }
{ %fail }
{$mode objfpc}
function Test(a: integer): boolean;
var
  i: Integer;
begin
  for i:=1 to a do
    Result:=true;
end;

begin
end.
