{ %OPT=-Sew -vw -Oodfa }
{ %NORUN }
{$mode objfpc}
function Test(a: integer): boolean;
var
  i: Integer;
begin
  for i:=1 to 2 do
    Result:=true;
end;

begin
end.
