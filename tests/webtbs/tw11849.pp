{ %OPT=-Sew -Oodfa }
{ %norun }
{$mode objfpc}
procedure GiveMe(out i: integer);
begin
  i:=0;
end;

function Test(a: integer): integer;
var
  i: integer;
begin
  GiveMe(i);
  Result:=i;
end;


begin
end.
