{ %norun }
{ %opt=-O- }
type
  te = (gtNone,gtRadial,gtDiamond,gtAngular);
  ts = set of te;

var
  gt1,gt2 : te;

begin
  writeln;
  if not ([gt1,gt2] <= [gtRadial,gtDiamond,gtAngular]) then
    writeln;
end.

