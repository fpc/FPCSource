{ %opt=-vh -Seh }
{ %norun }
{$mode objfpc}
uses
  uw21808a, uw21808b;

var
  x: TC = nil;
begin
  x.Q; // hint should appear only when this line is commented out
end.
