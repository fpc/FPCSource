{ %fail }
{$mode objfpc}{$H+}{$R+}
type
  TmyRange = 1..10;
var
  x: integer;
  Mark: set of TmyRange;
begin
  Mark := [5,11];{<-Why doesn't 11 cause a range check error?}
  for x in Mark do write(x,' IN Mark');
end.
