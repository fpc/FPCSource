{$mode objfpc}{$H+}

{$R-}

type
  TmyRange = 1..10;
var
  x: integer;
  Mark: set of TmyRange;
begin
  Mark := [5,11]; { don't throw a range check error here if range checking is off }
  for x in Mark do write(x,' IN Mark');
  writeln;
end.
