{$mode objfpc}

program tarrconstr12;

procedure CheckArray(Actual, Expected: array of Integer; Code: LongInt);
var
  i: SizeInt;
begin
  if Length(Actual) <> Length(Expected) then
    Halt(Code);
  for i := 0 to High(Actual) do
    if Actual[i] <> Expected[i] then
      Halt(Code);
end;

var
  a: array[0..2,0..2] of integer;
  i, j: integer;
begin
  a := [[1,2,3],[10,20,30],[100,200,300]];

  for i := 0 to 2 do
    for j := 0 to 2 do
      writeln(i,',',j,':',a[i,j]);

  CheckArray(a[0], [1, 2, 3], 1);
  CheckArray(a[1], [10,20,30], 2);
  CheckArray(a[2], [100,200,300], 3);
end.
