{ %OPT = -gh }

program tarrconstr15;

{$mode objfpc}{$H+}

procedure CheckArray(Actual, Expected: array of String; Code: LongInt);
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
  arr: array[0..3] of String;
  i: SizeInt;
begin
  HaltOnNotReleased := True;

  arr := ['Alpha', 'Beta', 'Gamma', 'Delta'];
  CheckArray(arr, ['Alpha', 'Beta', 'Gamma', 'Delta'], 1);

  { ensure that everything is freed correctly }
  for i := Low(arr) to High(arr) do
    UniqueString(arr[i]);
end.
