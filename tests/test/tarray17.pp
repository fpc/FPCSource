program tarray17;

{$mode objfpc}
{$modeswitch arrayoperators}
{$COperators on}

function CheckArray(aArr, aExpected: array of LongInt): Boolean;
var
  i: LongInt;
begin
  if Length(aArr) <> Length(aExpected) then
    Exit(False);
  for i := Low(aArr) to High(aArr) do
    if aArr[i] <> aExpected[i] then
      Exit(False);
  Result := True;
end;

var
  a: array of LongInt;
begin
  a := [1, 2, 3];

  a := a + [4];
  if not CheckArray(a, [1, 2, 3, 4]) then
    Halt(1);

  a := [0] + a;
  if not CheckArray(a, [0, 1, 2, 3, 4]) then
    Halt(2);

  a += [5];
  if not CheckArray(a, [0, 1, 2, 3, 4, 5]) then
    Halt(3);

  Writeln('ok');
end.
