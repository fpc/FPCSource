{ 
  Test provided by Ondrej Pokorny
  in work item #42102 
  ArrayOperators bug on i8086
}
program tarray24; // modified tarray17.pp

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
  a, b: array of LongInt;
begin
  a := [1, 2, 3];
  b := [4];

  a := a + b;
  if not CheckArray(a, [1, 2, 3, 4]) then
    Halt(1);

  b := [0];
  a := b + a;
  if not CheckArray(a, [0, 1, 2, 3, 4]) then
    Halt(2);

  b := [5];
  a += b;
  if not CheckArray(a, [0, 1, 2, 3, 4, 5]) then
    Halt(3);

  Writeln('ok');
end.

