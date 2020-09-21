program tarrconstr16;

type
  TEnum = (
    teOne,
    teTwo,
    teThree
  );

  TTest1 = array[0..2] of LongInt;
  TTest2 = array[1..3] of LongInt;
  TTest3 = array[TEnum] of LongInt;
  TTest4 = array[-1..1] of LongInt;

procedure CheckArray(Actual, Expected: array of LongInt; Code: LongInt);
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
  arr1: TTest1;
  arr2: TTest2;
  arr3: TTest3;
  arr4: TTest4;
begin
  FillChar(arr1, SizeOf(arr1), 0);
  FillChar(arr2, SizeOf(arr2), 0);
  FillChar(arr3, SizeOf(arr3), 0);
  FillChar(arr4, SizeOf(arr4), 0);

  arr1 := [1, 2, 3];
  CheckArray(arr1, [1, 2, 3], 1);

  arr2 := [1, 2, 3];
  CheckArray(arr2, [1, 2, 3], 2);

  arr3 := [1, 2, 3];
  CheckArray(arr3, [1, 2, 3], 3);

  arr4 := [1, 2, 3];
  CheckArray(arr4, [1, 2, 3], 4);
end.
