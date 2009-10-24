program tforin4;

{$APPTYPE CONSOLE}

type
  TMyArray = array of integer;

function return_array: TMyArray;
begin
  SetLength(result, 3);
  result[0] := 1;
  result[1] := 2;
  result[2] := 3;
end;

procedure TestArrayReturn;
var
  i: integer;
begin
  for i in return_array do
    WriteLn(i);
end;

procedure TestDynamicArray;
var
  i: integer;
  a: array of integer;
begin
  setlength(a, 3);
  a[0]:=1;
  a[1]:=2;
  a[2]:=3;
  for i in a do
  begin
    WriteLn(i);
    a[2] := -1;
  end;
end;

procedure TestOpenArray(a: array of integer);
var
  i: integer;
begin
  for i in a do
  begin
    WriteLn(i);
    a[2] := -1;
  end;
end;

begin
  TestOpenArray([1,2,3]);
  TestDynamicArray;
  TestArrayReturn;
end.

