program ttlist;

{$MODE objfpc}{$H+}

uses
  Classes;

procedure Fail;
begin
  Writeln('Err!');
  Halt(1);
end;

function PtrNumComparer(P1, P2: Pointer): Integer;
begin
  if PtrUInt(P1) > PtrUInt(P2) then
    Result := 1
  else if PtrUInt(P1) < PtrUInt(P2) then
    Result := -1
  else
    Result := 0;
end;

procedure TestSort;
const
  NumTests = 100;
  NumItems = 20;
  MaxNum = 65535;
type
  TTestArray = array [0..NumItems - 1] of PtrUInt;
var
  Lst: TList;
  T, I, J: Integer;
  TstArr: TTestArray;
  SortedArr: TTestArray;

  procedure BubbleSort(var Arr: TTestArray);
  var
    Done: Boolean;
    I: Integer;
    tmp: PtrUInt;
  begin
    repeat
      Done := True;
      for I := Low(Arr) to High(Arr) - 1 do
        if Arr[I] > Arr[I + 1] then
        begin
          tmp := Arr[I];
          Arr[I] := Arr[I + 1];
          Arr[I + 1] := tmp;
          Done := False;
        end;
    until Done;
  end;

begin
  for T := 1 to NumTests do
  begin
    for I := Low(TstArr) to High(TstArr) do
      TstArr[I] := PtrUInt(Random(MaxNum));
    Lst := TList.Create;
    try
      for I := Low(TstArr) to High(TstArr) do
        Lst.Add(Pointer(TstArr[I]));
      Lst.Sort(@PtrNumComparer);

      SortedArr := TstArr;
      BubbleSort(SortedArr);

      for I := Low(SortedArr) to High(SortedArr) do
        if PtrUInt(Lst[I]) <> SortedArr[I] then
          Fail;
    finally
      Lst.Free;
    end;
  end;
end;

begin
  TestSort;
  Writeln('Ok!');
end.
