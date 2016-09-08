program tarray11;

{$mode objfpc}

type
  TLongIntArray = array of LongInt;

  ITest = interface
  end;

  TITestArray = array of ITest;

  TTest = class(TInterfacedObject, ITest)
  private
    fValue: LongInt;
  public
    constructor Create(aValue: LongInt);
    destructor Destroy; override;
  end;

var
  freed: array of LongInt;

constructor TTest.Create(aValue: LongInt);
begin
  fValue := aValue;
end;

destructor TTest.Destroy;
begin
  SetLength(freed, Length(freed) + 1);
  freed[High(freed)] := fValue;
  inherited;
end;

procedure CheckArray(a, b: array of LongInt; err: LongInt);
var
  i: LongInt;
begin
  if Length(a) <> Length(b) then
    Halt(err);
  for i := Low(a) to High(a) do begin
    if a[i] <> b[i] then
      Halt(err + 1);
  end;
end;

function CreateArray(len: LongInt): TLongIntArray;
var
  i: LongInt;
begin
  SetLength(Result, len);
  for i := 0 to len - 1 do
    Result[i] := i;
end;

procedure CreateArrayTest(len: LongInt; out arr: TITestArray);
var
  i: LongInt;
begin
  SetLength(arr, len);
  for i := 0 to len - 1 do
    arr[i] := TTest.Create(i);
end;

procedure CheckFreedArray(arr: array of LongInt; err: LongInt);
var
  l, f: LongInt;
  found: Boolean;
begin
  if Length(freed) <> Length(arr) then
    Halt(err);
  for f in freed do begin
    found := false;
    for l in arr do
      if l = f then begin
        found := true;
        break;
      end;
    if not found then
      Halt(err + 1);
  end;
end;

{procedure PrintArray(a: array of LongInt);
var
  i: LongInt;
begin
  Writeln('Length: ', Length(a));
  Write('Data: ');
  for i := Low(a) to High(a) do begin
    if i > Low(a) then
      Write(', ');
    Write(a[i]);
  end;
  Writeln;
end;}

var
  code: LongInt;

function next: LongInt;
begin
  code := code + 2;
  next := code;
end;

var
  a, b: TLongIntArray;
  c, d: TITestArray;
begin
  code := 0;

  { remove from the middle }
  a := CreateArray(10);
  Delete(a, 2, 4);
  CheckArray(a, [0, 1, 6, 7, 8, 9], next);

  { remove from the beginning }
  a := CreateArray(10);
  Delete(a, 0, 4);
  CheckArray(a, [4, 5, 6, 7, 8, 9], next);

  { remove from the end }
  a := CreateArray(10);
  Delete(a, 6, 4);
  CheckArray(a, [0, 1, 2, 3, 4, 5], next);

  { delete whole array }
  a := CreateArray(10);
  Delete(a, 0, 10);
  CheckArray(a, [], next);

  { out of bounds start and count are ignored }
  a := CreateArray(5);
  Delete(a, -1, 0);
  CheckArray(a, [0, 1, 2, 3, 4], next);
  a := CreateArray(5);
  Delete(a, -1, 2);
  PrintArray(a);
  CheckArray(a, [0, 1, 2, 3, 4], next);
  a := CreateArray(5);
  Delete(a, -1, -1);
  CheckArray(a, [0, 1, 2, 3, 4], next);
  a := CreateArray(5);
  Delete(a, 2, -1);
  CheckArray(a, [0, 1, 2, 3, 4], next);
  a := CreateArray(5);
  Delete(a, 5, 1);
  CheckArray(a, [0, 1, 2, 3, 4], next);
  a := CreateArray(5);

  { count is capped to the array's end }
  a := CreateArray(5);
  Delete(a, 3, 4);
  CheckArray(a, [0, 1, 2], next);

  { check that Delete does not influence copies }
  a := CreateArray(5);
  b := a;
  Delete(a, 2, 2);
  CheckArray(a, [0, 1, 4], next);
  CheckArray(b, [0, 1, 2, 3, 4], next);
  Delete(b, 1, 3);
  CheckArray(a, [0, 1, 4], next);
  CheckArray(b, [0, 4], next);

  { ensure that reference counted types are freed correctly }
  CreateArrayTest(5, c);
  Delete(c, 2, 2);
  CheckFreedArray([2, 3], next);
  freed := nil;
  c := nil;
  CheckFreedArray([0, 1, 4], next);
  freed := nil;

  { ensure that reference counted types are not destroyed if there's still a
    reference to them }
  CreateArrayTest(5, c);
  d := c;
  Delete(c, 2, 2);
  CheckFreedArray([], next);
  freed := nil;
  c := nil;
  CheckFreedArray([], next);
  freed := nil;
  d := nil;
  CheckFreedArray([0, 1, 2, 3, 4], next);
end.
