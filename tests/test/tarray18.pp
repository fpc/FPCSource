program tarray18;

{$mode objfpc}
{$modeswitch advancedrecords}

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

type
  TTest1 = record
    f: array of LongInt;
    class operator := (a: array of LongInt): TTest1;
  end;

  TTest2 = record
    f: array of LongInt;
    class operator Explicit(a: array of LongInt): TTest2;
  end;

  TTest3 = record
    f: array of LongInt;
  end;

  TTest4 = record
    f: array of LongInt;
  end;

function AssignArray(a: array of LongInt): specialize TArray<LongInt>;
var
  i: LongInt;
begin
  SetLength(Result, Length(a));
  for i := 0 to High(a) do
    Result[i] := a[i];
end;

class operator TTest1.:=(a: array of LongInt): TTest1;
begin
  Result.f := AssignArray(a);
end;

class operator TTest2.Explicit(a: array of LongInt): TTest2;
begin
  Result.f := AssignArray(a);
end;

operator :=(a: array of LongInt): TTest3;
begin
  Result.f := AssignArray(a);
end;

operator :=(a: array of LongInt): TTest4;
begin
  Result.f := AssignArray(a);
end;

procedure Test1(aRec: TTest1; a: array of LongInt; aCode: LongInt);
begin
  if not CheckArray(aRec.f, a) then
    Halt(aCode);
end;

procedure Test2(aRec: TTest2; a: array of LongInt; aCode: LongInt);
begin
  if not CheckArray(aRec.f, a) then
    Halt(aCode);
end;

procedure Test3(aRec: TTest3; a: array of LongInt; aCode: LongInt);
begin
  if not CheckArray(aRec.f, a) then
    Halt(aCode);
end;

procedure Test4(aRec: TTest4; a: array of LongInt; aCode: LongInt);
begin
  if not CheckArray(aRec.f, a) then
    Halt(aCode);
end;

var
  t1: TTest1;
  t2: TTest2;
  t3: TTest3;
  t4: TTest4;
begin
  t1 := [];
  if not CheckArray(t1.f, []) then
    Halt(1);
  t1 := [2, 4];
  if not CheckArray(t1.f, [2, 4]) then
    Halt(2);
  t1 := TTest1([]);
  if not CheckArray(t1.f, []) then
    Halt(3);
  t1 := TTest1([2, 4]);
  if not CheckArray(t1.f, [2, 4]) then
    Halt(4);

  t2 := TTest2([]);
  if not CheckArray(t2.f, []) then
    Halt(5);
  t2 := TTest2([2, 4]);
  if not CheckArray(t2.f, [2, 4]) then
    Halt(6);

  t3 := [];
  if not CheckArray(t3.f, []) then
    Halt(7);
  t3 := [2, 4];
  if not CheckArray(t3.f, [2, 4]) then
    Halt(8);
  t3 := TTest3([]);
  if not CheckArray(t3.f, []) then
    Halt(9);
  t3 := TTest3([2, 4]);
  if not CheckArray(t3.f, [2, 4]) then
    Halt(10);

  t4 := TTest4([]);
  if not CheckArray(t4.f, []) then
    Halt(11);
  t4 := TTest4([2, 4]);
  if not CheckArray(t4.f, [2, 4]) then
    Halt(12);

  Test1([], [], 13);
  Test1([2, 4], [2, 4], 14);

  Test2(TTest2([]), [], 15);
  Test2(TTest2([2, 4]), [2, 4], 16);

  Test3([], [], 17);
  Test3([2, 4], [2, 4], 18);

  Test4(TTest4([]), [], 19);
  Test4(TTest4([2, 4]), [2, 4], 20);

  Writeln('ok');
end.
