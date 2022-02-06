program tanonfunc44;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ test capturing record and array arguments }

type
  TIntFunc = reference to function: Integer;

  TIntArr = array[0..15] of Integer;
  TIntRec = record
    I1, I2: Integer;
    P1, P2: Pointer;
  end;

function TestCaptureArray(A: TIntArr): TIntFunc;
begin
  Result := function: Integer
    begin
      Result := A[0] + A[1];
    end;
end;

function TestCaptureRecord(R: TIntRec): TIntFunc;
begin
  Result := function: Integer
    begin
      Result := R.I1 + R.I2;
    end;
end;

var
  A: TIntArr;
  R: TIntRec;
  F: TIntFunc;
begin
  A[0] := 1;
  A[1] := 2;
  F := TestCaptureArray(A);
  if F() <> 3 then
    Halt(1);

  A[0] := 3;
  if F() <> 3 then
    Halt(2);

  R.I1 := 1;
  R.I2 := 2;
  F := TestCaptureRecord(R);
  if F() <> 3 then
    Halt(3);

  R.I1 := 3;
  if F() <> 3 then
    Halt(3);
end.

