program tanonfunc43;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ test capturing record and array variables }

type
  TIntFunc = reference to function: Integer;

function TestCaptureArray: TIntFunc;
var
  A: array[0..15] of Integer;
begin
  Result := function: Integer
    begin
      Result := A[0] + A[1];
    end;
  A[0] := 1;
  A[1] := 2;
end;

function TestCaptureRecord: TIntFunc;
var
  R: record
    I1, I2: Integer;
  end;
begin
  Result := function: Integer
    begin
      Result := R.I1 + R.I2;
    end;
  R.I1 := 1;
  R.I2 := 2;
end;

begin
  if TestCaptureArray()() <> 3 then
    Halt(1);

  if TestCaptureRecord()() <> 3 then
    Halt(2);
end.

