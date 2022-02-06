{ %FAIL }

program tanonfunc45;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ test that capturing open array arguments is rejected }

type
  TIntFunc = reference to function: Integer;

function TestCaptureArray(A: array of Integer): TIntFunc;
begin
  Result := function: Integer
    var
      I: Integer;
    begin
      Result := 0;
      for I in A do
        Inc(Result, I)
    end;
end;

begin
  if TestCaptureArray([1, 2, 3, 4])() <> 10 then
    Halt(1);
end.

