{ %FAIL }

program tfuncref38;

{$mode objfpc}
{$ModeSwitch functionreferences}

type
  TFuncRef = reference to function: LongInt;

function Test(aArg: array of LongInt): TFuncRef;

  function TestSub: LongInt;
  begin
    Result := aArg[2];
  end;

begin
  Result := @TestSub;
end;

begin
end.

