{ %FAIL }

program tfuncref37;

{$mode objfpc}
{$ModeSwitch functionreferences}

type
  TFuncRef = reference to function: LongInt;

function Test(var aArg: LongInt): TFuncRef;

  function TestSub: LongInt;
  begin
    Result := aArg;
  end;

begin
  Result := @TestSub;
end;

begin
end.

