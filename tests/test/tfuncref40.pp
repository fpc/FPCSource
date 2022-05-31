{ %FAIL }

program tfuncref40;

{$mode objfpc}
{$ModeSwitch functionreferences}

type
  TFuncRef = reference to function: LongInt;

generic function Test<T>(var aArg: LongInt): TFuncRef;

  function TestSub: LongInt;
  begin
    Result := aArg;
  end;

begin
  Result := @TestSub;
end;

begin
end.

