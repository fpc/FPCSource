{ %FAIL }

program tfuncref41;

{$mode objfpc}
{$ModeSwitch functionreferences}

type
  TFuncRef = reference to function: LongInt;

generic function Test<T>(aArg: array of LongInt): TFuncRef;

  function TestSub: LongInt;
  begin
    Result := aArg[2];
  end;

begin
  Result := @TestSub;
end;

begin
end.

