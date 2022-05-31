{ %FAIL }

program tfuncref39;

{$mode objfpc}
{$ModeSwitch functionreferences}

type
  TProcRef = reference to procedure;

generic function Test<T>: LongInt;

  procedure TestSub;
  begin
    Writeln(Result);
  end;

var
  tmp: TProcRef;
begin
  tmp := @TestSub;
end;

begin
end.

