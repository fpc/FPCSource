program tchlp42;

{$mode objfpc}

type
  TTest = class
    function Test: LongInt; virtual;
  end;

  TTestSub = class(TTest)
    function Test: LongInt; override;
  end;

  TTestHelper = class helper for TTest
    function Test: LongInt;
  end;

function TTestHelper.Test: LongInt;
begin
  Result := inherited Test * 10;
end;

function TTestSub.Test: LongInt;
begin
  Result := 2;
end;

function TTest.Test: LongInt;
begin
  Result := 1;
end;

var
  t: TTest;
begin
  t := TTestSub.Create;
  if t.Test <> 20 then
    Halt(1);
  Writeln('ok');
end.
