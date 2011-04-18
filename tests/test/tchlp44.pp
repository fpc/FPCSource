{ in a class helper Self always is of the type of the extended class }
program tchlp44;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TTest = class
    function Test: Integer;
  end;

  TTestSub = class(TTest)
    function Test: Integer;
  end;

  TTestHelper = class helper for TTest
    function AccessTest: Integer;
  end;

  TTestSubHelper = class helper(TTestHelper) for TTestSub
  end;

function TTest.Test: Integer;
begin
  Result := 1;
end;

function TTestSub.Test: Integer;
begin
  Result := 2;
end;

function TTestHelper.AccessTest: Integer;
begin
  Result := Test;
end;

var
  t: TTestSub;
  res: Integer;
begin
  t := TTestSub.Create;
  res := t.AccessTest;
  Writeln('t.AccessTest: ', res);
  if res <> 1 then
    Halt(1);
  Writeln('ok');
end.
