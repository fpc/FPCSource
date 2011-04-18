{ a helper of a parent class hides methods in the child class if its also a
  parent of the helper for the child class }
program tchlp90;

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
    function Test: Integer;
  end;

  TTestSubHelper = class helper(TTestHelper) for TTestSub
  end;

function TTest.Test: Integer;
begin
  Result := 1;
end;

function TTestSub.Test: Integer;
begin
  Result := 4;
end;

function TTestHelper.Test: Integer;
begin
  Result := 2;
end;

var
  t: TTestSub;
  res: Integer;
begin
  t := TTestSub.Create;
  res := t.Test;
  Writeln('b.TestFoo: ', res);
  if res <> 2 then
    Halt(1);
  Writeln('ok');
end.
