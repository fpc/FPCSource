{ a helper of a parent class hides the parent's methods }
program tchlp38;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TTest = class
    function Test: Integer;
  end;

  TTestSub = class(TTest)

  end;

  TTestHelper = class helper for TTest
    function Test: Integer;
  end;

function TTest.Test: Integer;
begin
  Result := 1;
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
