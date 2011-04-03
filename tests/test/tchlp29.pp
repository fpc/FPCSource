{ class helpers don't hide methods of the subclasses of the extended class }
program tchlp29;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest = class
    function Test: Integer;
  end;

  TTestHelper = class helper for TTest
    function Test: Integer;
  end;

  TTestSub = class(TTest)
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

function TTestSub.Test: Integer;
begin
  Result := 3;
end;

var
  t: TTestSub;
begin
  t := TTestSub.Create;
  if t.Test <> 3 then
    Halt(1);
  Writeln('ok');
end.

