{ methods introduced by the helper's parent hide the record methods as well }
program trhlp31;

{$ifdef fpc}
  {$mode objfpc}
  {$modeswitch advancedrecords}
{$endif}

type
  TTest = record
    function Test: Integer;
  end;

  TTestHelper = record helper for TTest
    function Test: Integer;
  end;

  TTestHelperSub = record helper(TTestHelper) for TTest
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
  t: TTest;
  res: Integer;
begin
  res := t.Test;
  Writeln('t.Test: ', res);
  if res <> 2 then
    Halt(1);
  Writeln('ok');
end.
