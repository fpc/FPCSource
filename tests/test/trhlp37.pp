{ test that helpers can access the methods of the parent helper using
  "inherited" }
program trhlp37;

{$ifdef fpc}
  {$mode objfpc}
  {$modeswitch advancedrecords}
{$endif}
{$apptype console}

type
  TTest = record

  end;

  TTestHelper = record helper for TTest
    function Test(aRecurse: Boolean): Integer;
  end;

  TTestHelperSub = record helper(TTestHelper) for TTest
    function Test(aRecurse: Boolean): Integer;
  end;

function TTestHelper.Test(aRecurse: Boolean): Integer;
begin
  Result := 1;
end;

function TTestHelperSub.Test(aRecurse: Boolean): Integer;
begin
  if aRecurse then
    Result := inherited Test(False)
  else
    Result := 2;
end;

var
  t: TTest;
  res: Integer;
begin
  res := t.Test(True);
  Writeln('t.Test: ', res);
  if res <> 1 then
    Halt(1);
  Writeln('ok');
end.
