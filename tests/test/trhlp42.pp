{ %NORUN }

{ This tests that methods introduced by a helper can be found in 
  with-Statements as well - Case 1: normal method in current helper }
program trhlp42;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  TTest = record
  end;

  TTestHelper = record helper for TTest
    procedure Test;
  end;

procedure TTestHelper.Test;
begin
end;

var
  t: TTest;
begin
  with t do
    Test;
end.
