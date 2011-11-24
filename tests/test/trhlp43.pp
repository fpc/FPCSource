{ %NORUN }

{ This tests that methods introduced by a helper can be found in 
  with-Statements as well - Case 2: class method in current helper }
program trhlp43;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  TTest = record
  end;

  TTestHelper = record helper for TTest
    class procedure Test; static;
  end;

class procedure TTestHelper.Test;
begin
end;

var
  T: TTest;
begin
  with T do
    Test;
end.
