{ %NORUN }

{ tests whether the methods of a parent helper are usable in a derived helper }
program trhlp36;

{$ifdef fpc}
  {$mode objfpc}
  {$modeswitch advancedrecords}
{$endif}
{$apptype console}

type
  TTest = record

  end;

  TTestHelper = record helper for TTest
    procedure Test;
  end;

  TTestHelperSub = record helper(TTestHelper) for TTest
    procedure AccessTest;
  end;

procedure TTestHelper.Test;
begin

end;

procedure TTestHelperSub.AccessTest;
begin
  Test;
end;

begin
end.
