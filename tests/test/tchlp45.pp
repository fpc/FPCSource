{ %NORUN }

{ tests whether the methods of a parent helper are usable in a derived helper }
program tchlp45;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TTest = class

  end;

  TTestHelper = class helper for TTest
    procedure Test;
  end;

  TTestHelperSub = class helper(TTestHelper) for TTest
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
