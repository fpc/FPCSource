{ %NORUN }

{ method modifiers of the extended class are completly irrelevant }
program tchlp10;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest = class
    procedure Test; virtual;
  end;

  TTestHelper = class helper for TTest
    procedure Test; virtual;
  end;

  TTestHelperSub = class helper(TTestHelper) for TTest
    procedure Test; override;
  end;

procedure TTest.Test;
begin

end;

procedure TTestHelper.Test;
begin

end;

procedure TTestHelperSub.Test;
begin

end;

begin

end.
