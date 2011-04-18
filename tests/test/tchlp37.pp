{ %NORUN }

{ helpers of a parent are available in a subclass as well }
program tchlp37;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TTest = class

  end;

  TTestSub = class(TTest)

  end;

  TTestHelper = class helper for TTest
    procedure Test;
  end;

procedure TTestHelper.Test;
begin

end;

var
  t: TTestSub;
begin
  t.Test;
end.
