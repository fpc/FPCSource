{ %NORUN }

{ this tests that helpers can introduce class methods for classes - mode
  Delphi }
program tchlp2;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest = class

  end;

  TTestHelper = class helper for TTest
    class procedure Test;
  end;

class procedure TTestHelper.Test;
begin

end;

begin
  TTest.Test;
end.

