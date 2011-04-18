{ %NORUN }

{ this tests that helpers can introduce class methods for records - mode
  Delphi }
program trhlp3;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest = record
  end;

  TTestHelper = record helper for TTest
    class procedure Test; static;
  end;

class procedure TTestHelper.Test;
begin

end;

begin
  TTest.Test;
end.

