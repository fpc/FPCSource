{ %NORUN }

{ this tests that helpers can introduce class methods for records - mode
  ObjFPC }
program trhlp4;

{$ifdef fpc}
  {$mode objfpc}
  {$modeswitch advancedrecords}
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

