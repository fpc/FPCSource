{ %NORUN }

{ this tests that helpers can introduce class methods for classes - mode
  ObjFPC }
program tchlp4;

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

