{ %NORUN }

{ this tests that helpers can introduce instance methods for classes - mode
  ObjFPC }
program tchlp3;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

type
  TTest = class

  end;

  TTestHelper = class helper for TTest
    procedure Test;
  end;

procedure TTestHelper.Test;
begin

end;

var
  t: TTest;
begin
  t.Test;
end.

