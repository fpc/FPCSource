{ %NORUN }

{ this tests that helpers can introduce instance methods for records - mode
  ObjFPC }
program trhlp2;

{$ifdef fpc}
  {$mode objfpc}
  {$modeswitch advancedrecords}
{$endif}

type
  TTest = record
  end;

  TTestHelper = record helper for TTest
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

