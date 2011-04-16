{ %NORUN }

{ this tests that helpers can introduce instance methods for records - mode
  Delphi }
program trhlp1;

{$ifdef fpc}
  {$mode delphi}
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
