{ %FAIL }

{ instance methods in record helpers must be static }
program trhlp6;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest = record

  end;

  TTestHelper = record helper for TTest
    class procedure Test;
  end;

class procedure TTestHelper.Test;
begin

end;

begin
end.
