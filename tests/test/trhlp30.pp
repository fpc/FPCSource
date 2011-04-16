{ %NORUN }

{ a helper can already be accessed when implementing a record's methods }
program trhlp30;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest = class
    procedure Test;
  end;

  TTestHelper = class helper for TTest
    procedure DoTest;
  end;

procedure TTest.Test;
begin
  DoTest;
end;

procedure TTestHelper.DoTest;
begin

end;

begin
end.
