{ %NORUN }

{ for helpers Self always refers to the extended record }
program trhlp35;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest = record
    procedure DoTest(aTest: TTest);
  end;

  TTestHelper = record helper for TTest
    procedure Test;
  end;

procedure TTest.DoTest(aTest: TTest);
begin

end;

procedure TTestHelper.Test;
begin
  DoTest(Self);
end;

var
  t: TTest;
begin
  t.Test;
end.

