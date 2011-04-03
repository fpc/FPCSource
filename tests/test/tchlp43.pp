{ %NORUN }

{ for helpers Self always refers to the extended class }
program tchlp43;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

type
  TTest = class
    procedure DoTest(aTest: TTest);
  end;

  TTestHelper = class helper for TTest
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
  t := TTest.Create;
  t.Test;
end.

