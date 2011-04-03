{ %FAIL }

{ overloading needs to be enabled explicitly }
program tchlp32;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest = class
    procedure Test(const aTest: String);
  end;

  TTestHelper = class helper for TTest
    procedure Test;
  end;

procedure TTest.Test(const aTest: String);
begin

end;

procedure TTestHelper.Test;
begin

end;

var
  t: TTest;
begin
  t := TTest.Create;
  t.Test('Foo');
end.

