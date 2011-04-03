{ %NORUN }

{ overloading needs to be enabled explicitly }
program tchlp33;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest = class
    procedure Test(const aTest: String);
  end;

  TTestHelper = class helper for TTest
    procedure Test; overload;
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
  t.Test;
  t.Test('Foo');
end.

