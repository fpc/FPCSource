{ %FAIL }

program tb0247;

{$WARN 4122 ERROR}

{$mode objfpc}

type
  TTest = class abstract

  end;

  TTestClass = class of TTest;

  TTestSub = class

  end;

var
  o: TObject;
  c: TTestClass;
begin
  { this should not create an error }
  o := c.Create;
  { this neither }
  o := TTestSub.Create;
  { but this should create an error }
  o := TTest.Create;
end.
