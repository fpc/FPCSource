{$mode objfpc}
{$h+}
program testsuite;

uses cmem,utests;

Var
  App : TTestSuite;

begin
  App:=TTestSuite.Create(nil);
  Try
    App.Title:='Free Pascal Compiler Test Suite Results';
    App.Initialize;
    App.Run;
  Finally
    App.Free;
  end;
end.