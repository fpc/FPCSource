{ Console test runner for fpdoc. }
program testfpdoc;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, tcUnitProperties;

var
  lApp: TTestRunner;

begin
  DefaultFormat:=fPlain;
  DefaultRunAllTests:=True;
  lApp:=TTestRunner.Create(nil);
  lApp.Initialize;
  lApp.Title:='fpdoc tests';
  lApp.Run;
  lApp.Free;
end.
