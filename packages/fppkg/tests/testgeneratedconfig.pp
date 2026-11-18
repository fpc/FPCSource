{ Console test runner for the generated fppkg configuration. }
program testgeneratedconfig;

{$mode objfpc}{$H+}

uses
  {$ifdef unix}
  cthreads,
  {$endif unix}
  Classes, consoletestrunner, tcGeneratedConfig;

var
  lApp: TTestRunner;

begin
  DefaultFormat:=fPlain;
  DefaultRunAllTests:=True;
  lApp:=TTestRunner.Create(nil);
  lApp.Initialize;
  lApp.Title:='fppkg generated configuration tests';
  lApp.Run;
  lApp.Free;
end.
