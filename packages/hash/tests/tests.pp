{ %CONFIGFILE=fpcunit-console-defaults.ini testdefaults.ini }

program tests;

{$mode objfpc}

uses
  consoletestrunner, TestsHMAC, testsha256, testonetimepass;

var
  Application: TTestRunner;
begin
  DefaultFormat:=fPlain;
  DefaultRunAllTests:=True;
  Application := TTestRunner.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.

