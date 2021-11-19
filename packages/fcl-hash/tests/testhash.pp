{ %CONFIGFILE=fpcunit-console-defaults.ini testdefaults.ini }

program testhash;

{$mode objfpc}

uses
  consoletestrunner, utestsha256, utestonetimepass, utestsha512, utestpem, fpECC, fphashutils, fpsha256;

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

