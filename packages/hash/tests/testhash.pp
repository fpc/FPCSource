{ %CONFIGFILE=fpcunit-console-defaults.ini testdefaults.ini }

program testhash;

{$mode objfpc}

uses
{$ifdef unix}
  cwstring,
{$endif}
  consoletestrunner, utestshmac;

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

