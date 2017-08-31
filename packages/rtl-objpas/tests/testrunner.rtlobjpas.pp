{ %CONFIGFILE=fpcunit-console-defaults.ini testdefaults.ini }

program testrunner.rtlobjpas;

{$mode objfpc}{$H+}
{ Invoke needs a function call manager }
{.$define testinvoke}

uses
  consoletestrunner,
{$ifdef testinvoke}
  tests.rtti.invoke,
{$endif}
  tests.rtti;

var
  Application: TTestRunner;

begin
  Application := TTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'RTL-ObjPas unit tests';
  Application.Run;
  Application.Free;
end.
