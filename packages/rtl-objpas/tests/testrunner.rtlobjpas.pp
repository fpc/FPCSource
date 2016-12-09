program testrunner.rtlobjpas;

{$mode objfpc}{$H+}

uses
  simpletestrunner,
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
