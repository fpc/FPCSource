program fclbase_unittests;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, tests_fptemplate, tests_rtti;

var
  Application: TTestRunner;

begin
  Application := TTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'FCL-Base unittests';
  Application.Run;
  Application.Free;
end.
