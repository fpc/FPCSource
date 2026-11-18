{ Console test runner for instantfpc. }
program testinstantfpc;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, tcInstantFPC;

var
  lApp: TTestRunner;

begin
  DefaultFormat:=fPlain;
  DefaultRunAllTests:=True;
  lApp:=TTestRunner.Create(nil);
  lApp.Initialize;
  lApp.Title:='instantfpc tests';
  lApp.Run;
  lApp.Free;
end.
