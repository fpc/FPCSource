{ Console test runner for ppufiles and ppumove. }
program testppuutils;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, tcPPUUtils;

var
  lApp: TTestRunner;

begin
  DefaultFormat:=fPlain;
  DefaultRunAllTests:=True;
  lApp:=TTestRunner.Create(nil);
  lApp.Initialize;
  lApp.Title:='ppufiles and ppumove tests';
  lApp.Run;
  lApp.Free;
end.
