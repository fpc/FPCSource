{ Console test runner for fcl-res. }
program testfclres;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, tcRCParser;

var
  lApp: TTestRunner;

begin
  DefaultFormat:=fPlain;
  DefaultRunAllTests:=True;
  lApp:=TTestRunner.Create(nil);
  lApp.Initialize;
  lApp.Title:='fcl-res tests';
  lApp.Run;
  lApp.Free;
end.
