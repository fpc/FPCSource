{ Runs the TInetServer tests. }
program tinetservertests;

{$mode objfpc}{$H+}

uses
{$ifdef unix}
  cthreads,
{$endif}
  Classes, consoletestrunner, tcinetserver;

var
  lApp: TTestRunner;

begin
  DefaultFormat:=fPlain;
  DefaultRunAllTests:=True;
  lApp:=TTestRunner.Create(nil);
  lApp.Initialize;
  lApp.Title:='inetservertests';
  lApp.Run;
  lApp.Free;
end.
