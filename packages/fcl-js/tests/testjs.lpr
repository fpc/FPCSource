program testjs;

{$mode objfpc}{$H+}

uses
  {$IFDEF Unix}
  cwstring,
  {$ENDIF}
  Classes, consoletestrunner,
  // tests
  tcscanner, tcparser, tcwriter, TCSrcMap, tctsparser, tctstopas;

var
  Application: TTestRunner;

begin
  DefaultFormat:=fplain;
  DefaultRunAllTests:=True;
  Application := TTestRunner.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
