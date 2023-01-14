program testidl;

{$mode objfpc}
{$H+}

uses
  {$ifdef unix}
  cwstring,
  {$endif}
  consoletestrunner, webidlscanner, tcidlscanner, webidlparser, webidldefs,
  tcidlparser, tcwebidldefs, tcwebidl2wasmjob;

Var
  Application : TTestRunner;

begin
  DefaultRunAllTests:=True;
  DefaultFormat:=fPlain;
  Application:=TTestRunner.Create(Nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.

