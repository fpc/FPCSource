program testidl;

{$mode objfpc}
{$H+}

uses
  consoletestrunner, webidlscanner, tcidlscanner, webidlparser, webidldefs,
  tcidlparser, tcwebidldefs;

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

