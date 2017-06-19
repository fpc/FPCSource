program testpassrc;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, tcscanner, tctypeparser, tcstatements,
  tcbaseparser, tcmoduleparser, tconstparser, tcvarparser, tcclasstype,
  tcexprparser, tcprocfunc, tcpassrcutil, tcresolver, tcgenerics,
  tcuseanalyzer, pasresolveeval;

type

  { TLazTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  Application := TMyTestRunner.Create(nil);
  DefaultFormat:=fplain;
  DefaultRunAllTests:=True;
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
