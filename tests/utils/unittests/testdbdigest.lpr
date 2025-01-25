program testdbdigest;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, tctestsql, digestanalyst, tsdb, tstypes, tsstring, tsutils, tcsetup, tcanalyst, tctsutils;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  // Will stop V_ERROR from exiting.
  IsCGI:=True;
  DefaultRunAllTests:=True;
  DefaultFormat:=fPlain;
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'FPCUnit Console test runner';
  Application.Run;
  Application.Free;
end.
