program testcss;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, tccssScanner, tccssparser, tccsstree;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  DefaultFormat:=fPlain;
  DefaultRunAllTests:=True;
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'CSS tests runner';
  Application.Run;
  Application.Free;
end.
