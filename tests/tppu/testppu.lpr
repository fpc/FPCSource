program testppu;

{$mode objfpc}{$H+}

uses
{$IFDEF UNIX}
  cwstring,
{$ENDIF}
  Classes, consoletestrunner, tcrecompile, tstppuutils;

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
