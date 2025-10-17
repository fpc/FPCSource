program testpassrc;

{$mode objfpc}{$H+}

uses
{$IFDEF UNIX}
  cwstring,
{$ENDIF}
  Classes,
  syntax.highlighter,
  syntax.pascal,
  syntax.bash,
  syntax.javascript,
  syntax.html,
  syntax.css,
  syntax.ini,
  syntax.sql,
  syntax.json,
  unittest.pascal,
  unittest.assembler,
  unittest.bash,
  unittest.javascript,
  unittest.css,
  unittest.html,
  unittest.ini,
  unittest.sql,
  unittest.json,
  unittest.htmlrender,
  consoletestrunner;

type

  { TLazTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  Application:=TMyTestRunner.Create(nil);
  DefaultFormat:=fplain;
  DefaultRunAllTests:=True;
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
