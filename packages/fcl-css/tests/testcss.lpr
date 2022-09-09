program testcss;

{$mode objfpc}{$H+}

uses
  Classes, sysutils, consoletestrunner, tcCSSScanner, tcCSSParser, tcCSSTree,
  tcCSSResolver;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;
  Dir: String;

begin
  DefaultFormat:=fPlain;
  DefaultRunAllTests:=True;
  Application := TMyTestRunner.Create(nil);
  Dir:=ExtractFilePath(Application.ExeName);
  if Dir<>'' then
    SetCurrentDir(Dir);
  Application.Initialize;
  Application.Title := 'CSS tests runner';
  Application.Run;
  Application.Free;
end.
