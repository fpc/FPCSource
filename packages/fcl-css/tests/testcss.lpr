program testcss;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  {$IFDEF FPC_DOTTEDUNITS}
  UnixApi.CWString,
  {$ELSE FPC_DOTTEDUNITS}
  cwstring,
  {$ENDIF FPC_DOTTEDUNITS}
  {$ENDIF}
  {$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, FpcUnit.Runners.Console,
  {$ELSE FPC_DOTTEDUNITS}
  Classes, sysutils, consoletestrunner,
  {$ENDIF FPC_DOTTEDUNITS}
  tcCSSScanner, tcCSSParser, tcCSSTree, tcCSSResolver, tcCSSSkipInvalid;

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
