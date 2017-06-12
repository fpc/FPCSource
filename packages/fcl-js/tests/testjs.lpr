program testjs;

{$mode objfpc}{$H+}

uses
  {$IFDEF Unix}
  cwstring,
  {$ENDIF}
  Classes, consoletestrunner, tcscanner, jsparser, jsscanner, jstree, jsbase,
  tcparser, jswriter, tcwriter, jstoken, JSSrcMap, TCSrcMap;

var
  Application: TTestRunner;

{$IFDEF WINDOWS}{$R testjs.rc}{$ENDIF}

begin
  DefaultFormat:=fplain;
  DefaultRunAllTests:=True;
  Application := TTestRunner.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
