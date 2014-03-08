program testjs;

{$mode objfpc}{$H+}

uses
  cwstring,Classes, consoletestrunner, tcscanner, jsparser, jsscanner, jstree, jsbase,
  tcparser, jswriter, tctextwriter, tcwriter, jstoken;

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
