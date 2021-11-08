program testfpweb;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, tchttproute, tcjwt, jsonparser,
  fpjwasha256, fpjwasha512, fpjwasha384, fpjwaes256;

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
  Application.Title := 'FPCUnit Console test runner';
  Application.Run;
  Application.Free;
end.
