program fpcunithttp2;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, tcfphttp2consts, tcfphttp2frames, tcfphttp2connection, tcfphttp2request, tcfphttp2server, tcfphttp2limits,sysutils;

type

  { TLazTestRunner }

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
  Application.Run;
  Application.Free;
end.
