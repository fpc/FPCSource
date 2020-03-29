program testjsonconf;

{$mode objfpc}{$H+}

uses
  {$ifdef unix}cwstring,{$endif}
  Classes, consoletestrunner, jsonconftest, jsonconf;

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
