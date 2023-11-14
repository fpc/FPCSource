program testcompat;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cwstring,{$ENDIF}
  Classes, consoletestrunner, tcnetencoding, tciotuils, utcimagelist;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  DefaultRunAllTests:=true;
  DefaultFormat:=fPlain;
  Application := TMyTestRunner.Create(nil);

  Application.Initialize;
  Application.Title:='testcompat';
  Application.Run;
  Application.Free;
end.
