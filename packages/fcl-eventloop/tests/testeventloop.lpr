program testeventloop;

{$mode objfpc}{$H+}

uses
  {$IFDEF FPC_VALGRIND}cmem,{$ENDIF}
  {$IFDEF UNIX}cwstring, cthreads,{$ENDIF}
  Classes, consoletestrunner, uteventloop;

type
  TEventLoopTestRunner = class(TTestRunner)
  end;

var
  Application: TEventLoopTestRunner;

begin
  DefaultRunAllTests := True;
  DefaultFormat := fPlain;
  Application := TEventLoopTestRunner.Create(nil);
  try
    Application.Initialize;
    Application.Title := 'fcl-eventloop unit tests';
    Application.Run;
  finally
    Application.Free;
  end;
end.
