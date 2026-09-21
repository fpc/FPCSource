{
  Console runner for the fpwebsocketclient lifetime tests.

  Kept separate from testfpweb: these tests start threads and sockets, and
  one that hangs must not take the unrelated fcl-web tests down with it.
  Every test is guarded by a watchdog (see tcwsclienthelpers) which ends
  the process with exit code 99 after printing the test name to stderr.

  The tests need the loopback interface. TLS tests are ignored when no
  usable OpenSSL is available.
}
program testwebsocketclient;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads, BaseUnix,{$ENDIF}
  Classes, SysUtils, consoletestrunner,
  tcwsclienthelpers, tcwsclientbasic, tcwsclientpump, tcwsclientpeer,
  tcwsclientcallbacks, tcwsclientfree;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  {$IFDEF UNIX}
  { Writing to a socket whose peer has gone away raises SIGPIPE, which kills
    the process by default. Several tests deliberately shut sockets down
    under a live peer, so ignore it and let write() report EPIPE. }
  fpSignal(SIGPIPE,SignalHandler(SIG_IGN));
  {$ENDIF}
  Randomize;
  DefaultFormat:=fPlain;
  DefaultRunAllTests:=True;
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'FPCUnit Console test runner';
  Application.Run;
  Application.Free;
end.
