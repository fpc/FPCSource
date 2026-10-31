{ %SKIPTARGET=$nothread }
{ TThread.Free closes the thread handle once, issue #41955 }
program tthreadclose;

{$mode objfpc}

uses
{$ifdef unix}
  cthreads,
{$endif}
  Classes;

type
  TTestThread = class(TThread)
  protected
    procedure Execute; override;
  end;

var
  OriginalManager, TestManager: TThreadManager;
  ExpectedHandle: TThreadHandle;
  CloseCount: Integer;
  Failed: Boolean;


procedure TTestThread.Execute;

begin
end;


// Counts the closes of ExpectedHandle and forwards only the first one
function CountCloseThread(aHandle: TThreadHandle): DWord;

begin
  if aHandle=ExpectedHandle then
    begin
    Inc(CloseCount);
    if CloseCount>1 then
      Exit(0);
    end;
  Result:=OriginalManager.CloseThread(aHandle);
end;


// Frees a thread, optionally after WaitFor, and checks that its handle was closed once
procedure CheckClose(aWaitFirst: Boolean);

var
  lWorker: TTestThread;

begin
  CloseCount:=0;
  lWorker:=TTestThread.Create(False);
  ExpectedHandle:=lWorker.Handle;
  if aWaitFirst then
    lWorker.WaitFor;
  lWorker.Free;
  if CloseCount<>1 then
    begin
    WriteLn('WaitFirst=',aWaitFirst,': CloseThread called ',CloseCount,' times');
    Failed:=True;
    end;
end;


begin
  GetThreadManager(OriginalManager);
  TestManager:=OriginalManager;
  TestManager.CloseThread:=@CountCloseThread;
  if not SetThreadManager(TestManager) then
    Halt(2);
  CheckClose(True);
  CheckClose(False);
  if not SetThreadManager(OriginalManager) then
    Halt(3);
  if Failed then
    Halt(1);
  WriteLn('ok');
end.
