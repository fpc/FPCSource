{ %target=win32,win64,linux,freebsd,darwin }
{ TThread.WaitFor on a FreeOnTerminate thread that frees itself during the wait, issue #41378 }
program twaitfreeonterm;

{$mode objfpc}{$h+}

uses
{$ifdef unix}
  cthreads, BaseUnix,
{$endif}
{$ifdef windows}
  Windows,
{$endif}
  SysUtils, Classes;

type
  TFreeThread = class(TThread)
  protected
    procedure Execute; override;
  public
    destructor Destroy; override;
  end;

  TRetThread = class(TThread)
  protected
    procedure Execute; override;
  end;

  TWaiter = class(TThread)
  protected
    procedure Execute; override;
  public
    // Thread to wait for
    Target: TThread;
    // Result of Target.WaitFor
    WaitResult: Integer;
  end;

  THelper = class
    // OnTerminate handler, queues DoSleep
    procedure DoTerminate(aSender: TObject);
    // Keeps the main thread busy while the thread finishes
    procedure DoSleep;
  end;

var
  gDestroyCount: LongInt;


procedure TFreeThread.Execute;

begin
  Sleep(100);
  ReturnValue:=7;
end;


destructor TFreeThread.Destroy;

begin
  InterlockedIncrement(gDestroyCount);
  inherited Destroy;
end;


procedure TRetThread.Execute;

begin
  Sleep(100);
  ReturnValue:=42;
end;


procedure TWaiter.Execute;

begin
  WaitResult:=Target.WaitFor;
end;


procedure THelper.DoTerminate(aSender: TObject);

begin
  TThread.Queue(nil,@DoSleep);
end;


procedure THelper.DoSleep;

begin
  Sleep(500);
end;


{ Stops the process when the main thread has not finished after 10 seconds }
function Watchdog(aParam: Pointer): PtrInt;

begin
  Result:=0;
  Sleep(10000);
{$ifdef windows}
  ExitProcess(10);
{$else}
  FpExit(10);
{$endif}
end;


{ Waits in the main thread until aThread has finished }
procedure WaitFinished(aThread: TThread);

begin
  while not aThread.Finished do
    CheckSynchronize(10);
end;


var
  lFree: TFreeThread;
  lRet: TRetThread;
  lWaiter: TWaiter;
  lHelper: THelper;
  lId: TThreadID;

begin
  BeginThread(@Watchdog,nil,lId);

  lHelper:=THelper.Create;
  lFree:=TFreeThread.Create(True);
  lFree.FreeOnTerminate:=True;
  lFree.OnTerminate:=@lHelper.DoTerminate;
  lFree.Start;
  if lFree.WaitFor<>7 then
    halt(1);
  if gDestroyCount<>1 then
    halt(2);
  lHelper.Free;

  lFree:=TFreeThread.Create(True);
  lFree.FreeOnTerminate:=True;
  lWaiter:=TWaiter.Create(True);
  lWaiter.Target:=lFree;
  lWaiter.Start;
  lFree.Start;
  WaitFinished(lWaiter);
  if lWaiter.WaitResult<>7 then
    halt(3);
  lWaiter.Free;
  if gDestroyCount<>2 then
    halt(4);

  lRet:=TRetThread.Create(False);
  if lRet.WaitFor<>42 then
    halt(5);
  lRet.Free;

  lRet:=TRetThread.Create(False);
  lWaiter:=TWaiter.Create(True);
  lWaiter.Target:=lRet;
  lWaiter.Start;
  WaitFinished(lWaiter);
  if lWaiter.WaitResult<>42 then
    halt(6);
  lWaiter.Free;
  lRet.Free;

  lRet:=TRetThread.Create(False);
  lRet.Free;
  lRet:=TRetThread.Create(True);
  lRet.Free;
  writeln('ok');
end.
