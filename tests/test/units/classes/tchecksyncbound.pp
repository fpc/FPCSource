{%skiptarget=$nothread }
{ CheckSynchronize only runs the entries queued when it is called, issue #34049 }
program tchecksyncbound;

{$mode objfpc}{$h+}

uses
{$ifdef unix}
  cthreads,
{$endif}
  SysUtils, Classes;

type
  TSyncThread = class(TThread)
  protected
    procedure Execute; override;
    procedure DoSync;
  end;

  TWatchdog = class(TThread)
  protected
    procedure Execute; override;
  end;

  THandlers = class
  public
    // Queues itself again
    procedure Requeue;
    // Removes the queued Removed calls and queues Added
    procedure RemoveAndAdd;
    // Counts its calls
    procedure Removed;
    // Counts its calls
    procedure Added;
    // Counts its calls
    procedure Delayed;
  end;

const
  SyncLimit = 300;

var
  gSyncCount: Integer = 0;
  gStop: Boolean = False;
  gDone: Boolean = False;
  gRequeueCount: Integer = 0;
  gRemovedCount: Integer = 0;
  gAddedCount: Integer = 0;
  gDelayedCount: Integer = 0;
  gHandlers: THandlers;


procedure TSyncThread.DoSync;

begin
  Inc(gSyncCount);
  Sleep(1);
end;


procedure TSyncThread.Execute;

begin
  while not gStop do
    Synchronize(@DoSync);
end;


procedure TWatchdog.Execute;

var
  i: Integer;
begin
  for i:=1 to 300 do
    begin
    Sleep(100);
    if gDone then
      exit;
    end;
  Writeln('Failed: CheckSynchronize does not return');
  Halt(1);
end;


procedure THandlers.Requeue;

begin
  Inc(gRequeueCount);
  TThread.ForceQueue(nil,@Requeue);
end;


procedure THandlers.RemoveAndAdd;

begin
  TThread.RemoveQueuedEvents(@Removed);
  TThread.ForceQueue(nil,@Added);
end;


procedure THandlers.Removed;

begin
  Inc(gRemovedCount);
end;


procedure THandlers.Added;

begin
  Inc(gAddedCount);
end;


procedure THandlers.Delayed;

begin
  Inc(gDelayedCount);
end;


procedure Check(aCondition: Boolean; aCode: Integer; const aMsg: string);

begin
  if not aCondition then
    begin
    Writeln('Failed: ',aMsg);
    Halt(aCode);
    end;
end;


{ Runs CheckSynchronize 100 times }
procedure DrainQueue;

var
  i: Integer;
begin
  for i:=1 to 100 do
    CheckSynchronize(0);
end;


procedure TestSynchronizeThreads;

var
  lThreads: array[1..2] of TSyncThread;
  i, lBefore, lMax, lCalls: Integer;
begin
  for i:=1 to 2 do
    lThreads[i]:=TSyncThread.Create(False);
  lMax:=0;
  lCalls:=0;
  while gSyncCount<SyncLimit do
    begin
    lBefore:=gSyncCount;
    CheckSynchronize(10);
    Inc(lCalls);
    if gSyncCount-lBefore>lMax then
      lMax:=gSyncCount-lBefore;
    end;
  gStop:=True;
  for i:=1 to 2 do
    begin
    while not lThreads[i].Finished do
      CheckSynchronize(10);
    lThreads[i].Free;
    end;
  Check(lCalls>1,1,'CheckSynchronize returns while two threads call Synchronize');
  Check(lMax<=2,2,'CheckSynchronize runs at most one entry per synchronizing thread');
end;


procedure TestRequeue;

var
  i: Integer;
begin
  gRequeueCount:=0;
  TThread.ForceQueue(nil,@gHandlers.Requeue);
  for i:=1 to 5 do
    begin
    Check(CheckSynchronize(0),10,'CheckSynchronize returns True when an entry ran');
    Check(gRequeueCount=i,11,'an entry queued by a queued method runs in the next call');
    end;
  TThread.RemoveQueuedEvents(@gHandlers.Requeue);
  Check(not CheckSynchronize(0),12,'CheckSynchronize returns False for an empty queue');
end;


procedure TestRemove;

begin
  TThread.ForceQueue(nil,@gHandlers.RemoveAndAdd);
  TThread.ForceQueue(nil,@gHandlers.Removed);
  TThread.ForceQueue(nil,@gHandlers.Removed);
  CheckSynchronize(0);
  Check(gRemovedCount=0,20,'removed entries do not run');
  DrainQueue;
  Check(gAddedCount=1,21,'entry queued after a removal runs');
end;


procedure TestDelayed;

var
  lStart: QWord;
begin
  TThread.ForceQueue(nil,@gHandlers.Delayed,200);
  TThread.ForceQueue(nil,@gHandlers.Added);
  gAddedCount:=0;
  Check(CheckSynchronize(0),30,'due entry behind a delayed entry runs');
  Check((gAddedCount=1) and (gDelayedCount=0),31,'delayed entry waits for its time');
  lStart:=GetTickCount64;
  while (gDelayedCount=0) and (GetTickCount64-lStart<5000) do
    CheckSynchronize(10);
  Check(gDelayedCount=1,32,'delayed entry runs after its time');
end;


var
  lWatchdog: TWatchdog;
begin
  lWatchdog:=TWatchdog.Create(False);
  gHandlers:=THandlers.Create;
  try
    TestSynchronizeThreads;
    TestRequeue;
    TestRemove;
    TestDelayed;
  finally
    gHandlers.Free;
  end;
  gDone:=True;
  lWatchdog.WaitFor;
  lWatchdog.Free;
  Writeln('ok');
end.
