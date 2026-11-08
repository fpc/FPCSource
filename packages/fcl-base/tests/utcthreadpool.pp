{ Tests of the idle wait interval of TFPSimpleThreadPool from fpthreadpool }
unit utcThreadPool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpthreadpool, punit;

// Registers the TFPSimpleThreadPool suite
procedure RegisterTests;

implementation

type
  { TProbePool }

  TProbePool = class(TFPSimpleThreadPool)
  protected
    type
      TProbeList = class(TThreadPoolList)
      public
        // Create a thread and record its wait interval in the pool
        function AddThread: TAbstractTaskThread; override;
      end;
    function CreateThreadList: TAbstractThreadList; override;
  public
    // Wait intervals of the threads created so far
    Intervals: array of Integer;
    // Wait interval of a newly created thread list
    class function NewListInterval: Integer;
  end;

  { TStampTask }

  TStampTask = class(TThreadPoolTask)
  protected
    procedure DoExecute; override;
  public
    // Count the destroyed task in gTasksFreed
    destructor Destroy; override;
  end;

  { TWatchdog }

  TWatchdog = class(TThread)
  private
    FDone: PRTLEvent;
    FMsg: String;
  protected
    procedure Execute; override;
  public
    // Creates a started thread that halts the program with aMsg unless Stop is called within 10 seconds
    constructor Create(const aMsg: String);
    destructor Destroy; override;
    // Signal that the watched operation finished and wait for the thread
    procedure Stop;
  end;

var
  gPool: TProbePool;
  gRanAt: QWord;
  gTasksFreed: Integer;


function TProbePool.TProbeList.AddThread: TAbstractTaskThread;

var
  lPool: TProbePool;

begin
  Result:=inherited AddThread;
  lPool:=Pool as TProbePool;
  SetLength(lPool.Intervals,Length(lPool.Intervals)+1);
  lPool.Intervals[High(lPool.Intervals)]:=(Result as TTaskThread).WaitInterval;
end;


function TProbePool.CreateThreadList: TAbstractThreadList;

begin
  Result:=TProbeList.CreateList(Self);
end;


class function TProbePool.NewListInterval: Integer;

var
  lList: TThreadPoolList;

begin
  lList:=TThreadPoolList.CreateList(nil);
  try
    Result:=lList.ThreadTaskWaitInterval;
  finally
    lList.Free;
  end;
end;


procedure TStampTask.DoExecute;

begin
  gRanAt:=GetTickCount64;
end;


destructor TStampTask.Destroy;

begin
  InterLockedIncrement(gTasksFreed);
  inherited Destroy;
end;


constructor TWatchdog.Create(const aMsg: String);

begin
  FMsg:=aMsg;
  FDone:=RTLEventCreate;
  inherited Create(False);
end;


destructor TWatchdog.Destroy;

begin
  RTLEventDestroy(FDone);
  inherited Destroy;
end;


procedure TWatchdog.Execute;

begin
  RTLEventWaitFor(FDone,10000);
  if not Terminated then
    begin
    Writeln('FAILED: ',FMsg);
    Halt(2);
    end;
end;


procedure TWatchdog.Stop;

begin
  Terminate;
  RTLEventSetEvent(FDone);
  WaitFor;
end;


// Create gPool with a fixed number of threads
procedure CreatePool;

begin
  gPool:=TProbePool.Create;
  gPool.MinThreads:=4;
  gPool.MaxThreads:=4;
end;


// Free gPool, halting the program if that does not return within 10 seconds
procedure FreePool;

var
  lWatchdog: TWatchdog;

begin
  lWatchdog:=TWatchdog.Create('Freeing the thread pool did not return within 10 seconds');
  try
    FreeAndNil(gPool);
  finally
    lWatchdog.Stop;
    lWatchdog.Free;
  end;
end;


// True if every thread created by gPool has wait interval aInterval
function AllIntervalsAre(aInterval: Integer): Boolean;

var
  I: Integer;

begin
  Result:=Length(gPool.Intervals)>0;
  for I:=0 to High(gPool.Intervals) do
    if gPool.Intervals[I]<>aInterval then
      Result:=False;
end;


function SetIntervalZero: TTestString;

begin
  Result:='';
  gPool.ThreadTaskWaitInterval:=0;
end;


function SetIntervalNegative: TTestString;

begin
  Result:='';
  gPool.ThreadTaskWaitInterval:=-5;
end;


function TestDefaultInterval: TTestString;

begin
  Result:='';
  CreatePool;
  try
    AssertEquals('Pool default ThreadTaskWaitInterval', DefaultThreadTaskWaitInterval, gPool.ThreadTaskWaitInterval);
    gPool.AddTask(TStampTask.Create);
    AssertEquals('Threads created on the first task', 4, Length(gPool.Intervals));
    AssertTrue('Every thread waits DefaultThreadTaskWaitInterval, not 0', AllIntervalsAre(DefaultThreadTaskWaitInterval));
  finally
    FreePool;
  end;
end;


function TestListDefaultInterval: TTestString;

begin
  Result:='';
  AssertEquals('A thread list created on its own has the default interval',
    DefaultThreadTaskWaitInterval, TProbePool.NewListInterval);
end;


function TestSetInterval: TTestString;

begin
  Result:='';
  CreatePool;
  try
    gPool.ThreadTaskWaitInterval:=250;
    AssertEquals('ThreadTaskWaitInterval reads back', 250, gPool.ThreadTaskWaitInterval);
    gPool.AddTask(TStampTask.Create);
    AssertTrue('Every thread waits the interval set on the pool', AllIntervalsAre(250));
  finally
    FreePool;
  end;
end;


function TestRejectInvalidInterval: TTestString;

begin
  Result:='';
  CreatePool;
  try
    gPool.ThreadTaskWaitInterval:=300;
    AssertException('ThreadTaskWaitInterval 0 raises EThreadPool', EThreadPool, @SetIntervalZero);
    AssertException('Negative ThreadTaskWaitInterval raises EThreadPool', EThreadPool, @SetIntervalNegative);
    AssertEquals('A rejected value leaves the interval unchanged', 300, gPool.ThreadTaskWaitInterval);
  finally
    FreePool;
  end;
end;


function TestIdleThreadWakesForTask: TTestString;

var
  lStart: QWord;

begin
  Result:='';
  CreatePool;
  try
    gPool.AddTask(TStampTask.Create);
    Sleep(DefaultThreadTaskWaitInterval+500);
    gRanAt:=0;
    lStart:=GetTickCount64;
    gPool.AddTask(TStampTask.Create);
    while (gRanAt=0) and (GetTickCount64-lStart<5000) do
      Sleep(1);
    AssertTrue('A task added to an idle pool runs', gRanAt<>0);
    AssertTrue(Format('A task added to an idle pool runs without waiting for the interval (%d ms)',[gRanAt-lStart]),
      (gRanAt<>0) and (gRanAt-lStart<500));
    lStart:=GetTickCount64;
  finally
    FreePool;
  end;
  AssertTrue(Format('Freeing an idle pool does not wait for the interval (%d ms)',[GetTickCount64-lStart]),
    GetTickCount64-lStart<500);
end;


function TestFreeRightAfterAddTask: TTestString;

var
  lPool: TFPSimpleThreadPool;
  lWatchdog: TWatchdog;
  lStart: QWord;
  I: Integer;

begin
  Result:='';
  for I:=1 to 5 do
    begin
    lPool:=TFPSimpleThreadPool.Create;
    lPool.MinThreads:=4;
    lPool.MaxThreads:=4;
    lPool.AddTask(TStampTask.Create);
    lWatchdog:=TWatchdog.Create('Freeing a pool right after AddTask did not return within 10 seconds');
    lStart:=GetTickCount64;
    lPool.Free;
    lWatchdog.Stop;
    lWatchdog.Free;
    AssertTrue(Format('Freeing a pool right after AddTask returns at once (round %d, %d ms)',[I,GetTickCount64-lStart]),
      GetTickCount64-lStart<500);
    end;
end;


function TestTerminateRunningTasksWithSynchronize: TTestString;

var
  lPool: TFPSimpleThreadPool;
  lWatchdog: TWatchdog;
  lStart: QWord;
  I: Integer;

begin
  Result:='';
  gTasksFreed:=0;
  lPool:=TFPSimpleThreadPool.Create;
  try
    lPool.MinThreads:=4;
    lPool.MaxThreads:=4;
    for I:=1 to 3 do
      begin
      lPool.AddTask(TStampTask.Create);
      lWatchdog:=TWatchdog.Create('TerminateRunningTasks did not return within 10 seconds');
      lPool.TerminateRunningTasks;
      lWatchdog.Stop;
      lWatchdog.Free;
      lStart:=GetTickCount64;
      while (lPool.ThreadCount>0) and (GetTickCount64-lStart<5000) do
        CheckSynchronize(10);
      AssertEquals(Format('No live threads after TerminateRunningTasks (round %d)',[I]), 0, lPool.ThreadCount);
      AssertEquals(Format('The task is freed, run or not (round %d)',[I]), I, gTasksFreed);
      end;
    gRanAt:=0;
    lPool.AddTask(TStampTask.Create);
    lStart:=GetTickCount64;
    while (gRanAt=0) and (GetTickCount64-lStart<5000) do
      CheckSynchronize(10);
    AssertTrue('The pool runs tasks again after TerminateRunningTasks', gRanAt<>0);
    AssertEquals('New threads replace the terminated ones', 4, lPool.ThreadCount);
  finally
    lWatchdog:=TWatchdog.Create('Freeing the thread pool did not return within 10 seconds');
    lPool.Free;
    lWatchdog.Stop;
    lWatchdog.Free;
  end;
  AssertEquals('Every task is freed', 4, gTasksFreed);
end;


procedure RegisterTests;

var
  lSuite: PSuite;

begin
  lSuite:=AddSuite('TFPSimpleThreadPool');
  AddTest('DefaultInterval', @TestDefaultInterval, lSuite);
  AddTest('ListDefaultInterval', @TestListDefaultInterval, lSuite);
  AddTest('SetInterval', @TestSetInterval, lSuite);
  AddTest('RejectInvalidInterval', @TestRejectInvalidInterval, lSuite);
  AddTest('IdleThreadWakesForTask', @TestIdleThreadWakesForTask, lSuite);
  AddTest('FreeRightAfterAddTask', @TestFreeRightAfterAddTask, lSuite);
  AddTest('TerminateRunningTasksWithSynchronize', @TestTerminateRunningTasksWithSynchronize, lSuite);
end;

end.
