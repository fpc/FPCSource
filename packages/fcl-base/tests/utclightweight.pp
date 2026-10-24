{ Tests of TLightweightEvent and TLightweightSemaphore from SyncObjs }
unit utcLightweight;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, punit;

// Registers the lightweight event and semaphore suites
procedure RegisterTests;

implementation

type
  { TWaitThread }

  TWaitThread = class(TThread)
  private
    FObject: TSynchroObject;
    FTimeout: Cardinal;
    FResult: TWaitResult;
    FDone: LongInt;
  protected
    procedure Execute; override;
  public
    // Creates a started thread that calls WaitFor(aTimeout) on aObject
    constructor Create(aObject: TSynchroObject; aTimeout: Cardinal);
    // Result of the wait
    property WaitResult: TWaitResult read FResult;
    // 1 once the wait has returned
    property Done: LongInt read FDone;
  end;

  { TWorkerThread }

  TWorkerThread = class(TThread)
  protected
    procedure Execute; override;
  end;

  TCountFunc = function: LongInt;

const
  CWorkerRounds = 200;

var
  gEvent: TLightweightEvent;
  gSemaphore: TLightweightSemaphore;
  gActive, gMaxActive, gRounds: LongInt;


constructor TWaitThread.Create(aObject: TSynchroObject; aTimeout: Cardinal);

begin
  FObject:=aObject;
  FTimeout:=aTimeout;
  FResult:=wrError;
  FDone:=0;
  inherited Create(False);
end;


procedure TWaitThread.Execute;

begin
  FResult:=FObject.WaitFor(FTimeout);
  InterlockedExchange(FDone,1);
end;


procedure TWorkerThread.Execute;

var
  lActive, lMax, i: LongInt;

begin
  for i:=1 to CWorkerRounds do
    begin
    gSemaphore.Acquire;
    lActive:=InterlockedIncrement(gActive);
    repeat
      lMax:=gMaxActive;
    until (lActive<=lMax) or (InterlockedCompareExchange(gMaxActive,lActive,lMax)=lMax);
    if i mod 16=0 then
      Sleep(1);
    InterlockedIncrement(gRounds);
    InterlockedDecrement(gActive);
    gSemaphore.Release;
    end;
end;


// Returns True when aRun raises ESyncObjectException
function RaisesSyncError(aRun: TProcedure): Boolean;

begin
  Result:=False;
  try
    aRun();
  except
    on ESyncObjectException do
      Result:=True;
  end;
end;


// Waits at most 5 seconds until aCount() returns aExpected; True when it does
function WaitUntil(aCount: TCountFunc; aExpected: LongInt): Boolean;

var
  lStart: QWord;

begin
  lStart:=GetTickCount64;
  while aCount()<>aExpected do
    begin
    if GetTickCount64-lStart>5000 then
      Exit(False);
    Sleep(1);
    end;
  Result:=True;
end;


// Returns the number of threads blocked on gEvent
function EventBlocked: LongInt;

begin
  Result:=gEvent.BlockedCount;
end;


// Creates an event with a negative spin count
procedure CreateNegativeSpin;

begin
  TLightweightEvent.Create(False,-1).Free;
end;


// Creates a semaphore with an initial count above the maximum
procedure CreateAboveMax;

begin
  TLightweightSemaphore.Create(3,2).Free;
end;


// Creates a semaphore with a negative initial count
procedure CreateNegativeCount;

begin
  TLightweightSemaphore.Create(-1).Free;
end;


// Creates a semaphore with a maximum count of 0
procedure CreateZeroMax;

begin
  TLightweightSemaphore.Create(0,0).Free;
end;


// Releases gSemaphore past its maximum
procedure ReleasePastMax;

begin
  gSemaphore.Release(2);
end;


// Releases gSemaphore with a count of 0
procedure ReleaseZero;

begin
  gSemaphore.Release(0);
end;


function TestEventCreate: TTestString;

var
  lEvent: TLightweightEvent;

begin
  Result:='';
  lEvent:=TLightweightEvent.Create;
  try
    AssertFalse('Create gives a reset event', lEvent.IsSet);
    AssertTrue('Create gives a positive spin count', lEvent.SpinCount>=1);
    AssertEquals('No thread is blocked after Create', 0, lEvent.BlockedCount);
  finally
    lEvent.Free;
  end;
  lEvent:=TLightweightEvent.Create(True);
  try
    AssertTrue('Create(True) gives a set event', lEvent.IsSet);
  finally
    lEvent.Free;
  end;
  lEvent:=TLightweightEvent.Create(False,0);
  try
    AssertEquals('Create(False,0) has a spin count of 0', 0, lEvent.SpinCount);
  finally
    lEvent.Free;
  end;
  AssertTrue('A negative spin count raises ESyncObjectException', RaisesSyncError(@CreateNegativeSpin));
end;


function TestEventSetReset: TTestString;

var
  lEvent: TLightweightEvent;

begin
  Result:='';
  lEvent:=TLightweightEvent.Create;
  try
    AssertTrue('WaitFor(0) on a reset event times out', lEvent.WaitFor(0)=wrTimeout);
    lEvent.SetEvent;
    AssertTrue('SetEvent sets the event', lEvent.IsSet);
    AssertTrue('WaitFor(0) on a set event is signalled', lEvent.WaitFor(0)=wrSignaled);
    AssertTrue('The event stays set after a successful wait', lEvent.WaitFor(0)=wrSignaled);
    lEvent.Acquire;
    AssertTrue('Acquire on a set event returns and leaves it set', lEvent.IsSet);
    lEvent.ResetEvent;
    AssertFalse('ResetEvent clears the event', lEvent.IsSet);
    AssertTrue('WaitFor(0) after ResetEvent times out', lEvent.WaitFor(0)=wrTimeout);
  finally
    lEvent.Free;
  end;
end;


function TestEventTimeout: TTestString;

var
  lEvent: TLightweightEvent;
  lStart: QWord;

begin
  Result:='';
  lEvent:=TLightweightEvent.Create;
  try
    lStart:=GetTickCount64;
    AssertTrue('WaitFor(100) on a reset event times out', lEvent.WaitFor(100)=wrTimeout);
    AssertTrue('WaitFor(100) waits for the timeout', GetTickCount64-lStart>=80);
    AssertEquals('No thread is blocked after the timeout', 0, lEvent.BlockedCount);
  finally
    lEvent.Free;
  end;
end;


// Checks that SetEvent wakes CWaiters threads blocked on an event with aSpinCount
procedure CheckEventWakeup(aSpinCount: Integer);

const
  CWaiters = 4;

var
  lThreads: array[0..CWaiters-1] of TWaitThread;
  lStart: QWord;
  i: Integer;

begin
  gEvent:=TLightweightEvent.Create(False,aSpinCount);
  try
    for i:=0 to CWaiters-1 do
      lThreads[i]:=TWaitThread.Create(gEvent,10000);
    AssertTrue('Spin count '+IntToStr(aSpinCount)+': all waiters block', WaitUntil(@EventBlocked,CWaiters));
    lStart:=GetTickCount64;
    gEvent.SetEvent;
    AssertTrue('Spin count '+IntToStr(aSpinCount)+': SetEvent wakes the blocked waiters', WaitUntil(@EventBlocked,0));
    AssertTrue('Spin count '+IntToStr(aSpinCount)+': the waiters wake well before their timeout', GetTickCount64-lStart<2000);
    for i:=0 to CWaiters-1 do
      begin
      lThreads[i].WaitFor;
      AssertTrue('Spin count '+IntToStr(aSpinCount)+': waiter '+IntToStr(i)+' is signalled', lThreads[i].WaitResult=wrSignaled);
      lThreads[i].Free;
      end;
    AssertEquals('Spin count '+IntToStr(aSpinCount)+': no thread is blocked after SetEvent', 0, gEvent.BlockedCount);
  finally
    FreeAndNil(gEvent);
  end;
end;


function TestEventWakeup: TTestString;

begin
  Result:='';
  CheckEventWakeup(0);
  CheckEventWakeup(10);
end;


function TestSemaphoreCreate: TTestString;

var
  lSemaphore: TLightweightSemaphore;

begin
  Result:='';
  lSemaphore:=TLightweightSemaphore.Create(2,3);
  try
    AssertEquals('Create(2,3) has a count of 2', 2, lSemaphore.CurrentCount);
  finally
    lSemaphore.Free;
  end;
  AssertTrue('An initial count above the maximum raises ESyncObjectException', RaisesSyncError(@CreateAboveMax));
  AssertTrue('A negative initial count raises ESyncObjectException', RaisesSyncError(@CreateNegativeCount));
  AssertTrue('A maximum count of 0 raises ESyncObjectException', RaisesSyncError(@CreateZeroMax));
end;


function TestSemaphoreCount: TTestString;

begin
  Result:='';
  gSemaphore:=TLightweightSemaphore.Create(2,3);
  try
    AssertTrue('First WaitFor(0) takes a slot', gSemaphore.WaitFor(0)=wrSignaled);
    AssertTrue('Second WaitFor(0) takes a slot', gSemaphore.WaitFor(0)=wrSignaled);
    AssertEquals('Both slots are taken', 0, gSemaphore.CurrentCount);
    AssertTrue('WaitFor(0) without a free slot times out', gSemaphore.WaitFor(0)=wrTimeout);
    gSemaphore.Release;
    AssertEquals('Release returns one slot', 1, gSemaphore.CurrentCount);
    AssertEquals('Release(2) returns the count before the release', 1, gSemaphore.Release(2));
    AssertEquals('Release(2) returns two slots', 3, gSemaphore.CurrentCount);
    AssertTrue('Releasing past the maximum raises ESyncObjectException', RaisesSyncError(@ReleasePastMax));
    AssertTrue('Release(0) raises ESyncObjectException', RaisesSyncError(@ReleaseZero));
    AssertEquals('Failed releases leave the count unchanged', 3, gSemaphore.CurrentCount);
    gSemaphore.Acquire;
    AssertEquals('Acquire takes a slot', 2, gSemaphore.CurrentCount);
  finally
    FreeAndNil(gSemaphore);
  end;
end;


function TestSemaphoreTimeout: TTestString;

var
  lSemaphore: TLightweightSemaphore;
  lStart: QWord;

begin
  Result:='';
  lSemaphore:=TLightweightSemaphore.Create(0);
  try
    lStart:=GetTickCount64;
    AssertTrue('WaitFor(100) without a free slot times out', lSemaphore.WaitFor(100)=wrTimeout);
    AssertTrue('WaitFor(100) waits for the timeout', GetTickCount64-lStart>=80);
    AssertEquals('The count stays 0 after the timeout', 0, lSemaphore.CurrentCount);
  finally
    lSemaphore.Free;
  end;
end;


function TestSemaphoreWakeup: TTestString;

const
  CWaiters = 4;

var
  lThreads: array[0..CWaiters-1] of TWaitThread;
  lDone, i: Integer;

begin
  Result:='';
  gSemaphore:=TLightweightSemaphore.Create(0);
  try
    for i:=0 to CWaiters-1 do
      lThreads[i]:=TWaitThread.Create(gSemaphore,10000);
    Sleep(200);
    AssertEquals('The waiters wait for a free slot', 0, gSemaphore.CurrentCount);
    gSemaphore.Release(2);
    Sleep(300);
    lDone:=0;
    for i:=0 to CWaiters-1 do
      Inc(lDone,lThreads[i].Done);
    AssertEquals('Release(2) lets two waiters through', 2, lDone);
    gSemaphore.Release(2);
    for i:=0 to CWaiters-1 do
      begin
      lThreads[i].WaitFor;
      AssertTrue('Waiter '+IntToStr(i)+' gets a slot', lThreads[i].WaitResult=wrSignaled);
      lThreads[i].Free;
      end;
    AssertEquals('All slots are taken by the waiters', 0, gSemaphore.CurrentCount);
  finally
    FreeAndNil(gSemaphore);
  end;
end;


function TestSemaphoreLimit: TTestString;

const
  CWorkers = 6;
  CSlots = 2;

var
  lThreads: array[0..CWorkers-1] of TWorkerThread;
  i: Integer;

begin
  Result:='';
  gActive:=0;
  gMaxActive:=0;
  gRounds:=0;
  gSemaphore:=TLightweightSemaphore.Create(CSlots);
  try
    for i:=0 to CWorkers-1 do
      lThreads[i]:=TWorkerThread.Create(False);
    for i:=0 to CWorkers-1 do
      begin
      lThreads[i].WaitFor;
      lThreads[i].Free;
      end;
    AssertEquals('Every worker completes its rounds', CWorkers*CWorkerRounds, gRounds);
    AssertTrue('At most '+IntToStr(CSlots)+' workers hold a slot at once, got '+IntToStr(gMaxActive), gMaxActive<=CSlots);
    AssertEquals('All slots are free afterwards', CSlots, gSemaphore.CurrentCount);
  finally
    FreeAndNil(gSemaphore);
  end;
end;


procedure RegisterTests;

var
  lSuite: PSuite;

begin
  lSuite:=AddSuite('TLightweightEvent');
  AddTest('Create', @TestEventCreate, lSuite);
  AddTest('SetReset', @TestEventSetReset, lSuite);
  AddTest('Timeout', @TestEventTimeout, lSuite);
  AddTest('Wakeup', @TestEventWakeup, lSuite);
  lSuite:=AddSuite('TLightweightSemaphore');
  AddTest('Create', @TestSemaphoreCreate, lSuite);
  AddTest('Count', @TestSemaphoreCount, lSuite);
  AddTest('Timeout', @TestSemaphoreTimeout, lSuite);
  AddTest('Wakeup', @TestSemaphoreWakeup, lSuite);
  AddTest('Limit', @TestSemaphoreLimit, lSuite);
end;


end.
