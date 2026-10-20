{ Tests of TCountdownEvent from SyncObjs }
unit utcCountdownEvent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {$IFNDEF VER3_2}System.TimeSpan, {$ENDIF}SyncObjs, punit;

// Registers the TCountdownEvent suite
procedure RegisterTests;

implementation

type
  { TSignalThread }

  TSignalThread = class(TThread)
  private
    FEvent: TCountdownEvent;
    FSignals: Integer;
    FDelay: Integer;
    FReachedZero: PInteger;
    FWorkDone: PInteger;
  protected
    procedure Execute; override;
  public
    // Creates a started thread that signals aEvent aSignals times after aDelay milliseconds
    constructor Create(aEvent: TCountdownEvent; aSignals, aDelay: Integer; aReachedZero, aWorkDone: PInteger);
  end;

  { TWaitThread }

  TWaitThread = class(TThread)
  private
    FEvent: TCountdownEvent;
    FResult: TWaitResult;
  protected
    procedure Execute; override;
  public
    // Creates a started thread that waits for aEvent
    constructor Create(aEvent: TCountdownEvent);
    // Result of the wait
    property WaitResult: TWaitResult read FResult;
  end;


constructor TSignalThread.Create(aEvent: TCountdownEvent; aSignals, aDelay: Integer; aReachedZero, aWorkDone: PInteger);

begin
  FEvent:=aEvent;
  FSignals:=aSignals;
  FDelay:=aDelay;
  FReachedZero:=aReachedZero;
  FWorkDone:=aWorkDone;
  inherited Create(False);
end;


procedure TSignalThread.Execute;

var
  i: Integer;

begin
  if FDelay>0 then
    Sleep(FDelay);
  for i:=1 to FSignals do
    begin
    InterlockedIncrement(FWorkDone^);
    if FEvent.Signal then
      InterlockedIncrement(FReachedZero^);
    end;
end;


constructor TWaitThread.Create(aEvent: TCountdownEvent);

begin
  FEvent:=aEvent;
  FResult:=wrError;
  inherited Create(False);
end;


procedure TWaitThread.Execute;

begin
  FResult:=FEvent.WaitFor(10000);
end;


{ Returns True when aRun raises ESyncObjectException }
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


var
  gEvent: TCountdownEvent;


{ Creates an event with a negative count }
procedure CreateNegative;

begin
  TCountdownEvent.Create(-1).Free;
end;


{ Signals gEvent once }
procedure SignalOnce;

begin
  gEvent.Signal;
end;


{ Signals gEvent with a count of 0 }
procedure SignalZero;

begin
  gEvent.Signal(0);
end;


{ Signals gEvent 5 times at once }
procedure SignalFive;

begin
  gEvent.Signal(5);
end;


{ Adds 1 to gEvent }
procedure AddOne;

begin
  gEvent.AddCount;
end;


{ Adds 0 to gEvent }
procedure AddZero;

begin
  gEvent.TryAddCount(0);
end;


{ Adds High(Integer) to gEvent }
procedure AddMax;

begin
  gEvent.AddCount(High(Integer));
end;


{ Resets gEvent to a negative count }
procedure ResetNegative;

begin
  gEvent.Reset(-1);
end;


function TestCreate: TTestString;

var
  lEvent: TCountdownEvent;

begin
  Result:='';
  lEvent:=TCountdownEvent.Create(3);
  try
    AssertEquals('CurrentCount after Create(3)', 3, lEvent.CurrentCount);
    AssertEquals('InitialCount after Create(3)', 3, lEvent.InitialCount);
    AssertFalse('Event with a count of 3 is not set', lEvent.IsSet);
    AssertTrue('WaitFor(0) times out while the count is not 0', lEvent.WaitFor(0)=wrTimeout);
  finally
    lEvent.Free;
  end;
  lEvent:=TCountdownEvent.Create(0);
  try
    AssertTrue('Event created with a count of 0 is set', lEvent.IsSet);
    AssertTrue('WaitFor(0) succeeds on an event created with a count of 0', lEvent.WaitFor(0)=wrSignaled);
  finally
    lEvent.Free;
  end;
  AssertTrue('Create(-1) raises ESyncObjectException', RaisesSyncError(@CreateNegative));
end;


function TestSignal: TTestString;

begin
  Result:='';
  gEvent:=TCountdownEvent.Create(3);
  try
    AssertFalse('First Signal does not reach 0', gEvent.Signal);
    AssertEquals('CurrentCount after one Signal', 2, gEvent.CurrentCount);
    AssertTrue('Signal(0) raises ESyncObjectException', RaisesSyncError(@SignalZero));
    AssertTrue('Signal(5) with a count of 2 raises ESyncObjectException', RaisesSyncError(@SignalFive));
    AssertEquals('Failed Signal calls leave the count unchanged', 2, gEvent.CurrentCount);
    AssertTrue('Signal(2) reaches 0', gEvent.Signal(2));
    AssertTrue('Event is set at 0', gEvent.IsSet);
    AssertTrue('WaitFor(0) succeeds at 0', gEvent.WaitFor(0)=wrSignaled);
    AssertTrue('Signal on a set event raises ESyncObjectException', RaisesSyncError(@SignalOnce));
    AssertEquals('Count stays 0 after a failed Signal', 0, gEvent.CurrentCount);
  finally
    FreeAndNil(gEvent);
  end;
end;


function TestAddCount: TTestString;

begin
  Result:='';
  gEvent:=TCountdownEvent.Create(1);
  try
    gEvent.AddCount(2);
    AssertEquals('AddCount(2) raises the count to 3', 3, gEvent.CurrentCount);
    AssertTrue('TryAddCount succeeds while the event is not set', gEvent.TryAddCount);
    AssertEquals('TryAddCount raises the count to 4', 4, gEvent.CurrentCount);
    AssertEquals('AddCount leaves InitialCount unchanged', 1, gEvent.InitialCount);
    AssertTrue('TryAddCount(0) raises ESyncObjectException', RaisesSyncError(@AddZero));
    AssertTrue('AddCount past High(Integer) raises ESyncObjectException', RaisesSyncError(@AddMax));
    AssertEquals('Failed additions leave the count unchanged', 4, gEvent.CurrentCount);
    AssertTrue('Signal(4) reaches 0', gEvent.Signal(4));
    AssertFalse('TryAddCount fails once the event is set', gEvent.TryAddCount);
    AssertTrue('AddCount on a set event raises ESyncObjectException', RaisesSyncError(@AddOne));
    AssertTrue('Event stays set after failed additions', gEvent.IsSet);
  finally
    FreeAndNil(gEvent);
  end;
end;


function TestReset: TTestString;

begin
  Result:='';
  gEvent:=TCountdownEvent.Create(2);
  try
    gEvent.Signal(2);
    gEvent.Reset;
    AssertEquals('Reset restores InitialCount', 2, gEvent.CurrentCount);
    AssertFalse('Reset to a positive count clears the event', gEvent.IsSet);
    AssertTrue('WaitFor(0) times out after Reset', gEvent.WaitFor(0)=wrTimeout);
    gEvent.Reset(5);
    AssertEquals('Reset(5) sets CurrentCount', 5, gEvent.CurrentCount);
    AssertEquals('Reset(5) sets InitialCount', 5, gEvent.InitialCount);
    gEvent.Reset(0);
    AssertTrue('Reset(0) sets the event', gEvent.IsSet);
    AssertTrue('WaitFor(0) succeeds after Reset(0)', gEvent.WaitFor(0)=wrSignaled);
    AssertTrue('Reset(-1) raises ESyncObjectException', RaisesSyncError(@ResetNegative));
  finally
    FreeAndNil(gEvent);
  end;
end;


function TestWaitTimeout: TTestString;

var
  lEvent: TCountdownEvent;
  lStart: QWord;

begin
  Result:='';
  lEvent:=TCountdownEvent.Create(1);
  try
    lStart:=GetTickCount64;
    AssertTrue('WaitFor(100) times out while the count is not 0', lEvent.WaitFor(100)=wrTimeout);
    AssertTrue('WaitFor(100) waits for the timeout', GetTickCount64-lStart>=80);
{$IFNDEF VER3_2}
    AssertTrue('WaitFor with a TTimeSpan times out while the count is not 0', lEvent.WaitFor(TTimeSpan.FromMilliseconds(10))=wrTimeout);
    lEvent.Signal;
    AssertTrue('WaitFor with a TTimeSpan succeeds at 0', lEvent.WaitFor(TTimeSpan.FromMilliseconds(10))=wrSignaled);
{$ENDIF}
  finally
    lEvent.Free;
  end;
end;


function TestWorkers: TTestString;

const
  CWorkers = 8;

var
  lEvent: TCountdownEvent;
  lThreads: array[0..CWorkers-1] of TSignalThread;
  lReachedZero, lWorkDone: Integer;
  i: Integer;

begin
  Result:='';
  lReachedZero:=0;
  lWorkDone:=0;
  lEvent:=TCountdownEvent.Create(CWorkers);
  try
    for i:=0 to CWorkers-1 do
      lThreads[i]:=TSignalThread.Create(lEvent, 1, 10+5*i, @lReachedZero, @lWorkDone);
    AssertTrue('WaitFor returns once every worker has signalled', lEvent.WaitFor(10000)=wrSignaled);
    AssertEquals('All workers finished their work before the wait returned', CWorkers, lWorkDone);
    for i:=0 to CWorkers-1 do
      begin
      lThreads[i].WaitFor;
      lThreads[i].Free;
      end;
    AssertEquals('Exactly one Signal reported reaching 0', 1, lReachedZero);
  finally
    lEvent.Free;
  end;
end;


function TestConcurrentSignals: TTestString;

const
  CThreads = 4;
  CSignals = 2500;

var
  lEvent: TCountdownEvent;
  lThreads: array[0..CThreads-1] of TSignalThread;
  lReachedZero, lWorkDone: Integer;
  i: Integer;

begin
  Result:='';
  lReachedZero:=0;
  lWorkDone:=0;
  lEvent:=TCountdownEvent.Create(CThreads*CSignals);
  try
    for i:=0 to CThreads-1 do
      lThreads[i]:=TSignalThread.Create(lEvent, CSignals, 0, @lReachedZero, @lWorkDone);
    for i:=0 to CThreads-1 do
      begin
      lThreads[i].WaitFor;
      lThreads[i].Free;
      end;
    AssertEquals('Concurrent signals bring the count to 0', 0, lEvent.CurrentCount);
    AssertEquals('Exactly one of the concurrent signals reported reaching 0', 1, lReachedZero);
    AssertTrue('Event is set after the concurrent signals', lEvent.WaitFor(0)=wrSignaled);
  finally
    lEvent.Free;
  end;
end;


function TestWaiters: TTestString;

const
  CWaiters = 4;

var
  lEvent: TCountdownEvent;
  lThreads: array[0..CWaiters-1] of TWaitThread;
  i: Integer;

begin
  Result:='';
  lEvent:=TCountdownEvent.Create(2);
  try
    for i:=0 to CWaiters-1 do
      lThreads[i]:=TWaitThread.Create(lEvent);
    Sleep(50);
    lEvent.Signal;
    Sleep(50);
    for i:=0 to CWaiters-1 do
      AssertFalse('Waiter '+IntToStr(i)+' still waits while the count is 1', lThreads[i].Finished);
    lEvent.Signal;
    for i:=0 to CWaiters-1 do
      begin
      lThreads[i].WaitFor;
      AssertTrue('Waiter '+IntToStr(i)+' is released when the count reaches 0', lThreads[i].WaitResult=wrSignaled);
      lThreads[i].Free;
      end;
  finally
    lEvent.Free;
  end;
end;


procedure RegisterTests;

var
  lSuite: PSuite;

begin
  lSuite:=AddSuite('TCountdownEvent');
  AddTest('Create', @TestCreate, lSuite);
  AddTest('Signal', @TestSignal, lSuite);
  AddTest('AddCount', @TestAddCount, lSuite);
  AddTest('Reset', @TestReset, lSuite);
  AddTest('WaitTimeout', @TestWaitTimeout, lSuite);
  AddTest('Workers', @TestWorkers, lSuite);
  AddTest('ConcurrentSignals', @TestConcurrentSignals, lSuite);
  AddTest('Waiters', @TestWaiters, lSuite);
end;


end.
