{ Tests of TConditionVariableCS and TConditionVariableMutex from SyncObjs }
unit utcConditionVariable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, punit;

// Registers the condition variable suites
procedure RegisterTests;

implementation

type
  TLockKind = (lkCriticalSection, lkRTLCriticalSection, lkMutex);

  { TWaiterThread }

  TWaiterThread = class(TThread)
  private
    FTimeout: Cardinal;
    FResult: TWaitResult;
  protected
    procedure Execute; override;
  public
    // Creates a started thread that enters the lock, counts itself as waiting and waits aTimeout milliseconds
    constructor Create(aTimeout: Cardinal);
    // Result of the wait
    property WaitResult: TWaitResult read FResult;
  end;

  { TTryLockThread }

  TTryLockThread = class(TThread)
  private
    FLocked: Boolean;
  protected
    procedure Execute; override;
  public
    // Creates a started thread that tries to take the lock without waiting
    constructor Create;
    // True when the lock could be taken
    property Locked: Boolean read FLocked;
  end;

  { TProducerThread }

  TProducerThread = class(TThread)
  protected
    procedure Execute; override;
  end;

  { TConsumerThread }

  TConsumerThread = class(TThread)
  protected
    procedure Execute; override;
  end;

const
  CItems = 2000;
  CBufferSize = 4;

var
  gKind: TLockKind;
  gCS: TCriticalSection;
  gRTLCS: TRTLCriticalSection;
  gCondCS: TConditionVariableCS;
{$IFNDEF WINCE}
  gMutex: TMutex;
  gCondMutex: TConditionVariableMutex;
{$ENDIF}
  gWaiting, gWoken: Integer;
  gBuffer: array[0..CBufferSize-1] of Integer;
  gCount, gHead, gProduced, gConsumed: Integer;
  gSum: Int64;
  gNotFull, gNotEmpty: TSynchroObject;


// Takes the lock of the current kind
procedure LockAcquire;

begin
  case gKind of
    lkCriticalSection: gCS.Enter;
    lkRTLCriticalSection: EnterCriticalSection(gRTLCS);
{$IFNDEF WINCE}
    lkMutex: gMutex.Acquire;
{$ENDIF}
  end;
end;


// Releases the lock of the current kind
procedure LockRelease;

begin
  case gKind of
    lkCriticalSection: gCS.Leave;
    lkRTLCriticalSection: LeaveCriticalSection(gRTLCS);
{$IFNDEF WINCE}
    lkMutex: gMutex.Release;
{$ENDIF}
  end;
end;


// Takes the lock of the current kind without waiting; True on success
function LockTry: Boolean;

begin
  case gKind of
    lkCriticalSection: Result:=gCS.TryEnter;
    lkRTLCriticalSection: Result:=TryEnterCriticalSection(gRTLCS)<>0;
{$IFNDEF WINCE}
    lkMutex: Result:=gMutex.WaitFor(0)=wrSignaled;
{$ENDIF}
  else
    Result:=False;
  end;
end;


// Waits on aCondition with the lock of the current kind
function CondWait(aCondition: TSynchroObject; aTimeout: Cardinal): TWaitResult;

begin
  case gKind of
    lkCriticalSection: Result:=TConditionVariableCS(aCondition).WaitFor(gCS,aTimeout);
    lkRTLCriticalSection: Result:=TConditionVariableCS(aCondition).WaitFor(gRTLCS,aTimeout);
{$IFNDEF WINCE}
    lkMutex: Result:=TConditionVariableMutex(aCondition).WaitFor(gMutex,aTimeout);
{$ENDIF}
  else
    Result:=wrError;
  end;
end;


// Wakes all waiters of aCondition
procedure CondReleaseAll(aCondition: TSynchroObject);

begin
  if aCondition is TConditionVariableCS then
    TConditionVariableCS(aCondition).ReleaseAll
{$IFNDEF WINCE}
  else
    TConditionVariableMutex(aCondition).ReleaseAll
{$ENDIF};
end;


// Returns a new condition variable for the current kind
function NewCondition: TSynchroObject;

begin
{$IFNDEF WINCE}
  if gKind=lkMutex then
    Exit(TConditionVariableMutex.Create);
{$ENDIF}
  Result:=TConditionVariableCS.Create;
end;


// Returns the condition variable used by the waiter threads for the current kind
function Condition: TSynchroObject;

begin
{$IFNDEF WINCE}
  if gKind=lkMutex then
    Exit(gCondMutex);
{$ENDIF}
  Result:=gCondCS;
end;


// Returns gWaiting read under the lock
function WaitingCount: Integer;

begin
  LockAcquire;
  Result:=gWaiting;
  LockRelease;
end;


// Waits at most 5 seconds until aCount threads are waiting; True when they are
function WaitForWaiters(aCount: Integer): Boolean;

var
  lStart: QWord;

begin
  lStart:=GetTickCount64;
  while WaitingCount<aCount do
    begin
    if GetTickCount64-lStart>5000 then
      Exit(False);
    Sleep(1);
    end;
  Result:=True;
end;


constructor TWaiterThread.Create(aTimeout: Cardinal);

begin
  FTimeout:=aTimeout;
  FResult:=wrError;
  inherited Create(False);
end;


procedure TWaiterThread.Execute;

begin
  LockAcquire;
  Inc(gWaiting);
  FResult:=CondWait(Condition,FTimeout);
  Inc(gWoken);
  LockRelease;
end;


constructor TTryLockThread.Create;

begin
  FLocked:=False;
  inherited Create(False);
end;


procedure TTryLockThread.Execute;

begin
  FLocked:=LockTry;
  if FLocked then
    LockRelease;
end;


procedure TProducerThread.Execute;

var
  lItem: Integer;

begin
  repeat
    LockAcquire;
    while (gCount=CBufferSize) and (gProduced<CItems) do
      CondWait(gNotFull,INFINITE);
    if gProduced>=CItems then
      begin
      LockRelease;
      Break;
      end;
    Inc(gProduced);
    lItem:=gProduced;
    gBuffer[(gHead+gCount) mod CBufferSize]:=lItem;
    Inc(gCount);
    gNotEmpty.Release;
    LockRelease;
  until False;
  LockAcquire;
  CondReleaseAll(gNotEmpty);
  LockRelease;
end;


procedure TConsumerThread.Execute;

begin
  repeat
    LockAcquire;
    while (gCount=0) and (gConsumed<CItems) do
      CondWait(gNotEmpty,INFINITE);
    if (gCount=0) and (gConsumed>=CItems) then
      begin
      LockRelease;
      Break;
      end;
    Inc(gSum,gBuffer[gHead]);
    gHead:=(gHead+1) mod CBufferSize;
    Dec(gCount);
    Inc(gConsumed);
    gNotFull.Release;
    if gConsumed>=CItems then
      CondReleaseAll(gNotEmpty);
    LockRelease;
  until False;
end;


// Creates the locks and the condition variables for the threads
procedure Setup;

begin
  gCS:=TCriticalSection.Create;
  InitCriticalSection(gRTLCS);
  gCondCS:=TConditionVariableCS.Create;
{$IFNDEF WINCE}
  gMutex:=TMutex.Create;
  gCondMutex:=TConditionVariableMutex.Create;
{$ENDIF}
  gWaiting:=0;
  gWoken:=0;
end;


// Frees what Setup created
procedure TearDown;

begin
{$IFNDEF WINCE}
  FreeAndNil(gCondMutex);
  FreeAndNil(gMutex);
{$ENDIF}
  FreeAndNil(gCondCS);
  DoneCriticalSection(gRTLCS);
  FreeAndNil(gCS);
end;


// Returns the name of the current kind for assertion messages
function KindName: string;

const
  CNames: array[TLockKind] of string = ('TCriticalSection', 'TRTLCriticalSection', 'TMutex');

begin
  Result:=CNames[gKind];
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


// Calls Acquire on gCondCS
procedure AcquireCS;

begin
  gCondCS.Acquire;
end;


{$IFNDEF WINCE}
// Calls Acquire on gCondMutex
procedure AcquireMutex;

begin
  gCondMutex.Acquire;
end;
{$ENDIF}


// Checks a timed out wait for the current kind
procedure CheckTimeout;

var
  lStart: QWord;
  lTry: TTryLockThread;

begin
  LockAcquire;
  try
    lStart:=GetTickCount64;
    AssertTrue(KindName+': WaitFor(100) without Release times out', CondWait(Condition,100)=wrTimeout);
    AssertTrue(KindName+': WaitFor(100) waits for the timeout', GetTickCount64-lStart>=80);
    lTry:=TTryLockThread.Create;
    lTry.WaitFor;
    AssertFalse(KindName+': the lock is held again after a timed out WaitFor', lTry.Locked);
    lTry.Free;
    AssertTrue(KindName+': WaitFor(0) times out at once', CondWait(Condition,0)=wrTimeout);
  finally
    LockRelease;
  end;
end;


// Checks that Release and ReleaseAll without waiters are not remembered
procedure CheckNoMemory;

begin
  if Condition is TConditionVariableCS then
    begin
    gCondCS.Release;
    gCondCS.ReleaseAll;
    end
{$IFNDEF WINCE}
  else
    begin
    gCondMutex.Release;
    gCondMutex.ReleaseAll;
    end
{$ENDIF};
  LockAcquire;
  try
    AssertTrue(KindName+': a Release before WaitFor does not wake a later wait', CondWait(Condition,50)=wrTimeout);
  finally
    LockRelease;
  end;
end;


// Checks that Release wakes one waiter and ReleaseAll the others
procedure CheckReleaseOne;

const
  CWaiters = 3;

var
  lThreads: array[0..CWaiters-1] of TWaiterThread;
  lWoken: Integer;
  i: Integer;

begin
  for i:=0 to CWaiters-1 do
    lThreads[i]:=TWaiterThread.Create(10000);
  AssertTrue(KindName+': all waiters are waiting', WaitForWaiters(CWaiters));
  LockAcquire;
  Condition.Release;
  LockRelease;
  Sleep(200);
  LockAcquire;
  lWoken:=gWoken;
  LockRelease;
  AssertEquals(KindName+': Release wakes one waiter', 1, lWoken);
  LockAcquire;
  CondReleaseAll(Condition);
  LockRelease;
  for i:=0 to CWaiters-1 do
    begin
    lThreads[i].WaitFor;
    AssertTrue(KindName+': waiter '+IntToStr(i)+' is signalled', lThreads[i].WaitResult=wrSignaled);
    lThreads[i].Free;
    end;
  AssertEquals(KindName+': all waiters are woken', CWaiters, gWoken);
end;


// Checks that ReleaseAll wakes every waiter
procedure CheckReleaseAll;

const
  CWaiters = 4;

var
  lThreads: array[0..CWaiters-1] of TWaiterThread;
  lStart: QWord;
  i: Integer;

begin
  for i:=0 to CWaiters-1 do
    lThreads[i]:=TWaiterThread.Create(10000);
  AssertTrue(KindName+': all waiters are waiting', WaitForWaiters(CWaiters));
  lStart:=GetTickCount64;
  LockAcquire;
  CondReleaseAll(Condition);
  LockRelease;
  for i:=0 to CWaiters-1 do
    begin
    lThreads[i].WaitFor;
    AssertTrue(KindName+': waiter '+IntToStr(i)+' is signalled by ReleaseAll', lThreads[i].WaitResult=wrSignaled);
    lThreads[i].Free;
    end;
  AssertTrue(KindName+': ReleaseAll wakes the waiters before their timeout', GetTickCount64-lStart<5000);
end;


// Checks a bounded buffer with two producers and two consumers
procedure CheckProducerConsumer;

var
  lThreads: array[0..3] of TThread;
  i: Integer;

begin
  gCount:=0;
  gHead:=0;
  gProduced:=0;
  gConsumed:=0;
  gSum:=0;
  gNotFull:=NewCondition;
  gNotEmpty:=NewCondition;
  try
    lThreads[0]:=TProducerThread.Create(False);
    lThreads[1]:=TConsumerThread.Create(False);
    lThreads[2]:=TProducerThread.Create(False);
    lThreads[3]:=TConsumerThread.Create(False);
    for i:=0 to 3 do
      begin
      lThreads[i].WaitFor;
      lThreads[i].Free;
      end;
    AssertEquals(KindName+': every item is consumed', CItems, gConsumed);
    AssertEquals(KindName+': the consumed items add up', Int64(CItems)*(CItems+1) div 2, gSum);
  finally
    FreeAndNil(gNotFull);
    FreeAndNil(gNotEmpty);
  end;
end;


// Runs aCheck for aKind with fresh locks
procedure RunForKind(aKind: TLockKind; aCheck: TProcedure);

begin
  gKind:=aKind;
  Setup;
  try
    aCheck();
  finally
    TearDown;
  end;
end;


// Runs aCheck for the kinds of TConditionVariableCS
procedure RunForCS(aCheck: TProcedure);

begin
  RunForKind(lkCriticalSection,aCheck);
  RunForKind(lkRTLCriticalSection,aCheck);
end;


function TestCSAcquire: TTestString;

begin
  Result:='';
  gCondCS:=TConditionVariableCS.Create;
  try
    AssertTrue('TConditionVariableCS.Acquire raises ESyncObjectException', RaisesSyncError(@AcquireCS));
  finally
    FreeAndNil(gCondCS);
  end;
end;


function TestCSTimeout: TTestString;

begin
  Result:='';
  RunForCS(@CheckTimeout);
end;


function TestCSNoMemory: TTestString;

begin
  Result:='';
  RunForCS(@CheckNoMemory);
end;


function TestCSReleaseOne: TTestString;

begin
  Result:='';
  RunForCS(@CheckReleaseOne);
end;


function TestCSReleaseAll: TTestString;

begin
  Result:='';
  RunForCS(@CheckReleaseAll);
end;


function TestCSProducerConsumer: TTestString;

begin
  Result:='';
  RunForCS(@CheckProducerConsumer);
end;


{$IFNDEF WINCE}
function TestMutexAcquire: TTestString;

begin
  Result:='';
  gCondMutex:=TConditionVariableMutex.Create;
  try
    AssertTrue('TConditionVariableMutex.Acquire raises ESyncObjectException', RaisesSyncError(@AcquireMutex));
  finally
    FreeAndNil(gCondMutex);
  end;
end;


function TestMutexTimeout: TTestString;

begin
  Result:='';
  RunForKind(lkMutex,@CheckTimeout);
end;


function TestMutexNoMemory: TTestString;

begin
  Result:='';
  RunForKind(lkMutex,@CheckNoMemory);
end;


function TestMutexReleaseOne: TTestString;

begin
  Result:='';
  RunForKind(lkMutex,@CheckReleaseOne);
end;


function TestMutexReleaseAll: TTestString;

begin
  Result:='';
  RunForKind(lkMutex,@CheckReleaseAll);
end;


function TestMutexProducerConsumer: TTestString;

begin
  Result:='';
  RunForKind(lkMutex,@CheckProducerConsumer);
end;
{$ENDIF}


procedure RegisterTests;

var
  lSuite: PSuite;

begin
  lSuite:=AddSuite('TConditionVariableCS');
  AddTest('Acquire', @TestCSAcquire, lSuite);
  AddTest('Timeout', @TestCSTimeout, lSuite);
  AddTest('NoMemory', @TestCSNoMemory, lSuite);
  AddTest('ReleaseOne', @TestCSReleaseOne, lSuite);
  AddTest('ReleaseAll', @TestCSReleaseAll, lSuite);
  AddTest('ProducerConsumer', @TestCSProducerConsumer, lSuite);
{$IFNDEF WINCE}
  lSuite:=AddSuite('TConditionVariableMutex');
  AddTest('Acquire', @TestMutexAcquire, lSuite);
  AddTest('Timeout', @TestMutexTimeout, lSuite);
  AddTest('NoMemory', @TestMutexNoMemory, lSuite);
  AddTest('ReleaseOne', @TestMutexReleaseOne, lSuite);
  AddTest('ReleaseAll', @TestMutexReleaseAll, lSuite);
  AddTest('ProducerConsumer', @TestMutexProducerConsumer, lSuite);
{$ENDIF}
end;


end.
