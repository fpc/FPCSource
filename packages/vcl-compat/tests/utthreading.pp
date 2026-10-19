unit utthreading;

{$mode objfpc}{$H+}

{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

{ $DEFINE DEBUGTEST}

interface

uses
  Classes, SysUtils, SyncObjs, fpcunit, testutils, testregistry, generics.collections, system.threading, system.timespan;

type
  TNotifyProc = reference to procedure(Sender : TObject);
  ESomeThing = Class(Exception);

  { TMyTask }

  TMyTask = Class(TTask)
    Function GetException : TObject;
  end;

  { TLiveObject }

  TLiveObject = Class(TObject)
    OnDestroy : TNotifyProc;
    Constructor Create(aOnDestroy : TNotifyProc); overload;
    Destructor Destroy; override;
  end;

  { Ex1 }
  Ex1 = Class(Exception)
    OnDestroy : TNotifyProc;
    Id : Integer;
    Constructor Create(aID : Integer; aOnDestroy : TNotifyProc); overload;
    Destructor Destroy; override;
  end;
  Ex2 = Class(Ex1);
  Ex3 = Class(Ex2);

  { TTestTExceptionList }

  TTestTExceptionList = class(TTestCase)
  private
    FList: TExceptionList;
    FEx : Array[1..3] of exception;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHookUp;
    procedure TestCreate;
    procedure TestAdd;
    procedure TestGrow;
    procedure TestClear;
    procedure TestTruncate;
    procedure TestGrowCapacity;
    procedure TestFlatten;
    procedure TestFlatten2;
    procedure TestAddFromTaskNonEx;
    procedure TestAddFromTaskEx;
    procedure TestAddFromTaskAggEx;
  end;

  { TTestAggregateException }

  TTestAggregateException = Class(TTestCase)
  private
    class var HandleExCalledCount: Integer;   // Number of times HandleEx is called.
    class var HandleExNoHandleIndex: Integer; // When HandleExCalledCount=HandleExNoHandleIndex, don't set handled to true
    class var HandleExRaiseErrorIndex : Integer ; // When HandleExCalledCount=HandleExRaiseErrorIndex, raise exception
  private
    FEx : EAggregateException;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    property Ex : EAggregateException Read FEx Write FEx;
  published
    procedure TestHookUp;
    procedure TestAdd;
    procedure TestCreateWithArray;
    procedure TestCreateWithArray2;
    procedure TestToString;
    procedure TestHandleException1;
    procedure TestHandleException2;
    procedure TestHandleException3;
    // for E in Aggregate: exactly the inner exceptions, in order
    procedure TestEnumerate;
    procedure TestEnumerateAfterAdd;
  end;

  { TTestSparseArray }

  { TThreadedTestCase }

  TThreadedTestCase = Class(TTestcase)
  Public
    Type
      TPredicate = reference to procedure(out Done : Boolean);

      { TNotifyThread }

      TNotifyThread = class(TThread)
        Constructor create (aOnTerminate : TNotifyEvent); overload;
      end;
  Private
      FTerminatedCount : Integer;
      FWaitTerminatedCount: Integer;
      FErrors : TStrings;
      FCheckFailures : TStrings;
      FLock : TCriticalSection;
  Protected
      Procedure SetUp; override;
      // Joins the test threads, then fails if a ThreadCheck failed.
      Procedure TearDown; override;
      // Assert* must not be called from worker threads: the exception is
      // raised in that thread, not in the test, and fpcunit's counters are
      // not thread-safe. These record the failure; TearDown reports it.
      Procedure ThreadCheck(aCondition : Boolean; const aMsg : String);
      Procedure ThreadCheckSame(const aMsg : String; aExpected, aActual : TObject);
      Procedure ThreadTerminated(Sender : TObject);
      procedure WaitForTerminateCount(out Done : Boolean);
      Procedure AssertNoThreadErrors;
      Procedure AssertThreadErrors;
      Property TerminatedCount : Integer Read FTerminatedCount;
      Property WaitTerminatedCount : Integer Read FWaitTerminatedCount;
  Public
    constructor create; override;
    destructor destroy; override;
    // Simple polling loop that runs until predicate returns true or timeout (in milliseconds) was reached
    // Calls checksynchronize with aInterval.
    // Returns true if predicate was true, false if timeout was reached.
    function WaitForCondition(aPredicate : TPredicate; aTimeOut : Integer; aInterval : Integer = 10) : Boolean;

  end;

  TTestSparseArray = class(TThreadedTestCase)
  public
    Type
      TSparseObjectArray = specialize TSparseArray<TObject>;
      TObjectArray = Array of TObject;


      TSparseThread = Class(TNotifyThread)
        FList : TObjectArray;
        FArray:TSparseObjectArray;
        Constructor Create(aArray :TSparseObjectArray; aList : TObjectArray; aOnDestroy : TNotifyEvent);
        procedure DoItem(Itm : TObject); virtual; abstract;
        procedure Execute; override;
      end;

      { TAddThread }

      TAddThread = Class(TSparseThread)
        procedure DoItem(Itm : TObject); override;
      end;

      { TRemoveThread }

      TRemoveThread = Class(TSparseThread)
        procedure DoItem(Itm : TObject); override;
      end;
  private
    FSparse: TSparseObjectArray;
    FList1,
    FList2 : TObjectArray;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    property Sparse : TSparseObjectArray Read FSparse Write FSparse;
  published
    procedure TestHookUp;
    procedure TestAdd;
    procedure TestRemove;
  end;

  { TTestWorkStealingQueue }

  TTestWorkStealingQueue = class(TThreadedTestCase)

  protected
    Type
    TMyWorkQueue = specialize TWorkStealingQueue<Int64>;
    TInt64DynArray = Array of Int64;

    { TWorkQueueThread }

    TWorkQueueThread = Class(TNotifyThread)
      FList : TInt64DynArray;
      FQueue: TMyWorkQueue;
      Constructor Create(aQueue : TMyWorkQueue; aList : TInt64DynArray; aOnDestroy : TNotifyEvent);
    end;

    { TPushThread }

    TPushThread = Class(TWorkQueueThread)
      Procedure Execute; override;
    end;

    { TSingleAddThread }

    TSingleAddThread = Class(TNotifyThread)
      FValue : Int64;
      FSleep : integer;
      FQueue: TMyWorkQueue;
      Constructor Create(aQueue : TMyWorkQueue; aSleep : integer; aValue : Int64; aOnDestroy : TNotifyEvent);
      Procedure Execute; override;
    end;

    { TPopThread }

    TPopThread = Class(TWorkQueueThread)
      procedure Execute; override;
    end;

  private
    FQueue: TMyWorkQueue;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    property Queue : TMyWorkQueue Read FQueue Write FQueue;
  published
    procedure TestHookUp;
    procedure TestPush;
    procedure TestPushThreaded;
    procedure TestPop;
    procedure TestPopThreaded;
    procedure TestPopThreadedErr;
    procedure TestSteal;
    procedure TestStealFailTimeout;
    procedure TestRemove;
    procedure TestFindAndRemove;
    // TrySteal without timeout must be about as cheap as LocalPop
    procedure TestStealNoWaitIsCheap;
  end;

  { TCachedObject }

  TCachedObject = Class(TObject)
    class var _Cache : TObjectCache;
    Class Function newinstance: tobject; override;
    Procedure FreeInstance; override;
  end;

  { TTestObjectCache }

  TTestObjectCache = class(TTestCase)
  private
    FCache : TObjectCache;
  protected
    procedure ActivateCache;
    procedure DeActivateCache;
    procedure SetUp; override;
    procedure TearDown; override;
    property Cache : TObjectCache read FCache;
  Published
    Procedure TestHookup;
    Procedure TestAdd;
    Procedure TestClear;
    Procedure TestRemove;
    Procedure TestCreate;
  end;

  { TTestObjectCaches }

  TTestObjectCaches = Class(TTestCase)
  private
    FCaches: TObjectCaches;
  protected
    Procedure Setup; override;
    Procedure TearDown; override;
    Property Caches : TObjectCaches Read FCaches;
  Published
    Procedure TestHookup;
    procedure TestAdd;
    procedure TestGetValue;
  end;

  { TFreePoolThread }

  TFreePoolThread = class(TThreadedTestCase.TNotifyThread)
  private
    FPool: TThreadPool;
    procedure DoEmptyExecute(Sender: TObject);
  protected
    procedure Execute; override;
  public
    // Creates a thread that queues work in aPool, frees aPool at once and then calls aOnTerminate
    constructor Create(aPool: TThreadPool; aOnTerminate: TNotifyEvent);
  end;

  { TTestThreading }

  TTestThreading = class(TThreadedTestCase)
  private
    FThreadPool: TThreadPool;
    FWorkCount : integer;
    FWorkDone : Integer;
    FThreadsTerminated : Integer;
    FThreadsStarted : Integer;
    procedure DoThreadStart(arg: TThread);
    procedure DoThreadTerminate(arg: TThread);
    procedure WaitForWorkDoneCount(out Done : Boolean);
    procedure DoBurnCyclesExecute(Sender: TObject);
    procedure DoSimpleExecute(Sender: TObject);
    procedure DoCountWork(Sender: TObject);
    procedure DoSlowThreadTerminate(arg: TThread);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    property MyThreadPool : TThreadPool Read FThreadPool;
    property WorkCount : Integer Read FWorkCount Write FWorkCount;
    property WorkDone : Integer Read FWorkDone Write FWorkDone;
  published
    procedure TestHookUp;
    procedure TestCurrentOutsideTask;
    procedure TestSetMaxWorkerThreads;
    procedure TestSetMinWorkerThreads;
    procedure TestExecuteWork;
    procedure TestExecuteLotsOfWork;
    // Frees a pool whose worker threads have just been created
    procedure TestFreeWithNewThreads;
    // Pool.Free must return only when its threads have really ended
    procedure TestFreeJoinsThreads;
    // A failed ThreadCheck in a worker thread must be recorded
    procedure TestThreadCheckRecordsFailure;
  end;

  { TTestTask }

  TTestTask = class(TThreadedTestCase)
  private
    Type

      { TTaskThread }

      TTaskThread = Class(TThread)
        FTask : ITask;
        FSleep : Integer;
        Constructor Create(aTask : ITask; aSleep : Integer);
        procedure DoTask(aTask : ITask); virtual; abstract;
        procedure Execute; override;
      end;

      { TStartTaskThread }

      TStartTaskThread = class(TTaskThread)
        procedure DoTask(aTask : ITask); override;
      end;

    function CalcIntegerEvent(Sender: TObject): Integer;
    procedure CheckTaskCanceled;
  private
    FTask: ITask;
    FRaise : Boolean;
    FWorkExecuted : Boolean;
    FBlockedPool : TThreadPool;
    FBlockStarted : TEvent;
    FBlockRelease : TEvent;
    procedure CreateTask;
    procedure CreateBlockedTask;
    procedure ReleaseBlockedPool;
    procedure DoBlockWork(Sender: TObject);
    procedure OnTask(Sender: TObject);
    procedure DoQuickWork(Sender: TObject);
    procedure DoSlowWork(Sender: TObject);
    procedure DoRaiseWork(Sender: TObject);
    procedure StartTask;
    procedure WaitForTask;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    property Task : ITask Read FTask;
  Published
    Procedure TestHookup;
    procedure TestId;
    procedure TestStatus;
    Procedure TestShouldExecute;
    procedure TestExecuteWork;
    procedure TestExecuteWorkException;
    procedure TestWaitCardinal;
    procedure TestWaitTimeSpan;
    procedure TestCancel;
    procedure TestCheckCanceled;
    procedure TestStart;
    procedure TestStartTwice;
    procedure TestStartException;
    Procedure TestFuture;
    Procedure TestFutureEvent;
    // Waits for tasks that are all complete already
    procedure TestWaitForAllCompleted;
    // Waits for a complete task and a running task
    procedure TestWaitForAllMixed;
    // Waits for a running task with a timeout that expires
    procedure TestWaitForAllTimeout;
    // Waits for a complete task that raised an exception
    procedure TestWaitForAllException;
    // Waits for the first of a slow and a quick task
    procedure TestWaitForAny;
    // Waits for a slow and a complete task
    procedure TestWaitForAnyCompleted;
    // Waits for a running task with a timeout that expires
    procedure TestWaitForAnyTimeout;
  end;

  { TTestParallel }

  TTestParallel = Class(TThreadedTestCase)
  Public
    Type TResultArray = Array[1..255] of Integer;
  Private
    FResults : TResultArray;
    class var _Results : TResultArray;
    procedure CheckLocal;
    procedure DoEvent(aSender: TObject; aIndex: Integer);
    procedure DoEvent64(aSender: TObject; aIndex: Int64);
    procedure DoSleepEvent(aSender: TObject; aIndex: Integer);
    procedure DoCountEvent(aSender: TObject; aIndex: Integer);
    procedure DoCountEvent64(aSender: TObject; aIndex: Int64);
    procedure CheckCountedOnce(const aMsg: String);
  Protected
    procedure SetUp; override;
    procedure TearDown; override;
  Published
    Procedure TestHookup;
    Procedure TestForEvent;
    {$IFDEF CPU64}
    Procedure TestForEvent64;
    {$ENDIF}
    Procedure TestForColdPoolParallel;
    Procedure TestForEachIterationOnce;
    {$IFDEF CPU64}
    Procedure TestForEachIterationOnce64;
    {$ENDIF}
    // Loops with a TLoopState parameter
    Procedure TestForWithState;
    Procedure TestForBreak;
    Procedure TestForStop;
    // The exception raised in an iteration reaches the caller
    Procedure TestForExceptionPreserved;
    // TParallel.Join starts the task and completes without deadlocking
    Procedure TestJoin;
  end;

  { TTestSyncObjsPhase2 }

  TTestSyncObjsPhase2 = Class(TThreadedTestCase)
  Published
    procedure TestCountdownEventBasic;
    procedure TestCountdownEventParallel;
    procedure TestBarrierMultiPhase;
    procedure TestBarrierAddRemoveParticipant;
    procedure TestBarrierPostPhaseException;
    procedure TestSlimReaderWriterLockConcurrent;
    procedure TestChannelUnboundedMultiProducer;
    procedure TestChannelBoundedBackpressure;
  end;

  { TTestContinuationsAndCancellation }

  TTestContinuationsAndCancellation = Class(TThreadedTestCase)
  Published
    procedure TestCancellationTokenBasics;
    procedure TestContinueWithAndOptions;
    procedure TestThenByPipeline;
    procedure TestTreeCancellationAndParallelFor;
  end;


implementation

uses DateUtils, System.Diagnostics;

// The global Random is not thread-safe, and these helpers run in pool threads.
// Thread-safe replacement: hash of an atomic counter (splitmix32 finalizer).
var
  _RandCounter : Cardinal;

function ThreadRandom(aRange: Integer): Integer;
var
  X : Cardinal;
begin
  X:=Cardinal(AtomicIncrement(_RandCounter))*$9E3779B9;
  X:=(X xor (X shr 16))*$85EBCA6B;
  X:=(X xor (X shr 13))*$C2B2AE35;
  X:=X xor (X shr 16);
  Result:=X mod Cardinal(aRange);
end;

// Threads created by the tests. They are not FreeOnTerminate: OnTerminate runs
// before a thread has really ended, so TThreadedTestCase.TearDown joins and
// frees them. Otherwise they could outlive the test (or the program).
var
  _TestThreads : TThreadList;

procedure RegisterTestThread(aThread: TThread);
begin
  _TestThreads.Add(aThread);
end;

procedure JoinTestThreads;
var
  L : TList;
  Threads : array of TThread;
  I : Integer;
begin
  Threads:=[];
  L:=_TestThreads.LockList;
  try
    SetLength(Threads,L.Count);
    For I:=0 to L.Count-1 do
      Threads[I]:=TThread(L[I]);
    L.Clear;
  finally
    _TestThreads.UnlockList;
  end;
  For I:=0 to Length(Threads)-1 do
    begin
    Threads[I].WaitFor;
    Threads[I].Free;
    end;
end;

procedure HandleEx(const aException: Exception; var aHandled: Boolean); forward;

{ TMyTask }

function TMyTask.GetException: TObject;
begin
  Result:=FException;
end;

{ TLiveObject }

constructor TLiveObject.Create(aOnDestroy: TNotifyProc);
begin
  OnDestroy:=aOnDestroy;
end;

destructor TLiveObject.Destroy;
begin
  if assigned(OnDestroy) then
    OnDestroy(Self);
  inherited Destroy;
end;

{ Ex1 }

constructor Ex1.Create(aID: Integer; aOnDestroy: TNotifyProc);
begin
  ID:=AID;
  OnDestroy:=aOnDestroy;
end;

destructor Ex1.Destroy;
begin
  if assigned(OnDestroy) then
    OnDestroy(Self);
  inherited Destroy;
end;

{ TTestTExceptionList }

procedure TTestTExceptionList.SetUp;
begin
  inherited SetUp;
  FList:=Default(TExceptionList);
  FEx[1]:=Ex1.Create('ex1');
  FEx[2]:=Ex2.Create('ex2');
  FEx[3]:=Ex3.Create('ex3');

end;


procedure TTestTExceptionList.TearDown;

Var
  I : Integer;

begin
  FList:=Default(TExceptionList);
  For I:=1 to 3 do
    FreeAndNil(FEx[i]);
  inherited TearDown;
end;

procedure TTestTExceptionList.TestHookUp;
begin
  AssertTrue('List',Flist.List=nil);
  AssertEquals('Count',0,Flist.Count);
  AssertEquals('Capacity',0,Flist.Capacity);
end;

procedure TTestTExceptionList.TestCreate;
begin
  FList:=TExceptionList.Create(10);
  AssertEquals('List',10,Length(Flist.List));
  AssertEquals('Count',0,Flist.Count);
  AssertEquals('Capacity',10,Flist.Capacity);
end;

procedure TTestTExceptionList.TestAdd;

var
  E : Exception;

begin
  FList:=TExceptionList.Create(10);
  E:=FEx[1];
  FList.Add(E);
  AssertEquals('List',10,Length(Flist.List));
  AssertEquals('Count',1,Flist.Count);
  AssertSame('Exc',E,Flist.list[0]);
  E:=FEx[2];
  FList.Add(E);
  AssertEquals('List',10,Length(Flist.List));
  AssertEquals('Count',2,Flist.Count);
  AssertSame('Exc',E,Flist.list[1]);
end;

procedure TTestTExceptionList.TestGrow;

var
  Ex : Array[1..20] of Exception;
  E : Exception;
  I : Integer;

begin
  FList:=TExceptionList.Create(10);
  For I:=1 to 20 do
    Ex[I]:=Nil;
  try
    For I:=1 to 20 do
      begin
      E:=Ex1.Create('Ex'+IntToStr(i));
      Ex[I]:=E;
      FList.Add(E);
      end;
    AssertEquals('List',20,Length(Flist.List));
    AssertEquals('Count',20,Flist.Count);
    For I:=1 to 20 do
      AssertSame('Exc'+IntToStr(i),Ex[i],Flist.list[i-1]);
  finally
    For I:=1 to 20 do
      FreeAndNil(Ex[I]);
  end;
end;

procedure TTestTExceptionList.TestClear;

var
  Ex : Array[1..20] of Exception;
  E : Exception;
  I : Integer;
  P : TNotifyProc;

  procedure dodestroy (sender : tobject);
  begin
    Ex[(sender as Ex1).id]:=Nil;
  end;

begin
  P:=@DoDestroy;
  Flist:=TExceptionList.Create(10);
  For I:=1 to 20 do
    begin
    E:=Ex1.Create(i,P);
    Ex[I]:=E;
    FList.Add(E);
    end;
  Flist.ClearList;
  For I:=1 to 20 do
    AssertNull('Ex '+IntToStr(I),Ex[I]);
end;

procedure TTestTExceptionList.TestTruncate;

var
  Ex : TExceptionArray;
  I : Integer;

begin
  FList:=TExceptionList.Create(10);
  For I:=1 to 3 do
    FList.Add(FEx[i]);
  Ex:=Flist.Truncate;
  AssertEquals('Length',3,Length(Ex));
  For I:=1 to 3 do
    AssertSame('Ex'+IntToStr(i),FEx[i],Ex[i-1]);
end;

procedure TTestTExceptionList.TestGrowCapacity;
begin
  FList:=TExceptionList.Create(10);
  AssertEquals('Capacity before',10,FList.Capacity);
  Flist.GrowCapacity(5);
  AssertEquals('Capacity after smaller',10,FList.Capacity);
  Flist.GrowCapacity(15);
  AssertEquals('Capacity after bigger',15,FList.Capacity);
end;

procedure TTestTExceptionList.TestFlatten;

var
  I : Integer;

begin
  FList:=TExceptionList.Create(1);
  For I:=1 to 3 do
    Flist.Flatten(FEx[i]);
  AssertEquals('All in list',3,FList.Count);
  For I:=1 to 3 do
    AssertSame('Ex'+IntToStr(i),FEx[i],FList.List[i-1]);
end;

procedure TTestTExceptionList.TestFlatten2;

var
  A : EAggregateException;
  I : Integer;

begin
  FList:=TExceptionList.Create(1);
  A:=EAggregateException.Create('a',[Fex[1],Fex[2],Fex[3]]);
  try
    FList.Flatten(A);
    AssertEquals('Cleared A',0,A.Count);
    AssertEquals('All in list',3,FList.Count);
    AssertEquals('List capacity',3,FList.Capacity);
    For I:=1 to 3 do
      AssertSame('Ex'+IntToStr(i),FEx[i],FList.List[i-1]);
  finally
    A.Free;
  end;
end;

procedure TTestTExceptionList.TestAddFromTaskNonEx;

var
  aTask : TMyTask;
  aParams : TTask.TTaskParams;
  O : TLiveObject;
  P : TNotifyProc;

  Procedure DoDestroy(sender : TObject);
  begin
    if sender=o then
      O:=Nil;
  end;

begin
  P:=@DoDestroy;
  aParams:=Default(TTask.TTaskParams);
  aTask:=TMyTask.Create(aParams);
  try
    O:=TLiveObject.Create(P);
    aTask.SetExceptionObject(O);
    FList.AddFromTask(aTask);
    AssertNull('No more exception',aTask.GetException);
    AssertEquals('Nothing added',0,FList.count);
    AssertNull('Object destroyed',O);
  finally
    aTask.Free;
  end;
end;

procedure TTestTExceptionList.TestAddFromTaskEx;

var
  aTask : TMyTask;
  aParams : TTask.TTaskParams;

begin
  aParams:=Default(TTask.TTaskParams);
  aTask:=TMyTask.Create(aParams);
  try
    aTask.SetExceptionObject(Fex[1]);
    FList.AddFromTask(aTask);
    AssertNull('No more exception',aTask.GetException);
    AssertEquals('Something added',1,FList.count);
    AssertSame('Correct object',Fex[1],Flist.List[0]);
  finally
    aTask.Free;
  end;
end;

procedure TTestTExceptionList.TestAddFromTaskAggEx;
var
  A : EAggregateException;
  I : Integer;
  aTask : TMyTask;
  aParams : TTask.TTaskParams;

begin
  FList:=TExceptionList.Create(1);
  aTask:=Nil;
  A:=EAggregateException.Create('a',[Fex[1],Fex[2],Fex[3]]);
  try
    aParams:=Default(TTask.TTaskParams);
    aTask:=TMyTask.Create(aParams);
    aTask.SetExceptionObject(A);
    FList.AddFromTask(aTask);
    AssertNull('No more exception',aTask.GetException);
    AssertEquals('All in list',3,FList.Count);
    AssertEquals('List capacity',3,FList.Capacity);
    For I:=1 to 3 do
      AssertSame('Ex'+IntToStr(i),FEx[i],FList.List[i-1]);
  finally
    aTask.Free;
  end;
end;

{ TTestAggregateException }

procedure HandleEx(const aException: Exception; var aHandled: Boolean);
begin
  Inc(TTestAggregateException.HandleExCalledCount);
  aHandled:=TTestAggregateException.HandleExCalledCount<>TTestAggregateException.HandleExNoHandleIndex;
  if (TTestAggregateException.HandleExCalledCount= TTestAggregateException.HandleExRaiseErrorIndex) then
    Raise Ex1.Create('Xevious');
end;

procedure TTestAggregateException.SetUp;
begin
  inherited SetUp;
  FEx:=EAggregateException.Create('x');
  HandleExCalledCount:=0;
  HandleExNoHandleIndex:=0;
end;

procedure TTestAggregateException.TearDown;
begin
  FreeAndNil(FEx);
  inherited TearDown;
end;

procedure TTestAggregateException.TestHookUp;
begin
  AssertNotNull('Have exception',Fex);
  AssertEquals('Message','x',Fex.Message);
  AssertEquals('Count',0,Fex.Count);
  AssertEquals('HandleExCalledCount',0,HandleExCalledCount);
  AssertEquals('HandleExNoHandleIndex',0,HandleExNoHandleIndex);

end;

procedure TTestAggregateException.TestAdd;

var
  E : Ex1;
  P : TNotifyProc;

  Procedure DoDestroy(sender : TObject);
    begin
      if sender=E then
        E:=Nil;
    end;

begin
  P:=@DoDestroy;
  E:=Ex1.Create(0,P);
  try
    Ex.Add(E);
    AssertEquals('Count',1,Ex.Count);
    AssertSame('Inner',E,Ex.InnerExceptions[0]);
  finally
    FreeAndNil(FEx);
  end;
  AssertNull('Exception freed',E);
end;

function EnumeratedMessages(aEx: EAggregateException): String;
var
  E : Exception;
begin
  Result:='';
  for E in aEx do
    begin
    if not Assigned(E) then
      Exit(Result+'<nil>');
    Result:=Result+E.Message+';';
    end;
end;

procedure TTestAggregateException.TestEnumerate;
// Regression test: MoveNext checked the bound before incrementing, so the
// enumerator returned one element past the end.
begin
  FreeAndNil(Fex);
  Fex:=EAggregateException.Create('X',[Exception.Create('a'),Exception.Create('b')]);
  AssertEquals('Enumerated','a;b;',EnumeratedMessages(Ex));
end;

procedure TTestAggregateException.TestEnumerateAfterAdd;
// Regression test: Add grows the list by 10, and the enumerator walked the
// whole array instead of the Count used elements.
begin
  Ex.Add(Exception.Create('a'));
  Ex.Add(Exception.Create('b'));
  Ex.Add(Exception.Create('c'));
  AssertEquals('Count',3,Ex.Count);
  AssertEquals('Enumerated','a;b;c;',EnumeratedMessages(Ex));
end;

procedure TTestAggregateException.TestCreateWithArray;
var
  E1 : Ex1;
  E2 : Ex2;
  P : TNotifyProc;

  Procedure DoDestroy(sender : TObject);
    begin
      if sender=E1 then
        E1:=Nil;
      if sender=E2 then
        E2:=Nil;
    end;

begin
  FreeAndNil(Fex);
  P:=@DoDestroy;
  E1:=Ex1.Create(1,P);
  try
    E2:=Ex2.Create(2,P);
    Fex:=EAggregateException.Create('X',[E1,E2]);
    AssertEquals('Msg','X',Ex.Message);
    AssertEquals('Count',2,Ex.Count);
    AssertSame('Inner 1',E1,Ex.InnerExceptions[0]);
    AssertSame('Inner 2',E2,Ex.InnerExceptions[1]);
  finally
    FreeAndNil(FEx);
  end;
  AssertNull('Exception freed',E1);
  AssertNull('Exception freed',E2);
end;

procedure TTestAggregateException.TestCreateWithArray2;
var
  E1 : Ex1;
  E2 : Ex2;
  P : TNotifyProc;

  Procedure DoDestroy(sender : TObject);
    begin
      if sender=E1 then
        E1:=Nil;
      if sender=E2 then
        E2:=Nil;
    end;

begin
  FreeAndNil(Fex);
  P:=@DoDestroy;
  E1:=Ex1.Create(1,P);
  try
    E2:=Ex2.Create(2,P);
    Fex:=EAggregateException.Create([E1,E2]);
    AssertEquals('Count',2,Ex.Count);
    AssertSame('Inner 1',E1,Ex.InnerExceptions[0]);
    AssertSame('Inner 2',E2,Ex.InnerExceptions[1]);
  finally
    FreeAndNil(FEx);
  end;
  AssertNull('Exception freed',E1);
  AssertNull('Exception freed',E2);
end;

procedure TTestAggregateException.TestToString;

Const
  S = 'EAggregateException: x'+sLineBreak+
      'Aggregate exception for 2 exceptions'+sLineBreak+
      '#0 Ex1: 1'+sLineBreak+
      '#1 Ex2: 2';

begin
  Ex.Add(Ex1.Create('1'));
  Ex.Add(Ex2.Create('2'));
  AssertEquals('ToString',S,Ex.ToString);
end;

procedure TTestAggregateException.TestHandleException1;

Var
  P : TExceptionHandlerProc;

begin
  P:=@HandleEx;
  Ex.Add(Ex1.Create('1'));
  Ex.Add(Ex2.Create('2'));
  Ex.Handle(P);
  AssertEquals('Handler called',2,HandleExCalledCount);
end;

procedure TTestAggregateException.TestHandleException2;
Var
  P : TExceptionHandlerProc;
  HaveEx : Boolean;

begin
  P:=@HandleEx;
  HandleExNoHandleIndex:=2;
  Ex.Add(Ex1.Create('1'));
  Ex.Add(Ex2.Create('2'));
  HaveEx:=False;
  try
    Ex.Handle(P);
  except
    on E : EAggregateException do
      HaveEx:=True;
  end;
  AssertTrue('Have exception',HaveEx);
  AssertEquals('Handler called',2,HandleExCalledCount);
  AssertEquals('Still own processed', 1, Ex.Count);

end;

procedure TTestAggregateException.TestHandleException3;
Var
  P : TExceptionHandlerProc;
  HaveEx : Boolean;

begin
  P:=@HandleEx;
  HandleExNoHandleIndex:=2;
  HandleExRaiseErrorIndex:=2;
  Ex.Add(Ex1.Create('1'));
  Ex.Add(Ex2.Create('2'));
  HaveEx:=False;
  try
    Ex.Handle(P);
  except
    on E : Ex1 do
      HaveEx:=True;
  end;
  AssertTrue('Have exception',HaveEx);
  AssertEquals('Handler called',2,HandleExCalledCount);
  AssertEquals('Still own all', 2, Ex.Count);
end;

{ TThreadedTestCase }

procedure TThreadedTestCase.SetUp;
begin
  inherited SetUp;
  FTerminatedCount:=0;
  FWaitTerminatedCount:=0;
  FLock.Enter;
  try
    FErrors.Clear;
    FCheckFailures.Clear;
  finally
    FLock.Leave;
  end;
end;

procedure TThreadedTestCase.TearDown;

var
  Failures : String;

begin
  JoinTestThreads;
  inherited TearDown;
  FLock.Enter;
  try
    Failures:=FCheckFailures.Text;
    FCheckFailures.Clear;
  finally
    FLock.Leave;
  end;
  if Failures<>'' then
    Fail('Checks failed in worker threads:'+sLineBreak+Failures);
end;

procedure TThreadedTestCase.ThreadCheck(aCondition: Boolean; const aMsg: String);
begin
  if aCondition then
    exit;
  FLock.Enter;
  try
    FCheckFailures.Add(Format('[thread %d] %s',[PtrUInt(GetCurrentThreadId),aMsg]));
  finally
    FLock.Leave;
  end;
end;

procedure TThreadedTestCase.ThreadCheckSame(const aMsg: String; aExpected, aActual: TObject);
begin
  ThreadCheck(aExpected=aActual,Format('%s: expected %p, got %p',[aMsg,Pointer(aExpected),Pointer(aActual)]));
end;

procedure TThreadedTestCase.ThreadTerminated(Sender: TObject);

var
  O : TObject;
  Error : String;

begin
  AtomicIncrement(FTerminatedCount);
  O:=(Sender as TThread).FatalException;
  if Assigned(O) then
    begin
    Error:=Sender.ClassName+' : '+O.ClassName;
    if (O is Exception) then
      Error:=Error+'('+Exception(O).Message+')';
    FLock.Enter;
    try
      FErrors.Add(Error)
    finally
      FLock.Leave;
    end;
    end;
end;

procedure TThreadedTestCase.WaitForTerminateCount(out Done: Boolean);
begin
  Done:=(FWaitTerminatedCount>0) and (FTerminatedCount>=FWaitTerminatedCount);
{$IFDEF DEBUGTEST}
  Writeln('Done:=(',FWaitTerminatedCount,'>0) and (',FTerminatedCount,'>=',FWaitTerminatedCount,') : ',Done);
{$ENDIF}
end;

procedure TThreadedTestCase.AssertNoThreadErrors;
begin
  if FErrors.Count<>0 then
    Fail('Unexpected thread errors:'+sLineBreak+FErrors.Text);
end;

procedure TThreadedTestCase.AssertThreadErrors;
begin
  if FErrors.Count=0 then
    Fail('Expected thread errors, but none were recorded');
end;

constructor TThreadedTestCase.create;


begin
  inherited create;
  FLock:=TCriticalSection.Create;
  Flush(output);
  FErrors:=TStringList.Create;
  FCheckFailures:=TStringList.Create;
end;

destructor TThreadedTestCase.destroy;
begin
  Flush(output);
  FreeAndNil(FErrors);
  FreeAndNil(FCheckFailures);
  FreeAndNil(FLock);
  inherited destroy;
end;

function TThreadedTestCase.WaitForCondition(aPredicate: TPredicate; aTimeOut: Integer; aInterval: Integer): Boolean;

Var
  aStart : TDateTime;

begin
  aStart:=Now;
  Result:=False;
  Repeat
    CheckSynchronize(aInterval);
    aPredicate(Result);
  until Result or (MilliSecondsBetween(Now,aStart)>aTimeOut);
end;

{ TThreadedTestCase.TNotifyThread }

constructor TThreadedTestCase.TNotifyThread.create(aOnTerminate: TNotifyEvent);
begin
  OnTerminate:=aOnTerminate;
  FreeOnTerminate:=False; // Joined and freed in TThreadedTestCase.TearDown
  Inherited Create(False);
  RegisterTestThread(Self);
end;

{ TFreePoolThread }

constructor TFreePoolThread.Create(aPool: TThreadPool; aOnTerminate: TNotifyEvent);

begin
  FPool:=aPool;
  Inherited Create(aOnTerminate);
end;

procedure TFreePoolThread.DoEmptyExecute(Sender: TObject);

begin
end;

procedure TFreePoolThread.Execute;

begin
  FPool.QueueWorkItem(Self,@DoEmptyExecute);
  FreeAndNil(FPool);
end;

{ TTestSparseArray }

procedure TTestSparseArray.SetUp;

var
  I : Integer;

begin
  inherited SetUp;
  SetLength(FList1,10);
  For I:=0 to Length(FList1)-1 do
    FList1[I]:=Ex1.Create(I,Nil);
  SetLength(FList2,10);
  For I:=0 to Length(FList2)-1 do
    FList2[I]:=Ex1.Create(I,Nil);
  FSparse:=TSparseObjectArray.Create(5);
end;

procedure TTestSparseArray.TearDown;

var
  I : Integer;

begin
  For I:=0 to Length(FList1)-1 do
    FreeAndNil(FList1[i]);
  SetLength(FList1,0);
  For I:=0 to Length(FList2)-1 do
    FreeAndNil(FList2[i]);
  SetLength(FList2,0);
  FreeAndNil(FSparse);
  inherited TearDown;
end;

procedure TTestSparseArray.TestHookUp;
begin
  AssertNotNull('Have obj',Sparse);
  AssertEquals('Have list 1 of objects',10,Length(FList1));
  AssertEquals('Have list 2 of objects',10,Length(FList2));
end;

procedure TTestSparseArray.TestAdd;

var
  I : Integer;
  L : Array of TObject;

begin
  FWaitTerminatedCount:=2;
  TAddThread.Create(FSparse,FList1,@ThreadTerminated);
  TAddThread.Create(FSparse,FList2,@ThreadTerminated);
  AssertEquals('All added',True,WaitForCondition(@WaitForTerminateCount,2000));
  L:=FSparse.Current;
  AssertEquals('Length',20,Length(L));
end;

procedure TTestSparseArray.TestRemove;
var
  I : Integer;
  L : Array of TObject;
  O : TObject;

begin
  FWaitTerminatedCount:=2;
  for O in FList1 do
    FSparse.Add(O);
  for O in FList2 do
    FSparse.Add(O);
  L:=FSparse.Current;
  AssertEquals('Length',20,Length(L));
  TRemoveThread.Create(FSparse,FList1,@ThreadTerminated);
  TRemoveThread.Create(FSparse,FList2,@ThreadTerminated);
  AssertEquals('All added',True,WaitForCondition(@WaitForTerminateCount,2000));
  L:=FSparse.Current;
  // Still at 20, but all nil.
  AssertEquals('Length',20,Length(L));
  for O in L do
    AssertNull('Null',O);
end;

{ TTestSparseArray.TAddThread }

constructor TTestSparseArray.TSparseThread.Create(aArray: TSparseObjectArray; aList: TObjectArray; aOnDestroy: TNotifyEvent);
begin
  {$IFDEF DEBUGTEST}
  Writeln('TTestSparseArray.TSparseThread.Create');
  {$ENDIF}
  FArray:=aArray;
  FList:=AList;
  Inherited Create(aOnDestroy);
end;

procedure TTestSparseArray.TSparseThread.Execute;

var
  O : TObject;

begin
  {$IFDEF DEBUGTEST}
  Writeln('TTestSparseArray.TSparseThread.Execute');
  {$ENDIF}
  For O in FList do
    begin
    Sleep(ThreadRandom(100));
    {$IFDEF DEBUGTEST}
    Writeln('Handling ',O.ToString);
    {$ENDIF}
    DoItem(O);
    if Terminated then
      break;
    end;
end;

procedure TTestSparseArray.TAddThread.DoItem(Itm: TObject);
begin
  FArray.Add(Itm);
end;

{ TTestSparseArray.TRemoveThread }

procedure TTestSparseArray.TRemoveThread.DoItem(Itm: TObject);
begin
  FArray.Remove(Itm);
end;

{ TTestWorkStealingQueue }

procedure TTestWorkStealingQueue.SetUp;
begin
  inherited SetUp;
  FQueue:=TMyWorkQueue.Create;
end;

procedure TTestWorkStealingQueue.TearDown;
begin
  FreeAndNil(FQueue);
  inherited TearDown;
end;

procedure TTestWorkStealingQueue.TestHookUp;
begin
  AssertNotNull('Have queue',Queue);
  AssertTrue('Queue is empty',Queue.IsEmpty);
end;

procedure TTestWorkStealingQueue.TestPush;

var
  I : int64;

begin
  Queue.LocalPush(1);
  AssertTrue('Can pop',Queue.LocalPop(I));
  AssertEquals('Correct popped',1,I);
  AssertFalse('Can no longer pop',Queue.LocalPop(I));
end;

procedure TTestWorkStealingQueue.TestPushThreaded;

Var
  L1,L2 : TInt64DynArray;
  I : INteger;

begin
  FWaitTerminatedCount:=2;
  SetLength(L1,10);
  For I:=1 to 10 do
    L1[I-1]:=I;
  SetLength(L2,10);
  For I:=11 to 20 do
    L2[I-11]:=I;
  TPushThread.Create(FQueue,L1,@ThreadTerminated);
  TPushThread.Create(FQueue,L2,@ThreadTerminated);
  AssertNoThreadErrors;
  AssertEquals('All added',True,WaitForCondition(@WaitForTerminateCount,6000));
  AssertEquals('Length',20,FQueue.Count);
end;

procedure TTestWorkStealingQueue.TestPop;

Var
  I : Integer;
  IP : Int64;

begin
  For I:=1 to 20 do
    FQueue.LocalPush(I);
  For I:=1 to 20 do
    if not FQueue.LocalPop(IP) then
      Fail('Failed to pop at '+IntToStr(I))
    else
      AssertEquals('Correct value popped at '+IntToStr(I),21-I,IP);
  AssertEquals('Length',0,FQueue.Count);
end;

procedure TTestWorkStealingQueue.TestPopThreaded;
Var
  I : INteger;
  L2,L1 : TInt64DynArray;

begin
  FWaitTerminatedCount:=2;
  For I:=1 to 20 do
    FQueue.LocalPush(I);
  SetLength(L1,10);
  SetLength(L2,10);
  TPopThread.Create(FQueue,L1,@ThreadTerminated);
  TPopThread.Create(FQueue,L2,@ThreadTerminated);
  AssertEquals('All added',True,WaitForCondition(@WaitForTerminateCount,4000));
  AssertNoThreadErrors;
  AssertEquals('Length',0,FQueue.Count);
end;

procedure TTestWorkStealingQueue.TestPopThreadedErr;
Var
  I : INteger;
  L2,L1 : TInt64DynArray;

begin
  FWaitTerminatedCount:=2;
  For I:=1 to 20 do
    FQueue.LocalPush(I);
  SetLength(L1,20);
  SetLength(L2,20);
  TPopThread.Create(FQueue,L1,@ThreadTerminated);
  TPopThread.Create(FQueue,L2,@ThreadTerminated);
  AssertEquals('All added',True,WaitForCondition(@WaitForTerminateCount,4000));
  AssertThreadErrors;
  AssertEquals('Length',0,FQueue.Count);
end;

procedure TTestWorkStealingQueue.TestSteal;

var
  I : Int64;

begin
  FWaitTerminatedCount:=1;
  TSingleAddThread.Create(FQueue,100,321,@ThreadTerminated);
  AssertTrue('Can steal',FQueue.TrySteal(I,400));
  AssertEquals('All added',True,WaitForCondition(@WaitForTerminateCount,4000));
  AssertEquals('Correct popped',321,I);
end;

procedure TTestWorkStealingQueue.TestStealFailTimeout;
var
  I : Int64;

begin
  FWaitTerminatedCount:=1;
  TSingleAddThread.Create(FQueue,1000,321,@ThreadTerminated);
  AssertFalse('Cannot steal',FQueue.TrySteal(I,400));
  AssertEquals('All added',True,WaitForCondition(@WaitForTerminateCount,4000));
end;

procedure TTestWorkStealingQueue.TestStealNoWaitIsCheap;
// Regression test: TrySteal(Item, 0) on an empty queue reset and waited on the
// queue event (two system calls) although no wait was requested. The pool
// calls it for every queue while holding a spin lock, so idle workers ended
// up in TSpinWait's Sleep(1) and joined a parallel loop one by one.
const
  Calls = 200000;
var
  I : Integer;
  Item : Int64;
  W : TStopWatch;
  PopTime, StealTime : Int64;
begin
  W:=TStopWatch.StartNew;
  For I:=1 to Calls do
    FQueue.LocalPop(Item);
  PopTime:=W.ElapsedMilliseconds;
  W:=TStopWatch.StartNew;
  For I:=1 to Calls do
    AssertFalse('Nothing to steal',FQueue.TrySteal(Item,0));
  StealTime:=W.ElapsedMilliseconds;
  AssertTrue(Format('TrySteal(0): %d ms, LocalPop: %d ms',[StealTime,PopTime]),
             StealTime<=4*PopTime+20);
end;

procedure TTestWorkStealingQueue.TestRemove;

var
  I : integer;

begin
  For I:=1 to 20 do
    FQueue.LocalPush(I);
  AssertEquals('Count before',20,FQueue.Count);
  AssertTrue('Remove existing',FQueue.Remove(18));
  AssertEquals('Count after',19,FQueue.Count);
  AssertFalse('Remove un existing',FQueue.Remove(33));
  AssertEquals('Count after 2',19,FQueue.Count);
end;

procedure TTestWorkStealingQueue.TestFindAndRemove;

var
  I : integer;

begin
  For I:=1 to 20 do
    FQueue.LocalPush(I);
  AssertEquals('Count before',20,FQueue.Count);
  AssertTrue('Remove existing',FQueue.LocalFindAndRemove(18));
  AssertEquals('Count after',19,FQueue.Count);
  AssertFalse('Remove un existing',FQueue.LocalFindAndRemove(33));
  AssertEquals('Count after 2',19,FQueue.Count);
end;

{ TTestWorkStealingQueue.TWorkQueueThread }

constructor TTestWorkStealingQueue.TWorkQueueThread.Create(aQueue: TMyWorkQueue; aList: TInt64DynArray; aOnDestroy: TNotifyEvent);
begin
  FList:=aList;
  FQueue:=aQueue;
  Inherited Create(aOnDestroy);
end;

{ TTestWorkStealingQueue.TAddThread }

procedure TTestWorkStealingQueue.TPushThread.Execute;

var
  I : Int64;

begin
  For I in FList do
    begin
    Sleep(ThreadRandom(100));
    {$IFDEF DEBUGTEST}
    Writeln('Pushing');
    {$ENDIF}
    FQueue.LocalPush(I);
    end;
  {$IFDEF DEBUGTEST}
  Writeln('Done');
  {$ENDIF}
end;

{ TTestWorkStealingQueue.TSingleAddThread }

constructor TTestWorkStealingQueue.TSingleAddThread.Create(aQueue: TMyWorkQueue; aSleep : integer; aValue: Int64; aOnDestroy: TNotifyEvent);
begin
  FValue:=aValue;
  FQueue:=aQueue;
  FSleep:=aSleep;
  Inherited Create(aOnDestroy);
end;

procedure TTestWorkStealingQueue.TSingleAddThread.Execute;
begin
  Sleep(FSleep);
  FQueue.LocalPush(FValue);
end;

{ TTestWorkStealingQueue.TRemoveThread }

procedure TTestWorkStealingQueue.TPopThread.Execute;

var
  i : Integer;

begin
  For I:=0 to Length(FList)-1 do
    begin
    Sleep(ThreadRandom(100));
    if not FQueue.LocalPop(FList[I]) then
      raise Exception.CreateFmt('Failed to get item %d',[I]);
    end;
end;

{ TCachedObject }

class function TCachedObject.newinstance: tobject;
var
  Obj : Pointer;

begin
  Result:=Nil;
  if Assigned(_cache) then
    begin
    Obj:=_cache.Remove;
    if Assigned(Obj) then
      Result:=InitInstance(Obj);
    end;
  If Not Assigned(Result) then
    Result:=inherited NewInstance;
end;

procedure TCachedObject.FreeInstance;


begin
  CleanupInstance;
  if Assigned(_Cache) then
    if _Cache.Insert(Pointer(Self)) then
      Exit;
  Inherited;
end;

{ TTestObjectCache }

procedure TTestObjectCache.ActivateCache;
begin
  TCachedObject._Cache:=FCache;
end;

procedure TTestObjectCache.DeActivateCache;
begin
  TCachedObject._Cache:=Nil;
end;

procedure TTestObjectCache.SetUp;
begin
  inherited SetUp;
  FCache:=TObjectCache.Create(TCachedObject);
end;

procedure TTestObjectCache.TearDown;
begin
  TCachedObject._Cache:=Nil;
  FreeAndNil(FCache);
  inherited TearDown;
end;

procedure TTestObjectCache.TestHookup;
begin
  AssertNotNull('Have cache',Cache);
  AssertNull('Cache not active',TCachedObject._Cache);
end;

procedure TTestObjectCache.TestAdd;

Var
  Obj : TCachedObject;

begin
  // Create without cache.
  Obj:=TCachedObject.Create;
  Cache.Insert(Obj);
  AssertEquals('Count',1,Cache.Count);
  // The memory of the object is now managed by the cache.
end;

procedure TTestObjectCache.TestClear;

Var
  Obj : TCachedObject;
  I : integer;

begin
  // Create without cache.
  For I:=1 to 10 do
    begin
    Obj:=TCachedObject.Create;
    Cache.Insert(Obj);
    end;
  // The memory of the objects is now managed by the cache.
  AssertEquals('Count',10,Cache.Count);
  Cache.Clear;
  AssertEquals('Count',0,Cache.Count);
end;

procedure TTestObjectCache.TestRemove;
Var
  Obj : Array[1..10] of TCachedObject;
  I : integer;
  P : Pointer;

begin
  // Create without cache.
  For I:=1 to 10 do
    begin
    Obj[i]:=TCachedObject.Create;
    AssertTrue('Insert '+IntToStr(I)+'OK',Cache.Insert(Obj[i]));
    end;
  // The memory of the objects is now managed by the cache.
  AssertEquals('Count',10,Cache.Count);
  For I:=1 to 10 do
    begin
    P:=Cache.Remove;
    AssertNotNull('Got pointer',P);
    // Free the memory.
    FreeMem(P);
    end;
  AssertNull('No 11th pointer',Cache.Remove);
end;

procedure TTestObjectCache.TestCreate;
Var
  Obj : Array[1..10] of TCachedObject;
  I : Integer;
begin
  ActivateCache;
  For I:=1 to 10 do
    Obj[i]:=TCachedObject.Create;
  // nothing in cache yet.
  AssertEquals('Count',0,Cache.Count);
  For I:=1 to 10 do
    FreeAndNil(Obj[i]);
  // All objects should be in cache.
  AssertEquals('Count',10,Cache.Count);
  For I:=1 to 10 do
    Obj[i]:=TCachedObject.Create;
  // Pointers from cache should have been reused.
  AssertEquals('Count',0,Cache.Count);
  DeActivateCache;
  For I:=1 to 10 do
    FreeAndNil(Obj[i]);
  // Cache was disabled, to object should have been freed...
  AssertEquals('Count',0,Cache.Count);
end;

{ TTestObjectCaches }

procedure TTestObjectCaches.Setup;
begin
  inherited Setup;
  FCaches:=TObjectCaches.Create([doOwnsValues]);
end;

procedure TTestObjectCaches.TearDown;
begin
  FreeAndNil(FCaches);
  inherited TearDown;
end;

procedure TTestObjectCaches.TestHookup;
begin
  AssertNotNull('Have caches',Caches);
end;

procedure TTestObjectCaches.TestAdd;
begin
  Caches.AddObjectCache(TCachedObject);
  AssertEquals('Count',1,Caches.Count);
end;

procedure TTestObjectCaches.TestGetValue;

var
  C : TObjectCache;

begin
  TestAdd;
  AssertFalse('Get cache (nok)',Caches.TryGetValue(TComponent,C));
  AssertTrue('Get cache (ok)',Caches.TryGetValue(TCachedObject,C));
  AssertEquals('Count',1,Caches.Count);
end;

{ TTestThreading }

procedure TTestThreading.TestHookUp;
begin
  AssertNotNull('Have Default',TThreadPool.Default);
  AssertNotNull('Have current',TThreadPool.Current);
  AssertNotNull('Have instance',FThreadPool);
end;

procedure TTestThreading.TestCurrentOutsideTask;
begin
  AssertSame('Current is default outside task',TThreadPool.Default,TThreadPool.Current);
end;

procedure TTestThreading.TestSetMaxWorkerThreads;

var
  C : Integer;

begin
  C:=FThreadPool.MaxWorkerThreads;
  try
    AssertFalse('No zero',FThreadPool.SetMaxWorkerThreads(0));
    AssertFalse('Bigger than min',FThreadPool.SetMaxWorkerThreads(FThreadPool.MinWorkerThreads));
    AssertTrue('Big value',FThreadPool.SetMaxWorkerThreads(256));
  finally
    FThreadPool.SetMaxWorkerThreads(C);
  end;
end;

procedure TTestThreading.TestSetMinWorkerThreads;

var
  C : Integer;

begin
  C:=FThreadPool.MinWorkerThreads;
  try
    AssertFalse('No negative',FThreadPool.SetMinWorkerThreads(-1));
    AssertFalse('Smaller than max',FThreadPool.SetMinWorkerThreads(FThreadPool.MaxWorkerThreads+1));
    AssertTrue('zero',FThreadPool.SetMinWorkerThreads(0));
  finally
    FThreadPool.SetMinWorkerThreads(C);
  end;
end;

procedure TTestThreading.DoSimpleExecute(Sender : TObject);

begin
  ThreadCheckSame('Sender',Self,Sender);
  ThreadTerminated(TThread.CurrentThread); // Will reduce count
end;

var
  _PoolWorkDone : Integer;
  _SlowTerminateCalls : Integer;

procedure TTestThreading.DoCountWork(Sender: TObject);
begin
  Sleep(5);
  AtomicIncrement(_PoolWorkDone);
end;

// Number of OS threads of this process, -1 if unknown.
function LiveThreadCount: Integer;
{$IFDEF LINUX}
var
  Info : TSearchRec;
begin
  Result:=0;
  if FindFirst('/proc/self/task/*',faDirectory,Info)=0 then
    try
      repeat
        if (Info.Name<>'.') and (Info.Name<>'..') then
          Inc(Result);
      until FindNext(Info)<>0;
    finally
      FindClose(Info);
    end;
end;
{$ELSE}
begin
  Result:=-1;
end;
{$ENDIF}

procedure TTestThreading.DoSlowThreadTerminate(arg: TThread);
begin
  // Keep the terminating worker alive a little longer, after it left the
  // pool's thread list: Destroy must still wait for it. Longer than the
  // monitor tick (500 ms), which otherwise hides the problem.
  AtomicIncrement(_SlowTerminateCalls);
  Sleep(1500);
end;

procedure TTestThreading.TestFreeJoinsThreads;
// Regression test: worker and monitor threads were FreeOnTerminate and left
// the pool's list in their destructor, so Destroy returned while they were
// still running their destructors and the RTL thread epilogue. At program
// exit this raced with unit finalization (memory manager shutdown).
// OnThreadTerminate runs after the thread left the list, so a slow handler
// makes the window deterministic.
const
  Rounds = 3;
  Items = 32;
var
  Pool : TThreadPool;
  R, I, Before, After : Integer;
  Start : QWord;
begin
  Before:=LiveThreadCount;
  if Before<0 then
    Ignore('Thread count not available on this platform');
  For R:=1 to Rounds do
    begin
    _PoolWorkDone:=0;
    _SlowTerminateCalls:=0;
    Pool:=TThreadPool.Create;
    try
      Pool.OnThreadTerminate:=@DoSlowThreadTerminate;
      For I:=1 to Items do
        Pool.QueueWorkItem(Self,@DoCountWork);
      Start:=GetTickCount64;
      While (_PoolWorkDone<Items) and (GetTickCount64-Start<10000) do
        Sleep(1);
      AssertEquals('All work done',Items,_PoolWorkDone);
    finally
      Pool.Free;
    end;
    AssertTrue('OnThreadTerminate called',_SlowTerminateCalls>0);
    // No sleep: the threads must already be gone.
    After:=LiveThreadCount;
    AssertTrue(Format('Round %d: %d threads before, %d after Free',[R,Before,After]),
               After<=Before);
    end;
end;

type
  TCheckingThread = class(TThread)
  private
    FTest : TTestThreading;
  protected
    procedure Execute; override;
  public
    constructor Create(aTest : TTestThreading);
  end;

constructor TCheckingThread.Create(aTest: TTestThreading);
begin
  FTest:=aTest;
  inherited Create(False);
end;

procedure TCheckingThread.Execute;
begin
  FTest.ThreadCheck(True,'Must not be recorded');
  FTest.ThreadCheck(False,'Expected failure');
end;

procedure TTestThreading.TestThreadCheckRecordsFailure;

var
  T : TCheckingThread;
  Recorded : String;

begin
  T:=TCheckingThread.Create(Self);
  try
    T.WaitFor;
  finally
    T.Free;
  end;
  FLock.Enter;
  try
    Recorded:=FCheckFailures.Text;
    // Clear it, so TearDown does not fail this test.
    FCheckFailures.Clear;
  finally
    FLock.Leave;
  end;
  AssertTrue('Failure recorded: '+Recorded,Pos('Expected failure',Recorded)>0);
  AssertTrue('Passed check not recorded',Pos('Must not be recorded',Recorded)=0);
end;

procedure TTestThreading.WaitForWorkDoneCount(out Done: Boolean);
begin
  Done:=(WorkCount>0) and (WorkDone>=WorkCount);
{$IFDEF DEBUGTEST}
  Writeln('Done:=(',WorkCount,'>0) and (',WorkDone,'>=',WorkCount,') -> ',Done);
{$ENDIF}
end;

procedure TTestThreading.DoThreadTerminate(arg: TThread);
begin
  AtomicIncrement(FThreadsTerminated);
end;

procedure TTestThreading.DoThreadStart(arg: TThread);
begin
  AtomicIncrement(FThreadsStarted);
end;

procedure TTestThreading.DoBurnCyclesExecute(Sender : TObject);

var
  Cycles : Integer;
  I,J,K,BurnCount : Integer;
  T : TDateTime;
begin
{$IFDEF DEBUGTEST}
  Writeln('Thread ',TThread.CurrentThread.ThreadID,': Starting');
{$ENDIF}
  ThreadCheckSame('Sender',Self,Sender);
  T:=Now;
  Cycles:=10+ThreadRandom(2);
  For I:=1 to Cycles do
    begin
    BurnCount:=100000*(1+ThreadRandom(5));
    For J:=1 to BurnCount do
      if (J and 1)=1 then
        K:=K+J
      else
        K:=K-J;
    Sleep(10+ThreadRandom(10));
    end;
{$IFDEF DEBUGTEST}
  Writeln('Thread ',TThread.CurrentThread.ThreadID,': worked milliseconds ',MillisecondsBetween(Now,T));
{$ENDIF}
  // ThreadTerminated(TThread.CurrentThread); // Will reduce count
  AtomicIncrement(FWorkDone);
{$IFDEF DEBUGTEST}
  Writeln('Thread ',TThread.CurrentThread.ThreadID,': Work Done ',FTerminatedCount) ;
{$ENDIF}
end;


procedure TTestThreading.TestExecuteWork;
begin
  FWaitTerminatedCount:=1;
  FThreadPool.QueueWorkItem(Self,@DoSimpleExecute);
  AssertTrue('Task executed',WaitForCondition(@WaitForTerminateCount,500));
end;

procedure TTestThreading.TestExecuteLotsOfWork;


var
  i, Count : Integer;
{$IFDEF DEBUGTEST}
  T : TDateTime;
{$ENDIF}

begin
  Count:=TThread.ProcessorCount*2;
  WorkCount:=Count;
{$IFDEF DEBUGTEST}
  T:=Now;
{$ENDIF}
  For I:=1 to Count do
    begin
{$IFDEF DEBUGTEST}
    Writeln('Main loop queueing work item ',I,'/',count);
{$ENDIF}
    FThreadPool.QueueWorkItem(Self,@DoBurnCyclesExecute);
{$IFDEF DEBUGTEST}
    Writeln('Main loop sleep ',I,'/',count);
{$ENDIF}
    Sleep(4);
{$IFDEF DEBUGTEST}
    Writeln('Main loop wake ',I,'/',count);
{$ENDIF}
    end;
  AssertTrue('Tasks executed',WaitForCondition(@WaitForWorkDoneCount,10000));
{$IFDEF DEBUGTEST}
  Writeln('Milliseconds ',MillisecondsBetween(Now,T));
{$ENDIF}
  FreeAndNil(FThreadPool);
  AssertEquals('Threads all stopped',FThreadsStarted,FThreadsTerminated);
end;

procedure TTestThreading.TestFreeWithNewThreads;

var
  Pool : TThreadPool;

begin
  Pool:=FThreadPool;
  FThreadPool:=Nil;
  FWaitTerminatedCount:=1;
  TFreePoolThread.Create(Pool,@ThreadTerminated);
  AssertTrue('Pool freed',WaitForCondition(@WaitForTerminateCount,5000));
  AssertEquals('Threads all stopped',FThreadsStarted,FThreadsTerminated);
end;

procedure TTestThreading.SetUp;
begin
  Inherited;
  FThreadPool:=TThreadPool.Create;
  FThreadPool.OnThreadStart:=@DoThreadStart;
  FThreadPool.OnThreadTerminate:=@DoThreadTerminate;
  FThreadsTerminated:=0;
  FThreadsStarted:=0;
end;

procedure TTestThreading.TearDown;
begin
  FreeAndNil(FThreadPool);
  Inherited;
end;

{ TTestTask }

procedure TTestTask.OnTask(Sender : TObject);

begin
  // Runs in a pool thread.
  ThreadCheckSame('Sender',Self,Sender);
  ThreadCheck(FTask=TTask.CurrentTask,'Current task');
//  Writeln('FTask.Status = ',FTask.Status,', current : ',TTask.CurrentTask.Status); //TTaskStatus.Running
  ThreadCheck(TTask.CurrentTask.Status=TTaskStatus.Running,'Task status');
  if FRaise then
    Raise ESomeThing.Create('MrDo');
  FWorkExecuted:=True;
end;

procedure TTestTask.SetUp;
begin
  inherited SetUp;
  CreateTask;
end;

procedure TTestTask.CreateTask;
begin
  FTask:=TTask.Create(Self,@OnTask);
  FWorkExecuted:=False;
  FRaise:=False;
end;

// Creates the task in a pool whose only worker thread is busy until ReleaseBlockedPool
procedure TTestTask.CreateBlockedTask;

begin
  FBlockStarted:=TEvent.Create;
  FBlockRelease:=TEvent.Create;
  FBlockedPool:=TThreadPool.Create;
  AssertTrue('Set minimum threads',FBlockedPool.SetMinWorkerThreads(0));
  AssertTrue('Set maximum threads',FBlockedPool.SetMaxWorkerThreads(1));
  TTask.Run(Self,@DoBlockWork,FBlockedPool);
  AssertTrue('Worker thread blocked',FBlockStarted.WaitFor(5000)=wrSignaled);
  FTask:=TTask.Create(Self,@OnTask,FBlockedPool);
end;


// Lets the worker thread of the blocked pool continue
procedure TTestTask.ReleaseBlockedPool;

begin
  if Assigned(FBlockRelease) then
    FBlockRelease.SetEvent;
end;


procedure TTestTask.DoBlockWork(Sender: TObject);

begin
  FBlockStarted.SetEvent;
  FBlockRelease.WaitFor(5000);
end;


procedure TTestTask.TearDown;
begin
  ReleaseBlockedPool;
  FTask:=Nil;
  FreeAndNil(FBlockedPool);
  FreeAndNil(FBlockStarted);
  FreeAndNil(FBlockRelease);
  inherited TearDown;
end;

procedure TTestTask.TestHookup;
begin
  AssertNotNull('Have task',Task);
  AssertFalse('Work not executed',FWorkExecuted);
end;

procedure TTestTask.TestId;

var
  aID : Integer;

begin
  aID:=Task.Id;
  AssertTrue('Have ID',aID>0);
  CreateTask;
  AssertTrue('Have new ID',Task.ID<>aID);
end;

procedure TTestTask.TestStatus;
begin
  AssertTrue('Status created',Task.Status=TTaskStatus.Created);
end;

procedure TTestTask.TestShouldExecute;
begin
  AssertFalse('Should execute',Task.ShouldExecute);
end;

procedure TTestTask.TestExecuteWork;
begin
  Task.ExecuteWork;
  AssertTrue('Work executed',FWorkExecuted);
  AssertTrue('Status',Task.Status=TTaskStatus.Completed);
end;

procedure TTestTask.TestExecuteWorkException;

begin
  FRaise:=True;
  Task.ExecuteWork;
  AssertFalse('Work executed',FWorkExecuted);
  AssertTrue('Status',Task.Status=TTaskStatus.Exception);
end;

procedure TTestTask.TestWaitCardinal;
begin
  TStartTaskThread.Create(Task,200);
  AssertTrue('Wait OK',Task.Wait(400));
  AssertTrue('Work executed',FWorkExecuted);
end;

procedure TTestTask.TestWaitTimeSpan;
var
  T: TTimespan;
begin
  TStartTaskThread.Create(Task,200);
  T:=TTimeSpan.Create(0,0,0,0,400);
  AssertTrue('Wait OK',Task.Wait(T));
  AssertTrue('Work executed',FWorkExecuted);
end;

procedure TTestTask.TestCancel;
begin
  CreateBlockedTask;
  Task.Start;
  AssertTrue('Status',Task.Status>=TTaskStatus.WaitingToRun);
  Task.Cancel;
  AssertFalse('Work executed',FWorkExecuted);
end;

procedure TTestTask.TestCheckCanceled;
begin
  CreateBlockedTask;
  Task.Start;
  AssertTrue('Status',Task.Status>=TTaskStatus.WaitingToRun);
  Task.Cancel;
  AssertException('Cancel raises',EOperationCancelled,@CheckTaskCanceled);
end;

procedure TTestTask.TestStart;
begin
  Task.Start;
  AssertTrue('Status',Task.Status>=TTaskStatus.WaitingToRun);
  Task.Wait;
  AssertTrue('Work executed',FWorkExecuted);
end;

procedure TTestTask.TestStartTwice;
begin
  Task.Start;
  AssertTrue('Status',Task.Status>=TTaskStatus.WaitingToRun);
  Task.Wait;
  AssertTrue('Work executed',FWorkExecuted);
  AssertException('Cannot start twice',EInvalidOperation,@StartTask);
end;

procedure TTestTask.WaitForTask;

begin
  Task.Wait;
end;

procedure TTestTask.StartTask;

begin
  Task.Start;
end;

procedure TTestTask.CheckTaskCanceled;

begin
  Task.CheckCanceled;
end;


procedure TTestTask.TestStartException;
begin
  CreateBlockedTask;
  FRaise:=true;
  Task.Start;
  AssertTrue('Status',Task.Status=TTaskStatus.WaitingToRun);
  ReleaseBlockedPool;
  AssertException('Exception',EAggregateException,@WaitForTask);
  AssertFalse('Work executed',FWorkExecuted);
end;

function CalcInteger : Integer;

begin
  Sleep(40);
  Result:=42;
end;

procedure TTestTask.TestFuture;

begin
  AssertEquals('Calc future',42,(TTask.Specialize Future<Integer>(@CalcInteger)).Value)
end;

function TTestTask.CalcIntegerEvent(Sender : TObject) : Integer;

begin
//  Writeln('Here');
  Sleep(40);
  ThreadCheckSame('Sender',self,Sender); // Runs in a pool thread
  Result:=43;
//  Writeln('Here 2');
end;

procedure TTestTask.TestFutureEvent;
begin
  AssertEquals('Calc future',43,(TTask.Specialize Future<Integer>(Self,@CalcIntegerEvent)).Value)
end;

procedure TTestTask.DoQuickWork(Sender: TObject);

begin
  Sleep(10);
end;


procedure TTestTask.DoSlowWork(Sender: TObject);

begin
  Sleep(500);
end;


procedure TTestTask.DoRaiseWork(Sender: TObject);

begin
  Raise ESomeThing.Create('MrDo');
end;


procedure TTestTask.TestWaitForAllCompleted;

var
  lTasks : array of ITask;

begin
  lTasks:=[TTask.Run(Self,@DoQuickWork),TTask.Run(Self,@DoQuickWork)];
  lTasks[0].Wait;
  lTasks[1].Wait;
  AssertTrue('All tasks complete',TTask.WaitForAll(lTasks));
  AssertTrue('All tasks complete with timeout',TTask.WaitForAll(lTasks,100));
end;


procedure TTestTask.TestWaitForAllMixed;

var
  lTasks : array of ITask;

begin
  lTasks:=[TTask.Run(Self,@DoQuickWork),TTask.Run(Self,@DoSlowWork)];
  lTasks[0].Wait;
  AssertTrue('All tasks complete',TTask.WaitForAll(lTasks));
  AssertTrue('Slow task status',lTasks[1].Status=TTaskStatus.Completed);
end;


procedure TTestTask.TestWaitForAllTimeout;

var
  lTasks : array of ITask;
  lStart : QWord;

begin
  lTasks:=[TTask.Run(Self,@DoQuickWork),TTask.Run(Self,@DoSlowWork)];
  lStart:=GetTickCount64;
  AssertFalse('Timeout expires',TTask.WaitForAll(lTasks,50));
  AssertTrue('Returns before slow task completes',GetTickCount64-lStart<400);
  AssertTrue('All tasks complete',TTask.WaitForAll(lTasks));
end;


procedure TTestTask.TestWaitForAllException;

var
  lTasks : array of ITask;
  lRaised : Boolean;
  lTries : Integer;

begin
  lTasks:=[TTask.Run(Self,@DoRaiseWork)];
  lTries:=0;
  While (lTasks[0].Status<>TTaskStatus.Exception) and (lTries<200) do
    begin
    Sleep(10);
    Inc(lTries);
    end;
  AssertTrue('Task status',lTasks[0].Status=TTaskStatus.Exception);
  lRaised:=False;
  try
    TTask.WaitForAll(lTasks);
  except
    on E : EAggregateException do
      lRaised:=True;
  end;
  AssertTrue('Exception raised',lRaised);
end;


procedure TTestTask.TestWaitForAny;

var
  lTasks : array of ITask;

begin
  lTasks:=[TTask.Run(Self,@DoSlowWork),TTask.Run(Self,@DoQuickWork)];
  AssertEquals('Quick task completes first',1,TTask.WaitForAny(lTasks));
  AssertTrue('All tasks complete',TTask.WaitForAll(lTasks));
end;


procedure TTestTask.TestWaitForAnyCompleted;

var
  lTasks : array of ITask;

begin
  lTasks:=[TTask.Run(Self,@DoSlowWork),TTask.Run(Self,@DoQuickWork)];
  lTasks[1].Wait;
  AssertEquals('Complete task',1,TTask.WaitForAny(lTasks,100));
  AssertTrue('All tasks complete',TTask.WaitForAll(lTasks));
end;


procedure TTestTask.TestWaitForAnyTimeout;

var
  lTasks : array of ITask;
  lStart : QWord;

begin
  lTasks:=[TTask.Run(Self,@DoSlowWork)];
  lStart:=GetTickCount64;
  AssertEquals('Timeout expires',-1,TTask.WaitForAny(lTasks,50));
  AssertTrue('Returns before slow task completes',GetTickCount64-lStart<400);
  AssertTrue('All tasks complete',TTask.WaitForAll(lTasks));
end;


{ TTestTask.TTaskThread }

constructor TTestTask.TTaskThread.Create(aTask: ITask; aSleep: Integer);
begin
  FTask:=aTask;
  FSleep:=aSleep;
  FreeOnTerminate:=False; // Joined and freed in TThreadedTestCase.TearDown
  Inherited Create(False);
  RegisterTestThread(Self);
end;

procedure TTestTask.TTaskThread.Execute;

var
  OK : Boolean;

begin
  Sleep(FSleep);
  try
    OK:=False;
    DoTask(FTask);
    OK:=True;
  finally
    FTask:=Nil;
    // Writeln('Task done. No errors: ',OK);
  end;
end;

{ TTestTask.TStartTaskThread }

procedure TTestTask.TStartTaskThread.DoTask(aTask: ITask);
begin
  aTask.Start;
end;

{ TTestParallel }

procedure TTestParallel.SetUp;
begin
  inherited SetUp;
  FResults:=Default(TResultArray);
  _Results:=Default(TResultArray);
end;

procedure TTestParallel.TearDown;
begin
  inherited TearDown;
end;

procedure TTestParallel.CheckLocal;

var
  I : integer;

begin
  For I:=1 to 255 do
    AssertEquals('Element '+IntToStr(i),I,FResults[i]);
end;

procedure TTestParallel.TestHookup;

var
  I : integer;

begin
  For I:=1 to 255 do
    AssertEquals('Element '+IntToStr(i),0,FResults[i]);
  For I:=1 to 255 do
    AssertEquals('GLobal Element '+IntToStr(i),0,_Results[i]);
end;

procedure TTestParallel.DoEvent(aSender: TObject; aIndex: Integer);

begin
//  Writeln(TThread.CurrentThread.ThreadID,' EventIdx ',aIndex);
  Sleep(50+(10*(1+ThreadRandom(5))));
  FResults[aIndex]:=aIndex;
end;

procedure TTestParallel.DoEvent64(aSender: TObject; aIndex: Int64);
begin
  Sleep(50+(10*(1+ThreadRandom(5))));
  FResults[aIndex]:=aIndex;
end;

procedure TTestParallel.TestForEvent;

var
  L : TParallel.TLoopResult;

begin
  L:=TParallel.&For(Self,1,1,255,@DoEvent);
  AssertTrue('Correct result',L.Completed);
  CheckLocal;
end;

{$IFDEF CPU64}
procedure TTestParallel.TestForEvent64;
var
  L : TParallel.TLoopResult;

begin
  L:=TParallel.&For(Self,1,1,255,@DoEvent64);
  AssertTrue('Correct result',L.Completed);
  CheckLocal;
end;
{$ENDIF}

procedure TTestParallel.DoSleepEvent(aSender: TObject; aIndex: Integer);
begin
  Sleep(20);
  FResults[aIndex]:=aIndex;
end;

procedure TTestParallel.TestForColdPoolParallel;
// Regression test: on a freshly created pool the loop replicas are pushed on
// the local queue of the worker running the root task. Idle workers only looked
// at other local queues after a wake-up signal, which is not sent while the pool
// is still starting threads, so the root task ran all iterations by itself.
const
  Iterations = 48;
  IterationTime = 20;
  Rounds = 5;
var
  Pool : TThreadPool;
  L : TParallel.TLoopResult;
  R, I : Integer;
  Start, Elapsed : QWord;
begin
  if TThread.ProcessorCount<2 then
    Ignore('Needs at least 2 CPUs');
  For R:=1 to Rounds do
    begin
    FillChar(FResults,SizeOf(FResults),0);
    Pool:=TThreadPool.Create;
    try
      Start:=GetTickCount64;
      L:=TParallel.&For(Self,1,1,Iterations,@DoSleepEvent,Pool);
      Elapsed:=GetTickCount64-Start;
    finally
      Pool.Free;
    end;
    AssertTrue('Completed',L.Completed);
    For I:=1 to Iterations do
      AssertEquals('Element '+IntToStr(I),I,FResults[I]);
    AssertTrue(Format('Round %d ran (nearly) sequentially: %d ms',[R,Elapsed]),
               Elapsed<(Iterations*IterationTime) div 2);
    end;
end;

const
  CountIterations = 200000;
  CountRounds = 200;

var
  // One counter per iteration; must be exactly 1 after each loop.
  _Counts : array of Integer;

procedure TTestParallel.DoCountEvent(aSender: TObject; aIndex: Integer);
begin
  AtomicIncrement(_Counts[aIndex]);
end;

procedure TTestParallel.DoCountEvent64(aSender: TObject; aIndex: Int64);
begin
  AtomicIncrement(_Counts[aIndex]);
end;

procedure TTestParallel.CheckCountedOnce(const aMsg: String);
var
  I : Integer;
begin
  For I:=0 to CountIterations-1 do
    if _Counts[I]<>1 then
      Fail(Format('%s: iteration %d executed %d times',[aMsg,I,_Counts[I]]));
end;

procedure TTestParallel.TestForEachIterationOnce;
// Regression test: a worker claimed a range using one stride value and then
// executed a range computed with another one, read separately while other
// workers were growing the stride. Iterations got lost or ran twice.
var
  R : Integer;
  L : TParallel.TLoopResult;
begin
  SetLength(_Counts,CountIterations);
  For R:=1 to CountRounds do
    begin
    FillChar(_Counts[0],CountIterations*SizeOf(Integer),0);
    L:=TParallel.&For(Self,0,CountIterations-1,@DoCountEvent);
    AssertTrue('Completed',L.Completed);
    CheckCountedOnce('Round '+IntToStr(R));
    end;
end;

{$IFDEF CPU64}
procedure TTestParallel.TestForEachIterationOnce64;
var
  R : Integer;
  L : TParallel.TLoopResult;
begin
  SetLength(_Counts,CountIterations);
  For R:=1 to CountRounds do
    begin
    FillChar(_Counts[0],CountIterations*SizeOf(Integer),0);
    L:=TParallel.&For(Self,Int64(0),Int64(CountIterations-1),@DoCountEvent64);
    AssertTrue('Completed',L.Completed);
    CheckCountedOnce('Round '+IntToStr(R));
    end;
end;
{$ENDIF}

var
  _StateHits : array of Integer;

procedure TTestParallel.TestForWithState;
// Regression test: the TLoopState passed to the iteration was never created
// (nil), so every loop with a state parameter raised access violations.
var
  L : TParallel.TLoopResult;
  I : Integer;
begin
  SetLength(_StateHits,1000);
  FillChar(_StateHits[0],Length(_StateHits)*SizeOf(Integer),0);
  L:=TParallel.&For(0,999,
    procedure(aIndex: Integer; aState: TParallel.TLoopState)
    begin
      if Assigned(aState) and not aState.ShouldExit then
        AtomicIncrement(_StateHits[aIndex]);
    end);
  AssertTrue('Completed',L.Completed);
  For I:=0 to 999 do
    AssertEquals('Iteration '+IntToStr(I),1,_StateHits[I]);
  {$IFDEF CPU64}
  FillChar(_StateHits[0],Length(_StateHits)*SizeOf(Integer),0);
  L:=TParallel.&For(Int64(0),Int64(999),
    procedure(aIndex: Int64; aState: TParallel.TLoopState)
    begin
      if Assigned(aState) and not aState.ShouldExit then
        AtomicIncrement(_StateHits[aIndex]);
    end);
  AssertTrue('Completed 64',L.Completed);
  For I:=0 to 999 do
    AssertEquals('Iteration 64 '+IntToStr(I),1,_StateHits[I]);
  {$ENDIF}
end;

procedure TTestParallel.TestForBreak;
var
  L : TParallel.TLoopResult;
  I : Integer;
begin
  SetLength(_StateHits,10000);
  FillChar(_StateHits[0],Length(_StateHits)*SizeOf(Integer),0);
  L:=TParallel.&For(0,9999,
    procedure(aIndex: Integer; aState: TParallel.TLoopState)
    begin
      AtomicIncrement(_StateHits[aIndex]);
      if aIndex=100 then
        aState.Break;
    end);
  AssertFalse('Not completed',L.Completed);
  AssertEquals('Lowest break iteration',100,Integer(L.LowestBreakIteration));
  // Break guarantees that all iterations before the break index ran.
  For I:=0 to 100 do
    AssertEquals('Iteration '+IntToStr(I),1,_StateHits[I]);
end;

procedure TTestParallel.TestForStop;
var
  L : TParallel.TLoopResult;
  Ran, I : Integer;
begin
  SetLength(_StateHits,100000);
  FillChar(_StateHits[0],Length(_StateHits)*SizeOf(Integer),0);
  L:=TParallel.&For(0,99999,
    procedure(aIndex: Integer; aState: TParallel.TLoopState)
    begin
      AtomicIncrement(_StateHits[aIndex]);
      if aIndex=0 then
        aState.Stop;
      Sleep(0);
    end);
  AssertFalse('Not completed',L.Completed);
  AssertEquals('LowestBreakIteration is Null',varNull,TVarData(L.LowestBreakIteration).VType);
  Ran:=0;
  For I:=0 to High(_StateHits) do
    Inc(Ran,_StateHits[I]);
  AssertTrue('Stopped early: '+IntToStr(Ran)+' iterations ran',Ran<100000);
end;

procedure TTestParallel.TestForExceptionPreserved;
// Regression test: the iteration's exception was lost; the caller got an
// EAggregateException with Count=0.
var
  Raised : Boolean;
begin
  Raised:=False;
  try
    TParallel.&For(0,99,
      procedure(aIndex: Integer)
      begin
        if aIndex=42 then
          raise ESomeThing.Create('bad 42');
      end);
  except
    on E : EAggregateException do
      begin
      Raised:=True;
      AssertEquals('One inner exception',1,E.Count);
      AssertEquals('Inner class',ESomeThing.ClassName,E.InnerExceptions[0].ClassName);
      AssertEquals('Inner message','bad 42',E.InnerExceptions[0].Message);
      end;
  end;
  AssertTrue('EAggregateException raised',Raised);
end;

procedure TTestParallel.TestJoin;
// Regression test: TProcJoinTask/TEventJoinTask left Params uninitialized,
// set Params.Parent := Self (causing FSubTasks=2 and Wait to hang), and
// TParallel.Join did not start the returned task.
var
  A, B : Integer;
  T : ITask;
begin
  A:=0;
  B:=0;
  T:=TParallel.Join(
    procedure begin AtomicIncrement(A, 10); end,
    procedure begin AtomicIncrement(B, 20); end);
  AssertTrue('Join completed',T.Wait(5000));
  AssertEquals('Proc 1 ran',10,A);
  AssertEquals('Proc 2 ran',20,B);
end;

{ TTestSyncObjsPhase2 }

procedure TTestSyncObjsPhase2.TestCountdownEventBasic;
var
  CDE: TCountdownEvent;
  Raised: Boolean;
begin
  CDE := TCountdownEvent.Create(3);
  try
    AssertEquals('InitialCount', 3, CDE.InitialCount);
    AssertEquals('CurrentCount', 3, CDE.CurrentCount);
    AssertFalse('Not set initially', CDE.IsSet);
    AssertTrue('WaitFor(0) times out', CDE.WaitFor(0) = wrTimeout);

    CDE.AddCount(2);
    AssertEquals('CurrentCount after AddCount(2)', 5, CDE.CurrentCount);
    AssertTrue('TryAddCount(1)', CDE.TryAddCount(1));
    AssertEquals('CurrentCount after TryAddCount(1)', 6, CDE.CurrentCount);

    AssertFalse('Signal(3) does not set', CDE.Signal(3));
    AssertEquals('CurrentCount is 3', 3, CDE.CurrentCount);
    AssertTrue('Signal(3) sets event', CDE.Signal(3));
    AssertTrue('IsSet is True', CDE.IsSet);
    AssertTrue('WaitFor(0) succeeds', CDE.WaitFor(0) = wrSignaled);
    AssertFalse('TryAddCount fails when set', CDE.TryAddCount(1));

    Raised := False;
    try
      CDE.AddCount(1);
    except
      on E: ESyncObjectException do
        Raised := True;
    end;
    AssertTrue('AddCount raises when set', Raised);

    CDE.Reset(2);
    AssertEquals('Reset InitialCount', 2, CDE.InitialCount);
    AssertEquals('Reset CurrentCount', 2, CDE.CurrentCount);
    AssertFalse('Reset clears IsSet', CDE.IsSet);
    AssertTrue('Signal(2) sets after reset', CDE.Signal(2));
  finally
    CDE.Free;
  end;
end;

procedure TTestSyncObjsPhase2.TestCountdownEventParallel;
const
  WorkerCount = 32;
var
  CDE: TCountdownEvent;
  Sum: Integer;
  I: Integer;
begin
  Sum := 0;
  CDE := TCountdownEvent.Create(1);
  try
    for I := 1 to WorkerCount do
      begin
      CDE.AddCount(1);
      TTask.Run(
        procedure
        begin
          AtomicIncrement(Sum, 1);
          CDE.Signal;
        end);
      end;
    // Release initial registration token
    CDE.Signal;
    AssertTrue('Countdown completed within 5s', CDE.WaitFor(5000) = wrSignaled);
    AssertEquals('All workers contributed before WaitFor returned', WorkerCount, Sum);
  finally
    CDE.Free;
  end;
end;

procedure TTestSyncObjsPhase2.TestBarrierMultiPhase;
const
  NumWorkers = 4;
  NumPhases = 6;
var
  Barrier: TBarrier;
  Scratch, PhaseTotals: array[0..NumWorkers - 1] of Integer;
  CallbacksRun, NextIdx: Integer;
  Tasks: array[0..NumWorkers - 1] of ITask;
  W: Integer;
begin
  FillChar(Scratch, SizeOf(Scratch), 0);
  FillChar(PhaseTotals, SizeOf(PhaseTotals), 0);
  CallbacksRun := 0;
  NextIdx := 0;

  Barrier := TBarrier.Create(NumWorkers,
    procedure(aBarrier: TBarrier)
    begin
      Inc(CallbacksRun);
    end);
  try
    for W := 0 to NumWorkers - 1 do
      begin
      Tasks[W] := TTask.Run(
        procedure
        var
          WorkerIdx, P, J, S: Integer;
        begin
          WorkerIdx := AtomicIncrement(NextIdx) - 1;
          for P := 1 to NumPhases do
            begin
            Scratch[WorkerIdx] := P * (WorkerIdx + 1);
            Barrier.SignalAndWait;
            S := 0;
            for J := 0 to NumWorkers - 1 do
              Inc(S, Scratch[J]);
            PhaseTotals[WorkerIdx] := S;
            Barrier.SignalAndWait;
            end;
        end);
      end;

    AssertTrue('All barrier tasks completed', TTask.WaitForAll(Tasks, 10000));
    AssertEquals('Post-phase callback count', NumPhases * 2, CallbacksRun);
    AssertEquals('CurrentPhaseNumber', NumPhases * 2, Barrier.CurrentPhaseNumber);
    // Sum of (WorkerIdx+1) for 0..3 is 1+2+3+4 = 10; multiplied by NumPhases (6) = 60
    for W := 0 to NumWorkers - 1 do
      AssertEquals('Worker total after last phase', NumPhases * 10, PhaseTotals[W]);
  finally
    Barrier.Free;
  end;
end;

procedure TTestSyncObjsPhase2.TestBarrierAddRemoveParticipant;
var
  Barrier: TBarrier;
  T: ITask;
  Phase0Done: Integer;
begin
  Phase0Done := 0;
  Barrier := TBarrier.Create(1);
  try
    AssertEquals('Start with 1 participant', 1, Barrier.ParticipantCount);
    AssertEquals('AddParticipant returns phase 0', 0, Barrier.AddParticipant);
    AssertEquals('Now 2 participants', 2, Barrier.ParticipantCount);

    T := TTask.Run(
      procedure
      begin
        AtomicIncrement(Phase0Done);
        Barrier.SignalAndWait;
      end);

    AssertTrue('Main + worker finish phase 0', Barrier.SignalAndWait(5000));
    AssertTrue('Task finished', T.Wait(5000));
    AssertEquals('Phase0Done=1', 1, Phase0Done);
    AssertEquals('Phase is now 1', 1, Barrier.CurrentPhaseNumber);

    Barrier.RemoveParticipant;
    AssertEquals('Back to 1 participant', 1, Barrier.ParticipantCount);
    AssertTrue('Single participant advances phase immediately', Barrier.SignalAndWait(1000));
    AssertEquals('Phase is now 2', 2, Barrier.CurrentPhaseNumber);
  finally
    Barrier.Free;
  end;
end;

procedure TTestSyncObjsPhase2.TestBarrierPostPhaseException;
var
  Barrier: TBarrier;
  T: ITask;
  CaughtMain, CaughtWorker: Integer;
begin
  CaughtMain := 0;
  CaughtWorker := 0;
  Barrier := TBarrier.Create(2,
    procedure(aBarrier: TBarrier)
    begin
      raise ESomeThing.Create('post-phase boom');
    end);
  try
    T := TTask.Run(
      procedure
      begin
        try
          Barrier.SignalAndWait(5000);
        except
          on E: EBarrierPostPhaseException do
            AtomicIncrement(CaughtWorker);
        end;
      end);

    try
      Barrier.SignalAndWait(5000);
    except
      on E: EBarrierPostPhaseException do
        begin
        Inc(CaughtMain);
        AssertTrue('Exception message contains cause', Pos('post-phase boom', E.Message) > 0);
        end;
    end;

    AssertTrue('Worker finished', T.Wait(5000));
    AssertEquals('Main caught EBarrierPostPhaseException', 1, CaughtMain);
    AssertEquals('Worker caught EBarrierPostPhaseException', 1, CaughtWorker);
    AssertEquals('Phase still advanced to 1', 1, Barrier.CurrentPhaseNumber);
  finally
    Barrier.Free;
  end;
end;

procedure TTestSyncObjsPhase2.TestSlimReaderWriterLockConcurrent;
const
  NumReaders = 6;
  NumWriters = 2;
  Iterations = 800;
var
  RWLock: TSlimReaderWriterLock;
  SharedA, SharedB: Integer;
  Tasks: array[0..NumReaders + NumWriters - 1] of ITask;
  I: Integer;
begin
  SharedA := 1000;
  SharedB := -1000;
  RWLock := TSlimReaderWriterLock.Create;
  try
    // Verify basic non-contended TryBeginRead / TryBeginWrite
    AssertTrue('TryBeginRead', RWLock.TryBeginRead(0));
    AssertTrue('Second TryBeginRead', RWLock.TryBeginRead(0));
    AssertEquals('ReaderCount is 2', 2, RWLock.ReaderCount);
    AssertFalse('TryBeginWrite fails while readers active', RWLock.TryBeginWrite(0));
    RWLock.EndRead;
    RWLock.EndRead;

    AssertTrue('TryBeginWrite succeeds when unlocked', RWLock.TryBeginWrite(0));
    AssertTrue('IsWriteLocked', RWLock.IsWriteLocked);
    AssertFalse('TryBeginRead fails while write locked', RWLock.TryBeginRead(0));
    RWLock.EndWrite;

    for I := 0 to NumReaders - 1 do
      Tasks[I] := TTask.Run(
        procedure
        var
          K, Sum: Integer;
          G: TSlimReadLockGuard;
        begin
          for K := 1 to Iterations do
            begin
            G.Init(RWLock);
            Sum := SharedA + SharedB;
            G.Release;
            if Sum <> 0 then
              ThreadCheck(False, 'Invariant SharedA + SharedB = 0 violated: ' + IntToStr(Sum));
            end;
        end);

    for I := 0 to NumWriters - 1 do
      Tasks[NumReaders + I] := TTask.Run(
        procedure
        var
          K: Integer;
          G: TSlimWriteLockGuard;
        begin
          for K := 1 to Iterations do
            begin
            G.Init(RWLock);
            Inc(SharedA, 7);
            Dec(SharedB, 7);
            G.Release;
            end;
        end);

    AssertTrue('Readers and writers completed', TTask.WaitForAll(Tasks, 15000));
    AssertEquals('Final SharedA + SharedB = 0', 0, SharedA + SharedB);
    AssertEquals('Final SharedA value', 1000 + NumWriters * Iterations * 7, SharedA);
  finally
    RWLock.Free;
  end;
end;

procedure TTestSyncObjsPhase2.TestChannelUnboundedMultiProducer;
type
  TIntChannel = specialize TChannel<Integer>;
const
  NumProducers = 4;
  ItemsPerProducer = 250;
var
  Ch: TIntChannel;
  Producers: array[0..NumProducers - 1] of ITask;
  P, Item, CountReceived, NextBase: Integer;
  TotalSum, ExpectedSum: Int64;
  CloserTask: ITask;
begin
  NextBase := 0;
  Ch := TIntChannel.Create(0); // unbounded
  try
    for P := 0 to NumProducers - 1 do
      Producers[P] := TTask.Run(
        procedure
        var
          Base, K: Integer;
        begin
          Base := AtomicIncrement(NextBase) - 1;
          for K := 1 to ItemsPerProducer do
            Ch.Send(Base * ItemsPerProducer + K);
        end);

    CloserTask := TTask.Run(
      procedure
      begin
        TTask.WaitForAll(Producers);
        Ch.Close;
      end);

    CountReceived := 0;
    TotalSum := 0;
    for Item in Ch do
      begin
      Inc(CountReceived);
      Inc(TotalSum, Item);
      end;

    AssertTrue('CloserTask finished', CloserTask.Wait(5000));
    ExpectedSum := Int64(NumProducers * ItemsPerProducer) * (NumProducers * ItemsPerProducer + 1) div 2;
    AssertEquals('All items received via for-in', NumProducers * ItemsPerProducer, CountReceived);
    AssertEquals('Exact sum of all items', ExpectedSum, TotalSum);
    AssertTrue('Channel is closed', Ch.IsClosed);
  finally
    Ch.Free;
  end;
end;

procedure TTestSyncObjsPhase2.TestChannelBoundedBackpressure;
type
  TIntChannel = specialize TChannel<Integer>;
const
  TotalItems = 100;
var
  Ch: TIntChannel;
  Producer: ITask;
  I, Val: Integer;
  Raised: Boolean;
begin
  Ch := TIntChannel.Create(4); // bounded capacity = 4
  try
    AssertEquals('Capacity is 4', 4, Ch.Capacity);
    AssertTrue('TrySend 1', Ch.TrySend(10));
    AssertTrue('TrySend 2', Ch.TrySend(20));
    AssertTrue('TrySend 3', Ch.TrySend(30));
    AssertTrue('TrySend 4', Ch.TrySend(40));
    AssertFalse('TrySend(0ms) fails when full', Ch.TrySend(50, 0));

    AssertEquals('Receive 10', 10, Ch.Receive);
    AssertEquals('Receive 20', 20, Ch.Receive);
    AssertEquals('Receive 30', 30, Ch.Receive);
    AssertEquals('Receive 40', 40, Ch.Receive);
    AssertFalse('TryReceive(0ms) fails when empty', Ch.TryReceive(Val, 0));

    Producer := TTask.Run(
      procedure
      var
        K: Integer;
      begin
        for K := 1 to TotalItems do
          Ch.Send(K);
        Ch.Close;
      end);

    for I := 1 to TotalItems do
      begin
      AssertTrue('TryReceive item ' + IntToStr(I), Ch.TryReceive(Val, 5000));
      AssertEquals('FIFO order item ' + IntToStr(I), I, Val);
      end;

    AssertTrue('Producer finished', Producer.Wait(5000));
    AssertFalse('TryReceive on closed & drained channel returns False', Ch.TryReceive(Val, 0));

    Raised := False;
    try
      Ch.Send(999);
    except
      on E: EChannelClosedException do
        Raised := True;
    end;
    AssertTrue('Send on closed channel raises EChannelClosedException', Raised);

    Raised := False;
    try
      Ch.Receive;
    except
      on E: EChannelClosedException do
        Raised := True;
    end;
    AssertTrue('Receive on closed & empty channel raises EChannelClosedException', Raised);
  finally
    Ch.Free;
  end;
end;

{ TTestContinuationsAndCancellation }

procedure TTestContinuationsAndCancellation.TestCancellationTokenBasics;
var
  NoneTok: TCancellationToken;
  CTS1, CTS2, LinkedCTS, TimedCTS: TCancellationTokenSource;
  Reg1, Reg2: TCancellationTokenRegistration;
  CallbackCount: Integer;
  Raised: Boolean;
begin
  NoneTok := TCancellationToken.None;
  AssertFalse('None token CanBeCanceled is False', NoneTok.CanBeCanceled);
  AssertFalse('None token IsCancellationRequested is False', NoneTok.IsCancellationRequested);
  AssertTrue('None token equals TCancellationToken.None', NoneTok = TCancellationToken.None);

  CallbackCount := 0;
  CTS1 := TCancellationTokenSource.Create;
  try
    AssertTrue('CTS1.Token CanBeCanceled is True', CTS1.Token.CanBeCanceled);
    AssertFalse('CTS1.Token IsCancellationRequested initially False', CTS1.Token.IsCancellationRequested);
    AssertTrue('CTS1.Token <> NoneTok', CTS1.Token <> NoneTok);

    Reg1 := CTS1.Token.Register(
      procedure
      begin
        AtomicIncrement(CallbackCount);
      end);
    Reg2 := CTS1.Token.Register(
      procedure
      begin
        AtomicIncrement(CallbackCount, 10);
      end);
    // Unregister Reg2 before Cancel
    Reg2.Unregister;

    CTS1.Cancel;
    AssertTrue('CTS1.IsCancellationRequested is True after Cancel', CTS1.IsCancellationRequested);
    AssertTrue('CTS1.Token.IsCancellationRequested is True after Cancel', CTS1.Token.IsCancellationRequested);
    AssertEquals('Only Reg1 callback ran', 1, CallbackCount);
    AssertTrue('WaitHandle is signaled', CTS1.Token.WaitHandle.WaitFor(0) = wrSignaled);

    // Registering on already canceled token invokes callback immediately
    CTS1.Token.Register(
      procedure
      begin
        AtomicIncrement(CallbackCount, 100);
      end);
    AssertEquals('Immediate callback on canceled token ran', 101, CallbackCount);

    Raised := False;
    try
      CTS1.Token.ThrowIfCancellationRequested;
    except
      on E: EOperationCancelled do
        Raised := True;
    end;
    AssertTrue('ThrowIfCancellationRequested raised EOperationCancelled', Raised);

    // Reset produces a fresh non-canceled state
    CTS1.Reset;
    AssertFalse('After Reset, IsCancellationRequested is False', CTS1.IsCancellationRequested);
  finally
    CTS1.Free;
  end;

  // Test linked token source
  CTS1 := TCancellationTokenSource.Create;
  CTS2 := TCancellationTokenSource.Create;
  LinkedCTS := TCancellationTokenSource.CreateLinkedTokenSource([CTS1.Token, CTS2.Token]);
  try
    AssertFalse('LinkedCTS initially not canceled', LinkedCTS.IsCancellationRequested);
    CTS2.Cancel;
    AssertTrue('Canceling CTS2 cancels LinkedCTS', LinkedCTS.IsCancellationRequested);
    AssertFalse('CTS1 remains uncanceled', CTS1.IsCancellationRequested);
  finally
    LinkedCTS.Free;
    CTS2.Free;
    CTS1.Free;
  end;

  // Test CancelAfter
  TimedCTS := TCancellationTokenSource.Create(40);
  try
    AssertTrue('TimedCTS WaitHandle signals within 2000ms',
      TimedCTS.Token.WaitHandle.WaitFor(2000) = wrSignaled);
    AssertTrue('TimedCTS IsCancellationRequested is True', TimedCTS.IsCancellationRequested);
  finally
    TimedCTS.Free;
  end;
end;

procedure TTestContinuationsAndCancellation.TestContinueWithAndOptions;
var
  StepLog: Integer;
  T1, T2, T3: ITask;
  FaultTask, OnCompTask, OnFaultTask, OnCancelTask: ITask;
  CancelTask, COnComp, COnCancel: ITask;
  Fut: specialize IFuture<Integer>;
  FutCont: ITask;
  CapturedValue: Integer;
begin
  StepLog := 0;
  T1 := TTask.Run(
    procedure
    begin
      AtomicIncrement(StepLog, 1);
    end);
  T2 := T1.ContinueWith(
    procedure(aAntecedent: ITask)
    begin
      if aAntecedent.Status = TTaskStatus.Completed then
        AtomicIncrement(StepLog, 10);
    end);
  T3 := T2.ContinueWith(
    procedure(aAntecedent: ITask)
    begin
      if aAntecedent.Status = TTaskStatus.Completed then
        AtomicIncrement(StepLog, 100);
    end, [TTaskContinuationOption.ExecuteSynchronously]);

  AssertTrue('T3 chain completed', T3.Wait(5000));
  AssertEquals('All 3 stages executed in order', 111, StepLog);

  // Faulted antecedent with OnlyOnCompleted / OnlyOnFaulted / OnlyOnCanceled
  StepLog := 0;
  FaultTask := TTask.Run(
    procedure
    begin
      raise Exception.Create('Intentional fault');
    end);
  OnCompTask := FaultTask.ContinueWith(
    procedure(aAntecedent: ITask)
    begin
      if aAntecedent = nil then;
      AtomicIncrement(StepLog, 1);
    end, [TTaskContinuationOption.OnlyOnCompleted]);
  OnFaultTask := FaultTask.ContinueWith(
    procedure(aAntecedent: ITask)
    begin
      if aAntecedent.Status = TTaskStatus.Exception then
        AtomicIncrement(StepLog, 10);
    end, [TTaskContinuationOption.OnlyOnFaulted]);
  OnCancelTask := FaultTask.ContinueWith(
    procedure(aAntecedent: ITask)
    begin
      if aAntecedent = nil then;
      AtomicIncrement(StepLog, 100);
    end, [TTaskContinuationOption.OnlyOnCanceled]);

  AssertTrue('OnFaultTask completed', OnFaultTask.Wait(5000));
  AssertTrue('OnCompTask canceled', OnCompTask.Status = TTaskStatus.Canceled);
  AssertTrue('OnCancelTask canceled', OnCancelTask.Status = TTaskStatus.Canceled);
  AssertEquals('Only OnlyOnFaulted ran', 10, StepLog);

  // Canceled antecedent with OnlyOnCompleted / OnlyOnCanceled
  StepLog := 0;
  CancelTask := TTask.Create(
    procedure
    begin
      AtomicIncrement(StepLog, 1);
    end);
  COnComp := CancelTask.ContinueWith(
    procedure(aAntecedent: ITask)
    begin
      if aAntecedent = nil then;
      AtomicIncrement(StepLog, 10);
    end, [TTaskContinuationOption.OnlyOnCompleted]);
  COnCancel := CancelTask.ContinueWith(
    procedure(aAntecedent: ITask)
    begin
      if aAntecedent.Status = TTaskStatus.Canceled then
        AtomicIncrement(StepLog, 100);
    end, [TTaskContinuationOption.OnlyOnCanceled]);
  CancelTask.Cancel;
  AssertTrue('COnCancel completed after CancelTask.Cancel', COnCancel.Wait(5000));
  AssertTrue('COnComp was canceled', COnComp.Status = TTaskStatus.Canceled);
  AssertEquals('Only OnlyOnCanceled ran', 100, StepLog);

  // IFuture<T>.ContinueWith
  CapturedValue := 0;
  Fut := TTask.specialize Future<Integer>(
    function: Integer
    begin
      Result := 42;
    end);
  FutCont := Fut.ContinueWith(
    procedure(aFut: specialize IFuture<Integer>)
    begin
      CapturedValue := aFut.Value * 2;
    end);
  AssertTrue('FutCont completed', FutCont.Wait(5000));
  AssertEquals('FutCont captured 84', 84, CapturedValue);
end;

procedure TTestContinuationsAndCancellation.TestThenByPipeline;
var
  StartTask: ITask;
  Stage1, Stage2, Stage3: specialize IFuture<Integer>;
  FinalStage: specialize IFuture<string>;
  FaultStage1: specialize IFuture<Integer>;
  FaultStage2: specialize IFuture<Integer>;
  FaultStage3: specialize IFuture<string>;
  Stage2Ran, Stage3Ran: Integer;
  Raised: Boolean;
  CTS: TCancellationTokenSource;
  CancelStage1, CancelStage2: specialize IFuture<Integer>;
begin
  // 1. Multi-stage non-blocking pipeline: ITask -> IFuture<Integer> -> IFuture<Integer> -> Map -> IFuture<string>
  StartTask := TTask.Run(
    procedure
    begin
      // initial preparation task
    end);
  Stage1 := StartTask.specialize ThenBy<Integer>(
    function: Integer
    begin
      Result := 10;
    end);
  Stage2 := Stage1.specialize ThenBy<Integer, Integer>(
    function(const aVal: Integer): Integer
    begin
      Result := aVal * 3; // 30
    end);
  Stage3 := Stage2.Map(
    function(const aVal: Integer): Integer
    begin
      Result := aVal + 12; // 42
    end);
  FinalStage := TTask.specialize ThenBy<Integer, string>(Stage3,
    function(const aVal: Integer): string
    begin
      Result := 'Answer=' + IntToStr(aVal);
    end);

  AssertTrue('FinalStage completed', FinalStage.Wait(5000));
  AssertEquals('Pipeline produced expected string', 'Answer=42', FinalStage.Value);

  // 2. Exception propagation through .ThenBy pipeline
  Stage2Ran := 0;
  Stage3Ran := 0;
  FaultStage1 := TTask.specialize Future<Integer>(
    function: Integer
    begin
      raise Exception.Create('Stage1 boom');
      Result := 1;
    end);
  FaultStage2 := FaultStage1.specialize ThenBy<Integer, Integer>(
    function(const aVal: Integer): Integer
    begin
      AtomicIncrement(Stage2Ran);
      Result := aVal + 1;
    end);
  FaultStage3 := FaultStage2.specialize ThenBy<Integer, string>(
    function(const aVal: Integer): string
    begin
      AtomicIncrement(Stage3Ran);
      Result := IntToStr(aVal);
    end);

  Raised := False;
  try
    FaultStage3.Wait(5000);
  except
    on E: EAggregateException do
      Raised := True;
  end;
  AssertTrue('Exception propagated to FaultStage3', Raised);
  AssertTrue('FaultStage3 status is Exception', FaultStage3.Status = TTaskStatus.Exception);
  AssertEquals('FaultStage2 body did not run', 0, Stage2Ran);
  AssertEquals('FaultStage3 body did not run', 0, Stage3Ran);

  // 3. Cancellation propagation through .ThenBy pipeline
  CTS := TCancellationTokenSource.Create;
  try
    CTS.Cancel;
    CancelStage1 := TTask.specialize Future<Integer>(
      function: Integer
      begin
        Result := 100;
      end, CTS.Token);
    CancelStage2 := CancelStage1.specialize ThenBy<Integer, Integer>(
      function(const aVal: Integer): Integer
      begin
        Result := aVal * 2;
      end);
    AssertTrue('CancelStage1 is Canceled', CancelStage1.Status = TTaskStatus.Canceled);
    AssertTrue('CancelStage2 is Canceled', CancelStage2.Status = TTaskStatus.Canceled);
  finally
    CTS.Free;
  end;
end;

procedure TTestContinuationsAndCancellation.TestTreeCancellationAndParallelFor;
var
  CTS: TCancellationTokenSource;
  LoopRes: TParallel.TLoopResult;
  IterationsDone: Integer;
  ParentTask, ChildTask, DownstreamTask: ITask;
  DownstreamRan: Integer;
  StartedEvent: TLightweightEvent;
begin
  // 1. TParallel.&For with TCancellationToken canceled mid-loop
  IterationsDone := 0;
  CTS := TCancellationTokenSource.Create;
  try
    LoopRes := TParallel.&For(1, 200,
      procedure(aIndex: Integer)
      begin
        if aIndex = 1 then;
        if AtomicIncrement(IterationsDone) >= 8 then
          CTS.Cancel;
        Sleep(5);
      end, CTS.Token);
    AssertFalse('LoopRes.Completed is False when token canceled', LoopRes.Completed);
    AssertTrue('Loop stopped early before all 200 iterations', IterationsDone < 200);
  finally
    CTS.Free;
  end;

  // 2. Coordinated tree cancellation: parent task + child task + continuation
  DownstreamRan := 0;
  StartedEvent := TLightweightEvent.Create;
  CTS := TCancellationTokenSource.Create;
  try
    ParentTask := TTask.Run(
      procedure
      begin
        // Child task inherits ambient token from ParentTask automatically
        ChildTask := TTask.Run(
          procedure
          begin
            StartedEvent.SetEvent;
            while True do
              begin
              TTask.CurrentTask.CheckCanceled;
              Sleep(5);
              end;
          end);
        ChildTask.Wait;
      end, CTS.Token);

    DownstreamTask := ParentTask.ContinueWith(
      procedure(aAntecedent: ITask)
      begin
        if aAntecedent = nil then;
        AtomicIncrement(DownstreamRan);
      end, [TTaskContinuationOption.OnlyOnCompleted]);

    AssertTrue('ChildTask started', StartedEvent.WaitFor(5000) = wrSignaled);
    CTS.Cancel;

    try
      ParentTask.Wait(5000);
    except
      on E: Exception do ;
    end;

    AssertTrue('ChildTask status is Canceled', ChildTask.Status = TTaskStatus.Canceled);
    AssertTrue('DownstreamTask is Canceled', DownstreamTask.Status = TTaskStatus.Canceled);
    AssertEquals('DownstreamTask body did not run', 0, DownstreamRan);
  finally
    CTS.Free;
    StartedEvent.Free;
  end;
end;

initialization
  _TestThreads:=TThreadList.Create;
  RegisterTests([
                 TTestTExceptionList,
                 TTestAggregateException,
                 TTestSparseArray,
                 TTestWorkStealingQueue,
                 TTestObjectCache,
                 TTestObjectCaches,
                 TTestThreading,
                 TTestTask,
                 TTestParallel,
                 TTestSyncObjsPhase2,
                 TTestContinuationsAndCancellation
                 ]);

finalization
  JoinTestThreads;
  FreeAndNil(_TestThreads);
end.
