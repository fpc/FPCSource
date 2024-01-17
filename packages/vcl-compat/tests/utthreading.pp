unit utthreading;

{$mode objfpc}{$H+}

{$modeswitch functionreferences}

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
      FLock : TCriticalSection;
  Protected
      Procedure SetUp; override;
      Procedure TearDown; override;
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
    procedure CreateTask;
    procedure OnTask(Sender: TObject);
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
  Protected
    procedure SetUp; override;
    procedure TearDown; override;
  Published
    Procedure TestHookup;
    Procedure TestForEvent;
    {$IFDEF CPU64}
    Procedure TestForEvent64;
    {$ENDIF}
  end;


implementation

uses DateUtils;

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
    AssertSame('Ex'+IntToStr(i),FList.List[i],Ex[i]);
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
  finally
    FLock.Leave;
  end;
end;

procedure TThreadedTestCase.TearDown;
begin
  inherited TearDown;
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
end;

destructor TThreadedTestCase.destroy;
begin
  Flush(output);
  FreeAndNil(FErrors);
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
  FreeOnTerminate:=True;
  Inherited Create(False);
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
    Sleep(Random(100));
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
    Sleep(Random(100));
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
    Sleep(Random(100));
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
  AssertSame('Sender',Self,Sender);
  ThreadTerminated(TThread.CurrentThread); // Will reduce count
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
  AssertSame('Sender',Self,Sender);
  T:=Now;
  Cycles:=10+Random(2);
  For I:=1 to Cycles do
    begin
    BurnCount:=100000*(1+Random(5));
    For J:=1 to BurnCount do
      if (J and 1)=1 then
        K:=K+J
      else
        K:=K-J;
    Sleep(10+Random(10));
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
  AssertSame('Sender',Self,Sender);
  AssertSame('Current task',FTask,TTask.CurrentTask);
//  Writeln('FTask.Status = ',FTask.Status,', current : ',TTask.CurrentTask.Status); //TTaskStatus.Running
  AssertTrue('Task status',TTask.CurrentTask.Status=TTaskStatus.Running);
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

procedure TTestTask.TearDown;
begin
  FTask:=Nil;
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
  Task.Start;
  AssertTrue('Status',Task.Status>=TTaskStatus.WaitingToRun);
  Task.Cancel;
  AssertFalse('Work executed',FWorkExecuted);
end;

procedure TTestTask.TestCheckCanceled;
begin
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
  FRaise:=true;
  Task.Start;
  AssertTrue('Status',Task.Status=TTaskStatus.WaitingToRun);
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
  AssertSame('Sender',self,Sender);
  Result:=43;
//  Writeln('Here 2');
end;

procedure TTestTask.TestFutureEvent;
begin
  AssertEquals('Calc future',43,(TTask.Specialize Future<Integer>(Self,@CalcIntegerEvent)).Value)
end;

{ TTestTask.TTaskThread }

constructor TTestTask.TTaskThread.Create(aTask: ITask; aSleep: Integer);
begin
  FTask:=aTask;
  FSleep:=aSleep;
  FreeOnTerminate:=True;
  Inherited Create(False);
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
  Sleep(50+(10*(1+Random(5))));
  FResults[aIndex]:=aIndex;
end;

procedure TTestParallel.DoEvent64(aSender: TObject; aIndex: Int64);
begin
  Sleep(50+(10*(1+Random(5))));
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

initialization
  RegisterTests([
                 TTestTExceptionList,
                 TTestAggregateException,
                 TTestSparseArray,
                 TTestWorkStealingQueue,
                 TTestObjectCache,
                 TTestObjectCaches,
                 TTestThreading,
                 TTestTask,
                 TTestParallel
                 ]);
end.

