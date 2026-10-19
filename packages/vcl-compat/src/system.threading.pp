{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2024 by Michael Van Canneyt
    member of the Free Pascal development team

    Delphi-compatible threading unit

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit System.Threading;

interface

{$mode objfpc}
{$h+}
{$SCOPEDENUMS ON}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}

{ $DEFINE DEBUGTHREADPOOL}

{$IFDEF CPU64}
{$DEFINE THREAD64BIT}
{$ENDIF}

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.Timespan, System.SyncObjs, System.Contnrs, Fcl.ThreadPool;
{$ELSE}
  Contnrs, SysUtils, Classes, System.Timespan, Generics.Collections, SyncObjs, fpthreadpool
  {$IFDEF FPC_VALGRIND}, fpvalgrind{$ENDIF};
{$ENDIF}

type
  TLightweightEvent = TEvent;
  
  Generic TFunctionEvent<T> = function (Sender: TObject): T of object;
  Generic TProc<T> = reference to procedure (arg : T);
  Generic TProc2<T1,T2> = reference to procedure (arg1 : T1;arg2 : T2);
  Generic TFunc<T> = Reference to function : T;
  Generic TFunc1<TArg,TResult> = reference to function (const aArg : TArg) : TResult;
  TProcRef = Reference to Procedure;

  TExceptionHandlerEvent = procedure (const aException: Exception; var aHandled: Boolean) of object;
  TExceptionHandlerProc = reference to procedure (const aException: Exception; var aHandled: Boolean);
  TExceptionArray = Array of Exception;

  TTask = class;

  { EAggregateException }

  { TExceptionList }
  // Does not own the exceptions
  TExceptionList = Record
    List : TExceptionArray;
    Count : Integer;
    Class function Create(aCapacity : Integer) : TExceptionList; static;
    Class function Create(Initial : Exception; aCapacity : Integer) : TExceptionList; static;
    Class function Create(aExceptionArray: array of Exception) : TExceptionList; static;
    Procedure AddFromTask(aTask : TTask);
    Procedure Add(aException : Exception);
    Function GrowCapacity(aMinCapacity : Integer) : Integer;
    Function Capacity : Integer;
    function Truncate : TExceptionArray;
    // Will free exceptions.
    procedure ClearList;
    // Expands Aggregate exception. Clears list of exceptions
    procedure Flatten(aException: Exception);
  end;

  EAggregateException = class(Exception)
  Private
    FList: TExceptionList;
    function GetInnerException(aIndex: Integer): Exception;
    procedure clearlist;
  public
    type

    { TExceptionEnumerator }

    TExceptionEnumerator = class
    private
      FList : TExceptionArray;
      FCurrent : Integer;
      function GetCurrent: Exception;
    public
      constructor Create(aList : TExceptionArray);
      function MoveNext: Boolean; inline;
      property Current: Exception read GetCurrent;
    end;
  Public
    const MaxLoggedExceptions = 10;
  public
    constructor Create(const aExceptionArray: array of Exception); overload;
    constructor Create(const aMessage: string; const aExceptionArray: array of Exception); overload;
    destructor Destroy; override;
    function GetEnumerator: TExceptionEnumerator; inline;
    procedure Handle(aExceptionHandlerEvent: TExceptionHandlerEvent); overload;
    procedure Handle(const aExceptionHandlerProc: TExceptionHandlerProc); overload;
    procedure Add(aException: Exception);
    function ToString: RTLString; override;
    property Count: Integer read FList.Count;
    property InnerExceptions[aIndex: Integer]: Exception read GetInnerException; default;
  end;

  { TSparseArray }

  generic TSparseArray<T: class> = class
  public
    Type
      TArrayOfT = specialize TArray<T>;
  private
    FArray: TArrayOfT;
    FLock: TSpinLock;
  public
    constructor Create(aInitialSize: Integer);
    destructor Destroy; override;
    function Add(const aItem: T): Integer;
    procedure Lock;
    function Remove(const aItem: T) : Boolean;
    procedure Unlock;
    property Current: TArrayOfT read FArray;
  end;

  { TWorkStealingQueue }

  generic TWorkStealingQueue<T> = class
  Private
    Type
      TItemList = specialize TList<T>;
  Private
    FItems : TItemList;
    FLock : TSpinLock;
    FEvent : TEvent;
    function GetCount: Integer;
    function GetIsEmpty: Boolean;
  protected
    procedure Lock;
    procedure UnLock;
  public
    constructor Create;
    destructor Destroy; override;
    function LocalFindAndRemove(const aItem: T): Boolean;
    procedure LocalPush(const aItem: T);
    function LocalPop(out aItem: T): Boolean;
    function TrySteal(out aItem: T; aTimeout: Cardinal = 0): Boolean;
    function Remove(const aItem: T): Boolean;
    property IsEmpty: Boolean read GetIsEmpty;
    property Count: Integer read GetCount;
  end;

  { TObjectCache }

  TObjectCache = class
  Private
    FStack :{$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}Contnrs.TStack;
    FLock : TSpinLock;
    FItemClass : TClass;
  public const
    CObjectCacheLimit = 50;
  public
    constructor Create(aClass: TClass);
    destructor Destroy; override;
    procedure Clear;
    function Insert(Instance: Pointer): Boolean;
    function Remove: Pointer;
    function Count : Integer;
  end;

  { TObjectCaches }

  TObjectCaches = class(specialize TObjectDictionary<TClass, TObjectCache>)
  public
    procedure AddObjectCache(aClass: TClass);
  end;

  { TThreadPool }

  TThreadPool = class sealed
  public type
    // Initial:  -1
    // Execute: 0
    // Cancel >0
    IControlFlag = interface(IInterface)
      function Increment: Integer;
      function Value: Integer;
    end;
    TProcThread = specialize TProc<TThread>;
  private
    class var FDefaultPool: TThreadPool;
    const
      RetireDelay  = 5000;      // in milliseconds. Time after which a thread retires
      SuspendDelay = 5500;      // in milliseconds. Minimum time between 2 thread suspending themselves
      SuspendTime  = 6000;      // in milliseconds. Time for which a suspended thread sleeps.
      SuspendTries = 3;         // Number of tries for suspend loop
      MaxCPUUsage  = 95;        // CPU usage % at which we stop threads.
      MinCPUUsage  = 80;        // CPU usage % below which we add threads.
      NumCPUUsageSamples = 10;  // Number of samples for average CPU usage
      MaxThreadsPerCPU = 2;     // Max threads per CPU, used to determine MaxThreads.
      ThreadToRequestRatio = 8; // Number of requests per thread.
      IdleTimeout = 40 * 1000;  // Idle timeout
      MonitorThreadDelay = 500; // Delay between ticks
      MonitorMaxInactiveInterval = 30 * 1000; //
      MonitorIdleLimit = MonitorMaxInactiveInterval div MonitorThreadDelay;
      EnoughThreadsTimeout = 2 * IdleTimeout; // If there are enough threads, if the current thread will wait longer than this, kill it.
      NoRequestsTimeOut = 4 * IdleTimeout; // If there are no requests, if the current thread will wait longer than this, kill it.
    procedure GrowPool;
    function IsThrottledDelay(aLastCreationTick: UInt64; aThreadCount: Cardinal): Boolean;
    procedure LockQueue;
    procedure UnLockQueue;
    procedure WaitForThreads;
    procedure WorkQueued;
  Private
    FInteractive: Boolean;
    FOnThreadStart: TProcThread;
    FOnThreadTerminate: TProcThread;
    FUnlimitedWorkerThreadsWhenBlocked: Boolean;
    FSuspendGuard : integer; // Suspend guard
    FSuspendAtTick : Int64;  // Tick at which last suspend occurred
    FSuspendCount : Integer; // Number of suspended threads
    FMaxThreads: Integer; // Maximum number of worker threads.
    FMinThreads: Integer; // Minimum number of worker threads.
    FThreadCount : Integer;  // Number of worker threads
    FIdleThreads : Integer;  // number of worker threads in idle state
    FRetiring : Integer;     // Number of worker threads trying to retire
    FCPUUsage : Integer;     // CPU usage in %
    FAvgCPUUsage : Integer;  // Average CPU usage in %
    FRequestCount : Integer; // Number of work items in queue
    FPreviousRequestCount : Integer; // During monitor check, this is used to determine whether the number of request grows/shrinks
    FThreadCreationAt : Int64; // Tick at which the last thread was created.
    FRetireEvent : TEvent;
    FQueueLock : TSpinlock;
    FQueueEvent : TEvent;    // Set when a work item is queued.
    FCPUInfo: TThread.TSystemTimes;
    FCpuUsageArray: array[0..TThreadPool.NumCPUUsageSamples - 1] of Cardinal;
    FCurUsageSlot: Integer;

    class function GetCurrentThreadPool: TThreadPool; static;
  protected type

    { TSafeSharedInteger }

    TSafeSharedInteger = record
    private
      FValue : ^Integer;
      function GetInteger: Integer;
      procedure SetInteger(AValue: Integer);
    public
      constructor Create(var aSharedVar: Integer);
      function Increment: Integer; inline;
      function Decrement: Integer; inline;
      function CompareExchange(aValue: Integer; aComparand: Integer): Integer; inline;
      class operator Explicit(aValue: TSafeSharedInteger): Integer; inline;
      property Value: Integer read GetInteger write SetInteger;
    end;

    {$IFDEF THREAD64BIT}
    { TSafeSharedUInt64 }
    TSafeSharedUInt64 = record
    private
      FValue : ^UInt64;
      function GetUInt64: UInt64;
      procedure SetUInt64(AValue: UInt64);
    public
      constructor Create(var aSharedVar: UInt64);
      function Increment: UInt64; inline;
      function Decrement: UInt64; inline;
      class operator Explicit(aValue: TSafeSharedUInt64): UInt64; inline;

      property Value: UInt64 read GetUInt64 write SetUInt64;
    end;
    {$ENDIF}

    { IThreadPoolWorkItem }

    IThreadPoolWorkItem = interface(IInterface)
      function ShouldExecute: Boolean;
      procedure ExecuteWork;
    end;

    { TControlFlag }

    TControlFlag = class(TInterfacedObject, IControlFlag)
    Private
      FFlag : Integer;
    public
      function Increment: Integer;
      function Value: Integer;
      constructor Create; overload;
    end;

    { TAbstractWorkerData }

    TAbstractWorkerData = class(TInterfacedObject)
    protected
      FControlFlag: IControlFlag;
      function ShouldExecute: Boolean;
    public
      class function NewInstance: TObject; override;
      procedure FreeInstance; override;
      constructor Create(aFlag : IControlFlag);
    end;

    { TWorkerData }

    TWorkerData = class(TAbstractWorkerData, IThreadPoolWorkItem)
    protected
      FSender: TObject;
      FWorkerEvent: TNotifyEvent;
      FProc: TProcRef;
      procedure ExecuteWork;
    Public
      constructor Create(aFlag : IControlFlag; aSender : TObject; aEvent : TNotifyEvent);
      constructor Create(aFlag : IControlFlag; aProc: TProcRef);
    end;

    TLightweightEvent = TEvent;

    { TBaseWorkerThread }

    TBaseWorkerThread = class(TThread)
    private
      FThreadPool: TThreadPool;
      FRunningEvent: TLightweightEvent;
      FMyWorkerID : Integer;
    class var FWorkerID : Integer;
    protected
      class function NextWorkerID : Integer;
      function GetWorkerThreadName: string;
      procedure RemoveFromPool;
      procedure SafeTerminate;
      procedure Execute; override;
      // Sets the running event, also when Execute was never called
      procedure DoTerminate; override;
      property ThreadPool: TThreadPool read FThreadPool;
      property RunningEvent: TLightweightEvent read FRunningEvent;
      property MyWorkerID : Integer Write FMyWorkerID;
    public
      constructor Create(aThreadPool: TThreadPool);
      destructor Destroy; override;
      procedure BeforeDestruction; override;
    end;
    TBaseWorkerThreadList = Specialize TThreadList<TBaseWorkerThread>;

    { TQueueWorkerThread }

    Type
      TWorkStealingQueueThreadPoolWorkItem =  specialize TWorkStealingQueue<IThreadPoolWorkItem>;

    TQueueWorkerThread = class(TBaseWorkerThread)
    Protected
    const
      MaxDelay = RetireDelay * 60;
      MaxCheckWaitTime = MaxInt div 2;
    private
      FCheckWaitTime : Integer;
      FIdle: Boolean;
      FWorkQueue: TWorkStealingQueueThreadPoolWorkItem;
      FWorkException : Exception;
      FPoolRetireEvent : TEvent; // owned by pool!
      procedure AdjustWaitTime;
      procedure WrapExecute(var aItem: IThreadPoolWorkItem);
    protected
      function SuspendWork: Boolean;
      function TryToRetire: Boolean;
      procedure ExecuteWorkItem(var aItem: IThreadPoolWorkItem);
      procedure Execute; override;
      procedure PushLocalWorkToGlobal;
      property WorkQueue: TWorkStealingQueueThreadPoolWorkItem read FWorkQueue;
      Property CheckWaitTime : Integer Read FCheckWaitTime;
    public
      constructor Create(aThreadPool: TThreadPool);
      destructor Destroy; override;
      property Idle : Boolean Read FIdle Write FIdle;
    end;

    { TThreadPoolMonitor }

    TThreadPoolMonitor = class(TThread)
    private
      FThreadPool : TThreadPool;
      function GetThreadName: string;
    protected
      function IsThrottledDelay(aLastCreationTick: UInt64; aThreadCount: Cardinal): Boolean;
      procedure Execute; override;
      procedure GrowThreadPoolIfStarved;
    public
      constructor Create(aThreadPool: TThreadPool);
    end;

  private
    const
      MonitorNone    = 0;
      MonitorCreated = 1;

    Type
      TWorkStealingQueueThreadPoolWorkItemArray = specialize TSparseArray<TWorkStealingQueueThreadPoolWorkItem>;
      // The global work queue must retain a reference to the queued items. A raw
      // pointer queue (Contnrs.TQueue) does not, so a work item could be destroyed
      // while still enqueued, leading to a use-after-free on the worker threads.
      TWorkItemQueue = specialize TQueue<IThreadPoolWorkItem>;
      TMonitorResult = (mrTerminate,mrContinue,mrIdle);
    var
      FWorkQueue : TWorkItemQueue;
      FQueues : TWorkStealingQueueThreadPoolWorkItemArray;
      FThreads: TBaseWorkerThreadList;
      // Threads that left Execute; the pool joins and frees them (ReapFinishedThreads).
      FFinishedThreads: TBaseWorkerThreadList;
      // Owned by the pool, freed (joined) when a new monitor is created or in Destroy.
      FMonitorThread : TThread;

      FMonitorStatus : Integer;
      FShutDown : Boolean;
    procedure NewThread(aThread : TBaseWorkerThread);
    procedure RemoveThread(aThread : TBaseWorkerThread);
    procedure ThreadFinished(aThread : TBaseWorkerThread);
    procedure ReapFinishedThreads;
    procedure FreeMonitorThread;
  protected
    class threadvar QueueThread : TQueueWorkerThread;
    class var Caches : TObjectCaches;
    // Queue management.
    procedure RegisterWorkerThread(aThread: TQueueWorkerThread);
    procedure UnRegisterWorkerThread(aThread: TQueueWorkerThread);
    // Adding/Removing work
    function DoRemoveWorkItem(WorkerData: IThreadPoolWorkItem): Boolean;
    procedure DoQueueWorkItem(WorkerData: IThreadPoolWorkItem; PreferThread : TQueueWorkerThread);
    procedure AssignWorkToLocalQueue(WorkerData: IThreadPoolWorkItem; aThread: TQueueWorkerThread);
    procedure AssignWorkToGlobalQueue(WorkerData: IThreadPoolWorkItem);
    // Getting work.
    function CheckShouldTerminate(aThread: TQueueWorkerThread): Boolean;
    function GetWorkItemForThread(aThread: TQueueWorkerThread; out Itm: IThreadPoolWorkItem): Boolean;
    function GetWorkItemFromQueues(aSkip: TWorkStealingQueueThreadPoolWorkItem; out Itm: IThreadPoolWorkItem): Boolean;
    function TryGetStolenWorkItem(aThread: TQueueWorkerThread; out Itm: IThreadPoolWorkItem): Boolean;
    // Notification when work was queued
    procedure SignalExecuting(aThread: TQueueWorkerThread);
    // Monitoring & Thread management
    procedure CreateMonitorThread;
    procedure WaitForMonitorThread;
    procedure InitCPUStats;
    procedure StopCPUStats;
    function DoMonitor: TMonitorResult;
    function HaveNoWorkers: boolean;
    Procedure GrowIfStarved;
    function AddThreadToPool: TQueueWorkerThread;
  public
    constructor Create;
    destructor Destroy; override;

    class function NewControlFlag: IControlFlag; static;
    procedure QueueWorkItem(aSender: TObject; aWorkerEvent: TNotifyEvent; const aControlFlag: IControlFlag = nil); overload;
    procedure QueueWorkItem(const aWorkerEvent: TProcRef; const aControlFlag: IControlFlag = nil); overload;
    // Return true if new value was actually set.
    function SetMaxWorkerThreads(aValue: Integer): Boolean;
    function SetMinWorkerThreads(aValue: Integer): Boolean;
    property MaxWorkerThreads: Integer read FMaxThreads;
    property MinWorkerThreads: Integer read FMinThreads;
    property UnlimitedWorkerThreadsWhenBlocked: Boolean read FUnlimitedWorkerThreadsWhenBlocked  write FUnlimitedWorkerThreadsWhenBlocked default True;
    // if set, then wait loops will call checksynchronize if they are executed in main thread.
    property Interactive: Boolean read FInteractive write FInteractive default False;
    property OnThreadStart: TProcThread read FOnThreadStart write FOnThreadStart;
    property OnThreadTerminate: TProcThread read FOnThreadTerminate write FOnThreadTerminate;
    class property Default: TThreadPool read FDefaultPool;
    class property Current: TThreadPool read GetCurrentThreadPool;
  end;

  { TThreadPoolStats }

  TThreadPoolStats = record
  private
    FWorkerThreadCount: Integer;
    FMinLimitWorkerThreadCount: Integer;
    FMaxLimitWorkerThreadCount: Integer;
    FIdleWorkerThreadCount: Integer;
    FQueuedRequestCount: Integer;
    FRetiredWorkerThreadCount: Integer;
    FAverageCPUUsage: Integer;
    FCurrentCPUUsage: Integer;
    FThreadSuspended: Integer;
    FLastSuspendTick: UInt64;
    FLastThreadCreationTick: UInt64;
    FLastQueuedRequestCount: Integer;
    class function GetCurrent: TThreadPoolStats; static; inline;
    class function GetDefault: TThreadPoolStats; static; inline;
  public
    procedure Assign(const aPool: TThreadPool);
    property WorkerThreadCount: Integer read FWorkerThreadCount;
    property MinLimitWorkerThreadCount: Integer read FMinLimitWorkerThreadCount;
    property MaxLimitWorkerThreadCount: Integer read FMaxLimitWorkerThreadCount;
    property IdleWorkerThreadCount: Integer read FIdleWorkerThreadCount;
    property QueuedRequestCount: Integer read FQueuedRequestCount;
    property RetiredWorkerThreadCount: Integer read FRetiredWorkerThreadCount;
    property AverageCPUUsage: Integer read FAverageCPUUsage;
    property CurrentCPUUsage: Integer read FCurrentCPUUsage;
    property ThreadSuspended: Integer read FThreadSuspended;
    property LastSuspendTick: UInt64 read FLastSuspendTick;
    property LastThreadCreationTick: UInt64 read FLastThreadCreationTick;
    property LastQueuedRequestCount: Integer read FLastQueuedRequestCount;
    class function Get(const aPool: TThreadPool): TThreadPoolStats; static;
    class property Current: TThreadPoolStats read GetCurrent;
    class property Default: TThreadPoolStats read GetDefault;
  end;

  TTaskStatus = (Created, WaitingToRun, Running, Completed, WaitingForChildren, Canceled, Exception);

  TTaskContinuationOption = (
    NotOnCompleted,
    NotOnFaulted,
    NotOnCanceled,
    OnlyOnCompleted,
    OnlyOnFaulted,
    OnlyOnCanceled,
    ExecuteSynchronously
  );
  TTaskContinuationOptions = set of TTaskContinuationOption;

  ICancellationState = interface
    ['{D791878A-02E6-4232-9577-6866310E2538}']
    function IsCancellationRequested: Boolean;
    function GetWaitHandle: TEvent;
    procedure Cancel;
    procedure CancelAfter(aMillisecondsDelay: Cardinal);
    function RegisterCallback(const aCallback: TProcRef; aEvent: TNotifyEvent; aSender: TObject): Int64;
    procedure UnregisterCallback(aId: Int64);
  end;

  { TCancellationTokenRegistration }

  TCancellationTokenRegistration = record
  private
    FState: ICancellationState;
    FId: Int64;
  public
    procedure Unregister;
    procedure Dispose; inline;
  end;

  { TCancellationToken }

  TCancellationToken = record
  private
    FState: ICancellationState;
    function GetCanBeCanceled: Boolean; inline;
    function GetIsCancellationRequested: Boolean; inline;
    function GetWaitHandle: TEvent; inline;
  public
    class function None: TCancellationToken; static; inline;
    procedure ThrowIfCancellationRequested; inline;
    procedure CheckCanceled; inline;
    function Register(const aCallback: TProcRef): TCancellationTokenRegistration; overload;
    function Register(aEvent: TNotifyEvent; aSender: TObject = nil): TCancellationTokenRegistration; overload;
    class operator =(const aLeft, aRight: TCancellationToken): Boolean; inline;
    class operator <>(const aLeft, aRight: TCancellationToken): Boolean; inline;
    property CanBeCanceled: Boolean read GetCanBeCanceled;
    property IsCancellationRequested: Boolean read GetIsCancellationRequested;
    property WaitHandle: TEvent read GetWaitHandle;
  end;

  { TCancellationTokenSource }

  TCancellationTokenSource = class
  private
    FState: ICancellationState;
    FLinkedRegistrations: array of TCancellationTokenRegistration;
    procedure OnLinkedTokenCancel(Sender: TObject);
    function GetIsCancellationRequested: Boolean; inline;
    function GetToken: TCancellationToken; inline;
    function GetWaitHandle: TEvent; inline;
  public
    constructor Create; overload;
    constructor Create(aMillisecondsDelay: Cardinal); overload;
    constructor Create(const aDelay: TTimeSpan); overload;
    destructor Destroy; override;
    class function CreateLinkedTokenSource(const aToken1, aToken2: TCancellationToken): TCancellationTokenSource; overload; static;
    class function CreateLinkedTokenSource(const aTokens: array of TCancellationToken): TCancellationTokenSource; overload; static;
    procedure Cancel;
    procedure CancelAfter(aMillisecondsDelay: Cardinal); overload;
    procedure CancelAfter(const aDelay: TTimeSpan); overload;
    procedure Reset;
    property IsCancellationRequested: Boolean read GetIsCancellationRequested;
    property Token: TCancellationToken read GetToken;
    property WaitHandle: TEvent read GetWaitHandle;
  end;

  ITask = interface (TThreadPool.IThreadPoolWorkItem) ['{AD752DA0-556C-41B5-96F2-0B0CA932E364}']
    function Wait(aTimeout: Cardinal = INFINITE): Boolean; overload;
    function Wait(const aTimeout: TTimeSpan): Boolean; overload;
    procedure Cancel;
    procedure CheckCanceled;
    function Start: ITask;
    function GetStatus: TTaskStatus;
    function GetId: Integer;
    function ContinueWith(const aAction: specialize TProc<ITask>): ITask; overload;
    function ContinueWith(const aAction: specialize TProc<ITask>; aOptions: TTaskContinuationOptions): ITask; overload;
    function ContinueWith(const aAction: specialize TProc<ITask>; const aCancelToken: TCancellationToken; aOptions: TTaskContinuationOptions = []; aPool: TThreadPool = nil): ITask; overload;
    property Id: Integer read GetId;
    property Status: TTaskStatus read GetStatus;
  end;
  TITaskArray = array of ITask;
  TITaskProc = specialize TProc<ITask>;
  TITaskProcArray = Array of TITaskProc;

  generic IFuture<T> = interface(ITask)
    function StartFuture: specialize IFuture<T>; overload;
    function GetValue: T;
    function ContinueWith(const aAction: specialize TProc<specialize IFuture<T>>): ITask; overload;
    function ContinueWith(const aAction: specialize TProc<specialize IFuture<T>>; aOptions: TTaskContinuationOptions): ITask; overload;
    function ContinueWith(const aAction: specialize TProc<specialize IFuture<T>>; const aCancelToken: TCancellationToken; aOptions: TTaskContinuationOptions = []; aPool: TThreadPool = nil): ITask; overload;
    function Map(const aFunc: specialize TFunc1<T, T>; aOptions: TTaskContinuationOptions = []): specialize IFuture<T>; overload;
    function Map(const aFunc: specialize TFunc1<T, T>; const aCancelToken: TCancellationToken; aOptions: TTaskContinuationOptions = []; aPool: TThreadPool = nil): specialize IFuture<T>; overload;
    property Value: T read GetValue;
  end;

  TTaskArray = Array of TTask;

  TAbstractTask = class(TThreadPool.TAbstractWorkerData)
  protected
    type
    IInternalTask = interface(ITask) ['{4C5EFFA7-FB5F-4C38-3E20-84FF25678812}']  
      procedure AddCompleteEvent(const aProc: TITaskProc);
      procedure HandleChildCompletion(const aTask: IInternalTask);
      procedure HandleChildCompletion(const aTask: TTask);
      procedure SetExceptionObject(const aException: TObject);
      procedure RemoveCompleteEvent(const aProc: TITaskProc);
      function GetControlFlag: TThreadPool.IControlFlag;
    end;
  end;

  { TTask }

  TTask = class(TAbstractTask, TThreadPool.IThreadPoolWorkItem, ITask, TAbstractTask.IInternalTask)
  private
    class var FNextTaskID : integer;
    class threadvar _CurrentTask : TTask;

  protected
    type
      TOptionStateFlag = (Started, CallbackRun, ChildWait, Complete, Canceled, Faulted, Replicating, Replica, Raised, Destroying);
      TOptionStateFlags = set of TOptionStateFlag;
      TCreateFlag = (Replicating, Replica);
      TCreateFlags = set of TCreateFlag;
    const
      StateFlagMask = [TOptionStateFlag.Started, TOptionStateFlag.CallbackRun, TOptionStateFlag.ChildWait,
                       TOptionStateFlag.Complete, TOptionStateFlag.Canceled, TOptionStateFlag.Faulted];
      OptionFlagMask = [TOptionStateFlag.Replicating, TOptionStateFlag.Replica];
      ReplicatingStates = OptionFlagMask;
      StartedStates = [TOptionStateFlag.Started, TOptionStateFlag.Canceled, TOptionStateFlag.Faulted, TOptionStateFlag.Complete];
      CompleteStates = [TOptionStateFlag.Destroying, TOptionStateFlag.Complete, TOptionStateFlag.Faulted];
      CanceledStates = [TOptionStateFlag.Canceled, TOptionStateFlag.Faulted];

  Public
    Type

      { TTaskParams }

      TTaskParams = record
        Sender: TObject;
        Event: TNotifyEvent;
        Proc: TProcRef;
        Pool: TThreadPool;
        Parent: TTask;
        CreateFlags: TCreateFlags;
        ParentControlFlag: TThreadPool.IControlFlag;
        CancellationToken: TCancellationToken;
        Procedure ResolvePool;
      end;

  Private
    // Instance stuff
    FStateFlags : TOptionStateFlags;
    FReplicaRoot : TTask;
    FStatus : TTaskStatus;
    FParams : TTaskParams;
    FTaskID : Integer;
    FSubTasks : Integer;
    FStateLock : TSpinLock;
    FTasksWithExceptions : TITaskArray;
    FCompletedEvents : TITaskProcArray;
    FCompletedEventCount : Integer;
    FDone : Boolean;
    FCancelRegistration : TCancellationTokenRegistration;
    FExceptionClass : ExceptClass;
    FExceptionMessage : string;
    procedure OnCancelToken(Sender: TObject);
    function GetDoneEvent: TLightweightEvent;
  protected
    FException: TObject;
    FDoneEvent : TLightweightEvent;
    function UpdateStateAtomic(aNewState: TOptionStateFlags; aInvalidStates: TOptionStateFlags): Boolean; overload;
    function UpdateStateAtomic(aNewState: TOptionStateFlags; aInvalidStates: TOptionStateFlags; out aOldState: TOptionStateFlags): Boolean; overload;
    procedure SetTaskStop;
    function ShouldCreateReplica: Boolean; virtual;
    function CreateReplicaTask(const aProc: TProcRef; aParent: TTask; aCreateFlags: TCreateFlags; const aParentControlFlag: TThreadPool.IControlFlag): TTask; virtual;
    function CreateReplicaTask(const aParams : TTaskParams) : TTask; virtual;
    procedure CheckFaulted;
    procedure PropagateExceptionFrom(aSource: TTask);
    procedure AttachContinuation(aContTask: TTask; const aCancelToken: TCancellationToken; aOptions: TTaskContinuationOptions; aPropagateAntecedentState: Boolean);
    class function ShouldRunContinuation(aStatus: TTaskStatus; aOptions: TTaskContinuationOptions): Boolean; static;
    procedure SetComplete;
    procedure AddChild;
    procedure ForgetChild;
    Procedure LockState; inline;
    Procedure UnLockState; inline;
    function InternalExecuteNow: Boolean;
    function GetExceptionObject: Exception;
    function GetIsComplete: Boolean; inline;
    function GetIsReplicating: Boolean; inline;
    function GetHasExceptions: Boolean; inline;
    function GetIsCanceled: Boolean; inline;
    function GetIsQueued: Boolean; inline;
    function GetWasExceptionRaised: Boolean; inline;
    procedure QueueEvents; virtual;
    procedure Complete(UserEventRan: Boolean);
    procedure IntermediateCompletion;
    procedure FinalCompletion;
    procedure ProcessCompleteEvents; virtual;
    procedure SetRaisedState;
    procedure CalcStatus;
    procedure ForceStateFlags(aFlags : TOptionStateFlags); inline;
    function InternalWork(aCheckExecuting: Boolean): Boolean;
    procedure InternalExecute(var aCurrentTaskVar: TTask);
    procedure Execute;
    procedure DoCancel(aDestroying: Boolean);
    procedure ReplicaCallUserCode;
    procedure ExecuteReplicates(const aRoot: TTask);
    procedure CallUserCode; inline;
    procedure HandleException(const aChildTask: ITask; const aException: TObject);
    procedure HandleException(const aChildTask: TTask; const aException: TObject);
    function MarkAsStarted: Boolean;
    function TryExecuteNow(aWasQueued: Boolean): Boolean;
    { IThreadPoolWorkItem }
    procedure ExecuteWork;
    { ITask }
    function Wait(aTimeout: Cardinal = INFINITE): Boolean; overload;
    function Wait(const aTimeout: TTimeSpan): Boolean; overload;
    procedure Cancel;
    procedure CheckCanceled;
    function Start: ITask;
    function GetId: Integer;
    function GetStatus: TTaskStatus;
    function ContinueWith(const aAction: specialize TProc<ITask>): ITask; overload;
    function ContinueWith(const aAction: specialize TProc<ITask>; aOptions: TTaskContinuationOptions): ITask; overload;
    function ContinueWith(const aAction: specialize TProc<ITask>; const aCancelToken: TCancellationToken; aOptions: TTaskContinuationOptions = []; aPool: TThreadPool = nil): ITask; overload;
    { IInternalTask }
    // Registers aProc to be called on completion, calls it at once when the task is already done
    procedure AddCompleteEvent(const aProc: TITaskProc);
    procedure HandleChildCompletion(const aTask: TAbstractTask.IInternalTask);
    procedure HandleChildCompletion(const aTask: TTask);
    procedure SetExceptionObject(const aException: TObject);
    procedure RemoveCompleteEvent(const aProc: TITaskProc);
    function GetControlFlag: TThreadPool.IControlFlag;
    Property ID : Integer Read GetID;
    property IsComplete: Boolean read GetIsComplete;
    property IsReplicating: Boolean read GetIsReplicating;
    property HasExceptions: Boolean read GetHasExceptions;
    property IsCanceled: Boolean read GetIsCanceled;
    property IsQueued: Boolean read GetIsQueued;
    property WasExceptionRaised: Boolean read GetWasExceptionRaised;
    property DoneEvent: TLightweightEvent read GetDoneEvent;
    property ThreadPool: TThreadPool read FParams.Pool;
    class function DoWaitForAll(const aTasks: array of ITask; aTimeout: Cardinal): Boolean; static;
    class function DoWaitForAny(const aTasks: array of ITask; aTimeout: Cardinal): Integer; static;
    class function NewId: Integer; static;
  Public
  public
    class function CurrentTask: ITask; static;
    constructor Create(const aParams : TTaskParams); overload;
    constructor Create; overload;
    destructor Destroy; override;
    class function Create(aSender: TObject; aEvent: TNotifyEvent): ITask; overload; static;
    class function Create(const aProc: TProcRef): ITask; overload; static;
    class function Create(aSender: TObject; aEvent: TNotifyEvent; const aPool: TThreadPool): ITask; overload; static;
    class function Create(const aProc: TProcref; aPool: TThreadPool): ITask; overload; static;
    class function Create(aSender: TObject; aEvent: TNotifyEvent; const aCancelToken: TCancellationToken; const aPool: TThreadPool = nil): ITask; overload; static;
    class function Create(const aProc: TProcRef; const aCancelToken: TCancellationToken; aPool: TThreadPool = nil): ITask; overload; static;
    generic class function Future<T>(aSender: TObject; aEvent: specialize TFunctionEvent<T>): Specialize IFuture<T>; overload; static; inline;
    generic class function Future<T>(aSender: TObject; aEvent: specialize TFunctionEvent<T>; aPool: TThreadPool): Specialize IFuture<T>; overload; static; inline;
    generic class function Future<T>(aSender: TObject; aEvent: specialize TFunctionEvent<T>; const aCancelToken: TCancellationToken; aPool: TThreadPool = nil): Specialize IFuture<T>; overload; static; inline;
    generic class function Future<T>(const aFunc: specialize TFunc<T>): Specialize IFuture<T>; overload; static; inline;
    generic class function Future<T>(const aFunc: specialize TFunc<T>; aPool: TThreadPool): Specialize IFuture<T>; overload; static; inline;
    generic class function Future<T>(const aFunc: specialize TFunc<T>; const aCancelToken: TCancellationToken; aPool: TThreadPool = nil): Specialize IFuture<T>; overload; static; inline;
    generic class function ThenBy<TResult>(const aTask: ITask; const aFunc: specialize TFunc1<ITask, TResult>; const aCancelToken: TCancellationToken; aOptions: TTaskContinuationOptions = []; aPool: TThreadPool = nil): specialize IFuture<TResult>; overload; static;
    generic class function ThenBy<TResult>(const aTask: ITask; const aFunc: specialize TFunc<TResult>; const aCancelToken: TCancellationToken; aOptions: TTaskContinuationOptions = []; aPool: TThreadPool = nil): specialize IFuture<TResult>; overload; static;
    generic class function ThenBy<T, TResult>(const aTask: ITask; const aFunc: specialize TFunc1<T, TResult>): specialize IFuture<TResult>; overload; static; inline;
    generic class function ThenBy<T, TResult>(const aTask: ITask; const aFunc: specialize TFunc1<T, TResult>; aOptions: TTaskContinuationOptions): specialize IFuture<TResult>; overload; static; inline;
    generic class function ThenBy<T, TResult>(const aTask: ITask; const aFunc: specialize TFunc1<T, TResult>; const aCancelToken: TCancellationToken; aOptions: TTaskContinuationOptions = []; aPool: TThreadPool = nil): specialize IFuture<TResult>; overload; static;
    class function Run(aSender: TObject; aEvent: TNotifyEvent): ITask; overload; static; inline;
    class function Run(aSender: TObject; aEvent: TNotifyEvent; aPool: TThreadPool): ITask; overload; static; inline;
    class function Run(aSender: TObject; aEvent: TNotifyEvent; const aCancelToken: TCancellationToken; aPool: TThreadPool = nil): ITask; overload; static; inline;
    class function Run(const aFunc: TProcRef): ITask; overload; static; inline;
    class function Run(const aFunc: TProcRef; aPool: TThreadPool): ITask; overload; static; inline;
    class function Run(const aFunc: TProcRef; const aCancelToken: TCancellationToken; aPool: TThreadPool = nil): ITask; overload; static; inline;
    class function WaitForAll(const aTasks: array of ITask): Boolean; overload; static;
    class function WaitForAll(const aTasks: array of ITask; aTimeout: Cardinal): Boolean; overload; static;
    class function WaitForAll(const aTasks: array of ITask; const aTimeout: TTimeSpan): Boolean; overload; static;
    class function WaitForAny(const aTasks: array of ITask): Integer; overload; static;
    class function WaitForAny(const aTasks: array of ITask; aTimeout: Cardinal): Integer; overload; static;
    class function WaitForAny(const aTasks: array of ITask; const aTimeout: TTimeSpan): Integer; overload; static;
  end;

  { TFutureContinuationTask }

  generic TFutureContinuationTask<T> = class(TTask)
  public type
    IFutureT = specialize IFuture<T>;
    TFutureProcT = specialize TProc<IFutureT>;
  private
    FAntecedent: IFutureT;
    FAction: TFutureProcT;
    procedure RunAction(Sender: TObject);
  public
    constructor CreateContinuation(const aAntecedent: IFutureT; const aAction: TFutureProcT;
      const aCancelToken: TCancellationToken; aPool: TThreadPool);
    destructor Destroy; override;
  end;

  { TFuture }

  generic TFuture<T> = class(TTask, specialize IFuture<T>)
  Type
    TFunctionEventT = specialize TFunctionEvent<T>;
    TFunctionRefT = specialize TFunc<T>;
    TFunctionTaskRefT = specialize TFunc1<ITask, T>;
    TFutureProcT = specialize TProc<specialize IFuture<T>>;
    TMapFuncT = specialize TFunc1<T, T>;
  Var
    FResult : T;
    FFuncRef : TFunctionRefT;
    FFuncTaskRef : TFunctionTaskRefT;
    FAntecedentTask : ITask;
    FFuncEvent : TFunctionEventT;
    procedure RunFunc(Sender: TObject);
  Protected
    function StartFuture: specialize IFuture<T>;
    function GetValue: T;
  Public
    constructor Create(aSender: TObject; aEvent: TFunctionEventT; const aFunc: TFunctionRefT; aPool: TThreadPool); overload;
    constructor Create(aSender: TObject; aEvent: TFunctionEventT; const aFunc: TFunctionRefT; const aCancelToken: TCancellationToken; aPool: TThreadPool); overload;
    constructor CreateTaskContinuation(const aAntecedent: ITask; const aTaskFunc: TFunctionTaskRefT; const aFunc: TFunctionRefT; const aCancelToken: TCancellationToken; aPool: TThreadPool); overload;
    destructor Destroy; override;
    function ContinueWith(const aAction: TFutureProcT): ITask; overload;
    function ContinueWith(const aAction: TFutureProcT; aOptions: TTaskContinuationOptions): ITask; overload;
    function ContinueWith(const aAction: TFutureProcT; const aCancelToken: TCancellationToken; aOptions: TTaskContinuationOptions = []; aPool: TThreadPool = nil): ITask; overload;
    function Map(const aFunc: TMapFuncT; aOptions: TTaskContinuationOptions = []): specialize IFuture<T>; overload;
    function Map(const aFunc: TMapFuncT; const aCancelToken: TCancellationToken; aOptions: TTaskContinuationOptions = []; aPool: TThreadPool = nil): specialize IFuture<T>; overload;
  end;

  { TTransformFuture }

  generic TTransformFuture<T, TResult> = class sealed(specialize TFuture<TResult>)
  public type
    TTransformFunc = specialize TFunc1<T, TResult>;
    TSourceFuture = specialize TFuture<T>;
  private
    FSourceTask: ITask;
    FTransformFunc: TTransformFunc;
    function RunTransform(Sender: TObject): TResult;
  public
    constructor CreateTransform(const aSource: ITask; const aTransform: TTransformFunc;
      const aCancelToken: TCancellationToken; aPool: TThreadPool);
    destructor Destroy; override;
  end;

  { TITaskHelper }

  TITaskHelper = type helper for ITask
    generic function ThenBy<TResult>(const aFunc: specialize TFunc1<ITask, TResult>): specialize IFuture<TResult>; overload;
    generic function ThenBy<TResult>(const aFunc: specialize TFunc1<ITask, TResult>; aOptions: TTaskContinuationOptions): specialize IFuture<TResult>; overload;
    generic function ThenBy<TResult>(const aFunc: specialize TFunc1<ITask, TResult>; const aCancelToken: TCancellationToken; aOptions: TTaskContinuationOptions = []; aPool: TThreadPool = nil): specialize IFuture<TResult>; overload;
    generic function ThenBy<TResult>(const aFunc: specialize TFunc<TResult>): specialize IFuture<TResult>; overload;
    generic function ThenBy<TResult>(const aFunc: specialize TFunc<TResult>; aOptions: TTaskContinuationOptions): specialize IFuture<TResult>; overload;
    generic function ThenBy<TResult>(const aFunc: specialize TFunc<TResult>; const aCancelToken: TCancellationToken; aOptions: TTaskContinuationOptions = []; aPool: TThreadPool = nil): specialize IFuture<TResult>; overload;
    generic function ThenBy<T, TResult>(const aFunc: specialize TFunc1<T, TResult>): specialize IFuture<TResult>; overload;
    generic function ThenBy<T, TResult>(const aFunc: specialize TFunc1<T, TResult>; aOptions: TTaskContinuationOptions): specialize IFuture<TResult>; overload;
    generic function ThenBy<T, TResult>(const aFunc: specialize TFunc1<T, TResult>; const aCancelToken: TCancellationToken; aOptions: TTaskContinuationOptions = []; aPool: TThreadPool = nil): specialize IFuture<TResult>; overload;
  end;

  { TParallel }

  TParallel = class sealed
  public type
    {$MinEnumSize 4}
    TLoopStateFlag = (Exception, Broken, Stopped, Cancelled);
    TLoopStateFlagSet = Set of TLoopStateFlag;
    {$MinEnumSize default}
    const
      ShouldExitFlags = [TLoopStateFlag.Exception, TLoopStateFlag.Stopped, TLoopStateFlag.Cancelled];

    { TLoopState }
  type
    TLoopState = Class;
    TLoopState32 = Class;
    {$IFDEF THREAD64BIT}
    TLoopState64 = Class;
    {$ENDIF}

    TIteratorEvent32 = procedure (aSender: TObject; aIndex: Integer) of object;
    TIteratorStateEvent32 = procedure (aSender: TObject; aIndex: Integer; const aLoopState: TLoopState) of object;
    TIteratorEvent64 = procedure (aSender: TObject; aIndex: Int64) of object;
    TIteratorStateEvent64 = procedure (aSender: TObject; aIndex: Int64; const aLoopState: TLoopState) of object;
    TIteratorEvent = TIteratorEvent32;
    TIteratorStateEvent = TIteratorStateEvent32;
    TInt32LoopStateProc = specialize TProc2<Integer, TLoopState>;
    TInt32Proc = specialize TProc<Integer>;
    TInt64LoopStateProc = specialize TProc2<Int64, TLoopState>;
    TInt64Proc = specialize TProc<Int64>;

    // Global, for the whole loop

    { TInt32LoopProc }

    TInt32LoopProc = Record
      Sender : TObject;
      Event : TIteratorEvent32;
      Proc: TInt32Proc;
      StateEvent: TIteratorStateEvent32;
      ProcWithState: TInt32LoopStateProc;
      CancelToken: TCancellationToken;
      LowInclusive,
      HighExclusive,
      Index,
      Stride: Integer;
      Procedure Execute(Iteration : Integer; aState : TLoopState32);
      Function NumTasks : Integer;
      class function create(aSender: TObject; aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TIteratorEvent32) : TParallel.TInt32LoopProc; static;
      class function create(aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TInt32Proc) : TParallel.TInt32LoopProc; static;
      class function create(aSender: TObject; aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TIteratorStateEvent32) : TParallel.TInt32LoopProc; static;
      class function create(aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TInt32LoopStateProc) : TParallel.TInt32LoopProc; static;
      function ToString : String;
    end;

    {$IFDEF THREAD64BIT}

    { TInt64LoopProc }

    TInt64LoopProc = Record
      Sender : TObject;
      Event : TIteratorEvent64;
      Proc: TInt64Proc;
      StateEvent: TIteratorStateEvent64;
      ProcWithState: TInt64LoopStateProc;
      CancelToken: TCancellationToken;
      LowInclusive,
      HighExclusive,
      Index,
      Stride: Int64;
      Procedure Execute(Iteration : Int64; aState : TLoopState64);
      Function NumTasks : Integer;
      class function create(aSender: TObject; aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TIteratorEvent64) : TParallel.TInt64LoopProc; static;
      class function create(aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TInt64Proc) : TParallel.TInt64LoopProc; static;
      class function create(aSender: TObject; aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TIteratorStateEvent64) : TParallel.TInt64LoopProc; static;
      class function create(aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TInt64LoopStateProc) : TParallel.TInt64LoopProc; static;
      function ToString : String;
    end;

    {$ENDIF}

    { ILoopParams }

    ILoopParams = Interface
      procedure CreateRootTask(aParams : TTask.TTaskParams; aCount : Integer);
      procedure ClearRootTask;
      procedure HandleException;
      Function StartLoop : ITask;
    end;

    { TLoopParams }

    TLoopParams = Class(TInterfacedObject,ILoopParams{$ifndef inlazide},TProcRef{$ENDIF})
    private
      Errors : TExceptionArray;
      ErrorCount : Integer;
      StateFlags : TLoopStateFlagSet;
      FStateLock : TSpinLock;
      FStrideCount : Integer;
      FNextStrideAt : Integer;
      FRootTask: ITask;
      FCancelToken: TCancellationToken;
      FCancelRegistration: TCancellationTokenRegistration;
      procedure OnCancelToken(Sender: TObject);
    public
      Constructor Create;
      Destructor Destroy; override;
      procedure Lock;
      procedure UnLock;
      Procedure HandleException(O : TObject);
      procedure HandleException; overload;
      Function GetBreakAt : Variant; virtual; abstract;
      Procedure Stop;
      Procedure Cancel;
      Function StartLoop : ITask;
      procedure CreateRootTask(aParams : TTask.TTaskParams; aCount : Integer);
      procedure ClearRootTask;
      // We use the fact that in FPC a reference to procedure is an Interface.
      // Invoke is the method of the interface that is called...
      Procedure Invoke; virtual; abstract;
      function Break : Boolean;
      Function Stopped : Boolean;
      Function Cancelled : Boolean;
      Function Faulted : Boolean;
      Property BreakAt : Variant Read GetBreakAt;
    end;

    // Global, for all tasks in the loop

    { IInt32LoopParams }

    TInt32LoopParams = Class (TLoopParams)
    Private
      FFinalFlags : TLoopStateFlagSet;
      FLoopProc : TInt32LoopProc;
      FBreakAt : Integer;
      FMaxStride : Integer;
      Procedure UpdateBreakAt(aValue : Integer);
      Function GetCurrentStride : Integer;
      Function GetCurrentStart(out aStride : Integer) : Integer;
      Function GetNextStride : Integer;
      function ShouldExitLoop(CurrentIter: Integer): Boolean; overload;
      function ShouldExitLoop: Boolean; inline; overload;
    Public
      Constructor Create(aLoopProc : TInt32LoopProc);
      destructor Destroy; override;
      Function GetBreakAt : Variant; override;
      procedure Invoke; override;
      Property Stride : Integer Read FLoopProc.Stride;
      Property HighExclusive : Integer Read FLoopProc.HighExclusive;
      Property LowExclusive : Integer Read FLoopProc.LowInclusive;
      Property Index : Integer Read FLoopProc.Index;
    end;

    { IInt64LoopParams }
    {$IFDEF THREAD64BIT}
    TInt64LoopParams = Class (TLoopParams)
    Private
      FFinalFlags : TLoopStateFlagSet;
      FLoopProc : TInt64LoopProc;
      FBreakAt : Int64;
      FMaxStride : Int64;
      Procedure UpdateBreakAt(aValue : Int64);
      Function GetCurrentStride : Int64;
      Function GetCurrentStart(out aStride : Int64) : Int64;
      Function GetNextStride : int64;
      function ShouldExitLoop(CurrentIter: Int64): Boolean; overload;
      function ShouldExitLoop: Boolean; inline; overload;
    Public
      Constructor Create(aLoopProc : TInt64LoopProc);
      destructor Destroy; override;
      Function GetBreakAt : Variant; override;
      procedure Invoke; override;
      Property Stride : Int64 Read FLoopProc.Stride;
      Property HighExclusive : Int64 Read FLoopProc.HighExclusive;
      Property LowExclusive : Int64 Read FLoopProc.LowInclusive;
      Property Index : Int64 Read FLoopProc.Index;
    end;
    {$ENDIF}


    // Local, per task
    TLoopState = class
    Private
      FLoopParams : TLoopParams;
    protected
      Type
        TLoopStateFlag = TLoopParams;

    protected
      function GetStopped: Boolean; inline;
      function GetCancelled: Boolean; inline;
      function GetFaulted: Boolean; inline;
      function GetLowestBreakIteration: Variant; inline;
      procedure DoBreak; virtual; abstract;

      function DoShouldExit: Boolean; virtual; abstract;
      function DoGetLowestBreakIteration: Variant; virtual;
    public
      constructor Create(LoopParams : TLoopStateFlag);
      procedure Break;
      procedure Stop;
      function ShouldExit: Boolean;

      property Faulted: Boolean read GetFaulted;
      property Stopped: Boolean read GetStopped;
      property Cancelled: Boolean read GetCancelled;
      property LowestBreakIteration: Variant read GetLowestBreakIteration;
    end;

    // Local, per task

    { TLoopState32 }

    TLoopState32 = Class(TLoopState)
    private
      FCurrentIteration: Integer;
    Public
      Constructor Create(aParams: TInt32LoopParams);
      procedure DoBreak; override;
      function DoShouldExit: Boolean; override;
      Property CurrentIteration : Integer read FCurrentIteration Write FCurrentIteration;
    end;

    { TLoopState64 }

    {$IFDEF THREAD64BIT}
    TLoopState64 = Class(TLoopState)
    private
      FCurrentIteration: Int64;
    Public
      Constructor Create(aParams: TInt64LoopParams);
      procedure DoBreak; override;
      function DoShouldExit: Boolean; override;
      Property CurrentIteration : Int64 read FCurrentIteration Write FCurrentIteration;
    end;
    {$ENDIF}

    { TLoopResult }

    TLoopResult = record
    private
      FCompleted: Boolean;
      FLowestBreakIteration: Variant;
    public
      class function Create : TLoopResult; static;
      property Completed: Boolean read FCompleted;
      property LowestBreakIteration: Variant read FLowestBreakIteration;
    end;


  private
    class function Parallelize32(aLoop: TInt32LoopProc; aPool: TThreadPool): TLoopResult;
    {$IFDEF THREAD64BIT}
    class function Parallelize64(aLoop: TInt64LoopProc; aPool: TThreadPool): TLoopResult;
    {$ENDIF}
  public
    Type
      TProcInteger = specialize TProc<Integer>;
      TProcIntegerLoopState = specialize TProc2<Integer,TLoopState>;
      TProcInt64 = specialize TProc<Int64>;
      TProcInt64LoopState = specialize TProc2<Int64,TLoopState>;
    class function &For(aSender: TObject; aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TIteratorEvent): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TIteratorEvent; aPool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TIteratorEvent; const aCancelToken: TCancellationToken; aPool: TThreadPool = nil): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TIteratorStateEvent): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TIteratorStateEvent; aPool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TIteratorStateEvent; const aCancelToken: TCancellationToken; aPool: TThreadPool = nil): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aStride, aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TIteratorEvent): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aStride, aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TIteratorEvent; aPool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aStride, aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TIteratorEvent; const aCancelToken: TCancellationToken; aPool: TThreadPool = nil): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aStride, aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TIteratorStateEvent): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aStride, aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TIteratorStateEvent; aPool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aStride, aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TIteratorStateEvent; const aCancelToken: TCancellationToken; aPool: TThreadPool = nil): TLoopResult; overload; static; inline;
    class function &For(aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TProcInteger): TLoopResult; overload; static; inline;
    class function &For(aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TProcInteger; aPool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TProcInteger; const aCancelToken: TCancellationToken; aPool: TThreadPool = nil): TLoopResult; overload; static; inline;
    class function &For(aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TProcIntegerLoopState): TLoopResult; overload; static; inline;
    class function &For(aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TProcIntegerLoopState; aPool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TProcIntegerLoopState; const aCancelToken: TCancellationToken; aPool: TThreadPool = nil): TLoopResult; overload; static; inline;
    class function &For(aStride, aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TProcInteger): TLoopResult; overload; static; inline;
    class function &For(aStride, aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TProcInteger; aPool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(aStride, aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TProcInteger; const aCancelToken: TCancellationToken; aPool: TThreadPool = nil): TLoopResult; overload; static; inline;
    class function &For(aStride, aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TProcIntegerLoopState): TLoopResult; overload; static; inline;
    class function &For(aStride, aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TProcIntegerLoopState; aPool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(aStride, aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TProcIntegerLoopState; const aCancelToken: TCancellationToken; aPool: TThreadPool = nil): TLoopResult; overload; static; inline;
    {$IFDEF THREAD64BIT}
    class function &For(aSender: TObject; aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TIteratorEvent64): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TIteratorEvent64; aPool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TIteratorEvent64; const aCancelToken: TCancellationToken; aPool: TThreadPool = nil): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TIteratorStateEvent64): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TIteratorStateEvent64; aPool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TIteratorStateEvent64; const aCancelToken: TCancellationToken; aPool: TThreadPool = nil): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aStride, aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TIteratorEvent64): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aStride, aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TIteratorEvent64; aPool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aStride, aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TIteratorEvent64; const aCancelToken: TCancellationToken; aPool: TThreadPool = nil): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aStride, aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TIteratorStateEvent64): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aStride, aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TIteratorStateEvent64; aPool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aStride, aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TIteratorStateEvent64; const aCancelToken: TCancellationToken; aPool: TThreadPool = nil): TLoopResult; overload; static; inline;
    class function &For(aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TProcInt64): TLoopResult; overload; static; inline;
    class function &For(aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TProcInt64; aPool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TProcInt64; const aCancelToken: TCancellationToken; aPool: TThreadPool = nil): TLoopResult; overload; static; inline;
    class function &For(aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TProcInt64LoopState): TLoopResult; overload; static; inline;
    class function &For(aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TProcInt64LoopState; aPool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TProcInt64LoopState; const aCancelToken: TCancellationToken; aPool: TThreadPool = nil): TLoopResult; overload; static; inline;
    class function &For(aStride, aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TProcInt64): TLoopResult; overload; static; inline;
    class function &For(aStride, aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TProcInt64; aPool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(aStride, aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TProcInt64; const aCancelToken: TCancellationToken; aPool: TThreadPool = nil): TLoopResult; overload; static; inline;
    class function &For(aStride, aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TProcInt64LoopState): TLoopResult; overload; static; inline;
    class function &For(aStride, aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TProcInt64LoopState; aPool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(aStride, aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TProcInt64LoopState; const aCancelToken: TCancellationToken; aPool: TThreadPool = nil): TLoopResult; overload; static; inline;
    {$ENDIF}
    class function Join(aSender: TObject; aEvents: array of TNotifyEvent): ITask; overload; static;
    class function Join(aSender: TObject; aEvents: array of TNotifyEvent; aPool: TThreadPool): ITask; overload; static;
    class function Join(aSender: TObject; aEvents: array of TNotifyEvent; const aCancelToken: TCancellationToken; aPool: TThreadPool = nil): ITask; overload; static;
    class function Join(aSender: TObject; aEvent1, aEvent2: TNotifyEvent): ITask; overload; static; inline;
    class function Join(aSender: TObject; aEvent1, aEvent2: TNotifyEvent; aPool: TThreadPool): ITask; overload; static;
    class function Join(const aProcs: array of TProcRef): ITask; overload; static;
    class function Join(const aProcs: array of TProcRef; aPool: TThreadPool): ITask; overload; static;
    class function Join(const aProcs: array of TProcRef; const aCancelToken: TCancellationToken; aPool: TThreadPool = nil): ITask; overload; static;
    class function Join(const aProc1, aProc2: TProcRef): ITask; overload; static; inline;
    class function Join(const aProc1, aProc2: TProcRef; aPool: TThreadPool): ITask; overload; static;
  end;

function GetThreadPoolInteractive(APool: TThreadPool): Boolean;
procedure SetThreadPoolInteractive(APool: TThreadPool; AValue: Boolean);

{
  These must be exposed, otherwise they cannot be used inside generic methods :/

  At optimization level 1, the {$IFDEF USE_THREADLOG}ThreadLog is not called at all if the routine is empty.
  So if DEBUGTHREADPOOL is not defined, we must ensure the methods are empty.
  without optimization, the methods are called but will not do anything.
}
{$IFDEF USE_THREADLOG}
procedure ThreadLog(const Method,Msg: string); overload;
procedure ThreadLog(const Method,Fmt: string; Args: array of const); overload;
{$ENDIF}


implementation

uses system.diagnostics;

Resourcestring
  SWorkerThreadName = 'Worker Thread - %s #%d ThreadPool - %p';
  SAggregateException = 'Aggregate exception';
  SOperationCancelled = 'Operation cancelled';
  SCannotStartCompletedTask = 'Cannot start completed task';
  SErrBreakAfterStop = 'Break loop after loop was stopped';
  SErrInvalidTaskConstructor = 'Cannot use parameterless TTask constructor';
  SErrOneOrMoreTasksCancelled = 'One or more tasks where cancelled';
  SAggregateExceptionCount = 'Aggregate exception for %d exceptions';

Type
  TSpinLockHelper = record helper for TSpinLock
    procedure leave;
  end;

procedure TSpinLockHelper.leave;
begin
  Exit;
end;


{$IFDEF USE_THREADLOG}
procedure ThreadLog(const Method,Msg: string); overload;

{$IFDEF DEBUGTHREADPOOL}
var
  TID : String;
{$ENDIF}
begin
{$IFDEF DEBUGTHREADPOOL}
  if TThread.CurrentThread.ThreadID = MainThreadID then
    TID:='Main Thread'
  else
    TID:=IntToStr(PtrInt(TThread.CurrentThread.ThreadID));
  Writeln('[',TID:15,']{',Method,'} ',Msg);
  Flush(output);
{$ENDIF}
end;

procedure ThreadLog(const Method,Fmt: string; Args: array of const); overload;
begin
{$IFDEF DEBUGTHREADPOOL}
  ThreadLog(Method,SafeFormat(Fmt,Args));{}
{$ENDIF}
end;

{$ENDIF USE_THREADLOG}

Function BToS(B : Boolean) : String;
begin
  Result:=BoolToStr(B,True);
end;


function GetThreadPoolInteractive(APool: TThreadPool): Boolean;

begin
  Result:=aPool.FInteractive;
end;

procedure SetThreadPoolInteractive(APool: TThreadPool; AValue: Boolean);

begin
  aPool.FInteractive:=aValue;
end;

{ *********************************************************************
  Private classes, not part of interface.
  *********************************************************************}


Type

  { TReplicableTask }

  TReplicableTask = class(TTask)
  private
    FTaskCount: Integer;
  protected
    function ShouldCreateReplica: Boolean; override;
    function CreateReplicaTask(const aParams : TTaskParams): TTask; override;
  Public
    constructor Create(const aParams : TTaskParams; aTaskCount: Integer); overload;
  end;

  { TReplicatedTask }

  TReplicatedTask = class(TTask)
  end;

  { TProcJoinTask }

  TProcJoinTask = class(TReplicableTask)
    FProc : TParallel.TInt32LoopProc;
    FProcList : array of TProcref;
    constructor Create(const AProcs: array of TProcRef; APool: TThreadPool); overload;
    constructor Create(const AProcs: array of TProcRef; const ACancelToken: TCancellationToken; APool: TThreadPool); overload;
  private
    procedure JoinTasks;
  end;

  { TEventJoinTask }

  TEventJoinTask = class(TReplicableTask)
    FProc : TParallel.TInt32LoopProc;
    FEventList : array of TNotifyEvent;
    constructor Create(Sender: TObject; const AEvents : array of TNotifyEvent; APool: TThreadPool); overload;
    constructor Create(Sender: TObject; const AEvents : array of TNotifyEvent; const ACancelToken: TCancellationToken; APool: TThreadPool); overload;
  private
    procedure JoinTasks;
  end;

  { TCancellationState }

  TCancellationState = class(TInterfacedObject, ICancellationState)
  private
    type
      TCallbackEntry = record
        Id: Int64;
        Callback: TProcRef;
        Event: TNotifyEvent;
        Sender: TObject;
      end;

      { TTimerThread }

      TTimerThread = class(TThread)
      private
        FState: TCancellationState;
        FAbortEvent: TEvent;
        FDelayMs: Cardinal;
      protected
        procedure Execute; override;
      public
        constructor Create(aState: TCancellationState; aDelayMs: Cardinal);
        destructor Destroy; override;
        procedure AbortTimer;
      end;
  private
    FCanceled: Integer;
    FExecutingCount: Integer;
    FExecutingThreadID: TThreadID;
    FNextId: Int64;
    FLock: TSpinLock;
    FWaitEvent: TEvent;
    FCallbacks: array of TCallbackEntry;
    FCallbackCount: Integer;
    FTimerThread: TTimerThread;
    procedure StopTimer;
  public
    constructor Create;
    destructor Destroy; override;
    function IsCancellationRequested: Boolean;
    function GetWaitHandle: TEvent;
    procedure Cancel;
    procedure CancelAfter(aMillisecondsDelay: Cardinal);
    function RegisterCallback(const aCallback: TProcRef; aEvent: TNotifyEvent; aSender: TObject): Int64;
    procedure UnregisterCallback(aId: Int64);
  end;

{ *********************************************************************
  TCancellationState & TCancellationToken & TCancellationTokenSource
  *********************************************************************}

constructor TCancellationState.TTimerThread.Create(aState: TCancellationState; aDelayMs: Cardinal);
begin
  FState:=aState;
  FDelayMs:=aDelayMs;
  FAbortEvent:=TEvent.Create(nil,True,False,'');
  FreeOnTerminate:=False;
  inherited Create(False);
end;

destructor TCancellationState.TTimerThread.Destroy;
begin
  FreeAndNil(FAbortEvent);
  inherited Destroy;
end;

procedure TCancellationState.TTimerThread.AbortTimer;
begin
  if Assigned(FAbortEvent) then
    FAbortEvent.SetEvent;
end;

procedure TCancellationState.TTimerThread.Execute;
begin
  if FAbortEvent.WaitFor(FDelayMs)=wrTimeout then
    if Assigned(FState) then
      FState.Cancel;
end;

constructor TCancellationState.Create;
begin
  inherited Create;
  FCanceled:=0;
  FExecutingCount:=0;
  FExecutingThreadID:=TThreadID(0);
  FNextId:=0;
  FLock:=TSpinLock.Create(False);
  FWaitEvent:=nil;
  FCallbacks:=nil;
  FCallbackCount:=0;
  FTimerThread:=nil;
  {$IFDEF FPC_VALGRIND}
  VgBenignRace(@FCanceled,SizeOf(FCanceled));
  VgBenignRace(@FExecutingCount,SizeOf(FExecutingCount));
  VgBenignRace(@FExecutingThreadID,SizeOf(FExecutingThreadID));
  {$ENDIF}
end;

destructor TCancellationState.Destroy;
begin
  StopTimer;
  FreeAndNil(FWaitEvent);
  inherited Destroy;
end;

procedure TCancellationState.StopTimer;
var
  Timer: TTimerThread;
begin
  Timer:=nil;
  FLock.Enter;
  try
    if Assigned(FTimerThread) and (TThread.CurrentThread<>FTimerThread) then
      begin
      Timer:=FTimerThread;
      FTimerThread:=nil;
      end;
  finally
    FLock.Exit;
  end;
  if Assigned(Timer) then
    begin
    Timer.AbortTimer;
    Timer.WaitFor;
    Timer.Free;
    end;
end;

function TCancellationState.IsCancellationRequested: Boolean;
begin
  Result:=FCanceled<>0;
  if Result then
    begin
    ReadBarrier;
    {$IFDEF FPC_VALGRIND}
    VgHappensAfter(@FCanceled);
    {$ENDIF}
    end;
end;

function TCancellationState.GetWaitHandle: TEvent;
begin
  FLock.Enter;
  try
    if not Assigned(FWaitEvent) then
      FWaitEvent:=TEvent.Create(nil,True,FCanceled<>0,'');
    Result:=FWaitEvent;
  finally
    FLock.Exit;
  end;
end;

procedure TCancellationState.Cancel;
var
  LocalCallbacks: array of TCallbackEntry;
  Count, I: Integer;
begin
  {$IFDEF FPC_VALGRIND}
  VgHappensBefore(@FCanceled);
  {$ENDIF}
  WriteBarrier;
  if AtomicCmpExchange(FCanceled,1,0)<>0 then
    begin
    StopTimer;
    Exit;
    end;
  LocalCallbacks:=nil;
  Count:=0;
  FLock.Enter;
  try
    if Assigned(FWaitEvent) then
      FWaitEvent.SetEvent;
    if Assigned(FTimerThread) then
      FTimerThread.AbortTimer;
    Count:=FCallbackCount;
    if Count>0 then
      begin
      LocalCallbacks:=Copy(FCallbacks,0,Count);
      FCallbacks:=nil;
      FCallbackCount:=0;
      FExecutingThreadID:=GetCurrentThreadId;
      AtomicIncrement(FExecutingCount);
      end;
  finally
    FLock.Exit;
  end;
  StopTimer;
  if Count>0 then
    try
      for I:=0 to Count-1 do
        begin
        if Assigned(LocalCallbacks[I].Callback) then
          LocalCallbacks[I].Callback()
        else if Assigned(LocalCallbacks[I].Event) then
          LocalCallbacks[I].Event(LocalCallbacks[I].Sender);
        end;
    finally
      {$IFDEF FPC_VALGRIND}
      VgHappensBefore(@FExecutingCount);
      {$ENDIF}
      AtomicDecrement(FExecutingCount);
    end;
end;

procedure TCancellationState.CancelAfter(aMillisecondsDelay: Cardinal);
begin
  if IsCancellationRequested then
    Exit;
  if aMillisecondsDelay=0 then
    begin
    Cancel;
    Exit;
    end;
  StopTimer;
  FLock.Enter;
  try
    if FCanceled=0 then
      FTimerThread:=TTimerThread.Create(Self,aMillisecondsDelay);
  finally
    FLock.Exit;
  end;
end;

function TCancellationState.RegisterCallback(const aCallback: TProcRef; aEvent: TNotifyEvent; aSender: TObject): Int64;
var
  AlreadyCanceled: Boolean;
begin
  Result:=0;
  if IsCancellationRequested then
    begin
    if Assigned(aCallback) then
      aCallback()
    else if Assigned(aEvent) then
      aEvent(aSender);
    Exit;
    end;
  AlreadyCanceled:=False;
  FLock.Enter;
  try
    if FCanceled<>0 then
      AlreadyCanceled:=True
    else
      begin
      Inc(FNextId);
      Result:=FNextId;
      if FCallbackCount=Length(FCallbacks) then
        SetLength(FCallbacks,FCallbackCount+8);
      FCallbacks[FCallbackCount].Id:=Result;
      FCallbacks[FCallbackCount].Callback:=aCallback;
      FCallbacks[FCallbackCount].Event:=aEvent;
      FCallbacks[FCallbackCount].Sender:=aSender;
      Inc(FCallbackCount);
      end;
  finally
    FLock.Exit;
  end;
  if AlreadyCanceled then
    begin
    if Assigned(aCallback) then
      aCallback()
    else if Assigned(aEvent) then
      aEvent(aSender);
    end;
end;

procedure TCancellationState.UnregisterCallback(aId: Int64);
var
  I: Integer;
  Found: Boolean;
  Spinner: TSpinWait;
begin
  if aId=0 then
    Exit;
  Found:=False;
  FLock.Enter;
  try
    for I:=FCallbackCount-1 downto 0 do
      if FCallbacks[I].Id=aId then
        begin
        if I<FCallbackCount-1 then
          FCallbacks[I]:=FCallbacks[FCallbackCount-1];
        FCallbacks[FCallbackCount-1]:=Default(TCallbackEntry);
        Dec(FCallbackCount);
        Found:=True;
        Break;
        end;
  finally
    FLock.Exit;
  end;
  if not Found and (FCanceled<>0) and (GetCurrentThreadId<>FExecutingThreadID) then
    begin
    Spinner.Reset;
    while AtomicCmpExchange(FExecutingCount,0,0)>0 do
      Spinner.SpinCycle;
    {$IFDEF FPC_VALGRIND}
    VgHappensAfter(@FExecutingCount);
    {$ENDIF}
    end;
end;

procedure TCancellationTokenRegistration.Unregister;
begin
  if Assigned(FState) and (FId<>0) then
    begin
    FState.UnregisterCallback(FId);
    FState:=nil;
    FId:=0;
    end;
end;

procedure TCancellationTokenRegistration.Dispose;
begin
  Unregister;
end;

class function TCancellationToken.None: TCancellationToken;
begin
  Result.FState:=nil;
end;

function TCancellationToken.GetCanBeCanceled: Boolean;
begin
  Result:=Assigned(FState);
end;

function TCancellationToken.GetIsCancellationRequested: Boolean;
begin
  Result:=Assigned(FState) and FState.IsCancellationRequested;
end;

function TCancellationToken.GetWaitHandle: TEvent;
begin
  if Assigned(FState) then
    Result:=FState.GetWaitHandle
  else
    Result:=nil;
end;

procedure TCancellationToken.ThrowIfCancellationRequested;
begin
  if GetIsCancellationRequested then
    raise EOperationCancelled.Create(SOperationCancelled);
end;

procedure TCancellationToken.CheckCanceled;
begin
  ThrowIfCancellationRequested;
end;

function TCancellationToken.Register(const aCallback: TProcRef): TCancellationTokenRegistration;
begin
  Result:=Default(TCancellationTokenRegistration);
  if Assigned(FState) and Assigned(aCallback) then
    begin
    Result.FState:=FState;
    Result.FId:=FState.RegisterCallback(aCallback,nil,nil);
    end;
end;

function TCancellationToken.Register(aEvent: TNotifyEvent; aSender: TObject): TCancellationTokenRegistration;
begin
  Result:=Default(TCancellationTokenRegistration);
  if Assigned(FState) and Assigned(aEvent) then
    begin
    Result.FState:=FState;
    Result.FId:=FState.RegisterCallback(nil,aEvent,aSender);
    end;
end;

class operator TCancellationToken.=(const aLeft, aRight: TCancellationToken): Boolean;
begin
  Result:=aLeft.FState=aRight.FState;
end;

class operator TCancellationToken.<>(const aLeft, aRight: TCancellationToken): Boolean;
begin
  Result:=aLeft.FState<>aRight.FState;
end;

constructor TCancellationTokenSource.Create;
begin
  inherited Create;
  FState:=TCancellationState.Create;
  FLinkedRegistrations:=nil;
end;

constructor TCancellationTokenSource.Create(aMillisecondsDelay: Cardinal);
begin
  Create;
  CancelAfter(aMillisecondsDelay);
end;

constructor TCancellationTokenSource.Create(const aDelay: TTimeSpan);
var
  Ms: Int64;
begin
  Create;
  Ms:=Trunc(aDelay.TotalMilliseconds);
  if Ms<0 then
    Ms:=0;
  CancelAfter(Cardinal(Ms));
end;

destructor TCancellationTokenSource.Destroy;
var
  I: Integer;
begin
  for I:=0 to Length(FLinkedRegistrations)-1 do
    FLinkedRegistrations[I].Unregister;
  FLinkedRegistrations:=nil;
  if Assigned(FState) then
    (FState as TCancellationState).StopTimer;
  FState:=nil;
  inherited Destroy;
end;

procedure TCancellationTokenSource.OnLinkedTokenCancel(Sender: TObject);
begin
  if Sender=nil then;
  Cancel;
end;

class function TCancellationTokenSource.CreateLinkedTokenSource(const aToken1, aToken2: TCancellationToken): TCancellationTokenSource;
begin
  Result:=CreateLinkedTokenSource([aToken1,aToken2]);
end;

class function TCancellationTokenSource.CreateLinkedTokenSource(const aTokens: array of TCancellationToken): TCancellationTokenSource;
var
  I, Count: Integer;
begin
  Result:=TCancellationTokenSource.Create;
  SetLength(Result.FLinkedRegistrations,Length(aTokens));
  Count:=0;
  for I:=0 to Length(aTokens)-1 do
    if aTokens[I].CanBeCanceled then
      begin
      Result.FLinkedRegistrations[Count]:=aTokens[I].Register(@Result.OnLinkedTokenCancel,Result);
      Inc(Count);
      end;
  SetLength(Result.FLinkedRegistrations,Count);
end;

function TCancellationTokenSource.GetIsCancellationRequested: Boolean;
begin
  Result:=Assigned(FState) and FState.IsCancellationRequested;
end;

function TCancellationTokenSource.GetToken: TCancellationToken;
begin
  Result.FState:=FState;
end;

function TCancellationTokenSource.GetWaitHandle: TEvent;
begin
  if Assigned(FState) then
    Result:=FState.GetWaitHandle
  else
    Result:=nil;
end;

procedure TCancellationTokenSource.Cancel;
begin
  if Assigned(FState) then
    FState.Cancel;
end;

procedure TCancellationTokenSource.CancelAfter(aMillisecondsDelay: Cardinal);
begin
  if Assigned(FState) then
    FState.CancelAfter(aMillisecondsDelay);
end;

procedure TCancellationTokenSource.CancelAfter(const aDelay: TTimeSpan);
var
  Ms: Int64;
begin
  Ms:=Trunc(aDelay.TotalMilliseconds);
  if Ms<0 then
    Ms:=0;
  CancelAfter(Cardinal(Ms));
end;

procedure TCancellationTokenSource.Reset;
var
  I: Integer;
begin
  for I:=0 to Length(FLinkedRegistrations)-1 do
    FLinkedRegistrations[I].Unregister;
  FLinkedRegistrations:=nil;
  if Assigned(FState) then
    (FState as TCancellationState).StopTimer;
  FState:=TCancellationState.Create;
end;



{ *********************************************************************
  TExceptionList
  *********************************************************************}

class function TExceptionList.Create(aCapacity: Integer): TExceptionList;
begin
  Result:=Default(TExceptionList);
  SetLength(Result.List,aCapacity);
  Result.Count:=0;
end;

class function TExceptionList.Create(Initial: Exception; aCapacity: Integer): TExceptionList;
begin
  Result:=Create(aCapacity);
  Result.Flatten(Initial);
end;

class function TExceptionList.Create(aExceptionArray: array of Exception): TExceptionList;

var
  I,Len : Integer;
begin
  Len:=Length(aExceptionArray);
  Result:=Create(Len+1); // spare
  For I:=0 to Len-1 do
    Result.List[i]:=aExceptionArray[i];
  Result.Count:=Len;
end;

procedure TExceptionList.Flatten(aException : Exception);

var
  lList : TExceptionList;
  I : Integer;
  Agg : EAggregateException absolute aException;

begin
  if Not (aException is EAggregateException) then
    Add(aException)
  else
    begin
    lList:=Agg.Flist;
    Agg.Flist:=TExceptionList.Create(0);
    GrowCapacity(Count+lList.Count);
    For I:=0 to lList.Count-1 do
      Flatten(lList.List[i]);
    end;
end;

procedure TExceptionList.AddFromTask(aTask: TTask);

begin
  if not (aTask.FException is Exception) then
    FreeAndNil(aTask.FException)
  else
    begin
    Flatten(Exception(aTask.FException));
    if aTask.FException is EAggregateException then
      FreeAndNil(aTask.FException)
    else
      aTask.FException:=Nil;
    end;
end;

procedure TExceptionList.Add(aException: Exception);
begin
  If Count=Length(List) then
    SetLength(List,Count+10);
  List[Count]:=aException;
  Inc(Count);
end;

function TExceptionList.GrowCapacity(aMinCapacity: Integer): Integer;
begin
  If aMinCapacity>Length(List) then
    SetLength(List,aMinCapacity);
  Result:=Length(List);
end;

function TExceptionList.Capacity: Integer;
begin
  Result:=Length(List);
end;

function TExceptionList.Truncate: TExceptionArray;
begin
  SetLength(List,Count);
  Result:=List;
end;

procedure TExceptionList.ClearList;
begin
  While Count>0 do
    begin
    Dec(Count);
    FreeAndNil(List[Count]);
    end;
end;

{ *********************************************************************
  EAggregateException
  *********************************************************************}



function EAggregateException.GetInnerException(aIndex: Integer): Exception;
begin
  Result:=Exception(FList.List[aIndex]);
end;

constructor EAggregateException.Create(const aExceptionArray: array of Exception);
begin
  Create(SAggregateException,aExceptionArray);
end;

constructor EAggregateException.Create(const aMessage: string; const aExceptionArray: array of Exception);

begin
  Inherited Create(aMessage);
  Flist:=TExceptionList.Create(aExceptionArray);
end;

Procedure EAggregateException.ClearList;

begin
  FList.ClearList;
end;

destructor EAggregateException.Destroy;
begin
  ClearList;
  inherited Destroy;
end;

function EAggregateException.GetEnumerator: TExceptionEnumerator;
begin
  // FList.List has spare capacity (Add grows it by 10): only the first
  // FList.Count elements are exceptions.
  Result:=TExceptionEnumerator.Create(Copy(Self.FList.List,0,Self.FList.Count))
end;

procedure EAggregateException.Handle(aExceptionHandlerEvent: TExceptionHandlerEvent);

  procedure DoEvent(const aException: Exception; var aHandled: Boolean);

  begin
    aExceptionHandlerEvent(aException,aHandled);
  end;

begin
  Handle(TExceptionHandlerProc(@DoEvent));
end;

procedure EAggregateException.Handle(const aExceptionHandlerProc: TExceptionHandlerProc);

var
  I : Integer;
  Handled: Boolean;
  E : Exception;
  OurList,Unhandled: TExceptionList;

begin
  OurList:=TExceptionList.Create(Count);
  Unhandled:=TExceptionList.Create(Count);
  for I:=0 to FList.Count-1 do
    begin
    Handled:=False;
    E:=FList.List[i];
    AExceptionHandlerProc(E,Handled);
    if Handled then
      OurList.Add(E)
    else
      UnHandled.Add(E)
    end;
  // In case of an exception during proc, we still own all exceptions.
  if Unhandled.Count>0 then
    begin
    // When we got here, unhandled ones will be owned by new exception.
    // Make sure we still own the handled ones !
    FList:=OurList;
    raise EAggregateException.Create(Message,UnHandled.Truncate);
    end;
end;

procedure EAggregateException.Add(aException: Exception);
begin
  Flist.Add(aException);
end;

function EAggregateException.ToString: RTLString;
var
  S : String;
  I, Len: Integer;
begin
  S:=inherited ToString;
  S:=S+sLineBreak+Format(SAggregateExceptionCount,[Count]);
  Len:=MaxLoggedExceptions;
  if Count<Len then
    Len:=Count;
  for I:=0 to Len-1 do
    S:=S+sLineBreak+Format('#%d %s',[I,InnerExceptions[I].ToString]);
  if Count>Len then
    S:=S+sLineBreak+'...';
  Result:=S;
end;

{ *********************************************************************
  EAggregateException.TExceptionEnumerator
  *********************************************************************}

function EAggregateException.TExceptionEnumerator.GetCurrent: Exception;
begin
  Result:=Exception(FList[FCurrent]);
end;

constructor EAggregateException.TExceptionEnumerator.Create(aList: TExceptionArray);
begin
  FList:=aList;
  FCurrent:=-1;
end;

function EAggregateException.TExceptionEnumerator.MoveNext: Boolean;
begin
  // Advance first, then check: FCurrent starts at -1.
  Inc(FCurrent);
  Result:=FCurrent<Length(FList);
end;

{ *********************************************************************
  TSparseArray
  *********************************************************************}

constructor TSparseArray.Create(aInitialSize: Integer);
begin
  FLock:=TSpinLock.Create(False);
  if aInitialSize < 1 then
    aInitialSize:=1;
  SetLength(FArray,aInitialSize);
end;

destructor TSparseArray.Destroy;
begin
  inherited Destroy;
end;

procedure TSparseArray.Lock;
begin
  FLock.Enter;
end;

procedure TSparseArray.Unlock;
begin
  FLock.Exit;
end;

function TSparseArray.Add(const aItem: T): Integer;

var
  I,Len: Integer;
  Tmp : TArrayOfT;

begin
  Tmp:=Default(TArrayOfT);
  Lock;
  try
    I:=0;
    Len:=Length(FArray);
    While (I<Len) do
      begin
      if Not Assigned(FArray[i]) then
        begin
        FArray[i]:=aItem;
        Exit(I);
        end;
      Inc(I);
      end;
    SetLength(Tmp,Len*2);
    Move(Farray[0],Tmp[0],Len*SizeOf(T));
    FArray:=Tmp;
    FArray[Len]:=aItem;
    Result:=Len;
  finally
    UnLock;
  end;
end;


function TSparseArray.Remove(const aItem: T): Boolean;

var
  I: Integer;

begin
  Lock;
  try
    I:=Length(FArray)-1;
    While (I>=0) and (FArray[I]<>aItem) do
      Dec(I);
    Result:=(I>=0);
    if Result then
      FArray[I]:=nil;
  finally
    Unlock;
  end;
end;

{ *********************************************************************
  TWorkStealingQueue
  *********************************************************************}


function TWorkStealingQueue.GetCount: Integer;
begin
  Lock;
  try
    Result:=FItems.Count;
  finally
    UnLock;
  end;
end;

function TWorkStealingQueue.GetIsEmpty: Boolean;
begin
  Result:=GetCount=0;
end;

procedure TWorkStealingQueue.Lock;
begin
  {$IFDEF USE_THREADLOG}ThreadLog('TWorkStealingQueue.Lock','Enter %d',[PtrInt(Self)]);{$ENDIF USE_THREADLOG}
  try
    FLock.Enter;
  except
    on E : Exception do
	  begin
          {$IFDEF USE_THREADLOG}ThreadLog('TWorkStealingQueue.Lock','%d Exception: %s %s',[PtrInt(Self),E.ClassName,E.Message]);{$ENDIF USE_THREADLOG}
          Raise;
	  end;
  end;
  {$IFDEF USE_THREADLOG}ThreadLog('TWorkStealingQueue.Lock','Leave %d',[PtrInt(Self)]);{$ENDIF USE_THREADLOG}
end;

procedure TWorkStealingQueue.UnLock;
begin
  {$IFDEF USE_THREADLOG}ThreadLog('TWorkStealingQueue.UnLock','Enter %d',[PtrInt(Self)]);{$ENDIF USE_THREADLOG}
  FLock.Exit;
  {$IFDEF USE_THREADLOG}ThreadLog('TWorkStealingQueue.UnLock','Leave %d',[PtrInt(Self)]);{$ENDIF USE_THREADLOG}
end;

constructor TWorkStealingQueue.Create;
begin
  {$IFDEF USE_THREADLOG}ThreadLog('TWorkStealingQueue.Create',IntToStr(PtrInt(Self)));{$ENDIF USE_THREADLOG}
  FItems:=TItemList.Create;
  FLock:=TSpinLock.Create(False);
  FEvent:=TEvent.Create(False);
end;

destructor TWorkStealingQueue.Destroy;
begin
  {$IFDEF USE_THREADLOG}ThreadLog('TWorkStealingQueue.Destroy',IntToStr(PtrInt(Self)));{$ENDIF USE_THREADLOG}
  FreeAndNil(FItems);
  FreeAndNil(FEvent);
  inherited Destroy;
end;

function TWorkStealingQueue.LocalFindAndRemove(const aItem: T): Boolean;

begin
  Lock;
  try
    Result:=FItems.Remove(aItem)<>-1;
  finally
    UnLock
  end;
end;

procedure TWorkStealingQueue.LocalPush(const aItem: T);
begin
  Lock;
  try
    FItems.Add(aItem);
    FEvent.SetEvent;
  finally
    UnLock;
  end;
end;

function TWorkStealingQueue.LocalPop(out aItem: T): Boolean;

begin
  Lock;
  try
    Result:=FItems.Count>0;
    if Result then
      aItem:=FItems.ExtractIndex(FItems.Count-1);
  finally
    UnLock;
  end;
end;

function TWorkStealingQueue.TrySteal(out aItem: T; aTimeout: Cardinal): Boolean;
begin
  Result:=LocalPop(aItem);
  // No waiting requested: do not touch the event. The pool calls this for
  // every queue while holding its queue-list spin lock; the two event system
  // calls per empty queue made idle workers contend on that lock until
  // TSpinWait fell back to Sleep(1), so workers joined a loop one by one.
  If Result or (aTimeout=0) then
    exit;
  FEvent.ResetEvent;
  if FEvent.WaitFor(aTimeOut)=wrSignaled then
    Result:=LocalPop(aItem);
  // We can miss one if another thread got the item. Normally we'd need to wait again till timeout is really over.
end;

function TWorkStealingQueue.Remove(const aItem: T): Boolean;
begin
  Lock;
  try
    Result:=FItems.Remove(aItem)<>-1;
  finally
    UnLock;
  end;
end;

{ *********************************************************************
  TObjectCache
  *********************************************************************}

constructor TObjectCache.Create(aClass: TClass);
begin
  FItemClass:=aClass;
  FStack:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}Contnrs.TStack.Create();
  FLock:=TSpinLock.Create(False);
end;

destructor TObjectCache.Destroy;
begin
  Clear;
  FreeAndNil(FStack);
  inherited Destroy;
end;

procedure TObjectCache.Clear;

var
  P : Pointer;

begin
  FLock.Enter;
  try
    P:=FStack.Pop;
    While P<>Nil do
      begin
      FreeMem(P);
      P:=FStack.Pop;
      end;
  finally
    FLock.Exit;
  end;
end;

function TObjectCache.Insert(Instance: Pointer): Boolean;
begin
  FLock.Enter;
  try
    Result:=FStack.Count<CObjectCacheLimit;
    if Result then
      FStack.Push(Instance);
  finally
    FLock.Exit;
  end;
end;

function TObjectCache.Remove: Pointer;

begin
  FLock.Enter;
  try
    Result:=FStack.Pop;
  finally
    FLock.Exit;
  end;
end;

function TObjectCache.Count: Integer;
begin
  Result:=FStack.Count;
end;

{ *********************************************************************
  TObjectCaches
  *********************************************************************}

procedure TObjectCaches.AddObjectCache(aClass: TClass);
begin
  Add(aClass,TObjectCache.Create(aClass));
end;

{ *********************************************************************
  TThreadPool
  *********************************************************************}

class function TThreadPool.GetCurrentThreadPool: TThreadPool; static;

var
  Task: ITask;

begin
  Task:=TTask.CurrentTask;
  if Assigned(Task) then
    Result := (Task as tTask).ThreadPool
  else
    Result := TThreadPool.Default;
end;



procedure TThreadPool.WorkQueued;

var
  DoEventSignal : Boolean;

begin
  // Notify waiting threads.
  {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.WorkQueued','enter');{$ENDIF USE_THREADLOG}
  AtomicIncrement(FRequestCount);
  {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.WorkQueued','Queueing work (Requests: %d)',[FRequestCount]);{$ENDIF USE_THREADLOG}
  DoEventSignal:=FIdleThreads>=FRequestCount;
  {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.WorkQueued','DoEventSignal %s (%d>%d)',[BToS(DoEventSignal),FIdleThreads,FRequestCount]);{$ENDIF USE_THREADLOG}
  if DoEventSignal then
    FQueueEvent.SetEvent
  else
    GrowPool;
  {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.WorkQueued','leave');{$ENDIF USE_THREADLOG}
end;

procedure TThreadPool.GrowPool;

  procedure DoAdd;

  begin
    {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.GrowPool.DoAdd','Enter');{$ENDIF USE_THREADLOG}
    LockQueue;
    try
      AddThreadToPool;
    finally
      UnlockQueue;
      {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.GrowPool.DoAdd','Leave');{$ENDIF USE_THREADLOG}
    end;
  end;

Var
  DoGrow,NeedMinimum,IdleDeficit,HaveRoom : Boolean;

begin
  {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.GrowPool','Enter');{$ENDIF USE_THREADLOG}
  NeedMinimum:=(FThreadCount<FMinThreads);
  IdleDeficit:=(FIdleThreads<FRequestCount);
  HaveRoom:=(FThreadCount<FMaxThreads);
  DoGrow:=NeedMinimum or (IdleDeficit and HaveRoom);
  {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.GrowPool','DoGrow: %s, NeedMinimum: %s, IdleDeficit: %s, HaveRoom: %s',[BToS(DoGrow),BToS(NeedMinimum),BToS(IdleDeficit),BToS(HaveRoom)]);{$ENDIF USE_THREADLOG}
  if Not DoGrow then
    begin
    {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.GrowPool','Leave (not DoGrow)');{$ENDIF USE_THREADLOG}
    exit;
    end;
  if FRetiring>0 then
     begin
     {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.GrowPool','Waking retired threads: %d',[FRetiring]);{$ENDIF USE_THREADLOG}
     FRetireEvent.SetEvent;
     end
  else
    begin
    DoAdd;
    while (FThreadCount<FMinThreads) do
      begin
      {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.GrowPool','Adding thread to pool: %d<%d',[FThreadCount,FMinThreads]);{$ENDIF USE_THREADLOG}
      DoAdd;
      end;
    end;
  {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.GrowPool','Leave');{$ENDIF USE_THREADLOG}
end;


procedure TThreadPool.NewThread(aThread: TBaseWorkerThread);
begin
  if Assigned(FThreads) then
    FThreads.Add(aThread);
  if assigned(FOnThreadStart) then
    FOnThreadStart(aThread);
end;

procedure TThreadPool.RemoveThread(aThread: TBaseWorkerThread);
begin
  If Assigned(FThreads) then
    FThreads.Remove(aThread);
  AtomicDecrement(FThreadCount);
  {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.RemoveThread','Thread count now %d',[FThreadCount]);{$ENDIF USE_THREADLOG}
  if assigned(FOnThreadTerminate) then
    FOnThreadTerminate(aThread);
end;

procedure TThreadPool.ThreadFinished(aThread: TBaseWorkerThread);
begin
  // Called by the thread itself, before it leaves FThreads: a thread is
  // always in one of the two lists, so Destroy cannot miss it.
  If Assigned(FFinishedThreads) then
    FFinishedThreads.Add(aThread);
end;

// Join and free a monitor thread that has finished (FMonitorStatus=MonitorNone).
procedure TThreadPool.FreeMonitorThread;
begin
  if Assigned(FMonitorThread) then
    begin
    FMonitorThread.WaitFor;
    FreeAndNil(FMonitorThread);
    end;
end;

// Join and free the threads that have left Execute.
procedure TThreadPool.ReapFinishedThreads;

var
  List : specialize TList<TBaseWorkerThread>;
  Finished : array of TBaseWorkerThread;
  I : Integer;

begin
  if not Assigned(FFinishedThreads) then
    exit;
  Finished:=[];
  List:=FFinishedThreads.LockList;
  try
    SetLength(Finished,List.Count);
    For I:=0 to List.Count-1 do
      Finished[I]:=List[I];
    List.Clear;
  finally
    FFinishedThreads.UnlockList;
  end;
  For I:=0 to Length(Finished)-1 do
    begin
    // Join first: the thread entered the list from DoTerminate and may still
    // be running (RemoveFromPool, OnThreadTerminate). Free alone would run
    // the destructor before TThread.Destroy waits for the thread.
    Finished[I].WaitFor;
    Finished[I].Free;
    end;
end;

procedure TThreadPool.AssignWorkToLocalQueue(WorkerData: IThreadPoolWorkItem; aThread: TQueueWorkerThread);

begin
  aThread.WorkQueue.LocalPush(WorkerData);
  WorkQueued;
end;

procedure TThreadPool.AssignWorkToGlobalQueue(WorkerData: IThreadPoolWorkItem);

begin
  {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.AssignWorkToGlobalQueue','locking queue');{$ENDIF USE_THREADLOG}
  LockQueue;
  try
    FWorkQueue.Enqueue(WorkerData);
  finally
    {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.AssignWorkToGlobalQueue','unlocking queue');{$ENDIF USE_THREADLOG}
    UnLockQueue;
  end;
  WorkQueued;
end;

procedure TThreadPool.CreateMonitorThread;


var
  Status: Integer;

begin
  Status:=FMonitorStatus;
  if Status<>MonitorNone then
    exit;
  Status:=AtomicCmpExchange(FMonitorStatus, MonitorCreated, MonitorNone);
  if Status=MonitorNone then
    try
      // The previous monitor (if any) has finished: it resets FMonitorStatus
      // as its last action. Join and free it before replacing it.
      FreeMonitorThread;
      FMonitorThread:=TThreadPoolMonitor.Create(Self);
    except
      AtomicExchange(FMonitorStatus,MonitorNone);
     raise;
    end;
end;

procedure TThreadPool.WaitForMonitorThread;


begin
  While (FMonitorStatus<>MonitorNone) do
    TThread.Sleep(MonitorThreadDelay div 4);
  ReadBarrier;
  {$IFDEF FPC_VALGRIND}
  VgHappensAfter(@FMonitorStatus);
  {$ENDIF}
end;

procedure TThreadPool.DoQueueWorkItem(WorkerData: IThreadPoolWorkItem; PreferThread : TQueueWorkerThread);
begin
  {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.DoQueueWorkItem','enter');{$ENDIF USE_THREADLOG}
  if assigned(PreferThread) then
    AssignWorkToLocalQueue(WorkerData,PreferThread)
  else
    AssignWorkToGlobalQueue(WorkerData);
  if FMonitorStatus = MonitorNone then
    CreateMonitorThread;
  {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.DoQueueWorkItem','leave');{$ENDIF USE_THREADLOG}
end;


constructor TThreadPool.Create;
var
  PC: Integer;

begin
  FRetireEvent:=TLightweightEvent.Create;
  FQueueEvent:=TEvent.Create;
  FQueueLock:=TSpinLock.Create(False);
  FWorkQueue:=TWorkItemQueue.Create;
  PC:=TThread.ProcessorCount;
  FQueues:=TWorkStealingQueueThreadPoolWorkItemArray.Create(PC);
  {$IFDEF FPC_VALGRIND}
  // Flags polled without a lock; hand-offs are annotated separately.
  VgBenignRace(@FShutdown,SizeOf(FShutdown));
  VgBenignRace(@FMonitorStatus,SizeOf(FMonitorStatus));
  // Statistics for the pool growth heuristics, read without a lock.
  VgBenignRace(@FAvgCPUUsage,SizeOf(FAvgCPUUsage));
  VgBenignRace(@FThreadCreationAt,SizeOf(FThreadCreationAt));
  {$ENDIF}
  FMinThreads:=PC div 4;
  if FMinThreads<2 then
    FMinThreads:=2;
  FMaxThreads:=PC*MaxThreadsPerCPU;
  FThreads:=TBaseWorkerThreadList.Create;
  FThreads.Duplicates:=dupIgnore;
  FFinishedThreads:=TBaseWorkerThreadList.Create;
  FFinishedThreads.Duplicates:=dupIgnore;
{
  FThreads := TThreadList<TBaseWorkerThread>.Create;
  FThreads.Duplicates := dupIgnore;
}
end;

procedure TThreadPool.WaitForThreads;

var
  T : TThread;
  List : specialize TList<TBaseWorkerThread>;
  Empty : Boolean;

begin
  if Not Assigned(FThreads) then
    exit;
  Repeat
    List:=FThreads.LockList;
    try
      Empty:=List.Count=0;
      If not Empty then
        for T in List do
          begin
          T.Terminate;
          {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.WaitForThreads','Terminated thread');{$ENDIF USE_THREADLOG}
          end;
    finally
      FThreads.UnlockList;
    end;
    if not empty then
      begin
      FQueueEvent.SetEvent;
      FRetireEvent.SetEvent;
      // give threads time to deregister
      Sleep(MonitorThreadDelay div 4);
      end;
  Until Empty;
end;


destructor TThreadPool.Destroy;
begin
  FShutdown:=True;
  FQueueEvent.SetEvent;
  FRetireEvent.SetEvent;
  WaitForThreads;
  WaitForMonitorThread;
  // All threads are now in FFinishedThreads: wait until they have really
  // ended, so none of them is still running when the pool (or, for the
  // default pool, the program) is finalized.
  ReapFinishedThreads;
  FreeMonitorThread;
  FreeAndNil(FFinishedThreads);
  FreeAndNil(FWorkQueue);
  FreeAndNil(FQueues);
  FreeAndNil(FRetireEvent);
  FreeAndNil(FQueueEvent);
  FreeAndNil(FThreads);
  inherited Destroy;
end;

class function TThreadPool.NewControlFlag: IControlFlag;
begin
  Result:=TControlFlag.Create;
end;

procedure TThreadPool.QueueWorkItem(aSender: TObject; aWorkerEvent: TNotifyEvent; const aControlFlag: IControlFlag);

var
  WorkerData: TWorkerData;
  aFlag : IControlFlag;

begin
  aFlag:=aControlFlag;
  if aFlag=Nil then
    aFlag:=NewControlFlag;
  WorkerData:=TWorkerData.Create(aFlag,aSender,aWorkerEvent);
  DoQueueWorkItem(WorkerData,Nil);
end;

procedure TThreadPool.QueueWorkItem(const aWorkerEvent: TProcRef; const aControlFlag: IControlFlag);

var
  WorkerData: TWorkerData;
  aFlag : IControlFlag;

begin
  aFlag:=aControlFlag;
  if aFlag=Nil then
    aFlag:=NewControlFlag;
  WorkerData:=TWorkerData.Create(aFlag,aWorkerEvent);
  DoQueueWorkItem(WorkerData,Nil);
end;

function TThreadPool.SetMaxWorkerThreads(aValue: Integer): Boolean;
begin
  Result:=(aValue>FMinThreads);
  if Result then
    AtomicExchange(FMaxThreads,aValue);
end;

function TThreadPool.SetMinWorkerThreads(aValue: Integer): Boolean;
begin
  Result:=(aValue>=0) and (aValue<FMaxThreads);
  if Result then
    AtomicExchange(FMinThreads,aValue);
end;

procedure TThreadPool.SignalExecuting(aThread : TQueueWorkerThread);

begin
  {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.SignalExecuting','Enter (Requests left: %d, Idle: %d)',[FRequestCount,FIdleThreads]);{$ENDIF USE_THREADLOG}
  if aThread.Idle then
    AtomicDecrement(FIdleThreads);
  aThread.Idle:=False;
  AtomicDecrement(FRequestCount);
  {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.SignalExecuting','Leave (Requests left: %d, Idle: %d)',[FRequestCount,FIdleThreads]);{$ENDIF USE_THREADLOG}
end;

function TThreadPool.CheckShouldTerminate(aThread : TQueueWorkerThread) : Boolean;

var
  HighLoad: Boolean;
  aTick : Int64;
begin
  Result:=False;
  if FSuspendGuard<>0 then // We're suspending another thread.
    Exit;
  aTick:=GetTickCount64;
  HighLoad:=((FThreadCount-FIdleThreads)>2*FMinThreads) and
             (FAvgCPUUsage >= MaxCPUUsage) and
             (aTick>(FSuspendAtTick+SuspendDelay));
  if not HighLoad then
    exit;
  if AtomicCmpExchange(FSuspendGuard, 1, 0) = 0 then
    begin
    Result:=AThread.SuspendWork;
    FSuspendGuard:=0;
    AtomicIncrement(FSuspendCount);
    if Result then
      FSuspendAtTick:=aTick;
    end;
end;

procedure TThreadPool.LockQueue;
begin
  {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.LockQueue','Enter');{$ENDIF USE_THREADLOG}
  FQueueLock.Enter;
  {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.LockQueue','Leave');{$ENDIF USE_THREADLOG}
end;

procedure TThreadPool.UnLockQueue;
begin
  {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.UnLockQueue','Enter');{$ENDIF USE_THREADLOG}
  FQueueLock.Leave;
  {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.UnLockQueue','Leave');{$ENDIF USE_THREADLOG}
end;


// Return true if an item was found in one of the queues.
function TThreadPool.GetWorkItemFromQueues(aSkip: TWorkStealingQueueThreadPoolWorkItem; out Itm: IThreadPoolWorkItem): Boolean;

var
  I: integer;
  aQueue : TWorkStealingQueueThreadPoolWorkItem;

begin
  Result:=False;
  FQueues.Lock;
  try
    For I:=0 to Length(FQueues.Current)-1 do
      begin
      aQueue:=FQueues.Current[I];
      if (aQueue<> nil) and (aQueue<>aSkip) and aQueue.TrySteal(Itm) then
       Exit(True);
      end;
  finally
    FQueues.Unlock;
  end;
end;

procedure TThreadPool.RegisterWorkerThread(aThread : TQueueWorkerThread);

begin
  // The parent class already added us in the worker list.
  QueueThread:=aThread;
  FQueues.Add(aThread.WorkQueue);
end;

procedure TThreadPool.UnRegisterWorkerThread(aThread: TQueueWorkerThread);
begin
  FQueues.Remove(aThread.WorkQueue);
  if aThread.Idle then
    begin
    AtomicDecrement(FIdleThreads);
    {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.UnRegisterWorkerThread','Idle count: %d',[FIdleThreads]);{$ENDIF USE_THREADLOG}
    end;
  QueueThread:=Nil;
end;

function TThreadPool.DoRemoveWorkItem(WorkerData: IThreadPoolWorkItem): Boolean;
begin
  Result:=Assigned(QueueThread) and Assigned(QueueThread.WorkQueue);
  if Not Result then
    exit;
  Result:=QueueThread.WorkQueue.LocalFindAndRemove(WorkerData);
end;

// Steal an item from another thread's local queue.
// If more work is pending, wake another idle thread: a thread that resets
// FQueueEvent may consume a signal that was meant for several items.

function TThreadPool.TryGetStolenWorkItem(aThread: TQueueWorkerThread; out Itm: IThreadPoolWorkItem): Boolean;

begin
  {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.TryGetStolenWorkItem','Checking other queues');{$ENDIF USE_THREADLOG}
  Result:=GetWorkItemFromQueues(aThread.WorkQueue,Itm);
  if Result and (FRequestCount>1) then
    FQueueEvent.SetEvent;
  {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.TryGetStolenWorkItem','Got work: %s',[BToS(Result)]);{$ENDIF USE_THREADLOG}
end;

// if there is work, return it in Itm.
// If there is no work, return True if the thread should continue, False if it should terminate.

function TThreadPool.GetWorkItemForThread(aThread: TQueueWorkerThread; out Itm: IThreadPoolWorkItem): Boolean;

Var
  CheckThreadQueues : Boolean;

begin
  Result:=True;
  if FShutDown and (FRequestCount=0) then
    begin
    {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.GetWorkItemForThread','Shutting down, no work -> quit');{$ENDIF USE_THREADLOG}
    Exit(False);
    end;
  {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.GetWorkItemForThread','locking queue');{$ENDIF USE_THREADLOG}
  LockQueue;
  try
    if (FWorkQueue.Count > 0) then
      begin
      // FWorkQueue access is guarded by LockQueue.
      {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.GetWorkItemForThread','Have global work');{$ENDIF USE_THREADLOG}
      Itm:=FWorkQueue.Dequeue;
      if assigned(Itm) then
        begin
        {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.GetWorkItemForThread','Global work, -> no quit');{$ENDIF USE_THREADLOG}
        Exit(True); // We got work, do not stop thread
        end;
      end;
  finally
    {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.GetWorkItemForThread','unlocking queue');{$ENDIF USE_THREADLOG}
    UnLockQueue;
  end;
  // No local work, check global
  if not aThread.Idle then
    begin
    {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.GetWorkItemForThread','marking thread %d as idle',[PtrInt(aThread.ThreadID)]);{$ENDIF USE_THREADLOG}
    AtomicIncrement(FIdleThreads);
    aThread.Idle:=True;
    end;
  // FQueueEvent is a manual-reset event. Reset it *before* looking at the other
  // threads' local queues: work pushed after the check sets it again, so no
  // wake-up is lost. Checking before sleeping is needed because work may have
  // been queued while this thread was starting, when WorkQueued grew the pool
  // instead of signalling the event.
  if FShutdown then
    Exit(False); // Stop thread
  FQueueEvent.ResetEvent;
  if FShutdown then
    begin
    FQueueEvent.SetEvent;
    Exit(False); // Stop thread
    end;
  if TryGetStolenWorkItem(aThread,Itm) then
    Exit(True); // We got work, do not stop thread
  {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.GetWorkItemForThread','Waiting for queue event (%d ms.)',[aThread.CheckWaitTime]);{$ENDIF USE_THREADLOG}
  CheckThreadQueues:=(FQueueEvent.WaitFor(aThread.CheckWaitTime)<>wrTimeout);
  {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.GetWorkItemForThread','Work queued triggered: %s',[BToS(CheckThreadQueues)]);{$ENDIF USE_THREADLOG}
  if FShutdown then
    begin
    {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.GetWorkItemForThread','Shutdown -> quit');{$ENDIF USE_THREADLOG}
    Exit(False); // Stop thread
    end;
  if CheckThreadQueues and TryGetStolenWorkItem(aThread,Itm) then
    Exit(True); // We got work, do not stop thread
  if FShutdown then
    begin
    {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.GetWorkItemForThread','Shutdown -> quit');{$ENDIF USE_THREADLOG}
    Exit(False); // Stop thread
    end;
  // Nothing to do. Adjust waiting time or stop thread.
  if (FThreadCount > FMinThreads+1) then
    begin
    // The existing threads can handle the work ?
    if (FRequestCount < ThreadToRequestRatio * (FThreadCount-1)) then
      // we already increased wait time sufficiently ?
      begin
      if (aThread.CheckWaitTime>EnoughThreadsTimeOut) then
        begin
        {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.GetWorkItemForThread','Enough threads to handle workload -> quit');{$ENDIF USE_THREADLOG}
        Exit(False); // Stop thread
        end;
      end;
    aThread.AdjustWaitTime;
    end
  else  if (FRequestCount<=0) then
    // We've got one thread and no requests
    begin
    // if we waited long enough...
    if (aThread.CheckWaitTime>NoRequestsTimeOut) then
      begin
      {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.GetWorkItemForThread','One thread, waiting quite long -> quit');{$ENDIF USE_THREADLOG}
      Exit(False); // Stop thread
      end;
    aThread.AdjustWaitTime;
    end;
end;

procedure TThreadPool.InitCPUStats;

begin
  TThread.GetSystemTimes(FCPUInfo);
  FCurUsageSlot:=0;
  FillChar(FCPUUsageArray, SizeOf(FCPUUsageArray), 0);
end;

procedure TThreadPool.StopCPUStats;

begin
  FCurUsageSlot:=0;
  FillChar(FCPUUsageArray, SizeOf(FCPUUsageArray), 0);
  // FMonitorStatus is reset by the monitor thread when it is done: once it is
  // MonitorNone, Destroy may free the pool.
end;

function TThreadPool.HaveNoWorkers : boolean;

begin
  Result:=True
end;
function TThreadPool.IsThrottledDelay(aLastCreationTick: UInt64; aThreadCount: Cardinal): Boolean;

begin
  Result:=(GetTickCount64-aLastCreationTick)>1;
  if aThreadCount<>0 then; // Silence compiler warning
end;

procedure TThreadPool.GrowIfStarved;

var
  PrevRequestCount: Integer;
  AllowMoreThreads,IncreasingRequests,ThrottleOK,B,CreateNewThread: Boolean;
  HaveRoomForWork : Boolean;

begin
  HaveRoomForWork:=(FRequestCount>0) and (FThreadCount<FMaxThreads);
  if Not HaveRoomForWork then
    begin
    {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.GrowIfStarved','No work (%d>0) and (%d<%d) is False. Not creating new threads',[FRequestCount,FThreadCount,FMaxThreads]);{$ENDIF USE_THREADLOG}
    Exit;
    end;
  PrevRequestCount:=FPreviousRequestCount;
  FPreviousRequestCount:=FRequestCount;
  {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.GrowIfStarved','(FRequestCount>=PrevRequestCount) and IsThrottledDelay(FThreadCreationAt,FThreadCount):');{$ENDIF USE_THREADLOG}
  ThrottleOK:=IsThrottledDelay(FThreadCreationAt,FThreadCount);
  IncreasingRequests:=(FRequestCount>=PrevRequestCount);
  B:=IncreasingRequests and ThrottleOK;
  {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.GrowIfStarved','IncreasingRequests (%d>=%d) [%s] and ThrottleOK (%d,%d) [%s] : %s',[FRequestCount,PrevRequestCount,BToS(IncreasingRequests),FThreadCreationAt, FThreadCount, BToS(ThrottleOK),BToS(B)]);{$ENDIF USE_THREADLOG}
  if not B then
    Exit;
  if B then
    begin
    CreateNewThread:=False;
    {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.GrowIfStarved','locking queue');{$ENDIF USE_THREADLOG}
    LockQueue;
    try
      IncreasingRequests:=(FRequestCount>=PrevRequestCount);
      AllowMoreThreads:=(FThreadCount<FMaxThreads);
      {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.GrowIfStarved','IncreasingRequests (%d>=%d) : %s ',[FRequestCount,PrevRequestCount,BToS(IncreasingRequests)]);{$ENDIF USE_THREADLOG}
      {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.GrowIfStarved','AllowMoreThreads (%d<%d) : %s',[FThreadCount,FMaxThreads,BToS(AllowMoreThreads)]);{$ENDIF USE_THREADLOG}
      {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.GrowIfStarved','(FIdleThreads=FRetiring) : (%d=%d) %s',[FIdleThreads,FRetiring,BToS(FIdleThreads=FRetiring)]);{$ENDIF USE_THREADLOG}
      B:=IncreasingRequests and AllowMoreThreads and (FIdleThreads=FRetiring);
      {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.GrowIfStarved','Attempt to create new thread %s',[BToS(B)]);{$ENDIF USE_THREADLOG}
      if B then
      begin
        CreateNewThread:=FRetiring<=0;
        if CreateNewThread then
          AddThreadToPool;
      end;
    finally
      {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.GrowIfStarved','unlocking queue');{$ENDIF USE_THREADLOG}
      UnLockQueue;
    end;
    if Not CreateNewThread then
      FRetireEvent.SetEvent;
    end;
end;

function TThreadPool.AddThreadToPool : TQueueWorkerThread;

begin
  {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.AddThreadToPool','Enter');{$ENDIF USE_THREADLOG}
  // No ReapFinishedThreads here: this runs with the queue lock held, and
  // joining a thread that is still in OnThreadTerminate could block or deadlock.
  FThreadCreationAt:=GetTickCount64;
  Result:=TQueueWorkerThread.Create(Self);
  AtomicIncrement(FThreadCount);
  {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.AddThreadToPool','Leave (thread count: %d)',[FThreadCount]);{$ENDIF USE_THREADLOG}
end;

function TThreadPool.DoMonitor : TMonitorResult;

var
  I: Integer;
  AvgCPU: Cardinal;

begin
  Result:=TMonitorResult.mrContinue;
  if FShutdown then
    Exit(TMonitorResult.mrTerminate);
  ReapFinishedThreads;
  TThread.Sleep(MonitorThreadDelay);
//  FCurrentCPUUsage:=TThread.GetCPUUsage(CPUInfo);
  FCPUUsageArray[FCurUsageSlot]:=FCPUUsage;
  if FCurUsageSlot = NumCPUUsageSamples - 1 then
    FCurUsageSlot:=0
  else
    Inc(FCurUsageSlot);
  AvgCPU:=0;
  for I:=0 to NumCPUUsageSamples - 1 do
    Inc(AvgCPU, FCPUUsageArray[I]);
  FAvgCPUUsage:=AvgCPU div TThreadPool.NumCPUUsageSamples;
  if FCPUUsage < MinCPUUsage then
    GrowIfStarved;
  if FShutdown then
    Exit(TMonitorResult.mrTerminate)
  else if HaveNoWorkers then
    Exit(TMonitorResult.mrIdle);
end;


{ *********************************************************************
  TThreadPool.TSafeSharedInteger
  *********************************************************************}


function TThreadPool.TSafeSharedInteger.GetInteger: Integer;
begin
  Result:=FValue^;
end;

procedure TThreadPool.TSafeSharedInteger.SetInteger(AValue: Integer);
begin
  FValue^:=aValue;
end;

constructor TThreadPool.TSafeSharedInteger.Create(var aSharedVar: Integer);
begin
  FValue:=@aSharedVar;
end;

function TThreadPool.TSafeSharedInteger.Increment: Integer;
begin
  Result:=AtomicIncrement(FValue^);
end;

function TThreadPool.TSafeSharedInteger.Decrement: Integer;
begin
  Result:=AtomicDecrement(FValue^);
end;

function TThreadPool.TSafeSharedInteger.CompareExchange(aValue: Integer; aComparand: Integer): Integer;
begin
  Result:=AtomicCmpExchange(FValue^,aValue,aComparand);
end;

class operator TThreadPool.TSafeSharedInteger.Explicit(aValue: TSafeSharedInteger): Integer;
begin
  Result:=aValue.FValue^;
end;

{$IFDEF THREAD64BIT}

{ *********************************************************************
  TThreadPool.TSafeSharedUInt64
  *********************************************************************}

function TThreadPool.TSafeSharedUInt64.GetUInt64: UInt64;
begin
  Result:=FValue^;
end;

procedure TThreadPool.TSafeSharedUInt64.SetUInt64(AValue: UInt64);
begin
  FValue^:=aValue;
end;

constructor TThreadPool.TSafeSharedUInt64.Create(var aSharedVar: UInt64);
begin
  FValue:=@aSharedVar;
end;

function TThreadPool.TSafeSharedUInt64.Increment: UInt64;
begin
  Result:=AtomicIncrement(FValue^);
end;

function TThreadPool.TSafeSharedUInt64.Decrement: UInt64;
begin
  Result:=AtomicDecrement(FValue^);
end;

class operator TThreadPool.TSafeSharedUInt64.Explicit(aValue: TSafeSharedUInt64): UInt64;
begin
  Result:=aValue.FValue^;
end;

{$ENDIF THREAD64BIT}

{ *********************************************************************
  TThreadPool.TControlFlag
  *********************************************************************}

function TThreadPool.TControlFlag.Increment: Integer;
begin
  Result:=AtomicIncrement(FFlag);
end;

function TThreadPool.TControlFlag.Value: Integer;
begin
  Result:=AtomicCmpExchange(FFlag,0,0);
end;

constructor TThreadPool.TControlFlag.Create;
begin
  inherited Create;
  FFlag:=-1;
end;

{ *********************************************************************
  TThreadPool.TAbstractWorkerData
  *********************************************************************}

function TThreadPool.TAbstractWorkerData.ShouldExecute: Boolean;
begin
  // This is a misnomer. if ShouldExecute is true, the task will NOT be executed.
  Result:=FControlFlag.Increment>0;
end;

class function TThreadPool.TAbstractWorkerData.NewInstance: TObject;

var
  Obj : Pointer;
  ObjCache: TObjectCache;

begin
  Result:=Nil;
  if TThreadPool.Caches.TryGetValue(Self,ObjCache) then
    begin
    Obj:=ObjCache.Remove;
    if Assigned(Obj) then
      begin
      {$IFDEF FPC_VALGRIND}
      // Recycled memory: earlier accesses by other threads are unrelated.
      VgCleanMemory(Obj,InstanceSize);
      {$ENDIF}
      Result:=InitInstance(Obj);
      TAbstractWorkerData(Result).FRefCount:=1;
      end;
    end;
  If Not Assigned(Result) then
    Result:=inherited NewInstance;
end;

procedure TThreadPool.TAbstractWorkerData.FreeInstance;
var
  ObjCache: TObjectCache;
begin
  CleanupInstance;
  if TThreadPool.Caches.TryGetValue(Self.ClassType,ObjCache) then
    if ObjCache.Insert(Pointer(Self)) then
      Exit;
  Inherited;
end;

constructor TThreadPool.TAbstractWorkerData.Create(aFlag: IControlFlag);
begin
  Inherited Create;
  FControlFlag:=aFlag;
end;

{ *********************************************************************
  TThreadPool.TWorkerData
  *********************************************************************}

procedure TThreadPool.TWorkerData.ExecuteWork;
begin
  if Assigned(FWorkerEvent) then
    FWorkerEvent(FSender)
  else if Assigned(FProc) then
    FProc;
end;

constructor TThreadPool.TWorkerData.Create(aFlag: IControlFlag; aSender: TObject; aEvent: TNotifyEvent);
begin
  Inherited Create(aFlag);
  FSender:=aSender;
  FWorkerEvent:=aEvent;
end;

constructor TThreadPool.TWorkerData.Create(aFlag: IControlFlag; aProc: TProcRef);
begin
  Inherited Create(aFlag);
  FProc:=aProc;
end;

{ *********************************************************************
  TThreadPool.TBaseWorkerThread
  *********************************************************************}


class function TThreadPool.TBaseWorkerThread.NextWorkerID: Integer;
begin
  Result:=AtomicIncrement(FWorkerID);
end;

procedure TThreadPool.TBaseWorkerThread.RemoveFromPool;
begin

  if Assigned(FThreadPool) then
    FThreadPool.RemoveThread(Self);
  // So we don't try to do it again.
  FThreadPool:=Nil;
end;

procedure TThreadPool.TBaseWorkerThread.SafeTerminate;
begin
  Terminate; // DoTerminate hands the thread over to the pool for freeing.
end;

Function TThreadPool.TBaseWorkerThread.GetWorkerThreadName : string;

begin
  Result:=Format(SWorkerThreadName,[ClassName,FMyWorkerID,Pointer(ThreadPool)]);
end;

procedure TThreadPool.TBaseWorkerThread.Execute;

begin
  NameThreadForDebugging(GetWorkerThreadName);
  FRunningEvent.SetEvent;
end;

procedure TThreadPool.TBaseWorkerThread.DoTerminate;
begin
  if Assigned(FRunningEvent) then
    FRunningEvent.SetEvent;
  // Hand the thread over to the pool, which will join and free it.
  if Assigned(FThreadPool) then
    FThreadPool.ThreadFinished(Self);
  RemoveFromPool;
  inherited DoTerminate;
end;

constructor TThreadPool.TBaseWorkerThread.Create(aThreadPool: TThreadPool);
begin
  // Not FreeOnTerminate: the pool joins and frees its threads, see
  // DoTerminate and TThreadPool.ReapFinishedThreads.
  FreeOnTerminate:=False;
  inherited Create(False);
  FRunningEvent:=TLightweightEvent.Create(False);
  FThreadPool:= AThreadPool;
  if Assigned(FThreadPool) then
    FThreadPool.NewThread(Self);
  FMyWorkerID:=NextWorkerID;
end;

destructor TThreadPool.TBaseWorkerThread.Destroy;
begin
  RemoveFromPool;
  FreeAndNil(FRunningEvent);
  inherited Destroy;
end;

procedure TThreadPool.TBaseWorkerThread.BeforeDestruction;
begin
  if FRunningEvent <> nil then
    FRunningEvent.WaitFor(INFINITE);
  inherited BeforeDestruction;
end;

{ *********************************************************************
  TThreadPool.TQueueWorkerThread
  *********************************************************************}

function TThreadPool.TQueueWorkerThread.SuspendWork: Boolean;

var
  I,Limit,Usage: Integer;

begin
  Limit:=TThreadPool.SuspendTries;
  I:=0;
  Usage:=ThreadPool.FCPUUsage-4;
  while (I<Limit) do
    begin
    Sleep(TThreadPool.SuspendTime);
    if (ThreadPool.FCPUUsage<Usage) then
      Limit:=0;
    Inc(I);
    end;
  Result:=Limit<>0;
end;

function TThreadPool.TQueueWorkerThread.TryToRetire: Boolean;

// Return true if we can retire.

var
  aTime : Integer;

begin
  Result:=False;
  AtomicIncrement(ThreadPool.FRetiring);
  try
    aTime:=TThreadPool.RetireDelay;
    while True do
      begin
      if (FPoolRetireEvent.WaitFor(aTime)<>wrTimeout) then
        // We were signaled, so do not retire
        Exit
      else
        // Timeout ?
        begin
        // total time exceeded: retire if there is no work.
        if (aTime>MaxDelay) then
          Exit(FWorkQueue.Count=0);
        // We must wait, lets wait longer
        aTime:=2*aTime;
        if aTime>MaxDelay then
          aTime:=MaxDelay;
        end;
      end;
  finally
    AtomicDecrement(ThreadPool.FRetiring);
  end;
end;

procedure TThreadPool.TQueueWorkerThread.ExecuteWorkItem(var aItem: IThreadPoolWorkItem);

begin
  try
    aItem.ExecuteWork;
  except
    On E : Exception do
      FWorkException:=E;
  end;
  aItem:=nil;
end;

procedure TThreadPool.TQueueWorkerThread.WrapExecute(var aItem : IThreadPoolWorkItem);

begin
  ThreadPool.SignalExecuting(Self);
  if aItem.ShouldExecute then
    begin
    aItem:=nil;
    Exit;
    end;
  ExecuteWorkItem(aItem);
end;

procedure TThreadPool.TQueueWorkerThread.AdjustWaitTime;
begin
  if FCheckWaitTime < MaxCheckWaitTime then
    FCheckWaitTime:=(FCheckWaitTime *2)
  else
    FCheckWaitTime:=IdleTimeout;
end;

procedure TThreadPool.TQueueWorkerThread.Execute;

var
  Itm: IThreadPoolWorkItem;

begin
  // Set event
  inherited Execute;
  FCheckWaitTime:=IdleTimeout;
  ThreadPool.RegisterWorkerThread(Self);
  try
    While not Terminated do
      begin
      Itm:=Nil;
      // If we do not have work assigned
      If not WorkQueue.LocalPop(Itm) then
        // Ask for more work
        if not ThreadPool.GetWorkItemForThread(Self,Itm) then
          begin
          // if it returned false, we stop
          {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.TQueueWorkerThread.Execute','No work, stopping');{$ENDIF USE_THREADLOG}
          Terminate;
          end;
      if Assigned(Itm) then
        begin
        {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.TQueueWorkerThread.Execute','Calling WrapExecute. Idle: %s',[BToS(Idle)]);{$ENDIF USE_THREADLOG}
        WrapExecute(Itm);
        FCheckWaitTime:=IdleTimeout;
        {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.TQueueWorkerThread.Execute','Called WrapExecute. Idle: %s',[BToS(Idle)]);{$ENDIF USE_THREADLOG}
        end;
      if ThreadPool.CheckShouldTerminate(Self) then
        begin
        {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.TQueueWorkerThread.Execute','Threadpool said to stop; terminating');{$ENDIF USE_THREADLOG}
        Terminate;
        end;
      if Terminated then
        {$IFDEF USE_THREADLOG}ThreadLog('TThreadPool.TQueueWorkerThread.Execute','Thread Terminated');{$ENDIF USE_THREADLOG}
      end;
  finally
    ThreadPool.UnRegisterWorkerThread(Self);
  end;
end;

procedure TThreadPool.TQueueWorkerThread.PushLocalWorkToGlobal;

var
  Itm: IThreadPoolWorkItem;

begin
  while FWorkQueue.LocalPop(Itm) do
    ThreadPool.DoQueueWorkItem(Itm,Nil);
end;

constructor TThreadPool.TQueueWorkerThread.Create(aThreadPool: TThreadPool);
begin
  FPoolRetireEvent:=aThreadPool.FRetireEvent;
  FWorkQueue:=TWorkStealingQueueThreadPoolWorkItem.Create;
  Inherited Create(aThreadPool);
end;

destructor TThreadPool.TQueueWorkerThread.Destroy;
begin
  if Assigned(ThreadPool) and Assigned(ThreadPool.FQueues) then
    begin
    // Make sure the queue is no longer published to stealers, even if an
    // exceptional exit path skipped UnRegisterWorkerThread (no-op otherwise).
    ThreadPool.FQueues.Remove(FWorkQueue);
    // Serialize the destruction with the steal loop: GetWorkItemFromQueues
    // captures queue references from FQueues.Current and uses them while
    // holding FQueues' lock. Taking the same lock here guarantees no stealer
    // is left operating on a freed queue (whose internal critical section
    // and event are freed with it).
    ThreadPool.FQueues.Lock;
    try
      FreeAndNil(FWorkQueue);
    finally
      ThreadPool.FQueues.Unlock;
    end;
    end
  else
    FreeAndNil(FWorkQueue);
  inherited Destroy;
end;

{ *********************************************************************
  TThreadPool.TThreadPoolMonitor
  *********************************************************************}



function TThreadPool.TThreadPoolMonitor.IsThrottledDelay(aLastCreationTick: UInt64; aThreadCount: Cardinal): Boolean;
begin
  Result:=FThreadPool.IsThrottledDelay(aLastCreationTick,aThreadCount);
end;

function TThreadPool.TThreadPoolMonitor.GetThreadName : string;

begin
  Result:=Format('Thread Pool Monitor Thread - %s ThreadPool - %p', [ClassName, Pointer(FThreadPool)])
end;

procedure TThreadPool.TThreadPoolMonitor.Execute;

Var
  IdleCount : Integer;
  Res : TMonitorResult;

begin
  try
  NameThreadForDebugging(GetThreadName);
  TThread.Sleep(TThreadPool.MonitorThreadDelay);
  FThreadPool.InitCPUStats;
  IdleCount:=TThreadPool.MonitorIdleLimit;
  while not Terminated do
    begin
    Res:=FThreadPool.DoMonitor;
    case res of
      TMonitorResult.mrContinue :
        IdleCount:=TThreadPool.MonitorIdleLimit;
      TMonitorResult.mrIdle :
        begin
        Dec(IdleCount);
        if IdleCount=0 then
          Terminate;
        end;
      TMonitorResult.mrTerminate:
        Terminate;
    end;
    end;
    FThreadPool.StopCPUStats;
    FThreadPool.ReapFinishedThreads;
  finally
    // Last access to FThreadPool: after this, WaitForMonitorThread returns and
    // the pool may be freed.
    {$IFDEF FPC_VALGRIND}
    VgHappensBefore(@FThreadPool.FMonitorStatus);
    {$ENDIF}
    AtomicExchange(FThreadPool.FMonitorStatus,MonitorNone);
  end;
end;

procedure TThreadPool.TThreadPoolMonitor.GrowThreadPoolIfStarved;
begin
  FThreadPool.GrowIfStarved
end;

constructor TThreadPool.TThreadPoolMonitor.Create(aThreadPool: TThreadPool);
begin
  FThreadPool:=aThreadPool;
  FreeOnTerminate:=False; // Owned by the pool (FMonitorThread)
  Inherited Create(False);
end;

{ *********************************************************************
  TThreadPoolStats
  *********************************************************************}

class function TThreadPoolStats.GetCurrent: TThreadPoolStats;
begin
  Result.Assign(TThreadPool.Current);
end;

class function TThreadPoolStats.GetDefault: TThreadPoolStats;
begin
  Result.Assign(TThreadPool.Default);
end;

class function TThreadPoolStats.Get(const aPool: TThreadPool): TThreadPoolStats;
begin
  Result.Assign(aPool);
end;

Procedure TThreadPoolStats.Assign(const aPool: TThreadPool);

begin
  FWorkerThreadCount:=aPool.FThreadCount;
  FMinLimitWorkerThreadCount:=aPool.FMinThreads;
  FMaxLimitWorkerThreadCount:=aPool.FMaxThreads;
  FIdleWorkerThreadCount:=aPool.FIdleThreads;
  FQueuedRequestCount:=aPool.FRequestCount;
  FRetiredWorkerThreadCount:=aPool.FRetiring;
  FAverageCPUUsage:=aPool.FAvgCPUUsage;
  FCurrentCPUUsage:=aPool.FCPUUsage;
  FThreadSuspended:=aPool.FSuspendCount;
  FLastSuspendTick:=aPool.FSuspendAtTick;
  FLastThreadCreationTick:=aPool.FThreadCreationAt;
  FLastQueuedRequestCount:=aPool.FPreviousRequestCount;
end;

{ *********************************************************************
  TTask
  *********************************************************************}

class function TTask.NewId: Integer;
begin
  Result:=AtomicIncrement(FNextTaskID);
end;

class function TTask.CurrentTask: ITask;
begin
  Result:=_CurrentTask;
end;

constructor TTask.Create;
begin
  raise ENoConstructException.Create(SErrInvalidTaskConstructor);
end;

destructor TTask.Destroy;
begin
  FCancelRegistration.Unregister;
  FreeAndNil(FException);
  FreeAndNil(FDoneEvent);
  inherited Destroy;
end;

procedure TTask.OnCancelToken(Sender: TObject);
begin
  if Sender=nil then;
  DoCancel(False);
end;

class function TTask.Run(aSender: TObject; aEvent: TNotifyEvent; aPool: TThreadPool): ITask;
begin
  Result:=TTask.Create(aSender,aEvent,aPool);
  Result.Start;
end;

class function TTask.Run(aSender: TObject; aEvent: TNotifyEvent): ITask; overload; static; inline;
begin
  Result:=Run(aSender,aEvent,TThreadPool.Default);
end;

class function TTask.Run(aSender: TObject; aEvent: TNotifyEvent; const aCancelToken: TCancellationToken; aPool: TThreadPool): ITask;
begin
  Result:=TTask.Create(aSender,aEvent,aCancelToken,aPool);
  Result.Start;
end;

class function TTask.Run(const aFunc: TProcRef; aPool: TThreadPool): ITask;
begin
  Result:=TTask.Create(aFunc,aPool);
  Result.Start;
end;

class function TTask.Run(const aFunc: TProcRef): ITask;
begin
  Result:=Run(aFunc,TThreadPool.Default);
end;

class function TTask.Run(const aFunc: TProcRef; const aCancelToken: TCancellationToken; aPool: TThreadPool): ITask;
begin
  Result:=TTask.Create(aFunc,aCancelToken,aPool);
  Result.Start;
end;

function TTask.GetIsComplete: Boolean;
begin
  // Lock-free check (fast path of Wait). The flags are written under
  // FStateLock, whose Exit is a full barrier; the read barrier pairs with it,
  // so a caller that sees the task complete also sees its results.
  Result:=FDone or ((FStateFlags*CompleteStates) <> []);
  if Result then
    begin
    ReadBarrier;
    {$IFDEF FPC_VALGRIND}
    VgHappensAfter(@FStateLock); // Same address as the TSpinLock annotation
    {$ENDIF}
    end;
end;

function TTask.GetIsReplicating: Boolean;
begin
  Result:=(FStateFlags*ReplicatingStates) = [TOptionStateFlag.Replicating];
end;

function TTask.GetHasExceptions: Boolean;
begin
  Result:=(FException<>nil) or (Length(FTasksWithExceptions)>0);
end;

function TTask.GetIsCanceled: Boolean;
begin
  Result:=(FStateFlags*CanceledStates)=[TOptionStateFlag.Canceled];
end;

function TTask.GetIsQueued: Boolean;
begin
  Result:=(FStateFlags*StartedStates) = [TOptionStateFlag.Started];
end;

function TTask.GetDoneEvent: TLightweightEvent;
begin
  Result:=FDoneEvent;
end;

function TTask.UpdateStateAtomic(aNewState: TOptionStateFlags; aInvalidStates: TOptionStateFlags): Boolean;
var
  Old : TOptionStateFlags;

begin
  Result:=UpdateStateAtomic(aNewState,aInvalidStates,Old);
end;

Procedure TTask.LockState;

begin
  FStateLock.Enter;
end;

Procedure TTask.UnLockState;

begin
  FStateLock.Exit;
end;

Procedure TTask.CalcStatus;

  function GetNewStatus : TTaskStatus;

  var
    OSF : TOptionStateFlags;

    Function Have(F : TOptionStateFlag) : boolean; inline;
    begin
      Result:=F in OSF;
    end;

  begin
    OSF:=FStateFlags;
    if Have(TOptionStateFlag.Faulted) then
      Exit(TTaskStatus.Exception);
    if Have(TOptionStateFlag.Canceled) or (Assigned(FParams.ParentControlFlag) and (FParams.ParentControlFlag.Value>0)) then
      Exit(TTaskStatus.Canceled);
    if Have(TOptionStateFlag.Complete) then
      Exit(TTaskStatus.Completed);
    if Have(TOptionStateFlag.ChildWait) then
      Exit(TTaskStatus.WaitingForChildren);
    if Have(TOptionStateFlag.CallbackRun) then
      Exit(TTaskStatus.Running);
    if Have(TOptionStateFlag.Started) then
      Exit(TTaskStatus.WaitingToRun);
    Result:=TTaskStatus.Created;
  end;

begin
  FStatus:=GetNewStatus;
end;

procedure TTask.ForceStateFlags(aFlags : TOptionStateFlags);

begin
  FStateFlags:=aFlags;
  CalcStatus;
end;

function TTask.UpdateStateAtomic(aNewState: TOptionStateFlags; aInvalidStates: TOptionStateFlags; out aOldState: TOptionStateFlags
  ): Boolean;

begin
  LockState;
  try
    aOldState:=FStateFlags;
    Result:=(FStateFlags*aInvalidStates)=[];
    if Not Result then
      Exit;
    ForceStateFlags(FStateFlags+aNewState);
  finally
    UnLockState;
  end;
end;

procedure TTask.SetTaskStop;
begin
  // 0 -> 1, and >1 means not execute
  FControlFlag.Increment;
end;

function TTask.ShouldCreateReplica: Boolean;
begin
  // Indicate we CAN create a replica (will be overridden in TParallelTask)
  // The actual replication will be decided on the basis of flags.
  Result:=False;
end;

function TTask.CreateReplicaTask(const aParams : TTaskParams) : TTask;

begin
  Result:=TTask.Create(aParams);
end;

function TTask.CreateReplicaTask(const aProc: TProcRef; aParent: TTask; aCreateFlags: TCreateFlags;
  const aParentControlFlag: TThreadPool.IControlFlag): TTask;

var
  aParams : TTaskParams;

begin
  aParams:=Default(TTaskParams);
  aParams.Proc:=aProc;
  aParams.Parent:=aParent;
  aParams.Pool:=ThreadPool;
  aParams.CreateFlags:=aCreateFlags;
  aParams.ParentControlFlag:=aParentControlFlag;
  aParams.CancellationToken:=aParent.FParams.CancellationToken;
  Result:=CreateReplicaTask(aParams);
  Result.FReplicaRoot:=aParent;
end;

procedure TTask.CheckFaulted;

var
  E: TObject;
begin
  {$IFDEF USE_THREADLOG}ThreadLog('TTask.CheckFaulted','CheckFaulted');{$ENDIF USE_THREADLOG}
  E:=GetExceptionObject;
  if Assigned(E) then
    begin
    {$IFDEF USE_THREADLOG}ThreadLog('TTask.CheckFaulted','CheckFaulted have error');{$ENDIF USE_THREADLOG}
    SetRaisedState;
    raise E;
    end;
end;

procedure TTask.PropagateExceptionFrom(aSource: TTask);
var
  Ex: Exception;
  ExClass: ExceptClass;
  ExMsg: string;
begin
  if not Assigned(aSource) then
    Exit;
  Ex:=aSource.GetExceptionObject;
  if not Assigned(Ex) then
    begin
    aSource.LockState;
    try
      ExClass:=aSource.FExceptionClass;
      ExMsg:=aSource.FExceptionMessage;
    finally
      aSource.UnLockState;
    end;
    if Assigned(ExClass) then
      Ex:=EAggregateException.Create([ExClass.Create(ExMsg)])
    else
      Ex:=EAggregateException.Create([Exception.Create(SAggregateException)]);
    end;
  aSource.UpdateStateAtomic([TOptionStateFlag.Raised],[]);
  LockState;
  try
    SetExceptionObject(Ex);
  finally
    UnLockState;
  end;
end;

class function TTask.ShouldRunContinuation(aStatus: TTaskStatus; aOptions: TTaskContinuationOptions): Boolean;
begin
  if (TTaskContinuationOption.OnlyOnCompleted in aOptions) and (aStatus<>TTaskStatus.Completed) then
    Exit(False);
  if (TTaskContinuationOption.OnlyOnFaulted in aOptions) and (aStatus<>TTaskStatus.Exception) then
    Exit(False);
  if (TTaskContinuationOption.OnlyOnCanceled in aOptions) and (aStatus<>TTaskStatus.Canceled) then
    Exit(False);
  if (TTaskContinuationOption.NotOnCompleted in aOptions) and (aStatus=TTaskStatus.Completed) then
    Exit(False);
  if (TTaskContinuationOption.NotOnFaulted in aOptions) and (aStatus=TTaskStatus.Exception) then
    Exit(False);
  if (TTaskContinuationOption.NotOnCanceled in aOptions) and (aStatus=TTaskStatus.Canceled) then
    Exit(False);
  Result:=True;
end;


procedure TTask.SetComplete;

begin
  LockState;
  try
    FDone:=True;
  finally
    UnLockState;
  end;
  FDoneEvent.SetEvent;
end;

procedure TTask.AddChild;
begin
  AtomicIncrement(FSubTasks);
end;

procedure TTask.ForgetChild;
begin
  AtomicDecrement(FSubTasks);
end;

function TTask.InternalExecuteNow: Boolean;
begin
  if IsQueued then
    Result:=TryExecuteNow(True)
  else
    Result:=False;
end;

function TTask.GetExceptionObject: Exception;

var
  TI : ITask;
  T : TTask;
  Exceptions : TExceptionList;

begin
  Result:=Nil;
  if not HasExceptions then
    Exit;
  if Length(FTasksWithExceptions)=0 then
    begin
    // Object is not nil since HasExceptions returned true.
    LockState;
    try
      if FException is EAggregateException then
        Result:=Exception(FException)
      else
        Result:=EAggregateException.Create([Exception(FException)]);
      FException:=Nil;
      Exit;
    finally
      UnlockState;
    end;
    end;
  Exceptions:=TExceptionList.Create(Length(FTasksWithExceptions)+1);
  if assigned(FException) then
    begin
    LockState;
    try
      Exceptions.Add(FException as Exception);
      FException:=Nil;
    Finally
      UnlockState;
    end;
    end;
  for TI in FTasksWithExceptions do
    begin
    T:=TI as TTask;
    T.LockState;
    try
      Exceptions.AddFromTask(T);
      FreeAndNil(T.FException);
    finally
      T.UnlockState;
    end;
    end;
  Result:=EAggregateException.Create(Exceptions.Truncate);
end;



function TTask.GetWasExceptionRaised: Boolean;
begin
  Result:=TOptionStateFlag.Raised in FStateFlags;
end;

procedure TTask.QueueEvents;
begin
  FParams.Pool.DoQueueWorkItem(Self,FParams.Pool.QueueThread);
end;

procedure TTask.Complete(UserEventRan: Boolean);

var
  I,Last: Integer;
  LastTask : Boolean;
begin
  if not UserEventRan then
    begin
    IntermediateCompletion;
    exit;
    end;
  if Length(FTasksWithExceptions)>0 then
    begin
    LockState;
    try
      Last:=Length(FTasksWithExceptions)-1;
      for I:=Last downto 0 do
        if (FTasksWithExceptions[I] as TTask).WasExceptionRaised then
          begin
          if I<>Last then
            FTasksWithExceptions[I]:=FTasksWithExceptions[Last];
          FTasksWithExceptions[Last]:=Nil;
          Dec(Last);
          end;
      SetLength(FTasksWithExceptions,Last+1);
    finally
      UnLockState;
    end;
    end;
  LastTask:=((FSubTasks=1) and not IsReplicating) or (AtomicDecrement(FSubTasks)<=0);
  if LastTask then
    IntermediateCompletion
  else
    UpdateStateAtomic([TOptionStateFlag.ChildWait], [TOptionStateFlag.Faulted, TOptionStateFlag.Canceled, TOptionStateFlag.Complete]);
end;

procedure TTask.IntermediateCompletion;

var
  State: TOptionStateFlags;

begin
  FCancelRegistration.Unregister;
  State:=[];
  if HasExceptions then
    Include(State,TOptionStateFlag.Faulted);
  if IsCanceled then
    Include(State,TOptionStateFlag.Canceled)
  else
    Include(State,TOptionStateFlag.Complete);
  UpdateStateAtomic(State,[]);
  SetComplete;
  FinalCompletion;
end;

procedure TTask.FinalCompletion;
begin
  if (FParams.Parent<>nil) and (TOptionStateFlag.Replica in FStateFlags) then
    FParams.Parent.HandleChildCompletion(Self);
  ProcessCompleteEvents;
end;

procedure TTask.ProcessCompleteEvents;

  function MakeProc(const ATask: ITask; const AProc: specialize TProc<ITask>): TProcRef;
  begin
    Result :=
      procedure
      begin
        AProc(ATask);
      end;
  end;


var
  ProcList : TITaskProcArray;
  I, Count : Integer;
  Proc : TITaskProc;

begin
  if FCompletedEventCount=0 then
    exit;
  Repeat
    LockState;
    try
      ProcList:=FCompletedEvents;
      Count:=FCompletedEventCount;
      FCompletedEvents:=Nil;
      FCompletedEventCount:=0;
    finally
      UnLockState;
    end;
    For I:=0 to Count-1 do
      begin
      Proc:=ProcList[i];
      if (TOptionStateFlag.ChildWait in FStateFlags) then
        // Schedule for later execution
        Run(MakeProc(Self,Proc),FParams.Pool)
      else
        try
          // Execute immediatly
          Proc(Self);
        except
          // What to do with an exception ??
        end;
      end;
  until (FCompletedEventCount=0);
end;

procedure TTask.SetRaisedState;
begin
  if Assigned(FParams.Parent) and (_CurrentTask=(FParams.Parent as ITask)) then
    UpdateStateAtomic([TOptionStateFlag.Raised], []);
end;

function TTask.InternalWork(aCheckExecuting: Boolean): Boolean;

var
  BusyCheck : Boolean;

begin
  {$IFDEF USE_THREADLOG}ThreadLog('TTask.InternalWork','Enter');{$ENDIF USE_THREADLOG}
  BusyCheck:=aCheckExecuting or (TOptionStateFlag.Replicating in FStateFlags);
  {$IFDEF USE_THREADLOG}ThreadLog('TTask.InternalWork','busycheck: %s:=%s or (%s));',[BToS(BusyCheck),BToS(aCheckExecuting),BToS(TOptionStateFlag.Replicating in FStateFlags)]);{$ENDIF USE_THREADLOG}
  if Not BusyCheck then
    begin
    {$IFDEF USE_THREADLOG}ThreadLog('TTask.InternalWork','set running');{$ENDIF USE_THREADLOG}
    if not UpdateStateAtomic([TOptionStateFlag.CallbackRun],[TOptionStateFlag.CallbackRun,TOptionStateFlag.Canceled,TOptionStateFlag.Complete,TOptionStateFlag.Faulted]) then
      Exit(False);
    end
  else if not UpdateStateAtomic([TOptionStateFlag.CallbackRun], [TOptionStateFlag.CallbackRun]) and
    not (TOptionStateFlag.Canceled in FStateFlags) then
      Exit(False);
  if IsCanceled or FParams.CancellationToken.IsCancellationRequested then
    begin
    UpdateStateAtomic([TOptionStateFlag.Canceled],[TOptionStateFlag.Faulted,TOptionStateFlag.Complete]);
    Complete(False);
    end
  else
    begin
    {$IFDEF USE_THREADLOG}ThreadLog('TTask.InternalWork','calling internalexecute');{$ENDIF USE_THREADLOG}
    InternalExecute(_CurrentTask);
    end;
  Result:=True;
end;

procedure TTask.InternalExecute(var aCurrentTaskVar: TTask);

var
  Old : TTask;

begin
  Old:=aCurrentTaskVar;
  try
    aCurrentTaskVar:=Self;
    Execute;
    Complete(True);
  finally
    aCurrentTaskVar:=Old;
  end;
end;

procedure TTask.CallUserCode;
begin
  if Assigned(FParams.Event) then
    FParams.Event(FParams.Sender)
  else if Assigned(FParams.Proc) then
    FParams.Proc;
end;

procedure TTask.ReplicaCallUserCode;
begin
  try
    FReplicaRoot.CallUserCode;
  except
    on E: EOperationCancelled do
      UpdateStateAtomic([TOptionStateFlag.Canceled],[TOptionStateFlag.Faulted,TOptionStateFlag.Complete]);
    else
      HandleException(Self, TObject(AcquireExceptionObject));
  end;
end;

procedure TTask.Execute;
begin
  if IsReplicating then
    ExecuteReplicates(Self)
  else if Assigned(FReplicaRoot) then
    ReplicaCallUserCode
  else
    try
      CallUserCode;
    except
      on E: EOperationCancelled do
        UpdateStateAtomic([TOptionStateFlag.Canceled],[TOptionStateFlag.Faulted,TOptionStateFlag.Complete]);
      else
        HandleException(Self,TObject(AcquireExceptionObject));
    end;
end;

procedure TTask.ExecuteReplicates(const aRoot: TTask);

var
  Sub : ITask;

begin
  FReplicaRoot:=aRoot;
  While aRoot.ShouldCreateReplica do
    begin
    {$IFDEF USE_THREADLOG}ThreadLog('TTask.ExecuteReplicates','Creating replica');{$ENDIF USE_THREADLOG}
    Sub:=aRoot.CreateReplicaTask(nil,aRoot,[TCreateFlag.Replicating, TCreateFlag.Replica],FParams.ParentControlFlag);
    {$IFDEF USE_THREADLOG}ThreadLog('TTask.ExecuteReplicates','Starting replica');{$ENDIF USE_THREADLOG}
    Sub.Start;
    {$IFDEF USE_THREADLOG}ThreadLog('TTask.ExecuteReplicates','Started replica');{$ENDIF USE_THREADLOG}
    end;
  ReplicaCallUserCode;
end;


procedure TTask.HandleException(const aChildTask: ITask; const aException: TObject);

begin
  HandleException(aChildTask as TTask,aException)
end;

procedure TTask.HandleException(const aChildTask: TTask; const aException: TObject);


var
  I,Len : Integer;

begin
  if aChildTask=Self then
    begin
    LockState;
    try
      SetExceptionObject(aException);
    finally
      UnlockState;
    end;
    Exit;
    end;
  LockState;
  try
    aChildTask.SetExceptionObject(aException);
    Len:=Length(FTasksWithExceptions);
    I:=Len-1;
    While (I>=0) and (FTasksWithExceptions[i].Id<>aChildTask.FTaskId) do
      Dec(I);
    if I<0 then
      begin
      SetLength(FTasksWithExceptions,Len+1);
      FTasksWithExceptions[Len]:=aChildTask;
      end;
  finally
    UnlockState;
  end;
end;

function TTask.MarkAsStarted: Boolean;
begin
  Result:=UpdateStateAtomic([TOptionStateFlag.Started],[TOptionStateFlag.Started,TOptionStateFlag.Canceled]);
end;

function TTask.TryExecuteNow(aWasQueued: Boolean): Boolean;

begin
  Result:=not aWasQueued or FParams.Pool.DoRemoveWorkItem(Self);
  if not Result then
    Exit;
  AtomicDecrement(FParams.Pool.FRequestCount);
  Result:=InternalWork(False);
end;

procedure TTask.ExecuteWork;
begin
  try
    InternalWork(False);
  except
    HandleException(Self, TObject(AcquireExceptionObject));
    Complete(False);
  end;
end;

// Returns the milliseconds left of aTimeout since aWatch was started, INFINITE for an infinite timeout
function RemainingTime(const aWatch: TStopWatch; aTimeout: Cardinal): Cardinal;

var
  lElapsed : Int64;

begin
  if aTimeout=INFINITE then
    Exit(INFINITE);
  lElapsed:=aWatch.ElapsedMilliseconds;
  if lElapsed>=aTimeout then
    Result:=0
  else
    Result:=aTimeout-lElapsed;
end;


function TTask.Wait(aTimeout: Cardinal): Boolean;

  Procedure RunChecks; inline;
  begin
    {$IFDEF USE_THREADLOG}ThreadLog('TTask.Wait.RunChecks','Enter');{$ENDIF USE_THREADLOG}
    try
      CheckCanceled;
      CheckFaulted;
    finally
      {$IFDEF USE_THREADLOG}ThreadLog('TTask.Wait.RunChecks','Leave');{$ENDIF USE_THREADLOG}
    end;
  end;

var
  NeedSync : Boolean;
  Watch : TStopWatch;

begin
  {$IFDEF USE_THREADLOG}ThreadLog('TTask.Wait','Enter (atimeout: %d) ',[aTimeout]);{$ENDIF USE_THREADLOG}
  Result:=IsComplete;
  if Result then
    begin
    {$IFDEF USE_THREADLOG}ThreadLog('TTask.Wait','Complete');{$ENDIF USE_THREADLOG}
    Runchecks;
    Exit;
    end;
  NeedSync:=(TThread.CurrentThread.ThreadID=MainThreadID) and FParams.Pool.Interactive;
  if Not NeedSync then
    begin
    {$IFDEF USE_THREADLOG}ThreadLog('TTask.Wait','Waiting for done event (%d)',[aTimeout]);{$ENDIF USE_THREADLOG}
    Result:=DoneEvent.WaitFor(aTimeout)<>wrTimeout;
    if Result then
      RunChecks;
    end
  else
    begin
    Watch:=TStopWatch.StartNew;
    While not IsComplete and (RemainingTime(Watch,aTimeout)>0) do
      CheckSynchronize(1);
    Result:=IsComplete;
    if Result then
      RunChecks;
    end;
end;

function TTask.Wait(const aTimeout: TTimeSpan): Boolean;
begin
  Result:=Wait(Trunc(aTimeOut.TotalMilliseconds));
end;

procedure TTask.DoCancel(aDestroying : Boolean);

var
  LFlags, OldFlags: TOptionStateFlags;

begin
  if IsComplete then
    exit;
  SetTaskStop;
  LFlags:=[TOptionStateFlag.Canceled];
  if aDestroying then
    Include(LFlags, TOptionStateFlag.Destroying);
  if not UpdateStateAtomic(LFlags,[TOptionStateFlag.Faulted,TOptionStateFlag.Complete,TOptionStateFlag.Canceled],OldFlags) then
    exit;
  if not (TOptionStateFlag.CallbackRun in OldFlags) then
    begin
    if (TOptionStateFlag.Started in OldFlags) and Assigned(FParams.Pool) then
      if FParams.Pool.DoRemoveWorkItem(Self) then
        AtomicDecrement(FParams.Pool.FRequestCount);
    Complete(False);
    end;
end;

procedure TTask.Cancel;

begin
  DoCancel(False);
end;


procedure TTask.CheckCanceled;
begin
  if (TOptionStateFlag.Canceled in FStateFlags) or FParams.CancellationToken.IsCancellationRequested then
    raise EOperationCancelled.Create(SOperationCancelled);
end;

function TTask.Start: ITask;
begin
  if IsCanceled then
    begin
    if not FDone then
      Complete(False);
    Exit(Self);
    end;
  if IsComplete then
    raise EInvalidOperation.Create(SCannotStartCompletedTask);
  Result:=Self;
  if Not MarkAsStarted then
    Exit;
  try
    GetDoneEvent;
    QueueEvents;
  except
    Complete(False);
    raise;
  end;
end;

function TTask.GetId: Integer;
begin
  Result:=FTaskID;
end;

function TTask.GetStatus: TTaskStatus;

begin
  Result:=FStatus;
end;

procedure TTask.AttachContinuation(aContTask: TTask; const aCancelToken: TCancellationToken;
  aOptions: TTaskContinuationOptions; aPropagateAntecedentState: Boolean);
var
  AntecedentTask: TTask;
  ContTaskI: ITask;
  EffToken: TCancellationToken;
begin
  AntecedentTask:=Self;
  ContTaskI:=aContTask;
  EffToken:=aCancelToken;
  AddCompleteEvent(
    procedure(aCompletedTask: ITask)
    begin
      if aContTask.IsCanceled or EffToken.IsCancellationRequested or
         not TTask.ShouldRunContinuation(aCompletedTask.Status,aOptions) then
        begin
        aContTask.Cancel;
        Exit;
        end;
      if aPropagateAntecedentState then
        begin
        if aCompletedTask.Status=TTaskStatus.Canceled then
          begin
          aContTask.Cancel;
          Exit;
          end;
        if aCompletedTask.Status=TTaskStatus.Exception then
          begin
          aContTask.PropagateExceptionFrom(AntecedentTask);
          aContTask.Complete(False);
          Exit;
          end;
        end;
      if TTaskContinuationOption.ExecuteSynchronously in aOptions then
        aContTask.ExecuteWork
      else
        ContTaskI.Start;
    end);
end;

function TTask.ContinueWith(const aAction: specialize TProc<ITask>): ITask;
begin
  Result:=ContinueWith(aAction,TCancellationToken.None,[],FParams.Pool);
end;

function TTask.ContinueWith(const aAction: specialize TProc<ITask>; aOptions: TTaskContinuationOptions): ITask;
begin
  Result:=ContinueWith(aAction,TCancellationToken.None,aOptions,FParams.Pool);
end;

function TTask.ContinueWith(const aAction: specialize TProc<ITask>; const aCancelToken: TCancellationToken;
  aOptions: TTaskContinuationOptions; aPool: TThreadPool): ITask;
var
  Antecedent: ITask;
  ContTask: TTask;
  Params: TTaskParams;
  EffToken: TCancellationToken;
begin
  Antecedent:=Self;
  EffToken:=aCancelToken;
  if (not EffToken.CanBeCanceled) and (TTaskContinuationOption.NotOnCanceled in aOptions) then
    EffToken:=FParams.CancellationToken;
  Params:=Default(TTaskParams);
  if Assigned(aPool) then
    Params.Pool:=aPool
  else
    Params.Pool:=FParams.Pool;
  Params.CancellationToken:=EffToken;
  Params.Proc:=
    procedure
    begin
      if Assigned(aAction) then
        aAction(Antecedent);
    end;
  ContTask:=TTask.Create(Params);
  Result:=ContTask;
  AttachContinuation(ContTask,EffToken,aOptions,False);
end;

procedure TTask.AddCompleteEvent(const aProc: TITaskProc);

var
  lDone : Boolean;

begin
  LockState;
  try
    lDone:=FDone;
    if not lDone then
      begin
      If Length(FCompletedEvents)=FCompletedEventCount then
        SetLength(FCompletedEvents,FCompletedEventCount+32);
      FCompletedEvents[FCompletedEventCount]:=aProc;
      Inc(FCompletedEventCount);
      end;
  finally
    UnlockState;
  end;
  if lDone then
    aProc(Self);
end;

procedure TTask.HandleChildCompletion(const aTask: TAbstractTask.IInternalTask);

begin
  HandleChildCompletion(aTask as TTask);
end;

procedure TTask.HandleChildCompletion(const aTask: TTask);


begin
  if Not Assigned(aTask) then
    exit;
  if aTask.HasExceptions and not aTask.WasExceptionRaised then
    HandleException(aTask,aTask.GetExceptionObject);
  if AtomicDecrement(FSubTasks)=0 then
    IntermediateCompletion;
end;

procedure TTask.SetExceptionObject(const aException: TObject);

begin
  if Assigned(aException) and (aException is Exception) and not Assigned(FExceptionClass) then
    begin
    if aException is EAggregateException then
      begin
      if EAggregateException(aException).Count>0 then
        begin
        FExceptionClass:=ExceptClass(EAggregateException(aException).InnerExceptions[0].ClassType);
        FExceptionMessage:=EAggregateException(aException).InnerExceptions[0].Message;
        end
      else
        begin
        FExceptionClass:=ExceptClass(aException.ClassType);
        FExceptionMessage:=Exception(aException).Message;
        end;
      end
    else
      begin
      FExceptionClass:=ExceptClass(aException.ClassType);
      FExceptionMessage:=Exception(aException).Message;
      end;
    end;
  if not assigned(FException) then
    FException:=aException
  else if aException is Exception then
    begin
    if not (FException is EAggregateException) then
      // This is not correct, we don't know whether FException is an Exception.
      FException:=EAggregateException.Create([Exception(FException),Exception(aException)])
    else
      EAggregateException(FException).Add(Exception(aException))
    end;
end;

procedure TTask.RemoveCompleteEvent(const aProc: TITaskProc);

var
  I,Idx : Integer;

begin
  If FCompletedEventCount=0 then
    exit;// Don't bother locking
  LockState;
  try
    Idx:=FCompletedEventCount-1;
    While (Idx>=0) and (FCompletedEvents[Idx]<>aProc) do
      Dec(Idx);
    if Idx>=0 then
      begin
      For I:=Idx to FCompletedEventCount-2 do
        FCompletedEvents[I]:=FCompletedEvents[I+1];
      Dec(FCompletedEventCount)
      end;
  finally
    UnLockState;
  end;
end;

function TTask.GetControlFlag: TThreadPool.IControlFlag;
begin
  Result:=FControlFlag;
end;

constructor TTask.Create(const aParams : TTaskParams);

begin
  inherited Create(TThreadPool.NewControlFlag);
  FTaskID:=NewId;
  FSubTasks:=1;
  FParams:=aParams;
  FParams.ResolvePool;
  if not FParams.CancellationToken.CanBeCanceled then
    if Assigned(FParams.Parent) then
      FParams.CancellationToken:=FParams.Parent.FParams.CancellationToken
    else if Assigned(_CurrentTask) then
      FParams.CancellationToken:=_CurrentTask.FParams.CancellationToken;
  if FParams.Parent<>nil then
    FParams.Parent.AddChild;
  FStateFlags:=[];
  FStatus:=TTaskStatus.Created;
  FDone:=False;
  FCompletedEvents:=nil;
  FCompletedEventCount:=0;
  FTasksWithExceptions:=nil;
  FException:=nil;
  FExceptionClass:=nil;
  FExceptionMessage:='';
  FCancelRegistration:=Default(TCancellationTokenRegistration);
  if TCreateFlag.Replicating in aParams.CreateFlags then
    Include(FStateFlags, TOptionStateFlag.Replicating);
  if TCreateFlag.Replica in aParams.CreateFlags then
    Include(FStateFlags, TOptionStateFlag.Replica);
  FStateLock:=TSpinLock.Create(False);
  FDoneEvent:=TEvent.Create;
  {$IFDEF FPC_VALGRIND}
  // FStateFlags is written under FStateLock only, but read without the lock
  // in several places (e.g. GetIsComplete, the fast path of Wait).
  VgBenignRace(@FStateFlags,SizeOf(FStateFlags));
  VgBenignRace(@FStatus,SizeOf(FStatus)); // Snapshot derived from FStateFlags
  VgBenignRace(@FDone,SizeOf(FDone));
  {$ENDIF}
  if FParams.CancellationToken.CanBeCanceled then
    FCancelRegistration:=FParams.CancellationToken.Register(@OnCancelToken,Self);
end;

class function TTask.DoWaitForAll(const aTasks: array of ITask; aTimeout: Cardinal): Boolean;

var
  I,lAdded: Integer;
  Task: TTask;
  TaskI : ITask;
  ExceptionCount,CompletedCount,CancelCount : Integer;
  CompleteProc: TITaskProc;
  ExceptionList: TExceptionList;
  Lock : TSpinLock;
  Event : TEvent;
  NeedSync : Boolean;
  Watch : TStopWatch;
  lRemaining : Cardinal;

  procedure TaskCompleted(aTask: ITask);
  begin
    if (aTask as TTask).HasExceptions then
      AtomicIncrement(ExceptionCount)
    else if (aTask as TTask).IsCanceled then
      AtomicIncrement(CancelCount);
    AtomicIncrement(CompletedCount);
    Lock.Enter;
    try
      if Assigned(Event) then
        Event.SetEvent;
    finally
      Lock.Exit;
    end;
  end;

begin
  Result:=True;
  if Length(aTasks)=0 then
    Exit;
  Watch:=TStopWatch.StartNew;
  ExceptionCount:=0;
  CompletedCount:=0;
  CancelCount:=0;
  lAdded:=0;
  NeedSync:=(TThread.CurrentThread.ThreadID=MainThreadID) and ((aTasks[0] as TTask).FParams.Pool.FInteractive);
  Lock:=TSpinLock.Create(False);
  Event:=TEvent.Create;
  CompleteProc:=@TaskCompleted;
  try
    for TaskI in aTasks do
      begin
      Task:=TaskI as TTask;
      if (aTimeout=INFINITE) and not Task.IsComplete then
        Task.InternalExecuteNow;
      Task.AddCompleteEvent(CompleteProc);
      Inc(lAdded);
      end;
    lRemaining:=RemainingTime(Watch,aTimeout);
    While (CompletedCount<Length(aTasks)) and (lRemaining>0) do
      begin
      if NeedSync then
        CheckSynchronize(1)
      else
        Event.WaitFor(lRemaining);
      Event.ResetEvent;
      lRemaining:=RemainingTime(Watch,aTimeout);
      end;
    Result:=CompletedCount>=Length(aTasks);
  finally
    For I:=0 to lAdded-1 do
      (aTasks[I] as TTask).RemoveCompleteEvent(CompleteProc);
    Lock.Enter;
    try
      FreeAndNil(Event);
    finally
      Lock.Exit;
    end;
  end;
  if not Result or ((ExceptionCount=0) and (CancelCount=0)) then
    Exit;
  if (ExceptionCount=0) and (CancelCount>0) then
    raise EOperationCancelled.Create(SErrOneOrMoreTasksCancelled);
  ExceptionList:=TExceptionList.create(Length(aTasks));
  for TaskI in aTasks do
    ExceptionList.AddFromTask(TaskI as TTask);
  if ExceptionList.Count>0 then
    raise EAggregateException.Create(ExceptionList.Truncate);
end;


class function TTask.DoWaitForAny(const aTasks: array of ITask; aTimeout: Cardinal): Integer;

var
  Res : Integer;
  Lock : TSpinLock;
  Event : TEvent;

  Function MakeCompleted(aIndex : integer) : TITaskProc;

  begin
    Result:=Procedure (aTask : ITask)
      begin
      Lock.Enter;
      Try
        if Res=-1 then
          Res:=aIndex;
        if Assigned(Event) then
          Event.SetEvent;
      finally
        Lock.Exit;
      end;
      if aTask<>Nil then;
      end;
    end;

  // Res is written by the completion callbacks under Lock.
  Function NoResult : Boolean;

  begin
    Lock.Enter;
    try
      Result:=(Res=-1);
    finally
      Lock.Exit;
    end;
  end;

var
  I,Len,lAdded : Integer;
  WaitProcs: Array of TITaskProc;
  NeedSync : Boolean;
  Watch : TStopWatch;
  lRemaining : Cardinal;

begin
  Result:=-1;
  Len:=Length(aTasks);
  if Len=0 then
    Exit;
  Watch:=TStopWatch.StartNew;
  Res:=-1;
  lAdded:=0;
  WaitProcs:=[];
  SetLength(WaitProcs,Len);
  NeedSync:=(TThread.CurrentThread.ThreadID=MainThreadID) and ((aTasks[0] as TTask).FParams.Pool.FInteractive);
  Lock:=TSpinLock.Create(False);
  Event:=TEvent.Create;
  try
    While (lAdded<Len) and NoResult do
      begin
      WaitProcs[lAdded]:=MakeCompleted(lAdded);
      (aTasks[lAdded] as TTask).AddCompleteEvent(WaitProcs[lAdded]);
      Inc(lAdded);
      end;
    lRemaining:=RemainingTime(Watch,aTimeout);
    While NoResult and (lRemaining>0) do
      begin
      if NeedSync then
        CheckSynchronize(1)
      else
        Event.WaitFor(lRemaining);
      lRemaining:=RemainingTime(Watch,aTimeout);
      end;
  finally
    For I:=0 to lAdded-1 do
      begin
      (aTasks[I] as TTask).RemoveCompleteEvent(WaitProcs[I]);
      WaitProcs[I]:=Nil;
      end;
    Lock.Enter;
    try
      Result:=Res;
      FreeAndNil(Event);
    finally
      Lock.Exit;
    end;
  end;
  if Result<>-1 then
    aTasks[Result].Wait(0);
end;


class function TTask.Create(aSender: TObject; aEvent: TNotifyEvent; const aPool: TThreadPool): ITask;
var
  Params : TTaskParams;
begin
  Params:=Default(TTaskParams);
  Params.Sender:=aSender;
  Params.Event:=aEvent;
  Params.Pool:=aPool;
  Result:=TTask.Create(Params);
end;

class function TTask.Create(const aProc: TProcRef; aPool: TThreadPool): ITask;
var
  Params : TTaskParams;
begin
  Params:=Default(TTaskParams);
  Params.Proc:=aProc;
  Params.Pool:=aPool;
  Result:=TTask.Create(Params);
end;

class function TTask.Create(aSender: TObject; aEvent: TNotifyEvent; const aCancelToken: TCancellationToken; const aPool: TThreadPool): ITask;
var
  Params : TTaskParams;
begin
  Params:=Default(TTaskParams);
  Params.Sender:=aSender;
  Params.Event:=aEvent;
  Params.Pool:=aPool;
  Params.CancellationToken:=aCancelToken;
  Result:=TTask.Create(Params);
end;

class function TTask.Create(const aProc: TProcRef; const aCancelToken: TCancellationToken; aPool: TThreadPool): ITask;
var
  Params : TTaskParams;
begin
  Params:=Default(TTaskParams);
  Params.Proc:=aProc;
  Params.Pool:=aPool;
  Params.CancellationToken:=aCancelToken;
  Result:=TTask.Create(Params);
end;

class function TTask.Create(aSender: TObject; aEvent: TNotifyEvent): ITask;
begin
  Result:=Create(aSender,aEvent,TThreadPool.Default);
end;

class function TTask.Create(const aProc: TProcRef): ITask;
begin
  Result:=Create(aProc,TThreadPool.Default);
end;

class function TTask.WaitForAll(const aTasks: array of ITask): Boolean;
begin
  Result:=WaitForAll(aTasks,INFINITE);
end;

class function TTask.WaitForAll(const aTasks: array of ITask; aTimeout: Cardinal): Boolean;
begin
  Result:=DoWaitForAll(aTasks,aTimeOut);
end;

class function TTask.WaitForAll(const aTasks: array of ITask; const aTimeout: TTimeSpan): Boolean;
begin
  Result:=WaitForAll(aTasks,Trunc(aTimeOut.TotalMilliseconds));
end;

class function TTask.WaitForAny(const aTasks: array of ITask): Integer;
begin
  Result:=WaitForAny(aTasks,INFINITE);
end;

class function TTask.WaitForAny(const aTasks: array of ITask; aTimeout: Cardinal): Integer;
begin
  Result:=DoWaitForAny(aTasks,aTimeOut);
end;

class function TTask.WaitForAny(const aTasks: array of ITask; const aTimeout: TTimeSpan): Integer;
begin
  Result:=WaitForAny(aTasks,Trunc(aTimeOut.TotalMilliseconds));
end;

generic class function TTask.Future<T>(aSender: TObject; aEvent: specialize TFunctionEvent<T>) : specialize IFuture<T>;
begin
  Result:=specialize TFuture<T>.Create(aSender,aEvent,Nil,TThreadPool.Default);
  Result.StartFuture;
end;

generic class function TTask.Future<T>(aSender: TObject; aEvent: specialize TFunctionEvent<T>; aPool: TThreadPool): Specialize IFuture<T>;
begin
  Result:=specialize TFuture<T>.Create(aSender,aEvent,Nil,aPool);
  Result.StartFuture;
end;

generic class function TTask.Future<T>(aSender: TObject; aEvent: specialize TFunctionEvent<T>; const aCancelToken: TCancellationToken; aPool: TThreadPool): Specialize IFuture<T>;
begin
  Result:=specialize TFuture<T>.Create(aSender,aEvent,Nil,aCancelToken,aPool);
  Result.StartFuture;
end;

generic class function TTask.Future<T>(const aFunc: specialize TFunc<T>): Specialize IFuture<T>; overload; static; inline;

begin
  Result:=specialize TFuture<T>.Create(Nil,Nil,aFunc,TThreadPool.Default);
  Result.StartFuture;
end;

generic class function TTask.Future<T>(const aFunc: specialize TFunc<T>; aPool: TThreadPool): Specialize IFuture<T>; overload; static; inline;

begin
  Result:=specialize TFuture<T>.Create(Nil,Nil,aFunc,aPool);
  Result.StartFuture;
end;

generic class function TTask.Future<T>(const aFunc: specialize TFunc<T>; const aCancelToken: TCancellationToken; aPool: TThreadPool): Specialize IFuture<T>; overload; static; inline;
begin
  Result:=specialize TFuture<T>.Create(Nil,Nil,aFunc,aCancelToken,aPool);
  Result.StartFuture;
end;

{ *********************************************************************
  TTask.TTaskParams
  *********************************************************************}

procedure TTask.TTaskParams.ResolvePool;
begin
  if Not Assigned(Pool) and Assigned(Parent) then
    Pool:=Parent.ThreadPool;
  if Not Assigned(Pool) then
    Pool:=TThreadPool.Current;
end;

{ *********************************************************************
  TFutureContinuationTask
  *********************************************************************}

constructor TFutureContinuationTask.CreateContinuation(const aAntecedent: IFutureT;
  const aAction: TFutureProcT; const aCancelToken: TCancellationToken; aPool: TThreadPool);
var
  Params: TTaskParams;
begin
  Params:=Default(TTaskParams);
  Params.Event:=@RunAction;
  Params.Sender:=Self;
  Params.Pool:=aPool;
  Params.CancellationToken:=aCancelToken;
  FAntecedent:=aAntecedent;
  FAction:=aAction;
  inherited Create(Params);
end;

destructor TFutureContinuationTask.Destroy;
begin
  FAntecedent:=nil;
  FAction:=nil;
  inherited Destroy;
end;

procedure TFutureContinuationTask.RunAction(Sender: TObject);
var
  Ant: IFutureT;
begin
  if Sender=nil then;
  Ant:=FAntecedent;
  FAntecedent:=nil;
  if Assigned(FAction) then
    FAction(Ant);
end;

{ *********************************************************************
  TFuture
  *********************************************************************}

procedure TFuture.RunFunc(Sender: TObject);
var
  Ant: ITask;
begin
  FResult:=Default(T);
  Ant:=FAntecedentTask;
  FAntecedentTask:=nil;
  if Assigned(FFuncRef) then
    FResult:=FFuncRef()
  else if Assigned(FFuncTaskRef) then
    FResult:=FFuncTaskRef(Ant)
  else if Assigned(FFuncEvent) then
    FResult:=FFuncEvent(Sender);
end;

function TFuture.StartFuture: specialize IFuture<T>;
begin
  inherited Start;
  Result:=Self;
end;

Generic function TFuture.GetValue: T;
begin
  Wait;
  Result:=FResult;
end;

constructor TFuture.Create(aSender: TObject; aEvent: TFunctionEventT; const aFunc: specialize TFunc<T>; aPool: TThreadPool);
begin
  Create(aSender,aEvent,aFunc,TCancellationToken.None,aPool);
end;

constructor TFuture.Create(aSender: TObject; aEvent: TFunctionEventT; const aFunc: specialize TFunc<T>;
  const aCancelToken: TCancellationToken; aPool: TThreadPool);
var
  Params : TTaskParams;
begin
  Params:=Default(TTaskParams);
  Params.Event:=@RunFunc;
  Params.Sender:=aSender;
  Params.Pool:=aPool;
  Params.CancellationToken:=aCancelToken;
  FFuncEvent:=aEvent;
  FFuncRef:=aFunc;
  FFuncTaskRef:=nil;
  FAntecedentTask:=nil;
  inherited Create(Params);
end;

constructor TFuture.CreateTaskContinuation(const aAntecedent: ITask; const aTaskFunc: TFunctionTaskRefT;
  const aFunc: TFunctionRefT; const aCancelToken: TCancellationToken; aPool: TThreadPool);
var
  Params : TTaskParams;
begin
  Params:=Default(TTaskParams);
  Params.Event:=@RunFunc;
  Params.Sender:=Self;
  Params.Pool:=aPool;
  Params.CancellationToken:=aCancelToken;
  FFuncEvent:=nil;
  FFuncRef:=aFunc;
  FFuncTaskRef:=aTaskFunc;
  FAntecedentTask:=aAntecedent;
  inherited Create(Params);
end;

destructor TFuture.Destroy;
begin
  FAntecedentTask:=nil;
  FFuncTaskRef:=nil;
  FFuncRef:=nil;
  inherited Destroy;
end;

function TFuture.ContinueWith(const aAction: TFutureProcT): ITask;
begin
  Result:=ContinueWith(aAction,TCancellationToken.None,[],FParams.Pool);
end;

function TFuture.ContinueWith(const aAction: TFutureProcT; aOptions: TTaskContinuationOptions): ITask;
begin
  Result:=ContinueWith(aAction,TCancellationToken.None,aOptions,FParams.Pool);
end;

function TFuture.ContinueWith(const aAction: TFutureProcT; const aCancelToken: TCancellationToken;
  aOptions: TTaskContinuationOptions; aPool: TThreadPool): ITask;
type
  TContTask = specialize TFutureContinuationTask<T>;
var
  ContTask: TContTask;
  EffPool: TThreadPool;
  EffToken: TCancellationToken;
begin
  if Assigned(aPool) then
    EffPool:=aPool
  else
    EffPool:=FParams.Pool;
  EffToken:=aCancelToken;
  if (not EffToken.CanBeCanceled) and (TTaskContinuationOption.NotOnCanceled in aOptions) then
    EffToken:=FParams.CancellationToken;
  ContTask:=TContTask.CreateContinuation(Self,aAction,EffToken,EffPool);
  Result:=ContTask;
  AttachContinuation(ContTask,EffToken,aOptions,False);
end;

{ *********************************************************************
  TTransformFuture
  *********************************************************************}

constructor TTransformFuture.CreateTransform(const aSource: ITask; const aTransform: TTransformFunc;
  const aCancelToken: TCancellationToken; aPool: TThreadPool);
begin
  FSourceTask:=aSource;
  FTransformFunc:=aTransform;
  inherited Create(Self,@RunTransform,nil,aCancelToken,aPool);
end;

destructor TTransformFuture.Destroy;
begin
  FSourceTask:=nil;
  FTransformFunc:=nil;
  inherited Destroy;
end;

function TTransformFuture.RunTransform(Sender: TObject): TResult;
var
  Src: ITask;
begin
  if Sender=nil then;
  Src:=FSourceTask;
  FSourceTask:=nil;
  Result:=FTransformFunc(TSourceFuture(Src as TTask).GetValue);
end;

{ *********************************************************************
  TTask.ThenBy & TFuture.Map
  *********************************************************************}

generic class function TTask.ThenBy<TResult>(const aTask: ITask; const aFunc: specialize TFunc1<ITask, TResult>;
  const aCancelToken: TCancellationToken; aOptions: TTaskContinuationOptions; aPool: TThreadPool): specialize IFuture<TResult>;
type
  TFutureResult = specialize TFuture<TResult>;
var
  AntecedentTask: TTask;
  ContFuture: TFutureResult;
  EffPool: TThreadPool;
  EffToken: TCancellationToken;
begin
  AntecedentTask:=aTask as TTask;
  if Assigned(aPool) then
    EffPool:=aPool
  else
    EffPool:=AntecedentTask.FParams.Pool;
  EffToken:=aCancelToken;
  if not EffToken.CanBeCanceled then
    EffToken:=AntecedentTask.FParams.CancellationToken;
  ContFuture:=TFutureResult.CreateTaskContinuation(aTask,aFunc,nil,EffToken,EffPool);
  Result:=ContFuture;
  AntecedentTask.AttachContinuation(ContFuture,EffToken,aOptions,aOptions=[]);
end;

generic class function TTask.ThenBy<TResult>(const aTask: ITask; const aFunc: specialize TFunc<TResult>;
  const aCancelToken: TCancellationToken; aOptions: TTaskContinuationOptions; aPool: TThreadPool): specialize IFuture<TResult>;
type
  TFutureResult = specialize TFuture<TResult>;
var
  AntecedentTask: TTask;
  ContFuture: TFutureResult;
  EffPool: TThreadPool;
  EffToken: TCancellationToken;
begin
  AntecedentTask:=aTask as TTask;
  if Assigned(aPool) then
    EffPool:=aPool
  else
    EffPool:=AntecedentTask.FParams.Pool;
  EffToken:=aCancelToken;
  if not EffToken.CanBeCanceled then
    EffToken:=AntecedentTask.FParams.CancellationToken;
  ContFuture:=TFutureResult.CreateTaskContinuation(aTask,nil,aFunc,EffToken,EffPool);
  Result:=ContFuture;
  AntecedentTask.AttachContinuation(ContFuture,EffToken,aOptions,aOptions=[]);
end;

generic class function TTask.ThenBy<T, TResult>(const aTask: ITask; const aFunc: specialize TFunc1<T, TResult>;
  const aCancelToken: TCancellationToken; aOptions: TTaskContinuationOptions; aPool: TThreadPool): specialize IFuture<TResult>;
type
  TTransformResult = specialize TTransformFuture<T, TResult>;
var
  AntecedentTask: TTask;
  ContFuture: TTransformResult;
  EffPool: TThreadPool;
  EffToken: TCancellationToken;
begin
  AntecedentTask:=aTask as TTask;
  if Assigned(aPool) then
    EffPool:=aPool
  else
    EffPool:=AntecedentTask.FParams.Pool;
  EffToken:=aCancelToken;
  if not EffToken.CanBeCanceled then
    EffToken:=AntecedentTask.FParams.CancellationToken;
  ContFuture:=TTransformResult.CreateTransform(aTask,aFunc,EffToken,EffPool);
  Result:=ContFuture;
  AntecedentTask.AttachContinuation(ContFuture,EffToken,aOptions,True);
end;

generic class function TTask.ThenBy<T, TResult>(const aTask: ITask; const aFunc: specialize TFunc1<T, TResult>): specialize IFuture<TResult>;
begin
  Result:=TTask.specialize ThenBy<T, TResult>(aTask,aFunc,TCancellationToken.None,[],nil);
end;

generic class function TTask.ThenBy<T, TResult>(const aTask: ITask; const aFunc: specialize TFunc1<T, TResult>;
  aOptions: TTaskContinuationOptions): specialize IFuture<TResult>;
begin
  Result:=TTask.specialize ThenBy<T, TResult>(aTask,aFunc,TCancellationToken.None,aOptions,nil);
end;

function TFuture.Map(const aFunc: TMapFuncT; aOptions: TTaskContinuationOptions): specialize IFuture<T>;
begin
  Result:=Map(aFunc,TCancellationToken.None,aOptions,FParams.Pool);
end;

function TFuture.Map(const aFunc: TMapFuncT; const aCancelToken: TCancellationToken;
  aOptions: TTaskContinuationOptions; aPool: TThreadPool): specialize IFuture<T>;
begin
  Result:=TTask.specialize ThenBy<T, T>(Self,aFunc,aCancelToken,aOptions,aPool);
end;

{ *********************************************************************
  TITaskHelper
  *********************************************************************}

generic function TITaskHelper.ThenBy<TResult>(const aFunc: specialize TFunc1<ITask, TResult>): specialize IFuture<TResult>;
begin
  Result:=TTask.specialize ThenBy<TResult>(Self,aFunc,TCancellationToken.None,[],nil);
end;

generic function TITaskHelper.ThenBy<TResult>(const aFunc: specialize TFunc1<ITask, TResult>;
  aOptions: TTaskContinuationOptions): specialize IFuture<TResult>;
begin
  Result:=TTask.specialize ThenBy<TResult>(Self,aFunc,TCancellationToken.None,aOptions,nil);
end;

generic function TITaskHelper.ThenBy<TResult>(const aFunc: specialize TFunc1<ITask, TResult>;
  const aCancelToken: TCancellationToken; aOptions: TTaskContinuationOptions;
  aPool: TThreadPool): specialize IFuture<TResult>;
begin
  Result:=TTask.specialize ThenBy<TResult>(Self,aFunc,aCancelToken,aOptions,aPool);
end;

generic function TITaskHelper.ThenBy<TResult>(const aFunc: specialize TFunc<TResult>): specialize IFuture<TResult>;
begin
  Result:=TTask.specialize ThenBy<TResult>(Self,aFunc,TCancellationToken.None,[],nil);
end;

generic function TITaskHelper.ThenBy<TResult>(const aFunc: specialize TFunc<TResult>;
  aOptions: TTaskContinuationOptions): specialize IFuture<TResult>;
begin
  Result:=TTask.specialize ThenBy<TResult>(Self,aFunc,TCancellationToken.None,aOptions,nil);
end;

generic function TITaskHelper.ThenBy<TResult>(const aFunc: specialize TFunc<TResult>;
  const aCancelToken: TCancellationToken; aOptions: TTaskContinuationOptions;
  aPool: TThreadPool): specialize IFuture<TResult>;
begin
  Result:=TTask.specialize ThenBy<TResult>(Self,aFunc,aCancelToken,aOptions,aPool);
end;

generic function TITaskHelper.ThenBy<T, TResult>(const aFunc: specialize TFunc1<T, TResult>): specialize IFuture<TResult>;
begin
  Result:=TTask.specialize ThenBy<T, TResult>(Self,aFunc,TCancellationToken.None,[],nil);
end;

generic function TITaskHelper.ThenBy<T, TResult>(const aFunc: specialize TFunc1<T, TResult>;
  aOptions: TTaskContinuationOptions): specialize IFuture<TResult>;
begin
  Result:=TTask.specialize ThenBy<T, TResult>(Self,aFunc,TCancellationToken.None,aOptions,nil);
end;

generic function TITaskHelper.ThenBy<T, TResult>(const aFunc: specialize TFunc1<T, TResult>;
  const aCancelToken: TCancellationToken; aOptions: TTaskContinuationOptions;
  aPool: TThreadPool): specialize IFuture<TResult>;
begin
  Result:=TTask.specialize ThenBy<T, TResult>(Self,aFunc,aCancelToken,aOptions,aPool);
end;


{ *********************************************************************
  TReplicableTask
  *********************************************************************}

constructor TEventJoinTask.Create(Sender: TObject; const AEvents: array of TNotifyEvent; APool: TThreadPool);
begin
  Create(Sender,AEvents,TCancellationToken.None,APool);
end;

constructor TEventJoinTask.Create(Sender: TObject; const AEvents: array of TNotifyEvent;
  const ACancelToken: TCancellationToken; APool: TThreadPool);
var
  I,Len : integer;
  Params : TTaskParams;

begin
  // Copy procs
  Len:=Length(AEvents);
  SetLength(FEventList,Len);
  For I:=0 to Len-1 do
    FEventList[I]:=aEvents[i];
  Params:=Default(TTaskParams);
  Params.Proc:=@JoinTasks;
  Params.Pool:=aPool;
  Params.Sender:=Sender;
  Params.CancellationToken:=ACancelToken;
  Inherited Create(Params);
end;

procedure TEventJoinTask.JoinTasks;

var
  Proc : TParallel.TInt32Proc;
  Loop : TParallel.TInt32LoopProc;

begin
  Proc:=Procedure(aIndex : Integer)
    begin
      FEventList[aIndex](Self.FParams.Sender);
    end;
  Loop:=TParallel.TInt32LoopProc.create(0,Length(FEventList)-1,Proc);
  Loop.CancelToken:=Self.FParams.CancellationToken;
  TParallel.Parallelize32(Loop,Self.ThreadPool);
end;

{ *********************************************************************
  TReplicableTask
  *********************************************************************}

function TReplicableTask.ShouldCreateReplica: Boolean;

begin
  {$IFDEF USE_THREADLOG}ThreadLog('TReplicableTask.ShouldCreateReplica','Enter (TaskCount: %d)',[FTaskCount]);{$ENDIF USE_THREADLOG}
  Result:=False;
  if (FTaskCount<=0) then
    exit;
  AtomicDecrement(FTaskCount);
  Result:=FTaskCount>0;
  {$IFDEF USE_THREADLOG}ThreadLog('TReplicableTask.ShouldCreateReplica','Leave: %s ',[BToS(Result)]);{$ENDIF USE_THREADLOG}
end;

function TReplicableTask.CreateReplicaTask(const aParams : TTaskParams): TTask;

begin
  {$IFDEF USE_THREADLOG}ThreadLog('TReplicableTask.CreateReplicaTask','Enter');{$ENDIF USE_THREADLOG}
  Result:=TReplicatedTask.Create(aParams);
  {$IFDEF USE_THREADLOG}ThreadLog('TReplicableTask.CreateReplicaTask','Leave');{$ENDIF USE_THREADLOG}
end;

constructor TReplicableTask.Create(const aParams : TTaskParams; aTaskCount: Integer);
begin
  {$IFDEF USE_THREADLOG}ThreadLog('TReplicableTask.Create','Enter  (%d)',[aTaskCount]);{$ENDIF USE_THREADLOG}
  inherited Create(aParams);
  FTaskCount:=aTaskCount;
  if FTaskCount<0 then
    FTaskCount:=2*TThread.ProcessorCount;
  {$IFDEF USE_THREADLOG}ThreadLog('TReplicableTask.Create','Leave');{$ENDIF USE_THREADLOG}
end;

{ *********************************************************************
  TProcJoinTask
  *********************************************************************}

procedure TProcJoinTask.JoinTasks;

var
  Proc : TParallel.TInt32Proc;
  Loop : TParallel.TInt32LoopProc;

begin
  Proc:=Procedure(aIndex : Integer)
    begin
      FProcList[aIndex];
    end;
  Loop:=TParallel.TInt32LoopProc.create(0,Length(FProcList)-1,Proc);
  Loop.CancelToken:=Self.FParams.CancellationToken;
  TParallel.Parallelize32(Loop,Self.ThreadPool);
end;

constructor TProcJoinTask.Create(const AProcs: array of TProcRef; APool: TThreadPool);
begin
  Create(AProcs,TCancellationToken.None,APool);
end;

constructor TProcJoinTask.Create(const AProcs: array of TProcRef; const ACancelToken: TCancellationToken; APool: TThreadPool);

var
  I,Len : integer;
  Params : TTaskParams;

begin
  // Copy procs
  Len:=Length(aProcs);
  SetLength(FProcList,Len);
  For I:=0 to Len-1 do
    FProcList[I]:=aProcs[i];
  Params:=Default(TTaskParams);
  Params.Proc:=@JoinTasks;
  Params.Pool:=aPool;
  Params.CancellationToken:=ACancelToken;
  Inherited Create(Params);
end;


{ *********************************************************************
  TParallel
  *********************************************************************}

class function TParallel.Parallelize32(aLoop: TInt32LoopProc; aPool: TThreadPool): TLoopResult;


var
  LoopParams : TInt32LoopParams;
  LoopI : ILoopParams;
  TaskParams : TTask.TTaskParams;
  ControlFlag: TThreadPool.IControlFlag;
  aTask : ITask;

begin
  Result:=TLoopResult.Create;
  With aLoop do
    if HighExclusive<=LowInclusive then
      Exit;
  if (not aLoop.CancelToken.CanBeCanceled) and Assigned(TTask._CurrentTask) then
    aLoop.CancelToken:=TTask._CurrentTask.FParams.CancellationToken;
  if aLoop.CancelToken.IsCancellationRequested then
    begin
    Result.FCompleted:=False;
    Result.FLowestBreakIteration:=Null;
    Exit;
    end;
  aLoop.Index:=aLoop.LowInclusive;
  if aLoop.Stride<=0 then
    aLoop.Stride:=1;
  ControlFlag:=Nil;
  if TTask.CurrentTask <> nil then
    ControlFlag:=(TTask.CurrentTask as TAbstractTask.IInternalTask).GetControlFlag;
  LoopParams:=TInt32LoopParams.Create(aLoop);
  LoopI:=LoopParams;
  try
    TaskParams:=Default(TTask.TTaskParams);
    TaskParams.ParentControlFlag:=ControlFlag;
    TaskParams.Pool:=aPool;
    TaskParams.CreateFlags:=[TTask.TCreateFlag.Replicating];
    TaskParams.Proc:=LoopParams;
    LoopI.CreateRootTask(TaskParams,aLoop.NumTasks);
    try
      aTask:=LoopI.StartLoop;
      aTask.Wait;
    except
      on E: EOperationCancelled do
        begin
        if not aLoop.CancelToken.CanBeCanceled then
          LoopI.HandleException;
        end;
      else
        LoopI.HandleException;
    end;
    With Result do
      begin
      FCompleted:=LoopParams.StateFlags=[];
      if not FCompleted then
        FLowestBreakIteration:=LoopParams.BreakAt
      end
  finally
    LoopI.ClearRootTask; // Root task holds a reference to the loop. We need to free the root task.
    TaskParams.Proc:=Nil;
    LoopI:=Nil;
    aTask:=nil;
    LoopParams:=nil;
  end;
end;


class function TParallel.&For(aSender: TObject; aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TIteratorEvent; aPool: TThreadPool): TLoopResult;
var
  aLoop: TInt32LoopProc;
begin
  aLoop:=TInt32LoopProc.Create(aSender,aLowInclusive,aHighInclusive,aIteratorEvent);
  Result:=Parallelize32(aLoop,aPool);
end;

class function TParallel.&For(aSender: TObject; aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TIteratorEvent): TLoopResult;

begin
  Result:=&For(aSender,aLowInclusive,aHighInclusive,aIteratorEvent,TThreadPool.Default);
end;

class function TParallel.&For(aSender: TObject; aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TIteratorEvent;
  const aCancelToken: TCancellationToken; aPool: TThreadPool): TLoopResult;
var
  aLoop: TInt32LoopProc;
begin
  aLoop:=TInt32LoopProc.Create(aSender,aLowInclusive,aHighInclusive,aIteratorEvent);
  aLoop.CancelToken:=aCancelToken;
  Result:=Parallelize32(aLoop,aPool);
end;

class function TParallel.&For(aSender: TObject; aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TIteratorStateEvent;
  aPool: TThreadPool): TLoopResult;
var
  aLoop: TInt32LoopProc;
begin
  aLoop:=TInt32LoopProc.Create(aSender,aLowInclusive,aHighInclusive,aIteratorEvent);
  Result:=Parallelize32(aLoop,aPool);
end;

class function TParallel.&For(aSender: TObject; aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TIteratorStateEvent): TLoopResult;
begin
  Result:=&For(aSender,aLowInclusive,aHighInclusive,aIteratorEvent,TThreadPool.Default);
end;

class function TParallel.&For(aSender: TObject; aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TIteratorStateEvent;
  const aCancelToken: TCancellationToken; aPool: TThreadPool): TLoopResult;
var
  aLoop: TInt32LoopProc;
begin
  aLoop:=TInt32LoopProc.Create(aSender,aLowInclusive,aHighInclusive,aIteratorEvent);
  aLoop.CancelToken:=aCancelToken;
  Result:=Parallelize32(aLoop,aPool);
end;

class function TParallel.&For(aSender: TObject; aStride, aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TIteratorEvent;
  aPool: TThreadPool): TLoopResult;
var
  aLoop: TInt32LoopProc;
begin
  aLoop:=TInt32LoopProc.Create(aSender,aLowInclusive,aHighInclusive,aIteratorEvent);
  aLoop.Stride:=aStride;
  Result:=Parallelize32(aLoop,aPool);
end;

class function TParallel.&For(aSender: TObject; aStride, aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TIteratorEvent
  ): TLoopResult;
begin
  Result:=&For(aSender,aStride,aLowInclusive,aHighInclusive,aIteratorEvent,TThreadPool.Default);
end;

class function TParallel.&For(aSender: TObject; aStride, aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TIteratorEvent;
  const aCancelToken: TCancellationToken; aPool: TThreadPool): TLoopResult;
var
  aLoop: TInt32LoopProc;
begin
  aLoop:=TInt32LoopProc.Create(aSender,aLowInclusive,aHighInclusive,aIteratorEvent);
  aLoop.Stride:=aStride;
  aLoop.CancelToken:=aCancelToken;
  Result:=Parallelize32(aLoop,aPool);
end;

class function TParallel.&For(aSender: TObject; aStride, aLowInclusive, aHighInclusive: Integer;
  aIteratorEvent: TIteratorStateEvent; aPool: TThreadPool): TLoopResult;
var
  aLoop: TInt32LoopProc;
begin
  aLoop:=TInt32LoopProc.Create(aSender,aLowInclusive,aHighInclusive,aIteratorEvent);
  aLoop.Stride:=aStride;
  Result:=Parallelize32(aLoop,aPool);
end;

class function TParallel.&For(aSender: TObject; aStride, aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TIteratorStateEvent
  ): TLoopResult;
begin
  Result:=&For(aSender,aStride,aLowInclusive,aHighInclusive,aIteratorEvent,TThreadPool.Default);
end;

class function TParallel.&For(aSender: TObject; aStride, aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TIteratorStateEvent;
  const aCancelToken: TCancellationToken; aPool: TThreadPool): TLoopResult;
var
  aLoop: TInt32LoopProc;
begin
  aLoop:=TInt32LoopProc.Create(aSender,aLowInclusive,aHighInclusive,aIteratorEvent);
  aLoop.Stride:=aStride;
  aLoop.CancelToken:=aCancelToken;
  Result:=Parallelize32(aLoop,aPool);
end;


class function TParallel.&For(aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TProcInteger; aPool: TThreadPool
  ): TLoopResult;
var
  aLoop: TInt32LoopProc;
begin
  aLoop:=TInt32LoopProc.Create(aLowInclusive,aHighInclusive,aIteratorEvent);
  Result:=Parallelize32(aLoop,aPool);
end;

class function TParallel.&For(aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TProcInteger): TLoopResult;
begin
  Result:=&For(aLowInclusive,aHighInclusive,aIteratorEvent,TThreadPool.Default);
end;

class function TParallel.&For(aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TProcInteger;
  const aCancelToken: TCancellationToken; aPool: TThreadPool): TLoopResult;
var
  aLoop: TInt32LoopProc;
begin
  aLoop:=TInt32LoopProc.Create(aLowInclusive,aHighInclusive,aIteratorEvent);
  aLoop.CancelToken:=aCancelToken;
  Result:=Parallelize32(aLoop,aPool);
end;


class function TParallel.&For(aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TProcIntegerLoopState;
  aPool: TThreadPool): TLoopResult;
var
  aLoop: TInt32LoopProc;
begin
  aLoop:=TInt32LoopProc.Create(aLowInclusive,aHighInclusive,aIteratorEvent);
  Result:=Parallelize32(aLoop,aPool);
end;

class function TParallel.&For(aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TProcIntegerLoopState): TLoopResult;
begin
  Result:=&For(aLowInclusive,aHighInclusive,aIteratorEvent,TThreadPool.Default);
end;

class function TParallel.&For(aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TProcIntegerLoopState;
  const aCancelToken: TCancellationToken; aPool: TThreadPool): TLoopResult;
var
  aLoop: TInt32LoopProc;
begin
  aLoop:=TInt32LoopProc.Create(aLowInclusive,aHighInclusive,aIteratorEvent);
  aLoop.CancelToken:=aCancelToken;
  Result:=Parallelize32(aLoop,aPool);
end;


class function TParallel.&For(aStride, aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TProcInteger; aPool: TThreadPool): TLoopResult;

var
  aLoop: TInt32LoopProc;

begin
  aLoop:=TInt32LoopProc.Create(aLowInclusive,aHighInclusive,aIteratorEvent);
  aLoop.Stride:=aStride;
  Result:=Parallelize32(aLoop,aPool);
end;

class function TParallel.&For(aStride, aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TProcInteger): TLoopResult;

begin
  Result:=&For(aStride, aLowInclusive,aHighInclusive,aIteratorEvent,TThreadPool.Default);
end;

class function TParallel.&For(aStride, aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TProcInteger;
  const aCancelToken: TCancellationToken; aPool: TThreadPool): TLoopResult;
var
  aLoop: TInt32LoopProc;
begin
  aLoop:=TInt32LoopProc.Create(aLowInclusive,aHighInclusive,aIteratorEvent);
  aLoop.Stride:=aStride;
  aLoop.CancelToken:=aCancelToken;
  Result:=Parallelize32(aLoop,aPool);
end;

class function TParallel.&For(aStride, aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TProcIntegerLoopState;
  aPool: TThreadPool): TLoopResult;
var
  aLoop: TInt32LoopProc;

begin
  aLoop:=TInt32LoopProc.Create(aLowInclusive,aHighInclusive,aIteratorEvent);
  aLoop.Stride:=aStride;
  Result:=Parallelize32(aLoop,aPool);
end;

class function TParallel.&For(aStride, aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TProcIntegerLoopState): TLoopResult;
begin
  Result:=&For(aStride, aLowInclusive,aHighInclusive,aIteratorEvent,TThreadPool.Default);
end;

class function TParallel.&For(aStride, aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TProcIntegerLoopState;
  const aCancelToken: TCancellationToken; aPool: TThreadPool): TLoopResult;
var
  aLoop: TInt32LoopProc;
begin
  aLoop:=TInt32LoopProc.Create(aLowInclusive,aHighInclusive,aIteratorEvent);
  aLoop.Stride:=aStride;
  aLoop.CancelToken:=aCancelToken;
  Result:=Parallelize32(aLoop,aPool);
end;


{$IFDEF THREAD64BIT}

class function TParallel.Parallelize64(aLoop: TInt64LoopProc; aPool: TThreadPool): TLoopResult;

var
  LoopParams : TInt64LoopParams;
  LoopI : ILoopParams;
  TaskParams : TTask.TTaskParams;
  ControlFlag: TThreadPool.IControlFlag;
  aTask : ITask;

begin
  Result:=TLoopResult.Create;
  With aLoop do
    if HighExclusive<=LowInclusive then
      Exit;
  if (not aLoop.CancelToken.CanBeCanceled) and Assigned(TTask._CurrentTask) then
    aLoop.CancelToken:=TTask._CurrentTask.FParams.CancellationToken;
  if aLoop.CancelToken.IsCancellationRequested then
    begin
    Result.FCompleted:=False;
    Result.FLowestBreakIteration:=Null;
    Exit;
    end;
  aLoop.Index:=aLoop.LowInclusive;
  if aLoop.Stride<=0 then
    aLoop.Stride:=1;
  ControlFlag:=Nil;
  if TTask.CurrentTask <> nil then
    ControlFlag:=(TTask.CurrentTask as TAbstractTask.IInternalTask).GetControlFlag;
  LoopParams:=TInt64LoopParams.Create(aLoop);
  LoopI:=LoopParams;
  try
    TaskParams:=Default(TTask.TTaskParams);
    TaskParams.ParentControlFlag:=ControlFlag;
    TaskParams.Pool:=aPool;
    TaskParams.CreateFlags:=[TTask.TCreateFlag.Replicating];
    TaskParams.Proc:=LoopParams;
    LoopI.CreateRootTask(TaskParams,aLoop.NumTasks);
    try
      aTask:=LoopI.StartLoop;
      aTask.Wait;
    except
      on E: EOperationCancelled do
        begin
        if not aLoop.CancelToken.CanBeCanceled then
          LoopI.HandleException;
        end;
      else
        LoopI.HandleException;
    end;
    With Result do
      begin
      FCompleted:=LoopParams.StateFlags=[];
      if not FCompleted then
        FLowestBreakIteration:=LoopParams.BreakAt
      end
  finally
    LoopI.ClearRootTask; // Root task holds a reference to the loop. We need to free the root task.
    TaskParams.Proc:=Nil;
    LoopI:=Nil;
    aTask:=nil;
    LoopParams:=nil;
  end;
end;


class function TParallel.&For(aSender: TObject; aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TIteratorEvent64;
  aPool: TThreadPool): TLoopResult;
var
  aLoop: TInt64LoopProc;
begin
  aLoop:=TInt64LoopProc.Create(aSender,aLowInclusive,aHighInclusive,aIteratorEvent);
  Result:=Parallelize64(aLoop,aPool);
end;

class function TParallel.&For(aSender: TObject; aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TIteratorEvent64
  ): TLoopResult;
begin
  Result:=&For(aSender,aLowInclusive,aHighInclusive,aIteratorEvent,TThreadPool.Default);
end;

class function TParallel.&For(aSender: TObject; aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TIteratorEvent64;
  const aCancelToken: TCancellationToken; aPool: TThreadPool): TLoopResult;
var
  aLoop: TInt64LoopProc;
begin
  aLoop:=TInt64LoopProc.Create(aSender,aLowInclusive,aHighInclusive,aIteratorEvent);
  aLoop.CancelToken:=aCancelToken;
  Result:=Parallelize64(aLoop,aPool);
end;

class function TParallel.&For(aSender: TObject; aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TIteratorStateEvent64;
  aPool: TThreadPool): TLoopResult;
var
  aLoop: TInt64LoopProc;
begin
  aLoop:=TInt64LoopProc.Create(aSender,aLowInclusive,aHighInclusive,aIteratorEvent);
  Result:=Parallelize64(aLoop,aPool);
end;

class function TParallel.&For(aSender: TObject; aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TIteratorStateEvent64): TLoopResult;
begin
  Result:=&For(aSender,aLowInclusive,aHighInclusive,aIteratorEvent,TThreadPool.Default);
end;

class function TParallel.&For(aSender: TObject; aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TIteratorStateEvent64;
  const aCancelToken: TCancellationToken; aPool: TThreadPool): TLoopResult;
var
  aLoop: TInt64LoopProc;
begin
  aLoop:=TInt64LoopProc.Create(aSender,aLowInclusive,aHighInclusive,aIteratorEvent);
  aLoop.CancelToken:=aCancelToken;
  Result:=Parallelize64(aLoop,aPool);
end;

class function TParallel.&For(aSender: TObject; aStride, aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TIteratorEvent64;
  aPool: TThreadPool): TLoopResult;
var
  aLoop: TInt64LoopProc;
begin
  aLoop:=TInt64LoopProc.Create(aSender,aLowInclusive,aHighInclusive,aIteratorEvent);
  aLoop.Stride:=aStride;
  Result:=Parallelize64(aLoop,aPool);
end;

class function TParallel.&For(aSender: TObject; aStride, aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TIteratorEvent64
  ): TLoopResult;
begin
  Result:=&For(aSender,aStride,aLowInclusive,aHighInclusive,aIteratorEvent,TThreadPool.Default);
end;

class function TParallel.&For(aSender: TObject; aStride, aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TIteratorEvent64;
  const aCancelToken: TCancellationToken; aPool: TThreadPool): TLoopResult;
var
  aLoop: TInt64LoopProc;
begin
  aLoop:=TInt64LoopProc.Create(aSender,aLowInclusive,aHighInclusive,aIteratorEvent);
  aLoop.Stride:=aStride;
  aLoop.CancelToken:=aCancelToken;
  Result:=Parallelize64(aLoop,aPool);
end;


class function TParallel.&For(aSender: TObject; aStride, aLowInclusive, aHighInclusive: Int64;
  aIteratorEvent: TIteratorStateEvent64; aPool: TThreadPool): TLoopResult;
var
  aLoop: TInt64LoopProc;
begin
  aLoop:=TInt64LoopProc.Create(aSender,aLowInclusive,aHighInclusive,aIteratorEvent);
  aLoop.Stride:=aStride;
  Result:=Parallelize64(aLoop,aPool);
end;

class function TParallel.&For(aSender: TObject; aStride, aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TIteratorStateEvent64
  ): TLoopResult;
begin
  Result:=&For(aSender,aStride,aLowInclusive,aHighInclusive,aIteratorEvent,TThreadPool.Default);
end;

class function TParallel.&For(aSender: TObject; aStride, aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TIteratorStateEvent64;
  const aCancelToken: TCancellationToken; aPool: TThreadPool): TLoopResult;
var
  aLoop: TInt64LoopProc;
begin
  aLoop:=TInt64LoopProc.Create(aSender,aLowInclusive,aHighInclusive,aIteratorEvent);
  aLoop.Stride:=aStride;
  aLoop.CancelToken:=aCancelToken;
  Result:=Parallelize64(aLoop,aPool);
end;

class function TParallel.&For(aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TProcInt64; aPool: TThreadPool
  ): TLoopResult;
var
  aLoop: TInt64LoopProc;
begin
  aLoop:=TInt64LoopProc.Create(aLowInclusive,aHighInclusive,aIteratorEvent);
  Result:=Parallelize64(aLoop,aPool);
end;

class function TParallel.&For(aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TProcInt64): TLoopResult;
begin
  Result:=&For(aLowInclusive,aHighInclusive,aIteratorEvent,TThreadPool.Default);
end;

class function TParallel.&For(aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TProcInt64;
  const aCancelToken: TCancellationToken; aPool: TThreadPool): TLoopResult;
var
  aLoop: TInt64LoopProc;
begin
  aLoop:=TInt64LoopProc.Create(aLowInclusive,aHighInclusive,aIteratorEvent);
  aLoop.CancelToken:=aCancelToken;
  Result:=Parallelize64(aLoop,aPool);
end;

class function TParallel.&For(aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TProcInt64LoopState; aPool: TThreadPool
  ): TLoopResult;
var
  aLoop: TInt64LoopProc;
begin
  aLoop:=TInt64LoopProc.Create(aLowInclusive,aHighInclusive,aIteratorEvent);
  Result:=Parallelize64(aLoop,aPool);
end;

class function TParallel.&For(aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TProcInt64LoopState): TLoopResult;
begin
  Result:=&For(aLowInclusive,aHighInclusive,aIteratorEvent,TThreadPool.Default);
end;

class function TParallel.&For(aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TProcInt64LoopState;
  const aCancelToken: TCancellationToken; aPool: TThreadPool): TLoopResult;
var
  aLoop: TInt64LoopProc;
begin
  aLoop:=TInt64LoopProc.Create(aLowInclusive,aHighInclusive,aIteratorEvent);
  aLoop.CancelToken:=aCancelToken;
  Result:=Parallelize64(aLoop,aPool);
end;

class function TParallel.&For(aStride, aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TProcInt64; aPool: TThreadPool): TLoopResult;

var
  aLoop: TInt64LoopProc;
begin
  aLoop:=TInt64LoopProc.Create(aLowInclusive,aHighInclusive,aIteratorEvent);
  aLoop.Stride:=aStride;
  Result:=Parallelize64(aLoop,aPool);
end;

class function TParallel.&For(aStride, aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TProcInt64): TLoopResult;
begin
  Result:=&For(aStride,aLowInclusive,aHighInclusive,aIteratorEvent,TThreadPool.Default);
end;

class function TParallel.&For(aStride, aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TProcInt64;
  const aCancelToken: TCancellationToken; aPool: TThreadPool): TLoopResult;
var
  aLoop: TInt64LoopProc;
begin
  aLoop:=TInt64LoopProc.Create(aLowInclusive,aHighInclusive,aIteratorEvent);
  aLoop.Stride:=aStride;
  aLoop.CancelToken:=aCancelToken;
  Result:=Parallelize64(aLoop,aPool);
end;

class function TParallel.&For(aStride, aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TProcInt64LoopState;
  aPool: TThreadPool): TLoopResult;
var
  aLoop: TInt64LoopProc;
begin
  aLoop:=TInt64LoopProc.Create(aLowInclusive,aHighInclusive,aIteratorEvent);
  aLoop.Stride:=aStride;
  Result:=Parallelize64(aLoop,aPool);
end;

class function TParallel.&For(aStride, aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TProcInt64LoopState
  ): TLoopResult;
begin
  Result:=&For(aStride,aLowInclusive,aHighInclusive,aIteratorEvent,TThreadPool.Default);
end;

class function TParallel.&For(aStride, aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TProcInt64LoopState;
  const aCancelToken: TCancellationToken; aPool: TThreadPool): TLoopResult;
var
  aLoop: TInt64LoopProc;
begin
  aLoop:=TInt64LoopProc.Create(aLowInclusive,aHighInclusive,aIteratorEvent);
  aLoop.Stride:=aStride;
  aLoop.CancelToken:=aCancelToken;
  Result:=Parallelize64(aLoop,aPool);
end;
{$ENDIF}

class function TParallel.Join(aSender: TObject; aEvents: array of TNotifyEvent; aPool: TThreadPool): ITask;
begin
  Result:=TEventJoinTask.Create(aSender,aEvents,aPool).Start;
end;

class function TParallel.Join(aSender: TObject; aEvents: array of TNotifyEvent; const aCancelToken: TCancellationToken; aPool: TThreadPool): ITask;
begin
  Result:=TEventJoinTask.Create(aSender,aEvents,aCancelToken,aPool).Start;
end;

class function TParallel.Join(const aProcs: array of TProcRef; aPool: TThreadPool): ITask;
begin
  Result:=TProcJoinTask.Create(aProcs,aPool).Start;
end;

class function TParallel.Join(const aProcs: array of TProcRef; const aCancelToken: TCancellationToken; aPool: TThreadPool): ITask;
begin
  Result:=TProcJoinTask.Create(aProcs,aCancelToken,aPool).Start;
end;

class function TParallel.Join(aSender: TObject; aEvents: array of TNotifyEvent): ITask;
begin
  Result:=Join(aSender,aEvents,TThreadPool.Default);
end;

class function TParallel.Join(aSender: TObject; aEvent1, aEvent2: TNotifyEvent; aPool: TThreadPool): ITask;
begin
  Result:=Join(aSender,[aEvent1,aEvent2],aPool);
end;

class function TParallel.Join(aSender: TObject; aEvent1, aEvent2: TNotifyEvent): ITask;
begin
  Result:=Join(aSender,aEvent1,aEvent2,TThreadPool.Default);
end;

class function TParallel.Join(const aProcs: array of TProcRef): ITask;
begin
  Result:=Join(aProcs,TThreadPool.Default);
end;

class function TParallel.Join(const aProc1, aProc2: TProcRef; aPool: TThreadPool): ITask;

begin
  Result:=Join([aProc1,aProc2],aPool);
end;

class function TParallel.Join(const aProc1, aProc2: TProcRef): ITask;
begin
  Result:=Join([aProc1,aProc2],TThreadPool.Default);
end;

{ *********************************************************************
  TParallel.TLoopState
  *********************************************************************}

constructor TParallel.TLoopState.Create(LoopParams : TLoopStateFlag);
begin
  FLoopParams:=LoopParams;
end;

function TParallel.TLoopState.GetStopped: Boolean;
begin
  Result:=FLoopParams.Stopped;
end;

function TParallel.TLoopState.GetCancelled: Boolean;
begin
  Result:=FLoopParams.Cancelled;
end;

function TParallel.TLoopState.GetFaulted: Boolean;
begin
  Result:=FLoopParams.Faulted;
end;

function TParallel.TLoopState.GetLowestBreakIteration: Variant;
begin
  Result:=DoGetLowestBreakIteration;
end;


function TParallel.TLoopState.DoGetLowestBreakIteration: Variant;
begin
  Result:=FLoopParams.GetBreakAt;
end;

procedure TParallel.TLoopState.Break;
begin
  DoBreak;
end;

procedure TParallel.TLoopState.Stop;
begin
  FLoopParams.Stop;
end;

function TParallel.TLoopState.ShouldExit: Boolean;
begin
  Result:=DoShouldExit;
end;

{ *********************************************************************
  TParallel.TLoopState32
  *********************************************************************}

constructor TParallel.TLoopState32.Create(aParams: TInt32LoopParams);
begin
  Inherited Create(aParams);
end;

procedure TParallel.TLoopState32.DoBreak;

begin
  // update state
  TInt32LoopParams(FLoopParams).UpdateBreakAt(CurrentIteration);
  if not FLoopParams.Break then
    exit;
end;

function TParallel.TLoopState32.DoShouldExit: Boolean;
begin
  Result:=TInt32LoopParams(FLoopParams).ShouldExitLoop(CurrentIteration);
end;

{ *********************************************************************
  TParallel.TLoopState64
  *********************************************************************}
{$IFDEF THREAD64BIT}
constructor TParallel.TLoopState64.Create(aParams: TInt64LoopParams);
begin
  Inherited Create(aParams);
end;

procedure TParallel.TLoopState64.DoBreak;

begin
  // update state
  TInt64LoopParams(FLoopParams).UpdateBreakAt(CurrentIteration);
  if not FLoopParams.Break then
    exit;
end;

function TParallel.TLoopState64.DoShouldExit: Boolean;
begin
  Result:=TInt64LoopParams(FLoopParams).ShouldExitLoop(CurrentIteration);
end;
{$ENDIF}


{ *********************************************************************
  TInt32LoopParams
  *********************************************************************}


procedure TParallel.TInt32LoopParams.UpdateBreakAt(aValue: Integer);
begin
  Lock;
  try
    if aValue<FBreakAt then
      FBreakAt:=aValue;
  finally
    Unlock;
  end;
end;

function TParallel.TInt32LoopParams.GetBreakAt: Variant;
begin
  Lock;
  try
    if TLoopStateFlag.Broken in StateFlags then
      Result:=FBreakAt
    else
      Result:=Null;
  finally
    Unlock;
  end;
end;

function TParallel.TInt32LoopParams.GetCurrentStride: Integer;
begin
  Result:=Stride
end;

function TParallel.TInt32LoopParams.GetNextStride: Integer;

Var
  NewValue : Integer;
  NextOK,MaxReached : Boolean;

begin
  Result:=GetCurrentStride;
  MaxReached:=(Result>=FMaxStride);
  NextOK:=(AtomicIncrement(FStrideCount) mod FNextStrideAt) = 0;
  {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt32LoopParams.GetNextStride','Current: %d, Count: %d, nextat: %d',[Result, FStrideCount, FNextStrideAt]);{$ENDIF USE_THREADLOG}
  {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt32LoopParams.GetNextStride','if %s or not %s then',[BToS(MaxReached), BToS(NextOK)]);{$ENDIF USE_THREADLOG}
  if MaxReached  or Not NextOK then
    begin
    {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt32LoopParams.GetNextStride','Early exit');{$ENDIF USE_THREADLOG}
    exit;
    end;
  NewValue:=Result*2;
  if (NewValue>FMaxStride) then
    NewValue:=FMaxStride;
  // Only get new value if old did not change
  AtomicCmpExchange(FLoopProc.Stride,NewValue,Result);
  {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt32LoopParams.GetNextStride','Result: %d',[Result]);{$ENDIF USE_THREADLOG}
end;

function TParallel.TInt32LoopParams.ShouldExitLoop: Boolean;
var
  Flags: TLoopStateFlagSet;

begin
  Result:=False;
  if FCancelToken.IsCancellationRequested then
    begin
    Cancel;
    Exit(True);
    end;
  Flags:=StateFlags;
  If (Flags=[]) then
    exit;
  If (Flags*ShouldExitFlags)<>[] then
    Exit(True);
end;


function TParallel.TInt32LoopParams.ShouldExitLoop(CurrentIter : Integer): Boolean;

begin
  Result:=ShouldExitLoop();
  if Result then
    exit;
  Result:=(TLoopStateFlag.Broken in StateFlags) and (CurrentIter>FBreakAt);
end;



// Claim the next range of iterations. The stride used for the claim is
// returned in aStride: the caller must execute exactly that many iterations,
// since other workers may change the stride concurrently (GetNextStride).

function TParallel.TInt32LoopParams.GetCurrentStart(out aStride: Integer): Integer;

begin
  aStride:=GetCurrentStride;
  {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt32LoopParams.GetCurrentStart','Index: %d, Stride: %d',[FLoopProc.Index,aStride]);{$ENDIF USE_THREADLOG}
  Result:=TInterlocked.Add(FLoopProc.Index,aStride)-aStride;
  {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt32LoopParams.GetCurrentStart','Result : %d',[Result]);{$ENDIF USE_THREADLOG}
end;

constructor TParallel.TInt32LoopParams.Create(aLoopProc: TInt32LoopProc);
begin
  inherited Create;
  FLoopProc:=aLoopProc;
  FCancelToken:=aLoopProc.CancelToken;
  if FCancelToken.CanBeCanceled then
    FCancelRegistration:=FCancelToken.Register(@OnCancelToken,Self);
  FNextStrideAt:=TThread.ProcessorCount;
  FBreakAt:=aLoopProc.HighExclusive+1;
  FMaxStride:=FNextStrideAt*16; // 16 loops max
  {$IFDEF FPC_VALGRIND}
  VgBenignRace(@FBreakAt,SizeOf(FBreakAt));
  VgBenignRace(@FLoopProc.Stride,SizeOf(FLoopProc.Stride));
  {$ENDIF}
end;

destructor TParallel.TInt32LoopParams.Destroy;
begin
  {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt32LoopParams.Destroy','Enter (%d)',[PtrInt(Self)]);{$ENDIF USE_THREADLOG}
  inherited Destroy;
  {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt32LoopParams.Destroy','Leave (%d)',[PtrInt(Self)]);{$ENDIF USE_THREADLOG}
end;

procedure TParallel.TInt32LoopParams.Invoke;

var
  I, Start, Limit, UpperLimit, MyStride: Integer;
  State: TLoopState32;

begin
  {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt32LoopParams.Invoke','Enter');{$ENDIF USE_THREADLOG}
  {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt32LoopParams.Invoke','Loop params: '+Self.FLoopProc.ToString);{$ENDIF USE_THREADLOG}
  UpperLimit:=HighExclusive;
  State:=TLoopState32.Create(Self);
  try
    Try
      Start:=GetCurrentStart(MyStride);
      {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt32LoopParams.Invoke','Start: %d, Upper: %d, Stride: %d',[Start,UpperLimit,MyStride]);{$ENDIF USE_THREADLOG}
      while Start<UpperLimit do
        begin
        I:=Start;
        Limit:=Start+MyStride;
        If Limit>UpperLimit then
          Limit:=UpperLimit;
        {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt32LoopParams.Invoke','Inner loop from %d to Limit: %d',[I,Limit]);{$ENDIF USE_THREADLOG}
        while (I<Limit) and not ShouldExitLoop(I) do
          begin
          {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt32LoopParams.Invoke','Executing loop at %d',[I]);{$ENDIF USE_THREADLOG}
          FLoopProc.Execute(I,State);
          Inc(I);
          end;
        if ShouldExitLoop(I) then
          system.Break;
        GetNextStride;
        Start:=GetCurrentStart(MyStride);
        {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt32LoopParams.Invoke','Next loop from %d to %d',[Start,Start+MyStride]);{$ENDIF USE_THREADLOG}
        end;
    except
      on E: EOperationCancelled do
        begin
        Cancel;
        if not FCancelToken.CanBeCanceled then
          raise;
        end;
      else
        begin
        Lock;
        try
          Include(StateFlags,TLoopStateFlag.Exception);
        finally
          Unlock;
        end;
        raise;
        end;
    end;
  finally
    State.Free;
  end;
  {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt32LoopParams.Invoke','leave');{$ENDIF USE_THREADLOG}
end;


{ *********************************************************************
  TInt64LoopParams
  *********************************************************************}

{$IFDEF THREAD64BIT}

procedure TParallel.TInt64LoopParams.UpdateBreakAt(aValue: Int64);
begin
  Lock;
  try
    if aValue<FBreakAt then
      FBreakAt:=aValue;
  finally
    Unlock;
  end;
end;

function TParallel.TInt64LoopParams.GetBreakAt: Variant;
begin
  Lock;
  try
    if TLoopStateFlag.Broken in StateFlags then
      Result:=FBreakAt
    else
      Result:=Null;
  finally
    Unlock;
  end;
end;

function TParallel.TInt64LoopParams.GetCurrentStride: Int64;
begin
  Result:=Stride
end;

function TParallel.TInt64LoopParams.GetNextStride: Int64;

Var
  NewValue : Int64;
  NextOK,MaxReached : Boolean;

begin
  Result:=GetCurrentStride;
  MaxReached:=(Result>=FMaxStride);
  NextOK:=(AtomicIncrement(FStrideCount) mod FNextStrideAt) = 0;
  {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt64LoopParams.GetNextStride','Current: %d, Count: %d, nextat: %d',[Result, FStrideCount, FNextStrideAt]);{$ENDIF USE_THREADLOG}
  {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt64LoopParams.GetNextStride','if %s or not %s then',[BToS(MaxReached), BToS(NextOK)]);{$ENDIF USE_THREADLOG}
  if MaxReached  or Not NextOK then
    begin
    {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt64LoopParams.GetNextStride','Early exit');{$ENDIF USE_THREADLOG}
    exit;
    end;
  NewValue:=Result*2;
  if (NewValue>FMaxStride) then
    NewValue:=FMaxStride;
  // Only get new value if old did not change
  AtomicCmpExchange(FLoopProc.Stride,NewValue,Result);
  {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt64LoopParams.GetNextStride','Result: %d',[Result]);{$ENDIF USE_THREADLOG}
end;

function TParallel.TInt64LoopParams.ShouldExitLoop: Boolean;
var
  Flags: TLoopStateFlagSet;

begin
  Result:=False;
  if FCancelToken.IsCancellationRequested then
    begin
    Cancel;
    Exit(True);
    end;
  Flags:=StateFlags;
  If (Flags=[]) then
    exit;
  If (Flags*ShouldExitFlags)<>[] then
    Exit(True);
end;


function TParallel.TInt64LoopParams.ShouldExitLoop(CurrentIter : Int64): Boolean;

begin
  Result:=ShouldExitLoop();
  if Result then
    exit;
  Result:=(TLoopStateFlag.Broken in StateFlags) and (CurrentIter>FBreakAt);
end;



// Claim the next range of iterations. The stride used for the claim is
// returned in aStride: the caller must execute exactly that many iterations,
// since other workers may change the stride concurrently (GetNextStride).

function TParallel.TInt64LoopParams.GetCurrentStart(out aStride: Int64): Int64;

begin
  aStride:=GetCurrentStride;
  {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt64LoopParams.GetCurrentStart','Index: %d, Stride: %d',[FLoopProc.Index,aStride]);{$ENDIF USE_THREADLOG}
  Result:=TInterlocked.Add(FLoopProc.Index,aStride)-aStride;
  {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt64LoopParams.GetCurrentStart','Result : %d',[Result]);{$ENDIF USE_THREADLOG}
end;

constructor TParallel.TInt64LoopParams.Create(aLoopProc: TInt64LoopProc);
begin
  inherited Create;
  FLoopProc:=aLoopProc;
  FCancelToken:=aLoopProc.CancelToken;
  if FCancelToken.CanBeCanceled then
    FCancelRegistration:=FCancelToken.Register(@OnCancelToken,Self);
  FNextStrideAt:=TThread.ProcessorCount;
  FBreakAt:=aLoopProc.HighExclusive+1;
  FMaxStride:=FNextStrideAt*16; // 16 loops max
  {$IFDEF FPC_VALGRIND}
  VgBenignRace(@FBreakAt,SizeOf(FBreakAt));
  VgBenignRace(@FLoopProc.Stride,SizeOf(FLoopProc.Stride));
  {$ENDIF}
end;

destructor TParallel.TInt64LoopParams.Destroy;
begin
  {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt64LoopParams.Destroy','Enter (%d)',[PtrInt(Self)]);{$ENDIF USE_THREADLOG}
  inherited Destroy;
  {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt64LoopParams.Destroy','Leave (%d)',[PtrInt(Self)]);{$ENDIF USE_THREADLOG}
end;

procedure TParallel.TInt64LoopParams.Invoke;

var
  I, Start, Limit, UpperLimit, MyStride: Int64;
  State: TLoopState64;

begin
  {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt64LoopParams.Invoke','Enter');{$ENDIF USE_THREADLOG}
  {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt64LoopParams.Invoke','Loop params: '+Self.FLoopProc.ToString);{$ENDIF USE_THREADLOG}
  UpperLimit:=HighExclusive;
  State:=TLoopState64.Create(Self);
  try
    Try
      Start:=GetCurrentStart(MyStride);
      {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt64LoopParams.Invoke','Start: %d, Upper: %d, Stride: %d',[Start,UpperLimit,MyStride]);{$ENDIF USE_THREADLOG}
      while Start<UpperLimit do
        begin
        I:=Start;
        Limit:=Start+MyStride;
        If Limit>UpperLimit then
          Limit:=UpperLimit;
        {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt64LoopParams.Invoke','Inner loop from %d to Limit: %d',[I,Limit]);{$ENDIF USE_THREADLOG}
        while (I<Limit) and not ShouldExitLoop(I) do
          begin
          {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt64LoopParams.Invoke','Executing loop at %d',[I]);{$ENDIF USE_THREADLOG}
          FLoopProc.Execute(I,State);
          Inc(I);
          end;
        if ShouldExitLoop(I) then
          system.Break;
        GetNextStride;
        Start:=GetCurrentStart(MyStride);
        {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt64LoopParams.Invoke','Next loop from %d to %d',[Start,Start+MyStride]);{$ENDIF USE_THREADLOG}
        end;
    except
      on E: EOperationCancelled do
        begin
        Cancel;
        if not FCancelToken.CanBeCanceled then
          raise;
        end;
      else
        begin
        Lock;
        try
          Include(StateFlags,TLoopStateFlag.Exception);
        finally
          Unlock;
        end;
        raise;
        end;
    end;
  finally
    State.Free;
  end;
  {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt64LoopParams.Invoke','leave');{$ENDIF USE_THREADLOG}
end;

{$ENDIF}

{ *********************************************************************
  TParallel.TInt32LoopProc
  *********************************************************************}

procedure TParallel.TInt32LoopProc.Execute(Iteration: Integer; aState: TLoopState32);

begin
  {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt32LoopProc.Execute','enter (%d)',[Iteration]);{$ENDIF USE_THREADLOG}
  //       This would make it so that only a single virtual call is made to process the iterations.
  if Assigned(Event) then
    Event(Sender, Iteration)
  else if Assigned(Proc) then
    Proc(Iteration)
  else if Assigned(ProcWithState) then
  begin
    aState.CurrentIteration:=Iteration;
    ProcWithState(Iteration,aState);
  end
  else if Assigned(StateEvent) then
  begin
    aState.CurrentIteration:=Iteration;
    StateEvent(Sender,Iteration, aState);
  end;
  {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt32LoopProc.Execute','leave (%d) ',[Iteration]);{$ENDIF USE_THREADLOG}
end;

function TParallel.TInt32LoopProc.NumTasks: Integer;

var
  aMax : Integer;

begin
  Result:=HighExclusive-LowInclusive;
  aMax:=TThread.ProcessorCount*2;
  if Result>aMax then
    Result:=aMax;
end;

class function TParallel.TInt32LoopProc.create(aSender: TObject; aLowInclusive, aHighInclusive: Integer;
  aIteratorEvent: TIteratorEvent32): TParallel.TInt32LoopProc;
begin
  Result:=Default(TInt32LoopProc);
  Result.LowInclusive:=aLowInclusive;
  Result.HighExclusive:=aHighInclusive+1;
  Result.Sender:=aSender;
  Result.Event:=aIteratorEvent;
  Result.Stride:=-1;
end;

class function TParallel.TInt32LoopProc.create(aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TInt32Proc
  ): TParallel.TInt32LoopProc;
begin
  Result:=Default(TInt32LoopProc);
  Result.LowInclusive:=aLowInclusive;
  Result.HighExclusive:=aHighInclusive+1;
  Result.Proc:=aIteratorEvent;
  Result.Stride:=-1;
end;

class function TParallel.TInt32LoopProc.create(aSender: TObject; aLowInclusive, aHighInclusive: Integer;
  aIteratorEvent: TIteratorStateEvent32): TParallel.TInt32LoopProc;
begin
  Result:=Default(TInt32LoopProc);
  Result.LowInclusive:=aLowInclusive;
  Result.HighExclusive:=aHighInclusive+1;
  Result.Sender:=aSender;
  Result.StateEvent:=aIteratorEvent;
  Result.Stride:=-1;
end;

class function TParallel.TInt32LoopProc.create(aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TInt32LoopStateProc): TParallel.TInt32LoopProc;
begin
  Result:=Default(TInt32LoopProc);
  Result.LowInclusive:=aLowInclusive;
  Result.HighExclusive:=aHighInclusive+1;
  Result.ProcWithState:=aIteratorEvent;
  Result.Stride:=-1;
end;

function TParallel.TInt32LoopProc.ToString: String;
begin
  Result:=Format('loop from %d to %d with step %d currently at %d',[LowInclusive,HighExclusive,Stride,Index]);
end;

{ *********************************************************************
  TParallel.TInt64LoopProc
  *********************************************************************}

{$IFDEF THREAD64BIT}

procedure TParallel.TInt64LoopProc.Execute(Iteration: Int64; aState: TLoopState64);

begin
  {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt64LoopProc.Execute','enter (%d)',[Iteration]);{$ENDIF USE_THREADLOG}
  //       This would make it so that only a single virtual call is made to process the iterations.
  if Assigned(Event) then
    Event(Sender, Iteration)
  else if Assigned(Proc) then
    Proc(Iteration)
  else if Assigned(ProcWithState) then
  begin
    aState.CurrentIteration:=Iteration;
    ProcWithState(Iteration,aState);
  end
  else if Assigned(StateEvent) then
  begin
    aState.CurrentIteration:=Iteration;
    StateEvent(Sender,Iteration, aState);
  end;
  {$IFDEF USE_THREADLOG}ThreadLog('TParallel.TInt64LoopProc.Execute','leave (%d) ',[Iteration]);{$ENDIF USE_THREADLOG}
end;

function TParallel.TInt64LoopProc.NumTasks: Integer;

var
  aMax : Integer;

begin
  Result:=HighExclusive-LowInclusive;
  aMax:=TThread.ProcessorCount*2;
  if Result>aMax then
    Result:=aMax;
end;

class function TParallel.TInt64LoopProc.create(aSender: TObject; aLowInclusive, aHighInclusive: Int64;
  aIteratorEvent: TIteratorEvent64): TParallel.TInt64LoopProc;
begin
  Result:=Default(TInt64LoopProc);
  Result.LowInclusive:=aLowInclusive;
  Result.HighExclusive:=aHighInclusive+1;
  Result.Sender:=aSender;
  Result.Event:=aIteratorEvent;
  Result.Stride:=-1;
end;

class function TParallel.TInt64LoopProc.create(aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TInt64Proc
  ): TParallel.TInt64LoopProc;
begin
  Result:=Default(TInt64LoopProc);
  Result.LowInclusive:=aLowInclusive;
  Result.HighExclusive:=aHighInclusive+1;
  Result.Proc:=aIteratorEvent;
  Result.Stride:=-1;
end;

class function TParallel.TInt64LoopProc.create(aSender: TObject; aLowInclusive, aHighInclusive: Int64;
  aIteratorEvent: TIteratorStateEvent64): TParallel.TInt64LoopProc;
begin
  Result:=Default(TInt64LoopProc);
  Result.LowInclusive:=aLowInclusive;
  Result.HighExclusive:=aHighInclusive+1;
  Result.Sender:=aSender;
  Result.StateEvent:=aIteratorEvent;
  Result.Stride:=-1;
end;

class function TParallel.TInt64LoopProc.create(aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TInt64LoopStateProc): TParallel.TInt64LoopProc;
begin
  Result:=Default(TInt64LoopProc);
  Result.LowInclusive:=aLowInclusive;
  Result.HighExclusive:=aHighInclusive+1;
  Result.ProcWithState:=aIteratorEvent;
  Result.Stride:=-1;
end;

function TParallel.TInt64LoopProc.ToString: String;
begin
  Result:=Format('loop from %d to %d with step %d currently at %d',[LowInclusive,HighExclusive,Stride,Index]);
end;

{$ENDIF}

{ *********************************************************************
  TParallel.TLoopParams
  *********************************************************************}

function TParallel.TLoopParams.StartLoop: ITask;
begin
  Result:=FRootTask.Start;
end;

procedure TParallel.TLoopParams.CreateRootTask(aParams: TTask.TTaskParams; aCount: Integer);
begin
  FRootTask:=TReplicableTask.Create(aParams,aCount);
end;

procedure TParallel.TLoopParams.ClearRootTask;
begin
  FCancelRegistration.Unregister;
  FRootTask:=Nil;
end;

procedure TParallel.TLoopParams.OnCancelToken(Sender: TObject);
begin
  if Sender=nil then;
  Cancel;
end;

procedure TParallel.TLoopParams.Stop;
begin
  Lock;
  Try
    Include(StateFlags,TLoopStateFlag.Stopped);
  finally
    UnLock;
  end;
end;

procedure TParallel.TLoopParams.Cancel;
begin
  Lock;
  Try
    Include(StateFlags,TLoopStateFlag.Cancelled);
  finally
    UnLock;
  end;
end;

function TParallel.TLoopParams.Break: Boolean;
begin
  Result:=False;
  lock;
  try
    if TLoopStateFlag.Stopped in StateFlags then
      raise EInvalidOperation.Create(SErrBreakAfterStop);
    if (StateFlags*[TLoopStateFlag.Exception, TLoopStateFlag.Cancelled])<>[] then
      Exit(False);
    Include(StateFlags,TLoopStateFlag.Broken);
    Result:=True;
  finally
    UnLock
  end;
end;

function TParallel.TLoopParams.Stopped: Boolean;
begin
  Result:=TLoopStateFlag.Stopped in StateFlags;
end;

function TParallel.TLoopParams.Cancelled: Boolean;
begin
  Result:=TLoopStateFlag.Cancelled in StateFlags;
end;

function TParallel.TLoopParams.Faulted: Boolean;
begin
  Result:=TLoopStateFlag.Exception in StateFlags;
end;

constructor TParallel.TLoopParams.Create;
begin
  FStateLock:=TSpinLock.Create(False);
  FCancelRegistration:=Default(TCancellationTokenRegistration);
  {$IFDEF FPC_VALGRIND}
  VgBenignRace(@StateFlags,SizeOf(StateFlags));
  {$ENDIF}
end;

destructor TParallel.TLoopParams.Destroy;
begin
  FCancelRegistration.Unregister;
  inherited Destroy;
end;

procedure TParallel.TLoopParams.Lock;
begin
  FStateLock.Enter;
end;

procedure TParallel.TLoopParams.UnLock;
begin
  FStateLock.Exit;
end;

procedure TParallel.TLoopParams.HandleException(O: TObject);

var
  E : Exception absolute O;
  ErrorTasks : TITaskArray;
  ExcList : TExceptionList;
  aTask: ITask;


begin
  if not (O is Exception) then
    begin
    O.Free;
    exit;
    end;
  Lock;
  try
    ErrorTasks:=(FRootTask as TTask).FTasksWithExceptions;
    if not assigned(ErrorTasks) then
      raise E;
    ExcList:=TExceptionList.Create(E,Length(ErrorTasks)+1);
    if E is EAggregateException then
      E.Free;
    for aTask in ErrorTasks do
      ExcList.AddFromTask(aTask as TTask);
  finally
    Unlock;
  end;
  raise EAggregateException.Create(ExcList.Truncate);
end;

procedure TParallel.TLoopParams.HandleException;
begin
  HandleException(TObject(AcquireExceptionObject));
end;

{ *********************************************************************
  TParallel.TLoopResult
  *********************************************************************}

class function TParallel.TLoopResult.Create: TLoopResult;
begin
  Result:=Default(TLoopResult);
  Result.FCompleted:=True;
  Result.FLowestBreakIteration:=NULL;
end;

{ *********************************************************************
  Auxiliary
  *********************************************************************}

Procedure InitThreading;

begin
  // Caches needs to exist before they can be used to register objects
  // We don't know the order of class constructors, so we do it manually here
  TThreadPool.Caches:=TObjectCaches.Create([doOwnsValues]);
  TThreadPool.Caches.AddObjectCache(TTask);
  TThreadPool.Caches.AddObjectCache(TReplicableTask);
  TThreadPool.Caches.AddObjectCache(TReplicatedTask);
  TThreadPool.FDefaultPool:=TThreadPool.Create;
end;

Procedure DoneThreading;
begin
  FreeAndNil(TThreadPool.FDefaultPool);
  FreeAndNil(TThreadPool.Caches);
end;

Initialization
  InitThreading;
Finalization
  DoneThreading;
end.
