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

{ $DEFINE DEBUGTHREADPOOL}

{$IFDEF CPU64}
{$DEFINE THREAD64BIT}
{$ENDIF}

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.Timespan, System.SyncObjs, System.Contnrs, Fcl.ThreadPool;
{$ELSE}
  Contnrs, SysUtils, Classes, System.Timespan, Generics.Collections, SyncObjs, fpthreadpool;
{$ENDIF}

type
  TLightweightEvent = TEvent;
  Generic TFunctionEvent<T> = function (Sender: TObject): T of object;
  Generic TProc<T> = reference to procedure (arg : T);
  Generic TProc2<T1,T2> = reference to procedure (arg1 : T1;arg2 : T2);
  Generic TFunc<T> = Reference to function : T;
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
    FLock: TCriticalSection;
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
    FLock : TCriticalSection;
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
    FLock : TCriticalSection;
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
      SuspendDelay = 5500;      // in milliseconds. Minimum time between 2 thread suspending themselved
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
    FQueueLock : TCriticalSection;
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
      TMonitorResult = (mrTerminate,mrContinue,mrIdle);
    var
      FWorkQueue : {$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}Contnrs.TQueue;
      FQueues : TWorkStealingQueueThreadPoolWorkItemArray;
      FThreads: TBaseWorkerThreadList;

      FMonitorStatus : Integer;
      FShutDown : Boolean;
    procedure NewThread(aThread : TBaseWorkerThread);
    procedure RemoveThread(aThread : TBaseWorkerThread);
  protected
    class threadvar QueueThread : TQueueWorkerThread;
    class var Caches : TObjectCaches;
    // Queue management.
    procedure RegisterWorkerThread(aThread: TQueueWorkerThread);
    procedure UnRegisterWorkerThread(aThread: TQueueWorkerThread);
    // Adding/Removing work
    function DoRemoveWorkItem(const WorkerData: IThreadPoolWorkItem): Boolean;
    procedure DoQueueWorkItem(const WorkerData: IThreadPoolWorkItem; PreferThread : TQueueWorkerThread);
    procedure AssignWorkToLocalQueue(const WorkerData: IThreadPoolWorkItem; aThread: TQueueWorkerThread);
    procedure AssignWorkToGlobalQueue(const WorkerData: IThreadPoolWorkItem);
    // Getting work.
    function CheckShouldTerminate(aThread: TQueueWorkerThread): Boolean;
    function GetWorkItemForThread(aThread: TQueueWorkerThread; out Itm: IThreadPoolWorkItem): Boolean;
    function GetWorkItemFromQueues(aSkip: TWorkStealingQueueThreadPoolWorkItem; out Itm: IThreadPoolWorkItem): Boolean;
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

  ITask = interface (TThreadPool.IThreadPoolWorkItem) ['{AD752DA0-556C-41B5-96F2-0B0CA932E364}']
    function Wait(aTimeout: Cardinal = INFINITE): Boolean; overload;
    function Wait(const aTimeout: TTimeSpan): Boolean; overload;
    procedure Cancel;
    procedure CheckCanceled;
    function Start: ITask;
    function GetStatus: TTaskStatus;
    function GetId: Integer;
    property Id: Integer read GetId;
    property Status: TTaskStatus read GetStatus;
  end;
  TITaskArray = array of ITask;
  TITaskProc = specialize TProc<ITask>;
  TITaskProcArray = Array of TITaskProc;

  generic IFuture<T> = interface(ITask)
    function StartFuture: specialize IFuture<T>; overload;
    function GetValue: T;
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
        Procedure ResolvePool;
      end;

  Private
    // Instance stuff
    FStateFlags : TOptionStateFlags;
    FStatus : TTaskStatus;
    FParams : TTaskParams;
    FTaskID : Integer;
    FSubTasks : Integer;
    FStateLock : TCriticalSection;
    FTasksWithExceptions : Array of TTask;
    FCompletedEvents : TITaskProcArray;
    FCompletedEventCount : Integer;
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
    { IInternalTask }
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
    generic class function Future<T>(aSender: TObject; aEvent: specialize TFunctionEvent<T>): Specialize IFuture<T>; overload; static; inline;
    generic class function Future<T>(aSender: TObject; aEvent: specialize TFunctionEvent<T>; aPool: TThreadPool): Specialize IFuture<T>; overload; static; inline;
    generic class function Future<T>(const aFunc: specialize TFunc<T>): Specialize IFuture<T>; overload; static; inline;
    generic class function Future<T>(const aFunc: specialize TFunc<T>; aPool: TThreadPool): Specialize IFuture<T>; overload; static; inline;
    class function Run(aSender: TObject; aEvent: TNotifyEvent): ITask; overload; static; inline;
    class function Run(aSender: TObject; aEvent: TNotifyEvent; aPool: TThreadPool): ITask; overload; static; inline;
    class function Run(const aFunc: TProcRef): ITask; overload; static; inline;
    class function Run(const aFunc: TProcRef; aPool: TThreadPool): ITask; overload; static; inline;
    class function WaitForAll(const aTasks: array of ITask): Boolean; overload; static;
    class function WaitForAll(const aTasks: array of ITask; aTimeout: Cardinal): Boolean; overload; static;
    class function WaitForAll(const aTasks: array of ITask; const aTimeout: TTimeSpan): Boolean; overload; static;
    class function WaitForAny(const aTasks: array of ITask): Integer; overload; static;
    class function WaitForAny(const aTasks: array of ITask; aTimeout: Cardinal): Integer; overload; static;
    class function WaitForAny(const aTasks: array of ITask; const aTimeout: TTimeSpan): Integer; overload; static;
  end;

  { TFuture }

  generic TFuture<T> = class sealed(TTask, specialize IFuture<T>)
  Type
    TFunctionEventT = specialize TFunctionEvent<T>;
    TFunctionRefT = specialize TFunc<T>;
  Var
    FResult : T;
    FFuncRef : TFunctionRefT;
    FFuncEvent : TFunctionEventT;
    procedure RunFunc(Sender: TObject);
  Protected
    function StartFuture: specialize IFuture<T>;
    function GetValue: T;
  Public
    constructor Create(aSender: TObject; aEvent: TFunctionEventT; const aFunc: TFunctionRefT; aPool: TThreadPool); overload;
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
      State :  TLoopState32;
      LowInclusive,
      HighExclusive,
      Index,
      Stride: Integer;
      Procedure Execute(Iteration : Integer);
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
      State :  TLoopState64;
      LowInclusive,
      HighExclusive,
      Index,
      Stride: Int64;
      Procedure Execute(Iteration : Int64);
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
      FStateLock : TCriticalSection;
      FStrideCount : Integer;
      FNextStrideAt : Integer;
      FRootTask: ITask;
    public
      Constructor Create;
      Destructor Destroy; override;
      procedure Lock;
      procedure UnLock;
      Procedure HandleException(O : TObject);
      procedure HandleException; overload;
      Function GetBreakAt : Variant; virtual; abstract;
      Procedure Stop;
      Function StartLoop : ITask;
      procedure CreateRootTask(aParams : TTask.TTaskParams; aCount : Integer);
      procedure ClearRootTask;
      // We use the fact that in FPC a reference to procedure is an Interface.
      // Invoke is the method of the interface that is called...
      Procedure Invoke; virtual; abstract;
      function Break : Boolean;
      Function Stopped : Boolean;
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
      Function GetCurrentStart : Integer;
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
      Function GetCurrentStart : Int64;
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
    class function &For(aSender: TObject; aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TIteratorStateEvent): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TIteratorStateEvent; aPool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aStride, aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TIteratorEvent): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aStride, aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TIteratorEvent; aPool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aStride, aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TIteratorStateEvent): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aStride, aLowInclusive, aHighInclusive: Integer; aIteratorEvent: TIteratorStateEvent; aPool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(aLowInclusive, aHighInclusive: Integer; const aIteratorEvent: TProcInteger): TLoopResult; overload; static; inline;
    class function &For(aLowInclusive, aHighInclusive: Integer; const aIteratorEvent: TProcInteger; aPool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(aLowInclusive, aHighInclusive: Integer; const aIteratorEvent: TProcIntegerLoopState): TLoopResult; overload; static; inline;
    class function &For(aLowInclusive, aHighInclusive: Integer; const aIteratorEvent: TProcIntegerLoopState; aPool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(aStride, aLowInclusive, aHighInclusive: Integer; const aIteratorEvent: TProcInteger): TLoopResult; overload; static; inline;
    class function &For(aStride, aLowInclusive, aHighInclusive: Integer; const aIteratorEvent: TProcInteger; aPool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(aStride, aLowInclusive, aHighInclusive: Integer; const aIteratorEvent: TProcIntegerLoopState): TLoopResult; overload; static; inline;
    class function &For(aStride, aLowInclusive, aHighInclusive: Integer; const aIteratorEvent: TProcIntegerLoopState; aPool: TThreadPool): TLoopResult; overload; static; inline;
    {$IFDEF THREAD64BIT}
    class function &For(aSender: TObject; aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TIteratorEvent64): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TIteratorEvent64; aPool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TIteratorStateEvent64): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TIteratorStateEvent64; aPool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aStride, aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TIteratorEvent64): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aStride, aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TIteratorEvent64; aPool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aStride, aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TIteratorStateEvent64): TLoopResult; overload; static; inline;
    class function &For(aSender: TObject; aStride, aLowInclusive, aHighInclusive: Int64; aIteratorEvent: TIteratorStateEvent64; aPool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(aLowInclusive, aHighInclusive: Int64; const aIteratorEvent: TProcInt64): TLoopResult; overload; static; inline;
    class function &For(aLowInclusive, aHighInclusive: Int64; const aIteratorEvent: TProcInt64; aPool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(aLowInclusive, aHighInclusive: Int64; const aIteratorEvent: TProcInt64LoopState): TLoopResult; overload; static; inline;
    class function &For(aLowInclusive, aHighInclusive: Int64; const aIteratorEvent: TProcInt64LoopState; aPool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(aStride, aLowInclusive, aHighInclusive: Int64; const aIteratorEvent: TProcInt64): TLoopResult; overload; static; inline;
    class function &For(aStride, aLowInclusive, aHighInclusive: Int64; const aIteratorEvent: TProcInt64; aPool: TThreadPool): TLoopResult; overload; static; inline;
    class function &For(aStride, aLowInclusive, aHighInclusive: Int64; const aIteratorEvent: TProcInt64LoopState): TLoopResult; overload; static; inline;
    class function &For(aStride, aLowInclusive, aHighInclusive: Int64; const aIteratorEvent: TProcInt64LoopState; aPool: TThreadPool): TLoopResult; overload; static; inline;
    {$ENDIF}
    class function Join(aSender: TObject; aEvents: array of TNotifyEvent): ITask; overload; static;
    class function Join(aSender: TObject; aEvents: array of TNotifyEvent; aPool: TThreadPool): ITask; overload; static;
    class function Join(aSender: TObject; aEvent1, aEvent2: TNotifyEvent): ITask; overload; static; inline;
    class function Join(aSender: TObject; aEvent1, aEvent2: TNotifyEvent; aPool: TThreadPool): ITask; overload; static;
    class function Join(const aProcs: array of TProcRef): ITask; overload; static;
    class function Join(const aProcs: array of TProcRef; aPool: TThreadPool): ITask; overload; static;
    class function Join(const aProc1, aProc2: TProcRef): ITask; overload; static; inline;
    class function Join(const aProc1, aProc2: TProcRef; aPool: TThreadPool): ITask; overload; static;
  end;

function GetThreadPoolInteractive(APool: TThreadPool): Boolean;
procedure SetThreadPoolInteractive(APool: TThreadPool; AValue: Boolean);

{
  These must be exposed, otherwise they cannot be used inside generic methods :/

  At optimization level 1, the threadlog is not called at all if the routine is empty.
  So if DEBUGTHREADPOOL is not defined, we must ensure the methods are empty.
  without optimization, the methods are called but will not do anything.
}
procedure ThreadLog(const Method,Msg: string); overload;
procedure ThreadLog(const Method,Fmt: string; Args: array of const); overload;


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
  ThreadLog(Method,SafeFormat(Fmt,Args));
{$ENDIF}
end;


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
    constructor Create(const AProcs: array of TProcRef; APool: TThreadPool);
  private
    procedure JoinTasks;
  end;

  { TEventJoinTask }

  TEventJoinTask = class(TReplicableTask)
    FProc : TParallel.TInt32LoopProc;
    FEventList : array of TNotifyEvent;
    constructor Create(Sender: TObject; const AEvents : array of TNotifyEvent; APool: TThreadPool);
  private
    procedure JoinTasks;
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
  Result.List[0]:=Initial;
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
  Result:=TExceptionEnumerator.Create(Self.FList.List)
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
  Result:=Assigned(FList) and (FCurrent<Length(FList));
  if Result then
    Inc(FCurrent);
end;

{ *********************************************************************
  TSparseArray
  *********************************************************************}

constructor TSparseArray.Create(aInitialSize: Integer);
begin
  FLock:=TCriticalSection.Create;
  if aInitialSize < 1 then
    aInitialSize:=1;
  SetLength(FArray,aInitialSize);
end;

destructor TSparseArray.Destroy;
begin
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TSparseArray.Lock;
begin
  FLock.Enter;
end;

procedure TSparseArray.Unlock;
begin
  FLock.Leave;
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
  Result:=FItems.Count;
end;

function TWorkStealingQueue.GetIsEmpty: Boolean;
begin
  Result:=FItems.Count=0;
end;

procedure TWorkStealingQueue.Lock;
begin
  ThreadLog('TWorkStealingQueue.Lock','Enter %d',[PtrInt(Self)]);
  try
    FLock.Enter;
  except
    on E : Exception do
      ThreadLog('TWorkStealingQueue.Lock','%d Exception: %s %s',[PtrInt(Self),E.ClassName,E.Message]);
  end;
  ThreadLog('TWorkStealingQueue.Lock','Leave %d',[PtrInt(Self)]);
end;

procedure TWorkStealingQueue.UnLock;
begin
  ThreadLog('TWorkStealingQueue.UnLock','Enter %d',[PtrInt(Self)]);
  FLock.Leave;
  ThreadLog('TWorkStealingQueue.UnLock','Leave %d',[PtrInt(Self)]);
end;

constructor TWorkStealingQueue.Create;
begin
  ThreadLog('TWorkStealingQueue.Create',IntToStr(PtrInt(Self)));
  FItems:=TItemList.Create;
  FLock:=TCriticalSection.Create;
  FEvent:=TEvent.Create(False);
end;

destructor TWorkStealingQueue.Destroy;
begin
  ThreadLog('TWorkStealingQueue.Destroy',IntToStr(PtrInt(Self)));
  FreeAndNil(FItems);
  FreeAndNil(FEvent);
  FreeAndNil(Flock);
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
  If Result then
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
  FLock:=TCriticalSection.Create;
end;

destructor TObjectCache.Destroy;
begin
  Clear;
  FreeAndNil(FStack);
  FreeAndNil(FLock);
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
    FLock.Leave;
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
    FLock.Leave;
  end;
end;

function TObjectCache.Remove: Pointer;

begin
  FLock.Enter;
  try
    Result:=FStack.Pop;
  finally
    FLock.Leave;
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
  ThreadLog('TThreadPool.WorkQueued','enter');
  AtomicIncrement(FRequestCount);
  ThreadLog('TThreadPool.WorkQueued','Queueing work (Requests: %d)',[FRequestCount]);
  DoEventSignal:=FIdleThreads>=FRequestCount;
  ThreadLog('TThreadPool.WorkQueued','DoEventSignal %s (%d>%d)',[BToS(DoEventSignal),FIdleThreads,FRequestCount]);
  if DoEventSignal then
    FQueueEvent.SetEvent
  else
    GrowPool;
  ThreadLog('TThreadPool.WorkQueued','leave');
end;

procedure TThreadPool.GrowPool;

  procedure DoAdd;

  begin
    ThreadLog('TThreadPool.GrowPool.DoAdd','Enter');
    LockQueue;
    try
      AddThreadToPool;
    finally
      UnlockQueue;
      ThreadLog('TThreadPool.GrowPool.DoAdd','Leave');
    end;
  end;

Var
  DoGrow,NeedMinimum,IdleDeficit,HaveRoom : Boolean;

begin
  ThreadLog('TThreadPool.GrowPool','Enter');
  NeedMinimum:=(FThreadCount<FMinThreads);
  IdleDeficit:=(FIdleThreads<FRequestCount);
  HaveRoom:=(FThreadCount<FMaxThreads);
  DoGrow:=NeedMinimum or (IdleDeficit and HaveRoom);
  ThreadLog('TThreadPool.GrowPool','DoGrow: %s, NeedMinimum: %s, IdleDeficit: %s, HaveRoom: %s',[BToS(DoGrow),BToS(NeedMinimum),BToS(IdleDeficit),BToS(HaveRoom)]);
  if Not DoGrow then
    begin
    ThreadLog('TThreadPool.GrowPool','Leave (not DoGrow)');
    exit;
    end;
  if FRetiring>0 then
     begin
     ThreadLog('TThreadPool.GrowPool','Waking retired threads: %d',[FRetiring]);
     FRetireEvent.SetEvent;
     end
  else
    begin
    DoAdd;
    while (FThreadCount<FMinThreads) do
      begin
      ThreadLog('TThreadPool.GrowPool','Adding thread to pool: %d<%d',[FThreadCount,FMinThreads]);
      DoAdd;
      end;
    end;
  ThreadLog('TThreadPool.GrowPool','Leave');
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
  ThreadLog('TThreadPool.RemoveThread','Thread count now %d',[FThreadCount]);
  if assigned(FOnThreadTerminate) then
    FOnThreadTerminate(aThread);
end;

procedure TThreadPool.AssignWorkToLocalQueue(const WorkerData: IThreadPoolWorkItem; aThread: TQueueWorkerThread);

begin
  aThread.WorkQueue.LocalPush(WorkerData);
  WorkQueued;
end;

procedure TThreadPool.AssignWorkToGlobalQueue(const WorkerData: IThreadPoolWorkItem);

begin
  ThreadLog('TThreadPool.AssignWorkToGlobalQueue','locking queue');
  LockQueue;
  try
    FWorkQueue.Push(WorkerData);
  finally
    ThreadLog('TThreadPool.AssignWorkToGlobalQueue','unlocking queue');
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
      TThreadPoolMonitor.Create(Self);
    except
      AtomicExchange(FMonitorStatus,MonitorNone);
     raise;
    end;
end;

procedure TThreadPool.WaitForMonitorThread;


begin
  While (FMonitorStatus<>MonitorNone) do
    TThread.Sleep(MonitorThreadDelay div 4);
end;

procedure TThreadPool.DoQueueWorkItem(const WorkerData: IThreadPoolWorkItem; PreferThread : TQueueWorkerThread);
begin
  ThreadLog('TThreadPool.DoQueueWorkItem','enter');
  if assigned(PreferThread) then
    AssignWorkToLocalQueue(WorkerData,PreferThread)
  else
    AssignWorkToGlobalQueue(WorkerData);
  if FMonitorStatus = MonitorNone then
    CreateMonitorThread;
  ThreadLog('TThreadPool.DoQueueWorkItem','leave');
end;


constructor TThreadPool.Create;
var
  PC: Integer;

begin
  FRetireEvent:=TLightweightEvent.Create;
  FQueueEvent:=TEvent.Create;
  FQueueLock:=TCriticalSection.Create;
  FWorkQueue:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}Contnrs.TQueue.Create;
  PC:=TThread.ProcessorCount;
  FQueues:=TWorkStealingQueueThreadPoolWorkItemArray.Create(PC);
  FMinThreads:=PC div 4;
  if FMinThreads<2 then
    FMinThreads:=2;
  FMaxThreads:=PC*MaxThreadsPerCPU;
  FThreads:=TBaseWorkerThreadList.Create;
  FThreads.Duplicates:=dupIgnore;
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
          ThreadLog('TThreadPool.WaitForThreads','Terminated thread');
          end;
    finally
      FThreads.UnlockList;
    end;
    if not empty then
      // give threads time to deregister
      Sleep(MonitorThreadDelay div 4);
  Until Empty;
end;


destructor TThreadPool.Destroy;
begin
  FShutdown:=True;
  FQueueEvent.SetEvent;
  WaitForThreads;
  WaitForMonitorThread;
  FreeAndNil(FWorkQueue);
  FreeAndNil(FQueues);
  FreeAndNil(FRetireEvent);
  FreeAndNil(FQueueEvent);
  FreeAndNil(FQueueLock);
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
  ThreadLog('TThreadPool.SignalExecuting','Enter (Requests left: %d, Idle: %d)',[FRequestCount,FIdleThreads]);
  if aThread.Idle then
    AtomicDecrement(FIdleThreads);
  aThread.Idle:=False;
  AtomicDecrement(FRequestCount);
  ThreadLog('TThreadPool.SignalExecuting','Leave (Requests left: %d, Idle: %d)',[FRequestCount,FIdleThreads]);
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
  ThreadLog('TThreadPool.LockQueue','Enter');
  FQueueLock.Enter;
  ThreadLog('TThreadPool.LockQueue','Leave');
end;

procedure TThreadPool.UnLockQueue;
begin
  ThreadLog('TThreadPool.UnLockQueue','Enter');
  FQueueLock.Leave;
  ThreadLog('TThreadPool.UnLockQueue','Leave');
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
    Threadlog('TThreadPool.UnRegisterWorkerThread','Idle count: %d',[FIdleThreads]);
    end;
  QueueThread:=Nil;
end;

function TThreadPool.DoRemoveWorkItem(const WorkerData: IThreadPoolWorkItem): Boolean;
begin
  Result:=Assigned(QueueThread) and Assigned(QueueThread.WorkQueue);
  if Not Result then
    exit;
  Result:=QueueThread.WorkQueue.LocalFindAndRemove(WorkerData);
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
    ThreadLog('TThreadPool.GetWorkItemForThread','Shutting down, no work -> quit');
    Exit(False);
    end;
  ThreadLog('TThreadPool.GetWorkItemForThread','locking queue');
  LockQueue;
  try
    if (FWorkQueue.Count > 0) then
      begin
      // FWorkQueue is thread safe.
      ThreadLog('TThreadPool.GetWorkItemForThread','Have global work');
      Itm:=IThreadPoolWorkItem(FWorkQueue.Pop);
      if assigned(Itm) then
        begin
        ThreadLog('TThreadPool.GetWorkItemForThread','Global work, -> no quit');
        Exit(True); // We got work, do not stop thread
        end;
      end;
  finally
    ThreadLog('TThreadPool.GetWorkItemForThread','unlocking queue');
    UnLockQueue;
  end;
  // No local work, check global
  if not aThread.Idle then
    begin
    ThreadLog('TThreadPool.GetWorkItemForThread','marking thread %d as idle',[PtrInt(aThread.ThreadID)]);
    AtomicIncrement(FIdleThreads);
    aThread.Idle:=True;
    end;
  ThreadLog('TThreadPool.GetWorkItemForThread','Waiting for queue event (%d ms.)',[aThread.CheckWaitTime]);
  CheckThreadQueues:=(FQueueEvent.WaitFor(aThread.CheckWaitTime)<>wrTimeout);
  ThreadLog('TThreadPool.GetWorkItemForThread','Work queued triggered: %s',[BToS(CheckThreadQueues)]);
  if FShutdown then
    begin
    ThreadLog('TThreadPool.GetWorkItemForThread','Shutdown -> quit');
    Exit(False); // Stop thread
    end;
  if CheckThreadQueues then
    begin
    ThreadLog('TThreadPool.GetWorkItemForThread','Checking other queues');
    if GetWorkItemFromQueues(aThread.WorkQueue,Itm) then
      begin
      ThreadLog('TThreadPool.GetWorkItemForThread','Checked other queues, got work -> no quit');
      Exit(True); // We got work, do not stop thread
      end;
    ThreadLog('TThreadPool.GetWorkItemForThread','No work in other queues');
    end;
  if FShutdown then
    begin
    ThreadLog('TThreadPool.GetWorkItemForThread','Shutdown -> quit');
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
        ThreadLog('TThreadPool.GetWorkItemForThread','Enough threads to handle workload -> quit');
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
      ThreadLog('TThreadPool.GetWorkItemForThread','One thread, waiting quite long -> quit');
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
  FMonitorStatus:=MonitorNone;
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
    ThreadLog('TThreadPool.GrowIfStarved','No work (%d>0) and (%d<%d) is False. Not creating new threads',[FRequestCount,FThreadCount,FMaxThreads]);
    Exit;
    end;
  PrevRequestCount:=FPreviousRequestCount;
  FPreviousRequestCount:=FRequestCount;
  ThreadLog('TThreadPool.GrowIfStarved','(FRequestCount>=PrevRequestCount) and IsThrottledDelay(FThreadCreationAt,FThreadCount):');
  ThrottleOK:=IsThrottledDelay(FThreadCreationAt,FThreadCount);
  IncreasingRequests:=(FRequestCount>=PrevRequestCount);
  B:=IncreasingRequests and ThrottleOK;
  ThreadLog('TThreadPool.GrowIfStarved','IncreasingRequests (%d>=%d) [%s] and ThrottleOK (%d,%d) [%s] : %s',[FRequestCount,PrevRequestCount,BToS(IncreasingRequests),FThreadCreationAt, FThreadCount, BToS(ThrottleOK),BToS(B)]);
  if not B then
    Exit;
  if B then
    begin
    CreateNewThread:=False;
    ThreadLog('TThreadPool.GrowIfStarved','locking queue');
    LockQueue;
    try
      IncreasingRequests:=(FRequestCount>=PrevRequestCount);
      AllowMoreThreads:=(FThreadCount<FMaxThreads);
      ThreadLog('TThreadPool.GrowIfStarved','IncreasingRequests (%d>=%d) : %s ',[FRequestCount,PrevRequestCount,BToS(IncreasingRequests)]);
      ThreadLog('TThreadPool.GrowIfStarved','AllowMoreThreads (%d<%d) : %s',[FThreadCount,FMaxThreads,BToS(AllowMoreThreads)]);
      ThreadLog('TThreadPool.GrowIfStarved','(FIdleThreads=FRetiring) : (%d=%d) %s',[FIdleThreads,FRetiring,BToS(FIdleThreads=FRetiring)]);
      B:=IncreasingRequests and AllowMoreThreads and (FIdleThreads=FRetiring);
      ThreadLog('TThreadPool.GrowIfStarved','Attempt to create new thread %s',[BToS(B)]);
      if B then
      begin
        CreateNewThread:=FRetiring<=0;
        if CreateNewThread then
          AddThreadToPool;
      end;
    finally
      ThreadLog('TThreadPool.GrowIfStarved','unlocking queue');
      UnLockQueue;
    end;
    if Not CreateNewThread then
      FRetireEvent.SetEvent;
    end;
end;

function TThreadPool.AddThreadToPool : TQueueWorkerThread;

begin
  ThreadLog('TThreadPool.AddThreadToPool','Enter');
  FThreadCreationAt:=GetTickCount64;
  Result:=TQueueWorkerThread.Create(Self);
  AtomicIncrement(FThreadCount);
  ThreadLog('TThreadPool.AddThreadToPool','Leave (thread count: %d)',[FThreadCount]);
end;

function TThreadPool.DoMonitor : TMonitorResult;

var
  I: Integer;
  AvgCPU: Cardinal;

begin
  Result:=TMonitorResult.mrContinue;
  if FShutdown then
    Exit(TMonitorResult.mrTerminate);
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
  FreeOnTerminate:=True;
  RemoveFromPool;
  Terminate;
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

constructor TThreadPool.TBaseWorkerThread.Create(aThreadPool: TThreadPool);
begin
  FreeOnTerminate:=True;
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
          ThreadLog('TThreadPool.TQueueWorkerThread.Execute','No work, stopping');
          Terminate;
          end;
      if Assigned(Itm) then
        begin
        ThreadLog('TThreadPool.TQueueWorkerThread.Execute','Calling WrapExecute. Idle: %s',[BToS(Idle)]);
        WrapExecute(Itm);
        FCheckWaitTime:=IdleTimeout;
        ThreadLog('TThreadPool.TQueueWorkerThread.Execute','Called WrapExecute. Idle: %s',[BToS(Idle)]);
        end;
      if ThreadPool.CheckShouldTerminate(Self) then
        begin
        ThreadLog('TThreadPool.TQueueWorkerThread.Execute','Threadpool said to stop; terminating');
        Terminate;
        end;
      if Terminated then
        ThreadLog('TThreadPool.TQueueWorkerThread.Execute','Thread Terminated');
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

  finally
    FThreadPool.FMonitorStatus:=MonitorNone;
  end;
end;

procedure TThreadPool.TThreadPoolMonitor.GrowThreadPoolIfStarved;
begin
  FThreadPool.GrowIfStarved
end;

constructor TThreadPool.TThreadPoolMonitor.Create(aThreadPool: TThreadPool);
begin
  FThreadPool:=aThreadPool;
  FreeOnTerminate:=True;
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
  FreeAndNil(FException);
  FreeAndNil(FStateLock);
  FreeAndNil(FDoneEvent);
  inherited Destroy;
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

class function TTask.Run(const aFunc: TProcRef; aPool: TThreadPool): ITask;
begin
  Result:=TTask.Create(aFunc,aPool);
  Result.Start;
end;

class function TTask.Run(const aFunc: TProcRef): ITask;
begin
  Result:=Run(aFunc,TThreadPool.Default);
end;

function TTask.GetIsComplete: Boolean;
begin
  Result:=(FStateFlags*CompleteStates) <> [];
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
  FStateLock.Leave;
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
    if Have(TOptionStateFlag.Canceled) and Assigned(FParams.ParentControlFlag) and (FParams.ParentControlFlag.Value>0) then
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
  Result:=CreateReplicaTask(aParams);
end;

procedure TTask.CheckFaulted;

var
  E: TObject;
begin
  ThreadLog('TTask.CheckFaulted','CheckFaulted');
  E:=GetExceptionObject;
  if Assigned(E) then
    begin
    ThreadLog('TTask.CheckFaulted','CheckFaulted have error');
    SetRaisedState;
    raise E;
    end;
end;


procedure TTask.SetComplete;

begin
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
  for T in FTasksWithExceptions do
    begin
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
  LastTask:=((FSubTasks=1) and not IsReplicating) or (AtomicDecrement(FSubTasks)<=0);
  if LastTask then
    IntermediateCompletion
  else
    UpdateStateAtomic([TOptionStateFlag.ChildWait], [TOptionStateFlag.Faulted, TOptionStateFlag.Canceled, TOptionStateFlag.Complete]);
  if Length(FTasksWithExceptions)=0 then
    Exit;
  LockState;
  try
    Last:=Length(FTasksWithExceptions)-1;
    for I:=Last downto 0 do
      if TTask(FTasksWithExceptions[I]).WasExceptionRaised then
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

procedure TTask.IntermediateCompletion;

var
  State: TOptionStateFlags;

begin
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
  ThreadLog('TTask.InternalWork','Enter');
  BusyCheck:=aCheckExecuting or (TOptionStateFlag.Replicating in FStateFlags);
  ThreadLog('TTask.InternalWork','busycheck: %s:=%s or (%s));',[BToS(BusyCheck),BToS(aCheckExecuting),BToS(TOptionStateFlag.Replicating in FStateFlags)]);
  if Not BusyCheck then
    begin
    ThreadLog('TTask.InternalWork','set running');
    ForceStateFlags([TOptionStateFlag.CallbackRun]);
    end
  else if not UpdateStateAtomic([TOptionStateFlag.CallbackRun], [TOptionStateFlag.CallbackRun]) and
    not (TOptionStateFlag.Canceled in FStateFlags) then
      Exit(False);
  if IsCanceled then
    Complete(False)
  else
    begin
    ThreadLog('TTask.InternalWork','calling internalexecute');
    InternalExecute(_CurrentTask);
    end;
  Result:=True;
end;

procedure TTask.InternalExecute(var aCurrentTaskVar: TTask);

var
  Old : TTask;
  Executed : Boolean;

begin
  Old:=aCurrentTaskVar;
  try
    aCurrentTaskVar:=Self;
    Execute;
    Executed:=not (HasExceptions or IsCanceled);
    Complete(Executed);
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

procedure TTask.Execute;
begin
  if IsReplicating then
    ExecuteReplicates(Self)
  else
    try
      CallUserCode;
    except
      HandleException(Self,TObject(AcquireExceptionObject));
    end;
end;

procedure TTask.ExecuteReplicates(const aRoot: TTask);

 procedure DoCallUserCode;

 begin
   try
     aRoot.CallUserCode;
   except
     aRoot.HandleException(CurrentTask, TObject(AcquireExceptionObject));
     Complete(False);
   end;
 end;

var
  Sub : ITask;
  P : TProcRef;

begin
  P:=@DoCallusercode;
  While aRoot.ShouldCreateReplica do
    begin
    ThreadLog('TTask.ExecuteReplicates','Creating replica');
    Sub:=aRoot.CreateReplicaTask(P,aRoot,[TCreateFlag.Replicating, TCreateFlag.Replica],FParams.ParentControlFlag);
    ThreadLog('TTask.ExecuteReplicates','Starting replica');
    Sub.Start;
    ThreadLog('TTask.ExecuteReplicates','Started replica');
    end;
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
    SetExceptionObject(aException);
    Exit;
    end;
  LockState;
  try
    aChildTask.SetExceptionObject(aException);
    Len:=Length(FTasksWithExceptions);
    I:=Len-1;
    While (I>=0) and (FTasksWithExceptions[i].FTaskId<>aChildTask.FTaskId) do
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

function TTask.Wait(aTimeout: Cardinal): Boolean;

  Procedure RunChecks; inline;
  begin
    ThreadLog('TTask.Wait.RunChecks','Enter');
    try
      CheckCanceled;
      CheckFaulted;
    finally
      ThreadLog('TTask.Wait.RunChecks','Leave');
    end;
  end;

var
  NeedSync : Boolean;
  Watch : TStopWatch;

begin
  ThreadLog('TTask.Wait','Enter (atimeout: %d) ',[aTimeout]);
  Result:=IsComplete;
  if Result then
    begin
    ThreadLog('TTask.Wait','Complete');
    Runchecks;
    Exit;
    end;
  NeedSync:=(TThread.CurrentThread.ThreadID=MainThreadID) and FParams.Pool.Interactive;
  if Not NeedSync then
    begin
    ThreadLog('TTask.Wait','Waiting for done event (%d)',[aTimeout]);
    Result:=DoneEvent.WaitFor(aTimeout)<>wrTimeout;
    if Result then
      RunChecks;
    end
  else
    begin
    if aTimeOut=INFINITE then
      Watch:=Default(TStopWatch)
    else
      Watch:=TStopWatch.Create;
    Repeat
      CheckSynchronize(1);
    until IsComplete or (Not Watch.IsRunning) or (Watch.ElapsedMilliseconds>aTimeOut);
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
  LFlags: TOptionStateFlags;
  OldQueued: Boolean;

begin
  if IsComplete then
    exit;
  SetTaskStop;
  OldQueued:=IsQueued;
  LFlags:=[TOptionStateFlag.Canceled];
  if aDestroying then
    Include(LFlags, TOptionStateFlag.Destroying);
  UpdateStateAtomic(LFlags,[TOptionStateFlag.Faulted,TOptionStateFlag.Complete]);
  if not (OldQueued or IsQueued) then
    Complete(False);
end;

procedure TTask.Cancel;

begin
  DoCancel(False);
end;


procedure TTask.CheckCanceled;
begin
  if TOptionStateFlag.Canceled in FStateFlags then
    raise EOperationCancelled.Create(SOperationCancelled);
end;

function TTask.Start: ITask;
begin
  if IsComplete then
    raise EInvalidOperation.Create(SCannotStartCompletedTask);
  Result:=Self;flush(output);
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

procedure TTask.AddCompleteEvent(const aProc: TITaskProc);
begin
  LockState;
  try
    If Length(FCompletedEvents)=FCompletedEventCount then
      SetLength(FCompletedEvents,FCompletedEventCount+32);
    FCompletedEvents[FCompletedEventCount]:=aProc;
    Inc(FCompletedEventCount);
  finally
    UnlockState;
  end;
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
    While (Idx>0) and (FCompletedEvents[Idx]<>aProc) do
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
  if FParams.Parent<>nil then
    FParams.Parent.AddChild;
  FStateFlags:=[];
  FStatus:=TTaskStatus.Created;
  if TCreateFlag.Replicating in aParams.CreateFlags then
    Include(FStateFlags, TOptionStateFlag.Replicating);
  if TCreateFlag.Replica in aParams.CreateFlags then
    Include(FStateFlags, TOptionStateFlag.Replica);
  FStateLock:=TCriticalSection.Create;
  FDoneEvent:=TEvent.Create;
end;

class function TTask.DoWaitForAll(const aTasks: array of ITask; aTimeout: Cardinal): Boolean;

var
  I: Integer;
  Task: TTask;
  TaskI : ITask;
  WaitTasks: TITaskArray;
  ExceptionCount,CompletedCount,CancelCount : Integer;
  aWaitCompletedCount,aWaitCount : Integer;
  CompleteProc: TITaskProc;
  ExceptionList: TExceptionList;
  Event : TEvent;
  Waiting,
  NeedSync : Boolean;
  Watch : TStopWatch;

  Procedure DoWait;

  begin
    Result:=False;
    if not NeedSync then
      Event.WaitFor(aTimeOut)
    else
      Repeat
        CheckSynchronize(1);
      until (Event.WaitFor(0)=wrSignaled);
  end;

  procedure TaskCompleted(ATask: ITask);
  begin
    if Waiting then
      AtomicIncrement(aWaitCompletedCount);
    AtomicIncrement(CompletedCount);
    if (ATask as TTask).HasExceptions then
      AtomicIncrement(ExceptionCount)
    else if (aTask as TTask).IsCanceled then
      AtomicIncrement(CancelCount);
    Event.SetEvent;
  end;

begin
  Result:=True;
  ExceptionCount:=0;
  CompletedCount:=0;
  CancelCount:=0;
  aWaitCount:=0;
  aWaitCompletedCount:=0;
  WaitTasks:=[];
  CompleteProc:=@TaskCompleted;
  SetLength(WaitTasks,Length(aTasks));
  NeedSync:=(TThread.CurrentThread.ThreadID=MainThreadID) and ((aTasks[0] as TTask).FParams.Pool.FInteractive);
  Waiting:=False;
  for TaskI in aTasks do
    begin
    Task:=TaskI as TTask;
    if Task.IsComplete then
      begin
      TaskCompleted(TaskI);
      end
    else
      begin
      // Add to wait list
      if (aTimeout<>INFINITE) or not (Task.InternalExecuteNow and Task.IsComplete) then
        begin
        WaitTasks[aWaitCount]:=TaskI;
        Inc(aWaitCount);
        end;
      end;
    end;
  if aWaitCount>0 then
    begin
    Waiting:=True;
    Event:=TEvent.Create;
    try
      for TaskI in WaitTasks do
        begin
        Task:=(TaskI as TTask);
        Task.AddCompleteEvent(CompleteProc);
        end;
      if aTimeOut=INFINITE then
        Watch:=Default(TStopWatch)
      else
        Watch:=TStopWatch.Create;
      While (aWaitCompletedCount<aWaitCount) and ((Not Watch.IsRunning) or (Watch.ElapsedMilliseconds>aTimeOut)) do
        begin
        DoWait;
        Event.ResetEvent;
        end;
      Result:=aWaitCompletedCount>=aWaitCount;
    finally
      For I:=0 to aWaitCount-1 do
        (WaitTasks[I] as TTask).RemoveCompleteEvent(CompleteProc);
      FreeAndNil(Event);
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
  Lock : TCriticalSection;
  Event : TEvent;

  Function MakeCompleted(aIndex : integer) : TITaskProc;

  begin
    Result:=Procedure (aTask : ITask)
      begin
      Lock.Enter;
      Try
        if Res=-1 then
          begin
          Res:=aIndex;
          Event.SetEvent;
          end;
      finally
        Lock.Leave;
      end;
      if aTask<>Nil then;
      end;
    end;

var
  I,Len : Integer;
  WaitTasks: TITaskArray;
  WaitProcs: Array of TITaskProc;
  aWaitCount : Integer;
  NeedSync : Boolean;
  Watch : TStopWatch;

  Function FillWaitList : Integer;

  begin
    aWaitCount:=0;
    SetLength(WaitTasks,Length(aTasks));
    SetLength(WaitProcs,Length(aTasks));
    I:=0;
    Result:=0;
    While (Result=-1) and (I<Len) do
      begin
      if (aTasks[i] as TTask).IsComplete then
        Result:=I
      else
        begin
        WaitTasks[I]:=aTasks[i];
        WaitProcs[I]:=MakeCompleted(I);
        (aTasks[i] as TTask).AddCompleteEvent(WaitProcs[i]);
        Inc(aWaitCount);
        end;
      Inc(I);
      end;
  end;

  Procedure DoWait;

  begin
    if not NeedSync then
      Event.WaitFor(aTimeOut)
    else
      Repeat
        CheckSynchronize(1);
      until (Event.WaitFor(0)=wrSignaled);
  end;

begin
  Result:=-1;
  // Check if we have a task that is already done.
  Len:=Length(aTasks);
  I:=0;
  While (Result=-1) and (I<Len) do
    begin
    if (aTasks[i] as TTask).IsComplete then
      Result:=I;
    Inc(I);
    end;
  if (Result<>-1) then
    begin
    aTasks[Result].Wait(0);
    exit;
    end;
  WaitTasks:=[];
  NeedSync:=(TThread.CurrentThread.ThreadID=MainThreadID) and ((aTasks[0] as TTask).FParams.Pool.FInteractive);
  Event:=Nil;
  Lock:=TCriticalSection.Create;
  try
    Event:=TEvent.Create;
    Res:=FillWaitList;
    if (Res<>-1) then
      begin
      aTasks[Res].Wait(0);
      exit(Res);
      end;
    if aWaitCount>0 then
      try
        if aTimeOut=INFINITE then
          Watch:=Default(TStopWatch)
        else
          Watch:=TStopWatch.Create;
        While (Res=-1) and ((Not Watch.IsRunning) or (Watch.ElapsedMilliseconds>aTimeOut)) do
          DoWait;
        Result:=Res;
        if (Res<>-1) and ((Not watch.IsRunning) or (Watch.ElapsedMilliseconds<=aTimeOut))  then
          begin
          Result:=Res;
          aTasks[Result].Wait(0);
          end;
      finally
        For I:=0 to aWaitCount-1 do
          begin
          (WaitTasks[I] as TTask).RemoveCompleteEvent(WaitProcs[i]);
          WaitProcs[i]:=Nil;
          end;
      end;
  finally
    FreeAndNil(Event);
    FreeAndNil(Lock);
  end;
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
  TFuture
  *********************************************************************}

procedure TFuture.RunFunc(Sender: TObject);
begin
  FResult:=Default(T);
  if Assigned(FFuncRef) then
    FResult:=FFuncRef()
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

var
  Params : TTaskParams;

begin
  Params:=Default(TTaskParams);
  Params.Event:=@RunFunc;
  Params.Sender:=aSender;
  Params.Pool:=aPool;
  FFuncEvent:=aEvent;
  FFuncRef:=aFunc;
  inherited Create(Params);
end;


{ *********************************************************************
  TReplicableTask
  *********************************************************************}

constructor TEventJoinTask.Create(Sender: TObject; const AEvents: array of TNotifyEvent; APool: TThreadPool);
var
  I,Len : integer;
  Params : TTaskParams;

begin
  // Copy procs
  Len:=Length(AEvents);
  SetLength(FEventList,Len);
  For I:=0 to Len-1 do
    FEventList[I]:=aEvents[i];
  Params.Proc:=@JoinTasks;
  Params.Parent:=Self;
  Params.Pool:=aPool;
  Params.Sender:=Sender;
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
  TParallel.Parallelize32(Loop,Self.ThreadPool);
end;

{ *********************************************************************
  TReplicableTask
  *********************************************************************}

function TReplicableTask.ShouldCreateReplica: Boolean;

begin
  ThreadLog('TReplicableTask.ShouldCreateReplica','Enter (TaskCount: %d)',[FTaskCount]);
  Result:=False;
  if (FTaskCount<=0) then
    exit;
  AtomicDecrement(FTaskCount);
  Result:=FTaskCount>0;
  ThreadLog('TReplicableTask.ShouldCreateReplica','Leave: %s ',[BToS(Result)]);
end;

function TReplicableTask.CreateReplicaTask(const aParams : TTaskParams): TTask;

begin
  ThreadLog('TReplicableTask.CreateReplicaTask','Enter');
  Result:=TReplicatedTask.Create(aParams);
  ThreadLog('TReplicableTask.CreateReplicaTask','Leave');
end;

constructor TReplicableTask.Create(const aParams : TTaskParams; aTaskCount: Integer);
begin
  ThreadLog('TReplicableTask.Create','Enter  (%d)',[aTaskCount]);
  inherited Create(aParams);
  FTaskCount:=aTaskCount;
  if FTaskCount<0 then
    FTaskCount:=2*TThread.ProcessorCount;
  ThreadLog('TReplicableTask.Create','Leave');
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
  TParallel.Parallelize32(Loop,Self.ThreadPool);
end;

constructor TProcJoinTask.Create(const AProcs: array of TProcRef; APool: TThreadPool);

var
  I,Len : integer;
  Params : TTaskParams;

begin
  // Copy procs
  Len:=Length(aProcs);
  SetLength(FProcList,Len);
  For I:=0 to Len-1 do
    FProcList[I]:=aProcs[i];
  Params.Proc:=@JoinTasks;
  Params.Parent:=Self;
  Params.Pool:=aPool;
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


class function TParallel.&For(aLowInclusive, aHighInclusive: Integer; const aIteratorEvent: TProcInteger; aPool: TThreadPool
  ): TLoopResult;
var
  aLoop: TInt32LoopProc;
begin
  aLoop:=TInt32LoopProc.Create(aLowInclusive,aHighInclusive,aIteratorEvent);
  Result:=Parallelize32(aLoop,aPool);
end;

class function TParallel.&For(aLowInclusive, aHighInclusive: Integer; const aIteratorEvent: TProcInteger): TLoopResult;
begin
  Result:=&For(aLowInclusive,aHighInclusive,aIteratorEvent,TThreadPool.Default);
end;


class function TParallel.&For(aLowInclusive, aHighInclusive: Integer; const aIteratorEvent: TProcIntegerLoopState;
  aPool: TThreadPool): TLoopResult;
var
  aLoop: TInt32LoopProc;
begin
  aLoop:=TInt32LoopProc.Create(aLowInclusive,aHighInclusive,aIteratorEvent);
  Result:=Parallelize32(aLoop,aPool);
end;

class function TParallel.&For(aLowInclusive, aHighInclusive: Integer; const aIteratorEvent: TProcIntegerLoopState): TLoopResult;
begin
  Result:=&For(aLowInclusive,aHighInclusive,aIteratorEvent,TThreadPool.Default);
end;


class function TParallel.&For(aStride, aLowInclusive, aHighInclusive: Integer; const aIteratorEvent: TProcInteger; aPool: TThreadPool): TLoopResult;

var
  aLoop: TInt32LoopProc;

begin
  aLoop:=TInt32LoopProc.Create(aLowInclusive,aHighInclusive,aIteratorEvent);
  aLoop.Stride:=aStride;
  Result:=Parallelize32(aLoop,aPool);
end;

class function TParallel.&For(aStride, aLowInclusive, aHighInclusive: Integer; const aIteratorEvent: TProcInteger): TLoopResult;

begin
  Result:=&For(aStride, aLowInclusive,aHighInclusive,aIteratorEvent,TThreadPool.Default);
end;

class function TParallel.&For(aStride, aLowInclusive, aHighInclusive: Integer; const aIteratorEvent: TProcIntegerLoopState;
  aPool: TThreadPool): TLoopResult;
var
  aLoop: TInt32LoopProc;

begin
  aLoop:=TInt32LoopProc.Create(aLowInclusive,aHighInclusive,aIteratorEvent);
  aLoop.Stride:=aStride;
  Result:=Parallelize32(aLoop,aPool);
end;

class function TParallel.&For(aStride, aLowInclusive, aHighInclusive: Integer; const aIteratorEvent: TProcIntegerLoopState): TLoopResult;
begin
  Result:=&For(aStride, aLowInclusive,aHighInclusive,aIteratorEvent,TThreadPool.Default);
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

class function TParallel.&For(aLowInclusive, aHighInclusive: Int64; const aIteratorEvent: TProcInt64; aPool: TThreadPool
  ): TLoopResult;
var
  aLoop: TInt64LoopProc;
begin
  aLoop:=TInt64LoopProc.Create(aLowInclusive,aHighInclusive,aIteratorEvent);
  Result:=Parallelize64(aLoop,aPool);
end;

class function TParallel.&For(aLowInclusive, aHighInclusive: Int64; const aIteratorEvent: TProcInt64): TLoopResult;
begin
  Result:=&For(aLowInclusive,aHighInclusive,aIteratorEvent,TThreadPool.Default);
end;

class function TParallel.&For(aLowInclusive, aHighInclusive: Int64; const aIteratorEvent: TProcInt64LoopState; aPool: TThreadPool
  ): TLoopResult;
var
  aLoop: TInt64LoopProc;
begin
  aLoop:=TInt64LoopProc.Create(aLowInclusive,aHighInclusive,aIteratorEvent);
  Result:=Parallelize64(aLoop,aPool);
end;

class function TParallel.&For(aLowInclusive, aHighInclusive: Int64; const aIteratorEvent: TProcInt64LoopState): TLoopResult;
begin
  Result:=&For(aLowInclusive,aHighInclusive,aIteratorEvent,TThreadPool.Default);
end;

class function TParallel.&For(aStride, aLowInclusive, aHighInclusive: Int64; const aIteratorEvent: TProcInt64; aPool: TThreadPool): TLoopResult;

var
  aLoop: TInt64LoopProc;
begin
  aLoop:=TInt64LoopProc.Create(aLowInclusive,aHighInclusive,aIteratorEvent);
  aLoop.Stride:=aStride;
  Result:=Parallelize64(aLoop,aPool);
end;

class function TParallel.&For(aStride, aLowInclusive, aHighInclusive: Int64; const aIteratorEvent: TProcInt64): TLoopResult;
begin
  Result:=&For(aStride,aLowInclusive,aHighInclusive,aIteratorEvent,TThreadPool.Default);
end;

class function TParallel.&For(aStride, aLowInclusive, aHighInclusive: Int64; const aIteratorEvent: TProcInt64LoopState;
  aPool: TThreadPool): TLoopResult;
var
  aLoop: TInt64LoopProc;
begin
  aLoop:=TInt64LoopProc.Create(aLowInclusive,aHighInclusive,aIteratorEvent);
  aLoop.Stride:=aStride;
  Result:=Parallelize64(aLoop,aPool);
end;

class function TParallel.&For(aStride, aLowInclusive, aHighInclusive: Int64; const aIteratorEvent: TProcInt64LoopState
  ): TLoopResult;
begin
  Result:=&For(aStride,aLowInclusive,aHighInclusive,aIteratorEvent,TThreadPool.Default);
end;
{$ENDIF}

class function TParallel.Join(aSender: TObject; aEvents: array of TNotifyEvent; aPool: TThreadPool): ITask;
begin
  Result:=TEventJoinTask.Create(aSender,aEvents,aPool);
end;

class function TParallel.Join(const aProcs: array of TProcRef; aPool: TThreadPool): ITask;
begin
  Result:=TProcJoinTask.Create(aProcs,aPool);
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
  if not FLoopParams.Break then
    exit;
  TInt32LoopParams(FLoopParams).UpdateBreakAt(CurrentIteration);
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
  if not FLoopParams.Break then
    exit;
  TInt64LoopParams(FLoopParams).UpdateBreakAt(CurrentIteration);
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
  Result:=FBreakAt;
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
  ThreadLog('TParallel.TInt32LoopParams.GetNextStride','Current: %d, Count: %d, nextat: %d',[Result, FStrideCount, FNextStrideAt]);
  ThreadLog('TParallel.TInt32LoopParams.GetNextStride','if %s or not %s then',[BToS(MaxReached), BToS(NextOK)]);
  if MaxReached  or Not NextOK then
    begin
    ThreadLog('TParallel.TInt32LoopParams.GetNextStride','Early exit');
    exit;
    end;
  NewValue:=Result*2;
  if (NewValue>FMaxStride) then
    NewValue:=FMaxStride;
  // Only get new value if old did not change
  AtomicCmpExchange(FLoopProc.Stride,NewValue,Result);
  ThreadLog('TParallel.TInt32LoopParams.GetNextStride','Result: %d',[Result]);
end;

function TParallel.TInt32LoopParams.ShouldExitLoop: Boolean;
var
  Flags: TLoopStateFlagSet;

begin
  Result:=False;
  Flags:=StateFlags;
  If (Flags=[]) then
    exit;
  If (Flags*ShouldExitFlags)<>[] then
    Exit(True);
end;


function TParallel.TInt32LoopParams.ShouldExitLoop(CurrentIter : Integer): Boolean;

begin
  Result:=False;
  Result:=ShouldExitLoop;
  if Result then
    exit;
  Result:=(TLoopStateFlag.Broken in StateFlags) and (CurrentIter>FBreakAt);
end;



function TParallel.TInt32LoopParams.GetCurrentStart: Integer;

var
  aStride : Integer;

begin
  aStride:=GetCurrentStride;
  ThreadLog('TParallel.TInt32LoopParams.GetCurrentStart','Index: %d, Stride: %d',[FLoopProc.Index,aStride]);
  Result:=TInterlocked.Add(FLoopProc.Index,aStride)-aStride;
  ThreadLog('TParallel.TInt32LoopParams.GetCurrentStart','Result : %d',[Result]);
end;

constructor TParallel.TInt32LoopParams.Create(aLoopProc: TInt32LoopProc);
begin
  FLoopProc:=aLoopProc;
  FNextStrideAt:=TThread.ProcessorCount;
  FBreakAt:=aLoopProc.HighExclusive+1;
  FMaxStride:=FNextStrideAt*16; // 16 loops max
end;

destructor TParallel.TInt32LoopParams.Destroy;
begin
  ThreadLog('TParallel.TInt32LoopParams.Destroy','Enter (%d)',[PtrInt(Self)]);
  inherited Destroy;
  ThreadLog('TParallel.TInt32LoopParams.Destroy','Leave (%d)',[PtrInt(Self)]);
end;

procedure TParallel.TInt32LoopParams.Invoke;

var
  I, Start, Limit, UpperLimit, MyStride: Integer;

begin
  ThreadLog('TParallel.TInt32LoopParams.Invoke','Enter');
  ThreadLog('TParallel.TInt32LoopParams.Invoke','Loop params: '+Self.FLoopProc.ToString);
  UpperLimit:=HighExclusive;
  Try
    Start:=GetCurrentStart;
    MyStride:=GetCurrentStride;
    ThreadLog('TParallel.TInt32LoopParams.Invoke','Start: %d, Upper: %d, Stride: %d',[Start,UpperLimit,MyStride]);
    while Start<UpperLimit do
      begin
      I:=Start;
      Limit:=Start+MyStride;
      If Limit>UpperLimit then
        Limit:=UpperLimit;
      ThreadLog('TParallel.TInt32LoopParams.Invoke','Inner loop from %d to Limit: %d',[I,Limit]);
      while (I<Limit) and not ShouldExitLoop(Start) do
        begin
        ThreadLog('TParallel.TInt32LoopParams.Invoke','Executing loop at %d',[I]);
        FLoopProc.Execute(I);
        Inc(I);
        end;
      if ShouldExitLoop(Start) then
        Break;
      GetNextStride;
      MyStride:=GetCurrentStride;
      Start:=GetCurrentStart;
      ThreadLog('TParallel.TInt32LoopParams.Invoke','Next loop from %d to %d',[Start,Start+MyStride]);
      end;
  except
//    Fparams.SharedFlags.SetFaulted;
    raise;
  end;
  ThreadLog('TParallel.TInt32LoopParams.Invoke','leave');
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
  Result:=FBreakAt;
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
  ThreadLog('TParallel.TInt64LoopParams.GetNextStride','Current: %d, Count: %d, nextat: %d',[Result, FStrideCount, FNextStrideAt]);
  ThreadLog('TParallel.TInt64LoopParams.GetNextStride','if %s or not %s then',[BToS(MaxReached), BToS(NextOK)]);
  if MaxReached  or Not NextOK then
    begin
    ThreadLog('TParallel.TInt64LoopParams.GetNextStride','Early exit');
    exit;
    end;
  NewValue:=Result*2;
  if (NewValue>FMaxStride) then
    NewValue:=FMaxStride;
  // Only get new value if old did not change
  AtomicCmpExchange(FLoopProc.Stride,NewValue,Result);
  ThreadLog('TParallel.TInt64LoopParams.GetNextStride','Result: %d',[Result]);
end;

function TParallel.TInt64LoopParams.ShouldExitLoop: Boolean;
var
  Flags: TLoopStateFlagSet;

begin
  Result:=False;
  Flags:=StateFlags;
  If (Flags=[]) then
    exit;
  If (Flags*ShouldExitFlags)<>[] then
    Exit(True);
end;


function TParallel.TInt64LoopParams.ShouldExitLoop(CurrentIter : Int64): Boolean;

begin
  Result:=False;
  Result:=ShouldExitLoop;
  if Result then
    exit;
  Result:=(TLoopStateFlag.Broken in StateFlags) and (CurrentIter>FBreakAt);
end;



function TParallel.TInt64LoopParams.GetCurrentStart: Int64;

var
  aStride : Int64;

begin
  aStride:=GetCurrentStride;
  ThreadLog('TParallel.TInt64LoopParams.GetCurrentStart','Index: %d, Stride: %d',[FLoopProc.Index,aStride]);
  Result:=TInterlocked.Add(FLoopProc.Index,aStride)-aStride;
  ThreadLog('TParallel.TInt64LoopParams.GetCurrentStart','Result : %d',[Result]);
end;

constructor TParallel.TInt64LoopParams.Create(aLoopProc: TInt64LoopProc);
begin
  FLoopProc:=aLoopProc;
  FNextStrideAt:=TThread.ProcessorCount;
  FBreakAt:=aLoopProc.HighExclusive+1;
  FMaxStride:=FNextStrideAt*16; // 16 loops max
end;

destructor TParallel.TInt64LoopParams.Destroy;
begin
  ThreadLog('TParallel.TInt64LoopParams.Destroy','Enter (%d)',[PtrInt(Self)]);
  inherited Destroy;
  ThreadLog('TParallel.TInt64LoopParams.Destroy','Leave (%d)',[PtrInt(Self)]);
end;

procedure TParallel.TInt64LoopParams.Invoke;

var
  I, Start, Limit, UpperLimit, MyStride: Int64;

begin
  ThreadLog('TParallel.TInt64LoopParams.Invoke','Enter');
  ThreadLog('TParallel.TInt64LoopParams.Invoke','Loop params: '+Self.FLoopProc.ToString);
  UpperLimit:=HighExclusive;
  Try
    Start:=GetCurrentStart;
    MyStride:=GetCurrentStride;
    ThreadLog('TParallel.TInt64LoopParams.Invoke','Start: %d, Upper: %d, Stride: %d',[Start,UpperLimit,MyStride]);
    while Start<UpperLimit do
      begin
      I:=Start;
      Limit:=Start+MyStride;
      If Limit>UpperLimit then
        Limit:=UpperLimit;
      ThreadLog('TParallel.TInt64LoopParams.Invoke','Inner loop from %d to Limit: %d',[I,Limit]);
      while (I<Limit) and not ShouldExitLoop(Start) do
        begin
        ThreadLog('TParallel.TInt64LoopParams.Invoke','Executing loop at %d',[I]);
        FLoopProc.Execute(I);
        Inc(I);
        end;
      if ShouldExitLoop(Start) then
        Break;
      GetNextStride;
      MyStride:=GetCurrentStride;
      Start:=GetCurrentStart;
      ThreadLog('TParallel.TInt64LoopParams.Invoke','Next loop from %d to %d',[Start,Start+MyStride]);
      end;
  except
//    Fparams.SharedFlags.SetFaulted;
    raise;
  end;
  ThreadLog('TParallel.TInt64LoopParams.Invoke','leave');
end;

{$ENDIF}

{ *********************************************************************
  TParallel.TInt32LoopProc
  *********************************************************************}

procedure TParallel.TInt32LoopProc.Execute(Iteration: Integer);

begin
  ThreadLog('TParallel.TInt32LoopProc.Execute','enter (%d)',[Iteration]);
  //       This would make it so that only a single virtual call is made to process the iterations.
  if Assigned(Event) then
    Event(Sender, Iteration)
  else if Assigned(Proc) then
    Proc(Iteration)
  else if Assigned(ProcWithState) then
  begin
    State.CurrentIteration:=Iteration;
    ProcWithState(Iteration,State);
  end
  else if Assigned(StateEvent) then
  begin
    State.CurrentIteration:=Iteration;
    StateEvent(Sender,Iteration, State);
  end;
  ThreadLog('TParallel.TInt32LoopProc.Execute','leave (%d) ',[Iteration]);
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

procedure TParallel.TInt64LoopProc.Execute(Iteration: Int64);

begin
  ThreadLog('TParallel.TInt64LoopProc.Execute','enter (%d)',[Iteration]);
  //       This would make it so that only a single virtual call is made to process the iterations.
  if Assigned(Event) then
    Event(Sender, Iteration)
  else if Assigned(Proc) then
    Proc(Iteration)
  else if Assigned(ProcWithState) then
  begin
    State.CurrentIteration:=Iteration;
    ProcWithState(Iteration,State);
  end
  else if Assigned(StateEvent) then
  begin
    State.CurrentIteration:=Iteration;
    StateEvent(Sender,Iteration, State);
  end;
  ThreadLog('TParallel.TInt64LoopProc.Execute','leave (%d) ',[Iteration]);
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
  FRootTask:=Nil;
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
  finally
    UnLock
  end;
end;

function TParallel.TLoopParams.Stopped: Boolean;
begin
  Result:=TLoopStateFlag.Stopped in StateFlags;
end;

function TParallel.TLoopParams.Faulted: Boolean;
begin
  Result:=TLoopStateFlag.Exception in StateFlags;
end;

constructor TParallel.TLoopParams.Create;
begin
  FStateLock:=TCriticalSection.Create;
end;

destructor TParallel.TLoopParams.Destroy;
begin
  FreeAndNil(FStateLock);
  inherited Destroy;
end;

procedure TParallel.TLoopParams.Lock;
begin
  FStateLock.Enter;
end;

procedure TParallel.TLoopParams.UnLock;
begin
  FStateLock.Leave;
end;

procedure TParallel.TLoopParams.HandleException(O: TObject);

var
  E : Exception absolute O;
  ErrorTasks : TTaskArray;
  ExcList : TExceptionList;
  aTask: TTask;


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
    for aTask in ErrorTasks do
      ExcList.AddFromTask(aTask);
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
