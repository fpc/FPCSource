 unit fpthreadpool;

{$mode ObjFPC}{$H+}
{$DEFINE DEBUGTHREADPOOL}

interface

Uses Classes, SysUtils, DateUtils, SyncObjs;


Const
  DefaultAddTimeOut      = 0;
  DefaultQueueTasks      = True;
  DefaultTerminateWait   = 50;

Type
  EThreadPool = Class(Exception);

  TFPCustomSimpleThreadPool = Class;
  TFPCustomSimpleThreadPoolClass = Class of TFPCustomSimpleThreadPool;

  { TThreadPoolTask }

  TExceptionEvent = procedure (Sender : TObject; aException : Exception);

  TThreadPoolTask = class(TObject)
  private
    FDoneOnException: Boolean;
    FOnCancel: TNotifyEvent;
    FOnDone: TNotifyEvent;
    FOnException: TExceptionEvent;
    FOnQueued: TNotifyEvent;
    FOnTerminate: TNotifyEvent;
    FTerminated: Boolean;
  Protected
    Procedure DoExecute; virtual; abstract;
    Procedure DoDone; virtual;
    Procedure DoOnException(E : Exception); virtual;
    Procedure DoTerminate; virtual;
    Procedure DoQueued; virtual;
    Procedure DoCancel; virtual;
  Public
    // Call when a task is put on wait queue
    Procedure Queued;
    // Call when a task must be executed
    Procedure Execute;
    // Call to indicate a running task it should terminate
    procedure Terminate;
    // Call when a task is canceled. (i.e. never executed)
    procedure Cancel;
    // Called when task is done
    Property OnDone : TNotifyEvent Read FOnDone Write FOnDone;
    // Called when an exception is raised during the task
    Property OnException : TExceptionEvent Read FOnException Write FOnException;
    // Set to true when the OnDone event should also be executed when the task raised an exception.
    Property DoneOnException : Boolean Read FDoneOnException Write FDoneOnException;
    // Called when the Terminate procedure was called.
    Property OnTerminate : TNotifyEvent Read FOnTerminate Write FOnTerminate;
    // Called when the task is put on the wait queue.
    Property OnQueued : TNotifyEvent Read FOnQueued Write FOnQueued;
    // Called when the task is canceled: removed from the queue.
    Property OnCancel : TNotifyEvent Read FOnCancel Write FOnCancel;
    // Set when terminated is called;
    Property Terminated : Boolean Read FTerminated;
  end;

  TTaskCallBack = Procedure (aData : TObject);

  { TCallBackThreadPoolTask }

  TCallBackThreadPoolTask = class(TThreadPoolTask)
  Private
    FCallback : TTaskCallBack;
    FData : TObject;
  Protected
    Procedure DoExecute; override;
    Property Callback : TTaskCallBack Read FCallBack;
    Property Data : TObject Read FData;
  Public
    Constructor Create(aCallback : TTaskCallBack; aData : TObject = Nil); virtual;
  end;

  { TEventThreadPoolTask }

  TEventThreadPoolTask = class(TThreadPoolTask)
  Private
    FEvent : TNotifyEvent;
    FSender : TObject;
  Protected
    Procedure DoExecute; override;
    Property Event : TNotifyEvent Read FEvent;
    Property Sender : TObject Read FSender;
  Public
    Constructor Create(aEvent : TNotifyEvent; aSender : TObject = Nil); virtual;
  end;

  { TSimpleThreadPoolTask }

  TSimpleThreadPoolTask = class(TThreadPoolTask)
  Private
    FProcedure : TProcedure;
  Protected
    Procedure DoExecute; override;
    Property Proc : TProcedure Read FProcedure;
  Public
    Constructor Create(aProc : TProcedure); virtual;
  end;


  { TFPCustomSimpleThreadPool }

  TFPCustomSimpleThreadPool = class(TObject)
  Protected
    Type

       { TAutoCheckQueueThread }

       TAutoCheckQueueThread = Class(TThread)
       private
         FInterval: Integer;
         FPool: TFPCustomSimpleThreadPool;
       Public
         Constructor Create(aPool : TFPCustomSimpleThreadPool; aInterval : Integer);
         Procedure Execute; override;
         Property Pool : TFPCustomSimpleThreadPool Read FPool Write FPool;
         Property Interval : Integer Read FInterval;
       end;
       { TAbstractTaskThread }

       TAbstractTaskThread = class(TThread)
       private
         FTask: TThreadPoolTask;
         procedure SetTask(AValue: TThreadPoolTask);
       protected
         procedure TerminatedSet; override;
         procedure DoSetTask(AValue: TThreadPoolTask); virtual;
       Public
         Procedure FreeTask;
         Property Task : TThreadPoolTask Read FTask Write SetTask;
       end;

       { TAbstractThreadList }

       TAbstractThreadList = class(TObject)
       private
         FPool: TFPCustomSimpleThreadPool;
       public
         Constructor CreateList(aPool : TFPCustomSimpleThreadPool); virtual;
         // Return a thread ready to execute task.
         Function GetAvailableThread : TAbstractTaskThread; virtual; abstract;
         // Add thread which must execute task
         Function AddThread : TAbstractTaskThread; virtual; abstract;
         // Current thread count
         Function GetThreadCount : Word; virtual; abstract;
         // Busy thread count
         Function GetBusyThreadCount : Word; virtual; abstract;
         // Idle thread count
         Function GetIdleThreadCount : Word; virtual; abstract;
         // Terminate all treads.
         Procedure TerminateThreads; virtual; abstract;
         // Our pool
         Property Pool : TFPCustomSimpleThreadPool Read FPool;
       end;

       { TTaskThread }

       TTaskThread = Class(TAbstractTaskThread)
       Private
         FTaskEvent : TEventObject;
         FTaskDoneEvent : TNotifyEvent;
         FWaitInterval : Integer;
       Protected
         procedure TerminatedSet; override;
       Public
         Constructor create(aWaitInterval : Integer; aTaskDoneEvent,aOnTerminate : TNotifyEvent); virtual;
         Destructor Destroy; override;
         procedure DoSetTask(AValue: TThreadPoolTask); override;
         procedure Execute; override;
         Property WaitInterval : Integer Read FWaitInterval;
       end;

       { TThreadPoolList }

       TThreadPoolList = class (TAbstractThreadList)
       private
         FThreadTaskWaitInterval: Integer;
         FList : TThreadList;
         procedure TaskDone(Sender: TObject);
         Procedure ThreadTerminated(Sender : TObject);
       public
         Constructor CreateList(aPool : TFPCustomSimpleThreadPool); override;
         Destructor Destroy; override;
         Procedure TerminateThreads; override;
         Function GetAvailableThread : TAbstractTaskThread; override;
         Function AddThread : TAbstractTaskThread; override;
         Function GetThreadCount : Word; override;
         Function GetBusyThreadCount : Word; override;
         Function GetIdleThreadCount : Word; override;
         Property ThreadTaskWaitInterval : Integer Read FThreadTaskWaitInterval Write FThreadTaskWaitInterval;
       end;

  private
    class var _Instance : TFPCustomSimpleThreadPool;
    class var _DefaultInstanceClass : TFPCustomSimpleThreadPoolClass;
  private
    FAutoCheckQueuedInterval: Integer;
    FMaxThreads: Word;
    FMinThreads: Word;
    FAddTimeout: Cardinal;
    FQueueTasks: Boolean;
    FWaitQueueLock : TCriticalSection;
    FTaskQueueLock : TCriticalSection;
    FTaskList : TAbstractThreadList;
    FWaitQueue : TThreadList;
    FAutoCheckQueueThread : TAutoCheckQueueThread;
    FThreadDoneEvent : TEventObject;
    class function GetInstance: TFPCustomSimpleThreadPool; static;
    procedure SetAutoCheckQueuedInterval(AValue: Integer);
    class procedure SetDefaultInstanceClass(AValue: TFPCustomSimpleThreadPoolClass); static;
    procedure SetMaxThreads(AValue: Word);
    procedure SetMinThreads(AValue: Word);
    // Number of busy threads
    Function GetBusyThreadCount : Word; virtual;
    // Number of Idle threads
    Function GetIdleThreadCount : Word;
    // Number of threads
    Function GetThreadCount : Word;
  Protected
    // Create thread to check queue
    function CreateAutoCheckQueueThread: TAutoCheckQueueThread;
    // Check wait list, see if task can be transferred to tasklist
    Procedure DoCheckQueuedTasks; virtual;
    // Add task to wait list.
    Function AddTaskToQueue(aTask: TThreadPoolTask) : Boolean; virtual;
    // Cancel tasks in the wait queue
    Procedure DoCancelQueuedTasks;
    // Terminate running tasks. If DoWait is true, wait till the task queue is empty.
    Procedure DoTerminateRunningTasks(DoWait : Boolean);
    // Create the list of threads.
    function CreateThreadList : TAbstractThreadList; virtual;
    // Actually add a task.
    Function DoAddTask (aTask : TThreadPoolTask) : Boolean; virtual;
    // Min number of threads
    Property MinThreads : Word Read FMinThreads Write SetMinThreads;
    // Max number of threads
    Property MaxThreads : Word Read FMaxThreads Write SetMaxThreads;
    // when QueueTasks is False, Timeout in milliseconds to wait for thread to become available. Set to Zero to not wait.
    Property AddTimeout : Cardinal Read FAddTimeout Write FAddTimeout;
    // Set QueueTasks to add the tasks to a queue if they cannot be executed within the AddTimeout interval
    Property QueueTasks : Boolean Read FQueueTasks Write FQueueTasks;
    // Number of busy threads
    Property BusyThreadCount : Word Read GetBusyThreadCount;
    // Number of Idle threads
    Property IdleThreadCount : Word Read GetIdleThreadCount;
    // Number of threads
    Property ThreadCount : Word Read GetThreadCount;
    // Set to a value>0  to start a thread that runs the CheckQueuedTasks every AutoCheckQueuedInterval ms.
    Property AutoCheckQueuedInterval : Integer Read FAutoCheckQueuedInterval Write SetAutoCheckQueuedInterval;
  Public
    constructor Create; virtual;
    destructor destroy; override;
    class constructor InitClass;
    class destructor DoneClass;
    // This needs to be called on regular basis to check if queued tasks can be executed.
    procedure CheckQueuedTasks;
    // Cancel queued tasks
    Procedure CancelQueuedTasks;
    // Terminate running tasks
    Procedure TerminateRunningTasks;
    // Will return true if the task was executed or put in queue.
    // If False is returned, you must free the task. If true is returned, the thread pool will free the task.
    Function AddTask (aTask : TThreadPoolTask) : Boolean; overload;
    Function AddTask (aCallBack : TTaskCallBack; aData : TObject = Nil) : Boolean; overload;
    Function AddTask (aEvent : TNotifyEvent; aData : TObject = Nil): Boolean; overload;
    class property DefaultInstanceClass : TFPCustomSimpleThreadPoolClass Read _DefaultInstanceClass Write SetDefaultInstanceClass;
    class property Instance : TFPCustomSimpleThreadPool read GetInstance;
  end;

  TFPSimpleThreadPool = class(TFPCustomSimpleThreadPool)
  Public
    Property MinThreads;
    Property MaxThreads;
    Property AddTimeout;
    Property QueueTasks;
    Property BusyThreadCount;
    Property IdleThreadCount;
    Property ThreadCount;
    Property AutoCheckQueuedInterval;
  end;

Implementation

Resourcestring
  SErrMinLargerThanMax = 'MinThreads (%d) must be less than MaxThreads (%d)';
  SErrMaxLessThanMin = 'MaxThreads (%d) must be greater than MinThreads (%d)';
  SErrInstanceAlreadyCreated = 'Thread pool instance already created';
  SErrTaskAlreadySet = 'Cannot set task: task already set';

{$IFDEF DEBUGTHREADPOOL}
Procedure DoLog(Const Msg : String);

begin
  Writeln(Output,Msg);
  Flush(Output);
end;

Procedure DoLog(Const Fmt : String; Const Args : Array of const);
begin
  DoLog(Format(Fmt,Args))
end;

{ TFPCustomSimpleThreadPool.TAbstractThreadList }

constructor TFPCustomSimpleThreadPool.TAbstractThreadList.CreateList(aPool: TFPCustomSimpleThreadPool);
begin
  FPool:=aPool;
end;

{$ENDIF}

{ TFPCustomSimpleThreadPool.TAutoCheckQueueThread }

constructor TFPCustomSimpleThreadPool.TAutoCheckQueueThread.Create(aPool: TFPCustomSimpleThreadPool; aInterval: Integer);
begin
  FPool:=aPool;
  FInterval:=aInterval;
  FreeOnTerminate:=True;
  Inherited Create(False);
end;

procedure TFPCustomSimpleThreadPool.TAutoCheckQueueThread.Execute;
begin
  While not Terminated do
    begin
    Sleep(FInterval);
    If Assigned(FPool) then
      FPool.CheckQueuedTasks;
    end;
end;


{ TSimpleThreadPoolTask }

procedure TSimpleThreadPoolTask.DoExecute;
begin
  FProcedure;
end;

constructor TSimpleThreadPoolTask.Create(aProc: TProcedure);
begin
  FProcedure:=aProc;
end;

{ TEventThreadPoolTask }

procedure TEventThreadPoolTask.DoExecute;

begin
  FEvent(FSender);
end;

constructor TEventThreadPoolTask.Create(aEvent: TNotifyEvent; aSender: TObject = Nil);
begin
  FSender:=aSender;
  FEvent:=aEvent;
end;

{ TCallBackThreadPoolTask }

procedure TCallBackThreadPoolTask.DoExecute;
begin
  FCallBack(FData);
end;

constructor TCallBackThreadPoolTask.Create(aCallback: TTaskCallBack; aData: TObject);
begin
  FCallBack:=aCallBack;
  FData:=aData;
end;

{ TFPCustomSimpleThreadPool.TTaskThread }

procedure TFPCustomSimpleThreadPool.TTaskThread.TerminatedSet;
begin
  FTaskEvent.SetEvent;
  inherited TerminatedSet;
end;

constructor TFPCustomSimpleThreadPool.TTaskThread.create(aWaitInterval : Integer; aTaskDoneEvent,aOnTerminate : TNotifyEvent);
begin
  FTaskEvent:=TEventObject.Create(Nil,False,False,'');
  FTaskDoneEvent:=aTaskDoneEvent;
  FWaitInterval:=aWaitInterval;
  OnTerminate:=aOnTerminate;
  inherited create(False);
end;

destructor TFPCustomSimpleThreadPool.TTaskThread.Destroy;
begin
  FreeAndNil(FTaskEvent);
  inherited Destroy;
end;

procedure TFPCustomSimpleThreadPool.TTaskThread.DoSetTask(AValue: TThreadPoolTask);
begin
  inherited DoSetTask(AValue);
  FTaskEvent.SetEvent;
end;

procedure TFPCustomSimpleThreadPool.TTaskThread.Execute;
begin
  While Not Terminated do
    begin
    if (FTaskEvent.WaitFor(FWaitInterval)=wrSignaled) then
      begin
      FTaskEvent.ResetEvent;
      // Task can be nil,
      If Assigned(Task) then
        try
          Task.Execute;
        finally
          FreeTask;
          if Assigned(FTaskDoneEvent) then
            FTaskDoneEvent(Self);
        end;
      end;
    end;
end;

{ TFPCustomSimpleThreadPool.TThreadPoolList }

procedure TFPCustomSimpleThreadPool.TThreadPoolList.ThreadTerminated(Sender: TObject);
begin
  FList.Remove(Sender);
end;

procedure TFPCustomSimpleThreadPool.TThreadPoolList.TaskDone(Sender: TObject);
begin
  If Assigned(Pool) and Assigned(Pool.FThreadDoneEvent) then
    Pool.FThreadDoneEvent.SetEvent;

end;

constructor TFPCustomSimpleThreadPool.TThreadPoolList.CreateList(aPool : TFPCustomSimpleThreadPool);
begin
  Inherited;
  FList:=TThreadList.Create;
end;

destructor TFPCustomSimpleThreadPool.TThreadPoolList.Destroy;
begin
  FreeAndNil(FList);
  Inherited;
end;

function TFPCustomSimpleThreadPool.TThreadPoolList.GetAvailableThread: TAbstractTaskThread;

Var
  L : TList;
  I : Integer;

begin
  Result:=Nil;
  L:=FList.LockList;
  try
    For I:=0 to L.Count-1 do
      If TAbstractTaskThread(L[i]).Task=Nil then
        Result:=TAbstractTaskThread(L[i]);
  finally
    FList.UnlockList;
  end;
end;

function TFPCustomSimpleThreadPool.TThreadPoolList.AddThread: TAbstractTaskThread;
begin
  Result:=TTaskThread.Create(FThreadTaskWaitInterval,@TaskDone,@ThreadTerminated);
  FList.Add(Result);
end;

procedure TFPCustomSimpleThreadPool.TThreadPoolList.TerminateThreads;
Var
  L : TList;
  I : Integer;

begin
  L:=FList.LockList;
  try
    For I:=0 to L.Count-1 do
      TThread(L[i]).Terminate;
  finally
    FList.UnlockList;
  end;
end;

function TFPCustomSimpleThreadPool.TThreadPoolList.GetThreadCount: Word;

Var
  L : TList;

begin
  L:=FList.LockList;
  try
    Result:=L.Count;
  finally
    FList.UnlockList;
  end;
end;

function TFPCustomSimpleThreadPool.TThreadPoolList.GetBusyThreadCount: Word;

Var
  L : TList;
  I : Integer;

begin
  Result:=0;
  L:=FList.LockList;
  try
    For I:=0 to L.Count-1 do
      if Assigned(TAbstractTaskThread(L[i]).Task) then
        Inc(Result);
  finally
    FList.UnlockList;
  end;
end;

function TFPCustomSimpleThreadPool.TThreadPoolList.GetIdleThreadCount: Word;

Var
  L : TList;
  I : Integer;

begin
  Result:=0;
  L:=FList.LockList;
  try
    For I:=0 to L.Count-1 do
      if Not Assigned(TAbstractTaskThread(L[i]).Task) then
        Inc(Result);
  finally
    FList.UnlockList;
  end;
end;

{ TFPCustomSimpleThreadPool.TAbstractTaskThread }

procedure TFPCustomSimpleThreadPool.TAbstractTaskThread.SetTask(AValue: TThreadPoolTask);
begin
  if FTask=AValue then Exit;
  if (FTask<>Nil) and (aValue=Nil) then
     Raise EThreadPool.Create(SErrTaskAlreadySet);
  DoSetTask(aValue);
end;

procedure TFPCustomSimpleThreadPool.TAbstractTaskThread.DoSetTask(AValue: TThreadPoolTask);
begin
  FTask:=AValue;
end;

procedure TFPCustomSimpleThreadPool.TAbstractTaskThread.FreeTask;
begin
  FreeAndNil(FTask);
end;

procedure TFPCustomSimpleThreadPool.TAbstractTaskThread.TerminatedSet;
begin
  if Assigned(FTask) then
    FTask.Terminate;
  inherited TerminatedSet;
end;

{ TThreadPoolTask }

procedure TThreadPoolTask.DoDone;
begin
  if Assigned(FonDone) then
    FOnDone(Self);
end;

procedure TThreadPoolTask.DoOnException(E: Exception);
begin
  if Assigned(FOnException) then
    FOnException(Self,E);
end;

procedure TThreadPoolTask.DoTerminate;
begin
  if Assigned(FOnTerminate) then
    FOnTerminate(Self);
end;

procedure TThreadPoolTask.DoQueued;
begin
  If Assigned(FOnQueued) then
    FOnQueued(Self);
end;

procedure TThreadPoolTask.DoCancel;
begin
  If Assigned(FOnCancel) then
    FOnCancel(Self);
end;

procedure TThreadPoolTask.Queued;
begin
  DoQueued;
end;

procedure TThreadPoolTask.Execute;

Var
  RunOK : Boolean;
  S : String;

begin
  RunOK:=False;
  Try
    DoExecute;
    RunOK:=True;
  Except
    On E : exception do
      DoOnException(E);
  end;
  {$IFDEF DEBUGTHREADPOOL} DoLog('Done '+Self.ToString);{$ENDIF}
  if (DoneOnException Or RunOK) then
    DoDone;
end;

procedure TThreadPoolTask.Terminate;
begin
  FTerminated:=True;
  DoTerminate;
end;

procedure TThreadPoolTask.Cancel;
begin
  DoCancel;
end;

{ TFPCustomSimpleThreadPool }

class function TFPCustomSimpleThreadPool.GetInstance: TFPCustomSimpleThreadPool; static;
begin
  if _instance=nil then
    _instance:=_DefaultInstanceClass.Create;
  Result:=_Instance;
end;

Function TFPCustomSimpleThreadPool.CreateAutoCheckQueueThread :TAutoCheckQueueThread;
begin
  Result:=TAutoCheckQueueThread.Create(Self,AutoCheckQueuedInterval);
end;

procedure TFPCustomSimpleThreadPool.SetAutoCheckQueuedInterval(AValue: Integer);
begin
  FWaitQueueLock.Enter;
  try
    if FAutoCheckQueuedInterval=AValue then Exit;
    FAutoCheckQueuedInterval:=AValue;
    if FAutoCheckQueuedInterval=0 then
      begin
      if Assigned(FAutoCheckQueueThread) then
        begin
        FAutoCheckQueueThread.Pool:=nil;
        FAutoCheckQueueThread.Terminate;
        FAutoCheckQueueThread:=Nil;
        end;
      end
    else
      begin
      if Not Assigned(FAutoCheckQueueThread) then
        FAutoCheckQueueThread:=CreateAutoCheckQueueThread;
      end;
  finally
    FWaitQueueLock.Leave;
  end;
end;

class procedure TFPCustomSimpleThreadPool.SetDefaultInstanceClass(AValue: TFPCustomSimpleThreadPoolClass);
begin
  if _DefaultInstanceClass=AValue then Exit;
  if _Instance<>Nil then
    Raise EThreadPool.Create(SErrInstanceAlreadyCreated);
  _DefaultInstanceClass:=AValue;
end;

procedure TFPCustomSimpleThreadPool.SetMaxThreads(AValue: Word);
begin
  if FMaxThreads=AValue then Exit;
  if aValue<FMinThreads then
      Raise EThreadPool.CreateFmt(SErrMaxLessThanMin,[aValue,MinThreads]);
    FMaxThreads:=AValue;
end;

procedure TFPCustomSimpleThreadPool.SetMinThreads(AValue: Word);
begin
  if FMinThreads=AValue then Exit;
  if (FMaxThreads>0) and (aValue>FMaxThreads) then
      Raise EThreadPool.CreateFmt(SErrMinLargerThanMax,[aValue,MaxThreads]);
  FMinThreads:=AValue;
end;

function TFPCustomSimpleThreadPool.GetBusyThreadCount: Word;
begin
  Result:=FTaskList.GetBusyThreadCount;
end;

function TFPCustomSimpleThreadPool.GetIdleThreadCount: Word;
begin
  Result:=FTaskList.GetIdleThreadCount;
end;

function TFPCustomSimpleThreadPool.GetThreadCount: Word;
begin
  Result:=FTaskList.GetThreadCount;
end;

procedure TFPCustomSimpleThreadPool.DoCheckQueuedTasks;

Var
  L : TList;

begin
  L:=nil;
  FWaitQueueLock.Enter;
  try
    L:=FWaitQueue.LockList;
    While (L.Count>0) and DoAddTask(TThreadPoolTask(L[L.Count-1])) do
      L.Delete(L.Count-1);
  finally
    if Assigned(L) then
      FWaitQueue.UnlockList;
    FWaitQueueLock.Leave;
  end;
end;

function TFPCustomSimpleThreadPool.AddTaskToQueue(aTask: TThreadPoolTask): Boolean;
begin
  {$IFDEF DEBUGTHREADPOOL} DoLog('Adding task '+aTask.ToString+' to queue');{$ENDIF}
  FWaitQueueLock.Enter;
  try
    FWaitQueue.Add(aTask);
    aTask.Queued;
    Result:=True;
  finally
    FWaitQueueLock.Leave;
  end;
end;

procedure TFPCustomSimpleThreadPool.DoCancelQueuedTasks;

Var
  L : TList;
  aTask : TThreadPoolTask;
begin
  L:=Nil;
  FWaitQueueLock.Enter;
  try
    L:=FWaitQueue.LockList;
    While (L.Count>0)  do
      begin
      aTask:=TThreadPoolTask(L[L.Count-1]);
      L.Delete(L.Count-1);
      aTask.Cancel;
      aTask.Free;
      end;
  finally
    if Assigned(L) then
      FWaitQueue.UnlockList;
    FWaitQueueLock.Leave;
  end;
end;

procedure TFPCustomSimpleThreadPool.DoTerminateRunningTasks(DoWait: Boolean);
begin
  {$IFDEF DEBUGTHREADPOOL}DoLog('Terminating all threads');{$ENDIF}
  FTaskList.TerminateThreads;
  {$IFDEF DEBUGTHREADPOOL}DoLog('Terminated all threads, wait: '+BoolToStr(DoWait,True));{$ENDIF}
  if DoWait then
    begin
    While FTaskList.GetBusyThreadCount>0 do
      begin
      {$IFDEF DEBUGTHREADPOOL}DoLog('Not all threads terminated, wait: %d',[DefaultTerminateWait]);{$ENDIF}
      Sleep(DefaultTerminateWait);
      end;
    end;
end;

function TFPCustomSimpleThreadPool.CreateThreadList: TAbstractThreadList;
begin
  Result:=TThreadPoolList.CreateList(Self);
end;

function TFPCustomSimpleThreadPool.DoAddTask(aTask: TThreadPoolTask) : Boolean;

Var
  T : TAbstractTaskThread;
  WaitStart : TDateTime;
  TimeOut : Boolean;

begin
  WaitStart:=0;
  Result:=False;
  TimeOut:=False;
  Repeat
    FTaskQueueLock.Enter;
    try
      T:=FTaskList.GetAvailableThread;
      if (T=Nil) and (FTasklist.GetThreadCount<MaxThreads) then
        T:=FTasklist.AddThread;
    finally
      FTaskQueueLock.Leave;
    end;
    Result:=T<>Nil;
    if Result then
      T.Task:=aTask
    else if (Not QueueTasks) and (FAddTimeout>0) then
      begin
      {$IFDEF DEBUGTHREADPOOL}DoLog('No available thread for task %s waiting %d ms',[aTask.ToString,FAddTimeOut]);{$ENDIF}
      FThreadDoneEvent.ResetEvent;
      Timeout:=FThreadDoneEvent.WaitFor(FAddTimeout)<>wrSignaled;
      {$IFDEF DEBUGTHREADPOOL}
      If TimeOut then
        DoLog('TimeOut reached: '+BoolToStr(TimeOut,True));
      {$ENDIF}
      end;
  Until Result or TimeOut;
end;

constructor TFPCustomSimpleThreadPool.Create;
begin
  FAddTimeout:=DefaultAddTimeout;
  FWaitQueueLock:=TCriticalSection.Create;
  FTaskQueueLock:=TCriticalSection.Create;
  FWaitQueue:=TThreadList.Create;
  FTaskList:=CreateThreadList;
  MaxThreads:=TThread.ProcessorCount;
  MinThreads:=TThread.ProcessorCount-1;
  FQueueTasks:=DefaultQueueTasks;
  FThreadDoneEvent:=TEventObject.Create(Nil,False,False,'');
end;

destructor TFPCustomSimpleThreadPool.destroy;
begin
  // Disable the queue
  AutoCheckQueuedInterval:=0;
  {$IFDEF DEBUGTHREADPOOL}DoLog('Destroy : Cancelqueuedtasks');{$ENDIF}
  CancelQueuedTasks;
  {$IFDEF DEBUGTHREADPOOL}DoLog('Destroy : TerminateRunningTasks');{$ENDIF}
  TerminateRunningTasks;
  FreeAndNil(FWaitQueue);
  FreeAndNil(FTaskList);
  FreeAndNil(FWaitQueueLock);
  FreeAndNil(FTaskQueueLock);
  FreeAndNil(FThreadDoneEvent);
  inherited destroy;
end;

class constructor TFPCustomSimpleThreadPool.InitClass;
begin
  _DefaultInstanceClass:=TFPSimpleThreadPool;
  _Instance:=Nil;
end;

class destructor TFPCustomSimpleThreadPool.DoneClass;
begin
  FreeAndNil(_Instance);
end;

procedure TFPCustomSimpleThreadPool.CheckQueuedTasks;
begin
  DoCheckQueuedTasks;
end;

procedure TFPCustomSimpleThreadPool.CancelQueuedTasks;
begin
  DoCancelQueuedTasks;
end;

procedure TFPCustomSimpleThreadPool.TerminateRunningTasks;
begin
  DoTerminateRunningTasks(True);
end;

function TFPCustomSimpleThreadPool.AddTask(aTask: TThreadPoolTask): Boolean;
begin
  While ThreadCount<MinThreads do
    FTaskList.AddThread;
  CheckQueuedTasks;
  Result:=DoAddTask(aTask);
  if (not Result) and QueueTasks then
    Result:=AddTaskToQueue(aTask);
end;

function TFPCustomSimpleThreadPool.AddTask(aCallBack: TTaskCallBack; aData: TObject): Boolean;

Var
  T : TThreadPoolTask;

begin
  T:=TCallBackThreadPoolTask.Create(aCallBack,aData);
  Result:=AddTask(T);
  if not Result then
    T.Free;
end;

function TFPCustomSimpleThreadPool.AddTask(aEvent: TNotifyEvent; aData: TObject): Boolean;
Var
  T : TThreadPoolTask;

begin
  T:=TEventThreadPoolTask.Create(aEvent,aData);
  Result:=AddTask(T);
  if not Result then
    T.Free;
end;

end.

