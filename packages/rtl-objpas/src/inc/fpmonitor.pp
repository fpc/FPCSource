{$IFNDEF FPC_DOTTEDUNITS}
unit fpmonitor;
{$ENDIF}

{$mode objfpc}
{$modeswitch advancedrecords}
{$modeswitch anonymousfunctions}

{ $DEFINE MONITOR_STATS}
{ $DEFINE DEBUG_MONITOR}

interface

{$IFDEF MONITOR_STATS}
Type
  TMonitorCall = (mcEnter,mcExit,mcPulse,mcPulseAll,mcEnterTimeout,mcTryEnter,mcWait,mcSetDefaultSpinCount, mcGetDefaultSpinCount, mcWaitLock,mcFreeData);

  TMonitorCallStat = Record
    CallCount : Integer;
    LastObject : TObject;
  end;

  TMonitorCallStatsArray = Array[TMonitorCall] of TMonitorCallStat;
  TCallStats = record
    Stats : TMonitorCallStatsArray;
    Procedure Clear;
  end;

Procedure GetStats(out aStats : TMonitorCallStatsArray);
Procedure ClearStats;
{$ENDIF}

procedure RegisterMonitorSupport;
procedure UnRegisterMonitorSupport;

// Helpers shared with fpwinmonitor.
type
  TTryEnterProc = function(param : pointer) : boolean;

procedure ThrowNotOwnedBy(aThread : TThreadID);
function EmulateEnterTimeout(aTimeout : Cardinal; aTryEnter : TTryEnterProc; aParam : pointer) : boolean;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils;
{$ELSE}
uses sysutils;
{$ENDIF}


Type
  EMonitor = Class(Exception);

{$IFDEF MONITOR_STATS}

var
  _Stats : TCallStats;


Procedure TCallStats.Clear;

begin
  Stats:=Default(TMonitorCallStatsArray);
end;

Procedure SyncStat(aCall : TMonitorCall; aObject : TObject);

begin
  inc(_stats.Stats[aCall].CallCount);
  _stats.Stats[aCall].LastObject:=aObject;
end;

Procedure GetStats(out aStats : TMonitorCallStatsArray);

begin
  aStats:=_Stats.Stats;
end;

Procedure ClearStats;

begin
  _stats.Clear;
end;

{$ENDIF MONITOR_STATS}

Type

  { TPulseData }
  PPulseData = ^TPulseData;
  TPulseData = record
    Event : PEventState;
    Next,Prev : PPulseData; // Next is set to nil when PPulseData is not in the list, allowing to check this fact as “(PD^.Next=nil) and (PD<>PulseTail)”.
{$ifdef DEBUG_MONITOR}
    ThreadID : TThreadID;
{$endif}
    Class Function Create : TPulseData; static;
    Procedure Done;
    function Wait(aTimeout : Cardinal) : boolean;
    Procedure Pulse;
  end;
  TPulseDataArray = Array of TPulseData;

  { TMonitorData }

  PMonitorData = ^TMonitorData;
  TMonitorData = Record
    LockCount: Integer;
    LockOwnerThreadID : TThreadID;
    CriticalSection : TRTLCriticalSection;
    PulseLock : TRTLCriticalSection;
    PulseHead,PulseTail : PPulseData;
    Procedure EnterPulse;
    Procedure LeavePulse;
    Procedure Enter;
    function TryEnter : Boolean;
    Function Enter(aTimeout : Cardinal) : Boolean;
    Function Wait(aLock : PMonitorData; aTimeout : Cardinal) : Boolean;
    Procedure Leave;
    procedure Init;
    Procedure Done;
    Procedure Pulse;
    Procedure PulseAll;
  private
    procedure AddToPulseData(aPulse: PPulseData);
    function UnlockedPopPulseData: PPulseData; // Pulse/PulseAll require separate locking to not race with Waits.
    procedure RemoveFromPulseData(aPulse: PPulseData);
    procedure CheckLockOwner;
  end;

{ TPulseData }

class function TPulseData.Create: TPulseData;
begin
  Result.Event:=RTLEventCreate;
  if (Result.Event=Nil) then
    Raise EMonitor.Create('Could not create event');
  Result.Next:=nil; // Support the comment on Next. Prev is set by AddToPulseData and is otherwise unimportant.
{$ifdef DEBUG_MONITOR}
  Result.ThreadID:=GetCurrentThreadId;
{$endif}
end;

procedure TPulseData.Done;
begin
  RTLEventDestroy(Event);
end;

function TPulseData.Wait(aTimeout: Cardinal): boolean;

begin
  // actually wrSignaled
  Result:=BasicEventWaitFor(aTimeout,Event,False)=0;
end;

procedure TPulseData.Pulse;
begin
  BasicEventSetEvent(Event);
end;

  { TMonitorData }

procedure TMonitorData.EnterPulse;
begin
  EnterCriticalSection(PulseLock);
end;

procedure TMonitorData.LeavePulse;
begin
  LeaveCriticalSection(PulseLock);
end;

procedure TMonitorData.Enter;
var
  TID : TThreadID;
  IsOwner : Boolean;

begin
  TID:=GetCurrentThreadId;
  IsOwner:=(TID=LockOwnerThreadID);
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Begin enter. Is Owner: ',IsOwner);{$ENDIF}
  if IsOwner then
    begin
    {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Recursive enter detected');{$ENDIF}
    end
  else
    begin
    {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Entering critical section');{$ENDIF}
    EnterCriticalSection(CriticalSection);
    LockOwnerThreadID:=TID;
    {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Entered critical section');{$ENDIF}
    end;
  Inc(LockCount);
end;

function TMonitorData.TryEnter: Boolean;
var
  TID : TThreadID;
begin
  TID:=GetCurrentThreadId;
  Result:=TID=LockOwnerThreadID;
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Begin TryEnter. Is Owner: ',Result);{$ENDIF}
  if not Result then
    begin
    Result:=TryEnterCriticalSection(CriticalSection)<>0;
    if Result then
      LockOwnerThreadID:=TID;
    end;
  if Result then
    Inc(LockCount);
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' End TryEnter. Result: ',Result);{$ENDIF}
end;

function TMonitorData.Enter(aTimeout: Cardinal): Boolean;
begin
  // Should preferably use an event raised on Leave somehow.
  // And this event should preferably not exist until someone actually uses timeouted Enter, ant not be raised until there are outstanding timeouted Enters.
  // Sounds complex, so until then, spin-wait + exponentially wait.
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Begin Enter(',aTimeout,')');{$ENDIF}
  Result:=EmulateEnterTimeout(aTimeout,
    function(param : pointer) : boolean
    begin
      Result:=PMonitorData(param)^.TryEnter;
    end, @self);
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' End Enter(',aTimeout,'), Result: ',Result);{$ENDIF}
end;

procedure TMonitorData.CheckLockOwner;
begin
  if LockOwnerThreadID<>GetCurrentThreadId then
    ThrowNotOwnedBy(LockOwnerThreadID);
end;

function TMonitorData.UnlockedPopPulseData: PPulseData;

begin
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Begin UnlockedPopPulseData');{$ENDIF}
  Result:=PulseHead;
  if Result<>nil then
    begin
    PulseHead:=Result^.Next;
    if PulseHead<>nil then
      PulseHead^.Prev:=nil
    else
      PulseTail:=nil;
    Result^.Next:=nil; // Mark as removed for future RemoveFromPulseData call from Wait.
    end;
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' End UnlockedPopPulseData Result: ',HexStr(Result));{$ENDIF}
end;

procedure TMonitorData.RemoveFromPulseData(aPulse: PPulseData);

var
  Prev, Next: PPulseData;

begin
  EnterPulse;
  try
    {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Begin RemoveFromPulseData (Thread: ',aPulse^.ThreadID,')');{$ENDIF}
    Next:=aPulse^.Next;
    if (Next=nil) and (aPulse<>PulseTail) then // Already removed.
    begin
    {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Cancel RemoveFromPulseData (Thread: ',aPulse^.ThreadID,')');{$ENDIF}
    exit;
    end;
    Prev:=aPulse^.Prev;
    if Prev<>nil then
      Prev^.Next:=Next
    else
      PulseHead:=Next;
    if Next<>nil then
      Next^.Prev:=Prev
    else
      PulseTail:=Prev;
    {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' end RemoveFromPulseData.');{$ENDIF}
  finally
    LeavePulse;
  end;
end;

procedure TMonitorData.AddToPulseData(aPulse: PPulseData);

begin
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Begin AddToPulseData (Thread: ',aPulse^.ThreadID,')');{$ENDIF}
  EnterPulse; // try .. finally aren’t required as the code cannot throw, for now.
  aPulse^.Next:=nil;
  aPulse^.Prev:=PulseTail;
  if PulseTail<>nil then
    PulseTail^.Next:=aPulse
  else
    PulseHead:=aPulse;
  PulseTail:=aPulse;
  LeavePulse;
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' End AddToPulseData.');{$ENDIF}
end;

function TMonitorData.Wait(aLock : PMonitorData; aTimeout: Cardinal): Boolean;

var
  aPulse : TPulseData;
  PrevLockCount : Integer;

begin
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Begin Wait (aTimeout: ',aTimeOut,')');{$ENDIF}
  aLock^.CheckLockOwner;
  aPulse:=TPulseData.Create;
  AddToPulseData(@aPulse);

  // Forcibly unlock through any amount of recursive acquisitions!
  PrevLockCount:=aLock^.LockCount;
  aLock^.LockCount:=0;
  aLock^.LockOwnerThreadID:=TThreadID(0);
  LeaveCriticalSection(aLock^.CriticalSection);

  Result:=aPulse.Wait(aTimeOut);
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Wait Removing from Pulse data');{$ENDIF}
  RemoveFromPulseData(@aPulse);
  aPulse.Done;

  // Lock back as if nothing happened!
  EnterCriticalSection(aLock^.CriticalSection);
  aLock^.LockOwnerThreadID:=GetCurrentThreadId;
  aLock^.LockCount:=PrevLockCount;

  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' End Wait (aTimeout: ',aTimeOut,')');{$ENDIF}
end;

procedure TMonitorData.Leave;

begin
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Begin Leave. Is owner: ',GetCurrentThreadID=LockOwnerThreadID);{$ENDIF}
  CheckLockOwner;
  Dec(LockCount);
  {$IFDEF DEBUG_MONITOR}if LockCount>0 then Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Owner holds recursive lock: ',LockCount);{$ENDIF}
  if LockCount<>0 then
    Exit;
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Leaving critical section');{$ENDIF}
  LockOwnerThreadID:=TThreadID(0);
  LeaveCriticalSection(CriticalSection);
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' End Leave. Is owner: ',GetCurrentThreadID=LockOwnerThreadID);{$ENDIF}
end;

procedure TMonitorData.Init;
begin
  LockCount:=0;
  LockOwnerThreadID:= TThreadID(0);
  InitCriticalSection(CriticalSection);
  InitCriticalSection(PulseLock);
  PulseHead:=nil;
  PulseTail:=nil;
end;

procedure TMonitorData.Done;

begin
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Begin Done monitor data');{$ENDIF}
  DoneCriticalSection(PulseLock);
  DoneCriticalSection(CriticalSection);
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' End Done monitor data');{$ENDIF}
end;

procedure TMonitorData.Pulse;

var
  aPulse : PPulseData;
{$IFDEF DEBUG_MONITOR}HavePulse: Boolean;{$endif}
begin
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Begin Pulse');{$ENDIF}
  EnterPulse;
  try
    aPulse:=UnlockedPopPulseData;
{$IFDEF DEBUG_MONITOR}HavePulse:=aPulse<>nil;{$endif}
    if aPulse<>nil then
      aPulse^.Pulse; // Note this must be performed before LeavePulse, otherwise corresponding TMonitor.Wait call might destroy aPulse and quit.
  finally
    LeavePulse;
  end;
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' End Pulse (had pulse: ',HavePulse,')');{$ENDIF}
end;

procedure TMonitorData.PulseAll;

var
  aPulse : PPulseData;
{$IFDEF DEBUG_MONITOR}aCount : Integer;{$ENDIF}
begin
{$IFDEF DEBUG_MONITOR}
  Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Begin PulseAll');
  aCount:=0;
{$ENDIF}
  EnterPulse;
  try
    repeat
      aPulse:=UnlockedPopPulseData;
      if aPulse=nil then
        break;
      aPulse^.Pulse; // Note this must be performed before LeavePulse, otherwise corresponding TMonitor.Wait call might destroy aPulse and quit.
{$IFDEF DEBUG_MONITOR}Inc(aCount);{$endif}
    until false;
  finally
    LeavePulse;
  end;
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' End PulseAll (Pulse count: ',aCount,')');{$ENDIF}
end;


procedure ThrowNotOwnedBy(aThread : TThreadID);
begin
  Raise EMonitor.CreateFmt('Lock not owned by this thread %d <> %d',[aThread,GetCurrentThreadId]);
end;


function EmulateEnterTimeout(aTimeout : Cardinal; aTryEnter : TTryEnterProc; aParam : pointer) : boolean;

type
  StageEnum = (Spin, ThreadSwitch, SleepA, SleepB, SleepC, SleepD, SleepE, SleepF);

const
  SleepTime: array[SleepA .. High(StageEnum)] of uint8 = (2, 4, 8, 16, 30, 50);
  StageIterations: array[StageEnum] of uint8 = (40, 40, 8, 8, 8, 8, 8, 8);

var
  TimeA,TimeB,Elapsed : Int64;
  Stage : StageEnum;
  StageIteration,TimeToSleep : uint32;

begin
  TimeA:=-1;
  Stage:=Spin;
  Int32(StageIteration):=-1;
  Repeat
     Result:=aTryEnter(aParam);
     if Result or (aTimeout=0) then
       break;
     if TimeA=-1 then
       TimeA:=GetTickCount64; // Avoid GetTickCount64 call if first TryEnter succeeds. -1 is a possible timestamp, but nothing particularly bad will happen.
     Inc(Int32(StageIteration));
     if StageIteration>=StageIterations[Stage] then
       begin
       if Stage<High(Stage) then
         Inc(Stage);
       StageIteration:=0;
       end;
     case Stage of
       Spin: ;
       ThreadSwitch: System.ThreadSwitch;
       SleepA .. High(StageEnum):
         begin
         TimeToSleep:=SleepTime[Stage];
         if aTimeout<TimeToSleep then
           TimeToSleep:=aTimeout;
         Sleep(TimeToSleep);
         end;
     end;
     TimeB:=GetTickCount64;
     Elapsed:=TimeB-TimeA;
     TimeA:=TimeB; // Sum of Elapseds will always be exactly <current time> - <start time>.
     if Elapsed>=aTimeout then
       break;
     aTimeout:=aTimeout-Elapsed;
  until false;
end;


var
  _oldMonitor,
  _monitor : TMonitorManager;
  _DummySpinCount : Integer;

function GetMonitorData(aObject : TObject) : PMonitorData; inline;

begin
  Result:=PMonitorData(_monitor.DoGetMonitorObjectData(aObject));
end;

function SetMonitorData(aObject : TObject; aData,aComparand : PMonitorData) : PMonitorData; inline;

begin
  Result:=_monitor.DoSetMonitorObjectData(aObject,aData,aComparand);
end;

function SyncEnsureData(aObject : TObject) : PMonitorData;

begin
  repeat
    Result:=GetMonitorData(aObject);
    if Result<>Nil then
      begin
      ReadDependencyBarrier; // Read Result fields after Result pointer.
      exit;
      end;

    // At some point we could cache this.
    New(Result);
    Result^.Init;
    WriteBarrier; // Write pointer with SetMonitorData only after Result fields have been written.
    if SetMonitorData(aObject,Result,nil)=nil then
      break;
    Dispose(Result); // And retry GetMonitorData + ReadDependencyBarrier from the beginning of the loop, which will guaranteedly succeed.
  until false;
end;

procedure SyncFreeData(aData : PMonitorData);
begin
  aData^.Done;
  FreeMem(aData);
end;


procedure SyncEnter(const aObject : TObject);

begin
  {$IFDEF MONITOR_STATS}syncStat(mcEnter,aObject);{$ENDIF}
  SyncEnsureData(aObject)^.Enter;
end;

procedure syncLeave(const aObject : TObject);

begin
  {$IFDEF MONITOR_STATS}syncStat(mcExit,aObject);{$ENDIF}
  SyncEnsureData(aObject)^.Leave;
end;

procedure syncPulse(const aObject : TObject);

begin
  {$IFDEF MONITOR_STATS}syncStat(mcPulse,aObject);{$ENDIF}
  SyncEnsureData(aObject)^.Pulse;
end;

procedure syncPulseAll(const aObject : TObject);

begin
  {$IFDEF MONITOR_STATS}syncStat(mcPulseAll,aObject);{$ENDIF}
  SyncEnsureData(aObject)^.PulseAll;
end;


function syncTryEnter(const aObject : TObject) : Boolean;

begin
  {$IFDEF MONITOR_STATS}syncStat(mcTryEnter,aObject);{$ENDIF}
  Result:=SyncEnsureData(aObject)^.TryEnter;
end;

function syncEnterTimeout(const aObject : TObject; aTimeout : Cardinal) : Boolean;

begin
  Result:=False;
  {$IFDEF MONITOR_STATS}syncStat(mcEnterTimeout,aObject);{$ENDIF}
  Result:=SyncEnsureData(aObject)^.Enter(aTimeout);
end;

function syncWaitTimeout(const aObject : TObject; aTimeout : Cardinal) : Boolean;

var
  aLock : PMonitorData;

begin
  Result:=False;
  {$IFDEF MONITOR_STATS}syncStat(mcWait,aObject);{$ENDIF}
  aLock:=SyncEnsureData(aObject);
  Result:=aLock^.Wait(aLock,aTimeout);
end;

function syncDowaitLock(const aObject,aLock : TObject; aTimeout : Cardinal) : Boolean;

begin
  Result:=False;
  {$IFDEF MONITOR_STATS}syncStat(mcWaitLock,aObject);{$ENDIF}
  Result:=SyncEnsureData(aObject)^.Wait(SyncEnsureData(aLock),aTimeout);
end;

function syncGetDefaultSpinCount : Longint;
begin
  Result:=_DummySpinCount;
  {$IFDEF MONITOR_STATS}syncStat(mcGetDefaultSpinCount,Nil);{$ENDIF}
end;

procedure syncSetDefaultSpinCount(const aValue : Longint);
begin
  {$IFDEF MONITOR_STATS}syncStat(mcSetDefaultSpinCount,Nil);{$ENDIF}
  _DummySpinCount:=aValue;
end;

procedure syncFreeMonitorData(aData: Pointer);
begin
  {$IFDEF MONITOR_STATS}syncStat(mcFreeData,Nil);{$ENDIF}
  if Assigned(aData) then
    SyncFreeData(PMonitorData(aData));
end;

Procedure InitMonitorSupport;

begin
  _Monitor.DoEnter:=@syncEnter;
  _Monitor.DoExit:=@syncLeave;
  _Monitor.DoPulse:=@syncPulse;
  _Monitor.DoPulseAll:=@syncPulseAll;
  _Monitor.DoEnterTimeout:=@syncEnterTimeout;
  _Monitor.DoTryEnter:=@syncTryEnter;
  _Monitor.DoWait:=@syncWaitTimeout;
  _Monitor.DoSetDefaultSpinCount:=@syncSetDefaultSpinCount;
  _Monitor.DoGetDefaultSpinCount:=@syncGetDefaultSpinCount;
  _Monitor.DoWaitLock:=@syncDowaitLock;
  _Monitor.DoFreeMonitorData:=@syncFreeMonitorData;
end;

procedure RegisterMonitorSupport;

begin
  InitMonitorSupport;
  _OldMonitor:=SetMonitorManager(_Monitor);
end;

procedure UnRegisterMonitorSupport;

begin
  SetMonitorManager(_oldMonitor);
end;

Initialization
  RegisterMonitorSupport;
finalization
  UnRegisterMonitorSupport;

end.
