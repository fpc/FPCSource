{$IFNDEF FPC_DOTTEDUNITS}
unit fpmonitor;
{$ENDIF}

{$mode objfpc}
{$modeswitch advancedrecords}

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

  TPulseData = record
    Event : PEventState;
    ThreadID : TThreadID;
    Class Function Create : TPulseData; static;
    Procedure Done;
    function Wait(aTimeout : Cardinal) : boolean;
    function Match(aPulse : TPulseData) : boolean;
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
    PulseData : TPulseDataArray;
    PulseCount : Integer;
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
    procedure AddToPulseData(aPulse: TPulseData);
    function PopPulseData(out aData: TPulseData): Boolean;
    procedure RemoveFromPulseData(aPulse: TPulseData);
    procedure ShiftPulseData(Idx: Integer);
    procedure CheckLockOwner;
  end;

{ TPulseData }

class function TPulseData.Create: TPulseData;
begin
  Result.ThreadID:=GetCurrentThreadId;
  Result.Event:=RTLEventCreate;
  if (Result.Event=Nil) then
    Raise EMonitor.Create('Could not create event');
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

function TPulseData.Match(aPulse: TPulseData): boolean;
begin
  Result:=(aPulse.ThreadID=Self.ThreadID) and (aPulse.Event=Self.Event);
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Match ',aPulse.ThreadID,'=',Self.ThreadID,') and (',Ptrint(aPulse.Event),'=',Ptrint(Self.Event),' : ',Result);{$ENDIF}
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
  I : integer;
  TID : TThreadID;
  IsOwner : Boolean;

begin
  TID:=GetCurrentThreadId;
  IsOwner:=(TID=LockOwnerThreadID);
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Begin enter. Is Owner: ',IsOwner);{$ENDIF}
  if IsOwner then
    begin
    I:=AtomicIncrement(LockCount);
    if I<>1 then
      begin
      {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Recursive enter detected');{$ENDIF}
      exit;
      end;
    end;
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Entering critical section');{$ENDIF}
  EnterCriticalSection(CriticalSection);
  LockCount:=1;
  LockOwnerThreadID:=TID;
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Entered critical section');{$ENDIF}
end;

function TMonitorData.TryEnter: Boolean;
var
  TID : TThreadID;
begin
  TID:=GetCurrentThreadId;
  Result:=TID=LockOwnerThreadID;
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Begin TryEnter. Is Owner: ',Result);{$ENDIF}
  if Not Result then
    begin
    Result:=TryEnterCriticalSection(CriticalSection)<>0;
    if Result then
      begin
      LockOwnerThreadID:=TID;
      LockCount:=1;
      end;
    end;
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' End TryEnter. Result: ',Result);{$ENDIF}
end;

function TMonitorData.Enter(aTimeout: Cardinal): Boolean;

var
  Start : Int64;

begin
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Begin Enter(',aTimeout,')');{$ENDIF}
  Start:=GetTickCount64;
  Repeat
     Result:=TryEnter;
     if not Result then
       Sleep(2);
  until Result or ((GetTickCount64-Start)>aTimeout);
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' End Enter(',aTimeout,'), Result: ',Result);{$ENDIF}
end;

procedure TMonitorData.ShiftPulseData(Idx: Integer);
// Only call this inside EnterPulse/Leavepulse !

begin
  // Not sure if we need to preserve order. Better assume yes.
  // shift items. If need be, this can be done with a move.
  While Idx<PulseCount-1 do
    begin
    PulseData[Idx]:=PulseData[Idx+1];
    Inc(Idx);
    end;
  Dec(PulseCount);
end;

procedure TMonitorData.CheckLockOwner;
begin
  if LockOwnerThreadID<>GetCurrentThreadId then
    Raise EMonitor.CreateFmt('Lock not owned by this thread %d <> %d',[LockOwnerThreadID,GetCurrentThreadId]);
end;

function TMonitorData.PopPulseData (out aData : TPulseData) : Boolean;

begin
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Begin PopPulseData');{$ENDIF}
  EnterPulse;
  try
   Result:=PulseCount>0;
   if Result then
     begin
     aData:=PulseData[0];
     ShiftPulseData(0);
     end;
  finally
    LeavePulse;
  end;
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' End PopPulseData Result: ',Result,', left:',PulseCount);{$ENDIF}
end;

procedure TMonitorData.RemoveFromPulseData(aPulse: TPulseData);

var
  Idx : integer;

begin
  EnterPulse;
  try
    {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Begin RemoveFromPulseData (Thread: ',aPulse.ThreadID,', count: ',PulseCount,')');{$ENDIF}
    // Find index
    Idx:=PulseCount-1;
    While (Idx>=0) and Not PulseData[Idx].Match(aPulse) do
      Dec(Idx);
    if Idx<0 then
      begin
      {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' end RemoveFromPulseData. Count: ',PulseCount);{$ENDIF}
      exit;
      end;
    PulseData[Idx].Done;
    ShiftPulseData(Idx);
  finally
    LeavePulse;
  end;
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' end RemoveFromPulseData. Count: ',PulseCount);{$ENDIF}
end;

procedure TMonitorData.AddToPulseData(aPulse: TPulseData);

var
  Len : integer;

begin
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Begin AddToPulseData (Thread: ',aPulse.ThreadID,')');{$ENDIF}
  EnterPulse;
  try
    Len:=Length(PulseData);
    If PulseCount=Len then
      SetLength(PulseData,Len+10);
    PulseData[PulseCount]:=aPulse;
    Inc(PulseCount);
  finally
    LeavePulse;
  end;
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' End AddToPulseData. Count: ',PulseCount);{$ENDIF}
end;

function TMonitorData.Wait(aLock : PMonitorData; aTimeout: Cardinal): Boolean;

var
  aPulse : TPulseData;

begin
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Begin Wait (aTimeout: ',aTimeOut,')');{$ENDIF}
  aLock^.CheckLockOwner;
  aPulse:=TPulseData.Create;
  AddToPulseData(aPulse);
  aLock^.Leave;
  Result:=aPulse.Wait(aTimeOut);
  Sleep(20);
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Wait Removing from Pulse data');{$ENDIF}
  RemoveFromPulseData(aPulse);
  aLock^.Enter;
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' End Wait (aTimeout: ',aTimeOut,')');{$ENDIF}
end;

procedure TMonitorData.Leave;

var
  I : integer;

begin
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Begin Leave. Is owner: ',GetCurrentThreadID=LockOwnerThreadID);{$ENDIF}
  CheckLockOwner;
  I:=AtomicDecrement(LockCount);
  {$IFDEF DEBUG_MONITOR}if I>0 then Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Owner holds recursive lock: ',I);{$ENDIF}
  if I<>0 then
    Exit;
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Leaving critical section');{$ENDIF}
  LockOwnerThreadID:=TThreadID(0);
  LockCount:=0;
  LeaveCriticalSection(CriticalSection);
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' End Leave. Is owner: ',GetCurrentThreadID=LockOwnerThreadID);{$ENDIF}
end;

procedure TMonitorData.Init;
begin
  LockCount:=0;
  LockOwnerThreadID:= TThreadID(0);
  InitCriticalSection(CriticalSection);
  InitCriticalSection(PulseLock);
  PulseCount:=0;
end;

procedure TMonitorData.Done;

var
  I : integer;

begin
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Begin Done monitor data');{$ENDIF}
  // We don't lock as normally this is only called when object is destroyed.
  For I:=0 to PulseCount-1 do
    PulseData[i].Done;
  SetLength(PulseData,0);
  DoneCriticalSection(PulseLock);
  DoneCriticalSection(CriticalSection);
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' End Done monitor data');{$ENDIF}
end;

procedure TMonitorData.Pulse;

var
  aPulse : TPulseData;
  HavePulse: Boolean;
begin
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Begin Pulse');{$ENDIF}
  HavePulse:=PopPulseData(aPulse);
  if HavePulse then
    aPulse.Pulse;
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' End Pulse (had pulse: ',HavePulse,')');{$ENDIF}
end;

procedure TMonitorData.PulseAll;

var
  aPulse : TPulseData;
  aCount : Integer;
begin
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' Begin PulseAll');{$ENDIF}
  aCount:=0;
  While PopPulseData(aPulse) do
    begin
    aPulse.Pulse;
    Inc(aCount);
    end;
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadId,' End PulseAll (Pulse count: ',aCount,')');{$ENDIF}
end;


var
  _oldMonitor,
  _monitor : TMonitorManager;
  _DummySpinCount : Integer;
  _MemLock : TRTLCriticalSection;

function GetMonitorData(aObject : TObject) : PMonitorData; inline;

begin
  Result:=PMonitorData(_monitor.DoGetMonitorObjectData(aObject));
end;

procedure SetMonitorData(aObject : TObject; aData : PMonitorData); inline;

begin
  _monitor.DoSetMonitorObjectData(aObject,aData);
end;

function SyncEnsureData(aObject : TObject) : PMonitorData;

begin
  EnterCriticalSection(_MemLock);
  try
    Result:=GetMonitorData(aObject);
    if Result=Nil then
      begin
      // At some point we could cache this.
      GetMem(Result,SizeOf(TMonitorData));
      Result^:=Default(TMonitorData);
      Result^.Init;
      SetMonitorData(aObject,Result);
      end;
  finally
    LeaveCriticalSection(_MemLock);
  end;
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
  InitCriticalSection(_MemLock);
  InitMonitorSupport;
  _OldMonitor:=SetMonitorManager(_Monitor);
end;

procedure UnRegisterMonitorSupport;

begin
  DoneCriticalSection(_MemLock);
  SetMonitorManager(_oldMonitor);
end;

Initialization
  RegisterMonitorSupport;
finalization
  UnRegisterMonitorSupport;

end.
