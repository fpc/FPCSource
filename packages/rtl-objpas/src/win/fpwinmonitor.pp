{$IFNDEF FPC_DOTTEDUNITS}
unit fpwinmonitor;
{$ENDIF}

{$mode objfpc}
{$modeswitch advancedrecords}
{$modeswitch anonymousfunctions}

interface

uses
{$ifdef FPC_DOTTEDUNITS}
  System.MonitorSupport
{$else}
  FPMonitor
{$endif};

procedure RegisterWindowsMonitorSupport;
procedure UnregisterWindowsMonitorSupport;

implementation

uses
{$ifdef FPC_DOTTEDUNITS}
  Winapi.Windows, System.SysUtils
{$else}
  Windows, SysUtils
{$endif};

var
  IsSet: boolean = false;
  OldMonitor, NewMonitor: TMonitorManager;

type
  WinViVer = record
    class function TryLoad(var mm: TMonitorManager): boolean; static;

  type
    SRWLOCK = ^_SRWLOCK; _SRWLOCK = record end;
    CONDITION_VARIABLE = ^_CONDITION_VARIABLE; _CONDITION_VARIABLE = record end;
  class var
    AcquireSRWLockExclusive: procedure(var lock: SRWLOCK); stdcall;
    ReleaseSRWLockExclusive: procedure(var lock: SRWLOCK); stdcall;
    TryAcquireSRWLockExclusive: function(var lock: SRWLOCK): ByteBool; stdcall;
    WakeConditionVariable: procedure(var cv: CONDITION_VARIABLE); stdcall;
    WakeAllConditionVariable: procedure(var cv: CONDITION_VARIABLE); stdcall;
    SleepConditionVariableSRW: function(var cv: CONDITION_VARIABLE; var lock: SRWLOCK; dwMilliseconds: dword; flags: ULONG): BOOL; stdcall;

  type
    PMonitorData = ^MonitorData;
    MonitorData = record
      monLock, pulseLock: SRWLOCK;
      hey: CONDITION_VARIABLE;
      owner: TThreadID;
      nEnters, nWaiters, nPulses: int32; // nWaiters and nPulses are protected by pulseLock, nPulses = -1 if PulseAll was called.
      function TryEnter: boolean;
    end;

    class function EnsureMonitorData(obj: TObject): PMonitorData; static;
    class function Wait(condm, lockm: PMonitorData; timeout: cardinal): boolean; static;

    class procedure DoEnter(const obj: TObject); static;
    class procedure DoExit(const obj: TObject); static;
    class procedure DoPulse(const obj: TObject); static;
    class procedure DoPulseAll(const obj: TObject); static;
    class function DoEnterTimeout(const obj: TObject; timeout: cardinal): boolean; static;
    class function DoTryEnter(const obj: TObject): boolean; static;
    class function DoWait(const obj: TObject; timeout: cardinal): boolean; static;
    class function DoWaitLock(const monobj, lockobj: TObject; timeout: cardinal): boolean; static;
    class procedure DoFreeMonitorData(data: pointer); static;
  end;

class function WinViVer.TryLoad(var mm: TMonitorManager): boolean;
var
  k32: HANDLE;
begin
  k32 := GetModuleHandleW('kernel32');
  CodePointer(AcquireSRWLockExclusive) := GetProcAddress(k32, 'AcquireSRWLockExclusive');
  result := Assigned(AcquireSRWLockExclusive);
  if not result then
    exit;
  CodePointer(ReleaseSRWLockExclusive)    := GetProcAddress(k32, 'ReleaseSRWLockExclusive');
  CodePointer(TryAcquireSRWLockExclusive) := GetProcAddress(k32, 'TryAcquireSRWLockExclusive');
  CodePointer(WakeConditionVariable)      := GetProcAddress(k32, 'WakeConditionVariable');
  CodePointer(WakeAllConditionVariable)   := GetProcAddress(k32, 'WakeAllConditionVariable');
  CodePointer(SleepConditionVariableSRW)  := GetProcAddress(k32, 'SleepConditionVariableSRW');

  // Keep Do(Get|Set)DefaultSpinCount.
  mm.DoEnter               := @DoEnter;
  mm.DoEnterTimeout        := @DoEnterTimeout;
  mm.DoExit                := @DoExit;
  mm.DoTryEnter            := @DoTryEnter;
  mm.DoWait                := @DoWait;
  mm.DoWaitLock            := @DoWaitLock;
  mm.DoPulse               := @DoPulse;
  mm.DoPulseAll            := @DoPulseAll;
  mm.DoFreeMonitorData     := @DoFreeMonitorData;
end;

function WinViVer.MonitorData.TryEnter: boolean;
var
  tid: TThreadID;
begin
  tid := GetCurrentThreadId;
  result := (tid = owner) or TryAcquireSRWLockExclusive(monLock);
  if result then
  begin
    owner := tid;
    inc(nEnters);
  end;
end;

class function WinViVer.EnsureMonitorData(obj: TObject): PMonitorData;
begin
  repeat
    result := NewMonitor.DoGetMonitorObjectData(obj);
    if Assigned(result) then
    begin
      ReadDependencyBarrier;
      exit;
    end;

    new(result);
    FillChar(result^, sizeof(result^), 0);
    WriteBarrier;
    if NewMonitor.DoSetMonitorObjectData(obj, result, nil) = nil then
      break;
    dispose(result);
  until false;
end;

class function WinViVer.Wait(condm, lockm: PMonitorData; timeout: cardinal): boolean;
var
  tid: TThreadID;
  timeA, timeB, elapsed: int64;
  prevNEnters: int32;
  err: dword;
  failed: boolean;
begin
  tid := GetCurrentThreadId;
  if tid <> lockm^.owner then
    ThrowNotOwnedBy(lockm^.owner);

  AcquireSRWLockExclusive(condm^.pulseLock);
  inc(condm^.nWaiters);
  ReleaseSRWLockExclusive(condm^.pulseLock);

  if timeout <> INFINITE then
    timeA := GetTickCount64;

  failed := false;
  repeat
    prevNEnters := lockm^.nEnters;
    lockm^.nEnters := 0;
    lockm^.owner := TThreadID(0);
    result := SleepConditionVariableSRW(condm^.hey, lockm^.monLock, timeout, 0);
    lockm^.owner := tid;
    lockm^.nEnters := prevNEnters;

    AcquireSRWLockExclusive(condm^.pulseLock);
    if not result then
    begin
      err := GetLastError;
      failed := err <> ERROR_TIMEOUT;
      break;
    end;
    // SleepConditionVariableSRW can wake up spuriously, that’s why time, nWaiters, and nPulses are tracked manually.
    result := condm^.nPulses <> 0;
    if result then
    begin
      if condm^.nPulses > 0 then dec(condm^.nPulses);
      break;
    end;
    ReleaseSRWLockExclusive(condm^.pulseLock);

    if timeout <> INFINITE then
    begin
      timeB := GetTickCount64;
      elapsed := timeB - timeA;
      if elapsed >= timeout then
      begin
        AcquireSRWLockExclusive(condm^.pulseLock);
        break;
      end;
      timeout := timeout - elapsed;
      timeA := timeB;
    end;
  until false;
  // At loop exit, condm^.pulseLock is held.

  dec(condm^.nWaiters);
  if condm^.nWaiters = 0 then condm^.nPulses := 0;
  ReleaseSRWLockExclusive(condm^.pulseLock);
  if failed then RaiseLastOSError(err);
end;

class procedure WinViVer.DoEnter(const obj: TObject);
var
  tid: TThreadID;
  m: PMonitorData;
begin
  m := EnsureMonitorData(obj);
  tid := GetCurrentThreadId;
  if tid <> m^.owner then
  begin
    AcquireSRWLockExclusive(m^.monLock);
    m^.owner := tid;
  end;
  inc(m^.nEnters);
end;

class procedure WinViVer.DoExit(const obj: TObject);
var
  m: PMonitorData;
begin
  m := EnsureMonitorData(obj);
  if m^.owner <> GetCurrentThreadId then
    ThrowNotOwnedBy(m^.owner);
  dec(m^.nEnters);
  if m^.nEnters = 0 then
  begin
    m^.owner := TThreadID(0);
    ReleaseSRWLockExclusive(m^.monLock);
  end;
end;

class procedure WinViVer.DoPulse(const obj: TObject);
var
  m: PMonitorData;
begin
  m := EnsureMonitorData(obj);
  AcquireSRWLockExclusive(m^.pulseLock);
  if (m^.nWaiters > 0) and (m^.nPulses >= 0) then
    inc(m^.nPulses);
  ReleaseSRWLockExclusive(m^.pulseLock);
  WakeConditionVariable(m^.hey);
end;

class procedure WinViVer.DoPulseAll(const obj: TObject);
var
  m: PMonitorData;
begin
  m := EnsureMonitorData(obj);
  AcquireSRWLockExclusive(m^.pulseLock);
  if m^.nWaiters > 0 then
    m^.nPulses := -1;
  ReleaseSRWLockExclusive(m^.pulseLock);
  WakeAllConditionVariable(m^.hey);
end;

class function WinViVer.DoEnterTimeout(const obj: TObject; timeout: cardinal): boolean;
var
  m: PMonitorData;
begin
  m := EnsureMonitorData(obj);
  result := EmulateEnterTimeout(timeout,
    function(param: pointer): boolean
    begin
      result := PMonitorData(param)^.TryEnter;
    end, m);
end;

class function WinViVer.DoTryEnter(const obj: TObject): boolean;
begin
  result:=EnsureMonitorData(obj)^.TryEnter;
end;

class function WinViVer.DoWait(const obj: TObject; timeout: cardinal): boolean;
var
  m: PMonitorData;
begin
  m := EnsureMonitorData(obj);
  result := Wait(m, m, timeout);
end;

class function WinViVer.DoWaitLock(const monobj, lockobj: TObject; timeout: cardinal): boolean; static;
begin
  result := Wait(EnsureMonitorData(monobj), EnsureMonitorData(lockobj), timeout);
end;

class procedure WinViVer.DoFreeMonitorData(data: pointer); static;
begin
  if Assigned(data) then
    dispose(PMonitorData(data));
end;

procedure RegisterWindowsMonitorSupport;
begin
  NewMonitor := GetMonitorManager; // Reuse Do(Get|Set)DefaultSpinCount from the old monitor, supposedly FPMonitor.
  if WinViVer.TryLoad(NewMonitor) then
  begin
    OldMonitor := SetMonitorManager(NewMonitor);
    IsSet := true;
  end;
  // Otherwise keep the old monitor, supposedly FPMonitor.
end;

procedure UnregisterWindowsMonitorSupport;
begin
  if IsSet then
    SetMonitorManager(OldMonitor);
end;

Initialization
  RegisterWindowsMonitorSupport;
finalization
  UnregisterWindowsMonitorSupport;
end.
