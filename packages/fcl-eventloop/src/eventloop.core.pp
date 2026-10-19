{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by the Free Pascal development team

    Scalable Asynchronous I/O Event Loop - Core Architecture & Worker Group

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.
}
{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}
{$modeswitch advancedrecords}

unit eventloop.core;

interface

uses
  SysUtils, Classes, SyncObjs, eventloop.types;

type
  IEventLoop = interface;

  { Multi-core reactor loop pool }
  IEventLoopGroup = interface
    ['{B43F29C1-4D30-4740-BD76-A833E8972E20}']
    function Count: Integer;
    function GetLoop(AIndex: Integer): IEventLoop;
    function NextLoop: IEventLoop;
    procedure Start;
    procedure Stop;
    procedure Wait;
    property Loops[AIndex: Integer]: IEventLoop read GetLoop; default;
  end;

  { Single-threaded asynchronous event loop }
  IEventLoop = interface
    ['{7E12D66B-A483-49F1-92A3-31718F1BEA01}']
    function GetBackendKind: TEventLoopBackendKind;
    function IsRunning: Boolean;
    function InLoopThread: Boolean;
    procedure Run;
    procedure RunOnce(ATimeoutMs: Integer = -1);
    procedure Stop;
    procedure Wakeup;
    procedure Post(const AProc: TAsyncProc); overload;
    procedure Post(const ACallback: TAsyncCallback); overload;
    function SetTimer(ADelayMs: Cardinal; const ACallback: TAsyncTimerCallback; APeriodic: Boolean = False): Int64;
    procedure CancelTimer(ATimerId: Int64);
    procedure RegisterHandle(AHandle: TLoopHandle; AEvents: TEventLoopEvents; const ACallback: TAsyncIOCompletion; AUserData: Pointer = nil);
    procedure ModifyHandle(AHandle: TLoopHandle; AEvents: TEventLoopEvents);
    procedure UnregisterHandle(AHandle: TLoopHandle);
    property BackendKind: TEventLoopBackendKind read GetBackendKind;
    property Running: Boolean read IsRunning;
  end;

  { Registered handle metadata }
  PHandleRegistration = ^THandleRegistration;
  THandleRegistration = record
    Handle: TLoopHandle;
    Events: TEventLoopEvents;
    Callback: TAsyncIOCompletion;
    UserData: Pointer;
    Active: Boolean;
  end;

  { Internal timer entry }
  PTimerEntry = ^TTimerEntry;
  TTimerEntry = record
    Id: Int64;
    DeadlineMs: QWord;
    IntervalMs: Cardinal;
    Callback: TAsyncTimerCallback;
    Periodic: Boolean;
    Cancelled: Boolean;
  end;

  { Internal deferred task entry }
  TDeferredKind = (dkProc, dkCallback);
  TDeferredEntry = record
    Kind: TDeferredKind;
    Proc: TAsyncProc;
    Callback: TAsyncCallback;
  end;

  { Abstract base class implementing cross-cutting timer heap, deferred queue, and registry }
  TEventLoopBase = class(TInterfacedObject, IEventLoop)
  private
    FRunning: Boolean;
    FLoopThreadId: TThreadID;
    FBackendKind: TEventLoopBackendKind;

    { Handle registry }
    FRegistrations: array of THandleRegistration;
    FRegCapacity: Integer;
    FRegLock: TCriticalSection;

    { Timers }
    FTimers: array of TTimerEntry;
    FTimerCount: Integer;
    FNextTimerId: Int64;
    FTimerLock: TCriticalSection;

    { Deferred tasks }
    FDeferred: array of TDeferredEntry;
    FDeferredCount: Integer;
    FDeferredLock: TCriticalSection;

    procedure EnsureCapacity(AHandle: TLoopHandle);
  protected
    procedure DoRegisterHandle(AHandle: TLoopHandle; AEvents: TEventLoopEvents; const ACallback: TAsyncIOCompletion; AUserData: Pointer); virtual; abstract;
    procedure DoModifyHandle(AHandle: TLoopHandle; AEvents: TEventLoopEvents); virtual; abstract;
    procedure DoUnregisterHandle(AHandle: TLoopHandle); virtual; abstract;
    procedure DoPoll(ATimeoutMs: Integer); virtual; abstract;
    procedure DoWakeup; virtual; abstract;

    procedure TriggerHandle(AHandle: TLoopHandle; AEvents: TEventLoopEvents);
    procedure DrainDeferred;
    function ProcessTimers: Integer; { Returns ms until next deadline or -1 if empty }
  public
    constructor Create(ABackend: TEventLoopBackendKind);
    destructor Destroy; override;

    { IEventLoop }
    function GetBackendKind: TEventLoopBackendKind;
    function IsRunning: Boolean;
    function InLoopThread: Boolean;
    procedure Run;
    procedure RunOnce(ATimeoutMs: Integer = -1);
    procedure Stop;
    procedure Wakeup;
    procedure Post(const AProc: TAsyncProc); overload;
    procedure Post(const ACallback: TAsyncCallback); overload;
    function SetTimer(ADelayMs: Cardinal; const ACallback: TAsyncTimerCallback; APeriodic: Boolean = False): Int64;
    procedure CancelTimer(ATimerId: Int64);
    procedure RegisterHandle(AHandle: TLoopHandle; AEvents: TEventLoopEvents; const ACallback: TAsyncIOCompletion; AUserData: Pointer = nil);
    procedure ModifyHandle(AHandle: TLoopHandle; AEvents: TEventLoopEvents);
    procedure UnregisterHandle(AHandle: TLoopHandle);

    property BackendKind: TEventLoopBackendKind read GetBackendKind;
    property Running: Boolean read IsRunning;
  end;

  { Dedicated worker thread running an IEventLoop }
  TEventLoopWorkerThread = class(TThread)
  private
    FLoop: IEventLoop;
  protected
    procedure Execute; override;
  public
    constructor Create(ALoop: IEventLoop);
    property Loop: IEventLoop read FLoop;
  end;

  { Worker pool implementation }
  TEventLoopGroup = class(TInterfacedObject, IEventLoopGroup)
  private
    FLoops: array of IEventLoop;
    FThreads: array of TEventLoopWorkerThread;
    FCount: Integer;
    FRoundRobinIndex: Integer;
  public
    constructor Create(ALoops: array of IEventLoop);
    destructor Destroy; override;

    function Count: Integer;
    function GetLoop(AIndex: Integer): IEventLoop;
    function NextLoop: IEventLoop;
    procedure Start;
    procedure Stop;
    procedure Wait;
  end;

implementation

{ TEventLoopBase }

constructor TEventLoopBase.Create(ABackend: TEventLoopBackendKind);
begin
  inherited Create;
  FBackendKind := ABackend;
  FRunning := False;
  FLoopThreadId := 0;
  FRegCapacity := 0;
  FRegLock := TCriticalSection.Create;
  FTimerLock := TCriticalSection.Create;
  FDeferredLock := TCriticalSection.Create;
  FTimerCount := 0;
  FNextTimerId := 1;
  FDeferredCount := 0;
end;

destructor TEventLoopBase.Destroy;
var
  I: Integer;
begin
  Stop;
  FreeAndNil(FRegLock);
  FreeAndNil(FTimerLock);
  FreeAndNil(FDeferredLock);
  inherited Destroy;
end;

function TEventLoopBase.GetBackendKind: TEventLoopBackendKind;
begin
  Result := FBackendKind;
end;

function TEventLoopBase.IsRunning: Boolean;
begin
  Result := FRunning;
end;

function TEventLoopBase.InLoopThread: Boolean;
begin
  Result := (FLoopThreadId = GetCurrentThreadId);
end;

procedure TEventLoopBase.EnsureCapacity(AHandle: TLoopHandle);
var
  LReq: Integer;
  LOld: Integer;
  I: Integer;
begin
  LReq := Integer(AHandle) + 1;
  if LReq > FRegCapacity then
  begin
    LOld := FRegCapacity;
    if FRegCapacity = 0 then
      FRegCapacity := 64;
    while FRegCapacity < LReq do
      FRegCapacity := FRegCapacity * 2;
    SetLength(FRegistrations, FRegCapacity);
    for I := LOld to FRegCapacity - 1 do
    begin
      FRegistrations[I].Handle := InvalidLoopHandle;
      FRegistrations[I].Events := [];
      FRegistrations[I].Callback := nil;
      FRegistrations[I].UserData := nil;
      FRegistrations[I].Active := False;
    end;
  end;
end;

procedure TEventLoopBase.RegisterHandle(AHandle: TLoopHandle; AEvents: TEventLoopEvents; const ACallback: TAsyncIOCompletion; AUserData: Pointer);
begin
  if AHandle = InvalidLoopHandle then
    raise EEventLoopException.Create('Invalid handle');
  FRegLock.Acquire;
  try
    EnsureCapacity(AHandle);
    FRegistrations[Integer(AHandle)].Handle := AHandle;
    FRegistrations[Integer(AHandle)].Events := AEvents;
    FRegistrations[Integer(AHandle)].Callback := ACallback;
    FRegistrations[Integer(AHandle)].UserData := AUserData;
    FRegistrations[Integer(AHandle)].Active := True;
  finally
    FRegLock.Release;
  end;
  DoRegisterHandle(AHandle, AEvents, ACallback, AUserData);
end;

procedure TEventLoopBase.ModifyHandle(AHandle: TLoopHandle; AEvents: TEventLoopEvents);
begin
  if AHandle = InvalidLoopHandle then
    raise EEventLoopException.Create('Invalid handle');
  FRegLock.Acquire;
  try
    if (Integer(AHandle) < FRegCapacity) and FRegistrations[Integer(AHandle)].Active then
      FRegistrations[Integer(AHandle)].Events := AEvents;
  finally
    FRegLock.Release;
  end;
  DoModifyHandle(AHandle, AEvents);
end;

procedure TEventLoopBase.UnregisterHandle(AHandle: TLoopHandle);
begin
  if AHandle = InvalidLoopHandle then
    Exit;
  DoUnregisterHandle(AHandle);
  FRegLock.Acquire;
  try
    if Integer(AHandle) < FRegCapacity then
    begin
      FRegistrations[Integer(AHandle)].Active := False;
      FRegistrations[Integer(AHandle)].Callback := nil;
      FRegistrations[Integer(AHandle)].UserData := nil;
      FRegistrations[Integer(AHandle)].Events := [];
    end;
  finally
    FRegLock.Release;
  end;
end;

procedure TEventLoopBase.TriggerHandle(AHandle: TLoopHandle; AEvents: TEventLoopEvents);
var
  LReg: THandleRegistration;
begin
  if AHandle = InvalidLoopHandle then
    Exit;
  LReg.Active := False;
  FRegLock.Acquire;
  try
    if (Integer(AHandle) < FRegCapacity) and FRegistrations[Integer(AHandle)].Active then
      LReg := FRegistrations[Integer(AHandle)];
  finally
    FRegLock.Release;
  end;

  if LReg.Active and Assigned(LReg.Callback) then
  begin
    { Intersect with requested events unless error/hangup occurred }
    if (AEvents * [eleError, eleHangup] <> []) or (AEvents * LReg.Events <> []) then
      LReg.Callback(AHandle, AEvents, LReg.UserData);
  end;
end;

procedure TEventLoopBase.Post(const AProc: TAsyncProc);
var
  LIdx: Integer;
begin
  FDeferredLock.Acquire;
  try
    LIdx := FDeferredCount;
    Inc(FDeferredCount);
    if FDeferredCount > Length(FDeferred) then
      SetLength(FDeferred, (FDeferredCount * 3) div 2 + 8);
    FDeferred[LIdx].Kind := dkProc;
    FDeferred[LIdx].Proc := AProc;
    FDeferred[LIdx].Callback := nil;
  finally
    FDeferredLock.Release;
  end;
  Wakeup;
end;

procedure TEventLoopBase.Post(const ACallback: TAsyncCallback);
var
  LIdx: Integer;
begin
  FDeferredLock.Acquire;
  try
    LIdx := FDeferredCount;
    Inc(FDeferredCount);
    if FDeferredCount > Length(FDeferred) then
      SetLength(FDeferred, (FDeferredCount * 3) div 2 + 8);
    FDeferred[LIdx].Kind := dkCallback;
    FDeferred[LIdx].Proc := nil;
    FDeferred[LIdx].Callback := ACallback;
  finally
    FDeferredLock.Release;
  end;
  Wakeup;
end;

procedure TEventLoopBase.DrainDeferred;
var
  LBatch: array of TDeferredEntry;
  LCount: Integer;
  I: Integer;
begin
  LCount := 0;
  FDeferredLock.Acquire;
  try
    if FDeferredCount > 0 then
    begin
      LCount := FDeferredCount;
      SetLength(LBatch, LCount);
      for I := 0 to LCount - 1 do
        LBatch[I] := FDeferred[I];
      FDeferredCount := 0;
    end;
  finally
    FDeferredLock.Release;
  end;

  for I := 0 to LCount - 1 do
  begin
    try
      case LBatch[I].Kind of
        dkProc:
          if Assigned(LBatch[I].Proc) then
            LBatch[I].Proc();
        dkCallback:
          if Assigned(LBatch[I].Callback) then
            LBatch[I].Callback();
      end;
    except
      { Keep event loop resilient against user exceptions in callbacks }
    end;
    LBatch[I].Proc := nil;
    LBatch[I].Callback := nil;
  end;
end;

function TEventLoopBase.SetTimer(ADelayMs: Cardinal; const ACallback: TAsyncTimerCallback; APeriodic: Boolean): Int64;
var
  LNow: QWord;
  LIdx: Integer;
begin
  LNow := GetTickCount64;
  FTimerLock.Acquire;
  try
    Result := FNextTimerId;
    Inc(FNextTimerId);
    LIdx := FTimerCount;
    Inc(FTimerCount);
    if FTimerCount > Length(FTimers) then
      SetLength(FTimers, (FTimerCount * 3) div 2 + 4);
    FTimers[LIdx].Id := Result;
    FTimers[LIdx].DeadlineMs := LNow + ADelayMs;
    FTimers[LIdx].IntervalMs := ADelayMs;
    FTimers[LIdx].Callback := ACallback;
    FTimers[LIdx].Periodic := APeriodic;
    FTimers[LIdx].Cancelled := False;
  finally
    FTimerLock.Release;
  end;
  Wakeup;
end;

procedure TEventLoopBase.CancelTimer(ATimerId: Int64);
var
  I: Integer;
begin
  FTimerLock.Acquire;
  try
    for I := 0 to FTimerCount - 1 do
      if FTimers[I].Id = ATimerId then
      begin
        FTimers[I].Cancelled := True;
        Exit;
      end;
  finally
    FTimerLock.Release;
  end;
end;

function TEventLoopBase.ProcessTimers: Integer;
var
  LNow: QWord;
  LFired: array of TTimerEntry;
  LFiredCount: Integer;
  LMinNext: QWord;
  I, J: Integer;
  LDiff: Int64;
begin
  LNow := GetTickCount64;
  LFiredCount := 0;
  LMinNext := High(QWord);

  FTimerLock.Acquire;
  try
    { First pass: collect expired timers }
    for I := 0 to FTimerCount - 1 do
    begin
      if FTimers[I].Cancelled then
        Continue;
      if FTimers[I].DeadlineMs <= LNow then
      begin
        Inc(LFiredCount);
        if LFiredCount > Length(LFired) then
          SetLength(LFired, LFiredCount + 4);
        LFired[LFiredCount - 1] := FTimers[I];
        if FTimers[I].Periodic then
          FTimers[I].DeadlineMs := LNow + FTimers[I].IntervalMs
        else
          FTimers[I].Cancelled := True;
      end;
    end;

    { Compact cancelled timers and find smallest upcoming deadline }
    J := 0;
    for I := 0 to FTimerCount - 1 do
    begin
      if not FTimers[I].Cancelled then
      begin
        if I <> J then
          FTimers[J] := FTimers[I];
        if FTimers[J].DeadlineMs < LMinNext then
          LMinNext := FTimers[J].DeadlineMs;
        Inc(J);
      end;
    end;
    FTimerCount := J;
  finally
    FTimerLock.Release;
  end;

  { Invoke expired callbacks outside lock }
  for I := 0 to LFiredCount - 1 do
  begin
    if Assigned(LFired[I].Callback) then
    begin
      try
        LFired[I].Callback(LFired[I].Id);
      except
        { Resilient against callback exceptions }
      end;
    end;
  end;

  if LMinNext = High(QWord) then
    Result := -1
  else
  begin
    LNow := GetTickCount64;
    LDiff := Int64(LMinNext) - Int64(LNow);
    if LDiff <= 0 then
      Result := 0
    else if LDiff > High(Integer) then
      Result := High(Integer)
    else
      Result := Integer(LDiff);
  end;
end;

procedure TEventLoopBase.RunOnce(ATimeoutMs: Integer);
var
  LTimerMs: Integer;
  LEffTimeout: Integer;
begin
  FLoopThreadId := GetCurrentThreadId;
  DrainDeferred;
  LTimerMs := ProcessTimers;

  if ATimeoutMs < 0 then
    LEffTimeout := LTimerMs
  else if (LTimerMs < 0) or (ATimeoutMs < LTimerMs) then
    LEffTimeout := ATimeoutMs
  else
    LEffTimeout := LTimerMs;

  DoPoll(LEffTimeout);
  DrainDeferred;
  ProcessTimers;
end;

procedure TEventLoopBase.Run;
begin
  FRunning := True;
  FLoopThreadId := GetCurrentThreadId;
  try
    while FRunning do
      RunOnce(-1);
  finally
    FRunning := False;
  end;
end;

procedure TEventLoopBase.Stop;
begin
  FRunning := False;
  Wakeup;
end;

procedure TEventLoopBase.Wakeup;
begin
  DoWakeup;
end;

{ TEventLoopWorkerThread }

constructor TEventLoopWorkerThread.Create(ALoop: IEventLoop);
begin
  inherited Create(True);
  FLoop := ALoop;
  FreeOnTerminate := False;
end;

procedure TEventLoopWorkerThread.Execute;
begin
  if FLoop <> nil then
    FLoop.Run;
end;

{ TEventLoopGroup }

constructor TEventLoopGroup.Create(ALoops: array of IEventLoop);
var
  I: Integer;
begin
  inherited Create;
  FCount := Length(ALoops);
  SetLength(FLoops, FCount);
  SetLength(FThreads, FCount);
  for I := 0 to FCount - 1 do
  begin
    FLoops[I] := ALoops[I];
    FThreads[I] := TEventLoopWorkerThread.Create(FLoops[I]);
  end;
  FRoundRobinIndex := 0;
end;

destructor TEventLoopGroup.Destroy;
var
  I: Integer;
begin
  Stop;
  Wait;
  for I := 0 to FCount - 1 do
    FreeAndNil(FThreads[I]);
  inherited Destroy;
end;

function TEventLoopGroup.Count: Integer;
begin
  Result := FCount;
end;

function TEventLoopGroup.GetLoop(AIndex: Integer): IEventLoop;
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    raise EEventLoopException.CreateFmt('Invalid loop index %d (Count=%d)', [AIndex, FCount]);
  Result := FLoops[AIndex];
end;

function TEventLoopGroup.NextLoop: IEventLoop;
var
  LIdx: Integer;
begin
  if FCount = 0 then
    raise EEventLoopException.Create('No event loops in group');
  LIdx := InterlockedIncrement(FRoundRobinIndex) - 1;
  LIdx := (LIdx mod FCount + FCount) mod FCount;
  Result := FLoops[LIdx];
end;

procedure TEventLoopGroup.Start;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    FThreads[I].Start;
end;

procedure TEventLoopGroup.Stop;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    if FLoops[I] <> nil then
      FLoops[I].Stop;
end;

procedure TEventLoopGroup.Wait;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    if FThreads[I] <> nil then
      FThreads[I].WaitFor;
end;

end.
