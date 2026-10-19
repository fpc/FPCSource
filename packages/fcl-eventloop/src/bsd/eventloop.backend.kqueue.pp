{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by the Free Pascal development team

    Scalable Asynchronous I/O Event Loop - BSD / macOS kqueue Backend

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.
}
{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

unit eventloop.backend.kqueue;

interface

{$if defined(FREEBSD) or defined(DARWIN) or defined(OPENBSD) or defined(NETBSD) or defined(DRAGONFLY)}

uses
  SysUtils, Classes, BaseUnix, Unix, bsd, ctypes, eventloop.types, eventloop.core;

type
  { High-performance kqueue event loop engine for macOS / BSD }
  TKqueueEventLoop = class(TEventLoopBase)
  private
    FKqueueFd: cint;
    FWakeupPipeRead: cint;
    FWakeupPipeWrite: cint;
    FMaxEvents: Integer;
    FEventsBuffer: array of TKEvent;

    procedure InitWakeup;
    procedure CleanupWakeup;
    procedure UpdateFilter(AHandle: TLoopHandle; AFilter: cshort; AFlags: cushort);
    function KEventToEvents(constref AEvent: TKEvent): TEventLoopEvents;
  protected
    procedure DoRegisterHandle(AHandle: TLoopHandle; AEvents: TEventLoopEvents; const ACallback: TAsyncIOCompletion; AUserData: Pointer); override;
    procedure DoModifyHandle(AHandle: TLoopHandle; AEvents: TEventLoopEvents); override;
    procedure DoUnregisterHandle(AHandle: TLoopHandle); override;
    procedure DoPoll(ATimeoutMs: Integer); override;
    procedure DoWakeup; override;
  public
    constructor Create(AMaxEvents: Integer = 1024);
    destructor Destroy; override;
    property KqueueFd: cint read FKqueueFd;
  end;

{$else}

uses
  SysUtils, Classes, eventloop.types, eventloop.core;

type
  { Stub for cross-compilation target compatibility }
  TKqueueEventLoop = class(TEventLoopBase)
  public
    constructor Create(AMaxEvents: Integer = 1024);
  end;

{$endif}

implementation

{$if defined(FREEBSD) or defined(DARWIN) or defined(OPENBSD) or defined(NETBSD) or defined(DRAGONFLY)}

const
  EVFILT_READ_CONST  = -1;
  EVFILT_WRITE_CONST = -2;
  EV_ADD_CONST       = $0001;
  EV_DELETE_CONST    = $0002;
  EV_ENABLE_CONST    = $0004;
  EV_DISABLE_CONST   = $0008;
  EV_CLEAR_CONST     = $0020;
  EV_EOF_CONST       = $8000;
  EV_ERROR_CONST     = $4000;

{ TKqueueEventLoop }

constructor TKqueueEventLoop.Create(AMaxEvents: Integer);
begin
  inherited Create(bkKqueue);
  FMaxEvents := AMaxEvents;
  if FMaxEvents < 64 then
    FMaxEvents := 64;
  SetLength(FEventsBuffer, FMaxEvents);

  FKqueueFd := kqueue;
  if FKqueueFd < 0 then
    raise EEventLoopException.CreateFmt('kqueue() failed: %s (errno=%d)', [SysErrorMessage(fpgeterrno), fpgeterrno]);

  InitWakeup;
end;

destructor TKqueueEventLoop.Destroy;
begin
  CleanupWakeup;
  if FKqueueFd >= 0 then
  begin
    fpclose(FKqueueFd);
    FKqueueFd := -1;
  end;
  inherited Destroy;
end;

procedure TKqueueEventLoop.InitWakeup;
var
  LPipes: array[0..1] of cint;
  LChg: TKEvent;
begin
  if fppipe(LPipes) <> 0 then
    raise EEventLoopException.Create('kqueue: failed to create wakeup pipe');

  FWakeupPipeRead := LPipes[0];
  FWakeupPipeWrite := LPipes[1];

  fpfcntl(FWakeupPipeRead, F_SETFL, O_NONBLOCK);
  fpfcntl(FWakeupPipeWrite, F_SETFL, O_NONBLOCK);

  EV_SET(@LChg, FWakeupPipeRead, EVFILT_READ_CONST, EV_ADD_CONST or EV_ENABLE_CONST, 0, 0, nil);
  kevent(FKqueueFd, @LChg, 1, nil, 0, nil);
end;

procedure TKqueueEventLoop.CleanupWakeup;
var
  LChg: TKEvent;
begin
  if FWakeupPipeRead >= 0 then
  begin
    EV_SET(@LChg, FWakeupPipeRead, EVFILT_READ_CONST, EV_DELETE_CONST, 0, 0, nil);
    kevent(FKqueueFd, @LChg, 1, nil, 0, nil);
    fpclose(FWakeupPipeRead);
    FWakeupPipeRead := -1;
  end;
  if FWakeupPipeWrite >= 0 then
  begin
    fpclose(FWakeupPipeWrite);
    FWakeupPipeWrite := -1;
  end;
end;

procedure TKqueueEventLoop.UpdateFilter(AHandle: TLoopHandle; AFilter: cshort; AFlags: cushort);
var
  LChg: TKEvent;
begin
  EV_SET(@LChg, AHandle, AFilter, AFlags, 0, 0, nil);
  kevent(FKqueueFd, @LChg, 1, nil, 0, nil);
end;

function TKqueueEventLoop.KEventToEvents(constref AEvent: TKEvent): TEventLoopEvents;
begin
  Result := [];
  if AEvent.Filter = EVFILT_READ_CONST then
    Include(Result, eleRead);
  if AEvent.Filter = EVFILT_WRITE_CONST then
    Include(Result, eleWrite);
  if (AEvent.Flags and EV_ERROR_CONST) <> 0 then
    Include(Result, eleError);
  if (AEvent.Flags and EV_EOF_CONST) <> 0 then
    Include(Result, eleHangup);
end;

procedure TKqueueEventLoop.DoRegisterHandle(AHandle: TLoopHandle; AEvents: TEventLoopEvents; const ACallback: TAsyncIOCompletion; AUserData: Pointer);
begin
  if eleRead in AEvents then
    UpdateFilter(AHandle, EVFILT_READ_CONST, EV_ADD_CONST or EV_ENABLE_CONST or EV_CLEAR_CONST);
  if eleWrite in AEvents then
    UpdateFilter(AHandle, EVFILT_WRITE_CONST, EV_ADD_CONST or EV_ENABLE_CONST or EV_CLEAR_CONST);
end;

procedure TKqueueEventLoop.DoModifyHandle(AHandle: TLoopHandle; AEvents: TEventLoopEvents);
begin
  DoUnregisterHandle(AHandle);
  DoRegisterHandle(AHandle, AEvents, nil, nil);
end;

procedure TKqueueEventLoop.DoUnregisterHandle(AHandle: TLoopHandle);
begin
  UpdateFilter(AHandle, EVFILT_READ_CONST, EV_DELETE_CONST);
  UpdateFilter(AHandle, EVFILT_WRITE_CONST, EV_DELETE_CONST);
end;

procedure TKqueueEventLoop.DoWakeup;
var
  LByte: Byte;
begin
  if FWakeupPipeWrite >= 0 then
  begin
    LByte := 1;
    fpwrite(FWakeupPipeWrite, LByte, 1);
  end;
end;

procedure TKqueueEventLoop.DoPoll(ATimeoutMs: Integer);
var
  LTimeout: TTimeSpec;
  LPTimeout: PTimeSpec;
  LNReady: cint;
  I: Integer;
  LFd: cint;
  LDummy: array[0..63] of Byte;
  LEvents: TEventLoopEvents;
begin
  if ATimeoutMs < 0 then
    LPTimeout := nil
  else
  begin
    LTimeout.tv_sec := ATimeoutMs div 1000;
    LTimeout.tv_nsec := (ATimeoutMs mod 1000) * 1000000;
    LPTimeout := @LTimeout;
  end;

  LNReady := kevent(FKqueueFd, nil, 0, @FEventsBuffer[0], FMaxEvents, LPTimeout);
  if LNReady <= 0 then
    Exit;

  for I := 0 to LNReady - 1 do
  begin
    LFd := FEventsBuffer[I].Ident;
    if LFd = FWakeupPipeRead then
    begin
      fpread(FWakeupPipeRead, LDummy[0], SizeOf(LDummy));
    end
    else
    begin
      LEvents := KEventToEvents(FEventsBuffer[I]);
      TriggerHandle(LFd, LEvents);
    end;
  end;
end;

{$else}

constructor TKqueueEventLoop.Create(AMaxEvents: Integer);
begin
  inherited Create(bkKqueue);
  raise EEventLoopException.Create('kqueue backend is only available on BSD/Darwin platforms');
end;

{$endif}

end.

