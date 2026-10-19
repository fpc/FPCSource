{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by the Free Pascal development team

    Scalable Asynchronous I/O Event Loop - Linux epoll Backend

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.
}
{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

unit eventloop.backend.epoll;

interface

{$if defined(LINUX) or defined(ANDROID)}

uses
  SysUtils, Classes, BaseUnix, Unix, Linux, Syscall, eventloop.types, eventloop.core;

type
  { High-performance epoll event loop engine }
  TEpollEventLoop = class(TEventLoopBase)
  private
    FEpollFd: cint;
    FWakeupFd: cint;
    FWakeupPipeRead: cint;
    FWakeupPipeWrite: cint;
    FUseEventFd: Boolean;
    FMaxEvents: Integer;
    FEventsBuffer: array of TEpoll_Event;

    procedure InitWakeup;
    procedure CleanupWakeup;
    function EventsToEpoll(AEvents: TEventLoopEvents): cuint32;
    function EpollToEvents(AFlags: cuint32): TEventLoopEvents;
  protected
    procedure DoRegisterHandle(AHandle: TLoopHandle; AEvents: TEventLoopEvents; const ACallback: TAsyncIOCompletion; AUserData: Pointer); override;
    procedure DoModifyHandle(AHandle: TLoopHandle; AEvents: TEventLoopEvents); override;
    procedure DoUnregisterHandle(AHandle: TLoopHandle); override;
    procedure DoPoll(ATimeoutMs: Integer); override;
    procedure DoWakeup; override;
  public
    constructor Create(AMaxEvents: Integer = 1024);
    destructor Destroy; override;
    property EpollFd: cint read FEpollFd;
  end;

{$endif}

implementation

{$if defined(LINUX) or defined(ANDROID)}

const
  EPOLLRDHUP_CONST = $2000;
  EFD_CLOEXEC_CONST = $80000;
  EFD_NONBLOCK_CONST = $800;

{ TEpollEventLoop }

constructor TEpollEventLoop.Create(AMaxEvents: Integer);
begin
  inherited Create(bkEpoll);
  FMaxEvents := AMaxEvents;
  if FMaxEvents < 64 then
    FMaxEvents := 64;
  SetLength(FEventsBuffer, FMaxEvents);

  FEpollFd := epoll_create1(0);
  if FEpollFd < 0 then
    FEpollFd := epoll_create(FMaxEvents);
  if FEpollFd < 0 then
    raise EEventLoopException.CreateFmt('epoll_create failed: %s (errno=%d)', [SysErrorMessage(fpgeterrno), fpgeterrno]);

  InitWakeup;
end;

destructor TEpollEventLoop.Destroy;
begin
  CleanupWakeup;
  if FEpollFd >= 0 then
  begin
    fpclose(FEpollFd);
    FEpollFd := -1;
  end;
  inherited Destroy;
end;

procedure TEpollEventLoop.InitWakeup;
var
  LEv: TEpoll_Event;
  LPipes: array[0..1] of cint;
begin
  FWakeupFd := -1;
  FWakeupPipeRead := -1;
  FWakeupPipeWrite := -1;
  FUseEventFd := False;

  { Prefer eventfd2 for lightweight 8-byte atomic counter wakeup }
  FWakeupFd := cint(Do_SysCall(syscall_nr_eventfd2, 0, EFD_CLOEXEC_CONST or EFD_NONBLOCK_CONST));
  if FWakeupFd >= 0 then
  begin
    FUseEventFd := True;
    LEv.events := EPOLLIN or EPOLLET;
    LEv.data.fd := FWakeupFd;
    epoll_ctl(FEpollFd, EPOLL_CTL_ADD, FWakeupFd, @LEv);
  end
  else
  begin
    { Fallback to self-pipe }
    if cint(Do_SysCall(syscall_nr_pipe2, TSysParam(@LPipes[0]), O_NONBLOCK)) = 0 then
    begin
      FWakeupPipeRead := LPipes[0];
      FWakeupPipeWrite := LPipes[1];
      LEv.events := EPOLLIN or EPOLLET;
      LEv.data.fd := FWakeupPipeRead;
      epoll_ctl(FEpollFd, EPOLL_CTL_ADD, FWakeupPipeRead, @LEv);
    end
    else
      raise EEventLoopException.Create('Failed to create event loop wakeup mechanism');
  end;
end;

procedure TEpollEventLoop.CleanupWakeup;
begin
  if FUseEventFd and (FWakeupFd >= 0) then
  begin
    epoll_ctl(FEpollFd, EPOLL_CTL_DEL, FWakeupFd, nil);
    fpclose(FWakeupFd);
    FWakeupFd := -1;
  end
  else
  begin
    if FWakeupPipeRead >= 0 then
    begin
      epoll_ctl(FEpollFd, EPOLL_CTL_DEL, FWakeupPipeRead, nil);
      fpclose(FWakeupPipeRead);
      FWakeupPipeRead := -1;
    end;
    if FWakeupPipeWrite >= 0 then
    begin
      fpclose(FWakeupPipeWrite);
      FWakeupPipeWrite := -1;
    end;
  end;
end;

function TEpollEventLoop.EventsToEpoll(AEvents: TEventLoopEvents): cuint32;
begin
  Result := 0;
  if eleRead in AEvents then
    Result := Result or EPOLLIN or EPOLLPRI;
  if eleWrite in AEvents then
    Result := Result or EPOLLOUT;
  { epoll automatically reports EPOLLERR and EPOLLHUP }
end;

function TEpollEventLoop.EpollToEvents(AFlags: cuint32): TEventLoopEvents;
begin
  Result := [];
  if (AFlags and (EPOLLIN or EPOLLPRI)) <> 0 then
    Include(Result, eleRead);
  if (AFlags and EPOLLOUT) <> 0 then
    Include(Result, eleWrite);
  if (AFlags and EPOLLERR) <> 0 then
    Include(Result, eleError);
  if (AFlags and (EPOLLHUP or EPOLLRDHUP_CONST)) <> 0 then
    Include(Result, eleHangup);
end;

procedure TEpollEventLoop.DoRegisterHandle(AHandle: TLoopHandle; AEvents: TEventLoopEvents; const ACallback: TAsyncIOCompletion; AUserData: Pointer);
var
  LEv: TEpoll_Event;
  LRes: cint;
begin
  LEv.events := EventsToEpoll(AEvents);
  LEv.data.fd := AHandle;
  LRes := epoll_ctl(FEpollFd, EPOLL_CTL_ADD, AHandle, @LEv);
  if LRes < 0 then
    raise EEventLoopException.CreateFmt('epoll_ctl ADD failed for fd %d: %s (errno=%d)', [AHandle, SysErrorMessage(fpgeterrno), fpgeterrno]);
end;

procedure TEpollEventLoop.DoModifyHandle(AHandle: TLoopHandle; AEvents: TEventLoopEvents);
var
  LEv: TEpoll_Event;
  LRes: cint;
begin
  LEv.events := EventsToEpoll(AEvents);
  LEv.data.fd := AHandle;
  LRes := epoll_ctl(FEpollFd, EPOLL_CTL_MOD, AHandle, @LEv);
  if LRes < 0 then
    raise EEventLoopException.CreateFmt('epoll_ctl MOD failed for fd %d: %s (errno=%d)', [AHandle, SysErrorMessage(fpgeterrno), fpgeterrno]);
end;

procedure TEpollEventLoop.DoUnregisterHandle(AHandle: TLoopHandle);
begin
  epoll_ctl(FEpollFd, EPOLL_CTL_DEL, AHandle, nil);
end;

procedure TEpollEventLoop.DoWakeup;
var
  LVal: QWord;
  LByte: Byte;
begin
  if FUseEventFd and (FWakeupFd >= 0) then
  begin
    LVal := 1;
    fpwrite(FWakeupFd, LVal, SizeOf(LVal));
  end
  else if FWakeupPipeWrite >= 0 then
  begin
    LByte := 1;
    fpwrite(FWakeupPipeWrite, LByte, 1);
  end;
end;

procedure TEpollEventLoop.DoPoll(ATimeoutMs: Integer);
var
  LNReady: cint;
  I: Integer;
  LVal: QWord;
  LDummy: array[0..63] of Byte;
  LFd: cint;
  LEvents: TEventLoopEvents;
begin
  LNReady := epoll_wait(FEpollFd, @FEventsBuffer[0], FMaxEvents, ATimeoutMs);
  if LNReady <= 0 then
    Exit;

  for I := 0 to LNReady - 1 do
  begin
    LFd := FEventsBuffer[I].data.fd;
    if FUseEventFd and (LFd = FWakeupFd) then
    begin
      { Drain eventfd counter }
      fpread(FWakeupFd, LVal, SizeOf(LVal));
    end
    else if (not FUseEventFd) and (LFd = FWakeupPipeRead) then
    begin
      { Drain pipe bytes }
      fpread(FWakeupPipeRead, LDummy[0], SizeOf(LDummy));
    end
    else
    begin
      LEvents := EpollToEvents(FEventsBuffer[I].events);
      TriggerHandle(LFd, LEvents);
    end;
  end;
end;

{$endif}

end.

