{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by the Free Pascal development team

    Scalable Asynchronous I/O Event Loop - Windows IOCP Backend

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.
}
{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

unit eventloop.backend.iocp;

interface

{$ifdef MSWINDOWS}

uses
  SysUtils, Classes, Windows, eventloop.types, eventloop.core;

type
  { High-performance Windows I/O Completion Port event loop engine }
  TIOCPEventLoop = class(TEventLoopBase)
  private
    FIOCP: THandle;
    FMaxEvents: Integer;
  protected
    procedure DoRegisterHandle(AHandle: TLoopHandle; AEvents: TEventLoopEvents; const ACallback: TAsyncIOCompletion; AUserData: Pointer); override;
    procedure DoModifyHandle(AHandle: TLoopHandle; AEvents: TEventLoopEvents); override;
    procedure DoUnregisterHandle(AHandle: TLoopHandle); override;
    procedure DoPoll(ATimeoutMs: Integer); override;
    procedure DoWakeup; override;
  public
    constructor Create(AMaxEvents: Integer = 1024);
    destructor Destroy; override;
    property IOCPHandle: THandle read FIOCP;
  end;

{$else}

uses
  SysUtils, Classes, eventloop.types, eventloop.core;

type
  { Stub for cross-compilation target compatibility }
  TIOCPEventLoop = class(TEventLoopBase)
  public
    constructor Create(AMaxEvents: Integer = 1024);
  end;

{$endif}

implementation

{$ifdef MSWINDOWS}

const
  WAKEUP_COMPLETION_KEY = ULONG_PTR(-1);

constructor TIOCPEventLoop.Create(AMaxEvents: Integer);
begin
  inherited Create(bkIOCP);
  FMaxEvents := AMaxEvents;
  FIOCP := CreateIoCompletionPort(INVALID_HANDLE_VALUE, 0, 0, 1);
  if FIOCP = 0 then
    raise EEventLoopException.CreateFmt('CreateIoCompletionPort failed: %s (error=%d)', [SysErrorMessage(GetLastError), GetLastError]);
end;

destructor TIOCPEventLoop.Destroy;
begin
  if FIOCP <> 0 then
  begin
    CloseHandle(FIOCP);
    FIOCP := 0;
  end;
  inherited Destroy;
end;

procedure TIOCPEventLoop.DoRegisterHandle(AHandle: TLoopHandle; AEvents: TEventLoopEvents; const ACallback: TAsyncIOCompletion; AUserData: Pointer);
var
  LRes: THandle;
begin
  LRes := CreateIoCompletionPort(AHandle, FIOCP, ULONG_PTR(AHandle), 0);
  if LRes = 0 then
    raise EEventLoopException.CreateFmt('CreateIoCompletionPort attach failed for handle %d: %s', [AHandle, SysErrorMessage(GetLastError)]);
end;

procedure TIOCPEventLoop.DoModifyHandle(AHandle: TLoopHandle; AEvents: TEventLoopEvents);
begin
  { On Windows IOCP, notification triggers are per-operation (WSARecv/WSASend/ReadFile/WriteFile) }
end;

procedure TIOCPEventLoop.DoUnregisterHandle(AHandle: TLoopHandle);
begin
  { Closing handle or cancelling pending I/O clears it from IOCP }
  CancelIoEx(AHandle, nil);
end;

procedure TIOCPEventLoop.DoWakeup;
begin
  if FIOCP <> 0 then
    PostQueuedCompletionStatus(FIOCP, 0, WAKEUP_COMPLETION_KEY, nil);
end;

procedure TIOCPEventLoop.DoPoll(ATimeoutMs: Integer);
var
  LBytesTransferred: DWORD;
  LCompletionKey: ULONG_PTR;
  LOverlapped: POverlapped;
  LSuccess: BOOL;
  LTimeout: DWORD;
  LEvents: TEventLoopEvents;
begin
  if ATimeoutMs < 0 then
    LTimeout := INFINITE
  else
    LTimeout := DWORD(ATimeoutMs);

  LOverlapped := nil;
  LSuccess := GetQueuedCompletionStatus(FIOCP, LBytesTransferred, LCompletionKey, LOverlapped, LTimeout);

  if (not LSuccess) and (LOverlapped = nil) then
    Exit; { Timeout or port closed }

  if LCompletionKey = WAKEUP_COMPLETION_KEY then
    Exit; { Inter-thread wakeup }

  LEvents := [];
  if LSuccess then
  begin
    Include(LEvents, eleRead);
    Include(LEvents, eleWrite);
  end
  else
    Include(LEvents, eleError);

  TriggerHandle(TLoopHandle(LCompletionKey), LEvents);
end;

{$else}

constructor TIOCPEventLoop.Create(AMaxEvents: Integer);
begin
  inherited Create(bkIOCP);
  raise EEventLoopException.Create('IOCP backend is only available on Windows');
end;

{$endif}

end.
