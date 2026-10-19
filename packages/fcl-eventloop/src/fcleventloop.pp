{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by the Free Pascal development team

    Scalable Asynchronous I/O Event Loop - Public Facade & Factory

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.
}
{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

unit fcleventloop;

interface

uses
  SysUtils, Classes,
  eventloop.types,
  eventloop.core,
  eventloop.sockets,
  eventloop.pipes,
  eventloop.files
  {$if defined(LINUX) or defined(ANDROID)}
  , eventloop.backend.epoll, eventloop.backend.uring
  {$elseif defined(FREEBSD) or defined(DARWIN) or defined(OPENBSD) or defined(NETBSD) or defined(DRAGONFLY)}
  , eventloop.backend.kqueue
  {$elseif defined(MSWINDOWS)}
  , eventloop.backend.iocp
  {$endif};

type
  { Re-export primary types and interfaces }
  TEventLoopBackendKind = eventloop.types.TEventLoopBackendKind;
  TLoopHandle = eventloop.types.TLoopHandle;
  TEventLoopEvent = eventloop.types.TEventLoopEvent;
  TEventLoopEvents = eventloop.types.TEventLoopEvents;
  TAsyncCallback = eventloop.types.TAsyncCallback;
  TAsyncProc = eventloop.types.TAsyncProc;
  TAsyncIOCompletion = eventloop.types.TAsyncIOCompletion;
  TAsyncAcceptCallback = eventloop.types.TAsyncAcceptCallback;
  TAsyncReadCallback = eventloop.types.TAsyncReadCallback;
  TAsyncWriteCallback = eventloop.types.TAsyncWriteCallback;
  TAsyncAcceptProc = eventloop.types.TAsyncAcceptProc;
  TAsyncReadProc = eventloop.types.TAsyncReadProc;
  TAsyncWriteProc = eventloop.types.TAsyncWriteProc;
  TAsyncTimerCallback = eventloop.types.TAsyncTimerCallback;

  EEventLoopException = eventloop.types.EEventLoopException;
  EEventLoopClosed = eventloop.types.EEventLoopClosed;
  EEventLoopTimeout = eventloop.types.EEventLoopTimeout;
  ESocketException = eventloop.types.ESocketException;

  TAsyncBuffer = eventloop.types.TAsyncBuffer;
  PAsyncBuffer = eventloop.types.PAsyncBuffer;
  TAsyncBufferPool = eventloop.types.TAsyncBufferPool;

  IEventLoop = eventloop.core.IEventLoop;
  IEventLoopGroup = eventloop.core.IEventLoopGroup;
  IAsyncSocket = eventloop.sockets.IAsyncSocket;
  IAsyncServerSocket = eventloop.sockets.IAsyncServerSocket;
  IAsyncDatagramSocket = eventloop.sockets.IAsyncDatagramSocket;
  IAsyncPipe = eventloop.pipes.IAsyncPipe;
  IAsyncFile = eventloop.files.IAsyncFile;

  { Main entry point and factory }
  TEventLoop = class
  public
    class function Create(ABackend: TEventLoopBackendKind = bkAuto): IEventLoop; static;
    class function CreateGroup(AThreadCount: Integer = 0; ABackend: TEventLoopBackendKind = bkAuto): IEventLoopGroup; static;
    class function CreateTcpServer(ALoop: IEventLoop = nil): IAsyncServerSocket; static;
    class function CreateTcpClient(ALoop: IEventLoop = nil): IAsyncSocket; static;
    class function CreatePipe(ALoop: IEventLoop = nil): IAsyncPipe; static;
    class function OpenFile(const AFileName: string; AMode: Integer; ALoop: IEventLoop = nil): IAsyncFile; static;
  end;

implementation

{ TEventLoop }

class function TEventLoop.Create(ABackend: TEventLoopBackendKind): IEventLoop;
begin
  case ABackend of
    bkEpoll:
      begin
        {$if defined(LINUX) or defined(ANDROID)}
        Result := TEpollEventLoop.Create;
        {$else}
        raise EEventLoopException.Create('epoll is only supported on Linux/Android');
        {$endif}
      end;
    bkIOUring:
      begin
        {$if defined(LINUX) or defined(ANDROID)}
        if TIOUringEventLoop.IsSupported then
          Result := TIOUringEventLoop.Create
        else
          raise EEventLoopException.Create('io_uring is not supported on this kernel or is disabled by seccomp');
        {$else}
        raise EEventLoopException.Create('io_uring is only supported on Linux');
        {$endif}
      end;
    bkKqueue:
      begin
        {$if defined(FREEBSD) or defined(DARWIN) or defined(OPENBSD) or defined(NETBSD) or defined(DRAGONFLY)}
        Result := TKqueueEventLoop.Create;
        {$else}
        raise EEventLoopException.Create('kqueue is only supported on BSD/macOS');
        {$endif}
      end;
    bkIOCP:
      begin
        {$if defined(MSWINDOWS)}
        Result := TIOCPEventLoop.Create;
        {$else}
        raise EEventLoopException.Create('IOCP is only supported on Windows');
        {$endif}
      end;
    bkAuto:
      begin
        {$if defined(LINUX) or defined(ANDROID)}
        if TIOUringEventLoop.IsSupported then
          Result := TIOUringEventLoop.Create
        else
          Result := TEpollEventLoop.Create;
        {$elseif defined(FREEBSD) or defined(DARWIN) or defined(OPENBSD) or defined(NETBSD) or defined(DRAGONFLY)}
        Result := TKqueueEventLoop.Create;
        {$elseif defined(MSWINDOWS)}
        Result := TIOCPEventLoop.Create;
        {$else}
        raise EEventLoopException.Create('No native event loop backend available for this platform');
        {$endif}
      end;
  end;
end;

class function TEventLoop.CreateGroup(AThreadCount: Integer; ABackend: TEventLoopBackendKind): IEventLoopGroup;
var
  LLoops: array of IEventLoop;
  I: Integer;
begin
  if AThreadCount <= 0 then
    AThreadCount := TThread.ProcessorCount;
  if AThreadCount <= 0 then
    AThreadCount := 1;

  SetLength(LLoops, AThreadCount);
  for I := 0 to AThreadCount - 1 do
    LLoops[I] := Create(ABackend);

  Result := TEventLoopGroup.Create(LLoops);
end;

class function TEventLoop.CreateTcpServer(ALoop: IEventLoop): IAsyncServerSocket;
begin
  if ALoop = nil then
    ALoop := Create(bkAuto);
  Result := TAsyncServerSocket.Create(ALoop);
end;

class function TEventLoop.CreateTcpClient(ALoop: IEventLoop): IAsyncSocket;
begin
  if ALoop = nil then
    ALoop := Create(bkAuto);
  Result := TAsyncSocket.Create(ALoop);
end;

class function TEventLoop.CreatePipe(ALoop: IEventLoop): IAsyncPipe;
begin
  if ALoop = nil then
    ALoop := Create(bkAuto);
  Result := TAsyncPipe.Create(ALoop);
end;

class function TEventLoop.OpenFile(const AFileName: string; AMode: Integer; ALoop: IEventLoop): IAsyncFile;
begin
  Result := TAsyncFile.Open(AFileName, AMode, ALoop);
end;

end.
