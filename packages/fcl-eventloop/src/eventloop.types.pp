{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by the Free Pascal development team

    Scalable Asynchronous I/O Event Loop - Base Types & Error Classes

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.
}
{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}
{$modeswitch advancedrecords}

unit eventloop.types;

interface

uses
  SysUtils, Classes, SyncObjs
  {$ifdef UNIX}, ctypes, baseunix, unixtype{$endif}
  {$ifdef MSWINDOWS}, Windows{$endif};

type
  { Backend selector kind }
  TEventLoopBackendKind = (
    bkAuto,      { Selects optimal native OS multiplexer automatically }
    bkEpoll,     { Linux epoll (level / edge-triggered) }
    bkIOUring,   { Linux io_uring (zero-copy submission/completion rings) }
    bkKqueue,    { BSD / Darwin kqueue }
    bkIOCP       { Windows I/O Completion Ports }
  );

  { Generic handle type for OS resources across platforms }
  {$ifdef MSWINDOWS}
  TLoopHandle = THandle;
  {$else}
  TLoopHandle = cint;
  {$endif}

const
  InvalidLoopHandle: TLoopHandle = TLoopHandle(-1);

type
  { Polled I/O events }
  TEventLoopEvent = (
    eleRead,     { Ready to read / incoming data or incoming connection }
    eleWrite,    { Ready to write / socket buffer space available }
    eleError,    { Exceptional condition / error }
    eleHangup    { Remote peer closed connection or pipe broken }
  );
  TEventLoopEvents = set of TEventLoopEvent;

  { Callback types }
  TAsyncCallback = procedure of object;
  TAsyncProc = reference to procedure;
  TAsyncIOCompletion = procedure(AHandle: TLoopHandle; AEvents: TEventLoopEvents; AUserData: Pointer) of object;
  TAsyncAcceptCallback = procedure(AClientHandle: TLoopHandle; const AClientAddr: string; AClientPort: Word; AStatus: Integer) of object;
  TAsyncReadCallback = procedure(ABytesRead: NativeInt; AStatus: Integer) of object;
  TAsyncWriteCallback = procedure(ABytesWritten: NativeInt; AStatus: Integer) of object;
  TAsyncAcceptProc = reference to procedure(AClientHandle: TLoopHandle; const AClientAddr: string; AClientPort: Word; AStatus: Integer);
  TAsyncReadProc = reference to procedure(ABytesRead: NativeInt; AStatus: Integer);
  TAsyncWriteProc = reference to procedure(ABytesWritten: NativeInt; AStatus: Integer);
  TAsyncCloseCallback = procedure of object;
  TAsyncTimerCallback = procedure(ATimerId: Int64) of object;

  { Exception classes }
  EEventLoopException = class(Exception);
  EEventLoopClosed = class(EEventLoopException);
  EEventLoopTimeout = class(EEventLoopException);
  ESocketException = class(EEventLoopException);

  { Non-blocking buffer wrapper for zero-allocation I/O }
  PAsyncBuffer = ^TAsyncBuffer;
  TAsyncBuffer = record
  private
    FData: PByte;
    FCapacity: Integer;
    FSize: Integer;
    FPosition: Integer;
    FOwnsData: Boolean;
  public
    class function Create(ACapacity: Integer): TAsyncBuffer; static;
    class function Wrap(APtr: Pointer; ASize: Integer): TAsyncBuffer; static;
    procedure Free;
    procedure Reset;
    function Write(constref ASource; ACount: Integer): Integer;
    function Read(var ADest; ACount: Integer): Integer;
    function CurrentPtr: PByte;
    function Remaining: Integer;
    property Data: PByte read FData;
    property Capacity: Integer read FCapacity;
    property Size: Integer read FSize write FSize;
    property Position: Integer read FPosition write FPosition;
  end;

  { Thread-safe buffer pool to prevent GC/heap churn during 100k+ bursts }
  TAsyncBufferPool = class
  private
    FBufferSize: Integer;
    FMaxCount: Integer;
    FStack: array of PByte;
    FCount: Integer;
    FLock: TCriticalSection;
  public
    constructor Create(ABufferSize: Integer = 4096; AMaxPoolCount: Integer = 1024);
    destructor Destroy; override;
    function Acquire: PByte;
    procedure Release(ABuffer: PByte);
    property BufferSize: Integer read FBufferSize;
    property AvailableCount: Integer read FCount;
  end;

implementation

{ TAsyncBuffer }

class function TAsyncBuffer.Create(ACapacity: Integer): TAsyncBuffer;
begin
  Result.FData := GetMem(ACapacity);
  Result.FCapacity := ACapacity;
  Result.FSize := 0;
  Result.FPosition := 0;
  Result.FOwnsData := True;
end;

class function TAsyncBuffer.Wrap(APtr: Pointer; ASize: Integer): TAsyncBuffer;
begin
  Result.FData := PByte(APtr);
  Result.FCapacity := ASize;
  Result.FSize := ASize;
  Result.FPosition := 0;
  Result.FOwnsData := False;
end;

procedure TAsyncBuffer.Free;
begin
  if FOwnsData and (FData <> nil) then
  begin
    FreeMem(FData);
    FData := nil;
  end;
  FCapacity := 0;
  FSize := 0;
  FPosition := 0;
end;

procedure TAsyncBuffer.Reset;
begin
  FSize := 0;
  FPosition := 0;
end;

function TAsyncBuffer.CurrentPtr: PByte;
begin
  Result := FData + FPosition;
end;

function TAsyncBuffer.Remaining: Integer;
begin
  Result := FSize - FPosition;
  if Result < 0 then
    Result := 0;
end;

function TAsyncBuffer.Write(constref ASource; ACount: Integer): Integer;
var
  LAvail: Integer;
begin
  LAvail := FCapacity - FSize;
  if ACount > LAvail then
    ACount := LAvail;
  if ACount > 0 then
  begin
    Move(ASource, (FData + FSize)^, ACount);
    Inc(FSize, ACount);
  end;
  Result := ACount;
end;

function TAsyncBuffer.Read(var ADest; ACount: Integer): Integer;
var
  LRem: Integer;
begin
  LRem := Remaining;
  if ACount > LRem then
    ACount := LRem;
  if ACount > 0 then
  begin
    Move((FData + FPosition)^, ADest, ACount);
    Inc(FPosition, ACount);
  end;
  Result := ACount;
end;

{ TAsyncBufferPool }

constructor TAsyncBufferPool.Create(ABufferSize: Integer; AMaxPoolCount: Integer);
begin
  inherited Create;
  FBufferSize := ABufferSize;
  FMaxCount := AMaxPoolCount;
  SetLength(FStack, FMaxCount);
  FCount := 0;
  FLock := TCriticalSection.Create;
end;

destructor TAsyncBufferPool.Destroy;
var
  I: Integer;
begin
  FLock.Acquire;
  try
    for I := 0 to FCount - 1 do
      FreeMem(FStack[I]);
    FCount := 0;
  finally
    FLock.Release;
  end;
  FreeAndNil(FLock);
  inherited Destroy;
end;

function TAsyncBufferPool.Acquire: PByte;
begin
  FLock.Acquire;
  try
    if FCount > 0 then
    begin
      Dec(FCount);
      Result := FStack[FCount];
    end
    else
      Result := GetMem(FBufferSize);
  finally
    FLock.Release;
  end;
end;

procedure TAsyncBufferPool.Release(ABuffer: PByte);
begin
  if ABuffer = nil then
    Exit;
  FLock.Acquire;
  try
    if FCount < FMaxCount then
    begin
      FStack[FCount] := ABuffer;
      Inc(FCount);
    end
    else
      FreeMem(ABuffer);
  finally
    FLock.Release;
  end;
end;

end.
