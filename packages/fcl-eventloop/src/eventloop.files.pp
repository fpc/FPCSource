{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by the Free Pascal development team

    Scalable Asynchronous I/O Event Loop - Asynchronous File Operations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.
}
{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

unit eventloop.files;

interface

uses
  SysUtils, Classes, SyncObjs, System.Threading,
  eventloop.types, eventloop.core
  {$ifdef UNIX}, baseunix{$endif}
  {$ifdef MSWINDOWS}, Windows{$endif};

type
  IAsyncFile = interface
    ['{69A27F11-3E2A-4B6E-85F0-18DCEE348601}']
    function GetHandle: TLoopHandle;
    function GetSize: Int64;
    procedure AsyncRead(ABuffer: Pointer; ACount: NativeInt; AOffset: Int64; const ACallback: TAsyncReadCallback);
    procedure AsyncWrite(ABuffer: Pointer; ACount: NativeInt; AOffset: Int64; const ACallback: TAsyncWriteCallback);
    function ReadAsync(ABuffer: Pointer; ACount: NativeInt; AOffset: Int64; constref ACancellationToken: TCancellationToken): specialize IFuture<NativeInt>; overload;
    function ReadAsync(ABuffer: Pointer; ACount: NativeInt; AOffset: Int64 = -1): specialize IFuture<NativeInt>; overload;
    function WriteAsync(ABuffer: Pointer; ACount: NativeInt; AOffset: Int64; constref ACancellationToken: TCancellationToken): specialize IFuture<NativeInt>; overload;
    function WriteAsync(ABuffer: Pointer; ACount: NativeInt; AOffset: Int64 = -1): specialize IFuture<NativeInt>; overload;
    procedure Close;
    property Handle: TLoopHandle read GetHandle;
    property Size: Int64 read GetSize;
  end;

  TAsyncFile = class(TInterfacedObject, IAsyncFile)
  private
    FLoop: IEventLoop;
    FHandle: TLoopHandle;
    FFileName: string;
    FFileMode: Integer;
    FOwnsHandle: Boolean;
    FLock: TCriticalSection;
  public
    constructor Open(const AFileName: string; AMode: Integer; ALoop: IEventLoop = nil);
    destructor Destroy; override;

    function GetHandle: TLoopHandle;
    function GetSize: Int64;
    procedure AsyncRead(ABuffer: Pointer; ACount: NativeInt; AOffset: Int64; const ACallback: TAsyncReadCallback);
    procedure AsyncWrite(ABuffer: Pointer; ACount: NativeInt; AOffset: Int64; const ACallback: TAsyncWriteCallback);
    function ReadAsync(ABuffer: Pointer; ACount: NativeInt; AOffset: Int64; constref ACancellationToken: TCancellationToken): specialize IFuture<NativeInt>; overload;
    function ReadAsync(ABuffer: Pointer; ACount: NativeInt; AOffset: Int64 = -1): specialize IFuture<NativeInt>; overload;
    function WriteAsync(ABuffer: Pointer; ACount: NativeInt; AOffset: Int64; constref ACancellationToken: TCancellationToken): specialize IFuture<NativeInt>; overload;
    function WriteAsync(ABuffer: Pointer; ACount: NativeInt; AOffset: Int64 = -1): specialize IFuture<NativeInt>; overload;
    procedure Close;
  end;

implementation

constructor TAsyncFile.Open(const AFileName: string; AMode: Integer; ALoop: IEventLoop);
{$ifdef UNIX}
var
  LFlags: cint;
begin
  inherited Create;
  FLoop := ALoop;
  FFileName := AFileName;
  FFileMode := AMode;
  FOwnsHandle := True;
  FLock := TCriticalSection.Create;

  LFlags := 0;
  if (AMode and fmOpenReadWrite) = fmOpenReadWrite then
    LFlags := O_RDWR
  else if (AMode and fmOpenWrite) = fmOpenWrite then
    LFlags := O_WRONLY
  else
    LFlags := O_RDONLY;

  if (AMode and fmCreate) = fmCreate then
    LFlags := LFlags or O_CREAT or O_TRUNC;

  FHandle := fpopen(PAnsiChar(AFileName), LFlags, &664);
  if FHandle < 0 then
    raise EEventLoopException.CreateFmt('Failed to open file "%s": %s (errno=%d)', [AFileName, SysErrorMessage(fpgeterrno), fpgeterrno]);
end;
{$else}
begin
  inherited Create;
  FLoop := ALoop;
  FFileName := AFileName;
  FFileMode := AMode;
  FOwnsHandle := True;
  FLock := TCriticalSection.Create;
  raise EEventLoopException.Create('AsyncFile not implemented on this platform');
end;
{$endif}

destructor TAsyncFile.Destroy;
begin
  Close;
  FreeAndNil(FLock);
  inherited Destroy;
end;

function TAsyncFile.GetHandle: TLoopHandle;
begin
  Result := FHandle;
end;

function TAsyncFile.GetSize: Int64;
{$ifdef UNIX}
var
  LStat: Stat;
begin
  if (FHandle = InvalidLoopHandle) or (fpfstat(FHandle, LStat) < 0) then
    Result := 0
  else
    Result := LStat.st_size;
end;
{$else}
begin
  Result := 0;
end;
{$endif}

procedure TAsyncFile.AsyncRead(ABuffer: Pointer; ACount: NativeInt; AOffset: Int64; const ACallback: TAsyncReadCallback);
var
  LLoop: IEventLoop;
  LRes: NativeInt;
  LErr: Integer;
begin
  LLoop := FLoop;
  TTask.Run(
    procedure
    begin
      FLock.Acquire;
      try
        {$ifdef UNIX}
        if AOffset >= 0 then
          fplseek(FHandle, AOffset, SEEK_SET);
        LRes := fpread(FHandle, ABuffer^, ACount);
        if LRes >= 0 then
          LErr := 0
        else
          LErr := fpgeterrno;
        {$else}
        LRes := -1;
        LErr := 0;
        {$endif}
      finally
        FLock.Release;
      end;

      if LLoop <> nil then
        LLoop.Post(
          procedure
          begin
            if Assigned(ACallback) then
              ACallback(LRes, LErr);
          end
        )
      else if Assigned(ACallback) then
        ACallback(LRes, LErr);
    end
  );
end;

procedure TAsyncFile.AsyncWrite(ABuffer: Pointer; ACount: NativeInt; AOffset: Int64; const ACallback: TAsyncWriteCallback);
var
  LLoop: IEventLoop;
  LRes: NativeInt;
  LErr: Integer;
begin
  LLoop := FLoop;
  TTask.Run(
    procedure
    begin
      FLock.Acquire;
      try
        {$ifdef UNIX}
        if AOffset >= 0 then
          fplseek(FHandle, AOffset, SEEK_SET);
        LRes := fpwrite(FHandle, ABuffer^, ACount);
        if LRes >= 0 then
          LErr := 0
        else
          LErr := fpgeterrno;
        {$else}
        LRes := -1;
        LErr := 0;
        {$endif}
      finally
        FLock.Release;
      end;

      if LLoop <> nil then
        LLoop.Post(
          procedure
          begin
            if Assigned(ACallback) then
              ACallback(LRes, LErr);
          end
        )
      else if Assigned(ACallback) then
        ACallback(LRes, LErr);
    end
  );
end;

function TAsyncFile.ReadAsync(ABuffer: Pointer; ACount: NativeInt; AOffset: Int64; constref ACancellationToken: TCancellationToken): specialize IFuture<NativeInt>;
begin
  Result := TTask.specialize Future<NativeInt>(
    function: NativeInt
    begin
      if ACancellationToken.IsCancellationRequested then
        ACancellationToken.ThrowIfCancellationRequested;

      FLock.Acquire;
      try
        {$ifdef UNIX}
        if AOffset >= 0 then
          fplseek(FHandle, AOffset, SEEK_SET);
        Result := fpread(FHandle, ABuffer^, ACount);
        if Result < 0 then
          raise EEventLoopException.CreateFmt('File read failed: %s (errno=%d)', [SysErrorMessage(fpgeterrno), fpgeterrno]);
        {$else}
        Result := 0;
        {$endif}
      finally
        FLock.Release;
      end;
    end,
    ACancellationToken,
    nil
  );
end;

function TAsyncFile.ReadAsync(ABuffer: Pointer; ACount: NativeInt; AOffset: Int64): specialize IFuture<NativeInt>;
begin
  Result := ReadAsync(ABuffer, ACount, AOffset, TCancellationToken.None);
end;

function TAsyncFile.WriteAsync(ABuffer: Pointer; ACount: NativeInt; AOffset: Int64; constref ACancellationToken: TCancellationToken): specialize IFuture<NativeInt>;
begin
  Result := TTask.specialize Future<NativeInt>(
    function: NativeInt
    begin
      if ACancellationToken.IsCancellationRequested then
        ACancellationToken.ThrowIfCancellationRequested;

      FLock.Acquire;
      try
        {$ifdef UNIX}
        if AOffset >= 0 then
          fplseek(FHandle, AOffset, SEEK_SET);
        Result := fpwrite(FHandle, ABuffer^, ACount);
        if Result < 0 then
          raise EEventLoopException.CreateFmt('File write failed: %s (errno=%d)', [SysErrorMessage(fpgeterrno), fpgeterrno]);
        {$else}
        Result := 0;
        {$endif}
      finally
        FLock.Release;
      end;
    end,
    ACancellationToken,
    nil
  );
end;

function TAsyncFile.WriteAsync(ABuffer: Pointer; ACount: NativeInt; AOffset: Int64): specialize IFuture<NativeInt>;
begin
  Result := WriteAsync(ABuffer, ACount, AOffset, TCancellationToken.None);
end;

procedure TAsyncFile.Close;
begin
  FLock.Acquire;
  try
    if (FHandle <> InvalidLoopHandle) and FOwnsHandle then
    begin
      {$ifdef UNIX}
      fpclose(FHandle);
      {$endif}
      FHandle := InvalidLoopHandle;
    end;
  finally
    FLock.Release;
  end;
end;

end.
