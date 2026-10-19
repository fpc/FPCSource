{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by the Free Pascal development team

    Scalable Asynchronous I/O Event Loop - Non-blocking Pipes

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.
}
{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

unit eventloop.pipes;

interface

uses
  SysUtils, Classes, eventloop.types, eventloop.core
  {$ifdef UNIX}, baseunix, linux, syscall{$endif}
  {$ifdef MSWINDOWS}, Windows{$endif};

type
  IAsyncPipe = interface
    ['{DCB9B08F-DF31-4F2A-A801-D36C5BE55490}']
    function GetReaderHandle: TLoopHandle;
    function GetWriterHandle: TLoopHandle;
    procedure AsyncRead(ABuffer: Pointer; ACount: NativeInt; const ACallback: TAsyncReadCallback);
    procedure AsyncWrite(ABuffer: Pointer; ACount: NativeInt; const ACallback: TAsyncWriteCallback);
    procedure CloseReader;
    procedure CloseWriter;
    procedure Close;
    property ReaderHandle: TLoopHandle read GetReaderHandle;
    property WriterHandle: TLoopHandle read GetWriterHandle;
  end;

  TAsyncPipe = class(TInterfacedObject, IAsyncPipe)
  private
    FLoop: IEventLoop;
    FReadHandle: TLoopHandle;
    FWriteHandle: TLoopHandle;

    FReadBuffer: Pointer;
    FReadCount: NativeInt;
    FReadCallback: TAsyncReadCallback;

    FWriteBuffer: Pointer;
    FWriteCount: NativeInt;
    FWriteOffset: NativeInt;
    FWriteCallback: TAsyncWriteCallback;

    procedure HandleReadIO(AHandle: TLoopHandle; AEvents: TEventLoopEvents; AUserData: Pointer);
    procedure HandleWriteIO(AHandle: TLoopHandle; AEvents: TEventLoopEvents; AUserData: Pointer);
  public
    constructor Create(ALoop: IEventLoop);
    destructor Destroy; override;

    function GetReaderHandle: TLoopHandle;
    function GetWriterHandle: TLoopHandle;
    procedure AsyncRead(ABuffer: Pointer; ACount: NativeInt; const ACallback: TAsyncReadCallback);
    procedure AsyncWrite(ABuffer: Pointer; ACount: NativeInt; const ACallback: TAsyncWriteCallback);
    procedure CloseReader;
    procedure CloseWriter;
    procedure Close;
  end;

implementation

constructor TAsyncPipe.Create(ALoop: IEventLoop);
{$ifdef UNIX}
var
  LPipes: array[0..1] of cint;
  LRes: cint;
begin
  inherited Create;
  FLoop := ALoop;
  FReadHandle := InvalidLoopHandle;
  FWriteHandle := InvalidLoopHandle;

  LRes := cint(Do_SysCall(syscall_nr_pipe2, TSysParam(@LPipes[0]), O_NONBLOCK or $80000 {O_CLOEXEC}));
  if LRes < 0 then
  begin
    if fppipe(LPipes) < 0 then
      raise EEventLoopException.CreateFmt('fppipe failed: %s (errno=%d)', [SysErrorMessage(fpgeterrno), fpgeterrno]);
    fpfcntl(LPipes[0], F_SETFL, O_NONBLOCK);
    fpfcntl(LPipes[1], F_SETFL, O_NONBLOCK);
  end;

  FReadHandle := LPipes[0];
  FWriteHandle := LPipes[1];

  if FLoop <> nil then
  begin
    FLoop.RegisterHandle(FReadHandle, [], @HandleReadIO, Self);
    FLoop.RegisterHandle(FWriteHandle, [], @HandleWriteIO, Self);
  end;
end;
{$else}
begin
  inherited Create;
  FLoop := ALoop;
  FReadHandle := InvalidLoopHandle;
  FWriteHandle := InvalidLoopHandle;
  raise EEventLoopException.Create('Pipes not implemented on this platform');
end;
{$endif}

destructor TAsyncPipe.Destroy;
begin
  Close;
  inherited Destroy;
end;

function TAsyncPipe.GetReaderHandle: TLoopHandle;
begin
  Result := FReadHandle;
end;

function TAsyncPipe.GetWriterHandle: TLoopHandle;
begin
  Result := FWriteHandle;
end;

procedure TAsyncPipe.AsyncRead(ABuffer: Pointer; ACount: NativeInt; const ACallback: TAsyncReadCallback);
begin
  FReadBuffer := ABuffer;
  FReadCount := ACount;
  FReadCallback := ACallback;
  if FLoop <> nil then
    FLoop.ModifyHandle(FReadHandle, [eleRead]);
end;

procedure TAsyncPipe.AsyncWrite(ABuffer: Pointer; ACount: NativeInt; const ACallback: TAsyncWriteCallback);
{$ifdef UNIX}
var
  LBytes: NativeInt;
{$endif}
begin
  FWriteBuffer := ABuffer;
  FWriteCount := ACount;
  FWriteOffset := 0;
  FWriteCallback := ACallback;

  {$ifdef UNIX}
  if FWriteHandle <> InvalidLoopHandle then
  begin
    LBytes := fpwrite(FWriteHandle, ABuffer^, ACount);
    if LBytes > 0 then
    begin
      FWriteOffset := LBytes;
      if FWriteOffset >= FWriteCount then
      begin
        if Assigned(FWriteCallback) then
        begin
          FWriteCallback := nil;
          ACallback(FWriteOffset, 0);
        end;
        Exit;
      end;
    end;
  end;
  {$endif}

  if FLoop <> nil then
    FLoop.ModifyHandle(FWriteHandle, [eleWrite]);
end;

procedure TAsyncPipe.HandleReadIO(AHandle: TLoopHandle; AEvents: TEventLoopEvents; AUserData: Pointer);
var
  LBytes: NativeInt;
  LCb: TAsyncReadCallback;
  LErr: cint;
begin
  if (eleRead in AEvents) and Assigned(FReadCallback) then
  begin
    {$ifdef UNIX}
    LBytes := fpread(FReadHandle, FReadBuffer^, FReadCount);
    if LBytes >= 0 then
    begin
      LCb := FReadCallback;
      FReadCallback := nil;
      if FLoop <> nil then
        FLoop.ModifyHandle(FReadHandle, []);
      LCb(LBytes, 0);
    end
    else
    begin
      LErr := fpgeterrno;
      if (LErr <> ESysEAGAIN) and (LErr <> ESysEWOULDBLOCK) and (LErr <> ESysEINTR) then
      begin
        LCb := FReadCallback;
        FReadCallback := nil;
        if FLoop <> nil then
          FLoop.ModifyHandle(FReadHandle, []);
        LCb(-1, LErr);
      end;
    end;
    {$endif}
  end;
end;

procedure TAsyncPipe.HandleWriteIO(AHandle: TLoopHandle; AEvents: TEventLoopEvents; AUserData: Pointer);
var
  LBytes: NativeInt;
  LCb: TAsyncWriteCallback;
  LErr: cint;
begin
  if (eleWrite in AEvents) and Assigned(FWriteCallback) then
  begin
    {$ifdef UNIX}
    LBytes := fpwrite(FWriteHandle, (PByte(FWriteBuffer) + FWriteOffset)^, FWriteCount - FWriteOffset);
    if LBytes > 0 then
    begin
      Inc(FWriteOffset, LBytes);
      if FWriteOffset >= FWriteCount then
      begin
        LCb := FWriteCallback;
        FWriteCallback := nil;
        if FLoop <> nil then
          FLoop.ModifyHandle(FWriteHandle, []);
        LCb(FWriteOffset, 0);
      end;
    end
    else
    begin
      LErr := fpgeterrno;
      if (LErr <> ESysEAGAIN) and (LErr <> ESysEWOULDBLOCK) and (LErr <> ESysEINTR) then
      begin
        LCb := FWriteCallback;
        FWriteCallback := nil;
        if FLoop <> nil then
          FLoop.ModifyHandle(FWriteHandle, []);
        LCb(-1, LErr);
      end;
    end;
    {$endif}
  end;
end;

procedure TAsyncPipe.CloseReader;
begin
  if FReadHandle <> InvalidLoopHandle then
  begin
    if FLoop <> nil then
      FLoop.UnregisterHandle(FReadHandle);
    {$ifdef UNIX}
    fpclose(FReadHandle);
    {$endif}
    FReadHandle := InvalidLoopHandle;
  end;
end;

procedure TAsyncPipe.CloseWriter;
begin
  if FWriteHandle <> InvalidLoopHandle then
  begin
    if FLoop <> nil then
      FLoop.UnregisterHandle(FWriteHandle);
    {$ifdef UNIX}
    fpclose(FWriteHandle);
    {$endif}
    FWriteHandle := InvalidLoopHandle;
  end;
end;

procedure TAsyncPipe.Close;
begin
  CloseReader;
  CloseWriter;
end;

end.
