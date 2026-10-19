{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by the Free Pascal development team

    Scalable Asynchronous I/O Event Loop - Non-blocking TCP/UDP Sockets

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.
}
{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}
{$modeswitch advancedrecords}

unit eventloop.sockets;

interface

uses
  SysUtils, Classes, SyncObjs, System.Threading,
  eventloop.types, eventloop.core
  {$ifdef UNIX}, baseunix, sockets{$endif}
  {$ifdef MSWINDOWS}, Windows, WinSock2{$endif};

type
  IAsyncSocket = interface;

  { Non-blocking client / peer connection socket }
  IAsyncSocket = interface
    ['{84B2FE68-B234-4C23-8B37-646A94E27B10}']
    function GetHandle: TLoopHandle;
    function IsConnected: Boolean;
    procedure SetNoDelay(AValue: Boolean);
    procedure SetKeepAlive(AValue: Boolean);
    procedure AsyncConnect(const AHost: string; APort: Word; const ACallback: TAsyncCallback);
    procedure AsyncRead(ABuffer: Pointer; ACount: NativeInt; const ACallback: TAsyncReadCallback); overload;
    procedure AsyncRead(ABuffer: Pointer; ACount: NativeInt; const AProc: TAsyncReadProc); overload;
    procedure AsyncWrite(ABuffer: Pointer; ACount: NativeInt; const ACallback: TAsyncWriteCallback); overload;
    procedure AsyncWrite(ABuffer: Pointer; ACount: NativeInt; const AProc: TAsyncWriteProc); overload;
    function ReadAsync(ABuffer: Pointer; ACount: NativeInt; constref ACancellationToken: TCancellationToken): specialize IFuture<NativeInt>; overload;
    function ReadAsync(ABuffer: Pointer; ACount: NativeInt): specialize IFuture<NativeInt>; overload;
    function WriteAsync(ABuffer: Pointer; ACount: NativeInt; constref ACancellationToken: TCancellationToken): specialize IFuture<NativeInt>; overload;
    function WriteAsync(ABuffer: Pointer; ACount: NativeInt): specialize IFuture<NativeInt>; overload;
    procedure Close;
    property Handle: TLoopHandle read GetHandle;
    property Connected: Boolean read IsConnected;
  end;

  { Non-blocking server listening socket }
  IAsyncServerSocket = interface
    ['{91A7CB3F-7A9B-4D88-BBE8-4D2A572F8B70}']
    function GetHandle: TLoopHandle;
    function GetPort: Word;
    procedure Listen(const AHost: string; APort: Word = 0; ABacklog: Integer = 1024; AReusePort: Boolean = True);
    procedure AsyncAccept(const ACallback: TAsyncAcceptCallback); overload;
    procedure AsyncAccept(const AProc: TAsyncAcceptProc); overload;
    function AcceptAsync(constref ACancellationToken: TCancellationToken): specialize IFuture<IAsyncSocket>; overload;
    function AcceptAsync: specialize IFuture<IAsyncSocket>; overload;
    procedure Close;
    property Handle: TLoopHandle read GetHandle;
    property Port: Word read GetPort;
  end;

  { Non-blocking UDP datagram socket }
  IAsyncDatagramSocket = interface
    ['{3A94D290-E577-448F-A392-D08C19485C66}']
    function GetHandle: TLoopHandle;
    procedure Bind(const AHost: string; APort: Word);
    function SendTo(const AHost: string; APort: Word; ABuffer: Pointer; ACount: Integer): Integer;
    procedure Close;
    property Handle: TLoopHandle read GetHandle;
  end;

  { Implementation classes }
  TAsyncSocket = class(TInterfacedObject, IAsyncSocket)
  private
    FHandle: TLoopHandle;
    FLoop: IEventLoop;
    FOwnsHandle: Boolean;
    FConnected: Boolean;
    FConnecting: Boolean;
    FConnectCallback: TAsyncCallback;

    { Pending read }
    FReadBuffer: Pointer;
    FReadCount: NativeInt;
    FReadCallback: TAsyncReadCallback;
    FReadProc: TAsyncReadProc;

    { Pending write }
    FWriteBuffer: Pointer;
    FWriteCount: NativeInt;
    FWriteOffset: NativeInt;
    FWriteCallback: TAsyncWriteCallback;
    FWriteProc: TAsyncWriteProc;

    procedure HandleIO(AHandle: TLoopHandle; AEvents: TEventLoopEvents; AUserData: Pointer);
    procedure SetNonBlocking(AHandle: TLoopHandle);
    procedure UpdateLoopEvents;
  public
    constructor Create(ALoop: IEventLoop; AHandle: TLoopHandle = -1; AConnected: Boolean = False);
    destructor Destroy; override;

    function GetHandle: TLoopHandle;
    function IsConnected: Boolean;
    procedure SetNoDelay(AValue: Boolean);
    procedure SetKeepAlive(AValue: Boolean);
    procedure AsyncConnect(const AHost: string; APort: Word; const ACallback: TAsyncCallback);
    procedure AsyncRead(ABuffer: Pointer; ACount: NativeInt; const ACallback: TAsyncReadCallback); overload;
    procedure AsyncRead(ABuffer: Pointer; ACount: NativeInt; const AProc: TAsyncReadProc); overload;
    procedure AsyncWrite(ABuffer: Pointer; ACount: NativeInt; const ACallback: TAsyncWriteCallback); overload;
    procedure AsyncWrite(ABuffer: Pointer; ACount: NativeInt; const AProc: TAsyncWriteProc); overload;
    function ReadAsync(ABuffer: Pointer; ACount: NativeInt; constref ACancellationToken: TCancellationToken): specialize IFuture<NativeInt>; overload;
    function ReadAsync(ABuffer: Pointer; ACount: NativeInt): specialize IFuture<NativeInt>; overload;
    function WriteAsync(ABuffer: Pointer; ACount: NativeInt; constref ACancellationToken: TCancellationToken): specialize IFuture<NativeInt>; overload;
    function WriteAsync(ABuffer: Pointer; ACount: NativeInt): specialize IFuture<NativeInt>; overload;
    procedure Close;
  end;

  TAsyncServerSocket = class(TInterfacedObject, IAsyncServerSocket)
  private
    FHandle: TLoopHandle;
    FLoop: IEventLoop;
    FAcceptCallback: TAsyncAcceptCallback;
    FAcceptProc: TAsyncAcceptProc;

    procedure HandleIO(AHandle: TLoopHandle; AEvents: TEventLoopEvents; AUserData: Pointer);
    procedure SetNonBlocking(AHandle: TLoopHandle);
  public
    constructor Create(ALoop: IEventLoop);
    destructor Destroy; override;

    function GetHandle: TLoopHandle;
    function GetPort: Word;
    procedure Listen(const AHost: string; APort: Word = 0; ABacklog: Integer = 1024; AReusePort: Boolean = True);
    procedure AsyncAccept(const ACallback: TAsyncAcceptCallback); overload;
    procedure AsyncAccept(const AProc: TAsyncAcceptProc); overload;
    function AcceptAsync(constref ACancellationToken: TCancellationToken): specialize IFuture<IAsyncSocket>; overload;
    function AcceptAsync: specialize IFuture<IAsyncSocket>; overload;
    procedure Close;
    property Port: Word read GetPort;
  end;

implementation

const
  {$ifdef UNIX}
  SO_REUSEPORT_CONST = 15;
  TCP_NODELAY_CONST = 1;
  {$endif}
  {$ifdef MSWINDOWS}
  SO_REUSEPORT_CONST = 0;
  TCP_NODELAY_CONST = 1;
  {$endif}

{ Helper procedures }

procedure SetSocketNonBlocking(AHandle: TLoopHandle);
{$ifdef UNIX}
var
  LFlags: cint;
begin
  LFlags := fpfcntl(AHandle, F_GETFL, 0);
  if LFlags >= 0 then
    fpfcntl(AHandle, F_SETFL, LFlags or O_NONBLOCK);
end;
{$endif}
{$ifdef MSWINDOWS}
var
  LMode: u_long;
begin
  LMode := 1;
  ioctlsocket(AHandle, FIONBIO, @LMode);
end;
{$endif}

{ TAsyncSocket }

constructor TAsyncSocket.Create(ALoop: IEventLoop; AHandle: TLoopHandle; AConnected: Boolean);
begin
  inherited Create;
  FLoop := ALoop;
  FHandle := AHandle;
  FConnected := AConnected;
  FConnecting := False;
  FOwnsHandle := True;
  FReadBuffer := nil;
  FReadCount := 0;
  FReadCallback := nil;
  FWriteBuffer := nil;
  FWriteCount := 0;
  FWriteOffset := 0;
  FWriteCallback := nil;

  if FHandle <> InvalidLoopHandle then
  begin
    SetNonBlocking(FHandle);
    if FLoop <> nil then
      FLoop.RegisterHandle(FHandle, [], @HandleIO, Self);
  end;
end;

destructor TAsyncSocket.Destroy;
begin
  Close;
  inherited Destroy;
end;

function TAsyncSocket.GetHandle: TLoopHandle;
begin
  Result := FHandle;
end;

function TAsyncSocket.IsConnected: Boolean;
begin
  Result := FConnected;
end;

procedure TAsyncSocket.SetNonBlocking(AHandle: TLoopHandle);
begin
  SetSocketNonBlocking(AHandle);
end;

procedure TAsyncSocket.SetNoDelay(AValue: Boolean);
var
  LOpt: cint;
begin
  if FHandle = InvalidLoopHandle then
    Exit;
  LOpt := Ord(AValue);
  {$ifdef UNIX}
  fpsetsockopt(FHandle, 6 {IPPROTO_TCP}, TCP_NODELAY_CONST, @LOpt, SizeOf(LOpt));
  {$endif}
  {$ifdef MSWINDOWS}
  setsockopt(FHandle, IPPROTO_TCP, TCP_NODELAY, PAnsiChar(@LOpt), SizeOf(LOpt));
  {$endif}
end;

procedure TAsyncSocket.SetKeepAlive(AValue: Boolean);
var
  LOpt: cint;
begin
  if FHandle = InvalidLoopHandle then
    Exit;
  LOpt := Ord(AValue);
  {$ifdef UNIX}
  fpsetsockopt(FHandle, SOL_SOCKET, SO_KEEPALIVE, @LOpt, SizeOf(LOpt));
  {$endif}
  {$ifdef MSWINDOWS}
  setsockopt(FHandle, SOL_SOCKET, SO_KEEPALIVE, PAnsiChar(@LOpt), SizeOf(LOpt));
  {$endif}
end;

procedure TAsyncSocket.UpdateLoopEvents;
var
  LEvents: TEventLoopEvents;
begin
  if (FHandle = InvalidLoopHandle) or (FLoop = nil) then
    Exit;
  LEvents := [];
  if FConnecting then
    LEvents := [eleWrite]
  else
  begin
    if Assigned(FReadCallback) or Assigned(FReadProc) then
      Include(LEvents, eleRead);
    if Assigned(FWriteCallback) or Assigned(FWriteProc) then
      Include(LEvents, eleWrite);
  end;
  FLoop.ModifyHandle(FHandle, LEvents);
end;

procedure TAsyncSocket.AsyncConnect(const AHost: string; APort: Word; const ACallback: TAsyncCallback);
var
  LAddr: TInetSockAddr;
  LRes: cint;
  LErr: cint;
begin
  if FHandle = InvalidLoopHandle then
  begin
    {$ifdef UNIX}
    FHandle := fpsocket(AF_INET, SOCK_STREAM, 0);
    {$endif}
    {$ifdef MSWINDOWS}
    FHandle := socket(AF_INET, SOCK_STREAM, 0);
    {$endif}
    if FHandle = InvalidLoopHandle then
      raise ESocketException.Create('Failed to create socket');
    SetNonBlocking(FHandle);
    if FLoop <> nil then
      FLoop.RegisterHandle(FHandle, [], @HandleIO, Self);
  end;

  FConnectCallback := ACallback;
  FConnecting := True;

  FillChar(LAddr, SizeOf(LAddr), 0);
  LAddr.sin_family := AF_INET;
  LAddr.sin_port := htons(APort);
  if (AHost = '') or (AHost = '127.0.0.1') or (AHost = 'localhost') then
    LAddr.sin_addr := StrToNetAddr('127.0.0.1')
  else
    LAddr.sin_addr := StrToNetAddr(AHost);

  {$ifdef UNIX}
  LRes := fpconnect(FHandle, @LAddr, SizeOf(LAddr));
  if LRes = 0 then
  begin
    { Connected synchronously }
    FConnecting := False;
    FConnected := True;
    if Assigned(FConnectCallback) then
      FConnectCallback();
  end
  else
  begin
    LErr := fpgeterrno;
    if (LErr = ESysEINPROGRESS) or (LErr = ESysEWOULDBLOCK) then
      UpdateLoopEvents
    else
    begin
      FConnecting := False;
      raise ESocketException.CreateFmt('Connect failed: %s (errno=%d)', [SysErrorMessage(LErr), LErr]);
    end;
  end;
  {$endif}
end;

procedure TAsyncSocket.AsyncRead(ABuffer: Pointer; ACount: NativeInt; const ACallback: TAsyncReadCallback);
begin
  FReadBuffer := ABuffer;
  FReadCount := ACount;
  FReadCallback := ACallback;
  FReadProc := nil;
  UpdateLoopEvents;
end;

procedure TAsyncSocket.AsyncRead(ABuffer: Pointer; ACount: NativeInt; const AProc: TAsyncReadProc);
begin
  FReadBuffer := ABuffer;
  FReadCount := ACount;
  FReadCallback := nil;
  FReadProc := AProc;
  UpdateLoopEvents;
end;

procedure TAsyncSocket.AsyncWrite(ABuffer: Pointer; ACount: NativeInt; const ACallback: TAsyncWriteCallback);
{$ifdef UNIX}
var
  LBytes: NativeInt;
{$endif}
begin
  FWriteBuffer := ABuffer;
  FWriteCount := ACount;
  FWriteOffset := 0;
  FWriteCallback := ACallback;
  FWriteProc := nil;

  {$ifdef UNIX}
  if (FHandle <> InvalidLoopHandle) and FConnected and (not FConnecting) then
  begin
    LBytes := fpsend(FHandle, ABuffer, ACount, 0);
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

  UpdateLoopEvents;
end;

procedure TAsyncSocket.AsyncWrite(ABuffer: Pointer; ACount: NativeInt; const AProc: TAsyncWriteProc);
{$ifdef UNIX}
var
  LBytes: NativeInt;
{$endif}
begin
  FWriteBuffer := ABuffer;
  FWriteCount := ACount;
  FWriteOffset := 0;
  FWriteCallback := nil;
  FWriteProc := AProc;

  {$ifdef UNIX}
  if (FHandle <> InvalidLoopHandle) and FConnected and (not FConnecting) then
  begin
    LBytes := fpsend(FHandle, ABuffer, ACount, 0);
    if LBytes > 0 then
    begin
      FWriteOffset := LBytes;
      if FWriteOffset >= FWriteCount then
      begin
        if Assigned(FWriteProc) then
        begin
          FWriteProc := nil;
          AProc(FWriteOffset, 0);
        end;
        Exit;
      end;
    end;
  end;
  {$endif}

  UpdateLoopEvents;
end;

procedure TAsyncSocket.HandleIO(AHandle: TLoopHandle; AEvents: TEventLoopEvents; AUserData: Pointer);
var
  LBytes: NativeInt;
  LReadCb: TAsyncReadCallback;
  LReadProc: TAsyncReadProc;
  LWriteCb: TAsyncWriteCallback;
  LWriteProc: TAsyncWriteProc;
  LConnCb: TAsyncCallback;
  LErr: cint;
  LOptLen: TSockLen;
  LVal: cint;
begin
  if FConnecting then
  begin
    FConnecting := False;
    {$ifdef UNIX}
    LOptLen := SizeOf(LVal);
    LVal := 0;
    fpgetsockopt(FHandle, SOL_SOCKET, SO_ERROR, @LVal, @LOptLen);
    if LVal = 0 then
    begin
      FConnected := True;
      LConnCb := FConnectCallback;
      FConnectCallback := nil;
      UpdateLoopEvents;
      if Assigned(LConnCb) then
        LConnCb();
    end
    else
    begin
      Close;
      raise ESocketException.CreateFmt('Async connect failed with error %d', [LVal]);
    end;
    {$endif}
    Exit;
  end;

  { Read handling }
  if (eleRead in AEvents) and (Assigned(FReadCallback) or Assigned(FReadProc)) then
  begin
    {$ifdef UNIX}
    LBytes := fprecv(FHandle, FReadBuffer, FReadCount, 0);
    if LBytes > 0 then
    begin
      LReadCb := FReadCallback;
      LReadProc := FReadProc;
      FReadCallback := nil;
      FReadProc := nil;
      UpdateLoopEvents;
      if Assigned(LReadCb) then
        LReadCb(LBytes, 0)
      else if Assigned(LReadProc) then
        LReadProc(LBytes, 0);
    end
    else if LBytes = 0 then
    begin
      { Remote EOF }
      LReadCb := FReadCallback;
      LReadProc := FReadProc;
      FReadCallback := nil;
      FReadProc := nil;
      UpdateLoopEvents;
      if Assigned(LReadCb) then
        LReadCb(0, 0)
      else if Assigned(LReadProc) then
        LReadProc(0, 0);
    end
    else
    begin
      LErr := fpgeterrno;
      if (LErr <> ESysEAGAIN) and (LErr <> ESysEWOULDBLOCK) and (LErr <> ESysEINTR) then
      begin
        LReadCb := FReadCallback;
        LReadProc := FReadProc;
        FReadCallback := nil;
        FReadProc := nil;
        UpdateLoopEvents;
        if Assigned(LReadCb) then
          LReadCb(-1, LErr)
        else if Assigned(LReadProc) then
          LReadProc(-1, LErr);
      end;
    end;
    {$endif}
  end;

  { Write handling }
  if (eleWrite in AEvents) and (Assigned(FWriteCallback) or Assigned(FWriteProc)) then
  begin
    {$ifdef UNIX}
    LBytes := fpsend(FHandle, PByte(FWriteBuffer) + FWriteOffset, FWriteCount - FWriteOffset, 0);
    if LBytes > 0 then
    begin
      Inc(FWriteOffset, LBytes);
      if FWriteOffset >= FWriteCount then
      begin
        LWriteCb := FWriteCallback;
        LWriteProc := FWriteProc;
        FWriteCallback := nil;
        FWriteProc := nil;
        UpdateLoopEvents;
        if Assigned(LWriteCb) then
          LWriteCb(FWriteOffset, 0)
        else if Assigned(LWriteProc) then
          LWriteProc(FWriteOffset, 0);
      end;
    end
    else
    begin
      LErr := fpgeterrno;
      if (LErr <> ESysEAGAIN) and (LErr <> ESysEWOULDBLOCK) and (LErr <> ESysEINTR) then
      begin
        LWriteCb := FWriteCallback;
        LWriteProc := FWriteProc;
        FWriteCallback := nil;
        FWriteProc := nil;
        UpdateLoopEvents;
        if Assigned(LWriteCb) then
          LWriteCb(-1, LErr)
        else if Assigned(LWriteProc) then
          LWriteProc(-1, LErr);
      end;
    end;
    {$endif}
  end;

  if (eleHangup in AEvents) or (eleError in AEvents) then
  begin
    if Assigned(FReadCallback) or Assigned(FReadProc) then
    begin
      LReadCb := FReadCallback;
      LReadProc := FReadProc;
      FReadCallback := nil;
      FReadProc := nil;
      if Assigned(LReadCb) then
        LReadCb(0, 0)
      else if Assigned(LReadProc) then
        LReadProc(0, 0);
    end;
  end;
end;

type
  { Helper class for Future Read/Write adapters }
  TReadAsyncHelper = class
  private
    FBytes: NativeInt;
    FStatus: Integer;
    FDone: Boolean;
    FLock: TCriticalSection;
    FCond: PRTLEvent;
    procedure OnComplete(ABytes: NativeInt; AStatus: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    function WaitResult: NativeInt;
  end;

constructor TReadAsyncHelper.Create;
begin
  inherited Create;
  FBytes := 0;
  FStatus := 0;
  FDone := False;
  FLock := TCriticalSection.Create;
  FCond := RTLEventCreate;
end;

destructor TReadAsyncHelper.Destroy;
begin
  RTLEventDestroy(FCond);
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TReadAsyncHelper.OnComplete(ABytes: NativeInt; AStatus: Integer);
begin
  FLock.Acquire;
  try
    FBytes := ABytes;
    FStatus := AStatus;
    FDone := True;
    RTLEventSetEvent(FCond);
  finally
    FLock.Release;
  end;
end;

function TReadAsyncHelper.WaitResult: NativeInt;
begin
  FLock.Acquire;
  try
    while not FDone do
    begin
      FLock.Release;
      RTLEventWaitFor(FCond);
      FLock.Acquire;
    end;
    if FStatus <> 0 then
      raise ESocketException.CreateFmt('Async socket error %d', [FStatus]);
    Result := FBytes;
  finally
    FLock.Release;
  end;
end;

function TAsyncSocket.ReadAsync(ABuffer: Pointer; ACount: NativeInt; constref ACancellationToken: TCancellationToken): specialize IFuture<NativeInt>;
var
  LSelf: IAsyncSocket;
begin
  LSelf := Self;
  Result := TTask.specialize Future<NativeInt>(
    function: NativeInt
    var
      LHelper: TReadAsyncHelper;
    begin
      if ACancellationToken.IsCancellationRequested then
        ACancellationToken.ThrowIfCancellationRequested;

      LHelper := TReadAsyncHelper.Create;
      try
        LSelf.AsyncRead(ABuffer, ACount, @LHelper.OnComplete);
        Result := LHelper.WaitResult;
      finally
        LHelper.Free;
      end;
    end,
    ACancellationToken,
    nil
  );
end;

function TAsyncSocket.ReadAsync(ABuffer: Pointer; ACount: NativeInt): specialize IFuture<NativeInt>;
begin
  Result := ReadAsync(ABuffer, ACount, TCancellationToken.None);
end;

function TAsyncSocket.WriteAsync(ABuffer: Pointer; ACount: NativeInt; constref ACancellationToken: TCancellationToken): specialize IFuture<NativeInt>;
var
  LSelf: IAsyncSocket;
begin
  LSelf := Self;
  Result := TTask.specialize Future<NativeInt>(
    function: NativeInt
    var
      LHelper: TReadAsyncHelper;
    begin
      if ACancellationToken.IsCancellationRequested then
        ACancellationToken.ThrowIfCancellationRequested;

      LHelper := TReadAsyncHelper.Create;
      try
        LSelf.AsyncWrite(ABuffer, ACount, @LHelper.OnComplete);
        Result := LHelper.WaitResult;
      finally
        LHelper.Free;
      end;
    end,
    ACancellationToken,
    nil
  );
end;

function TAsyncSocket.WriteAsync(ABuffer: Pointer; ACount: NativeInt): specialize IFuture<NativeInt>;
begin
  Result := WriteAsync(ABuffer, ACount, TCancellationToken.None);
end;

procedure TAsyncSocket.Close;
begin
  if (FHandle <> InvalidLoopHandle) and FOwnsHandle then
  begin
    if FLoop <> nil then
      FLoop.UnregisterHandle(FHandle);
    {$ifdef UNIX}
    fpclose(FHandle);
    {$endif}
    {$ifdef MSWINDOWS}
    closesocket(FHandle);
    {$endif}
    FHandle := InvalidLoopHandle;
  end;
  FConnected := False;
  FConnecting := False;
end;

{ TAsyncServerSocket }

constructor TAsyncServerSocket.Create(ALoop: IEventLoop);
begin
  inherited Create;
  FLoop := ALoop;
  FHandle := InvalidLoopHandle;
  FAcceptCallback := nil;
end;

destructor TAsyncServerSocket.Destroy;
begin
  Close;
  inherited Destroy;
end;

function TAsyncServerSocket.GetHandle: TLoopHandle;
begin
  Result := FHandle;
end;

function TAsyncServerSocket.GetPort: Word;
var
  LAddr: TInetSockAddr;
  LLen: TSockLen;
begin
  Result := 0;
  if FHandle = InvalidLoopHandle then
    Exit;
  LLen := SizeOf(LAddr);
  FillChar(LAddr, SizeOf(LAddr), 0);
  {$ifdef UNIX}
  if fpgetsockname(FHandle, @LAddr, @LLen) = 0 then
    Result := ntohs(LAddr.sin_port);
  {$endif}
  {$ifdef MSWINDOWS}
  if getsockname(FHandle, TSockAddr(LAddr), LLen) = 0 then
    Result := ntohs(LAddr.sin_port);
  {$endif}
end;

procedure TAsyncServerSocket.SetNonBlocking(AHandle: TLoopHandle);
begin
  SetSocketNonBlocking(AHandle);
end;

procedure TAsyncServerSocket.Listen(const AHost: string; APort: Word; ABacklog: Integer; AReusePort: Boolean);
var
  LAddr: TInetSockAddr;
  LOpt: cint;
begin
  Close;

  {$ifdef UNIX}
  FHandle := fpsocket(AF_INET, SOCK_STREAM, 0);
  if FHandle < 0 then
    raise ESocketException.CreateFmt('Socket creation failed: %s (errno=%d)', [SysErrorMessage(fpgeterrno), fpgeterrno]);

  LOpt := 1;
  fpsetsockopt(FHandle, SOL_SOCKET, SO_REUSEADDR, @LOpt, SizeOf(LOpt));
  if AReusePort then
    fpsetsockopt(FHandle, SOL_SOCKET, SO_REUSEPORT_CONST, @LOpt, SizeOf(LOpt));

  SetNonBlocking(FHandle);

  FillChar(LAddr, SizeOf(LAddr), 0);
  LAddr.sin_family := AF_INET;
  LAddr.sin_port := htons(APort);
  if (AHost = '') or (AHost = '0.0.0.0') then
    LAddr.sin_addr.s_addr := 0
  else
    LAddr.sin_addr := StrToNetAddr(AHost);

  if fpbind(FHandle, @LAddr, SizeOf(LAddr)) < 0 then
    raise ESocketException.CreateFmt('Bind to %s:%d failed: %s (errno=%d)', [AHost, APort, SysErrorMessage(fpgeterrno), fpgeterrno]);

  if fplisten(FHandle, ABacklog) < 0 then
    raise ESocketException.CreateFmt('Listen failed: %s (errno=%d)', [SysErrorMessage(fpgeterrno), fpgeterrno]);

  if FLoop <> nil then
    FLoop.RegisterHandle(FHandle, [eleRead], @HandleIO, Self);
  {$endif}
end;

procedure TAsyncServerSocket.AsyncAccept(const ACallback: TAsyncAcceptCallback);
begin
  FAcceptCallback := ACallback;
  FAcceptProc := nil;
end;

procedure TAsyncServerSocket.AsyncAccept(const AProc: TAsyncAcceptProc);
begin
  FAcceptCallback := nil;
  FAcceptProc := AProc;
end;

procedure TAsyncServerSocket.HandleIO(AHandle: TLoopHandle; AEvents: TEventLoopEvents; AUserData: Pointer);
var
  LClient: cint;
  LAddr: TInetSockAddr;
  LAddrLen: TSockLen;
  LErr: cint;
begin
  if (eleRead in AEvents) and (Assigned(FAcceptCallback) or Assigned(FAcceptProc)) then
  begin
    {$ifdef UNIX}
    while True do
    begin
      LAddrLen := SizeOf(LAddr);
      LClient := fpaccept(FHandle, @LAddr, @LAddrLen);
      if LClient >= 0 then
      begin
        SetNonBlocking(LClient);
        if Assigned(FAcceptCallback) then
          FAcceptCallback(LClient, NetAddrToStr(LAddr.sin_addr), ntohs(LAddr.sin_port), 0)
        else if Assigned(FAcceptProc) then
          FAcceptProc(LClient, NetAddrToStr(LAddr.sin_addr), ntohs(LAddr.sin_port), 0);
      end
      else
      begin
        LErr := fpgeterrno;
        if (LErr = ESysEAGAIN) or (LErr = ESysEWOULDBLOCK) or (LErr = ESysEINTR) then
          Break
        else
        begin
          if Assigned(FAcceptCallback) then
            FAcceptCallback(InvalidLoopHandle, '', 0, LErr)
          else if Assigned(FAcceptProc) then
            FAcceptProc(InvalidLoopHandle, '', 0, LErr);
          Break;
        end;
      end;
    end;
    {$endif}
  end;
end;

type
  { Accept future helper }
  TAcceptHelper = class
  private
    FSocket: IAsyncSocket;
    FLoop: IEventLoop;
    FDone: Boolean;
    FLock: TCriticalSection;
    FCond: PRTLEvent;
    procedure OnAccept(AClientHandle: TLoopHandle; const AClientAddr: string; AClientPort: Word; AStatus: Integer);
  public
    constructor Create(ALoop: IEventLoop);
    destructor Destroy; override;
    function WaitSocket: IAsyncSocket;
  end;

constructor TAcceptHelper.Create(ALoop: IEventLoop);
begin
  inherited Create;
  FLoop := ALoop;
  FSocket := nil;
  FDone := False;
  FLock := TCriticalSection.Create;
  FCond := RTLEventCreate;
end;

destructor TAcceptHelper.Destroy;
begin
  RTLEventDestroy(FCond);
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TAcceptHelper.OnAccept(AClientHandle: TLoopHandle; const AClientAddr: string; AClientPort: Word; AStatus: Integer);
begin
  FLock.Acquire;
  try
    if AStatus = 0 then
      FSocket := TAsyncSocket.Create(FLoop, AClientHandle, True);
    FDone := True;
    RTLEventSetEvent(FCond);
  finally
    FLock.Release;
  end;
end;

function TAcceptHelper.WaitSocket: IAsyncSocket;
begin
  FLock.Acquire;
  try
    while not FDone do
    begin
      FLock.Release;
      RTLEventWaitFor(FCond);
      FLock.Acquire;
    end;
    Result := FSocket;
  finally
    FLock.Release;
  end;
end;

function TAsyncServerSocket.AcceptAsync(constref ACancellationToken: TCancellationToken): specialize IFuture<IAsyncSocket>;
var
  LServer: IAsyncServerSocket;
  LLoop: IEventLoop;
begin
  LServer := Self;
  LLoop := FLoop;
  Result := TTask.specialize Future<IAsyncSocket>(
    function: IAsyncSocket
    var
      LHelper: TAcceptHelper;
    begin
      if ACancellationToken.IsCancellationRequested then
        ACancellationToken.ThrowIfCancellationRequested;

      LHelper := TAcceptHelper.Create(LLoop);
      try
        LServer.AsyncAccept(@LHelper.OnAccept);
        Result := LHelper.WaitSocket;
      finally
        LHelper.Free;
      end;
    end,
    ACancellationToken,
    nil
  );
end;

function TAsyncServerSocket.AcceptAsync: specialize IFuture<IAsyncSocket>;
begin
  Result := AcceptAsync(TCancellationToken.None);
end;

procedure TAsyncServerSocket.Close;
begin
  if FHandle <> InvalidLoopHandle then
  begin
    if FLoop <> nil then
      FLoop.UnregisterHandle(FHandle);
    {$ifdef UNIX}
    fpclose(FHandle);
    {$endif}
    {$ifdef MSWINDOWS}
    closesocket(FHandle);
    {$endif}
    FHandle := InvalidLoopHandle;
  end;
end;

end.
