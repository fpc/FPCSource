{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by the Free Pascal development team

    Scalable Asynchronous I/O Event Loop - Unit Tests (fpcunit)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.
}
{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}
{$modeswitch advancedrecords}

unit uteventloop;

interface

uses
  SysUtils, Classes, SyncObjs, fpcunit, testregistry, System.Threading,
  eventloop.types, eventloop.core, eventloop.sockets, eventloop.pipes, eventloop.files, fcleventloop;

type
  { Core EventLoop functionality tests }
  TTestEventLoopCore = class(TTestCase)
  private
    FLoop: IEventLoop;
    FTimerFiredCount: Integer;
    FLastTimerId: Int64;
    procedure OnTimerCallback(ATimerId: Int64);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateAndDestroy;
    procedure TestDeferredTasks;
    procedure TestDeferredOrder;
    procedure TestOneShotTimer;
    procedure TestPeriodicTimer;
    procedure TestCancelTimer;
    procedure TestWakeupFromThread;
  end;

  { Non-blocking pipes tests }
  TTestPipes = class(TTestCase)
  private
    FLoop: IEventLoop;
    FPipe: IAsyncPipe;
    FReadBuffer: array[0..255] of AnsiChar;
    FBytesRead: NativeInt;
    FReadStatus: Integer;
    FReadDone: Boolean;
    procedure OnPipeRead(ABytesRead: NativeInt; AStatus: Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPipeWriteAndRead;
  end;

  { Non-blocking TCP sockets tests }
  TTestSockets = class(TTestCase)
  private
    FLoop: IEventLoop;
    FServer: IAsyncServerSocket;
    FClient: IAsyncSocket;
    FServerPeer: IAsyncSocket;
    FAcceptDone: Boolean;
    FConnectDone: Boolean;
    FReadDone: Boolean;
    FEchoReceived: string;
    FClientReadBuf: array[0..255] of AnsiChar;
    FServerReadBuf: array[0..255] of AnsiChar;

    procedure OnServerAccept(AClientHandle: TLoopHandle; const AClientAddr: string; AClientPort: Word; AStatus: Integer);
    procedure OnClientConnected;
    procedure OnServerRead(ABytesRead: NativeInt; AStatus: Integer);
    procedure OnClientRead(ABytesRead: NativeInt; AStatus: Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTcpEchoClientServer;
    procedure TestFutureReadWrite;
    procedure TestCancellationToken;
  end;

  { Multi-core reactor loop group tests }
  TTestEventLoopGroup = class(TTestCase)
  published
    procedure TestMultiThreadedReactor;
  end;

  { Buffer pool tests }
  TTestBufferPool = class(TTestCase)
  published
    procedure TestBufferAcquireRelease;
    procedure TestAsyncBufferOperations;
  end;

  { Asynchronous file I/O tests }
  TTestAsyncFiles = class(TTestCase)
  published
    procedure TestAsyncFileWriteAndRead;
  end;

implementation

{ TTestEventLoopCore }

procedure TTestEventLoopCore.SetUp;
begin
  FLoop := TEventLoop.Create(bkAuto);
  FTimerFiredCount := 0;
  FLastTimerId := 0;
end;

procedure TTestEventLoopCore.TearDown;
begin
  if FLoop <> nil then
  begin
    FLoop.Stop;
    FLoop := nil;
  end;
end;

procedure TTestEventLoopCore.OnTimerCallback(ATimerId: Int64);
begin
  Inc(FTimerFiredCount);
  FLastTimerId := ATimerId;
end;

procedure TTestEventLoopCore.TestCreateAndDestroy;
begin
  AssertNotNull('EventLoop created', FLoop);
  AssertFalse('Initial Running state', FLoop.IsRunning);
  AssertTrue('Backend kind valid', FLoop.BackendKind in [bkEpoll, bkIOUring, bkKqueue, bkIOCP]);
end;

procedure TTestEventLoopCore.TestDeferredTasks;
var
  LExecuted: Boolean;
begin
  LExecuted := False;
  FLoop.Post(
    procedure
    begin
      LExecuted := True;
    end
  );

  AssertFalse('Not executed before RunOnce', LExecuted);
  FLoop.RunOnce(10);
  AssertTrue('Executed after RunOnce', LExecuted);
end;

procedure TTestEventLoopCore.TestDeferredOrder;
var
  LLog: string;
begin
  LLog := '';
  FLoop.Post(procedure begin LLog := LLog + 'A'; end);
  FLoop.Post(procedure begin LLog := LLog + 'B'; end);
  FLoop.Post(procedure begin LLog := LLog + 'C'; end);

  FLoop.RunOnce(10);
  AssertEquals('FIFO deferred execution', 'ABC', LLog);
end;

procedure TTestEventLoopCore.TestOneShotTimer;
var
  LId: Int64;
  I: Integer;
begin
  LId := FLoop.SetTimer(50, @OnTimerCallback, False);
  AssertTrue('Valid timer ID', LId > 0);

  { Check before expiration }
  FLoop.RunOnce(5);
  AssertEquals('Timer not yet expired', 0, FTimerFiredCount);

  { Wait until timer expires }
  for I := 1 to 20 do
  begin
    if FTimerFiredCount > 0 then
      Break;
    Sleep(10);
    FLoop.RunOnce(20);
  end;

  AssertEquals('Timer fired once', 1, FTimerFiredCount);
  AssertEquals('Matching timer ID', LId, FLastTimerId);

  { Verify it does not fire again (one-shot) }
  Sleep(60);
  FLoop.RunOnce(10);
  AssertEquals('Timer stayed one-shot', 1, FTimerFiredCount);
end;

procedure TTestEventLoopCore.TestPeriodicTimer;
var
  LId: Int64;
  I: Integer;
begin
  LId := FLoop.SetTimer(30, @OnTimerCallback, True);
  AssertTrue('Valid periodic timer ID', LId > 0);

  for I := 1 to 20 do
  begin
    if FTimerFiredCount >= 3 then
      Break;
    Sleep(20);
    FLoop.RunOnce(20);
  end;

  AssertTrue('Periodic timer fired >= 3 times', FTimerFiredCount >= 3);
  FLoop.CancelTimer(LId);
  I := FTimerFiredCount;

  Sleep(50);
  FLoop.RunOnce(10);
  AssertEquals('Timer cancelled, no more ticks', I, FTimerFiredCount);
end;

procedure TTestEventLoopCore.TestCancelTimer;
var
  LId: Int64;
begin
  LId := FLoop.SetTimer(10, @OnTimerCallback, False);
  FLoop.CancelTimer(LId);

  Sleep(25);
  FLoop.RunOnce(10);
  AssertEquals('Cancelled timer never fired', 0, FTimerFiredCount);
end;

procedure TTestEventLoopCore.TestWakeupFromThread;
var
  LThreadExecuted: Boolean;
begin
  LThreadExecuted := False;
  TThread.Queue(nil,
    procedure
    begin
      Sleep(10);
      LThreadExecuted := True;
      FLoop.Wakeup;
    end
  );

  FLoop.RunOnce(500);
  AssertTrue('Woken up by external thread', LThreadExecuted);
end;

{ TTestPipes }

procedure TTestPipes.SetUp;
begin
  FLoop := TEventLoop.Create(bkAuto);
  FPipe := TEventLoop.CreatePipe(FLoop);
  FillChar(FReadBuffer, SizeOf(FReadBuffer), 0);
  FBytesRead := 0;
  FReadStatus := 0;
  FReadDone := False;
end;

procedure TTestPipes.TearDown;
begin
  if FPipe <> nil then
  begin
    FPipe.Close;
    FPipe := nil;
  end;
  if FLoop <> nil then
  begin
    FLoop.Stop;
    FLoop := nil;
  end;
end;

procedure TTestPipes.OnPipeRead(ABytesRead: NativeInt; AStatus: Integer);
begin
  FBytesRead := ABytesRead;
  FReadStatus := AStatus;
  FReadDone := True;
end;

procedure TTestPipes.TestPipeWriteAndRead;
const
  MSG = 'EventLoop Pipe Test 123';
var
  LMsg: AnsiString;
  I: Integer;
begin
  LMsg := MSG;
  FPipe.AsyncRead(@FReadBuffer[0], Length(LMsg), @OnPipeRead);
  FPipe.AsyncWrite(PAnsiChar(LMsg), Length(LMsg), nil);

  for I := 1 to 20 do
  begin
    if FReadDone then
      Break;
    FLoop.RunOnce(20);
  end;

  AssertTrue('Pipe read completed', FReadDone);
  AssertEquals('Pipe read byte count', Length(LMsg), FBytesRead);
  AssertEquals('Pipe read payload match', MSG, Copy(FReadBuffer, 1, FBytesRead));
end;

{ TTestSockets }

procedure TTestSockets.SetUp;
begin
  FLoop := TEventLoop.Create(bkAuto);
  FServer := TEventLoop.CreateTcpServer(FLoop);
  FClient := nil;
  FServerPeer := nil;
  FAcceptDone := False;
  FConnectDone := False;
  FReadDone := False;
  FEchoReceived := '';
  FillChar(FClientReadBuf, SizeOf(FClientReadBuf), 0);
  FillChar(FServerReadBuf, SizeOf(FServerReadBuf), 0);
end;

procedure TTestSockets.TearDown;
begin
  if FServerPeer <> nil then
  begin
    FServerPeer.Close;
    FServerPeer := nil;
  end;
  if FClient <> nil then
  begin
    FClient.Close;
    FClient := nil;
  end;
  if FServer <> nil then
  begin
    FServer.Close;
    FServer := nil;
  end;
  if FLoop <> nil then
  begin
    FLoop.Stop;
    FLoop := nil;
  end;
end;

procedure TTestSockets.OnServerAccept(AClientHandle: TLoopHandle; const AClientAddr: string; AClientPort: Word; AStatus: Integer);
begin
  if AStatus = 0 then
  begin
    FServerPeer := TAsyncSocket.Create(FLoop, AClientHandle, True);
    FAcceptDone := True;
    { Begin reading from client }
    FServerPeer.AsyncRead(@FServerReadBuf[0], SizeOf(FServerReadBuf), @OnServerRead);
  end;
end;

procedure TTestSockets.OnClientConnected;
begin
  FConnectDone := True;
end;

procedure TTestSockets.OnServerRead(ABytesRead: NativeInt; AStatus: Integer);
var
  LReply: AnsiString;
begin
  if (AStatus = 0) and (ABytesRead > 0) then
  begin
    { Echo back: "Echo: " + payload }
    LReply := 'Echo: ' + Copy(FServerReadBuf, 1, ABytesRead);
    FServerPeer.AsyncWrite(PAnsiChar(LReply), Length(LReply), TAsyncWriteCallback(nil));
  end;
end;

procedure TTestSockets.OnClientRead(ABytesRead: NativeInt; AStatus: Integer);
begin
  if (AStatus = 0) and (ABytesRead > 0) then
  begin
    FEchoReceived := Copy(FClientReadBuf, 1, ABytesRead);
    FReadDone := True;
  end;
end;

procedure TTestSockets.TestTcpEchoClientServer;
const
  TEST_PAYLOAD = 'FreePascal Async IO Rocks!';
var
  LMsg: AnsiString;
  I: Integer;
  LPort: Word;
begin
  FServer.Listen('127.0.0.1', 0);
  LPort := FServer.Port;
  AssertTrue('Server bound to valid dynamic port', LPort > 0);
  FServer.AsyncAccept(@OnServerAccept);

  FClient := TEventLoop.CreateTcpClient(FLoop);
  FClient.AsyncConnect('127.0.0.1', LPort, @OnClientConnected);

  for I := 1 to 50 do
  begin
    if FConnectDone and FAcceptDone then
      Break;
    FLoop.RunOnce(20);
  end;

  AssertTrue('Client connected', FConnectDone);
  AssertTrue('Server accepted client', FAcceptDone);

  { Send payload from client and await echo }
  LMsg := TEST_PAYLOAD;
  FClient.AsyncRead(@FClientReadBuf[0], SizeOf(FClientReadBuf), @OnClientRead);
  FClient.AsyncWrite(PAnsiChar(LMsg), Length(LMsg), TAsyncWriteCallback(nil));

  for I := 1 to 50 do
  begin
    if FReadDone then
      Break;
    FLoop.RunOnce(20);
  end;

  AssertTrue('Client received echo', FReadDone);
  AssertEquals('Echoed content match', 'Echo: ' + TEST_PAYLOAD, FEchoReceived);
end;

type
  TLoopPumpThread = class(TThread)
  private
    FLoop: IEventLoop;
    FStop: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(ALoop: IEventLoop);
    procedure TerminateLoop;
  end;

constructor TLoopPumpThread.Create(ALoop: IEventLoop);
begin
  inherited Create(True);
  FLoop := ALoop;
  FStop := False;
  FreeOnTerminate := False;
end;

procedure TLoopPumpThread.Execute;
begin
  while not FStop do
  begin
    FLoop.RunOnce(20);
    Sleep(2);
  end;
end;

procedure TLoopPumpThread.TerminateLoop;
begin
  FStop := True;
  if FLoop <> nil then
    FLoop.Wakeup;
end;

procedure TTestSockets.TestFutureReadWrite;
var
  LServer: IAsyncServerSocket;
  LClient: IAsyncSocket;
  LPeer: IAsyncSocket;
  LAcceptFut: specialize IFuture<IAsyncSocket>;
  LWriteFut: specialize IFuture<NativeInt>;
  LReadFut: specialize IFuture<NativeInt>;
  LMsg: AnsiString;
  LBuf: array[0..63] of AnsiChar;
  LPump: TLoopPumpThread;
  LPort: Word;
begin
  LServer := TEventLoop.CreateTcpServer(FLoop);
  LServer.Listen('127.0.0.1', 0);
  LPort := LServer.Port;
  AssertTrue('Server bound to valid dynamic port', LPort > 0);

  LPump := TLoopPumpThread.Create(FLoop);
  LPump.Start;
  try
    LAcceptFut := LServer.AcceptAsync;
    LClient := TEventLoop.CreateTcpClient(FLoop);
    LClient.AsyncConnect('127.0.0.1', LPort, nil);

    LPeer := LAcceptFut.Value;
    AssertNotNull('Peer accepted via Future', LPeer);

    LMsg := 'FutureData';
    LWriteFut := LClient.WriteAsync(PAnsiChar(LMsg), Length(LMsg));
    AssertEquals('WriteAsync bytes written', Length(LMsg), LWriteFut.Value);

    FillChar(LBuf, SizeOf(LBuf), 0);
    LReadFut := LPeer.ReadAsync(@LBuf[0], Length(LMsg));
    AssertEquals('ReadAsync bytes read', Length(LMsg), LReadFut.Value);
    AssertEquals('Read payload', 'FutureData', Copy(LBuf, 1, Length(LMsg)));

    LPeer.Close;
    LClient.Close;
    LServer.Close;
  finally
    LPump.TerminateLoop;
    LPump.WaitFor;
    FreeAndNil(LPump);
  end;
end;

procedure TTestSockets.TestCancellationToken;
var
  LSource: TCancellationTokenSource;
  LClient: IAsyncSocket;
  LFut: specialize IFuture<NativeInt>;
  LBuf: array[0..15] of AnsiChar;
  LCaught: Boolean;
begin
  LSource := TCancellationTokenSource.Create;
  try
    LSource.Cancel; { Pre-cancelled }
    LClient := TEventLoop.CreateTcpClient(FLoop);
    LCaught := False;
    try
      LFut := LClient.ReadAsync(@LBuf[0], SizeOf(LBuf), LSource.Token);
      LFut.Value;
    except
      on E: EOperationCancelled do
        LCaught := True;
    end;
    AssertTrue('Pre-cancelled token raised EOperationCancelled', LCaught);
  finally
    LSource.Free;
  end;
end;

{ TTestEventLoopGroup }

procedure TTestEventLoopGroup.TestMultiThreadedReactor;
var
  LGroup: IEventLoopGroup;
  LThreadsUsed: array[0..3] of TThreadID;
  LHitCounts: array[0..3] of Integer;
  I: Integer;
  LTotalHits: Integer;
begin
  LGroup := TEventLoop.CreateGroup(4, bkAuto);
  AssertEquals('Group count', 4, LGroup.Count);

  FillChar(LThreadsUsed, SizeOf(LThreadsUsed), 0);
  FillChar(LHitCounts, SizeOf(LHitCounts), 0);

  LGroup.Start;

  { Distribute 80 tasks round-robin across group loops }
  for I := 0 to 79 do
  begin
    LGroup.NextLoop.Post(
      procedure
      var
        LTid: TThreadID;
        K: Integer;
      begin
        LTid := GetCurrentThreadId;
        for K := 0 to 3 do
        begin
          if (LThreadsUsed[K] = 0) or (LThreadsUsed[K] = LTid) then
          begin
            LThreadsUsed[K] := LTid;
            InterlockedIncrement(LHitCounts[K]);
            Break;
          end;
        end;
      end
    );
  end;

  Sleep(150);
  LGroup.Stop;
  LGroup.Wait;

  LTotalHits := 0;
  for I := 0 to 3 do
    Inc(LTotalHits, LHitCounts[I]);

  AssertEquals('All 80 tasks processed across reactor group', 80, LTotalHits);
  AssertTrue('Multi-thread reactor utilized thread 0', LHitCounts[0] > 0);
  AssertTrue('Multi-thread reactor utilized thread 1', LHitCounts[1] > 0);
end;

{ TTestBufferPool }

procedure TTestBufferPool.TestBufferAcquireRelease;
var
  LPool: TAsyncBufferPool;
  B1, B2: PByte;
begin
  LPool := TAsyncBufferPool.Create(1024, 16);
  try
    AssertEquals('Initial pool size', 0, LPool.AvailableCount);
    B1 := LPool.Acquire;
    AssertNotNull('Acquired buffer 1', B1);
    B2 := LPool.Acquire;
    AssertNotNull('Acquired buffer 2', B2);

    LPool.Release(B1);
    AssertEquals('Pool available count after 1 release', 1, LPool.AvailableCount);
    LPool.Release(B2);
    AssertEquals('Pool available count after 2 releases', 2, LPool.AvailableCount);

    B1 := LPool.Acquire;
    AssertEquals('Pool available count after re-acquire', 1, LPool.AvailableCount);
    LPool.Release(B1);
  finally
    LPool.Free;
  end;
end;

procedure TTestBufferPool.TestAsyncBufferOperations;
var
  LBuf: TAsyncBuffer;
  LText: AnsiString;
  LOut: array[0..63] of AnsiChar;
  LRead: Integer;
begin
  LBuf := TAsyncBuffer.Create(128);
  try
    LText := 'Hello Buffer';
    LBuf.Write(LText[1], Length(LText));
    AssertEquals('Size written', Length(LText), LBuf.Size);
    AssertEquals('Remaining to read', Length(LText), LBuf.Remaining);

    FillChar(LOut, SizeOf(LOut), 0);
    LRead := LBuf.Read(LOut[0], Length(LText));
    AssertEquals('Read bytes', Length(LText), LRead);
    AssertEquals('Read text matches', 'Hello Buffer', Copy(LOut, 1, LRead));
    AssertEquals('Remaining after read', 0, LBuf.Remaining);
  finally
    LBuf.Free;
  end;
end;

{ TTestAsyncFiles }

procedure TTestAsyncFiles.TestAsyncFileWriteAndRead;
var
  LPath: string;
  LFile: IAsyncFile;
  LMsg: AnsiString;
  LWriteFut: specialize IFuture<NativeInt>;
  LReadFut: specialize IFuture<NativeInt>;
  LBuf: array[0..63] of AnsiChar;
begin
  LPath := GetTempFileName('', 'fcl_ev_');
  try
    LMsg := 'Asynchronous File I/O with EventLoop!';
    LFile := TEventLoop.OpenFile(LPath, fmOpenWrite or fmCreate);
    LWriteFut := LFile.WriteAsync(PAnsiChar(LMsg), Length(LMsg));
    AssertEquals('Bytes written to file', Length(LMsg), LWriteFut.Value);
    LFile.Close;

    LFile := TEventLoop.OpenFile(LPath, fmOpenRead);
    AssertEquals('File size check', Length(LMsg), LFile.Size);
    FillChar(LBuf, SizeOf(LBuf), 0);
    LReadFut := LFile.ReadAsync(@LBuf[0], Length(LMsg));
    AssertEquals('Bytes read from file', Length(LMsg), LReadFut.Value);
    AssertEquals('File content match', LMsg, Copy(LBuf, 1, Length(LMsg)));
    LFile.Close;
  finally
    DeleteFile(LPath);
  end;
end;

initialization
  RegisterTest(TTestEventLoopCore);
  RegisterTest(TTestPipes);
  RegisterTest(TTestSockets);
  RegisterTest(TTestEventLoopGroup);
  RegisterTest(TTestBufferPool);
  RegisterTest(TTestAsyncFiles);

end.
