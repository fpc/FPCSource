{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by the Free Pascal development team

    fpwebsocketclient tests: pump start and stop, blocked reads, fairness.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcwsclientpump;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpcunit, testregistry, fpwebsocket, fpwebsocketclient,
  tcwsclienthelpers;

Type

  { TTestWSClientPump }

  TTestWSClientPump = Class(TWSClientTestCase)
  Private
    Procedure RunStalledClientWithoutTerminate(aFree : Boolean);
  Published
    Procedure TestTerminateReturnsWhileReadBlocked;
    Procedure TestTerminateReturnsWhileTLSReadBlocked;
    Procedure TestTerminateFromOnDisconnectReturns;
    Procedure TestHealthyClientSurvivesInterruptOfStalledSibling;
    Procedure TestTerminateReturnsWhileCallbackWaitsInSynchronize;
    Procedure TestTerminateFromSynchronizedCallbackReturns;
    Procedure TestFreeStalledClientWithoutTerminate;
    Procedure TestDisconnectStalledClientWithoutTerminate;
    Procedure TestIncompleteFrameDoesNotStarveSibling;
    Procedure TestPayloadDuringTerminateLeavesConsistentState;
  end;

implementation

{ Scenario 6: partial-frame stall over plain TCP. select() reports the
  socket readable, the reader consumes the two header bytes and then
  blocks in recv waiting for a payload that never arrives. This is the
  case InterruptRead exists for. }
Procedure TTestWSClientPump.TestTerminateReturnsWhileReadBlocked;
Var
  Srv : TStallServer;
  Pump : TWSThreadMessagePump;
  Cli : TTestClient;
  Started, Elapsed : QWord;
begin
  Srv:=TStallServer.Create;
  Pump:=TWSThreadMessagePump.Create(Nil);
  Cli:=Nil;
  try
    Pump.Execute;
    Cli:=TTestClient.Create(Srv.Port,Pump,False,True);   // instrumented reads
    ConnectWithRetry(Cli.Client);
    AssertTrue('stall peer completed the handshake: '+Srv.LastError,
               WaitForCount(Srv.FHandshakes,1));
    AssertTrue('stall peer sent the partial frame: '+Srv.LastError,
               WaitForCount(Srv.FHalfSent,1));
    AssertReaderParked;

    Started:=GetTickCount64;
    Pump.Terminate;
    Elapsed:=GetTickCount64-Started;
    AssertTrue('Terminate returned on a stalled reader (%d ms)',[Int64(Elapsed)],
               Elapsed<WaitLimitMs);

    { The point of the rewrite: once Terminate is done the reader is gone,
      so the connection can be destroyed without racing it. }
    FreeAndNil(Cli);
  finally
    FreeAndNil(Cli);
    FreeAndNil(Pump);
    if Assigned(Srv) then
      begin
      Srv.Shutdown;
      FreeAndNil(Srv);
      end;
  end;
end;

{ Scenario 7: the same stall over TLS. Here the reader is blocked inside
  SSL_read. The half frame is written through the server's transport, so
  it travels inside a complete TLS record: the TLS layer hands over two
  plaintext bytes and then has nothing more. }
Procedure TTestWSClientPump.TestTerminateReturnsWhileTLSReadBlocked;
Var
  Srv : TEchoServer;
  Pump : TWSThreadMessagePump;
  Cli : TTestClient;
  Started, Elapsed : QWord;
begin
  RequireTLS;
  Srv:=TEchoServer.Create(smStallAfterMessage,True);
  Pump:=TWSThreadMessagePump.Create(Nil);
  Cli:=Nil;
  try
    Srv.Start;
    Pump.Execute;
    Cli:=TTestClient.Create(Srv.Port,Pump,True,True);   // instrumented reads
    Cli.Client.Connect;
    Cli.Client.SendMessage('stall');

    { FReceived is bumped before the write, so it proves nothing about the
      frame. FHalfSent is bumped after WriteBuffer returns. }
    AssertTrue('server wrote the partial frame: '+Srv.LastError,
               WaitForCount(Srv.FHalfSent,1));
    AssertTrue('no server errors so far: '+Srv.LastError,ReadCounter(Srv.FErrors)=0);
    { The counters prove the frame reader is inside a transport read that
      cannot complete; for the TLS transport that read descends into
      SSL_read, but the instrumentation itself only establishes the
      outstanding transport read. }
    AssertReaderParked;

    Started:=GetTickCount64;
    Pump.Terminate;
    Elapsed:=GetTickCount64-Started;
    AssertTrue('Terminate returned with the reader inside a TLS read (%d ms)',
               [Int64(Elapsed)],Elapsed<WaitLimitMs);
    FreeAndNil(Cli);
  finally
    FreeAndNil(Cli);
    FreeAndNil(Pump);
    { Let the parked server callback return before tearing the server down. }
    if Assigned(Srv) then
      Srv.Release;
    FreeAndNil(Srv);
  end;
end;

{ Scenario 8: the disconnect callback stops the pump. OnDisconnect runs on
  the reader thread; if that handler calls Terminate, a naive Terminate
  reaches WaitFor on the very thread it is waiting for. Reacting to a
  disconnect by stopping the pump is an ordinary thing for an application
  to do. }
Procedure TTestWSClientPump.TestTerminateFromOnDisconnectReturns;
Var
  Srv : TEchoServer;
  Pump : TWSThreadMessagePump;
  Cli : TTestClient;
  Deadline : QWord;
begin
  Srv:=TEchoServer.Create(smCloseAfterMessage,False);
  Pump:=TWSThreadMessagePump.Create(Nil);
  Cli:=Nil;
  try
    Srv.Start;
    Pump.Execute;
    Cli:=TTestClient.Create(Srv.Port,Pump);
    Cli.FTerminateOnDisconnect:=Pump;
    Cli.Client.Connect;
    Cli.Client.SendMessage('bye');

    AssertTrue('OnDisconnect fired',WaitForCount(Cli.FDisconnects,1));

    { Wait for either outcome, then report which one happened. }
    Deadline:=TThread.GetTickCount64+10000;
    While (ReadCounter(Cli.FTerminateReturned)=0)
      and (ReadCounter(Cli.FTerminateRaised)=0)
      and (TThread.GetTickCount64<Deadline) do
      Sleep(PollMs);

    { If the callback never ran, Terminate was never called from the reader
      thread, and nothing can be said about a self-join. }
    if ReadCounter(Cli.FTerminateEntered)=0 then
      Fail('the disconnect callback never ran, so Terminate was not called '
          +'from the reader thread and this test decides nothing');
    AssertTrue('Terminate from the disconnect callback returns normally '
               +'(raised=%d error="%s")',
               [ReadCounter(Cli.FTerminateRaised),Cli.TerminateError],
               ReadCounter(Cli.FTerminateReturned)>0);
  finally
    if Assigned(Cli) then
      Cli.FTerminateOnDisconnect:=Nil;
    FreeAndNil(Cli);
    FreeAndNil(Pump);
    FreeAndNil(Srv);
  end;
end;

{ Scenario 9: collateral damage of the forced interrupt. A healthy client
  sharing the pump with a stalled one must survive a Terminate that had
  to force its way out, or at least its owner must be told. }
Procedure TTestWSClientPump.TestHealthyClientSurvivesInterruptOfStalledSibling;
Var
  EchoSrv : TEchoServer;
  StallSrv : TStallServer;
  Pump : TWSThreadMessagePump;
  Healthy, Stalled : TTestClient;
  SendErr : String;
  ActiveAfter, Usable : Boolean;
  DiscAfter : LongInt;
begin
  EchoSrv:=TEchoServer.Create(smEcho,False);
  StallSrv:=TStallServer.Create;
  Pump:=TWSThreadMessagePump.Create(Nil);
  Healthy:=Nil;
  Stalled:=Nil;
  try
    EchoSrv.Start;
    Pump.Execute;

    Healthy:=TTestClient.Create(EchoSrv.Port,Pump);
    Healthy.Client.Connect;
    Healthy.Client.SendMessage('warmup');
    AssertTrue('healthy client works before the stall',
               WaitForCount(Healthy.FMessages,1) and (Healthy.LastMessage='warmup'));

    Stalled:=TTestClient.Create(StallSrv.Port,Pump,False,True);
    ConnectWithRetry(Stalled.Client);
    AssertTrue('stall peer sent its partial frame: '+StallSrv.LastError,
               WaitForCount(StallSrv.FHalfSent,1));
    AssertReaderParked;

    { This Terminate cannot finish gracefully, so it has to interrupt the
      stalled reader. }
    Pump.Terminate;

    { Snapshot before restarting the pump: once the reader runs again it can
      itself change Active and deliver notifications, which would blur what
      the forced interrupt did. }
    ActiveAfter:=Healthy.Client.Active;
    DiscAfter:=ReadCounter(Healthy.FDisconnects);

    { The real question: is it still usable? A client that reports Active
      but whose socket was shut down underneath it raises here rather
      than send, so the send is guarded to keep this a finding instead of
      an error. }
    Pump.Execute;
    SendErr:='';
    try
      Healthy.Client.SendMessage('after');
    except
      On E : Exception do
        SendErr:=E.ClassName+': '+E.Message;
    end;
    Usable:=(SendErr='') and WaitForCount(Healthy.FMessages,2,3000)
            and (Healthy.LastMessage='after');
    { The snapshot goes into the message: if the client is unusable, it
      shows whether the owner was at least told. }
    AssertTrue('healthy client still usable after the forced interrupt '
               +'(send raised "%s", last="%s"; before the restart Active=%s '
               +'with %d disconnect(s))',
               [SendErr,Healthy.LastMessage,BoolToStr(ActiveAfter,True),DiscAfter],
               Usable);
    Pump.Terminate;
  finally
    FreeAndNil(Healthy);
    FreeAndNil(Stalled);
    FreeAndNil(Pump);
    if Assigned(StallSrv) then
      begin
      StallSrv.Shutdown;
      FreeAndNil(StallSrv);
      end;
    FreeAndNil(EchoSrv);
  end;
end;

{ Scenario 10: a callback that waits for the main thread, while the main
  thread is inside Terminate. TThread.Synchronize parks the reader until
  someone runs CheckSynchronize on the main thread; interrupting the
  socket cannot help here, the reader is not in a read. Terminate must
  still return. }
Procedure TTestWSClientPump.TestTerminateReturnsWhileCallbackWaitsInSynchronize;
Var
  Srv : TEchoServer;
  Pump : TWSThreadMessagePump;
  Cli : TTestClient;
begin
  Srv:=TEchoServer.Create(smEcho,False);
  Pump:=TWSThreadMessagePump.Create(Nil);
  Cli:=Nil;
  try
    Srv.Start;
    Pump.Execute;
    Cli:=TTestClient.Create(Srv.Port,Pump);
    Cli.FSyncOnMessage:=True;
    Cli.Client.Connect;
    Cli.Client.SendMessage('sync');

    AssertTrue('the callback reached Synchronize',WaitForCount(Cli.FSyncEntered,1));
    { Nobody has serviced it, so it must still be waiting. Without this the
      test could pass with the reader long since finished. }
    Sleep(200);
    AssertTrue('the callback is still parked in Synchronize (it returned on '
               +'its own, so the main thread is not the only one that can '
               +'service it and this test proves nothing)',
               ReadCounter(Cli.FSyncReturned)=0);

    { If Terminate cannot return here, the watchdog reports the hang. }
    Pump.Terminate;

    { Release the parked callback so that the teardown below is not itself
      the thing that hangs. }
    CheckSynchronize(0);
  finally
    CheckSynchronize(0);
    FreeAndNil(Cli);
    FreeAndNil(Pump);
    FreeAndNil(Srv);
  end;
end;

{ Scenario 13: Terminate called from a synchronized callback. An OnMessage
  callback hands the message to the main thread with TThread.Synchronize,
  and the method that runs there stops the pump - the ordinary shape of
  "a message arrived, the form decided to close". The reader is parked in
  Synchronize until that method returns, and the method is inside
  Terminate. The main thread plays the part of an application's message
  loop and services the queue itself. }
Procedure TTestWSClientPump.TestTerminateFromSynchronizedCallbackReturns;
Var
  Srv : TEchoServer;
  Pump : TWSThreadMessagePump;
  Cli : TTestClient;
  Deadline : QWord;
begin
  Srv:=TEchoServer.Create(smEcho,False);
  Pump:=TWSThreadMessagePump.Create(Nil);
  Cli:=Nil;
  try
    Srv.Start;
    Pump.Execute;
    Cli:=TTestClient.Create(Srv.Port,Pump);
    Cli.FSyncTerminatePump:=Pump;
    Cli.Client.Connect;
    Cli.Client.SendMessage('close');

    { If Terminate cannot return inside the synchronized method, control
      never comes back here and the watchdog reports the hang. }
    Deadline:=TThread.GetTickCount64+10000;
    While (ReadCounter(Cli.FSyncTermReturned)=0)
          and (ReadCounter(Cli.FSyncTermRaised)=0)
          and (TThread.GetTickCount64<Deadline) do
      CheckSynchronize(10);

    if ReadCounter(Cli.FSyncTermEntered)=0 then
      Fail('the message never arrived, so the synchronized method never '
          +'reached Terminate and this test decides nothing');
    AssertTrue('Terminate returns when called from a synchronized callback '
               +'(raised=%d)',[ReadCounter(Cli.FSyncTermRaised)],
               ReadCounter(Cli.FSyncTermReturned)>0);
  finally
    if Assigned(Cli) then
      Cli.FSyncTerminatePump:=Nil;
    CheckSynchronize(0);
    FreeAndNil(Cli);
    FreeAndNil(Pump);
    FreeAndNil(Srv);
  end;
end;

{ Scenarios 27 and 28: a client whose frame read stalls is freed (27) or
  disconnected (28) while the pump keeps running - no Terminate first.
  The call must return, and another client of the same pump must still be
  served afterwards. }
Procedure TTestWSClientPump.RunStalledClientWithoutTerminate(aFree : Boolean);
Var
  Srv : TStallServer;
  SrvB : TEchoServer;
  Pump : TWSThreadMessagePump;
  Cli, B : TTestClient;
  Started, Elapsed : QWord;
begin
  Srv:=TStallServer.Create;
  SrvB:=TEchoServer.Create(smEcho,False);
  Pump:=TWSThreadMessagePump.Create(Nil);
  Cli:=Nil;
  B:=Nil;
  try
    SrvB.Start;
    B:=TTestClient.Create(SrvB.Port,Pump);
    ConnectWithRetry(B.Client);
    Pump.Execute;
    Cli:=TTestClient.Create(Srv.Port,Pump,False,True);   // instrumented reads
    ConnectWithRetry(Cli.Client);

    AssertTrue('stall peer completed the handshake: '+Srv.LastError,
               WaitForCount(Srv.FHandshakes,1));
    AssertTrue('stall peer sent the partial frame: '+Srv.LastError,
               WaitForCount(Srv.FHalfSent,1));
    AssertReaderParked;

    Started:=GetTickCount64;
    if aFree then
      FreeAndNil(Cli.FClient)
    else
      Cli.Client.Disconnect(False);
    Elapsed:=GetTickCount64-Started;
    B.Client.SendMessage('still served?');
    WaitForCount(B.FMessages,1);
    AssertTrue('the call returns while the read is stalled (%d ms)',[Int64(Elapsed)],
               Elapsed<WaitLimitMs);
    if not aFree then
      AssertFalse('the client is inactive',Cli.Client.Active);
    AssertTrue('the pump still serves the other client',ReadCounter(B.FMessages)>=1);
  finally
    Pump.Terminate;
    FreeAndNil(Cli);
    FreeAndNil(B);
    FreeAndNil(Pump);
    if Assigned(Srv) then
      begin
      Srv.Shutdown;
      FreeAndNil(Srv);
      end;
    FreeAndNil(SrvB);
  end;
end;

Procedure TTestWSClientPump.TestFreeStalledClientWithoutTerminate;
begin
  RunStalledClientWithoutTerminate(True);
end;

Procedure TTestWSClientPump.TestDisconnectStalledClientWithoutTerminate;
begin
  RunStalledClientWithoutTerminate(False);
end;

{ Scenario 37: an incomplete frame on one connection must not monopolize
  a shared message pump. The stall peer sends a complete frame header but
  withholds all five bytes of its announced payload. Read instrumentation
  establishes that the client has started processing that frame. Only
  then is a message sent through a second, already-proven connection on
  the same pump. The payload is released after the result has been
  captured, so it cannot accidentally unblock a single-reader
  implementation early and turn starvation into a pass.

  This deliberately does not require ReaderIsInsideRead: a valid redesign
  may use either an independent reader per session or a nonblocking
  incremental parser, and the externally observable fairness requirement
  is the same. }
Procedure TTestWSClientPump.TestIncompleteFrameDoesNotStarveSibling;
Var
  EchoSrv : TEchoServer;
  StallSrv : TStallServer;
  Pump : TWSThreadMessagePump;
  Healthy, Stalled : TTestClient;
  MessageBefore : LongInt;
  FrameReadStarted, Served : Boolean;
  SendErr : String;
begin
  EchoSrv:=TEchoServer.Create(smEcho,False);
  StallSrv:=TStallServer.Create;
  Pump:=TWSThreadMessagePump.Create(Nil);
  Healthy:=Nil;
  Stalled:=Nil;
  try
    EchoSrv.Start;
    Pump.Execute;

    Healthy:=TTestClient.Create(EchoSrv.Port,Pump);
    ConnectWithRetry(Healthy.Client);
    Healthy.Client.SendMessage('before-stall');
    AssertTrue('healthy client works before the partial frame',
               WaitForCount(Healthy.FMessages,1)
               and (Healthy.LastMessage='before-stall'));

    Stalled:=TTestClient.Create(StallSrv.Port,Pump,False,True);
    ConnectWithRetry(Stalled.Client);
    AssertTrue('stall peer completed the handshake: '+StallSrv.LastError,
               WaitForCount(StallSrv.FHandshakes,1));
    AssertTrue('stall peer sent only the frame header: '+StallSrv.LastError,
               WaitForCount(StallSrv.FHalfSent,1));

    { The instrumented connection is the stalled connection only, and
      ReadEntries counts frame reads alone (the handshake's line reads are
      counted apart). Seeing a frame read entry therefore proves that the
      pump has begun consuming the deliberately incomplete frame, rather
      than merely having completed the handshake. Give a blocking
      implementation time to enter its next payload read; an incremental
      implementation simply remains idle. }
    FrameReadStarted:=WaitForCount(ReadEntries,1,2000);
    if FrameReadStarted then
      Sleep(100);
    AssertTrue('the client began processing the incomplete frame ('
               +ReadCountersDetail+')',FrameReadStarted);
    AssertTrue('the missing payload is still withheld',
               ReadCounter(StallSrv.FRestSent)=0);

    MessageBefore:=ReadCounter(Healthy.FMessages);
    SendErr:='';
    try
      Healthy.Client.SendMessage('during-stall');
    except
      On E : Exception do
        SendErr:=E.ClassName+': '+E.Message;
    end;
    Served:=(SendErr='')
            and WaitForCount(Healthy.FMessages,MessageBefore+1,2000)
            and (Healthy.LastMessage='during-stall');
    AssertTrue('the healthy sibling accepts a send while the frame is '
               +'incomplete (%s)',[SendErr],SendErr='');
    AssertTrue('the healthy sibling is served while the frame is incomplete '
               +'(messages before=%d after=%d, last="%s")',
               [MessageBefore,ReadCounter(Healthy.FMessages),Healthy.LastMessage],
               Served);
    AssertTrue('the stalled payload remained withheld until after the '
               +'sibling result',ReadCounter(StallSrv.FRestSent)=0);
    AssertTrue('the healthy sibling remains active and was not disconnected '
               +'(Active=%s, OnDisconnect=%d)',
               [BoolToStr(Healthy.Client.Active,True),ReadCounter(Healthy.FDisconnects)],
               Healthy.Client.Active and (ReadCounter(Healthy.FDisconnects)=0));

    { Complete the staged frame only after all fairness observations have
      been captured. This keeps cleanup independent of the behaviour under
      test, even on an implementation that still has one blocking reader. }
    StallSrv.SendRest;
    WaitForCount(StallSrv.FRestSent,1,2000);
    WaitForCount(Stalled.FMessages,1,2000);
    Pump.Terminate;
  finally
    if Assigned(StallSrv) then
      StallSrv.SendRest;
    FreeAndNil(Healthy);
    FreeAndNil(Stalled);
    FreeAndNil(Pump);
    if Assigned(StallSrv) then
      begin
      StallSrv.Shutdown;
      FreeAndNil(StallSrv);
      end;
    FreeAndNil(EchoSrv);
  end;
end;

{ The harness's --interrupt-race mode: a payload arriving while Terminate
  is interrupting a parked read.

  Terminate waits gracefully for 100 ms and only then starts interrupting,
  so the peer is armed to deliver the missing payload 120 ms after
  Terminate is entered - about 20 ms into the interrupt loop. Whether the
  read completes inside the interrupt's claim window cannot be staged
  from here: that window lies between the socket read and EndRead inside
  TWSSocketHelper, which the instrumented transport wraps as a whole, and
  the interrupt goes to the helper directly. So this test does not claim
  to have hit the race; it checks that whatever the interleaving was, the
  client is left consistent: either the connection is intact or its owner
  was told, and a connection the pump still tracks has a usable socket. }

Type
  { The main thread is inside Terminate when the payload has to arrive, so
    the peer is released from a thread of its own. }
  TArmThread = Class(TThread)
  Private
    FServer : TStallServer;
    FDelayMs : Integer;
  Public
    Constructor Create(aServer : TStallServer; aDelayMs : Integer);
    Procedure Execute; override;
  end;

Constructor TArmThread.Create(aServer : TStallServer; aDelayMs : Integer);
begin
  FServer:=aServer;
  FDelayMs:=aDelayMs;
  FreeOnTerminate:=False;
  Inherited Create(False);
end;

Procedure TArmThread.Execute;
begin
  Sleep(FDelayMs);
  FServer.SendRest;
end;

Procedure TTestWSClientPump.TestPayloadDuringTerminateLeavesConsistentState;
Var
  Srv : TStallServer;
  Pump : TProbePump;
  Cli : TTestClient;
  Arm : TArmThread;
  Con : TWSClientConnection;
  Active, Tracked : Boolean;
  Disc : LongInt;
  SendErr : String;
begin
  Srv:=TStallServer.Create;
  Pump:=TProbePump.Create(Nil);
  Cli:=Nil;
  Arm:=Nil;
  try
    Pump.Execute;
    Cli:=TTestClient.Create(Srv.Port,Pump,False,True);
    ConnectWithRetry(Cli.Client);
    AssertTrue('the peer sent its partial frame: '+Srv.LastError,
               WaitForCount(Srv.FHalfSent,1));
    AssertReaderParked;
    Con:=Cli.Client.Connection;

    Arm:=TArmThread.Create(Srv,120);
    Pump.Terminate;
    Arm.WaitFor;

    Active:=Cli.Client.Active;
    Tracked:=Pump.Tracks(Con);
    Disc:=ReadCounter(Cli.FDisconnects);

    { A socket that was shut down cannot be written to. This tells a
      connection that merely survived Terminate from one that was broken
      without anybody being told. }
    SendErr:='';
    try
      Cli.Client.SendMessage('after');
    except
      On E : Exception do
        SendErr:=E.ClassName+': '+E.Message;
    end;

    AssertTrue('the connection is either intact or its owner was told '
               +'(Active=%s, OnDisconnect=%d, send raised "%s", payload delivered=%d)',
               [BoolToStr(Active,True),Disc,SendErr,ReadCounter(Srv.FRestSent)],
               ((SendErr='') and Active) or (Disc>=1) or (not Active));
    AssertTrue('a connection the pump still tracks has a usable socket '
               +'(tracked=%s, send raised "%s")',[BoolToStr(Tracked,True),SendErr],
               (not Tracked) or (SendErr=''));
  finally
    if Assigned(Arm) then
      begin
      Arm.WaitFor;
      FreeAndNil(Arm);
      end;
    FreeAndNil(Cli);
    FreeAndNil(Pump);
    if Assigned(Srv) then
      begin
      Srv.Shutdown;
      FreeAndNil(Srv);
      end;
  end;
end;

initialization
  RegisterTest(TTestWSClientPump);
end.
