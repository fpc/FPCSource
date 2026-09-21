{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by the Free Pascal development team

    fpwebsocketclient tests: lifecycle calls made from inside callbacks.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcwsclientcallbacks;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpcunit, testregistry, fpwebsocket, fpwebsocketclient,
  tcwsclienthelpers;

Type

  { TTestWSClientCallbacks }

  TTestWSClientCallbacks = Class(TWSClientTestCase)
  Private
    Procedure RunReconnectFromDisconnect(aMode : LongInt);
    Procedure RunSelfDisconnectOnReader(aCloseFrame : Boolean);
  Published
    Procedure TestOnErrorSynchronizedDisconnectOfOtherClientCompletes;
    Procedure TestOnMessageSynchronizedDisconnectOfOtherClientCompletes;
    Procedure TestSynchronizedSelfDisconnectFromOnMessage;
    Procedure TestSynchronizedSelfReconnectFromOnMessage;
    Procedure TestReconnectSynchronizedFromOnDisconnect;
    Procedure TestReconnectFromOnDisconnectOnReader;
    Procedure TestSelfDisconnectFromOnMessageOnReader;
    Procedure TestSelfDisconnectFromCloseFrameCallback;
    Procedure TestReconnectWhilePumpNotificationRuns;
    Procedure TestReconnectFromOnDisconnectInsideDisconnectingCallback;
  end;

implementation

{ ---------------------------------------------------------------------
  Scenario 16: an OnError handler that hands the error to the main
  thread, which then disconnects another client of the same pump. If the
  pump still held a list lock while calling OnError, the synchronized
  method would block in RemoveClient on that lock while the reader waits
  in Synchronize for the method to return.

  Client X has its connection reset in the middle of a frame, which
  produces the error; client Y is healthy and is the one the main thread
  disconnects.
  --------------------------------------------------------------------- }

Type
  TSyncErrorSink = Class
  Private
    FTarget : TCustomWebsocketClient;
    Procedure DisconnectTarget;
  Public
    FErrors : LongInt;
    FEntered : LongInt;    // the synchronized method started
    FReturned : LongInt;   // the synchronized method returned
    Procedure DoError(Sender : TObject; E : Exception);
  end;

Procedure TSyncErrorSink.DisconnectTarget;
Var
  T : TCustomWebsocketClient;
begin
  BumpCounter(FEntered);
  T:=FTarget;
  FTarget:=Nil;
  if Assigned(T) then
    T.Disconnect(False);
  BumpCounter(FReturned);
end;

Procedure TSyncErrorSink.DoError(Sender : TObject; E : Exception);
begin
  BumpCounter(FErrors);
  { Only the first error is handed over. }
  if (ReadCounter(FErrors)=1) and Assigned(FTarget) then
    TThread.Synchronize(TThread.CurrentThread,@DisconnectTarget);
end;

Procedure TTestWSClientCallbacks.TestOnErrorSynchronizedDisconnectOfOtherClientCompletes;
Var
  StallSrv : TStallServer;
  EchoSrv : TEchoServer;
  Pump : TWSThreadMessagePump;
  Sink : TSyncErrorSink;
  X, Y : TTestClient;
  Deadline : QWord;
begin
  StallSrv:=TStallServer.Create;
  { An orderly EOF is intentionally not an OnError (TestPeerClosesMidFrame
    verifies that policy). Use an abortive close so this test really
    exercises the OnError path it claims to test. }
  StallSrv.FAfterHalf:=2;
  EchoSrv:=TEchoServer.Create(smEcho,False);
  Pump:=TWSThreadMessagePump.Create(Nil);
  Sink:=TSyncErrorSink.Create;
  X:=Nil;
  Y:=Nil;
  try
    Pump.OnError:=@Sink.DoError;
    EchoSrv.Start;
    { Start the pump before either connection is registered. This ensures
      its per-session reader is active while X is reset mid-frame instead
      of depending on whether a previously closed socket is reported
      readable. }
    Pump.Execute;
    X:=TTestClient.Create(StallSrv.Port,Pump,False,True);   // instrumented reads
    Y:=TTestClient.Create(EchoSrv.Port,Pump);
    { Make the healthy target active and publish it to the error sink
      before arming X. The reset can then never beat FTarget initialization. }
    ConnectWithRetry(Y.Client);
    Sink.FTarget:=Y.Client;
    ConnectWithRetry(X.Client);
    { The peer resets only once X's reader is seen parked in the payload
      read, so the reset lands in the middle of the frame. }
    AssertTrue('the peer sent its partial frame: '+StallSrv.LastError,
               WaitForCount(StallSrv.FHalfSent,1));
    AssertReaderParked;
    StallSrv.CloseNow;

    { The main thread plays the part of an application's message loop. If
      the handler cannot complete, control never returns here and the
      watchdog reports the hang. }
    Deadline:=TThread.GetTickCount64+12000;
    While (ReadCounter(Sink.FReturned)=0) and (TThread.GetTickCount64<Deadline) do
      CheckSynchronize(10);

    AssertTrue('the peer of X left mid-frame: '+StallSrv.LastError,
               ReadCounter(StallSrv.FClosedAfterHalf)>0);
    if ReadCounter(Sink.FErrors)=0 then
      Fail('no error was reported, so this test decides nothing');
    AssertTrue('the synchronized disconnect completes (entered=%d)',
               [ReadCounter(Sink.FEntered)],ReadCounter(Sink.FReturned)>0);
    AssertTrue('Y is told it was disconnected',ReadCounter(Y.FDisconnects)>=1);
  finally
    Sink.FTarget:=Nil;
    CheckSynchronize(0);
    FreeAndNil(X);
    FreeAndNil(Y);
    FreeAndNil(Pump);
    FreeAndNil(Sink);
    FreeAndNil(EchoSrv);
    if Assigned(StallSrv) then
      begin
      StallSrv.Shutdown;
      FreeAndNil(StallSrv);
      end;
  end;
end;

{ Scenario 17: an OnMessage handler that hands the message to the main
  thread, which then disconnects another client of the same pump. The
  same shape as scenario 16, but from OnMessage - by far the more common
  place for it. }
Procedure TTestWSClientCallbacks.TestOnMessageSynchronizedDisconnectOfOtherClientCompletes;
Var
  SrvA, SrvB : TEchoServer;
  Pump : TWSThreadMessagePump;
  A, B : TTestClient;
  Deadline : QWord;
begin
  SrvA:=TEchoServer.Create(smEcho,False);
  SrvB:=TEchoServer.Create(smEcho,False);
  Pump:=TWSThreadMessagePump.Create(Nil);
  A:=Nil;
  B:=Nil;
  try
    SrvA.Start;
    SrvB.Start;
    A:=TTestClient.Create(SrvA.Port,Pump);
    B:=TTestClient.Create(SrvB.Port,Pump);
    ConnectWithRetry(A.Client);
    ConnectWithRetry(B.Client);
    A.FSyncDisconnectOther:=B.Client;
    Pump.Execute;
    A.Client.SendMessage('close the other one');

    { The main thread plays the part of an application's message loop. }
    Deadline:=TThread.GetTickCount64+10000;
    While (ReadCounter(A.FSyncOtherReturned)=0) and (TThread.GetTickCount64<Deadline) do
      CheckSynchronize(10);

    if ReadCounter(A.FMessages)=0 then
      Fail('no echo arrived, so this test decides nothing');
    AssertTrue('the synchronized disconnect completes (entered=%d)',
               [ReadCounter(A.FSyncOtherEntered)],ReadCounter(A.FSyncOtherReturned)>0);
    AssertTrue('B is told it was disconnected',ReadCounter(B.FDisconnects)>=1);
  finally
    if Assigned(A) then
      A.FSyncDisconnectOther:=Nil;
    CheckSynchronize(0);
    FreeAndNil(A);
    FreeAndNil(B);
    FreeAndNil(Pump);
    FreeAndNil(SrvA);
    FreeAndNil(SrvB);
  end;
end;

{ Scenario 21: a method that a client's own OnMessage synchronizes onto
  the main thread disconnects that client. The reader is inside the
  callback and waits for the main thread; the main thread disconnects the
  connection the reader is using. Waiting for the reader would wait for
  itself; freeing the connection at once would free it under the
  callback. It may only go once the callback returned. }
Procedure TTestWSClientCallbacks.TestSynchronizedSelfDisconnectFromOnMessage;
Var
  Srv : TEchoServer;
  Pump : TProbePump;
  A : TTestClient;
  C0 : TWebSocketClientConnection;
begin
  Srv:=TEchoServer.Create(smEcho,False);
  Pump:=TProbePump.Create(Nil);
  A:=Nil;
  try
    Srv.Start;
    A:=TTestClient.Create(Srv.Port,Pump,False,False,True);
    ConnectWithRetry(A.Client);
    C0:=A.Client.Connection;
    InterLockedExchange(A.FSyncSelfAction,1);
    Pump.Execute;
    A.Client.SendMessage('disconnect me from the main thread');

    { If the method cannot complete, control never returns here and the
      watchdog reports the hang. }
    WaitServicing(A.FSelfEntered,1);
    WaitServicing(A.FCallbackDone,1);
    WaitDestroyed(C0,2000);
    WaitServicing(A.FDisconnects,2,300);   // room for a second notification
    if ReadCounter(A.FMessages)=0 then
      Fail('no echo arrived, so this test decides nothing');
    AssertTrue('the synchronized disconnect returns (%s)',[A.SelfError],
               ReadCounter(A.FSelfReturned)=1);
    AssertEquals('the callback goes on after Synchronize',1,ReadCounter(A.FCallbackDone));
    AssertEquals('OnDisconnect count',1,ReadCounter(A.FDisconnects));
    AssertTrue('the client is inactive and no longer tracked',
               (not A.Client.Active) and (Pump.ClientCount=0));
    AssertEquals('times the connection is destroyed',1,DestroyedTimes(C0));
    AssertEquals('work on the connection after its destruction',0,
                 ReadCounter(ContinuedAfterFree));
  finally
    CheckSynchronize(0);
    Pump.Terminate;
    FreeAndNil(A);
    FreeAndNil(Pump);
    FreeAndNil(Srv);
  end;
end;

{ Scenario 22: the same method disconnects and reconnects the client.
  Besides the lifetime of the old connection this asks whether the
  replacement is registered and served, and whether anything the old
  connection still does changes the new one's state. }
Procedure TTestWSClientCallbacks.TestSynchronizedSelfReconnectFromOnMessage;
Var
  Srv : TEchoServer;
  Pump : TProbePump;
  A : TTestClient;
  C0, C1 : TWebSocketClientConnection;
begin
  Srv:=TEchoServer.Create(smEcho,False);
  Pump:=TProbePump.Create(Nil);
  A:=Nil;
  try
    Srv.Start;
    A:=TTestClient.Create(Srv.Port,Pump,False,False,True);
    ConnectWithRetry(A.Client);
    C0:=A.Client.Connection;
    InterLockedExchange(A.FSyncSelfAction,2);
    Pump.Execute;
    A.Client.SendMessage('reconnect me from the main thread');

    WaitServicing(A.FSelfEntered,1);
    WaitServicing(A.FCallbackDone,1);
    WaitDestroyed(C0,2000);
    C1:=A.Client.Connection;
    if ReadCounter(A.FSelfReturned)=1 then
      begin
      A.Client.SendMessage('over the new connection');
      WaitServicing(A.FMessages,2);
      end;
    WaitServicing(A.FDisconnects,2,300);
    if ReadCounter(A.FMessages)=0 then
      Fail('no echo arrived, so this test decides nothing');
    AssertTrue('the synchronized reconnect returns (%s)',[A.SelfError],
               ReadCounter(A.FSelfReturned)=1);
    AssertEquals('the callback goes on after Synchronize',1,ReadCounter(A.FCallbackDone));
    AssertEquals('OnDisconnect count, for the old connection',1,ReadCounter(A.FDisconnects));
    AssertTrue('the client is active on a new, tracked connection',
               A.Client.Active and Assigned(C1) and (C1<>C0) and Pump.Tracks(C1));
    AssertEquals('times the old connection is destroyed',1,DestroyedTimes(C0));
    AssertTrue('the new connection is alive',Assigned(C1) and not WasDestroyed(C1));
    AssertTrue('the new connection is served',ReadCounter(A.FMessages)>=2);
    AssertEquals('work on a connection after its destruction',0,
                 ReadCounter(ContinuedAfterFree));
  finally
    CheckSynchronize(0);
    Pump.Terminate;
    FreeAndNil(A);
    FreeAndNil(Pump);
    FreeAndNil(Srv);
  end;
end;

{ Scenarios 23 and 24: the peer closes, and OnDisconnect reconnects the
  client - through a synchronized method (23) or directly on the reader
  thread (24). The pump is still inside its notification for the old
  connection while the replacement is created and registered. }
Procedure TTestWSClientCallbacks.RunReconnectFromDisconnect(aMode : LongInt);
Var
  Srv : TEchoServer;
  Pump : TProbePump;
  A : TTestClient;
  C0, C1 : TWebSocketClientConnection;
begin
  Srv:=TEchoServer.Create(smCloseAfterMessage,False);
  Pump:=TProbePump.Create(Nil);
  A:=Nil;
  try
    Srv.Start;
    A:=TTestClient.Create(Srv.Port,Pump,False,False,True);
    ConnectWithRetry(A.Client);
    C0:=A.Client.Connection;
    InterLockedExchange(A.FReconnectOnDisconnect,aMode);
    Pump.Execute;
    A.Client.SendMessage('close, then reconnect me');

    WaitServicing(A.FSelfEntered,1);
    WaitServicing(A.FSelfReturned,1);
    WaitDestroyed(C0,2000);
    WaitServicing(A.FDisconnects,2,300);
    C1:=A.Client.Connection;
    if ReadCounter(A.FDisconnects)=0 then
      Fail('no OnDisconnect, so this test decides nothing');
    AssertTrue('the reconnect returns (%s)',[A.SelfError],ReadCounter(A.FSelfReturned)=1);
    AssertEquals('OnDisconnect count, for the old connection',1,ReadCounter(A.FDisconnects));
    AssertTrue('the client is active on a new, tracked connection',
               A.Client.Active and Assigned(C1) and (C1<>C0) and Pump.Tracks(C1));
    AssertEquals('times the old connection is destroyed',1,DestroyedTimes(C0));
    AssertEquals('work on a connection after its destruction',0,
                 ReadCounter(ContinuedAfterFree));
    A.Client.SendMessage('close again');
    AssertTrue('the new connection is served: its close is reported',
               WaitServicing(A.FDisconnects,2));
  finally
    CheckSynchronize(0);
    Pump.Terminate;
    FreeAndNil(A);
    FreeAndNil(Pump);
    FreeAndNil(Srv);
  end;
end;

Procedure TTestWSClientCallbacks.TestReconnectSynchronizedFromOnDisconnect;
begin
  RunReconnectFromDisconnect(2);
end;

Procedure TTestWSClientCallbacks.TestReconnectFromOnDisconnectOnReader;
begin
  RunReconnectFromDisconnect(1);
end;

{ Scenarios 25 and 26: a client disconnects itself from its own callback
  on the reader thread - from OnMessage (25) and from the control
  callback for a close frame (26). The pump is still inside CheckIncoming
  for that connection: HandleIncoming goes on after the callback (for a
  close frame the close reply has already been written before it). A
  second client shows whether the pump keeps serving. }
Procedure TTestWSClientCallbacks.RunSelfDisconnectOnReader(aCloseFrame : Boolean);
Var
  SrvA, SrvB : TEchoServer;
  Pump : TProbePump;
  A, B : TTestClient;
  C0 : TWebSocketClientConnection;
begin
  if aCloseFrame then
    SrvA:=TEchoServer.Create(smCloseFrameAfterMessage,False)
  else
    SrvA:=TEchoServer.Create(smEcho,False);
  SrvB:=TEchoServer.Create(smEcho,False);
  Pump:=TProbePump.Create(Nil);
  A:=Nil;
  B:=Nil;
  try
    SrvA.Start;
    SrvB.Start;
    A:=TTestClient.Create(SrvA.Port,Pump,False,False,True);
    B:=TTestClient.Create(SrvB.Port,Pump);
    ConnectWithRetry(A.Client);
    ConnectWithRetry(B.Client);
    C0:=A.Client.Connection;
    if aCloseFrame then
      A.FCloseFrameSelfDisconnect:=True
    else
      InterLockedExchange(A.FDirectSelfAction,1);
    Pump.Execute;
    A.Client.SendMessage('disconnect me from my own callback');

    WaitForCount(A.FSelfEntered,1);
    WaitDestroyed(C0,2000);
    Sleep(300);   // room for a second notification
    B.Client.SendMessage('still served?');
    WaitForCount(B.FMessages,1);
    if ReadCounter(A.FSelfEntered)<>1 then
      Fail('no echo or close frame arrived, so this test decides nothing');
    AssertTrue('the self disconnect returns (%s)',[A.SelfError],ReadCounter(A.FSelfReturned)=1);
    if aCloseFrame then
      AssertEquals('close control events',1,ReadCounter(A.FControlCloses));
    AssertEquals('OnDisconnect count',1,ReadCounter(A.FDisconnects));
    AssertTrue('the client is inactive and no longer tracked',
               (not A.Client.Active) and not Pump.Tracks(C0));
    AssertEquals('times the connection is destroyed',1,DestroyedTimes(C0));
    AssertEquals('work on the connection after its destruction (HandleIncoming '
                 +'went on, or a reply was sent, on a destroyed connection)',0,
                 ReadCounter(ContinuedAfterFree));
    AssertTrue('the pump still serves the other client',ReadCounter(B.FMessages)>=1);
  finally
    Pump.Terminate;
    FreeAndNil(A);
    FreeAndNil(B);
    FreeAndNil(Pump);
    FreeAndNil(SrvA);
    FreeAndNil(SrvB);
  end;
end;

Procedure TTestWSClientCallbacks.TestSelfDisconnectFromOnMessageOnReader;
begin
  RunSelfDisconnectOnReader(False);
end;

Procedure TTestWSClientCallbacks.TestSelfDisconnectFromCloseFrameCallback;
begin
  RunSelfDisconnectOnReader(True);
end;

{ Scenario 29: while the pump's OnDisconnect notification for a peer
  close is still running, the owner disconnects and reconnects the
  client. The notification is held until the reconnect has returned, so
  the test tells "the reconnect overlapped the notification" from "the
  reconnect waited for it". The late end of the old connection's
  notification must neither free the connection under itself, nor report
  a second disconnect, nor mark the reconnected client inactive. }
Procedure TTestWSClientCallbacks.TestReconnectWhilePumpNotificationRuns;
Var
  Srv : TEchoServer;
  Pump : TProbePump;
  A : TTestClient;
  C0, C1 : TWebSocketClientConnection;
begin
  Srv:=TEchoServer.Create(smCloseAfterMessage,False);
  Pump:=TProbePump.Create(Nil);
  A:=Nil;
  try
    Srv.Start;
    A:=TTestClient.Create(Srv.Port,Pump,False,False,True);
    ConnectWithRetry(A.Client);
    C0:=A.Client.Connection;
    A.FSlowConnection:=C0;
    A.FHoldDisconnectUntilRelease:=True;
    Pump.Execute;
    A.Client.SendMessage('bye');

    if not WaitForCount(A.FSlowDiscEntered,1) then
      Fail('the peer close was not reported, so this test decides nothing');
    A.Client.Disconnect(False);
    ConnectWithRetry(A.Client);
    C1:=A.Client.Connection;
    { Proven before the release: the reconnect returned while the old
      notification was still running, it did not wait for it. }
    AssertEquals('the notification finished before the reconnect returned',0,
                 ReadCounter(A.FSlowDiscDone));
    AssertTrue('the reconnected client is active on a new, tracked connection '
               +'while the old notification still runs',
               A.Client.Active and Assigned(C1) and (C1<>C0) and Pump.Tracks(C1));
    A.FHoldDisconnectUntilRelease:=False;   // the new connection's own close is not held
    A.Release;
    AssertTrue('the notification finished after the release',
               WaitForCount(A.FSlowDiscDone,1));
    AssertEquals('the hold ended by its time limit',0,ReadCounter(A.FHoldTimedOut));
    WaitDestroyed(C0,2000);
    WaitForCount(A.FDisconnects,2,300);   // room for a second notification
    AssertTrue('the connection outlives the running notification',
               ReadCounter(A.FSlowDiscSawDestroyed)=0);
    AssertEquals('OnDisconnect count',1,ReadCounter(A.FDisconnects));
    AssertTrue('the reconnected client stays active on its tracked connection',
               A.Client.Active and (A.Client.Connection=C1) and Pump.Tracks(C1));
    AssertEquals('times the old connection is destroyed',1,DestroyedTimes(C0));
    A.Client.SendMessage('bye again');
    AssertTrue('the new connection is served: its close is reported',
               WaitForCount(A.FDisconnects,2));
  finally
    if Assigned(A) then
      A.Release;
    Pump.Terminate;
    FreeAndNil(A);
    FreeAndNil(Pump);
    FreeAndNil(Srv);
  end;
end;

{ Scenario 33: a client disconnects itself from its own OnMessage, and
  its OnDisconnect handler reconnects it at once - all on the reader
  thread, inside the callback of the old connection. The reconnect
  replaces the handshake response. The old connection still refers to
  its own response while the callback runs, so that response must not be
  freed before the callback returns. }
Procedure TTestWSClientCallbacks.TestReconnectFromOnDisconnectInsideDisconnectingCallback;
Var
  Srv : TEchoServer;
  Pump : TProbePump;
  A : TTestClient;
  C0, C1 : TWebSocketClientConnection;
begin
  Srv:=TEchoServer.Create(smEcho,False);
  Pump:=TProbePump.Create(Nil);
  A:=Nil;
  try
    Srv.Start;
    A:=TTestClient.Create(Srv.Port,Pump,False,False,True);
    ConnectWithRetry(A.Client);
    C0:=A.Client.Connection;
    A.FOldResponse:=C0.HandShakeResponse;
    InterLockedExchange(A.FReconnectOnDisconnect,1);
    InterLockedExchange(A.FDirectSelfAction,1);
    Pump.Execute;
    A.Client.SendMessage('disconnect me, then reconnect me');

    WaitForCount(A.FCallbackDone,1);
    WaitDestroyed(C0,2000);
    C1:=A.Client.Connection;
    if Assigned(C1) and A.Client.Active then
      begin
      A.Client.SendMessage('over the new connection');
      WaitForCount(A.FMessages,2);
      end;
    if ReadCounter(A.FMessages)=0 then
      Fail('no echo arrived, so this test decides nothing');
    AssertTrue('disconnect and reconnect both return (%s)',[A.SelfError],
               ReadCounter(A.FSelfReturned)=2);
    AssertEquals('OnDisconnect count',1,ReadCounter(A.FDisconnects));
    AssertTrue('the client is active on a new, tracked connection',
               A.Client.Active and Assigned(C1) and (C1<>C0) and Pump.Tracks(C1));
    AssertTrue('the old handshake response outlives the callback',
               ReadCounter(A.FOldResponseFreed)=0);
    WaitDestroyed(Pointer(A.FOldResponse),2000);
    AssertEquals('times the old handshake response is destroyed, afterwards',1,
                 DestroyedTimes(Pointer(A.FOldResponse)));
    AssertEquals('times the old connection is destroyed',1,DestroyedTimes(C0));
    AssertTrue('the new connection is served',ReadCounter(A.FMessages)>=2);
    AssertEquals('work on a connection after its destruction',0,
                 ReadCounter(ContinuedAfterFree));
  finally
    Pump.Terminate;
    FreeAndNil(A);
    FreeAndNil(Pump);
    FreeAndNil(Srv);
  end;
end;

initialization
  RegisterTest(TTestWSClientCallbacks);
end.
