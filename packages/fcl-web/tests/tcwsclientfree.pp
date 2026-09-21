{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by the Free Pascal development team

    fpwebsocketclient tests: objects freed while their callbacks run.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcwsclientfree;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpcunit, testregistry, fpwebsocket, fpwebsocketclient,
  tcwsclienthelpers;

Type

  { TTestWSClientFree }

  TTestWSClientFree = Class(TWSClientTestCase)
  Published
    Procedure TestFreeClientWhileItsCallbackRuns;
    Procedure TestFreeClientWhileItsCallbackSynchronizes;
    Procedure TestFreeClientWhileItsDisconnectNotificationRuns;
    Procedure TestFreeClientFromWorkerWhileCallbackSynchronizes;
    Procedure TestFreeReconnectedClientWhileOldCallbackRuns;
    Procedure TestFreePumpWhileReleasedConnectionCallbackRuns;
    Procedure TestReassignPumpWhileCallbackRunsIsRefused;
  end;

implementation

{ ---------------------------------------------------------------------
  Held callbacks. A callback that merely sleeps could finish before the
  operation meant to overlap it, and the test would pass without the
  overlap. The callbacks below are therefore held until a release thread
  has observed that Free has begun (ProbeClientDestroying), and the test
  asserts that the release, not a time limit, ended the hold.
  --------------------------------------------------------------------- }

Type
  { Sets a flag shortly after a counter was reached: releases a held
    callback once another thread has been observed inside the operation
    that is meant to overlap it. After ten seconds it releases anyway,
    without FFired. }
  TReleaseWhen = Class(TThread)
  Private
    FWatch : PLongInt;
    FFlag : PLongInt;
  Public
    FFired : LongInt;
    Constructor Create(aWatch, aFlag : PLongInt);
    Procedure Execute; override;
  end;

Constructor TReleaseWhen.Create(aWatch, aFlag : PLongInt);
begin
  FWatch:=aWatch;
  FFlag:=aFlag;
  FreeOnTerminate:=False;
  Inherited Create(False);
end;

Procedure TReleaseWhen.Execute;
Var
  Deadline : QWord;
begin
  Deadline:=TThread.GetTickCount64+10000;
  While (InterLockedExchangeAdd(FWatch^,0)=0) and (TThread.GetTickCount64<Deadline) do
    Sleep(PollMs);
  if InterLockedExchangeAdd(FWatch^,0)<>0 then
    BumpCounter(FFired);
  { Free is now under way; give it time to reach its wait before the
    callback is let go. }
  Sleep(200);
  InterLockedExchange(FFlag^,1);
end;

{ Scenario 18: a client freed from the main thread while its own message
  callback is still running on the reader thread. Freeing the client
  disconnects it; the connection object must not be freed while the
  reader is still inside the connection's callback. The probe connection
  keeps its memory after destruction, so a callback that outlives its
  connection is counted instead of crashing. }
Procedure TTestWSClientFree.TestFreeClientWhileItsCallbackRuns;
Var
  Srv : TEchoServer;
  Pump : TWSThreadMessagePump;
  A : TTestClient;
  R : TReleaseWhen;
begin
  R:=Nil;
  Srv:=TEchoServer.Create(smEcho,False);
  Pump:=TWSThreadMessagePump.Create(Nil);
  A:=Nil;
  try
    Srv.Start;
    A:=TTestClient.Create(Srv.Port,Pump,False,False,True);
    ConnectWithRetry(A.Client);
    A.FSlowConnection:=A.Client.Connection;
    A.FHoldUntilRelease:=True;
    Pump.Execute;
    A.Client.SendMessage('take your time');

    if not WaitForCount(A.FSlowEntered,1) then
      Fail('no echo arrived, so this test decides nothing');
    { The callback is released only once this thread is inside Free. Free
      only the websocket client; the test wrapper and its counters stay. }
    R:=TReleaseWhen.Create(@ProbeClientDestroying,@A.FReleaseHold);
    FreeAndNil(A.FClient);
    WaitForCount(A.FSlowDone,1,3000);
    AssertTrue('the callback finished',ReadCounter(A.FSlowDone)>0);
    AssertTrue('the callback was released only after Free had begun',
               ReadCounter(R.FFired)=1);
    AssertTrue('the callback ended by the release, not by its time limit',
               ReadCounter(A.FHoldTimedOut)=0);
    AssertTrue('the connection outlives its running callback',
               ReadCounter(A.FSlowSawDestroyed)=0);
  finally
    if Assigned(R) then
      begin
      R.WaitFor;
      FreeAndNil(R);
      end;
    Pump.Terminate;
    FreeAndNil(A);
    FreeAndNil(Pump);
    FreeAndNil(Srv);
  end;
end;

{ Scenario 19: a client freed from the main thread while its own message
  callback waits in TThread.Synchronize. The callback waits for the main
  thread; the main thread, inside Free, waits for the reader to be done
  with that connection. Only servicing Synchronize while waiting lets
  both finish. }
Procedure TTestWSClientFree.TestFreeClientWhileItsCallbackSynchronizes;
Var
  Srv : TEchoServer;
  Pump : TWSThreadMessagePump;
  A : TTestClient;
begin
  Srv:=TEchoServer.Create(smEcho,False);
  Pump:=TWSThreadMessagePump.Create(Nil);
  A:=Nil;
  try
    Srv.Start;
    A:=TTestClient.Create(Srv.Port,Pump,False,False,True);
    ConnectWithRetry(A.Client);
    A.FSyncOnMessage:=True;
    Pump.Execute;
    A.Client.SendMessage('wait for the main thread');

    if not WaitForCount(A.FSyncEntered,1) then
      Fail('no echo arrived, so this test decides nothing');
    { Deliberately not servicing Synchronize here: the question is whether
      Free does. If it cannot, control never returns and the watchdog
      reports the hang. }
    FreeAndNil(A.FClient);
    CheckSynchronize(0);
    AssertTrue('the callback''s Synchronize completed',
               WaitForCount(A.FSyncReturned,1,2000));
  finally
    CheckSynchronize(0);
    Pump.Terminate;
    FreeAndNil(A);
    FreeAndNil(Pump);
    FreeAndNil(Srv);
  end;
end;

{ Scenario 20: the owner frees a client while the pump's OnDisconnect
  notification for that client is still running. The client is already
  inactive then, so its destructor's Disconnect returns at once - the
  question is whether the connection is freed under the running
  notification. }
Procedure TTestWSClientFree.TestFreeClientWhileItsDisconnectNotificationRuns;
Var
  Srv : TEchoServer;
  Pump : TWSThreadMessagePump;
  A : TTestClient;
  R : TReleaseWhen;
begin
  R:=Nil;
  Srv:=TEchoServer.Create(smCloseAfterMessage,False);
  Pump:=TWSThreadMessagePump.Create(Nil);
  A:=Nil;
  try
    Srv.Start;
    A:=TTestClient.Create(Srv.Port,Pump,False,False,True);
    ConnectWithRetry(A.Client);
    A.FSlowConnection:=A.Client.Connection;
    A.FHoldDisconnectUntilRelease:=True;
    Pump.Execute;
    A.Client.SendMessage('bye');

    if not WaitForCount(A.FSlowDiscEntered,1) then
      Fail('the peer close was not reported, so this test decides nothing');
    { The notification is released only once this thread is inside Free. }
    R:=TReleaseWhen.Create(@ProbeClientDestroying,@A.FReleaseHold);
    FreeAndNil(A.FClient);
    WaitForCount(A.FSlowDiscDone,1,3000);
    AssertTrue('the notification finished',ReadCounter(A.FSlowDiscDone)>0);
    AssertTrue('the notification was released only after Free had begun',
               ReadCounter(R.FFired)=1);
    AssertTrue('the notification ended by the release, not by its time limit',
               ReadCounter(A.FHoldTimedOut)=0);
    AssertTrue('the connection outlives the running notification',
               ReadCounter(A.FSlowDiscSawDestroyed)=0);
  finally
    if Assigned(R) then
      begin
      R.WaitFor;
      FreeAndNil(R);
      end;
    Pump.Terminate;
    FreeAndNil(A);
    FreeAndNil(Pump);
    FreeAndNil(Srv);
  end;
end;

{ ---------------------------------------------------------------------
  Scenario 30: a worker thread frees a client while the client's callback
  waits in Synchronize, and the main thread keeps servicing its queue.
  Nothing waits for the worker, so Free may wait for the callback - and
  has to, before the client goes.
  --------------------------------------------------------------------- }

Type
  TFreeClientThread = Class(TThread)
  Private
    FOwner : TTestClient;
  Public
    FDone : LongInt;
    FSyncReturnedWhenFreed : LongInt;
    Constructor Create(aOwner : TTestClient);
    Procedure Execute; override;
  end;

Constructor TFreeClientThread.Create(aOwner : TTestClient);
begin
  FOwner:=aOwner;
  FreeOnTerminate:=False;
  Inherited Create(False);
end;

Procedure TFreeClientThread.Execute;
begin
  FreeAndNil(FOwner.FClient);
  InterLockedExchange(FSyncReturnedWhenFreed,ReadCounter(FOwner.FSyncReturned));
  BumpCounter(FDone);
end;

Procedure TTestWSClientFree.TestFreeClientFromWorkerWhileCallbackSynchronizes;
Var
  Srv : TEchoServer;
  Pump : TWSThreadMessagePump;
  A : TTestClient;
  W : TFreeClientThread;
begin
  Srv:=TEchoServer.Create(smEcho,False);
  Pump:=TWSThreadMessagePump.Create(Nil);
  A:=Nil;
  W:=Nil;
  try
    Srv.Start;
    A:=TTestClient.Create(Srv.Port,Pump,False,False,True);
    ConnectWithRetry(A.Client);
    A.FSyncOnMessage:=True;
    Pump.Execute;
    A.Client.SendMessage('wait for the main thread');

    if not WaitForCount(A.FSyncEntered,1) then
      Fail('no echo arrived, so this test decides nothing');
    W:=TFreeClientThread.Create(A);
    { The queue is not serviced until the worker is inside Free; otherwise
      the callback could finish first and nothing would overlap. }
    WaitForCount(ProbeClientDestroying,1,2000);
    Sleep(200);
    AssertTrue('the worker is inside Free while the callback still waits '
               +'(destroying=%d, Synchronize returned=%d)',
               [ReadCounter(ProbeClientDestroying),ReadCounter(A.FSyncReturned)],
               (ReadCounter(ProbeClientDestroying)=1) and (ReadCounter(A.FSyncReturned)=0));
    WaitServicing(W.FDone,1);
    AssertTrue('the worker''s Free returns',ReadCounter(W.FDone)=1);
    AssertTrue('Free waited for the callback',ReadCounter(W.FSyncReturnedWhenFreed)=1);
  finally
    CheckSynchronize(0);
    if Assigned(W) then
      begin
      { After a failed check the worker may still be inside Free, waiting
        for a callback that only this thread can service. }
      WaitServicing(W.FDone,1);
      W.WaitFor;
      FreeAndNil(W);
      end;
    Pump.Terminate;
    FreeAndNil(A);
    FreeAndNil(Pump);
    FreeAndNil(Srv);
  end;
end;

{ Scenario 31: while a slow callback of a client's connection runs, the
  owner disconnects and reconnects the client and then frees it. The
  running callback belongs to a connection the client no longer holds.
  Freeing the client must still wait for it, because the callback runs
  on behalf of the client component. }
Procedure TTestWSClientFree.TestFreeReconnectedClientWhileOldCallbackRuns;
Var
  Srv : TEchoServer;
  Pump : TWSThreadMessagePump;
  A : TTestClient;
  R : TReleaseWhen;
begin
  R:=Nil;
  Srv:=TEchoServer.Create(smEcho,False);
  Pump:=TWSThreadMessagePump.Create(Nil);
  A:=Nil;
  try
    Srv.Start;
    A:=TTestClient.Create(Srv.Port,Pump,False,False,True);
    ConnectWithRetry(A.Client);
    A.FSlowConnection:=A.Client.Connection;
    A.FSlowComponent:=A.Client;
    A.FHoldUntilRelease:=True;
    Pump.Execute;
    A.Client.SendMessage('take your time');

    if not WaitForCount(A.FSlowEntered,1) then
      Fail('no echo arrived, so this test decides nothing');
    A.Client.Disconnect(False);
    ConnectWithRetry(A.Client);
    AssertTrue('the old callback still runs when Free starts (disconnect or '
               +'reconnect waited for the callback, so nothing overlaps)',
               ReadCounter(A.FSlowDone)=0);
    { The callback is released only once this thread is inside Free. }
    R:=TReleaseWhen.Create(@ProbeClientDestroying,@A.FReleaseHold);
    FreeAndNil(A.FClient);
    WaitForCount(A.FSlowDone,1,5000);
    AssertTrue('the callback finished',ReadCounter(A.FSlowDone)>0);
    AssertTrue('the callback was released only after Free had begun',
               ReadCounter(R.FFired)=1);
    AssertTrue('the callback ended by the release, not by its time limit',
               ReadCounter(A.FHoldTimedOut)=0);
    AssertTrue('the client outlives the callback of its old connection',
               ReadCounter(A.FSlowSawComponentDestroyed)=0);
    AssertTrue('the old connection outlives its callback',
               ReadCounter(A.FSlowSawDestroyed)=0);
  finally
    if Assigned(R) then
      begin
      R.WaitFor;
      FreeAndNil(R);
      end;
    Pump.Terminate;
    FreeAndNil(A);
    FreeAndNil(Pump);
    FreeAndNil(Srv);
  end;
end;

{ Scenario 32: the pump is freed while a connection its owner already
  disconnected is still inside a slow callback on the reader thread. The
  connection must be destroyed exactly once, after its callback, and the
  client must be freeable afterwards. }
Procedure TTestWSClientFree.TestFreePumpWhileReleasedConnectionCallbackRuns;
Var
  Srv : TEchoServer;
  Pump : TProbePump;
  A : TTestClient;
  C0 : TWebSocketClientConnection;
  R : TReleaseWhen;
begin
  R:=Nil;
  Srv:=TEchoServer.Create(smEcho,False);
  Pump:=TProbePump.Create(Nil);
  A:=Nil;
  try
    Srv.Start;
    A:=TTestClient.Create(Srv.Port,Pump,False,False,True);
    ConnectWithRetry(A.Client);
    C0:=A.Client.Connection;
    A.FSlowConnection:=C0;
    A.FHoldUntilRelease:=True;
    Pump.Execute;
    A.Client.SendMessage('take your time');

    if not WaitForCount(A.FSlowEntered,1) then
      Fail('no echo arrived, so this test decides nothing');
    A.Client.Disconnect(False);
    AssertTrue('the disconnected connection''s callback still runs when the '
               +'pump is freed (Disconnect waited for the callback, so nothing '
               +'was pending)',ReadCounter(A.FSlowDone)=0);
    { The callback is released only once this thread is inside the pump's Free. }
    R:=TReleaseWhen.Create(@ProbePumpDestroying,@A.FReleaseHold);
    FreeAndNil(Pump);
    WaitForCount(A.FSlowDone,1,4000);
    WaitDestroyed(C0,2000);
    AssertTrue('the callback finished',ReadCounter(A.FSlowDone)>0);
    AssertTrue('the callback was released only after the pump''s Free had begun',
               ReadCounter(R.FFired)=1);
    AssertTrue('the callback ended by the release, not by its time limit',
               ReadCounter(A.FHoldTimedOut)=0);
    AssertTrue('the connection outlives its callback',ReadCounter(A.FSlowSawDestroyed)=0);
    AssertEquals('times the connection is destroyed',1,DestroyedTimes(C0));
    AssertEquals('work on the connection after its destruction',0,
                 ReadCounter(ContinuedAfterFree));
    { The client is inactive and no longer refers to the freed pump. }
    AssertNull('the pump reference was cleared',A.Client.MessagePump);
    FreeAndNil(A);
  finally
    if Assigned(R) then
      begin
      R.WaitFor;
      FreeAndNil(R);
      end;
    if Assigned(Pump) then
      Pump.Terminate;
    FreeAndNil(A);
    FreeAndNil(Pump);
    FreeAndNil(Srv);
  end;
end;

{ Scenario 34: while a slow callback of a client runs on its pump, the
  owner disconnects the client, assigns it another pump and frees it. The
  running callback belongs to the old pump. The assignment is refused
  while that callback runs, and freeing the client still waits for it -
  the client must not be freed under its own callback. }
Procedure TTestWSClientFree.TestReassignPumpWhileCallbackRunsIsRefused;
Var
  Srv : TEchoServer;
  P0, P1 : TWSThreadMessagePump;
  A : TTestClient;
  Raised, HeldAtAssign : Boolean;
  Msg : String;
  R : TReleaseWhen;
begin
  R:=Nil;
  Srv:=TEchoServer.Create(smEcho,False);
  P0:=TWSThreadMessagePump.Create(Nil);
  P1:=TWSThreadMessagePump.Create(Nil);
  A:=Nil;
  try
    Srv.Start;
    A:=TTestClient.Create(Srv.Port,P0,False,False,True);
    ConnectWithRetry(A.Client);
    A.FSlowConnection:=A.Client.Connection;
    A.FSlowComponent:=A.Client;
    A.FHoldUntilRelease:=True;
    P0.Execute;
    A.Client.SendMessage('take your time');

    if not WaitForCount(A.FSlowEntered,1) then
      Fail('no echo arrived, so this test decides nothing');
    A.Client.Disconnect(False);
    HeldAtAssign:=ReadCounter(A.FSlowDone)=0;
    Raised:=False;
    Msg:='';
    try
      A.Client.MessagePump:=P1;
    except
      On E : Exception do
        begin
        Raised:=True;
        Msg:=E.ClassName+': '+E.Message;
        end;
    end;
    AssertTrue('the callback still runs when the pump is reassigned '
               +'(Disconnect waited for the callback, so nothing overlaps)',
               HeldAtAssign);
    AssertTrue('the reassignment is refused while the old pump runs the callback',
               Raised);
    AssertTrue('the refusal is an EWebSocketClient and the old pump stays '
               +'assigned (%s)',[Msg],
               (Pos('EWebSocketClient',Msg)=1) and (A.Client.MessagePump=P0));
    { Released only once this thread is inside Free. }
    R:=TReleaseWhen.Create(@ProbeClientDestroying,@A.FReleaseHold);
    FreeAndNil(A.FClient);
    WaitForCount(A.FSlowDone,1,4000);
    AssertTrue('the callback finished',ReadCounter(A.FSlowDone)>0);
    AssertTrue('the callback was released only after Free had begun',
               ReadCounter(R.FFired)=1);
    AssertTrue('the callback ended by the release, not by its time limit',
               ReadCounter(A.FHoldTimedOut)=0);
    AssertTrue('the client outlives the callback on its old pump',
               ReadCounter(A.FSlowSawComponentDestroyed)=0);
    AssertTrue('the connection outlives its callback',
               ReadCounter(A.FSlowSawDestroyed)=0);
  finally
    if Assigned(R) then
      begin
      R.WaitFor;
      FreeAndNil(R);
      end;
    P0.Terminate;
    P1.Terminate;
    FreeAndNil(A);
    FreeAndNil(P0);
    FreeAndNil(P1);
    FreeAndNil(Srv);
  end;
end;

initialization
  RegisterTest(TTestWSClientFree);
end.
