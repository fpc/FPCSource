{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by the Free Pascal development team

    fpwebsocketclient tests: peer close and reset, pending notifications,
    exceptions in callbacks and destructors.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcwsclientpeer;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpcunit, testregistry, fpwebsocket, fpwebsocketclient,
  tcwsclienthelpers;

Type
  TSkipRound = Record
    Errors, DiscA : LongInt;
    TrackedA, TrackedB, Armed : Boolean;
    Error : String;
  end;

  TMidFrameRound = Record
    Armed, Active, Tracked : Boolean;
    Disc, Messages, Errors, ReadErrors, LateErrors : LongInt;
    NotifiedMs : Int64;
    FirstError : String;
  end;

  { TTestWSClientPeer }

  TTestWSClientPeer = Class(TWSClientTestCase)
  Private
    Function RunSkipRound(aRaise : Boolean) : TSkipRound;
    Function RunMidFrameClose(aMode : LongInt) : TMidFrameRound;
    Procedure CheckMidFrameRound(Const R : TMidFrameRound; aExpectReadError : Boolean);
  Published
    Procedure TestConcurrentPeerClosesReportedOnce;
    Procedure TestDisconnectFromCallbackWhileSiblingCallbackRuns;
    Procedure TestPendingNotificationDeliveredWithoutFailingCallback;
    Procedure TestExceptionInLaterClientKeepsPendingNotification;
    Procedure TestCloseFrameCallbackRemovingEarlierClientKeepsLaterClient;
    Procedure TestPeerClosesMidFrame;
    Procedure TestPeerResetsMidFrame;
    Procedure TestConnectionDestructorRaisingOnReaderKeepsPumpServing;
    Procedure TestOnDisconnectRaisingDuringFreeStillDestroysClient;
  end;

implementation

{ ---------------------------------------------------------------------
  Scenario 11, control half: two peers close before the pump is started,
  so both closes are pending when the readers begin and are processed
  concurrently. Each must be reported exactly once and nothing may be
  called on a destroyed connection.
  --------------------------------------------------------------------- }
Procedure TTestWSClientPeer.TestConcurrentPeerClosesReportedOnce;
Var
  SrvA, SrvB : TEchoServer;
  Pump : TWSThreadMessagePump;
  A, B : TTestClient;
begin
  SrvA:=TEchoServer.Create(smCloseAfterMessage,False);
  SrvB:=TEchoServer.Create(smCloseAfterMessage,False);
  Pump:=TWSThreadMessagePump.Create(Nil);
  A:=Nil;
  B:=Nil;
  try
    SrvA.Start;
    SrvB.Start;
    { The pump is deliberately left stopped until both closes are waiting. }
    A:=TTestClient.Create(SrvA.Port,Pump,False,False,True);
    B:=TTestClient.Create(SrvB.Port,Pump,False,False,True);
    A.Client.Connect;
    B.Client.Connect;
    A.Client.SendMessage('bye');
    B.Client.SendMessage('bye');
    { CheckIncoming with DoRead=False answers "is there something waiting"
      without consuming it. }
    if not (WaitIncomingWaiting(A.Client.Connection)
            and WaitIncomingWaiting(B.Client.Connection)) then
      Fail('the peers had not both closed before the pump started, so the '
          +'two closes were not pending together');
    Pump.Execute;
    AssertTrue('A''s close is reported',WaitForCount(A.FDisconnects,1));
    AssertTrue('B''s close is reported',WaitForCount(B.FDisconnects,1));
    { Room for a duplicate notification before counting. }
    Sleep(300);
    Pump.Terminate;
    AssertEquals('OnDisconnect count of A',1,ReadCounter(A.FDisconnects));
    AssertEquals('OnDisconnect count of B',1,ReadCounter(B.FDisconnects));
    AssertEquals('calls on a destroyed connection',0,ReadCounter(UseAfterFree));
  finally
    FreeAndNil(A);
    FreeAndNil(B);
    FreeAndNil(Pump);
    FreeAndNil(SrvA);
    FreeAndNil(SrvB);
  end;
end;

{ ---------------------------------------------------------------------
  Scenario 11, provocation half. The harness staged "a connection that an
  earlier OnDisconnect destroys while the pump still holds it in its
  notification queue". The rewritten pump has no such queue: every
  session has its own reader, so B is processed by its own thread
  regardless of what A's callback does. The provocation that remains is
  therefore staged explicitly: B's reader is held inside B's message
  callback, and A's OnDisconnect - on A's reader - disconnects B at that
  moment. B's connection is in use by its callback and must not be
  destroyed under it, nor twice, nor be touched after its destruction.
  --------------------------------------------------------------------- }
Procedure TTestWSClientPeer.TestDisconnectFromCallbackWhileSiblingCallbackRuns;
Var
  SrvA, SrvB : TEchoServer;
  Pump : TProbePump;
  A, B : TTestClient;
  CB : TWebSocketClientConnection;
begin
  SrvA:=TEchoServer.Create(smCloseAfterMessage,False);
  SrvB:=TEchoServer.Create(smEcho,False);
  Pump:=TProbePump.Create(Nil);
  A:=Nil;
  B:=Nil;
  try
    SrvA.Start;
    SrvB.Start;
    Pump.Execute;
    A:=TTestClient.Create(SrvA.Port,Pump,False,False,True);
    B:=TTestClient.Create(SrvB.Port,Pump,False,False,True);
    ConnectWithRetry(A.Client);
    ConnectWithRetry(B.Client);
    CB:=B.Client.Connection;
    B.FSlowConnection:=CB;
    B.FHoldUntilRelease:=True;
    B.Client.SendMessage('hold');
    if not WaitForCount(B.FSlowEntered,1) then
      Fail('B''s echo never arrived, so this test decides nothing');

    { A's peer closes; A's OnDisconnect disconnects B while B's reader is
      still inside B's callback. }
    A.FTearDownOnDisconnect:=B.Client;
    A.Client.SendMessage('bye');
    if not WaitForCount(A.FDisconnects,1) then
      Fail('A''s close was not reported, so B was never disconnected from a '
          +'callback and this test decides nothing');

    { The overlap, proven before B is released: B's callback is still
      running, and B has already been disconnected on A's thread. }
    AssertEquals('B''s callback finished before it was released',0,
                 ReadCounter(B.FSlowDone));
    AssertFalse('B is inactive after the disconnect from A''s callback',
                B.Client.Active);
    AssertEquals('OnDisconnect count of B while its callback still runs',1,
                 ReadCounter(B.FDisconnects));
    AssertEquals('times B''s connection was destroyed under its running callback',
                 0,DestroyedTimes(CB));

    B.Release;
    AssertTrue('B''s callback finished after the release',
               WaitForCount(B.FSlowDone,1));
    AssertEquals('B''s hold ended by its time limit',0,ReadCounter(B.FHoldTimedOut));
    WaitDestroyed(CB,2000);
    WaitForCount(B.FDisconnects,2,300);   // room for a second notification
    AssertTrue('B''s connection outlives its callback',
               ReadCounter(B.FSlowSawDestroyed)=0);
    AssertEquals('times B''s connection is destroyed',1,DestroyedTimes(CB));
    AssertEquals('OnDisconnect count of B',1,ReadCounter(B.FDisconnects));
    AssertFalse('B is no longer tracked by the pump',Pump.Tracks(CB));
    AssertEquals('calls on a destroyed connection',0,ReadCounter(UseAfterFree));
    AssertEquals('work on a connection after its destruction',0,
                 ReadCounter(ContinuedAfterFree));
  finally
    if Assigned(B) then
      B.Release;
    Pump.Terminate;
    FreeAndNil(A);
    FreeAndNil(B);
    FreeAndNil(Pump);
    FreeAndNil(SrvA);
    FreeAndNil(SrvB);
  end;
end;

{ ---------------------------------------------------------------------
  Scenario 12: an exception in a later client, and the notifications that
  were already pending. Two clients: the first one's peer closes, the
  second one's callback raises. A control round with the same
  arrangement, minus the failing callback, shows what happens when
  nothing interferes.
  --------------------------------------------------------------------- }

Function TTestWSClientPeer.RunSkipRound(aRaise : Boolean) : TSkipRound;
Var
  SrvA, SrvB : TEchoServer;
  Pump : TProbePump;
  Sink : TErrorSink;
  A, B : TTestClient;
  ConA, ConB : TWSClientConnection;
begin
  Result.Errors:=0;
  Result.DiscA:=0;
  Result.TrackedA:=False;
  Result.TrackedB:=False;
  Result.Armed:=False;
  Result.Error:='';
  SrvA:=TEchoServer.Create(smCloseAfterMessage,False);
  SrvB:=TEchoServer.Create(smEcho,False);
  Pump:=TProbePump.Create(Nil);
  Sink:=TErrorSink.Create;
  A:=Nil;
  B:=Nil;
  try
    Pump.OnError:=@Sink.DoError;
    SrvA.Start;
    SrvB.Start;

    { Connect order is list order, so A is visited before B. }
    A:=TTestClient.Create(SrvA.Port,Pump);
    B:=TTestClient.Create(SrvB.Port,Pump);
    A.Client.Connect;
    B.Client.Connect;
    B.FRaiseOnMessage:=aRaise;
    ConA:=A.Client.Connection;
    ConB:=B.Client.Connection;

    A.Client.SendMessage('bye');    { the peer closes }
    B.Client.SendMessage('echo');   { the peer answers, so a message waits }

    { Both have to be waiting before the pump starts, otherwise A can be
      notified before B raises, and every assertion would hold without the
      arrangement ever existing. }
    Result.Armed:=(Pump.ClientCount=2) and A.Client.Active and B.Client.Active
              and WaitIncomingWaiting(ConA) and WaitIncomingWaiting(ConB);

    Pump.Execute;
    WaitForCount(A.FDisconnects,1);
    if aRaise then
      WaitForCount(Sink.FErrors,1);
    { An observation window: a further error, or a late duplicate, would
      arrive here. }
    Sleep(300);

    Result.Errors:=ReadCounter(Sink.FErrors);
    Result.Error:=Sink.LastError;
    Result.DiscA:=ReadCounter(A.FDisconnects);
    Result.TrackedA:=Pump.Tracks(ConA);
    Result.TrackedB:=Pump.Tracks(ConB);
    Pump.Terminate;
  finally
    FreeAndNil(A);
    FreeAndNil(B);
    FreeAndNil(Pump);
    FreeAndNil(Sink);
    FreeAndNil(SrvA);
    FreeAndNil(SrvB);
  end;
end;

{ Control round of scenario 12: no failing callback. }
Procedure TTestWSClientPeer.TestPendingNotificationDeliveredWithoutFailingCallback;
Var
  R : TSkipRound;
begin
  R:=RunSkipRound(False);
  AssertTrue('the pump had two live clients waiting before the pass',R.Armed);
  AssertTrue('the closed connection is reported (OnDisconnect ran %d time(s))',
             [R.DiscA],R.DiscA>=1);
  AssertTrue('no error is reported (%s)',[R.Error],R.Errors=0);
end;

{ Provocation round of scenario 12: the second client's callback raises. }
Procedure TTestWSClientPeer.TestExceptionInLaterClientKeepsPendingNotification;
Var
  R : TSkipRound;
begin
  R:=RunSkipRound(True);
  if not R.Armed then
    Fail('the two clients were not both waiting before the pass, so A and B '
        +'may well have been handled in different passes');
  AssertTrue('the failing callback is reported through OnError (reported: "%s")',
             [R.Error],(R.Errors>=1) and (Pos('deliberate failure',R.Error)>0));
  AssertTrue('the closed connection is still reported to its owner '
             +'(OnDisconnect ran %d time(s), still tracked=%s)',
             [R.DiscA,BoolToStr(R.TrackedA,True)],R.DiscA>=1);
end;

{ ---------------------------------------------------------------------
  Scenario 14: a control callback that removes an earlier client while
  the pump is still iterating. Three clients in list order A, B, C. Only B
  has something waiting: a close frame. B's control callback disconnects
  A, which sits before B. An implementation that deletes by a stale index
  would hit C, a connection nobody closed.
  --------------------------------------------------------------------- }
Procedure TTestWSClientPeer.TestCloseFrameCallbackRemovingEarlierClientKeepsLaterClient;
Var
  SrvA, SrvB, SrvC : TEchoServer;
  Pump : TProbePump;
  A, B, C : TTestClient;
  ConB, ConC : TWSClientConnection;
  MsgC : LongInt;
  SendErr : String;
begin
  SrvA:=TEchoServer.Create(smEcho,False);
  SrvB:=TEchoServer.Create(smCloseFrameAfterMessage,False);
  SrvC:=TEchoServer.Create(smEcho,False);
  Pump:=TProbePump.Create(Nil);
  A:=Nil;
  B:=Nil;
  C:=Nil;
  try
    SrvA.Start;
    SrvB.Start;
    SrvC.Start;

    { The pump stays stopped until B's close frame is waiting, so that the
      first pass is the one that meets it. Connect order is list order. }
    A:=TTestClient.Create(SrvA.Port,Pump);
    B:=TTestClient.Create(SrvB.Port,Pump);
    C:=TTestClient.Create(SrvC.Port,Pump);
    A.Client.Connect;
    B.Client.Connect;
    C.Client.Connect;
    ConB:=B.Client.Connection;
    ConC:=C.Client.Connection;
    B.FTearDownOnCloseFrame:=A.Client;
    B.Client.SendMessage('bye');

    AssertTrue('three registered clients with only B waiting are in place',
               (Pump.ClientCount=3)
               and WaitIncomingWaiting(ConB)
               and (A.Client.Connection.CheckIncoming(0,False)=irNone)
               and (ConC.CheckIncoming(0,False)=irNone));
    Pump.Execute;
    WaitForCount(B.FDisconnects,1);
    Sleep(500);

    if (ReadCounter(B.FControlCloses)=0) or (ReadCounter(A.FDisconnects)=0) then
      Fail('the close-frame callback of B did not remove A, so the list '
          +'never shifted and this test decides nothing');
    AssertTrue('B, whose peer closed, is reported',ReadCounter(B.FDisconnects)>=1);
    AssertTrue('C, which nobody closed, is still tracked by the pump '
               +'(Active=%s, %d disconnect(s))',
               [BoolToStr(C.Client.Active,True),ReadCounter(C.FDisconnects)],
               Pump.Tracks(ConC));

    { The practical consequence: does C still get its messages? }
    MsgC:=ReadCounter(C.FMessages);
    SendErr:='';
    try
      C.Client.SendMessage('still there?');
    except
      On E : Exception do
        SendErr:=E.ClassName+': '+E.Message;
    end;
    AssertTrue('C accepts a send (%s)',[SendErr],SendErr='');
    AssertTrue('C still receives its echo (the echo was sent back but nobody '
               +'reads C any more)',WaitForCount(C.FMessages,MsgC+1,2000));
  finally
    FreeAndNil(A);
    FreeAndNil(B);
    FreeAndNil(C);
    FreeAndNil(Pump);
    FreeAndNil(SrvA);
    FreeAndNil(SrvB);
    FreeAndNil(SrvC);
  end;
end;

{ ---------------------------------------------------------------------
  Scenario 15: the peer goes away in the middle of a frame. The frame
  header announces five payload bytes, and then the peer closes the
  connection instead of sending them - once gracefully, once with a
  reset. Asked: is the owner told, does the connection leave the pump,
  is an orderly EOF kept out of OnError while a reset is reported exactly
  once, and does the pump stop reporting errors for it afterwards.
  --------------------------------------------------------------------- }

Function TTestWSClientPeer.RunMidFrameClose(aMode : LongInt) : TMidFrameRound;
Var
  Srv : TStallServer;
  Pump : TProbePump;
  Sink : TErrorSink;
  Cli : TTestClient;
  Con : TWSClientConnection;
  Started : QWord;
  AtNotify : LongInt;
begin
  Result.Armed:=False;
  Result.Active:=False;
  Result.Tracked:=False;
  Result.Disc:=0;
  Result.Messages:=0;
  Result.Errors:=0;
  Result.ReadErrors:=0;
  Result.LateErrors:=0;
  Result.NotifiedMs:=-1;
  Result.FirstError:='';
  Srv:=TStallServer.Create;
  Srv.FAfterHalf:=aMode;
  Pump:=TProbePump.Create(Nil);
  Sink:=TErrorSink.Create;
  Cli:=Nil;
  try
    Pump.OnError:=@Sink.DoError;
    Pump.Execute;
    Cli:=TTestClient.Create(Srv.Port,Pump,False,True);
    ConnectWithRetry(Cli.Client);
    Con:=Cli.Client.Connection;
    { The peer leaves only once the reader is seen parked in the payload
      read, so the close really lands in the middle of the frame. }
    Result.Armed:=WaitForCount(Srv.FHalfSent,1) and WaitReaderParked;
    if Result.Armed then
      begin
      Srv.CloseNow;
      Result.Armed:=WaitForCount(Srv.FClosedAfterHalf,1);
      end;

    Started:=TThread.GetTickCount64;
    if WaitForCount(Cli.FDisconnects,1,3000) then
      Result.NotifiedMs:=TThread.GetTickCount64-Started;
    { The pump may report the error just after the disconnect, so let that
      one arrive first. A further second then: an error that keeps coming
      would mean the pump is still reading the dead connection. }
    WaitForCount(Sink.FErrors,1,1000);
    AtNotify:=ReadCounter(Sink.FErrors);
    Sleep(1000);
    Result.LateErrors:=ReadCounter(Sink.FErrors)-AtNotify;
    Result.Errors:=ReadCounter(Sink.FErrors);
    Result.ReadErrors:=ReadCounter(Sink.FReadErrors);
    Result.FirstError:=Sink.LastError;
    Result.Disc:=ReadCounter(Cli.FDisconnects);
    Result.Messages:=ReadCounter(Cli.FMessages);
    Result.Active:=Cli.Client.Active;
    Result.Tracked:=Pump.Tracks(Con);
    Pump.Terminate;
  finally
    FreeAndNil(Cli);
    FreeAndNil(Pump);
    FreeAndNil(Sink);
    if Assigned(Srv) then
      begin
      Srv.Shutdown;
      FreeAndNil(Srv);
      end;
  end;
end;

Procedure TTestWSClientPeer.CheckMidFrameRound(Const R : TMidFrameRound;
  aExpectReadError : Boolean);
begin
  if not R.Armed then
    Fail('the half frame, the parked read or the close did not happen, so '
        +'this test decides nothing');
  AssertTrue('the owner is told (OnDisconnect ran %d time(s))',[R.Disc],R.Disc=1);
  AssertTrue('the terminal read is reported before the bounded wait expires '
             +'(notification time=%d ms)',[R.NotifiedMs],R.NotifiedMs>=0);
  AssertTrue('an incomplete frame is never delivered (%d message callback(s))',
             [R.Messages],R.Messages=0);
  AssertFalse('the client becomes inactive',R.Active);
  AssertFalse('the connection leaves the pump',R.Tracked);
  if aExpectReadError then
    AssertTrue('a reset reports exactly one terminal read error '
               +'(OnError=%d, EWSReadError=%d, last: %s)',
               [R.Errors,R.ReadErrors,R.FirstError],
               (R.Errors=1) and (R.ReadErrors=1))
  else
    AssertTrue('an orderly EOF does not report a transport error '
               +'(OnError=%d, EWSReadError=%d, last: %s)',
               [R.Errors,R.ReadErrors,R.FirstError],
               (R.Errors=0) and (R.ReadErrors=0));
  AssertTrue('no errors keep arriving for it afterwards (%d more in one second)',
             [R.LateErrors],R.LateErrors=0);
end;

Procedure TTestWSClientPeer.TestPeerClosesMidFrame;
begin
  CheckMidFrameRound(RunMidFrameClose(1),False);
end;

Procedure TTestWSClientPeer.TestPeerResetsMidFrame;
begin
  CheckMidFrameRound(RunMidFrameClose(2),True);
end;

{ ---------------------------------------------------------------------
  Scenario 35: the destructor of a connection its owner released raises
  when the reader frees it after the callback. The reader has to go on
  serving the other clients of the pump.
  --------------------------------------------------------------------- }
Procedure TTestWSClientPeer.TestConnectionDestructorRaisingOnReaderKeepsPumpServing;
Var
  SrvA, SrvB : TEchoServer;
  Pump : TProbePump;
  Sink : TErrorSink;
  A, B : TTestClient;
  C0 : TWebSocketClientConnection;
  DiscRaised : Integer;
begin
  SrvA:=TEchoServer.Create(smEcho,False);
  SrvB:=TEchoServer.Create(smEcho,False);
  Pump:=TProbePump.Create(Nil);
  Sink:=TErrorSink.Create;
  Pump.OnError:=@Sink.DoError;
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
    A.FHoldUntilRelease:=True;
    Pump.Execute;
    A.Client.SendMessage('take your time');

    if not WaitForCount(A.FSlowEntered,1) then
      Fail('no echo arrived, so this test decides nothing');
    WatchSlowDone:=@A.FSlowDone;
    RaiseOnDestroyAddr:=C0;
    DiscRaised:=0;
    try
      A.Client.Disconnect(False);
    except
      On E : Exception do
        Inc(DiscRaised);
    end;
    { The callback is still held, so the disconnect overlapped it and the
      connection can only be freed by the reader afterwards. }
    AssertEquals('the callback finished before the release',0,
                 ReadCounter(A.FSlowDone));
    A.Release;
    WaitForCount(A.FSlowDone,1,3000);
    WaitForCount(DestructorRaised,1,3000);
    B.Client.SendMessage('still served?');
    WaitForCount(B.FMessages,1);
    if ReadCounter(DestructorRaised)<>1 then
      Fail('the connection was not freed, so this test decides nothing');
    AssertTrue('the pump still serves the other client',ReadCounter(B.FMessages)>=1);
    AssertTrue('the failure does not reach the disconnecting caller',DiscRaised=0);
    AssertTrue('the destructor ran on the reader thread, after the callback '
               +'(callback done when the destructor ran=%d)',[CallbackDoneAtDestroy],
               (DestructorThread<>MainThreadID) and (CallbackDoneAtDestroy=1));
    AssertTrue('the failure is reported through OnError',ReadCounter(Sink.FErrors)>=1);
  finally
    if Assigned(A) then
      A.Release;
    RaiseOnDestroyAddr:=Nil;
    WatchSlowDone:=Nil;
    Pump.Terminate;
    FreeAndNil(A);
    FreeAndNil(B);
    FreeAndNil(Pump);
    FreeAndNil(Sink);
    FreeAndNil(SrvA);
    FreeAndNil(SrvB);
  end;
end;

{ ---------------------------------------------------------------------
  Scenario 36: OnDisconnect raises while an active client is freed. The
  client and its connection must still be destroyed.
  --------------------------------------------------------------------- }
Procedure TTestWSClientPeer.TestOnDisconnectRaisingDuringFreeStillDestroysClient;
Var
  Srv : TEchoServer;
  Pump : TProbePump;
  Sink : TErrorSink;
  A : TTestClient;
  C0 : TWebSocketClientConnection;
  Comp : Pointer;
  FreeRaised : Integer;
  Msg : String;
begin
  Srv:=TEchoServer.Create(smEcho,False);
  Pump:=TProbePump.Create(Nil);
  Sink:=TErrorSink.Create;
  Pump.OnError:=@Sink.DoError;
  A:=Nil;
  try
    Srv.Start;
    A:=TTestClient.Create(Srv.Port,Pump,False,False,True);
    ConnectWithRetry(A.Client);
    C0:=A.Client.Connection;
    Comp:=Pointer(A.Client);
    Pump.Execute;
    InterLockedExchange(A.FRaiseOnDisconnect,1);
    FreeRaised:=0;
    Msg:='';
    try
      FreeAndNil(A.FClient);
    except
      On E : Exception do
        begin
        Inc(FreeRaised);
        Msg:=E.ClassName+': '+E.Message;
        end;
    end;
    WaitDestroyed(C0,2000);
    if ReadCounter(A.FDisconnects)<>1 then
      Fail('the handler did not run, so this test decides nothing');
    AssertTrue('the client is destroyed despite the failing handler',WasDestroyed(Comp));
    AssertEquals('times its connection is destroyed',1,DestroyedTimes(C0));
    AssertTrue('Free does not raise the handler''s exception (%s)',[Msg],FreeRaised=0);
    AssertTrue('the failure is reported through OnError',ReadCounter(Sink.FErrors)>=1);
  finally
    Pump.Terminate;
    FreeAndNil(A);
    FreeAndNil(Pump);
    FreeAndNil(Sink);
    FreeAndNil(Srv);
  end;
end;

initialization
  RegisterTest(TTestWSClientPeer);
end.
