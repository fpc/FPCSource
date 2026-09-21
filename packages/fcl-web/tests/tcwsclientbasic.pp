{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by the Free Pascal development team

    fpwebsocketclient tests: connection, echo, peer close, TLS.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcwsclientbasic;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpcunit, testregistry, fpwebsocketclient,
  tcwsclienthelpers;

Type

  { TTestWSClientConnection }

  TTestWSClientConnection = Class(TWSClientTestCase)
  Published
    Procedure TestAcceptKeyKnownAnswer;
    Procedure TestUpgradeHandshakeAndEcho;
    Procedure TestRepeatedExecuteTerminateKeepsConnectionUsable;
    Procedure TestPeerCloseNotifiesOnceAndClearsActive;
    Procedure TestTerminateWhileIdleKeepsConnectionUsable;
    Procedure TestTLSUpgradeAndEcho;
    Procedure TestTLSPeerCloseNotifiesOwner;
    Procedure TestPumpFreedBeforeClient;
  end;

implementation

{ RFC 6455 section 1.3 known-answer vector. If this were wrong every stall
  test would fail for the wrong reason. }
Procedure TTestWSClientConnection.TestAcceptKeyKnownAnswer;
Const
  RFCKey    = 'dGhlIHNhbXBsZSBub25jZQ==';
  RFCExpect = 's3pPLMBiTxaQ9kYGzzhZRbK+xOo=';
begin
  AssertEquals('RFC 6455 accept vector',RFCExpect,CalcAccept(RFCKey));
end;

{ Scenario 1: HTTP upgrade handshake against FPC's own TWebSocketServer,
  and echo. }
Procedure TTestWSClientConnection.TestUpgradeHandshakeAndEcho;
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
    Cli.Client.Connect;
    AssertTrue('client is active after connect',Cli.Client.Active);
    Cli.Client.SendMessage('ping');
    AssertTrue('echo received',WaitForCount(Cli.FMessages,1));
    AssertEquals('echo content','ping',Cli.LastMessage);
    AssertEquals('server saw the message',1,ReadCounter(Srv.FReceived));
    AssertTrue('no server errors: '+Srv.LastError,ReadCounter(Srv.FErrors)=0);
    Pump.Terminate;
  finally
    FreeAndNil(Cli);
    FreeAndNil(Pump);
    FreeAndNil(Srv);
  end;
end;

{ Scenario 2: repeated Execute/Terminate on a healthy pump; each echo is
  matched by content so a stale reply cannot pass for a fresh one. }
Procedure TTestWSClientConnection.TestRepeatedExecuteTerminateKeepsConnectionUsable;
Var
  Srv : TEchoServer;
  Pump : TWSThreadMessagePump;
  Cli : TTestClient;
  I : Integer;
  Expect : String;
begin
  Srv:=TEchoServer.Create(smEcho,False);
  Pump:=TWSThreadMessagePump.Create(Nil);
  Cli:=Nil;
  try
    Srv.Start;
    Pump.Execute;
    Cli:=TTestClient.Create(Srv.Port,Pump);
    Cli.Client.Connect;
    For I:=1 to 5 do
      begin
      Pump.Terminate;
      Pump.Execute;
      Expect:='round'+IntToStr(I);
      Cli.Client.SendMessage(Expect);
      { Match on content: waiting for a count alone could be satisfied by
        a reply from an earlier round. }
      AssertTrue('round %d echoed (last="%s")',[I,Cli.LastMessage],
                 WaitForCount(Cli.FMessages,I,2000) and (Cli.LastMessage=Expect));
      end;
    AssertTrue('client still active',Cli.Client.Active);
    Pump.Terminate;
  finally
    FreeAndNil(Cli);
    FreeAndNil(Pump);
    FreeAndNil(Srv);
  end;
end;

{ Scenario 3: peer close: exactly one OnDisconnect, and the client goes
  inactive. }
Procedure TTestWSClientConnection.TestPeerCloseNotifiesOnceAndClearsActive;
Var
  Srv : TEchoServer;
  Pump : TWSThreadMessagePump;
  Cli : TTestClient;
begin
  Srv:=TEchoServer.Create(smCloseAfterMessage,False);
  Pump:=TWSThreadMessagePump.Create(Nil);
  Cli:=Nil;
  try
    Srv.Start;
    Pump.Execute;
    Cli:=TTestClient.Create(Srv.Port,Pump);
    Cli.Client.Connect;
    Cli.Client.SendMessage('bye');
    AssertTrue('OnDisconnect fired',WaitForCount(Cli.FDisconnects,1));
    AssertFalse('client is no longer active',Cli.Client.Active);
    { Give any duplicate notification time to show up before counting. }
    Sleep(300);
    AssertEquals('OnDisconnect count',1,ReadCounter(Cli.FDisconnects));
    Pump.Terminate;
  finally
    FreeAndNil(Cli);
    FreeAndNil(Pump);
    FreeAndNil(Srv);
  end;
end;

{ Scenario 4: Terminate while a healthy connection is idle. The reader is
  in its bounded poll, not in a blocking payload read. What is asserted is
  the compatibility promise: stopping a healthy pump must be quick and
  must leave the connection usable. }
Procedure TTestWSClientConnection.TestTerminateWhileIdleKeepsConnectionUsable;
Var
  Srv : TEchoServer;
  Pump : TWSThreadMessagePump;
  Cli : TTestClient;
  Started, Elapsed : QWord;
begin
  Srv:=TEchoServer.Create(smEcho,False);
  Pump:=TWSThreadMessagePump.Create(Nil);
  Cli:=Nil;
  try
    Srv.Start;
    Pump.Execute;
    Cli:=TTestClient.Create(Srv.Port,Pump);
    Cli.Client.Connect;
    { Lets the reader settle into its polling loop before it is stopped.
      Not a readiness gate: no assertion below can fail if it is cut short,
      the Terminate would merely have less to stop. }
    Sleep(300);

    Started:=GetTickCount64;
    Pump.Terminate;
    Elapsed:=GetTickCount64-Started;
    AssertTrue('Terminate returned promptly (%d ms)',[Int64(Elapsed)],
               Elapsed<WaitLimitMs);
    AssertTrue('connection survived Terminate',Cli.Client.Active);

    Pump.Execute;
    Cli.Client.SendMessage('after');
    AssertTrue('echo works again after restart (last="%s")',[Cli.LastMessage],
               WaitForCount(Cli.FMessages,1) and (Cli.LastMessage='after'));
    Pump.Terminate;
  finally
    FreeAndNil(Cli);
    FreeAndNil(Pump);
    FreeAndNil(Srv);
  end;
end;

{ Scenario 5, first half: upgrade and echo over TLS. }
Procedure TTestWSClientConnection.TestTLSUpgradeAndEcho;
Var
  Srv : TEchoServer;
  Pump : TWSThreadMessagePump;
  Cli : TTestClient;
begin
  RequireTLS;
  Srv:=TEchoServer.Create(smEcho,True);
  Pump:=TWSThreadMessagePump.Create(Nil);
  Cli:=Nil;
  try
    Srv.Start;
    Pump.Execute;
    Cli:=TTestClient.Create(Srv.Port,Pump,True);
    Cli.Client.Connect;
    Cli.Client.SendMessage('tls');
    AssertTrue('TLS echo received',WaitForCount(Cli.FMessages,1));
    AssertEquals('TLS echo content','tls',Cli.LastMessage);
    Pump.Terminate;
  finally
    FreeAndNil(Cli);
    FreeAndNil(Pump);
    FreeAndNil(Srv);
  end;
end;

{ Scenario 5, second half: a peer close over TLS must reach the owner just
  the same. }
Procedure TTestWSClientConnection.TestTLSPeerCloseNotifiesOwner;
Var
  Srv : TEchoServer;
  Pump : TWSThreadMessagePump;
  Cli : TTestClient;
begin
  RequireTLS;
  Srv:=TEchoServer.Create(smCloseAfterMessage,True);
  Pump:=TWSThreadMessagePump.Create(Nil);
  Cli:=Nil;
  try
    Srv.Start;
    Pump.Execute;
    Cli:=TTestClient.Create(Srv.Port,Pump,True);
    Cli.Client.Connect;
    Cli.Client.SendMessage('bye');
    AssertTrue('TLS peer close fires OnDisconnect',WaitForCount(Cli.FDisconnects,1));
    AssertFalse('TLS peer close clears Active',Cli.Client.Active);
    Pump.Terminate;
  finally
    FreeAndNil(Cli);
    FreeAndNil(Pump);
    FreeAndNil(Srv);
  end;
end;

{ The harness's --pump-first mode: the pump is destroyed before a client
  that still references it. SetMessagePump registers a FreeNotification;
  the client's Notification override must clear its pump pointer, otherwise
  the client's destructor calls RemoveClient on a freed component. }
Procedure TTestWSClientConnection.TestPumpFreedBeforeClient;
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
    Cli.Client.Connect;
    AssertTrue('client is active',Cli.Client.Active);
    { Lets the reader settle before the pump goes; not a readiness gate,
      the assertions below do not depend on it. }
    Sleep(300);

    FreeAndNil(Pump);
    AssertNull('the pump reference was cleared',Cli.Client.MessagePump);
    FreeAndNil(Cli);
  finally
    FreeAndNil(Cli);
    FreeAndNil(Pump);
    FreeAndNil(Srv);
  end;
end;

initialization
  RegisterTest(TTestWSClientConnection);
end.
