{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by the Free Pascal development team

    Shared infrastructure for the fpwebsocketclient lifetime tests.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
  What is tested
  --------------
  The threaded message pump and the client lifetime handling of
  fpwebsocketclient: the upgrade handshake and echo, pump start and stop,
  readers blocked inside a partial frame, peer close and reset, lifecycle
  calls (Disconnect, Connect, Terminate, Free) made from inside callbacks,
  objects freed while their callbacks still run, and fairness between
  sessions sharing one pump.

  The tests need the loopback interface: every test starts its own server
  on 127.0.0.1 with a port chosen by the operating system. The TLS tests
  are ignored when no usable OpenSSL is available.

  The scenarios originate from the standalone regression harness
  wsshutdowntest.pas by Sven Harazim, extended by Roger Olsson. Comments
  in the test units name the scenario numbers of that harness.

  Probe classes keep their memory
  -------------------------------
  Several tests ask whether an object is still used after its destruction.
  A crash would answer that question at the cost of the whole run, so the
  probe classes (TProbeConnection, TProbeClient, TProbeResponse) override
  FreeInstance: CleanupInstance runs, the address is noted, but the block
  is not returned to the heap. The destructor has run and the fields are
  finalised, so the object is destroyed in every sense that matters - but
  the memory and the VMT pointer stay valid, and a later call lands in the
  probe class where it is counted instead of jumping into whatever the
  heap manager has since written there. The instances leak on purpose; a
  heap trace of this program will report them.

  Cross-thread state
  ------------------
  Callbacks fire on the pump's reader threads and on server threads, so
  nothing they touch may be a plain field read from the main thread.
  Counters are LongInt manipulated only through InterLocked*; strings are
  guarded by a critical section.
}
unit tcwsclienthelpers;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNIX}BaseUnix,{$ENDIF}
  {$IFDEF WINDOWS}Windows,{$ENDIF}
  SysUtils, Classes, sockets, ssockets, sslbase, sslsockets, opensslsockets,
  sha1, fpcunit, fpwebsocket, fpcustwsserver, fpwebsocketserver,
  fpwebsocketclient;

Const
  WaitLimitMs = 5000;   // how long a test waits for an expected event
  PollMs = 5;
  TestLimitS = 30;      // watchdog budget per test

{ ---------------------------------------------------------------------
  Counters
  --------------------------------------------------------------------- }

Function ReadCounter(Var aValue : LongInt) : LongInt;
Procedure BumpCounter(Var aValue : LongInt);

{ Wait until a counter reaches at least aWanted, or the limit expires. }
Function WaitForCount(Var aCounter : LongInt; aWanted : LongInt;
                      aLimitMs : Integer = WaitLimitMs) : Boolean;
{ The same, while playing the main thread's message loop so that
  synchronized methods can run. }
Function WaitServicing(Var aCounter : LongInt; aWanted : LongInt;
                       aLimitMs : Integer = WaitLimitMs) : Boolean;

{ ---------------------------------------------------------------------
  Watchdog. A test that hangs would otherwise make the whole run stall
  silently, and the hanging call can never report its own failure.
  --------------------------------------------------------------------- }

Procedure ArmWatchdog(Const aName : String; aLimitS : Integer);
Procedure DisarmWatchdog;

{ ---------------------------------------------------------------------
  Ports and connects
  --------------------------------------------------------------------- }

{ The port the operating system assigned to a socket bound to port 0. }
Function BoundPortOf(aSocket : TSocket) : Word;

{ A refused connect right after a server started is a setup race, not a
  result; retry for up to two seconds before it counts. }
Procedure ConnectWithRetry(aClient : TCustomWebsocketClient);

{ ---------------------------------------------------------------------
  Sec-WebSocket-Accept, needed by the hand-rolled stall peer.
  --------------------------------------------------------------------- }

Function CalcAccept(Const aKey : String) : String;

{ ---------------------------------------------------------------------
  Echo server. Three behaviours: echo, close after the first message, or
  send half a frame and then stay silent.
  --------------------------------------------------------------------- }

Type
  TServerMode = (smEcho, smCloseAfterMessage, smStallAfterMessage,
                 smCloseFrameAfterMessage);

  { TWebSocketServer's own threaded handler starts FreeOnTerminate workers
    that nobody joins, and StopServer does not join its accept thread
    either. In one process a late worker could run into the next test, so
    this handler owns its worker threads and joins them before the server
    state they use is freed. }
  TOwnedConnectionHandler = Class(TWSThreadedConnectionHandler)
  Private
    FThreads : TThreadList;
  Public
    Constructor Create(aServer : TCustomWSServer); override;
    Destructor Destroy; override;
    Procedure HandleConnection(aConnection : TWSServerConnection; DoHandshake : Boolean); override;
    Procedure ReportError(aConnection : TWSServerConnection; E : Exception);
    Procedure ConnectionFinished(aConnection : TWSServerConnection);
    { Waits for every worker to finish and frees it. False if one is still
      running when the limit expires; that worker is then never freed. }
    Function JoinWorkers(aLimitMs : Integer) : Boolean;
  end;

  { Joins the accept thread and the connection workers before the socket
    server and the handler are freed, and exposes the bound port
    (TWebSocketServer.Server is protected). }
  TTestWSServer = Class(TWebSocketServer)
  Private
    FAcceptDone : PRTLEvent;
    FAcceptPending : Boolean;
    FAcceptFinished : LongInt;
    FTeardownError : String;
  Protected
    Function CreateConnectionHandler : TWSServerConnectionHandler; override;
    Procedure StartServerSocket; override;
    Procedure StartAccepting; override;
    Procedure FreeServerSocket; override;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Function BoundPort : Word;
    { Non-empty when a thread did not stop within the limit at teardown. }
    Property TeardownError : String Read FTeardownError;
  end;

Var
  { Bumped when a test server could not join its threads; the base test
    case turns it into an error so that a thread left running is never
    silently carried into the next test. }
  ServerTeardownFailures : LongInt = 0;

Type
  TEchoServer = Class
  Private
    FServer : TTestWSServer;
    FPort : Word;
    FMode : TServerMode;
    FCert : TCertificateData;
    FRelease : LongInt;   // set by the test to let a parked callback return
    FStopping : LongInt;  // set during teardown so a parked callback exits
    FLastError : String;
    FErrLock : TRTLCriticalSection;
    Procedure DoMessage(Sender : TObject; Const aMessage : TWSMessage);
    Procedure DoGetHandler(Sender : TObject; Const aUseSSL : Boolean; Out aHandler : TSocketHandler);
    Procedure NoteError(Const aMsg : String);
  Public
    FReceived : LongInt;
    FHalfSent : LongInt;
    FErrors : LongInt;
    Constructor Create(aMode : TServerMode; aUseSSL : Boolean);
    Destructor Destroy; override;
    { Binds and listens before returning; Port is valid afterwards. }
    Procedure Start;
    Procedure Stop;
    Function LastError : String;
    Procedure Release;
    Property Port : Word Read FPort;
  end;

{ ---------------------------------------------------------------------
  Read instrumentation.

  The stall tests need to prove that the reader is actually parked in a
  payload read, not merely that time has passed. TWSConnection's
  CheckIncoming is not virtual, but GetTransport is, so a delegating
  IWSTransport can count entries and exits of the blocking reads.

  Frame reads (ReadBytes/ReadBuffer) and handshake line reads (ReadLn)
  are counted separately: the client reads its handshake response through
  the same transport, so a single counter would already be satisfied by a
  successful Connect. While ReadEntries > ReadExits the frame reader is
  inside a read. Only one instrumented client exists per test, and the
  counters are reset before every test.
  --------------------------------------------------------------------- }

Var
  ReadEntries : LongInt = 0;       // frame reads entered
  ReadExits : LongInt = 0;         // frame reads left
  LineReadEntries : LongInt = 0;   // handshake ReadLn calls entered
  LineReadExits : LongInt = 0;     // handshake ReadLn calls left

Function ReaderIsInsideRead : Boolean;
{ True once the reader has stayed inside a read for a while. A single
  sample can hit the short read of a frame header, which ends by itself
  before the read that stalls begins. }
Function WaitReaderParked(aStableMs : Integer = 100) : Boolean;
Function ReadCountersDetail : String;

Type
  { IWSTransport is a CORBA-style interface, so references through it are
    NOT reference counted. The wrapper therefore has to be owned and freed
    explicitly by the connection that installs it. }
  TInstrumentedTransport = Class(TObject, IWSTransport)
  Private
    FInner : IWSTransport;
  Public
    Constructor Create(aInner : IWSTransport);
    Function CanRead(aTimeOut: Integer) : Boolean;
    Procedure ReadBuffer(aBytes : TBytes);
    Function ReadBytes(var aBytes : TBytes; aCount : Integer) : Integer;
    Function WriteBytes(aBytes : TBytes; aCount : Integer) : Integer;
    Procedure WriteBuffer(aBytes : TBytes);
    Function ReadLn : String;
    Function PeerIP : string;
    Function PeerPort : word;
  end;

  TInstrumentedConnection = Class(TWebSocketClientConnection)
  Private
    FWrapper : TInstrumentedTransport;   // owned, see the note above
  Protected
    Function GetTransport : IWSTransport; override;
  Public
    Destructor Destroy; override;
  end;

  TInstrumentedClient = Class(TWebsocketClient)
  Protected
    Function CreateClientConnection(aTransport : TWSClientTransport) : TWebsocketClientConnection; override;
  end;

{ ---------------------------------------------------------------------
  Lifetime probes, see the unit header. Markers are published and read
  under a lock, so a reader never meets a slot that is counted but not
  yet written. The state below is reset before every test.
  --------------------------------------------------------------------- }

Var
  UseAfterFree : LongInt = 0;          // DoDisconnect entered on a destroyed connection
  ContinuedAfterFree : LongInt = 0;    // a connection kept working after its destruction
  ProbeClientDestroying : LongInt = 0; // Free of a probe client has begun
  RaiseOnDestroyAddr : Pointer = Nil;  // the probe connection whose destructor raises
  DestructorRaised : LongInt = 0;      // that destructor ran and raised
  DestructorThread : TThreadID = 0;    // the thread it ran on
  WatchSlowDone : PLongInt = Nil;      // the callback counter read by that destructor
  CallbackDoneAtDestroy : LongInt = -1;
  ProbePumpDestroying : LongInt = 0;   // Free of a probe pump has begun

Procedure ResetProbeState;
Function WasDestroyed(aAddr : Pointer) : Boolean;
{ How often an address was destroyed. Probe memory is never returned, so
  an address cannot be reused, and a count above one is a double free. }
Function DestroyedTimes(aAddr : Pointer) : Integer;
Function WaitDestroyed(aAddr : Pointer; aLimitMs : Integer = WaitLimitMs) : Boolean;

Type
  TProbeConnection = Class(TWebSocketClientConnection)
  Protected
    Function HandleIncoming(aFrame : TWSFrame) : Boolean; override;
  Public
    Destructor Destroy; override;
    Procedure FreeInstance; override;
    Procedure DoDisconnect; override;
    Procedure Send(aFrame : TWSFrame); overload; override;
  end;

  { The handshake response a probe client creates. Keeps its memory like
    the other probes, so a freed response can be recognised. }
  TProbeResponse = Class(TWSHandShakeResponse)
  Public
    Procedure FreeInstance; override;
  end;

  TProbeClient = Class(TWebsocketClient)
  Protected
    Function CreateClientConnection(aTransport : TWSClientTransport) : TWebsocketClientConnection; override;
    Function CreateHandshakeResponse(aHeaders : TStrings) : TWSHandShakeResponse; override;
  Public
    Procedure FreeInstance; override;
    Procedure BeforeDestruction; override;
  end;

  { TWSMessagePump.List is protected, so asking the pump which connections
    it still tracks needs a descendant rather than a cast. }
  TProbePump = Class(TWSThreadMessagePump)
  Public
    Function ClientCount : Integer;
    Function Tracks(aConnection : TWSClientConnection) : Boolean;
    Procedure BeforeDestruction; override;
  end;

  { TWSErrorEvent is a method pointer, so the pump's OnError needs an
    object to report into. }
  TErrorSink = Class
  Private
    FLast : String;
    FLock : TRTLCriticalSection;
  Public
    FErrors : LongInt;
    FReadErrors : LongInt;
    Constructor Create;
    Destructor Destroy; override;
    Procedure DoError(Sender : TObject; E : Exception);
    Function LastError : String;
  end;

{ ---------------------------------------------------------------------
  Client wrapper. Counters rather than flags, so "exactly once" can be
  asserted. The fields are public because the tests wait on them.
  --------------------------------------------------------------------- }

Type
  TTestClient = Class
  Private
    FLast : String;
    FLastLock : TRTLCriticalSection;
    FTerminateError : String;
    FTermLock : TRTLCriticalSection;
    FSelfError : String;
    Procedure DoNothing;
    Procedure DoSyncTerminate;
    Procedure DoControl(Sender : TObject; aType : TFrameType; Const aData : TBytes);
    Procedure DoSyncDisconnectOther;
    Procedure DoMessage(Sender : TObject; Const aMessage : TWSMessage);
    Procedure DoDisconnect(Sender : TObject);
    Procedure RunSelfAction;
    Procedure HoldUntilReleased;
  Public
    FClient : TWebsocketClient;
    FMessages : LongInt;
    FDisconnects : LongInt;
    { When set, OnDisconnect calls Terminate on this pump - from the reader
      thread. }
    FTerminateOnDisconnect : TWSMessagePump;
    FTerminateEntered : LongInt;    // the callback reached the Terminate call
    FTerminateReturned : LongInt;   // Terminate returned normally
    FTerminateRaised : LongInt;     // Terminate raised an exception
    { When set, OnMessage parks in TThread.Synchronize. That call only
      returns once someone runs CheckSynchronize on the main thread. }
    FSyncOnMessage : Boolean;
    FSyncEntered : LongInt;    // the callback is about to call Synchronize
    FSyncReturned : LongInt;   // Synchronize returned
    { When set, OnMessage raises an ordinary exception after counting the
      message - an application callback that fails, nothing more. }
    FRaiseOnMessage : Boolean;
    { When set, the first OnDisconnect disconnects this other client. }
    FTearDownOnDisconnect : TCustomWebsocketClient;
    { When set, OnMessage synchronizes a method that stops this pump. }
    FSyncTerminatePump : TWSMessagePump;
    { When set, receiving a close frame disconnects this other client from
      inside the control callback. }
    FTearDownOnCloseFrame : TCustomWebsocketClient;
    FControlCloses : LongInt;
    { When set, OnMessage synchronizes a method that disconnects this other
      client of the same pump. }
    FSyncDisconnectOther : TCustomWebsocketClient;
    FSyncOtherEntered : LongInt;   // the synchronized method started
    FSyncOtherReturned : LongInt;  // the synchronized method returned
    { When set, OnMessage stays in the callback this long and then checks
      whether the connection that delivered the message is still alive. }
    FSlowMessageMs : Integer;
    FSlowEntered : LongInt;        // the slow callback started
    FSlowDone : LongInt;           // the slow callback finished
    FSlowSawDestroyed : LongInt;   // its connection was destroyed before it finished
    FSlowConnection : TObject;     // the connection whose lifetime is checked
    FSlowDisconnectMs : Integer;
    FHoldDisconnectUntilRelease : Boolean; // OnDisconnect waits for FReleaseHold instead of sleeping
    FSlowDiscEntered : LongInt;    // the slow OnDisconnect started
    FSlowDiscDone : LongInt;       // the slow OnDisconnect finished
    FSlowDiscSawDestroyed : LongInt;
    FSyncTermEntered : LongInt;    // the synchronized method reached Terminate
    FSyncTermReturned : LongInt;   // Terminate returned inside it
    FSyncTermRaised : LongInt;     // Terminate raised inside it
    FSlowComponent : TObject;      // the client component whose lifetime is checked
    FSlowSawComponentDestroyed : LongInt;
    FSyncSelfAction : LongInt;     // OnMessage synchronizes an action on this client
    FDirectSelfAction : LongInt;   // OnMessage runs it directly on the reader thread
    FCloseFrameSelfDisconnect : Boolean;
    FReconnectOnDisconnect : LongInt; // 1 directly in OnDisconnect, 2 synchronized from it
    FPendingAction : LongInt;      // 1 disconnect, 2 disconnect and connect, 3 connect
    FSelfEntered : LongInt;        // the action started
    FSelfReturned : LongInt;       // the action returned
    FSelfRaised : LongInt;         // the action raised
    FCallbackDone : LongInt;       // OnMessage went on after the action
    FOldResponse : TObject;        // the handshake response of the connection in use
    FOldResponseFreed : LongInt;   // that response was freed while the callback still ran
    FRaiseOnDisconnect : LongInt;  // OnDisconnect raises once
    FHoldUntilRelease : Boolean;   // OnMessage waits for FReleaseHold instead of sleeping
    FReleaseHold : LongInt;
    FHoldTimedOut : LongInt;       // a hold ended by its time limit, not by a release
    Constructor Create(aPort : Word; aPump : TWSMessagePump; aUseSSL : Boolean = False;
                       aInstrument : Boolean = False; aProbe : Boolean = False);
    Destructor Destroy; override;
    { Lets a held callback (FHoldUntilRelease/FHoldDisconnectUntilRelease)
      return. }
    Procedure Release;
    Function LastMessage : String;
    Function TerminateError : String;
    Function SelfError : String;
    Property Client : TWebsocketClient Read FClient;
  end;

{ ---------------------------------------------------------------------
  Plain-TCP stall peer: completes the upgrade by hand, then sends two
  bytes of a frame header and holds the connection open.
  --------------------------------------------------------------------- }

Type
  TStallServer = Class(TThread)
  Private
    FPort : Word;
    FListener : TInetServer;
    FSendRest : LongInt;
    FCloseNow : LongInt;
    FLastError : String;
    FErrLock : TRTLCriticalSection;
    Procedure DoConnect(Sender : TObject; Data : TSocketStream);
    Procedure NoteError(Const aMsg : String);
  Public
    FHandshakes : LongInt;
    FHalfSent : LongInt;
    FRestSent : LongInt;
    { What happens after the half frame: 0 holds the connection, 1 closes it
      gracefully, 2 resets it - when CloseNow is called (or, failing that,
      when the wait limit expires). Set before the client connects. }
    FAfterHalf : LongInt;
    FClosedAfterHalf : LongInt;
    FErrors : LongInt;
    { Binds and listens before the accept thread starts; Port is valid
      afterwards. }
    Constructor Create;
    Destructor Destroy; override;
    Procedure Execute; override;
    Procedure Shutdown;
    { Release the five payload bytes the half frame announced. Used to make
      a parked read complete at a chosen moment. }
    Procedure SendRest;
    { Perform the close or reset announced by FAfterHalf now - the test
      calls this once it has seen the reader parked in the payload read. }
    Procedure CloseNow;
    Function LastError : String;
    Property Port : Word Read FPort;
  end;

{ ---------------------------------------------------------------------
  Is there a usable OpenSSL on this machine?
  --------------------------------------------------------------------- }

Function HaveTLS : Boolean;
Function TLSReason : String;

{ ---------------------------------------------------------------------
  Base class: resets the probe state and arms the watchdog for each test.
  --------------------------------------------------------------------- }

Type
  TWSClientTestCase = Class(TTestCase)
  Protected
    Procedure SetUp; override;
    Procedure TearDown; override;
    { Ignores the test when there is no usable OpenSSL. }
    Procedure RequireTLS;
    { Fails unless the instrumented reader is parked inside a read that
      cannot complete. }
    Procedure AssertReaderParked;
  end;

{ Polls until data is waiting on a connection (CheckIncoming without
  reading), or the limit expires. }
Function WaitIncomingWaiting(aConnection : TWSClientConnection;
                             aLimitMs : Integer = WaitLimitMs) : Boolean;

implementation

{ ---------------------------------------------------------------------
  Counters
  --------------------------------------------------------------------- }

Function ReadCounter(Var aValue : LongInt) : LongInt;
begin
  Result:=InterLockedExchangeAdd(aValue,0);
end;

Procedure BumpCounter(Var aValue : LongInt);
begin
  InterLockedIncrement(aValue);
end;

{ Limits are measured in real time: Sleep(PollMs) takes about 15 ms on
  Windows at the default timer resolution, so counting nominal poll
  intervals would stretch every limit threefold there. }
Function WaitForCount(Var aCounter : LongInt; aWanted : LongInt;
                      aLimitMs : Integer = WaitLimitMs) : Boolean;
Var
  Deadline : QWord;
begin
  Deadline:=TThread.GetTickCount64+QWord(aLimitMs);
  While (ReadCounter(aCounter)<aWanted) and (TThread.GetTickCount64<Deadline) do
    Sleep(PollMs);
  Result:=ReadCounter(aCounter)>=aWanted;
end;

Function WaitServicing(Var aCounter : LongInt; aWanted : LongInt;
                       aLimitMs : Integer = WaitLimitMs) : Boolean;
Var
  Deadline : QWord;
begin
  Deadline:=TThread.GetTickCount64+QWord(aLimitMs);
  While (ReadCounter(aCounter)<aWanted) and (TThread.GetTickCount64<Deadline) do
    CheckSynchronize(PollMs);
  Result:=ReadCounter(aCounter)>=aWanted;
end;

{ ---------------------------------------------------------------------
  Watchdog
  --------------------------------------------------------------------- }

Type
  TWatchdog = Class(TThread)
  Public
    Procedure Execute; override;
  end;

Var
  WatchLock : TRTLCriticalSection;
  WatchName : String = '';
  WatchLimitS : Integer = 0;
  WatchDeadline : QWord = 0;    // 0 = idle
  WatchGeneration : LongInt = 0; // bumped by every arm and disarm
  Watchdog : TWatchdog = Nil;

{ Ends the process without running unit finalization: the main thread is
  blocked in the hanging call and may hold locks that finalization needs. }
Procedure EndProcessNow(aCode : Integer);
begin
  {$IFDEF WINDOWS}
  ExitProcess(aCode);
  {$ELSE}
  {$IFDEF UNIX}
  FpExit(aCode);
  {$ELSE}
  Halt(aCode);
  {$ENDIF}
  {$ENDIF}
end;

Procedure TWatchdog.Execute;
begin
  While not Terminated do
    begin
    { Expiry is decided and acted on under the same lock that arms and
      disarms, so a deadline copied before a disarm can never end the
      process for a test that has already finished: while the lock is held
      here the armed generation cannot change. }
    EnterCriticalSection(WatchLock);
    try
      if (WatchDeadline<>0) and (GetTickCount64>WatchDeadline) then
        begin
        Writeln(ErrOutput);
        Writeln(ErrOutput,'HUNG: test "',WatchName,'" (watchdog generation ',
                WatchGeneration,') did not finish within ',WatchLimitS,
                ' s - the call under test never returned.');
        Flush(ErrOutput);
        Flush(Output);
        EndProcessNow(99);
        end;
    finally
      LeaveCriticalSection(WatchLock);
    end;
    Sleep(100);
    end;
end;

Procedure ArmWatchdog(Const aName : String; aLimitS : Integer);
begin
  EnterCriticalSection(WatchLock);
  try
    Inc(WatchGeneration);
    WatchName:=aName;
    WatchLimitS:=aLimitS;
    WatchDeadline:=GetTickCount64+QWord(aLimitS)*1000;
    if Watchdog=Nil then
      Watchdog:=TWatchdog.Create(False);
  finally
    LeaveCriticalSection(WatchLock);
  end;
end;

Procedure DisarmWatchdog;
begin
  EnterCriticalSection(WatchLock);
  try
    Inc(WatchGeneration);
    WatchName:='';
    WatchDeadline:=0;
  finally
    LeaveCriticalSection(WatchLock);
  end;
end;

Procedure StopWatchdog;
begin
  if Assigned(Watchdog) then
    begin
    Watchdog.Terminate;
    Watchdog.WaitFor;
    FreeAndNil(Watchdog);
    end;
end;

{ ---------------------------------------------------------------------
  Ports and connects
  --------------------------------------------------------------------- }

Function BoundPortOf(aSocket : TSocket) : Word;
Var
  Addr : TInetSockAddr;
  Len : TSockLen;
begin
  FillChar(Addr,SizeOf(Addr),0);
  Len:=SizeOf(Addr);
  if fpgetsockname(aSocket,psockaddr(@Addr),@Len)<>0 then
    Raise ESocketError.CreateFmt('getsockname failed: %d',[SocketError]);
  Result:=NToHs(Addr.sin_port);
end;

Procedure ConnectWithRetry(aClient : TCustomWebsocketClient);
Var
  Tries : Integer;
begin
  Tries:=0;
  repeat
    try
      aClient.Connect;
      Exit;
    except
      On E : ESocketError do
        begin
        Inc(Tries);
        if Tries>=20 then
          Raise;
        Sleep(100);
        end;
    end;
  until False;
end;

{ ---------------------------------------------------------------------
  Sec-WebSocket-Accept
  --------------------------------------------------------------------- }

Function CalcAccept(Const aKey : String) : String;
Var
  Hash : TSHA1Digest;
  B : TBytes;
begin
  Hash:=SHA1String(Trim(aKey)+SSecWebSocketGUID);
  SetLength(B,SizeOf(Hash));
  Move(Hash,B[0],Length(B));
  Result:=EncodeBytesBase64(B);
end;

{ ---------------------------------------------------------------------
  Echo server
  --------------------------------------------------------------------- }

{ Owned connection workers. The Execute body mirrors
  TWSThreadedConnectionHandler.TWSConnectionThread; only the ownership
  differs. }

Type
  TOwnedConnectionThread = Class(TThread)
  Private
    FHandler : TOwnedConnectionHandler;
    FConnection : TWSServerConnection;
    FDoHandshake : Boolean;
  Public
    Constructor Create(aHandler : TOwnedConnectionHandler;
                       aConnection : TWSServerConnection; aDoHandshake : Boolean);
    Procedure Execute; override;
  end;

Constructor TOwnedConnectionThread.Create(aHandler : TOwnedConnectionHandler;
  aConnection : TWSServerConnection; aDoHandshake : Boolean);
begin
  FHandler:=aHandler;
  FConnection:=aConnection;
  FDoHandshake:=aDoHandshake;
  FreeOnTerminate:=False;
  Inherited Create(True);
end;

Procedure TOwnedConnectionThread.Execute;
begin
  try
    if FDoHandshake then
      begin
      FConnection.PerformHandshake;
      if not FConnection.HandshakeResponseSent then
        Terminate;
      end;
    While not Terminated do
      if FConnection.CheckIncoming(FHandler.WaitTime)=irClose then
        begin
        if FConnection.CloseState<>csClosed then
          FConnection.Close('',CLOSE_NORMAL_CLOSURE);
        Terminate;
        end;
  except
    On E : Exception do
      FHandler.ReportError(FConnection,E);
  end;
  FHandler.ConnectionFinished(FConnection);
end;

Constructor TOwnedConnectionHandler.Create(aServer : TCustomWSServer);
begin
  inherited Create(aServer);
  FThreads:=TThreadList.Create;
end;

Destructor TOwnedConnectionHandler.Destroy;
begin
  JoinWorkers(WaitLimitMs);
  FreeAndNil(FThreads);
  inherited Destroy;
end;

Procedure TOwnedConnectionHandler.HandleConnection(aConnection : TWSServerConnection; DoHandshake : Boolean);
Var
  T : TOwnedConnectionThread;
begin
  T:=TOwnedConnectionThread.Create(Self,aConnection,DoHandshake);
  FThreads.Add(T);
  T.Start;
end;

Procedure TOwnedConnectionHandler.ReportError(aConnection : TWSServerConnection; E : Exception);
begin
  HandleError(aConnection,E);
end;

Procedure TOwnedConnectionHandler.ConnectionFinished(aConnection : TWSServerConnection);
begin
  RemoveConnection(aConnection);
end;

Function TOwnedConnectionHandler.JoinWorkers(aLimitMs : Integer) : Boolean;
Var
  L, Snapshot : TList;
  T : TThread;
  Deadline : QWord;
  I : Integer;
begin
  Result:=True;
  Deadline:=GetTickCount64+QWord(aLimitMs);
  Snapshot:=TList.Create;
  try
    L:=FThreads.LockList;
    try
      Snapshot.Assign(L);
      L.Clear;
    finally
      FThreads.UnlockList;
    end;
    For I:=0 to Snapshot.Count-1 do
      begin
      T:=TThread(Snapshot[I]);
      While (not T.Finished) and (GetTickCount64<Deadline) do
        Sleep(PollMs);
      if T.Finished then
        begin
        T.WaitFor;
        T.Free;
        end
      else
        begin
        { A running thread must never be freed; keep it so that a later
          join can retry, and report the failure. }
        Result:=False;
        L:=FThreads.LockList;
        try
          L.Add(T);
        finally
          FThreads.UnlockList;
        end;
        end;
      end;
  finally
    Snapshot.Free;
  end;
end;

{ TTestWSServer }

Constructor TTestWSServer.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FAcceptDone:=RTLEventCreate;
end;

Destructor TTestWSServer.Destroy;
begin
  inherited Destroy;
  RTLEventDestroy(FAcceptDone);
end;

Function TTestWSServer.CreateConnectionHandler : TWSServerConnectionHandler;
begin
  if ThreadMode=wtmThread then
    Result:=TOwnedConnectionHandler.Create(Self)
  else
    Result:=inherited CreateConnectionHandler;
end;

Procedure TTestWSServer.StartServerSocket;
begin
  { The inherited method binds, listens and then starts the accept thread
    (which calls StartAccepting below). }
  RTLEventResetEvent(FAcceptDone);
  InterLockedExchange(FAcceptFinished,0);
  FAcceptPending:=ThreadedAccept;
  inherited StartServerSocket;
end;

Procedure TTestWSServer.StartAccepting;
begin
  { Runs on the accept thread. }
  try
    inherited StartAccepting;
  finally
    InterLockedExchange(FAcceptFinished,1);
    RTLEventSetEvent(FAcceptDone);
  end;
end;

Procedure TTestWSServer.FreeServerSocket;
begin
  { StopServer has already stopped accepting and closed the connection
    sockets. Join the accept thread and the workers before the socket
    server and the handler they use are freed. }
  if FAcceptPending then
    begin
    FAcceptPending:=False;
    RTLEventWaitFor(FAcceptDone,WaitLimitMs);
    if InterLockedExchangeAdd(FAcceptFinished,0)=0 then
      FTeardownError:='the accept thread did not stop within '
                      +IntToStr(WaitLimitMs)+' ms';
    end;
  if (ConnectionHandler is TOwnedConnectionHandler)
     and not TOwnedConnectionHandler(ConnectionHandler).JoinWorkers(WaitLimitMs) then
    FTeardownError:='a connection worker did not stop within '
                    +IntToStr(WaitLimitMs)+' ms';
  inherited FreeServerSocket;
end;

Function TTestWSServer.BoundPort : Word;
begin
  Result:=BoundPortOf(Server.FPSocket.FD);
end;

{ TEchoServer }

Constructor TEchoServer.Create(aMode : TServerMode; aUseSSL : Boolean);
begin
  InitCriticalSection(FErrLock);
  FMode:=aMode;
  FServer:=TTestWSServer.Create(Nil);
  FServer.Port:=0;   // the operating system chooses a free port
  FServer.Host:='127.0.0.1';
  FServer.ThreadedAccept:=True;
  FServer.ThreadMode:=wtmThread;
  FServer.MessageWaitTime:=PollMs*5;   // the workers poll instead of spinning
  FServer.OnMessageReceived:=@DoMessage;
  if aUseSSL then
    begin
    { The server's built-in handler would also generate a certificate;
      supplying the handler here keeps the certificate data owned by the
      test and makes a handler failure visible through NoteError. The
      client runs with VerifyPeerCert:=False, so a generated self-signed
      certificate suffices. }
    FCert:=TCertificateData.Create;
    FCert.HostName:='127.0.0.1';
    FServer.OnGetSocketHandler:=@DoGetHandler;
    FServer.UseSSL:=True;
    end;
end;

Destructor TEchoServer.Destroy;
begin
  Stop;
  FreeAndNil(FServer);
  FreeAndNil(FCert);
  DoneCriticalSection(FErrLock);
  inherited Destroy;
end;

Procedure TEchoServer.NoteError(Const aMsg : String);
begin
  EnterCriticalSection(FErrLock);
  try
    FLastError:=aMsg;
  finally
    LeaveCriticalSection(FErrLock);
  end;
  BumpCounter(FErrors);
end;

Function TEchoServer.LastError : String;
begin
  EnterCriticalSection(FErrLock);
  try
    Result:=FLastError;
  finally
    LeaveCriticalSection(FErrLock);
  end;
end;

Procedure TEchoServer.DoGetHandler(Sender : TObject; Const aUseSSL : Boolean;
  Out aHandler : TSocketHandler);
Var
  S : TSSLSocketHandler;
  CK : TCertAndKey;
begin
  aHandler:=Nil;
  if not aUseSSL then
    Exit;
  S:=TSSLSocketHandler.GetDefaultHandler;
  try
    if FCert.NeedCertificateData then
      begin
      S.CertGenerator.HostName:=FCert.HostName;
      CK:=S.CertGenerator.CreateCertificateAndKey;
      FCert.Certificate.Value:=CK.Certificate;
      FCert.PrivateKey.Value:=CK.PrivateKey;
      end;
    S.CertificateData:=FCert;
    aHandler:=S;
  except
    On E : Exception do
      begin
      S.Free;
      NoteError('handler: '+E.Message);
      Raise;
      end;
  end;
end;

Procedure TEchoServer.DoMessage(Sender : TObject; Const aMessage : TWSMessage);
Var
  Con : TWSServerConnection;
  Half : TBytes;
begin
  BumpCounter(FReceived);
  try
    Con:=Sender as TWSServerConnection;
    Case FMode of
      smEcho:
        Con.Send(aMessage.AsString);
      smCloseAfterMessage:
        Con.Disconnect;
      smCloseFrameAfterMessage:
        { A proper close frame, 1000 = normal closure. Disconnect above
          only closes the socket, and then the client never dispatches a
          close control event. }
        Con.Close('bye',1000);
      smStallAfterMessage:
        begin
        { A final text frame announcing five payload bytes that never
          arrive. Written through the transport, so over TLS this lands
          inside a TLS record and parks the client in SSL_read. }
        Half:=[$81,$05];
        Con.Transport.WriteBuffer(Half);
        BumpCounter(FHalfSent);
        { Park here instead of returning. If this callback returned, the
          server would resume reading, notice the FIN that the client's
          own SHUT_RDWR produces, and close the connection - and that peer
          activity, not the local shutdown, would release the client's
          read. The plain-TCP peer holds its connection open in exactly
          the same way, so both tests differ only in transport. }
        While (ReadCounter(FRelease)=0) and (ReadCounter(FStopping)=0) do
          Sleep(25);
        end;
    end;
  except
    On E : Exception do
      NoteError('message: '+E.Message);
  end;
end;

Procedure TEchoServer.Release;
begin
  InterLockedExchange(FRelease,1);
end;

Procedure TEchoServer.Start;
begin
  { Bind and listen happen synchronously here; only accept runs in a
    thread. A client may therefore connect as soon as this returns. }
  FServer.Active:=True;
  FPort:=FServer.BoundPort;
end;

Procedure TEchoServer.Stop;
begin
  InterLockedExchange(FStopping,1);
  if Assigned(FServer) and FServer.Active then
    Try
      FServer.Active:=False;
      if FServer.TeardownError<>'' then
        begin
        NoteError('stop: '+FServer.TeardownError);
        BumpCounter(ServerTeardownFailures);
        end;
    except
      On E : Exception do
        NoteError('stop: '+E.Message);
    end;
end;

{ ---------------------------------------------------------------------
  Read instrumentation
  --------------------------------------------------------------------- }

Function ReaderIsInsideRead : Boolean;
begin
  Result:=ReadCounter(ReadEntries)>ReadCounter(ReadExits);
end;

Function WaitReaderParked(aStableMs : Integer = 100) : Boolean;
Var
  Deadline, InsideSince, Now : QWord;
begin
  Result:=False;
  Deadline:=TThread.GetTickCount64+WaitLimitMs;
  InsideSince:=0;   // 0 = not inside a read at the last sample
  Repeat
    Now:=TThread.GetTickCount64;
    if ReaderIsInsideRead then
      begin
      if InsideSince=0 then
        InsideSince:=Now
      else if Now-InsideSince>=QWord(aStableMs) then
        Exit(True);
      end
    else
      InsideSince:=0;
    Sleep(PollMs);
  until TThread.GetTickCount64>=Deadline;
end;

Function ReadCountersDetail : String;
begin
  Result:=Format('read entries=%d exits=%d',
                 [ReadCounter(ReadEntries),ReadCounter(ReadExits)]);
end;

Constructor TInstrumentedTransport.Create(aInner : IWSTransport);
begin
  FInner:=aInner;
end;

{ CanRead is a bounded select, not a blocking read - not counted. }
Function TInstrumentedTransport.CanRead(aTimeOut: Integer) : Boolean;
begin
  Result:=FInner.CanRead(aTimeOut);
end;

Procedure TInstrumentedTransport.ReadBuffer(aBytes : TBytes);
begin
  BumpCounter(ReadEntries);
  try
    FInner.ReadBuffer(aBytes);
  finally
    BumpCounter(ReadExits);
  end;
end;

Function TInstrumentedTransport.ReadBytes(var aBytes : TBytes; aCount : Integer) : Integer;
begin
  BumpCounter(ReadEntries);
  try
    Result:=FInner.ReadBytes(aBytes,aCount);
  finally
    BumpCounter(ReadExits);
  end;
end;

Function TInstrumentedTransport.WriteBytes(aBytes : TBytes; aCount : Integer) : Integer;
begin
  Result:=FInner.WriteBytes(aBytes,aCount);
end;

Procedure TInstrumentedTransport.WriteBuffer(aBytes : TBytes);
begin
  FInner.WriteBuffer(aBytes);
end;

{ Only the handshake reads lines; counted apart from the frame reads. }
Function TInstrumentedTransport.ReadLn : String;
begin
  BumpCounter(LineReadEntries);
  try
    Result:=FInner.ReadLn;
  finally
    BumpCounter(LineReadExits);
  end;
end;

Function TInstrumentedTransport.PeerIP : string;
begin
  Result:=FInner.PeerIP;
end;

Function TInstrumentedTransport.PeerPort : word;
begin
  Result:=FInner.PeerPort;
end;

Function TInstrumentedConnection.GetTransport : IWSTransport;
begin
  if FWrapper=Nil then
    FWrapper:=TInstrumentedTransport.Create(inherited GetTransport);
  Result:=FWrapper;
end;

Destructor TInstrumentedConnection.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FWrapper);
end;

Function TInstrumentedClient.CreateClientConnection(aTransport : TWSClientTransport) : TWebsocketClientConnection;
begin
  Result:=TInstrumentedConnection.Create(Self,aTransport,Options);
end;

{ ---------------------------------------------------------------------
  Lifetime probes
  --------------------------------------------------------------------- }

Var
  DestroyedAddrs : Array of Pointer;
  DestroyedCount : Integer = 0;
  DestroyLock : TRTLCriticalSection;

Procedure NoteDestroyed(aAddr : Pointer);
begin
  EnterCriticalSection(DestroyLock);
  try
    if DestroyedCount>=Length(DestroyedAddrs) then
      SetLength(DestroyedAddrs,DestroyedCount+64);
    DestroyedAddrs[DestroyedCount]:=aAddr;
    Inc(DestroyedCount);
  finally
    LeaveCriticalSection(DestroyLock);
  end;
end;

Function DestroyedTimes(aAddr : Pointer) : Integer;
Var
  I : Integer;
begin
  Result:=0;
  if aAddr=Nil then
    Exit;
  EnterCriticalSection(DestroyLock);
  try
    For I:=0 to DestroyedCount-1 do
      if DestroyedAddrs[I]=aAddr then
        Inc(Result);
  finally
    LeaveCriticalSection(DestroyLock);
  end;
end;

Function WasDestroyed(aAddr : Pointer) : Boolean;
begin
  Result:=DestroyedTimes(aAddr)>0;
end;

Function WaitDestroyed(aAddr : Pointer; aLimitMs : Integer = WaitLimitMs) : Boolean;
Var
  Deadline : QWord;
begin
  Deadline:=TThread.GetTickCount64+QWord(aLimitMs);
  While (not WasDestroyed(aAddr)) and (TThread.GetTickCount64<Deadline) do
    CheckSynchronize(PollMs);
  Result:=WasDestroyed(aAddr);
end;

{ Addresses noted in earlier tests can be forgotten: the memory behind
  them is never returned, so no later object can be created there. }
Procedure ResetProbeState;
begin
  EnterCriticalSection(DestroyLock);
  try
    DestroyedCount:=0;
  finally
    LeaveCriticalSection(DestroyLock);
  end;
  InterLockedExchange(ReadEntries,0);
  InterLockedExchange(ReadExits,0);
  InterLockedExchange(LineReadEntries,0);
  InterLockedExchange(LineReadExits,0);
  InterLockedExchange(UseAfterFree,0);
  InterLockedExchange(ContinuedAfterFree,0);
  InterLockedExchange(ProbeClientDestroying,0);
  InterLockedExchange(ProbePumpDestroying,0);
  InterLockedExchange(DestructorRaised,0);
  RaiseOnDestroyAddr:=Nil;
  DestructorThread:=0;
  WatchSlowDone:=Nil;
  CallbackDoneAtDestroy:=-1;
end;

Procedure TProbeConnection.FreeInstance;
begin
  { TObject.FreeInstance is CleanupInstance followed by FreeMem. The second
    half is skipped on purpose - see the unit header. }
  CleanupInstance;
  NoteDestroyed(Self);
end;

Procedure TProbeConnection.DoDisconnect;
begin
  if WasDestroyed(Self) then
    begin
    BumpCounter(UseAfterFree);
    Exit;
    end;
  inherited DoDisconnect;
end;

Function TProbeConnection.HandleIncoming(aFrame : TWSFrame) : Boolean;
begin
  Result:=inherited HandleIncoming(aFrame);
  { A callback run from inside may have destroyed this connection; whatever
    the inherited code did after that point ran on a destroyed object. }
  if WasDestroyed(Self) then
    BumpCounter(ContinuedAfterFree);
end;

Procedure TProbeConnection.Send(aFrame : TWSFrame);
begin
  if WasDestroyed(Self) then
    begin
    BumpCounter(ContinuedAfterFree);
    Exit;
    end;
  inherited Send(aFrame);
end;

Destructor TProbeConnection.Destroy;
begin
  inherited Destroy;
  { A connection destructor that fails after doing its work. Leaving by an
    exception skips FreeInstance, which keeps the memory anyway. }
  if (RaiseOnDestroyAddr<>Nil) and (Pointer(Self)=RaiseOnDestroyAddr) then
    begin
    RaiseOnDestroyAddr:=Nil;
    DestructorThread:=GetCurrentThreadID;
    if WatchSlowDone<>Nil then
      CallbackDoneAtDestroy:=InterLockedExchangeAdd(WatchSlowDone^,0);
    BumpCounter(DestructorRaised);
    Raise Exception.Create('deliberate failure in a connection destructor');
    end;
end;

Function TProbeClient.CreateClientConnection(aTransport : TWSClientTransport) : TWebsocketClientConnection;
begin
  Result:=TProbeConnection.Create(Self,aTransport,Options);
end;

Procedure TProbeClient.FreeInstance;
begin
  { As for the probe connection: the memory stays, so a callback that
    returns into a destroyed client component is counted, not a crash. }
  CleanupInstance;
  NoteDestroyed(Self);
end;

Function TProbeClient.CreateHandshakeResponse(aHeaders : TStrings) : TWSHandShakeResponse;
begin
  Result:=TProbeResponse.Create('',aHeaders);
end;

Procedure TProbeClient.BeforeDestruction;
begin
  { Marks the moment a thread has entered Free, before any teardown. }
  BumpCounter(ProbeClientDestroying);
  inherited BeforeDestruction;
end;

Procedure TProbeResponse.FreeInstance;
begin
  CleanupInstance;
  NoteDestroyed(Self);
end;

Function TProbePump.ClientCount : Integer;
Var
  L : TList;
begin
  L:=List.LockList;
  try
    Result:=L.Count;
  finally
    List.UnlockList;
  end;
end;

Function TProbePump.Tracks(aConnection : TWSClientConnection) : Boolean;
Var
  L : TList;
begin
  L:=List.LockList;
  try
    Result:=L.IndexOf(aConnection)>=0;
  finally
    List.UnlockList;
  end;
end;

Procedure TProbePump.BeforeDestruction;
begin
  { Marks the moment a thread has entered the pump's Free. }
  BumpCounter(ProbePumpDestroying);
  inherited BeforeDestruction;
end;

Constructor TErrorSink.Create;
begin
  InitCriticalSection(FLock);
end;

Destructor TErrorSink.Destroy;
begin
  DoneCriticalSection(FLock);
  inherited Destroy;
end;

Procedure TErrorSink.DoError(Sender : TObject; E : Exception);
begin
  EnterCriticalSection(FLock);
  try
    FLast:=E.ClassName+': '+E.Message;
  finally
    LeaveCriticalSection(FLock);
  end;
  if E is EWSReadError then
    BumpCounter(FReadErrors);
  BumpCounter(FErrors);
end;

Function TErrorSink.LastError : String;
begin
  EnterCriticalSection(FLock);
  try
    Result:=FLast;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

{ ---------------------------------------------------------------------
  Client wrapper
  --------------------------------------------------------------------- }

Constructor TTestClient.Create(aPort : Word; aPump : TWSMessagePump; aUseSSL : Boolean = False;
                               aInstrument : Boolean = False; aProbe : Boolean = False);
begin
  InitCriticalSection(FLastLock);
  InitCriticalSection(FTermLock);
  if aProbe then
    FClient:=TProbeClient.Create(Nil)
  else if aInstrument then
    FClient:=TInstrumentedClient.Create(Nil)
  else
    FClient:=TWebsocketClient.Create(Nil);
  FClient.HostName:='127.0.0.1';
  FClient.Port:=aPort;
  FClient.Resource:='/';
  FClient.UseSSL:=aUseSSL;
  FClient.MessagePump:=aPump;
  FClient.OnMessageReceived:=@DoMessage;
  FClient.OnDisconnect:=@DoDisconnect;
  FClient.OnControl:=@DoControl;
end;

Destructor TTestClient.Destroy;
begin
  FreeAndNil(FClient);
  DoneCriticalSection(FTermLock);
  DoneCriticalSection(FLastLock);
  inherited Destroy;
end;

Procedure TTestClient.DoNothing;
begin
  { The body is irrelevant; what matters is that it runs on the main
    thread, so the reader thread waits until the main thread services it. }
end;

Procedure TTestClient.DoControl(Sender : TObject; aType : TFrameType; Const aData : TBytes);
Var
  Other : TCustomWebsocketClient;
begin
  if aType<>ftClose then
    Exit;
  BumpCounter(FControlCloses);
  if Assigned(FTearDownOnCloseFrame) then
    begin
    Other:=FTearDownOnCloseFrame;
    FTearDownOnCloseFrame:=Nil;
    Other.Disconnect(False);
    end;
  if FCloseFrameSelfDisconnect then
    begin
    { Still inside CheckIncoming for this very connection: the close reply
      has already been written, and HandleIncoming goes on after the
      callback returns. }
    FCloseFrameSelfDisconnect:=False;
    InterLockedExchange(FPendingAction,1);
    RunSelfAction;
    end;
end;

Procedure TTestClient.DoSyncDisconnectOther;
Var
  Other : TCustomWebsocketClient;
begin
  BumpCounter(FSyncOtherEntered);
  Other:=FSyncDisconnectOther;
  FSyncDisconnectOther:=Nil;
  if Assigned(Other) then
    Other.Disconnect(False);
  BumpCounter(FSyncOtherReturned);
end;

Procedure TTestClient.DoSyncTerminate;
begin
  { Runs on the main thread, while the reader thread waits in Synchronize
    for this very method to return. }
  BumpCounter(FSyncTermEntered);
  try
    FSyncTerminatePump.Terminate;
    BumpCounter(FSyncTermReturned);
  except
    On E : Exception do
      BumpCounter(FSyncTermRaised);
  end;
end;

{ Held until another thread observed the operation this callback is meant
  to overlap; bounded, so a hang elsewhere stays visible. }
Procedure TTestClient.HoldUntilReleased;
Var
  HoldDeadline : QWord;
  Released : Boolean;
begin
  HoldDeadline:=TThread.GetTickCount64+10000;
  Released:=False;
  Repeat
    { The reason the loop ends is taken inside it: a release arriving
      just after the deadline must not hide the timeout. }
    if ReadCounter(FReleaseHold)<>0 then
      Released:=True
    else if TThread.GetTickCount64>=HoldDeadline then
      Break
    else
      Sleep(PollMs);
  until Released;
  if not Released then
    BumpCounter(FHoldTimedOut);
end;

Procedure TTestClient.Release;
begin
  InterLockedExchange(FReleaseHold,1);
end;

Procedure TTestClient.DoMessage(Sender : TObject; Const aMessage : TWSMessage);
begin
  EnterCriticalSection(FLastLock);
  try
    FLast:=aMessage.AsString;
  finally
    LeaveCriticalSection(FLastLock);
  end;
  BumpCounter(FMessages);
  if FSyncOnMessage then
    begin
    BumpCounter(FSyncEntered);
    TThread.Synchronize(TThread.CurrentThread,@DoNothing);
    BumpCounter(FSyncReturned);
    end;
  if Assigned(FSyncTerminatePump) then
    TThread.Synchronize(TThread.CurrentThread,@DoSyncTerminate);
  if Assigned(FSyncDisconnectOther) then
    TThread.Synchronize(TThread.CurrentThread,@DoSyncDisconnectOther);
  if ReadCounter(FSyncSelfAction)<>0 then
    begin
    InterLockedExchange(FPendingAction,InterLockedExchange(FSyncSelfAction,0));
    TThread.Synchronize(TThread.CurrentThread,@RunSelfAction);
    BumpCounter(FCallbackDone);
    end;
  if ReadCounter(FDirectSelfAction)<>0 then
    begin
    InterLockedExchange(FPendingAction,InterLockedExchange(FDirectSelfAction,0));
    RunSelfAction;
    { Still inside the old connection's callback: whatever the connection
      refers to has to be alive here. }
    if Assigned(FOldResponse) and WasDestroyed(Pointer(FOldResponse)) then
      BumpCounter(FOldResponseFreed);
    BumpCounter(FCallbackDone);
    end;
  if (FSlowMessageMs>0) or FHoldUntilRelease then
    begin
    BumpCounter(FSlowEntered);
    if FHoldUntilRelease then
      HoldUntilReleased
    else
      Sleep(FSlowMessageMs);
    { Sender is the client component here, not the connection, so the
      connection's address was noted before the message was sent. Only the
      pointer is looked up; the probe keeps a destroyed connection's memory,
      so this is safe either way. }
    if WasDestroyed(Pointer(FSlowConnection)) then
      BumpCounter(FSlowSawDestroyed);
    if Assigned(FSlowComponent) and WasDestroyed(Pointer(FSlowComponent)) then
      BumpCounter(FSlowSawComponentDestroyed);
    BumpCounter(FSlowDone);
    end;
  if FRaiseOnMessage then
    Raise Exception.Create('deliberate failure in an OnMessage callback');
end;

Procedure TTestClient.DoDisconnect(Sender : TObject);
Var
  Other : TCustomWebsocketClient;
  Mode : LongInt;
begin
  BumpCounter(FDisconnects);
  if InterLockedExchange(FRaiseOnDisconnect,0)<>0 then
    Raise Exception.Create('deliberate failure in an OnDisconnect handler');
  if (FSlowDisconnectMs>0) or FHoldDisconnectUntilRelease then
    begin
    BumpCounter(FSlowDiscEntered);
    if FHoldDisconnectUntilRelease then
      HoldUntilReleased
    else
      Sleep(FSlowDisconnectMs);
    if WasDestroyed(Pointer(FSlowConnection)) then
      BumpCounter(FSlowDiscSawDestroyed);
    BumpCounter(FSlowDiscDone);
    end;
  if ReadCounter(FReconnectOnDisconnect)<>0 then
    begin
    { Once only: the reconnected client gets its own OnDisconnect later. }
    Mode:=InterLockedExchange(FReconnectOnDisconnect,0);
    InterLockedExchange(FPendingAction,3);
    if Mode=2 then
      TThread.Synchronize(TThread.CurrentThread,@RunSelfAction)
    else
      RunSelfAction;
    end;
  if Assigned(FTearDownOnDisconnect) then
    begin
    { Once only: the teardown itself produces an OnDisconnect. }
    Other:=FTearDownOnDisconnect;
    FTearDownOnDisconnect:=Nil;
    Other.Disconnect(False);
    end;
  if Assigned(FTerminateOnDisconnect) then
    begin
    { Three outcomes have to be told apart: a normal return, an exception,
      and neither - the last one being a genuine self-join deadlock. On
      glibc pthread_join detects self-join and returns EDEADLK, which FPC
      ignores, so the failure surfaces later as an exception instead. }
    BumpCounter(FTerminateEntered);
    try
      FTerminateOnDisconnect.Terminate;
      BumpCounter(FTerminateReturned);
    except
      On E : Exception do
        begin
        EnterCriticalSection(FTermLock);
        try
          FTerminateError:=E.ClassName+': '+E.Message;
        finally
          LeaveCriticalSection(FTermLock);
        end;
        BumpCounter(FTerminateRaised);
        end;
    end;
    end;
end;

{ Runs the pending action on this client - on the reader thread or on the
  main thread, depending on who calls it. }
Procedure TTestClient.RunSelfAction;
Var
  Action : LongInt;
begin
  Action:=InterLockedExchange(FPendingAction,0);
  if Action=0 then
    Exit;
  BumpCounter(FSelfEntered);
  try
    if Action in [1,2] then
      FClient.Disconnect(False);
    if Action in [2,3] then
      ConnectWithRetry(FClient);
    BumpCounter(FSelfReturned);
  except
    On E : Exception do
      begin
      EnterCriticalSection(FTermLock);
      try
        FSelfError:=E.ClassName+': '+E.Message;
      finally
        LeaveCriticalSection(FTermLock);
      end;
      BumpCounter(FSelfRaised);
      end;
  end;
end;

Function TTestClient.SelfError : String;
begin
  EnterCriticalSection(FTermLock);
  try
    Result:=FSelfError;
  finally
    LeaveCriticalSection(FTermLock);
  end;
end;

Function TTestClient.TerminateError : String;
begin
  EnterCriticalSection(FTermLock);
  try
    Result:=FTerminateError;
  finally
    LeaveCriticalSection(FTermLock);
  end;
end;

Function TTestClient.LastMessage : String;
begin
  EnterCriticalSection(FLastLock);
  try
    Result:=FLast;
  finally
    LeaveCriticalSection(FLastLock);
  end;
end;

{ ---------------------------------------------------------------------
  Plain-TCP stall peer
  --------------------------------------------------------------------- }

{ SO_LINGER with a zero timeout turns the following close into a reset.
  The struct differs between the two platforms, and the TLinger of the
  sockets unit is the Unix shape, so it is spelled out here. }
Type
  TAbortLinger = packed record
    {$IFDEF WINDOWS}
    l_onoff  : Word;
    l_linger : Word;
    {$ELSE}
    l_onoff  : LongInt;
    l_linger : LongInt;
    {$ENDIF}
  end;

Function SetAbortiveClose(aSock : Longint) : Boolean;
Var
  L : TAbortLinger;
begin
  L.l_onoff:=1;
  L.l_linger:=0;
  Result:=fpsetsockopt(aSock,SOL_SOCKET,SO_LINGER,@L,SizeOf(L))=0;
end;

Constructor TStallServer.Create;
begin
  InitCriticalSection(FErrLock);
  FListener:=TInetServer.Create('127.0.0.1',0);   // port chosen by the OS
  FListener.OnConnect:=@DoConnect;
  FListener.QueueSize:=5;
  { Bind and listen here, so that a client may connect as soon as the
    constructor returns; StartAccepting only repeats the (idempotent)
    listen call. }
  FListener.Bind;
  FListener.Listen;
  FPort:=BoundPortOf(FListener.FPSocket.FD);
  FreeOnTerminate:=False;
  Inherited Create(False);
end;

{ Terminate, stop accepting, join - in that order - then release the
  listener. The destructor must not be the thing that stops the thread. }
Procedure TStallServer.Shutdown;
begin
  Terminate;
  if Assigned(FListener) then
    Try
      FListener.StopAccepting(True);
    except
      // the accept loop may already be gone
    end;
  WaitFor;
end;

Destructor TStallServer.Destroy;
begin
  { Safe even if the caller already did it: Terminate and StopAccepting
    are idempotent, and WaitFor on a finished thread returns at once. }
  Shutdown;
  FreeAndNil(FListener);
  inherited Destroy;
  DoneCriticalSection(FErrLock);
end;

Procedure TStallServer.NoteError(Const aMsg : String);
begin
  EnterCriticalSection(FErrLock);
  try
    FLastError:=aMsg;
  finally
    LeaveCriticalSection(FErrLock);
  end;
  BumpCounter(FErrors);
end;

Function TStallServer.LastError : String;
begin
  EnterCriticalSection(FErrLock);
  try
    Result:=FLastError;
  finally
    LeaveCriticalSection(FErrLock);
  end;
end;

Procedure TStallServer.DoConnect(Sender : TObject; Data : TSocketStream);

  { Read the request headers. Bounded, and gives up if the peer stops
    talking, so shutting the test down cannot hang in here forever. }
  Function ReadHeaders(Out aHeaders : String) : Boolean;
  Var
    C : Char;
    Res : String;
    N : Integer;
    IdleSince : QWord;
  begin
    Res:='';
    C:=#0;
    IdleSince:=GetTickCount64;
    While (Pos(#13#10#13#10,Res)=0) and (Length(Res)<8192) and (not Terminated) do
      begin
      { Data.Read goes straight into a blocking recv, so ask first. Without
        this the loop could never observe Terminated or its own deadline,
        and a stuck setup would look like the shutdown hang under test. }
      if not Data.CanRead(PollMs*10) then
        begin
        if GetTickCount64-IdleSince>WaitLimitMs then
          Break;
        Continue;
        end;
      N:=Data.Read(C,1);
      if N=1 then
        begin
        Res:=Res+C;
        IdleSince:=GetTickCount64;
        end
      else
        Break;   // peer closed or errored
      end;
    aHeaders:=Res;
    Result:=Pos(#13#10#13#10,Res)>0;
  end;

  Function ExtractKey(Const aHeaders : String; Out aKey : String) : Boolean;
  Var
    L : TStringList;
    I : Integer;
    N : String;
  begin
    aKey:='';
    L:=TStringList.Create;
    try
      L.Text:=aHeaders;
      For I:=0 to L.Count-1 do
        begin
        N:=L[I];
        if SameText(Copy(N,1,Length(SSecWebsocketKey)+1),SSecWebsocketKey+':') then
          begin
          aKey:=Trim(Copy(N,Length(SSecWebsocketKey)+2,Length(N)));
          Break;
          end;
        end;
    finally
      L.Free;
    end;
    Result:=aKey<>'';
  end;

  { Write everything or report failure - a short write would leave the
    client waiting for bytes we never sent, which would look like the
    stall we are trying to stage on purpose. }
  Function WriteAll(Const aBuf; aCount : Integer) : Boolean;
  Var
    Written, N : Integer;
    P : PByte;
  begin
    P:=@aBuf;
    Written:=0;
    While Written<aCount do
      begin
      N:=Data.Write(P[Written],aCount-Written);
      if N<=0 then
        Exit(False);
      Inc(Written,N);
      end;
    Result:=True;
  end;

Var
  Headers, Key, Resp : String;
  Half : Array[0..1] of Byte;
  Rest : Array[0..4] of Byte;
  Deadline : QWord;
begin
  try
    try
      if not ReadHeaders(Headers) then
        begin
        NoteError('incomplete request headers');
        Exit;
        end;
      if not ExtractKey(Headers,Key) then
        begin
        NoteError('no '+SSecWebsocketKey+' header');
        Exit;
        end;

      Resp:='HTTP/1.1 101 Switching Protocols'#13#10
           +'Upgrade: websocket'#13#10
           +'Connection: Upgrade'#13#10
           +SSecWebsocketAccept+': '+CalcAccept(Key)+#13#10
           +#13#10;
      if not WriteAll(Resp[1],Length(Resp)) then
        begin
        NoteError('short write on handshake response');
        Exit;
        end;
      BumpCounter(FHandshakes);

      { Final text frame announcing five payload bytes - which never come. }
      Half[0]:=$81;
      Half[1]:=$05;
      if not WriteAll(Half[0],2) then
        begin
        NoteError('short write on partial frame');
        Exit;
        end;
      BumpCounter(FHalfSent);

      { Leave mid-frame once the test has seen the client parked in the
        payload read (CloseNow), or when the wait limit expires. The
        finally below frees Data, which closes it. }
      if ReadCounter(FAfterHalf)<>0 then
        begin
        Deadline:=GetTickCount64+WaitLimitMs;
        While (ReadCounter(FCloseNow)=0) and (not Terminated)
              and (GetTickCount64<Deadline) do
          Sleep(PollMs);
        if (ReadCounter(FAfterHalf)=2) and not SetAbortiveClose(Data.Handle) then
          NoteError('SO_LINGER could not be set, the close will not be a reset')
        else
          BumpCounter(FClosedAfterHalf);
        Exit;
        end;

      { Hold the connection open. The client is now stuck waiting for a
        payload; a shutdown, a close, or SendRest can free it. }
      While not Terminated do
        begin
        if InterLockedExchange(FSendRest,0)=1 then
          begin
          Rest[0]:=$68; Rest[1]:=$65; Rest[2]:=$6C;   { 'hel' }
          Rest[3]:=$6C; Rest[4]:=$6F;                 { 'lo'  }
          if not WriteAll(Rest[0],5) then
            NoteError('short write on the frame payload')
          else
            BumpCounter(FRestSent);
          end;
        Sleep(1);
        end;
    except
      On E : Exception do
        NoteError('connection: '+E.Message);
    end;
  finally
    Data.Free;
  end;
end;

Procedure TStallServer.SendRest;
begin
  InterLockedExchange(FSendRest,1);
end;

Procedure TStallServer.CloseNow;
begin
  InterLockedExchange(FCloseNow,1);
end;

Procedure TStallServer.Execute;
begin
  try
    FListener.StartAccepting;
  except
    On E : Exception do
      if not Terminated then
        NoteError('accept: '+E.Message);
  end;
end;

{ ---------------------------------------------------------------------
  TLS availability
  --------------------------------------------------------------------- }

Var
  TLSChecked : Boolean = False;
  TLSAvailable : Boolean = False;
  TLSMissing : String = '';

Function HaveTLS : Boolean;
Var
  H : TSSLSocketHandler;
begin
  if not TLSChecked then
    begin
    TLSChecked:=True;
    try
      H:=TSSLSocketHandler.GetDefaultHandler;
      try
        TLSAvailable:=Assigned(H);
        if not TLSAvailable then
          TLSMissing:='no default SSL handler registered';
      finally
        H.Free;
      end;
    except
      On E : Exception do
        begin
        TLSAvailable:=False;
        TLSMissing:=E.Message;
        end;
    end;
    end;
  Result:=TLSAvailable;
end;

Function TLSReason : String;
begin
  HaveTLS;
  Result:=TLSMissing;
end;

{ ---------------------------------------------------------------------
  Base test case
  --------------------------------------------------------------------- }

Procedure TWSClientTestCase.SetUp;
begin
  inherited SetUp;
  ResetProbeState;
  InterLockedExchange(ServerTeardownFailures,0);
  ArmWatchdog(ClassName+'.'+TestName,TestLimitS);
end;

Procedure TWSClientTestCase.TearDown;
begin
  DisarmWatchdog;
  { Nothing of this test may be left in the synchronize queue. }
  CheckSynchronize(0);
  inherited TearDown;
  { A server thread still running would be carried into the next test;
    that is an error of this test, whatever else it reported. }
  if ReadCounter(ServerTeardownFailures)<>0 then
    Fail('a test server could not join its threads at teardown');
end;

Function WaitIncomingWaiting(aConnection : TWSClientConnection;
                             aLimitMs : Integer = WaitLimitMs) : Boolean;
Var
  Deadline : QWord;
begin
  Deadline:=TThread.GetTickCount64+QWord(aLimitMs);
  Repeat
    Result:=Assigned(aConnection)
            and (aConnection.CheckIncoming(0,False)=irWaiting);
    if Result then
      Exit;
    Sleep(PollMs);
  until TThread.GetTickCount64>=Deadline;
end;

Procedure TWSClientTestCase.RequireTLS;
begin
  if not HaveTLS then
    Ignore('no usable OpenSSL: '+TLSReason);
end;

Procedure TWSClientTestCase.AssertReaderParked;
Var
  Deadline : QWord;
begin
  { Precondition of the stall tests: the reader must actually be inside a
    read that cannot complete. Without this a test could pass because the
    reader noticed the stop request first and left gracefully. }
  Deadline:=TThread.GetTickCount64+WaitLimitMs;
  While (not ReaderIsInsideRead) and (TThread.GetTickCount64<Deadline) do
    Sleep(PollMs);
  AssertTrue('reader is parked inside a read ('+ReadCountersDetail+')',
             WaitReaderParked);
end;

initialization
  InitCriticalSection(WatchLock);
  InitCriticalSection(DestroyLock);

finalization
  StopWatchdog;
  DoneCriticalSection(DestroyLock);
  DoneCriticalSection(WatchLock);

end.
