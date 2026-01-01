program tcptest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, fpsockets, ctypes;


const
{$if defined(win32)}
  LibName = 'msvcrt';
{$elseif defined(win64)}
  LibName = 'msvcrt';
{$elseif defined(wince)}
  LibName = 'coredll';
{$elseif defined(netware)}
  LibName = 'clib';
{$elseif defined(netwlibc)}
  LibName = 'libc';
{$elseif defined(macos)}
  LibName = 'StdCLib';
{$elseif defined(beos)}
  LibName = 'root';
{$else}
  LibName = 'c';
{$endif}

procedure CExit(status: cint); cdecl; external LibName name 'exit';

const
  HelloStr = 'Hello Server';
  ReplyStr = 'Hello Client!';

var ClientError, ServerError: String;

procedure IPv4TestServer;
var
  sock: TFPSocket;
  Conn: TFPSocketConnection;
  Received: String;
begin
  ServerError := '';
  try
    sock := TCPSocket(stIPv4);
    try
      Bind(sock, '0.0.0.0', 1337);
      Listen(sock, 0);
      Conn := AcceptConnection(sock);
      try
        Received := ReceiveStr(Conn.Socket);
        sleep(500);
        SendStr(Conn.Socket, ReplyStr);
      finally
        CloseSocket(Conn.Socket);
      end;
    finally
      CloseSocket(sock);
    end;
    if Received <> HelloStr then
      ServerError := 'Unexpected response: ' + Received;
  except on E: Exception do
    ServerError := 'Exception: ' + E.Message;
  end;
end;

procedure IPv4TestClient;
var
  sock: TFPSocket;
  Received: String;
begin
  ClientError := '';
  try
    sock := TCPSocket(stIPv4);
    try
      Connect(sock, '127.0.0.1', 1337);
      SendStr(sock, HelloStr);
      Received := ReceiveStr(sock, 16);
      if Received <> ReplyStr then
        ClientError := 'Unexpected response: ' + Received;
    finally
      CloseSocket(sock);
    end;
  except on E: Exception do
    ClientError := 'Exception: ' + E.Message;
  end;
end;

procedure IPv6TestServer;
var
  sock: TFPSocket;
  Conn: TFPSocketConnection;
  Received: String;
begin
  ServerError := '';
  try
    sock := TCPSocket(stIPv6);
    try
      Bind(sock, '::0', 1337);
      Listen(sock, 0);
      Conn := AcceptConnection(sock);
      try
        Received := ReceiveStr(Conn.Socket);
        SendStr(Conn.Socket, ReplyStr);
      finally
        CloseSocket(Conn.Socket);
      end;
    finally
      CloseSocket(sock);
    end;
    if Received <> HelloStr then
      ServerError := 'Unexpected response: ' + Received;
  except on E: Exception do
    ServerError := 'Exception: ' + E.Message;
  end;
end;

procedure IPv6TestClient;
var
  sock: TFPSocket;
  Received: String;
begin
  ClientError := '';
  try
    sock := TCPSocket(stIPv6);
    try
      Connect(sock, '::1', 1337);
      SendStr(sock, HelloStr);
      Received := ReceiveStr(sock);
      if Received <> ReplyStr then
        ClientError := 'Unexpected response: ' + Received;
    finally
      CloseSocket(sock);
    end;
  except on E: Exception do
    ClientError := 'Exception: ' + E.Message;
  end;
end;

procedure DualStackTestServer;
var
  sock: TFPSocket;
  Conn: TFPSocketConnection;
  Received: String;
begin
  ServerError := '';
  try
    sock := TCPSocket(stIPDualStack);
    try
      Bind(sock, '::0', 1337);
      Listen(sock, 0);
      Conn := AcceptConnection(sock);
      try
        Received := ReceiveStr(Conn.Socket);
        SendStr(Conn.Socket, ReplyStr);
      finally
        CloseSocket(Conn.Socket);
      end;
    finally
      CloseSocket(sock);
    end;
    if not IsIPv4Mapped(Conn.ClientAddress) then
      ServerError := 'Expected IPv4 mapped Address, got ' + Conn.ClientAddress.Address;
    if Received <> HelloStr then
      ServerError := 'Unexpected response: ' + Received;
  except on E: Exception do
    ServerError := 'Exception: ' + E.Message;
  end;
end;

procedure CloseTestServer;
var
  sock: TFPSocket;
  Conn: TFPSocketConnection;
begin
  ServerError := '';
  try
    sock := TCPSocket(stIPv4);
    try
      Bind(sock, '0.0.0.0', 1337);
      Listen(sock, 0);
      Conn := AcceptConnection(sock);
      CloseSocket(Conn.Socket);
    finally
      CloseSocket(sock);
    end;
  except on E: Exception do
    ServerError := 'Exception: ' + E.Message;
  end;
end;

procedure CloseTestClient;
var
  sock: TFPSocket;
begin
  ClientError := '';
  try
    sock := TCPSocket(stIPv4);
    try
      Connect(sock, '127.0.0.1', 1337);
      Sleep(100);
      if not StreamClosed(sock) then
      begin
        ClientError := 'Should detect closed stream by server';
        Exit;
      end;
      try
        ReceiveStr(sock);
        ClientError := 'Should detect closed stream by server';
      except on E: EConnectionClosedException do
      end;
    finally
      CloseSocket(sock);
    end;
  except on E: Exception do
    ClientError := 'Exception: ' + E.Message;
  end;
end;

procedure DataAvailableTestClient;
var
  sock: TFPSocket;
begin
  ClientError := '';
  try
    sock := TCPSocket(stIPv4);
    try
      Connect(sock, '127.0.0.1', 1337);
      SendStr(sock, HelloStr);
      Sleep(600);
      if not DataAvailable(sock) then
      begin
        ClientError := 'Should have data from the server pending';
        Exit;
      end;
      if BytesAvailable(sock) <> Length(ReplyStr) then
        ClientError := 'Unexpected data length';
    finally
      CloseSocket(sock);
    end;
  except on E: Exception do
    ClientError := 'Exception: ' + E.Message;
  end;
end;

procedure ReceiveArrayTestServer;
var
  sock: TFPSocket;
  Conn: TFPSocketConnection;
  Received: Array of Integer; // Hello Server = 12 chars = divisible by 4
  i:Integer;
begin
  ServerError := '';
  try
    sock := TCPSocket(stIPv4);
    try
      Bind(sock, '0.0.0.0', 1337);
      Listen(sock, 0);
      Conn := AcceptConnection(sock);
      try
        Received := specialize ReceiveArray<Integer>(Conn.Socket);
        SendStr(Conn.Socket, ReplyStr);
      finally
        CloseSocket(Conn.Socket);
      end;
    finally
      CloseSocket(sock);
    end;
    if Length(Received) * SizeOf(Integer) <> Length(HelloStr) then
    begin
      ServerError := 'Unexpected response length ' + Length(Received).ToString;
      Exit;
    end;
    for i:=0 to Length(HelloStr) -1 do
      if PChar(@Received[0])[i]<>HelloStr[i+1] then
      begin
        ServerError := 'Unexpected response Char ' + PChar(@Received[0])[i] + '@' + i.ToString;;
        Exit;
      end;
  except on E: Exception do
    ServerError := 'Exception: ' + E.Message;
  end;
end;

procedure ReceiveArrayTestClient;
var
  sock: TFPSocket;
  Received: Array of Char;
  i:Integer;
begin
  ClientError := '';
  try
    sock := TCPSocket(stIPv4);
    try
      Connect(sock, '127.0.0.1', 1337);
      SendStr(sock, HelloStr);
      Received := specialize ReceiveArray<Char>(sock);
    finally
      CloseSocket(sock);
    end;
    if Length(Received) <> Length(ReplyStr) then
    begin
      ClientError := 'Unexpected response length ' + Length(Received).ToString;
      Exit;
    end;
    for i:=0 to Length(Received) -1 do
      if Received[i]<>ReplyStr[i+1] then
      begin
        ClientError := 'Unexpected response Char ' + Received[i] + '@' + i.ToString;
        Exit;
      end;
  except on E: Exception do
    ClientError := 'Exception: ' + E.Message;
  end;
end;

procedure ChunkTestServer;
type
  TChunkString = String[16];
var
  sock: TFPSocket;
  Conn: TFPSocketConnection;
  Received, toSend: TChunkString;
begin
  ServerError := '';
  try
    sock := TCPSocket(stIPv4);
    try
      Bind(sock, '0.0.0.0', 1337);
      Listen(sock, 0);
      Conn := AcceptConnection(sock);
      try
        Received := specialize Receive<TChunkString>(Conn.Socket);
        ToSend := ReplyStr;
        // Send in two halves with time delay (client must block until full chunk)
        Send(Conn.Socket, @toSend, SizeOf(toSend) div 2);
        Sleep(400);
        Send(Conn.Socket, PByte(@toSend) + SizeOf(toSend) div 2, SizeOf(toSend) - SizeOf(toSend) div 2);
      finally
        CloseSocket(Conn.Socket);
      end;
    finally
      CloseSocket(sock);
    end;
    if Received <> HelloStr then
      ServerError := 'Unexpected response: ' + Received;
  except on E: Exception do
    ServerError := 'Exception: ' + E.Message;
  end;
end;

procedure ChunkTestClient;
type
  TChunkString = String[16];
var
  sock: TFPSocket;
  Received: TChunkString;
begin
  ClientError := '';
  try
    sock := TCPSocket(stIPv4);
    try
      Connect(sock, '127.0.0.1', 1337);
      specialize Send<TChunkString>(sock, HelloStr);
      Received := specialize Receive<TChunkString>(sock);
      if Received <> ReplyStr then
        ClientError := 'Unexpected response: ' + Received;
    finally
      CloseSocket(sock);
    end;
  except on E: Exception do
    ClientError := 'Exception: ' + E.Message;
  end;
end;

procedure TestNonBlockingServer;
var
  sock: TFPSocket;
  Conn: TFPSocketConnection;
  Received: String;
begin
  ServerError := '';
  try
    sock := TCPSocket(stIPv4);
    try
      SetNonBlocking(sock, True);
      Bind(sock, '0.0.0.0', 1337);
      Listen(sock, 0);
      while not AcceptNonBlocking(sock).Unpack(Conn) do
        Sleep(100);
      try
        SetNonBlocking(Conn.Socket, True);
        repeat
          Received := ReceiveStr(Conn.Socket);
          Sleep(100);
        until Received<>'';
        Sleep(500);
        SendStr(Conn.Socket, ReplyStr);
      finally
        CloseSocket(Conn.Socket);
      end;
    finally
      CloseSocket(sock);
    end;
    if Received <> HelloStr then
      ServerError := 'Unexpected response: ' + Received;
  except on E: Exception do
    ServerError := 'Exception: ' + E.Message;
  end;
end;

procedure TestNonBlockingClient;
var
  sock: TFPSocket;
  Received: Array of Char;
  State:TConnectionState;
  i:Integer;
begin
  ClientError := '';
  try
    sock := TCPSocket(stIPv4);
    try
      SetNonBlocking(sock, True);
      Sleep(200);
      State := Connect(sock, '127.0.0.1', 1337);
      while State = csPending do
      begin
        Sleep(100);
        State:=ConnectionState(sock);
      end;
      if State <> csConnected then
      begin
        ClientError := 'Connection not successful';
        Exit;
      end;
      Sleep(200);
      SendStr(sock, HelloStr);
      repeat
        Received := specialize ReceiveArray<Char>(sock, 16);
        Sleep(100);
      until Received<>nil;
    finally
      CloseSocket(sock);
    end;
    for i:=0 to Length(Received) -1 do
      if Received[i]<>ReplyStr[i+1] then
      begin
        ClientError := 'Unexpected response Char ' + Received[i] + '@' + i.ToString;;
        Exit;
      end;
  except on E: Exception do
    ClientError := 'Exception: ' + E.Message;
  end;
end;

{$IfDef Unix}
// Different behavior between winsock and berkeley sockets
// Seems like winsock does not provide refused when the server closes while pending
procedure TestRefusedServer;
var
  sock: TFPSocket;
begin
  ServerError := '';
  try
    sock := TCPSocket(stIPv4);
    try
      Bind(sock, '0.0.0.0', 1337);
      Listen(sock, 1);
    finally
      CloseSocket(sock);
    end;
  except on E: Exception do
    ServerError := 'Exception: ' + E.Message;
  end;
end;

procedure TestRefusedClient;
var
  sock: TFPSocket;
  State: TConnectionState;
begin
  ClientError := '';
  try
    sock := TCPSocket(stIPv4);
    try
      SetNonBlocking(sock, True);
      Connect(sock, '127.0.0.1', 1337);
      Sleep(200);
      State:=ConnectionState(sock);
      if State <> csRefused then
      begin
        ClientError := 'Connection should be refused';
        Exit;
      end;
    finally
      CloseSocket(sock);
    end;
  except on E: Exception do
    ClientError := 'Exception: ' + E.Message;
  end;
end;
{$EndIf}

procedure TestFragmentationServer;
var
  sock: TFPSocket;
  Conn: TFPSocketConnection;
begin
  ServerError := '';
  try
    sock := TCPSocket(stIPv4);
    try
      Bind(sock, '0.0.0.0', 1337);
      Listen(sock, 0);
      Conn := AcceptConnection(sock);
      try
        SetNonBlocking(Conn.Socket, True);
        try
          while not specialize ReceiveNonBlocking<LongInt>(Conn.Socket) do
            Sleep(50);
          ServerError := 'Should have thrown fragmentation exception';
        except on E: EFragmentedData do
          if Length(e.Fragment) <> SizeOf(Word) then
            ServerError := 'Unexpected Fragment Size';
        on E: Exception do
          raise E;
        end;
      finally
        CloseSocket(Conn.Socket);
      end;
    finally
      CloseSocket(sock);
    end;
  except on E: Exception do
    ServerError := 'Exception: ' + E.Message;
  end;
end;

procedure TestFragmentationClient;
var
  sock: TFPSocket;
begin
  ClientError := '';
  try
    sock := TCPSocket(stIPv4);
    try
      Connect(sock, '127.0.0.1', 1337);
      specialize Send<Word>(sock, 42);
      Sleep(100);
    finally
      CloseSocket(sock);
    end;
  except on E: Exception do
    ClientError := 'Exception: ' + E.Message;
  end;
end;

procedure TestFragmentedArrayServer;
var
  sock: TFPSocket;
  Conn: TFPSocketConnection;
begin
  ServerError := '';
  try
    sock := TCPSocket(stIPv4);
    try
      Bind(sock, '0.0.0.0', 1337);
      Listen(sock, 0);
      Conn := AcceptConnection(sock);
      try
        SetNonBlocking(Conn.Socket, True);
        try
          while specialize ReceiveArray<LongInt>(Conn.Socket) = nil do
            Sleep(50);
          ServerError := 'Should have thrown fragmentation exception';
        except on E: EFragmentedData do
          if Length(e.Fragment) <> SizeOf(Word) then
            ServerError := 'Unexpected Fragment Size';
        on E: Exception do
          raise E;
        end;
      finally
        CloseSocket(Conn.Socket);
      end;
    finally
      CloseSocket(sock);
    end;
  except on E: Exception do
    ServerError := 'Exception: ' + E.Message;
  end;
end;

procedure TestFragmentedArrayClient;
var
  sock: TFPSocket;
begin
  ClientError := '';
  try
    sock := TCPSocket(stIPv4);
    try
      Connect(sock, '127.0.0.1', 1337);
      specialize SendArray<Word>(sock, [42]);
      Sleep(100);
    finally
      CloseSocket(sock);
    end;
  except on E: Exception do
    ClientError := 'Exception: ' + E.Message;
  end;
end;

{ When trying to read an array, and the array is fragmented, and you give it a
  read size thats larger than whats in the buffer, instead of a fragmented
  exception, a connection closed exception will be raised.
  This is suboptimal/undesired behavior, but arises from the internal calls to
  Receive, which raises an exception on end of stream. This test verifies that,
  arguably faulty behavior, so it may very well be fixed in the future }
procedure TestFragmentedCloseServer;
var
  sock: TFPSocket;
  Conn: TFPSocketConnection;
begin
  ServerError := '';
  try
    sock := TCPSocket(stIPv4);
    try
      Bind(sock, '0.0.0.0', 1337);
      Listen(sock, 0);
      Conn := AcceptConnection(sock);
      try
        try
          Sleep(100);
          specialize ReceiveArray<LongInt>(Conn.Socket, 2);
          ServerError := 'Should have thrown ConnectionClosed Exception';
        except on E: EConnectionClosedException do ;
        on E: Exception do
          raise E;
        end;
      finally
        CloseSocket(Conn.Socket);
      end;
    finally
      CloseSocket(sock);
    end;
  except on E: Exception do
    ServerError := 'Exception: ' + E.Message;
  end;
end;

procedure TestFragmentedCloseClient;
var
  sock: TFPSocket;
begin
  ClientError := '';
  try
    sock := TCPSocket(stIPv4);
    try
      Connect(sock, '127.0.0.1', 1337);
      specialize SendArray<Word>(sock, [42, 43, 44]);
      Sleep(100);
    finally
      CloseSocket(sock);
    end;
  except on E: Exception do
    ClientError := 'Exception: ' + E.Message;
  end;
end;

type
  TTimeoutThread = class(TThread)
  protected
    procedure Execute;override;
  end;

procedure TTimeoutThread.Execute;
var
  i: Integer;
begin
  for i:=1 to 100 do
  begin
    if Terminated then
      Exit;
    Sleep(100);
  end;
  if Terminated then
    Exit;
  WriteLn(' Timeout');
  // FPC Halt does not work with threads... so we just rawkill using libc
  cexit(1);
end;

procedure RunTest(const TestName: String; ASrv, ACli: TProcedure);
var
  Timeout, SrvThread, CliThread: TThread;
begin
  Write('Testing ', TestName, '...');
  SrvThread:=TThread.CreateAnonymousThread(ASrv);
  SrvThread.FreeOnTerminate := False;
  SrvThread.Start;
  CliThread:=TThread.CreateAnonymousThread(ACli);
  CliThread.FreeOnTerminate := False;
  CliThread.Start;
  Timeout:=TTimeoutThread.Create(false);
  SrvThread.WaitFor;
  if not ServerError.IsEmpty then
  begin
    WriteLn(LineEnding, '  Server Error: ', ServerError);
    Halt(1);
  end;
  CliThread.WaitFor;
  if not ClientError.IsEmpty then
  begin
    WriteLn(LineEnding, '  Client Error: ', ClientError);
    Halt(1);
  end;
  Timeout.Terminate;
  Timeout.Free;
  WriteLn(' Success!');
  CliThread.Free;
  SrvThread.Free;
  Sleep(800);
end;

begin
  RunTest('IPv4Test', @IPv4TestServer, @IPv4TestClient);
  RunTest('IPv6Test', @IPv6TestServer, @IPv6TestClient);
  RunTest('DualStackTest', @DualStackTestServer, @IPv4TestClient);
  RunTest('CloseTest', @CloseTestServer, @CloseTestClient);
  RunTest('DataAvailableTest', @IPv4TestServer, @DataAvailableTestClient);
  RunTest('ReceiveArrayTest', @ReceiveArrayTestServer, @ReceiveArrayTestClient);
  RunTest('ChunkTest', @ChunkTestServer, @ChunkTestClient);
  RunTest('NonBlockingTest', @TestNonBlockingServer, @TestNonBlockingClient);
  {$IfDef Unix}
  RunTest('RefusedTest', @TestRefusedServer, @TestRefusedClient);
  {$EndIf}
  RunTest('FragmentationTest', @TestFragmentationServer, @TestFragmentationClient);
  RunTest('FragmentedArrayTest', @TestFragmentedArrayServer, @TestFragmentedArrayClient);
  RunTest('FragmentedCloseTest', @TestFragmentedCloseServer, @TestFragmentedCloseClient);
end.
