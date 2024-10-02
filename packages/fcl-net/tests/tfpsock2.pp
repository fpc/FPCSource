program udptest;

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
  Received:TReceiveFromStringMessage;
begin
  ServerError := '';
  try
    sock := UDPSocket(stIPv4);
    try
      Bind(sock, '0.0.0.0', 1337);
      Received := ReceiveStrFrom(sock);
      sleep(500);
      SendStrTo(sock, Received.FromAddr, Received.FromPort, ReplyStr);
    finally
      CloseSocket(sock);
    end;
    if Received.Data <> HelloStr then
      ServerError := 'Unexpected response: ' + Received.Data;
  except on E: Exception do
    ServerError := 'Exception: ' + E.Message;
  end;
end;

procedure IPv4TestClient;
var
  sock: TFPSocket;
  Received: TReceiveFromStringMessage;
begin
  ClientError := '';
  try
    sock := UDPSocket(stIPv4);
    try
      Sleep(50);
      SendStrTo(sock, '127.0.0.1', 1337, HelloStr);
      Sleep(50);
      Received := ReceiveStrFrom(sock, 16);
    finally
      CloseSocket(sock);
    end;
    if Received.Data <> ReplyStr then
      ClientError := 'Unexpected response: ' + Received.Data;
  except on E: Exception do
    ClientError := 'Exception: ' + E.Message;
  end;
end;

procedure IPv6TestServer;
var
  sock: TFPSocket;
  Received:TReceiveFromStringMessage;
begin
  ServerError := '';
  try
    sock := UDPSocket(stIPv6);
    try
      Bind(sock, '::0', 1337);
      Received := ReceiveStrFrom(sock);
      SendStrTo(sock, Received.FromAddr, Received.FromPort, ReplyStr);
    finally
      CloseSocket(sock);
    end;
    if Received.Data <> HelloStr then
      ServerError := 'Unexpected response: ' + Received.Data;
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
    sock := UDPSocket(stIPv6);
    try
      Sleep(50);
      SendStrTo(sock, '::1', 1337, HelloStr);
      Sleep(50);
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
  Received:TReceiveFromStringMessage;
begin
  ServerError := '';
  try
    sock := UDPSocket(stIPDualStack);
    try
      Bind(sock, '::0', 1337);
      Received := ReceiveStrFrom(sock);
      SendStrTo(sock, Received.FromAddr, Received.FromPort, ReplyStr);
    finally
      CloseSocket(sock);
    end;
    if not IsIPv4Mapped(Received.FromAddr) then
      ServerError := 'Expected IPv4 mapped Address, got ' + Received.FromAddr.Address;
    if Received.Data <> HelloStr then
      ServerError := 'Unexpected response: ' + Received.Data;
  except on E: Exception do
    ServerError := 'Exception: ' + E.Message;
  end;
end;

procedure DataAvailableTestClient;
var
  sock: TFPSocket;
begin
  ClientError := '';
  try
    sock := UDPSocket(stIPv4);
    try
      Sleep(50);
      SendStrTo(sock, '127.0.0.1', 1337, HelloStr);
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
  Received: specialize TReceiveFromMessage<specialize TArray<Integer>>; // Hello Server = 12 chars = divisible by 4
  i:Integer;
begin
  ServerError := '';
  try
    sock := UDPSocket(stIPv4);
    try
      Bind(sock, '0.0.0.0', 1337);
      Received := specialize ReceiveArrayFrom<Integer>(sock);
      SendStrTo(sock, Received.FromAddr, Received.FromPort, ReplyStr);
    finally
      CloseSocket(sock);
    end;
    if Length(Received.Data) * SizeOf(Integer) <> Length(HelloStr) then
    begin
      ServerError := 'Unexpected response length ' + Length(Received.Data).ToString;
      Exit;
    end;
    for i:=0 to Length(HelloStr) -1 do
      if PChar(@Received.Data[0])[i]<>HelloStr[i+1] then
      begin
        ServerError := 'Unexpected response Char ' + PChar(@Received.Data[0])[i] + '@' + i.ToString;;
        Exit;
      end;
  except on E: Exception do
    ServerError := 'Exception: ' + E.Message;
  end;
end;

procedure ReceiveArrayTestClient;
var
  sock: TFPSocket;
  Received: specialize TReceiveFromMessage<specialize TArray<Char>>;
  i:Integer;
begin
  ClientError := '';
  try
    sock := UDPSocket(stIPv4);
    try
      Sleep(50);
      SendStrTo(sock, '127.0.0.1', 1337, HelloStr);
      Sleep(50);
      Received := specialize ReceiveArrayFrom<Char>(sock);
    finally
      CloseSocket(sock);
    end;
    if Length(Received.Data) <> Length(ReplyStr) then
    begin
      ClientError := 'Unexpected response length ' + Length(Received.Data).ToString;
      Exit;
    end;
    for i:=0 to Length(Received.Data) -1 do
      if Received.Data[i]<>ReplyStr[i+1] then
      begin
        ClientError := 'Unexpected response Char ' + Received.Data[i] + '@' + i.ToString;
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
  Received: specialize TReceiveFromMessage<TChunkString>;
begin
  ServerError := '';
  try
    sock := UDPSocket(stIPv4);
    try
      Bind(sock, '0.0.0.0', 1337);
      Received := specialize ReceiveFrom<TChunkString>(sock);
      specialize SendTo<TChunkString>(sock, Received.FromAddr, Received.FromPort, ReplyStr);
    finally
      CloseSocket(sock);
    end;
    if Received.Data <> HelloStr then
      ServerError := 'Unexpected response: ' + Received.Data;
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
    sock := UDPSocket(stIPv4);
    try
      Sleep(50);
      specialize SendTo<TChunkString>(sock, '127.0.0.1', 1337, HelloStr);
      Sleep(50);
      Received := specialize ReceiveFrom<TChunkString>(sock).Data;
    finally
      CloseSocket(sock);
    end; 
    if Received <> ReplyStr then
      ClientError := 'Unexpected response: ' + Received;
  except on E: Exception do
    ClientError := 'Exception: ' + E.Message;
  end;
end;

procedure UDPFragmentationTestServer;
type
  TChunkString = String[16];
var
  sock: TFPSocket;
begin
  ServerError := '';
  try
    sock := UDPSocket(stIPv4);
    try
      Bind(sock, '0.0.0.0', 1337);
      try
        specialize ReceiveFrom<TChunkString>(sock);
        ServerError := 'Should have thrown fragmentation error';
      except on E: EFragmentedData do
        if Length(e.Fragment) <> SizeOf(TChunkString) div 2 then
          ServerError := 'Unexpected Fragment Size';
      on E: Exception do
        raise E;
      end;
    finally
      CloseSocket(sock);
    end;
  except on E: Exception do
    ServerError := 'Exception: ' + E.Message;
  end;
end;

procedure UDPFragmentationTestClient;
type
  TChunkString = String[16];
var
  sock: TFPSocket;
  toSend: TChunkString;
begin
  ClientError := '';
  try
    sock := UDPSocket(stIPv4);
    try
      Sleep(50);
      toSend := HelloStr;
      // Send fragmented in two chunks -> UDP Fragmentation error
      SendTo(sock, '127.0.0.1', 1337, @toSend, SizeOf(toSend) div 2);
      Sleep(400);
      SendTo(sock, '127.0.0.1', 1337, PByte(@toSend) + SizeOf(toSend) div 2, SizeOf(toSend) - SizeOf(toSend) div 2);
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
  Received: TReceiveFromStringMessage;
begin
  ServerError := '';
  try
    sock := UDPSocket(stIPv4);
    try
      SetNonBlocking(sock, True);
      Bind(sock, '0.0.0.0', 1337);
      while not ReceiveStrFromNonBlocking(sock).Unpack(Received) do
        Sleep(100);
      Sleep(500);
      SendStrTo(sock, Received.FromAddr, Received.FromPort, ReplyStr);
    finally
      CloseSocket(sock);
    end;
    if Received.Data <> HelloStr then
      ServerError := 'Unexpected response: ' + Received.Data;
  except on E: Exception do
    ServerError := 'Exception: ' + E.Message;
  end;
end;

procedure TestNonBlockingClient;
var
  sock: TFPSocket;
  Received: specialize TReceiveFromMessage<specialize TArray<Char>>;
  i:Integer;
begin
  ClientError := '';
  try
    sock := UDPSocket(stIPv4);
    try
      SetNonBlocking(sock, True);
      Sleep(200);
      SendStrTo(sock, '127.0.0.1', 1337, HelloStr);
      while not specialize ReceiveArrayFromNonBlocking<Char>(sock, 16).unpack(Received) do
        Sleep(100);
    finally
      CloseSocket(sock);
    end;
    for i:=0 to Length(Received.Data) -1 do
      if Received.Data[i]<>ReplyStr[i+1] then
      begin
        ClientError := 'Unexpected response Char ' + Received.Data[i] + '@' + i.ToString;;
        Exit;
      end;
  except on E: Exception do
    ClientError := 'Exception: ' + E.Message;
  end;
end;

procedure TestFragmentationServer;
var
  sock: TFPSocket;
begin
  ServerError := '';
  try
    sock := UDPSocket(stIPv4);
    try
      Bind(sock, '0.0.0.0', 1337);
      SetNonBlocking(sock, True);
      try
        while not specialize ReceiveFromNonBlocking<LongInt>(sock) do
          Sleep(50);
        ServerError := 'Should have thrown fragmentation exception';
      except on E: EFragmentedData do
        if Length(e.Fragment) <> SizeOf(Word) then
          ServerError := 'Unexpected Fragment Size';
      on E: Exception do
        raise E;
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
    sock := UDPSocket(stIPv4);
    try
      Sleep(50);
      specialize SendTo<Word>(sock, '127.0.0.1', 1337, 42);
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
begin
  ServerError := '';
  try
    sock := UDPSocket(stIPv4);
    try
      Bind(sock, '0.0.0.0', 1337);
      SetNonBlocking(sock, True);
      try
        while specialize ReceiveArray<LongInt>(sock) = nil do
          Sleep(50);
        ServerError := 'Should have thrown fragmentation exception';
      except on E: EFragmentedData do
        if Length(e.Fragment) <> SizeOf(LongInt) + SizeOf(Word) then
          ServerError := 'Unexpected Fragment Size';
      on E: Exception do
        raise E;
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
    sock := UDPSocket(stIPv4);
    try
      Sleep(100);
      specialize SendArrayTo<Word>(sock, '127.0.0.1', 1337, [42, 43, 44]);
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
  Sleep(500);
end;

begin
  RunTest('IPv4Test', @IPv4TestServer, @IPv4TestClient);
  RunTest('IPv6Test', @IPv6TestServer, @IPv6TestClient);
  RunTest('DualStackTest', @DualStackTestServer, @IPv4TestClient);
  RunTest('DataAvailableTest', @IPv4TestServer, @DataAvailableTestClient);
  RunTest('ReceiveArrayTest', @ReceiveArrayTestServer, @ReceiveArrayTestClient);
  RunTest('ChunkTest', @ChunkTestServer, @ChunkTestClient);
  RunTest('UDPFragmentationTest', @UDPFragmentationTestServer, @UDPFragmentationTestClient);
  RunTest('NonBlockingTest', @TestNonBlockingServer, @TestNonBlockingClient);
  RunTest('FragmentationTest', @TestFragmentationServer, @TestFragmentationClient);
  RunTest('FragmentedArrayTest', @TestFragmentedArrayServer, @TestFragmentedArrayClient);
end.

