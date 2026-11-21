{
  This file is part of the Free Component Library.
  Tests for FpQuic.Endpoint over UDP on the loopback interface.
  See the file COPYING.FPC, included in this distribution, for details about the copyright.
}
unit utquicendpoint;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpcunit, testregistry, fpsockets, FpQuic.Consts, FpQuic.Buffers, FpQuic.Packets,
  FpQuic.TransportParams, FpQuic.OpenSSL, FpQuic.Connection, FpQuic.Udp, FpQuic.Endpoint, utquichelpers,
  utquicnetwork;

type
  // Writes a buffer to a stream in a background thread.
  TWriterThread = class(TThread)
  private
    FStream: TQuicStreamAdapter;
    FData: TBytes;
  protected
    procedure Execute; override;
  public
    Error: String;
    constructor Create(aStream: TQuicStreamAdapter; const aData: TBytes);
    destructor Destroy; override;
  end;

  // Server whose worker thread raises an exception when FailNext is set.
  TFailingServer = class(TQuicServer)
  protected
    procedure Poll(aWait: Integer); override;
  public
    FailNext: Boolean;
  end;

  TTestQuicEndpoint = class(TTestCase)
  private
    FServer: TQuicServer;
    FClient: TQuicClient;
    function ServerTLS: TQuicTLSConfig;
    function ClientTLS: TQuicTLSConfig;
    procedure StartServer;
    function ConnectClient(const aTLS: TQuicTLSConfig): TQuicEndpointConnection;
    function ReadString(aStream: TStream): String;
    procedure TicketStored(Sender: TObject; const aKey: String; var aTicket: TBytes);
    procedure TicketLoad(Sender: TObject; const aKey: String; var aTicket: TBytes);
    procedure LargeTransfer(Size: Integer);
    function StartRawClient: TTestNetwork;
    procedure SendDatagrams(aSocket: TFPSocket; const aDatagrams: array of TBytes);
    function ReceiveDatagram(aSocket: TFPSocket): TBytes;
    function WaitConnectionCount(aCount: Integer): Integer;
  protected
    FStoredKey: String;
    FStoreCount: Integer;
    FLoadTicket: TBytes;
    procedure TearDown; override;
  published
    procedure TestSessionCache;
    procedure TestEarlyData;
    procedure TestEcho;
    procedure TestLargeTransfer;
    procedure TestLargeTransferWithoutGso;
    procedure TestLargeTransferSmallBatches;
    procedure TestServerOpensStream;
    procedure TestReset;
    procedure TestResetAfterFinish;
    procedure TestCloseAndTimeouts;
    procedure TestHandshakeFailure;
    procedure TestConnectionRefused;
    procedure TestUnreachableAddress;
    procedure TestVersionNegotiation;
    procedure TestIgnoredDatagrams;
    procedure TestStreamLimit;
    procedure TestIPv6;
    procedure TestDualStack;
    procedure TestWriteTimeout;
    procedure TestRetry;
    procedure TestRetryTokens;
    procedure TestConnectionLimits;
    procedure TestWorkerException;
  end;

implementation

procedure TFailingServer.Poll(aWait: Integer);

begin
  if FailNext then
    begin
    FailNext:=False;
    raise Exception.Create('Worker failure');
    end;
  inherited Poll(aWait);
end;


constructor TWriterThread.Create(aStream: TQuicStreamAdapter; const aData: TBytes);

begin
  FStream:=aStream;
  FStream.Timeout:=120000;
  FData:=aData;
  inherited Create(False);
end;


destructor TWriterThread.Destroy;

begin
  FStream.Free;
  inherited Destroy;
end;


procedure TWriterThread.Execute;

begin
  try
    FStream.WriteBuffer(FData[0],Length(FData) div 2);
    FStream.WriteBuffer(FData[Length(FData) div 2],Length(FData)-Length(FData) div 2);
    FStream.Finish;
  except
    on E: Exception do
      Error:=E.Message;
  end;
end;


function TTestQuicEndpoint.ServerTLS: TQuicTLSConfig;

begin
  NeedOpenSSL;
  Result:=Default(TQuicTLSConfig);
  Result.Alpn:=['echo'];
  Result.CertificateFile:=TestDataDir+'server.crt';
  Result.PrivateKeyFile:=TestDataDir+'server.key';
end;


function TTestQuicEndpoint.ClientTLS: TQuicTLSConfig;

begin
  NeedOpenSSL;
  Result:=Default(TQuicTLSConfig);
  Result.Alpn:=['echo'];
  Result.ServerName:='localhost';
  Result.VerifyPeer:=True;
  Result.CAFile:=TestDataDir+'server.crt';
end;


procedure TTestQuicEndpoint.StartServer;

begin
  FServer:=TQuicServer.Create(ServerTLS,QuicDefaultEndpointParams);
  FServer.Listen('127.0.0.1',0);
end;


function TTestQuicEndpoint.ConnectClient(const aTLS: TQuicTLSConfig): TQuicEndpointConnection;

begin
  FClient:=TQuicClient.Create(aTLS,QuicDefaultEndpointParams);
  Result:=FClient.Connect('127.0.0.1',FServer.Port,5000);
end;


function TTestQuicEndpoint.ReadString(aStream: TStream): String;

var
  lBuf: array[0..1023] of Byte;
  lCount: Integer;

begin
  Result:='';
  repeat
    lCount:=aStream.Read(lBuf[0],SizeOf(lBuf));
    if lCount>0 then
      Result:=Result+TEncoding.UTF8.GetString(QuicBytesOf(@lBuf[0],lCount));
  until lCount=0;
end;


function TTestQuicEndpoint.StartRawClient: TTestNetwork;

begin
  Result:=TTestNetwork.Create;
  Result.ClientTLS:=ClientTLS;
  Result.Start;
  Result.Client.Collect(Result.Now);
end;


procedure TTestQuicEndpoint.SendDatagrams(aSocket: TFPSocket; const aDatagrams: array of TBytes);

var
  lData: TBytes;

begin
  for lData in aDatagrams do
    SendTo(aSocket,'127.0.0.1',FServer.Port,PByte(lData),Length(lData));
end;


function TTestQuicEndpoint.ReceiveDatagram(aSocket: TFPSocket): TBytes;

var
  lResult: TReceiveFromResult;
  I: Integer;

begin
  Result:=nil;
  SetLength(Result,2000);
  I:=0;
  repeat
    DataAvailable(aSocket,100);
    lResult:=ReceiveFrom(aSocket,PByte(Result),Length(Result));
    Inc(I);
  until (lResult.DataSize>0) or (I>50);
  if lResult.DataSize>0 then
    SetLength(Result,lResult.DataSize)
  else
    Result:=nil;
end;


function TTestQuicEndpoint.WaitConnectionCount(aCount: Integer): Integer;

var
  I: Integer;

begin
  I:=0;
  repeat
    Result:=FServer.ConnectionCount;
    if Result=aCount then
      Exit;
    Sleep(20);
    Inc(I);
  until I>25;
end;


procedure TTestQuicEndpoint.TearDown;

begin
  FreeAndNil(FClient);
  FreeAndNil(FServer);
end;


procedure TTestQuicEndpoint.TicketStored(Sender: TObject; const aKey: String; var aTicket: TBytes);

begin
  FStoredKey:=aKey;
  Inc(FStoreCount);
end;


procedure TTestQuicEndpoint.TicketLoad(Sender: TObject; const aKey: String; var aTicket: TBytes);

begin
  aTicket:=FLoadTicket;
end;


procedure TTestQuicEndpoint.TestSessionCache;

var
  lCache: TQuicSessionCache;

begin
  lCache:=TQuicSessionCache.Create;
  try
    AssertEquals('Empty cache',0,Length(lCache.Load('a')));
    lCache.OnStore:=@TicketStored;
    lCache.Store('a',QuicHexToBytes('01'));
    lCache.Store('b',QuicHexToBytes('02'));
    lCache.Store('a',QuicHexToBytes('03'));
    AssertEquals('Store events',3,FStoreCount);
    AssertEquals('Stored key','a',FStoredKey);
    AssertEquals('Latest ticket kept','03',QuicBytesToHex(lCache.Load('a')));
    AssertEquals('Other key','02',QuicBytesToHex(lCache.Load('b')));
    FLoadTicket:=QuicHexToBytes('04');
    lCache.OnLoad:=@TicketLoad;
    AssertEquals('Loaded from elsewhere','04',QuicBytesToHex(lCache.Load('c')));
  finally
    lCache.Free;
  end;
end;


procedure TTestQuicEndpoint.TestEarlyData;

var
  lServerTLS, lClientTLS: TQuicTLSConfig;
  lCache: TQuicSessionCache;
  lConnection, lServerConnection: TQuicEndpointConnection;
  lStream, lServerStream: TQuicStreamAdapter;
  lText: String;
  lEnd: QWord;

begin
  lServerTLS:=ServerTLS;
  lServerTLS.EnableEarlyData:=True;
  FServer:=TQuicServer.Create(lServerTLS,QuicDefaultEndpointParams);
  FServer.Listen('127.0.0.1',0);
  lClientTLS:=ClientTLS;
  lClientTLS.EnableEarlyData:=True;
  lCache:=TQuicSessionCache.Create;
  try
    lCache.OnStore:=@TicketStored;
    FClient:=TQuicClient.Create(lClientTLS,QuicDefaultEndpointParams);
    FClient.SessionCache:=lCache;
    AssertNotNull('First connection',FClient.Connect('127.0.0.1',FServer.Port,5000));
    FServer.Accept(5000).Free;
    lEnd:=GetTickCount64+5000;
    while (FStoreCount=0) and (GetTickCount64<lEnd) do
      Sleep(10);
    AssertTrue('Ticket stored',FStoreCount>0);
    AssertEquals('Ticket key','localhost|127.0.0.1|'+IntToStr(FServer.Port),FStoredKey);
    FreeAndNil(FClient);
    FClient:=TQuicClient.Create(lClientTLS,QuicDefaultEndpointParams);
    FClient.SessionCache:=lCache;
    lConnection:=FClient.Connect('127.0.0.1',FServer.Port,5000,True);
    AssertNotNull('Early connection',lConnection);
    lStream:=lConnection.OpenStream;
    AssertNotNull('Stream opened before the handshake',lStream);
    try
      lText:='early hello';
      lStream.WriteBuffer(lText[1],Length(lText));
      lStream.Finish;
      lServerConnection:=FServer.Accept(5000);
      AssertNotNull('Accepted',lServerConnection);
      try
        lServerStream:=lServerConnection.AcceptStream(5000);
        AssertNotNull('Server stream',lServerStream);
        try
          AssertTrue('Stream is early data',lServerStream.IsEarlyData);
          AssertEquals('Server reads',lText,ReadString(lServerStream));
          lServerStream.WriteBuffer(lText[1],Length(lText));
          lServerStream.Finish;
        finally
          lServerStream.Free;
        end;
        AssertEquals('Client reads the answer',lText,ReadString(lStream));
        AssertTrue('Handshake',lConnection.WaitHandshake(5000));
        AssertTrue('Client sees 0-RTT accepted',lConnection.EarlyDataAccepted);
        AssertFalse('Client stream is not early',lStream.IsEarlyData);
      finally
        lServerConnection.Free;
      end;
    finally
      lStream.Free;
    end;
    FreeAndNil(FClient);
  finally
    lCache.Free;
  end;
end;


procedure TTestQuicEndpoint.TestEcho;

var
  lConnection, lServerConnection: TQuicEndpointConnection;
  lStream, lServerStream: TQuicStreamAdapter;
  lText: String;

begin
  StartServer;
  lConnection:=ConnectClient(ClientTLS);
  AssertNotNull('Connected',lConnection);
  AssertEquals('ALPN','echo',lConnection.Alpn);
  AssertEquals('Server address','127.0.0.1',String(lConnection.Address.Address));
  AssertEquals('Server port',FServer.Port,lConnection.Port);
  lServerConnection:=FServer.Accept(5000);
  AssertNotNull('Accepted',lServerConnection);
  try
    lStream:=lConnection.OpenStream;
    try
      lText:='hello over udp';
      lStream.WriteBuffer(lText[1],Length(lText));
      lStream.Finish;
      lServerStream:=lServerConnection.AcceptStream(5000);
      AssertNotNull('Server stream',lServerStream);
      try
        AssertEquals('Server reads',lText,ReadString(lServerStream));
        lServerStream.WriteBuffer(lText[1],Length(lText));
        lServerStream.Finish;
      finally
        lServerStream.Free;
      end;
      AssertEquals('Client reads the echo',lText,ReadString(lStream));
      AssertEquals('Stream id',0,lStream.StreamId);
    finally
      lStream.Free;
    end;
  finally
    lServerConnection.Free;
  end;
end;


procedure TTestQuicEndpoint.TestLargeTransfer;

begin
  LargeTransfer(1536*1024);
end;


procedure TTestQuicEndpoint.TestLargeTransferWithoutGso;

begin
  QuicGsoEnabled:=False;
  try
    LargeTransfer(512*1024);
  finally
    QuicGsoEnabled:=True;
  end;
end;


procedure TTestQuicEndpoint.TestLargeTransferSmallBatches;

begin
  QuicMaxSegments:=2;
  try
    LargeTransfer(512*1024);
  finally
    QuicMaxSegments:=64;
  end;
end;


procedure TTestQuicEndpoint.LargeTransfer(Size: Integer);

var
  lConnection, lServerConnection: TQuicEndpointConnection;
  lServerStream: TQuicStreamAdapter;
  lWriter: TWriterThread;
  lData, lReceived: TBytes;
  lBuf: array[0..65535] of Byte;
  lCount, lTotal: Integer;
  I: Integer;

begin
  StartServer;
  lConnection:=ConnectClient(ClientTLS);
  lServerConnection:=FServer.Accept(5000);
  try
    lData:=nil;
    SetLength(lData,Size);
    for I:=0 to Size-1 do
      lData[I]:=(I*7) and $FF;
    lWriter:=TWriterThread.Create(lConnection.OpenStream,lData);
    try
      lServerStream:=lServerConnection.AcceptStream(60000);
      lServerStream.Timeout:=120000;
      try
        lReceived:=nil;
        SetLength(lReceived,Size);
        lTotal:=0;
        repeat
          lCount:=lServerStream.Read(lBuf[0],SizeOf(lBuf));
          Move(lBuf[0],lReceived[lTotal],lCount);
          Inc(lTotal,lCount);
        until lCount=0;
        AssertEquals('Size',Size,lTotal);
        AssertTrue('Content',QuicBytesEqual(lData,lReceived));
      finally
        lServerStream.Free;
      end;
      lWriter.WaitFor;
      AssertEquals('Writer error','',lWriter.Error);
    finally
      lWriter.Free;
    end;
  finally
    lServerConnection.Free;
  end;
end;


procedure TTestQuicEndpoint.TestServerOpensStream;

var
  lConnection, lServerConnection: TQuicEndpointConnection;
  lStream, lClientStream: TQuicStreamAdapter;
  lText: String;

begin
  StartServer;
  lConnection:=ConnectClient(ClientTLS);
  lServerConnection:=FServer.Accept(5000);
  try
    lStream:=lServerConnection.OpenStream(False);
    lText:='push';
    lStream.WriteBuffer(lText[1],4);
    lStream.Finish;
    lClientStream:=lConnection.AcceptStream(5000);
    AssertNotNull('Client accepts',lClientStream);
    AssertEquals('Pushed data','push',ReadString(lClientStream));
    lClientStream.Free;
    lStream.Free;
  finally
    lServerConnection.Free;
  end;
end;


procedure TTestQuicEndpoint.TestResetAfterFinish;

var
  lConnection, lServerConnection: TQuicEndpointConnection;
  lStream, lServerStream: TQuicStreamAdapter;
  lBuf: Byte;

begin
  StartServer;
  lConnection:=ConnectClient(ClientTLS);
  lServerConnection:=FServer.Accept(5000);
  lStream:=lConnection.OpenStream;
  try
    lBuf:=Ord('x');
    lStream.WriteBuffer(lBuf,1);
    lStream.Finish;
    lServerStream:=lServerConnection.AcceptStream(5000);
    try
      lServerStream.ReadBuffer(lBuf,1);
      Sleep(100);
      lServerStream.Reset(7);
    finally
      lServerStream.Free;
    end;
    try
      lStream.Read(lBuf,1);
      Fail('Read on a reset stream must fail when the own side is finished');
    except
      on E: EQuicStreamError do
        AssertTrue('Error code in message',Pos(' 7',E.Message)>0);
    end;
  finally
    lStream.Free;
    lServerConnection.Free;
  end;
end;


procedure TTestQuicEndpoint.TestReset;

var
  lConnection, lServerConnection: TQuicEndpointConnection;
  lStream, lServerStream: TQuicStreamAdapter;
  lBuf: Byte;

begin
  StartServer;
  lConnection:=ConnectClient(ClientTLS);
  lServerConnection:=FServer.Accept(5000);
  lStream:=lConnection.OpenStream;
  try
    lBuf:=Ord('x');
    lStream.WriteBuffer(lBuf,1);
    lServerStream:=lServerConnection.AcceptStream(5000);
    lServerStream.ReadBuffer(lBuf,1);
    lStream.Reset(42);
    try
      lServerStream.Read(lBuf,1);
      Fail('Read on a reset stream must fail');
    except
      on E: EQuicStreamError do
        AssertTrue('Error code in message',Pos('42',E.Message)>0);
    end;
    try
      lStream.Write(lBuf,1);
      Fail('Write on a reset stream must fail');
    except
      on E: EQuicStreamError do
        AssertTrue('Message',E.Message<>'');
    end;
    try
      lStream.Seek(0,soBeginning);
      Fail('Seek must fail');
    except
      on E: EStreamError do
        AssertTrue('Message',E.Message<>'');
    end;
    lServerStream.Free;
  finally
    lStream.Free;
    lServerConnection.Free;
  end;
end;


procedure TTestQuicEndpoint.TestCloseAndTimeouts;

var
  lConnection, lServerConnection: TQuicEndpointConnection;
  lStream: TQuicStreamAdapter;
  lBuf: Byte;

begin
  StartServer;
  AssertNull('Accept timeout',FServer.Accept(50));
  lConnection:=ConnectClient(ClientTLS);
  lServerConnection:=FServer.Accept(5000);
  try
    AssertNull('AcceptStream timeout',lServerConnection.AcceptStream(50));
    lStream:=lConnection.OpenStream;
    try
      lStream.Timeout:=50;
      try
        lStream.Read(lBuf,1);
        Fail('Read must time out');
      except
        on E: EQuicStreamError do
          AssertEquals('Timeout','Read timeout',E.Message);
      end;
      lServerConnection.Close(7,'done');
      AssertTrue('Client sees the close',lConnection.WaitClosed(5000));
      AssertTrue('Server closed',lServerConnection.WaitClosed(5000));
      AssertEquals('Close code',7,lConnection.Connection.CloseErrorCode);
      AssertNull('No stream after close',lConnection.AcceptStream(5000));
      try
        lStream.Read(lBuf,1);
        Fail('Read after close must fail');
      except
        on E: EQuicStreamError do
          AssertEquals('Closed','Connection closed',E.Message);
      end;
      AssertFalse('No handshake wait after close',lConnection.WaitHandshake(100));
    finally
      lStream.Free;
    end;
  finally
    lServerConnection.Free;
  end;
end;


procedure TTestQuicEndpoint.TestHandshakeFailure;

var
  lTLS: TQuicTLSConfig;
  I: Integer;

begin
  StartServer;
  lTLS:=ClientTLS;
  lTLS.Alpn:=['other'];
  AssertNull('Handshake fails',ConnectClient(lTLS));
  I:=0;
  while (FServer.ConnectionCount>0) and (I<500) do
    begin
    Sleep(20);
    Inc(I);
    end;
  AssertEquals('Failed server connection freed',0,FServer.ConnectionCount);
end;


procedure TTestQuicEndpoint.TestConnectionRefused;

var
  lSocket: TFPSocket;
  lPort: Word;

begin
  NeedOpenSSL;
  lSocket:=UDPSocket(stIPv4);
  Bind(lSocket,'127.0.0.1',0);
  lPort:=LocalEndpoint(lSocket).Second;
  CloseSocket(lSocket);
  FClient:=TQuicClient.Create(ClientTLS,QuicDefaultEndpointParams);
  AssertNull('No server',FClient.Connect('127.0.0.1',lPort,300));
end;


procedure TTestQuicEndpoint.TestUnreachableAddress;

begin
  NeedOpenSSL;
  FClient:=TQuicClient.Create(ClientTLS,QuicDefaultEndpointParams);
  AssertNull('Broadcast address',FClient.Connect('255.255.255.255',4433,200));
end;


procedure TTestQuicEndpoint.TestVersionNegotiation;

var
  lSocket: TFPSocket;
  lPacket, lReply: TBytes;
  lResult: TReceiveFromResult;
  lHeader: TQuicPacketHeader;
  I: Integer;

begin
  StartServer;
  lSocket:=UDPSocket(stIPv4);
  try
    lPacket:=nil;
    SetLength(lPacket,1200);
    lPacket[0]:=$C0;
    lPacket[1]:=$1A;
    lPacket[2]:=$2A;
    lPacket[5]:=1;
    lPacket[6]:=$AA;
    lPacket[7]:=1;
    lPacket[8]:=$BB;
    SendTo(lSocket,'127.0.0.1',FServer.Port,PByte(lPacket),Length(lPacket));
    lReply:=nil;
    SetLength(lReply,2000);
    I:=0;
    repeat
      DataAvailable(lSocket,100);
      lResult:=ReceiveFrom(lSocket,PByte(lReply),Length(lReply));
      Inc(I);
    until (lResult.DataSize>0) or (I>50);
    AssertTrue('Reply received',lResult.DataSize>0);
    AssertTrue('Parsed',QuicParseHeader(PByte(lReply),lResult.DataSize,8,lHeader));
    AssertTrue('Version Negotiation',lHeader.PacketType=qptVersionNegotiation);
    AssertEquals('Destination is the sender SCID','bb',QuicBytesToHex(lHeader.DestCid));
    AssertEquals('Source is the sender DCID','aa',QuicBytesToHex(lHeader.SrcCid));
    AssertEquals('Version 1 offered',QUIC_VERSION_1,lHeader.Versions[0]);
  finally
    CloseSocket(lSocket);
  end;
end;


procedure TTestQuicEndpoint.TestIgnoredDatagrams;

var
  lSocket: TFPSocket;
  lPacket: TBytes;
  lWriter: TQuicWriter;

begin
  StartServer;
  lSocket:=UDPSocket(stIPv4);
  try
    lPacket:=QuicHexToBytes('40 0102030405060708 00000000');
    SendTo(lSocket,'127.0.0.1',FServer.Port,PByte(lPacket),Length(lPacket));
    lPacket:=QuicHexToBytes('ff');
    SendTo(lSocket,'127.0.0.1',FServer.Port,PByte(lPacket),Length(lPacket));
    SetLength(lPacket,1200);
    lPacket[0]:=$E0;
    lPacket[4]:=1;
    SendTo(lSocket,'127.0.0.1',FServer.Port,PByte(lPacket),Length(lPacket));
    lWriter.Init;
    QuicWriteLongHeader(lWriter,qptInitial,QuicHexToBytes('0102030405060708'),nil,nil,0,4,1150);
    lWriter.WriteZeros(1200-lWriter.Length);
    lPacket:=lWriter.ToBytes;
    SendTo(lSocket,'127.0.0.1',FServer.Port,PByte(lPacket),Length(lPacket));
    AssertNull('Nothing accepted',FServer.Accept(1000));
    AssertEquals('Undecryptable Initial creates no connection',0,FServer.ConnectionCount);
  finally
    CloseSocket(lSocket);
  end;
end;


procedure TTestQuicEndpoint.TestStreamLimit;

var
  lParams: TQuicTransportParams;
  lConnection, lServerConnection: TQuicEndpointConnection;
  lStream: TQuicStreamAdapter;

begin
  lParams:=QuicDefaultEndpointParams;
  lParams.InitialMaxStreamsBidi:=0;
  FServer:=TQuicServer.Create(ServerTLS,lParams);
  FServer.Listen('127.0.0.1',0);
  lConnection:=ConnectClient(ClientTLS);
  lServerConnection:=FServer.Accept(5000);
  try
    AssertNull('Stream limit',lConnection.OpenStream);
    lStream:=lConnection.OpenStream(False);
    AssertNotNull('Uni stream allowed',lStream);
    lStream.Free;
  finally
    lServerConnection.Free;
  end;
end;


procedure TTestQuicEndpoint.TestIPv6;

var
  lConnection, lServerConnection: TQuicEndpointConnection;

begin
  FServer:=TQuicServer.Create(ServerTLS,QuicDefaultEndpointParams);
  FServer.Listen('::',0);
  FClient:=TQuicClient.Create(ClientTLS,QuicDefaultEndpointParams);
  lConnection:=FClient.Connect('::1',FServer.Port,1000);
  if lConnection<>nil then
    begin
    lServerConnection:=FServer.Accept(5000);
    AssertNotNull('IPv6 connection accepted',lServerConnection);
    lServerConnection.Free;
    end;
end;


procedure TTestQuicEndpoint.TestDualStack;

var
  lConnection, lServerConnection: TQuicEndpointConnection;
  lStream, lServerStream: TQuicStreamAdapter;
  lBuffer: array[0..15] of Byte;

begin
  FServer:=TQuicServer.Create(ServerTLS,QuicDefaultEndpointParams);
  FServer.Listen('::',0);
  FClient:=TQuicClient.Create(ClientTLS,QuicDefaultEndpointParams);
  lConnection:=FClient.Connect('127.0.0.1',FServer.Port,5000);
  AssertNotNull('IPv4 client connected to the dual-stack server',lConnection);
  lServerConnection:=FServer.Accept(5000);
  AssertNotNull('Connection accepted',lServerConnection);
  try
    AssertEquals('Peer address without IPv4 mapping','127.0.0.1',lServerConnection.Address.Address);
    lStream:=lConnection.OpenStream;
    try
      lStream.WriteBuffer(PAnsiChar('ping')^,4);
      lStream.Finish;
      lServerStream:=lServerConnection.AcceptStream(5000);
      try
        AssertEquals('Request length',4,lServerStream.Read(lBuffer[0],SizeOf(lBuffer)));
        lServerStream.WriteBuffer(lBuffer[0],4);
        lServerStream.Finish;
      finally
        lServerStream.Free;
      end;
      AssertEquals('Reply over the dual-stack socket',4,lStream.Read(lBuffer[0],SizeOf(lBuffer)));
    finally
      lStream.Free;
    end;
  finally
    lServerConnection.Free;
  end;
end;


procedure TTestQuicEndpoint.TestWriteTimeout;

var
  lConnection: TQuicEndpointConnection;
  lStream: TQuicStreamAdapter;
  lData: TBytes;

begin
  StartServer;
  lConnection:=ConnectClient(ClientTLS);
  lStream:=lConnection.OpenStream;
  try
    lStream.Timeout:=300;
    lData:=nil;
    SetLength(lData,2100*1024);
    try
      lStream.WriteBuffer(lData[0],Length(lData));
      Fail('Write must time out while the peer does not read');
    except
      on E: EQuicStreamError do
        AssertEquals('Timeout','Write timeout',E.Message);
    end;
  finally
    lStream.Free;
  end;
  AssertEquals('Unaccepted server connection',1,FServer.ConnectionCount);
  FreeAndNil(FServer);
end;


procedure TTestQuicEndpoint.TestRetry;

var
  lConnection, lAccepted: TQuicEndpointConnection;
  lStream, lPeer: TQuicStreamAdapter;

begin
  StartServer;
  FServer.RetryThreshold:=0;
  lConnection:=ConnectClient(ClientTLS);
  AssertNotNull('Connected after Retry',lConnection);
  FClient.Lock;
  try
    AssertTrue('Server sent retry_source_connection_id',lConnection.Connection.PeerParams.HasRetrySourceCid);
  finally
    FClient.Unlock;
  end;
  lStream:=lConnection.OpenStream;
  lAccepted:=nil;
  lPeer:=nil;
  try
    lStream.WriteBuffer(PAnsiChar('retry')^,5);
    lStream.Finish;
    lAccepted:=FServer.Accept(5000);
    AssertNotNull('Server accepted',lAccepted);
    lPeer:=lAccepted.AcceptStream(5000);
    AssertNotNull('Stream accepted',lPeer);
    AssertEquals('Data after Retry','retry',ReadString(lPeer));
  finally
    lPeer.Free;
    lStream.Free;
    lAccepted.Free;
  end;
end;


procedure TTestQuicEndpoint.TestRetryTokens;

var
  lNet: TTestNetwork;
  lSocket, lOther: TFPSocket;
  lReply, lPacket: TBytes;
  lHeader: TQuicPacketHeader;
  lWriter: TQuicWriter;

begin
  StartServer;
  FServer.RetryThreshold:=0;
  lNet:=StartRawClient;
  lSocket:=UDPSocket(stIPv4);
  lOther:=UDPSocket(stIPv4);
  try
    SendDatagrams(lSocket,[lNet.Client.Outbox[0]]);
    lReply:=ReceiveDatagram(lSocket);
    AssertTrue('Reply parsed',QuicParseHeader(PByte(lReply),Length(lReply),8,lHeader));
    AssertTrue('Retry packet',lHeader.PacketType=qptRetry);
    AssertEquals('No state before the Retry is answered',0,FServer.ConnectionCount);
    lNet.Client.Connection.ReceiveDatagram(lReply,lNet.Now);
    lNet.Client.Outbox:=nil;
    lNet.Client.Collect(lNet.Now);
    SendDatagrams(lOther,lNet.Client.Outbox);
    lWriter.Init;
    QuicWriteLongHeader(lWriter,qptInitial,QuicHexToBytes('0102030405060708'),nil,QuicHexToBytes('0102030405'),0,4,
      1150);
    lWriter.WriteZeros(1200-lWriter.Length);
    lPacket:=lWriter.ToBytes;
    SendDatagrams(lSocket,[lPacket]);
    Sleep(200);
    AssertEquals('Token from another port and short token rejected',0,FServer.ConnectionCount);
    SendDatagrams(lSocket,lNet.Client.Outbox);
    AssertEquals('Valid token creates the connection',1,WaitConnectionCount(1));
  finally
    CloseSocket(lOther);
    CloseSocket(lSocket);
    lNet.Free;
  end;
end;


procedure TTestQuicEndpoint.TestConnectionLimits;

var
  lNet: TTestNetwork;
  lSocket: TFPSocket;
  lOther: TQuicClient;

begin
  StartServer;
  FServer.RetryThreshold:=-1;
  FServer.MaxHandshakes:=1;
  lNet:=StartRawClient;
  lSocket:=UDPSocket(stIPv4);
  lOther:=TQuicClient.Create(ClientTLS,QuicDefaultEndpointParams);
  try
    SendDatagrams(lSocket,lNet.Client.Outbox);
    AssertEquals('Unfinished handshake',1,WaitConnectionCount(1));
    AssertNull('Handshake limit reached',lOther.Connect('127.0.0.1',FServer.Port,500));
    FServer.MaxHandshakes:=0;
    FServer.MaxConnections:=1;
    AssertNull('Connection limit reached',lOther.Connect('127.0.0.1',FServer.Port,500));
    AssertEquals('No connection added',1,FServer.ConnectionCount);
    FServer.MaxConnections:=0;
    AssertNotNull('Accepted without limits',ConnectClient(ClientTLS));
  finally
    lOther.Free;
    CloseSocket(lSocket);
    lNet.Free;
  end;
end;


procedure TTestQuicEndpoint.TestWorkerException;

var
  lServer: TFailingServer;
  lConnection, lServerConnection: TQuicEndpointConnection;

begin
  lServer:=TFailingServer.Create(ServerTLS,QuicDefaultEndpointParams);
  FServer:=lServer;
  FServer.Listen('127.0.0.1',0);
  lConnection:=ConnectClient(ClientTLS);
  AssertNotNull('Connected',lConnection);
  lServerConnection:=FServer.Accept(5000);
  AssertNotNull('Accepted',lServerConnection);
  try
    lServer.FailNext:=True;
    AssertTrue('Server connection closed',lServerConnection.WaitClosed(5000));
    AssertEquals('Error code',QUIC_INTERNAL_ERROR,lServerConnection.Connection.CloseErrorCode);
    AssertEquals('Reason','Worker failure',lServerConnection.Connection.CloseReason);
  finally
    lServerConnection.Free;
  end;
end;


initialization
  RegisterTest(TTestQuicEndpoint);
end.
