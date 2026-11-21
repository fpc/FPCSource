{
  This file is part of the Free Component Library.
  Deterministic in-memory network connecting a client and a server TQuicConnection for tests.
  See the file COPYING.FPC, included in this distribution, for details about the copyright.
}
unit utquicnetwork;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  SysUtils, Classes, FpQuic.Consts, FpQuic.Buffers, FpQuic.Packets, FpQuic.TransportParams, FpQuic.TLS,
  FpQuic.OpenSSL, FpQuic.Connection, utquichelpers;

type
  TTestNetwork = class;

  // Decides whether a datagram is delivered; return False to drop it.
  TDatagramFilter = function(aNetwork: TTestNetwork; aFromClient: Boolean; var aData: TBytes): Boolean of object;

  // One side of the test network with its TLS session and event log.
  TTestPeer = class(TObject, IQuicConnectionEvents)
  public
    Context: TQuicOpenSSLContext;
    TLSObject: TQuicOpenSSLTLS;
    Connection: TQuicConnection;
    HandshakeEvents: Integer;
    DataEvents: Integer;
    CloseEvents: Integer;
    Outbox: array of TBytes;
    // Latest session ticket received and the number of tickets.
    Ticket: TBytes;
    TicketEvents: Integer;
    // Raise an exception when a session ticket arrives.
    FailOnTicket: Boolean;
    destructor Destroy; override;
    procedure QuicHandshakeComplete(aConnection: TQuicConnection);
    procedure QuicStreamData(aConnection: TQuicConnection; aStreamId: QWord);
    procedure QuicConnectionClosed(aConnection: TQuicConnection);
    procedure QuicSessionTicket(aConnection: TQuicConnection; const aTicket: TBytes);
    // Collects all datagrams the connection wants to send.
    procedure Collect(aNow: QWord);
  end;

  // Client and server connected through queues, with a virtual clock in microseconds.
  TTestNetwork = class
  private
    FServerConfig: TQuicConnectionConfig;
  public
    Now: QWord;
    Client: TTestPeer;
    Server: TTestPeer;
    ClientTLS: TQuicTLSConfig;
    ServerTLS: TQuicTLSConfig;
    ClientParams: TQuicTransportParams;
    ServerParams: TQuicTransportParams;
    Filter: TDatagramFilter;
    Delivered: Integer;
    Dropped: Integer;
    // Session ticket the client resumes, empty for a full handshake.
    ClientTicket: TBytes;
    // Server context used instead of a new one when set; not freed by the network.
    SharedServerContext: TQuicOpenSSLContext;
    // Configuration of the server connection, available after Start.
    function ServerConfigForTest: TQuicConnectionConfig;
    // Creates the network with default TLS settings and transport parameters.
    constructor Create;
    destructor Destroy; override;
    // Creates the client connection.
    procedure Start;
    // Exchanges datagrams and fires timers until nothing happens or aMaxRounds rounds passed.
    procedure Run(aMaxRounds: Integer = 200);
    // Delivers pending datagrams once in each direction; returns True when something was delivered.
    function Step: Boolean;
    // Advances the clock to the earliest connection timer, if due within aHorizon microseconds, and fires it.
    function FireTimer(aHorizon: QWord = 2000000): Boolean;
    // Runs until both sides completed the handshake.
    procedure Handshake;
  end;

// Default transport parameters for test connections.
function TestParams: TQuicTransportParams;

implementation

function TestParams: TQuicTransportParams;

begin
  Result:=QuicDefaultTransportParams;
  Result.MaxIdleTimeout:=30000;
  Result.InitialMaxData:=100000;
  Result.InitialMaxStreamDataBidiLocal:=50000;
  Result.InitialMaxStreamDataBidiRemote:=50000;
  Result.InitialMaxStreamDataUni:=50000;
  Result.InitialMaxStreamsBidi:=10;
  Result.InitialMaxStreamsUni:=10;
end;


{ TTestPeer }

destructor TTestPeer.Destroy;

begin
  Connection.Free;
  TLSObject.Free;
  Context.Free;
  inherited Destroy;
end;


procedure TTestPeer.QuicHandshakeComplete(aConnection: TQuicConnection);

begin
  Inc(HandshakeEvents);
end;


procedure TTestPeer.QuicStreamData(aConnection: TQuicConnection; aStreamId: QWord);

begin
  Inc(DataEvents);
end;


procedure TTestPeer.QuicConnectionClosed(aConnection: TQuicConnection);

begin
  Inc(CloseEvents);
end;


procedure TTestPeer.QuicSessionTicket(aConnection: TQuicConnection; const aTicket: TBytes);

begin
  if FailOnTicket then
    raise Exception.Create('Ticket store failed');
  Ticket:=aTicket;
  Inc(TicketEvents);
end;


procedure TTestPeer.Collect(aNow: QWord);

var
  lData: TBytes;

begin
  if Connection=nil then
    Exit;
  while Connection.NextDatagram(aNow,lData) do
    Insert(lData,Outbox,Length(Outbox));
end;


{ TTestNetwork }

constructor TTestNetwork.Create;

begin
  NeedOpenSSL;
  Now:=1000000;
  ClientTLS:=Default(TQuicTLSConfig);
  ClientTLS.Alpn:=['hq-interop'];
  ClientTLS.ServerName:='localhost';
  ServerTLS:=Default(TQuicTLSConfig);
  ServerTLS.Alpn:=['hq-interop'];
  ServerTLS.CertificateFile:=TestDataDir+'server.crt';
  ServerTLS.PrivateKeyFile:=TestDataDir+'server.key';
  ClientParams:=TestParams;
  ServerParams:=TestParams;
  Client:=TTestPeer.Create;
  Server:=TTestPeer.Create;
end;


destructor TTestNetwork.Destroy;

begin
  Client.Free;
  Server.Free;
  inherited Destroy;
end;


procedure TTestNetwork.Start;

var
  lConfig: TQuicConnectionConfig;

begin
  Client.Context:=TQuicOpenSSLContext.Create(False,ClientTLS);
  Client.TLSObject:=TQuicOpenSSLTLS.Create(Client.Context);
  lConfig:=Default(TQuicConnectionConfig);
  lConfig.IsServer:=False;
  lConfig.TLS:=Client.TLSObject;
  lConfig.CipherFactory:=TestCipherFactory;
  lConfig.Params:=ClientParams;
  lConfig.Events:=Client;
  lConfig.Ticket:=ClientTicket;
  Client.Connection:=TQuicConnection.CreateClient(lConfig,Now);
  if SharedServerContext<>nil then
    Server.TLSObject:=TQuicOpenSSLTLS.Create(SharedServerContext)
  else
    begin
    Server.Context:=TQuicOpenSSLContext.Create(True,ServerTLS);
    Server.TLSObject:=TQuicOpenSSLTLS.Create(Server.Context);
    end;
  FServerConfig:=Default(TQuicConnectionConfig);
  FServerConfig.IsServer:=True;
  FServerConfig.TLS:=Server.TLSObject;
  FServerConfig.CipherFactory:=TestCipherFactory;
  FServerConfig.Params:=ServerParams;
  FServerConfig.Events:=Server;
end;


function TTestNetwork.ServerConfigForTest: TQuicConnectionConfig;

begin
  Result:=FServerConfig;
end;


function TTestNetwork.Step: Boolean;

var
  lData: TBytes;
  lHeader: TQuicPacketHeader;
  lFromClient: Boolean;
  lFrom, lTo: TTestPeer;

begin
  Result:=False;
  for lFromClient:=True downto False do
    begin
    if lFromClient then
      begin
      lFrom:=Client;
      lTo:=Server;
      end
    else
      begin
      lFrom:=Server;
      lTo:=Client;
      end;
    lFrom.Collect(Now);
    while Length(lFrom.Outbox)>0 do
      begin
      lData:=lFrom.Outbox[0];
      Delete(lFrom.Outbox,0,1);
      Result:=True;
      if Assigned(Filter) and not Filter(Self,lFromClient,lData) then
        begin
        Inc(Dropped);
        Continue;
        end;
      Inc(Delivered);
      if (lTo.Connection=nil) and QuicParseHeader(PByte(lData),Length(lData),QUIC_LOCAL_CID_LENGTH,lHeader) then
        lTo.Connection:=TQuicConnection.CreateServer(FServerConfig,lHeader.DestCid,Now);
      lTo.Connection.ReceiveDatagram(lData,Now);
      end;
    end;
end;


function TTestNetwork.FireTimer(aHorizon: QWord): Boolean;

var
  lNext, lServerNext: QWord;

begin
  lNext:=Client.Connection.NextTimeout;
  if Server.Connection<>nil then
    begin
    lServerNext:=Server.Connection.NextTimeout;
    if lServerNext<lNext then
      lNext:=lServerNext;
    end;
  Result:=(lNext<>High(QWord)) and (lNext<=Now+aHorizon);
  if not Result then
    Exit;
  if lNext>Now then
    Now:=lNext;
  Client.Connection.HandleTimeout(Now);
  if Server.Connection<>nil then
    Server.Connection.HandleTimeout(Now);
end;


procedure TTestNetwork.Run(aMaxRounds: Integer);

var
  lRound: Integer;

begin
  for lRound:=1 to aMaxRounds do
    if not Step and not FireTimer then
      Break;
end;


procedure TTestNetwork.Handshake;

var
  lRound: Integer;

begin
  Start;
  lRound:=0;
  while ((Server.Connection=nil) or not (Client.Connection.HandshakeConfirmed and Server.Connection.HandshakeComplete))
        and (lRound<100) do
    begin
    if not Step then
      FireTimer;
    Inc(lRound);
    end;
end;


end.
