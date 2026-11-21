{
  This file is part of the Free Component Library.
  Tests for the OpenSSL QUIC TLS session: in-memory handshakes between a client and a server session.
  See the file COPYING.FPC, included in this distribution, for details about the copyright.
}
unit utquicopenssltls;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  SysUtils, Classes, fpcunit, testregistry, FpQuic.Consts, FpQuic.Buffers, FpQuic.TransportParams, FpQuic.TLS,
  FpQuic.OpenSSL, utquichelpers;

type
  TTLSPeer = class;

  // Records the TLS output of one side and forwards handshake data to the other side.
  TTLSPeer = class(TObject, IQuicTLSEvents)
  public
    TLSObject: TQuicOpenSSLTLS;
    TLS: IQuicTLS;
    Context: TQuicOpenSSLContext;
    Peer: TTLSPeer;
    Pending: array of record
      Level: TQuicEncryptionLevel;
      Data: TBytes;
    end;
    WriteSecrets: array[TQuicEncryptionLevel] of TBytes;
    ReadSecrets: array[TQuicEncryptionLevel] of TBytes;
    Suite: TQuicCipherSuite;
    PeerParams: TBytes;
    Alert: Integer;
    AppDataReceived: Integer;
    EarlyWriteSecret: TBytes;
    EarlyReadSecret: TBytes;
    Ticket: TBytes;
    OwnsContext: Boolean;
    // Name of the event that raises an exception: send, secret, params, alert, early or ticket.
    FailOn: String;
    procedure CheckFail(const aEvent: String);
    constructor Create(aIsServer: Boolean; const aConfig: TQuicTLSConfig);
    constructor CreateShared(aContext: TQuicOpenSSLContext);
    destructor Destroy; override;
    procedure TLSSendCrypto(aLevel: TQuicEncryptionLevel; const aData: TBytes);
    procedure TLSSecret(aLevel: TQuicEncryptionLevel; aWrite: Boolean; aSuite: TQuicCipherSuite; const aSecret: TBytes);
    procedure TLSPeerTransportParams(const aParams: TBytes);
    procedure TLSAlert(aAlert: Byte);
    procedure TLSEarlySecret(aWrite: Boolean; aSuite: TQuicCipherSuite; const aSecret: TBytes);
    procedure TLSSessionTicket(const aTicket: TBytes);
    function Flush: Boolean;
  end;

  TTestQuicOpenSSLTLS = class(TTestCase)
  private
    FClient, FServer: TTLSPeer;
    function ServerConfig: TQuicTLSConfig;
    function ClientConfig: TQuicTLSConfig;
    procedure Handshake(const aClient, aServer: TQuicTLSConfig);
    procedure Exchange(const aClientParams, aServerParams: TBytes);
    function FirstTicket(aServerContext: TQuicOpenSSLContext; const aClient: TQuicTLSConfig): TBytes;
    function FailingHandshake(const aClient, aServer: TQuicTLSConfig; aServerFails: Boolean;
      const aEvent: String): String;
    procedure ExpectConfigError(const aConfig: TQuicTLSConfig; const aMessage: String);
  protected
    procedure TearDown; override;
  published
    procedure TestResumptionWithEarlyData;
    procedure TestEarlyDataRejectedForLowerLimits;
    procedure TestResumptionWithoutEarlyData;
    procedure TestInvalidTickets;
    procedure TestCertificateData;
    procedure TestBadCertificateData;
    procedure TestHandshake;
    procedure TestCipherSuites;
    procedure TestKeyLog;
    procedure TestVerifiedHandshake;
    procedure TestVerifiedByCAPath;
    procedure TestWrongServerName;
    procedure TestDefaultTrustStore;
    procedure TestAlpnMismatch;
    procedure TestBadCertificateFile;
    procedure TestEventExceptions;
    procedure TestEarlySecretException;
    procedure TestKeyLogError;
    procedure TestVerifyNeedsServerName;
    procedure TestMissingCAFile;
    procedure TestInvalidCipherSuites;
    procedure TestUnsupportedCipherSuite;
  end;

implementation

{ TTLSPeer }

constructor TTLSPeer.Create(aIsServer: Boolean; const aConfig: TQuicTLSConfig);

begin
  CreateShared(TQuicOpenSSLContext.Create(aIsServer,aConfig));
  OwnsContext:=True;
end;


constructor TTLSPeer.CreateShared(aContext: TQuicOpenSSLContext);

begin
  Alert:=-1;
  Context:=aContext;
  TLSObject:=TQuicOpenSSLTLS.Create(Context);
  TLS:=TLSObject;
end;


destructor TTLSPeer.Destroy;

begin
  TLSObject.Free;
  if OwnsContext then
    Context.Free;
  inherited Destroy;
end;


procedure TTLSPeer.CheckFail(const aEvent: String);

begin
  if FailOn=aEvent then
    raise Exception.Create('Event '+aEvent+' failed');
end;


procedure TTLSPeer.TLSEarlySecret(aWrite: Boolean; aSuite: TQuicCipherSuite; const aSecret: TBytes);

begin
  CheckFail('early');
  if aWrite then
    EarlyWriteSecret:=aSecret
  else
    EarlyReadSecret:=aSecret;
end;


procedure TTLSPeer.TLSSessionTicket(const aTicket: TBytes);

begin
  CheckFail('ticket');
  Ticket:=aTicket;
end;


procedure TTLSPeer.TLSSendCrypto(aLevel: TQuicEncryptionLevel; const aData: TBytes);

begin
  CheckFail('send');
  SetLength(Pending,Length(Pending)+1);
  Pending[High(Pending)].Level:=aLevel;
  Pending[High(Pending)].Data:=aData;
end;


procedure TTLSPeer.TLSSecret(aLevel: TQuicEncryptionLevel; aWrite: Boolean; aSuite: TQuicCipherSuite;
  const aSecret: TBytes);

begin
  CheckFail('secret');
  Suite:=aSuite;
  if aWrite then
    WriteSecrets[aLevel]:=aSecret
  else
    ReadSecrets[aLevel]:=aSecret;
end;


procedure TTLSPeer.TLSPeerTransportParams(const aParams: TBytes);

begin
  CheckFail('params');
  PeerParams:=aParams;
end;


procedure TTLSPeer.TLSAlert(aAlert: Byte);

begin
  CheckFail('alert');
  Alert:=aAlert;
end;


function TTLSPeer.Flush: Boolean;

var
  lLevel: TQuicEncryptionLevel;
  lData: TBytes;

begin
  Result:=Length(Pending)>0;
  while Length(Pending)>0 do
    begin
    lLevel:=Pending[0].Level;
    lData:=Pending[0].Data;
    Delete(Pending,0,1);
    if Peer.TLS.IsComplete and (lLevel=qelApplication) then
      Inc(Peer.AppDataReceived);
    Peer.TLS.ProvideData(lLevel,lData);
    end;
end;


{ TTestQuicOpenSSLTLS }

function TTestQuicOpenSSLTLS.ServerConfig: TQuicTLSConfig;

begin
  NeedOpenSSL;
  Result:=Default(TQuicTLSConfig);
  Result.Alpn:=['h3','hq-interop'];
  Result.CertificateFile:=TestDataDir+'server.crt';
  Result.PrivateKeyFile:=TestDataDir+'server.key';
end;


function TTestQuicOpenSSLTLS.ClientConfig: TQuicTLSConfig;

begin
  NeedOpenSSL;
  Result:=Default(TQuicTLSConfig);
  Result.Alpn:=['hq-interop'];
  Result.ServerName:='localhost';
end;


procedure TTestQuicOpenSSLTLS.Handshake(const aClient, aServer: TQuicTLSConfig);

var
  lRounds: Integer;

begin
  FClient:=TTLSPeer.Create(False,aClient);
  FServer:=TTLSPeer.Create(True,aServer);
  FClient.Peer:=FServer;
  FServer.Peer:=FClient;
  FServer.TLS.Start(FServer,QuicHexToBytes('0401aa'));
  FClient.TLS.Start(FClient,QuicHexToBytes('0402bbbb'));
  lRounds:=0;
  while (FClient.Flush or FServer.Flush) and (lRounds<20) do
    Inc(lRounds);
end;


// Encoded server transport parameters with the given connection data limit.
function ServerParamBytes(aMaxData: QWord): TBytes;

var
  lParams: TQuicTransportParams;

begin
  lParams:=QuicDefaultTransportParams;
  lParams.InitialMaxData:=aMaxData;
  lParams.InitialMaxStreamsBidi:=10;
  Result:=QuicEncodeTransportParams(lParams);
end;


procedure TTestQuicOpenSSLTLS.Exchange(const aClientParams, aServerParams: TBytes);

var
  lRounds: Integer;

begin
  FClient.Peer:=FServer;
  FServer.Peer:=FClient;
  FServer.TLS.Start(FServer,aServerParams);
  FClient.TLS.Start(FClient,aClientParams);
  lRounds:=0;
  while (FClient.Flush or FServer.Flush) and (lRounds<20) do
    Inc(lRounds);
end;


function TTestQuicOpenSSLTLS.FirstTicket(aServerContext: TQuicOpenSSLContext; const aClient: TQuicTLSConfig): TBytes;

begin
  FClient:=TTLSPeer.Create(False,aClient);
  FServer:=TTLSPeer.CreateShared(aServerContext);
  Exchange(QuicHexToBytes('0402bbbb'),ServerParamBytes(1000));
  AssertTrue('Full handshake complete',FClient.TLS.IsComplete);
  AssertFalse('No early data on a full handshake',FClient.TLS.EarlyDataAccepted);
  AssertEquals('No early secret on a full handshake',0,Length(FClient.EarlyWriteSecret));
  Result:=FClient.Ticket;
  AssertTrue('Ticket received',Length(Result)>0);
  TearDown;
end;


procedure TTestQuicOpenSSLTLS.TestResumptionWithEarlyData;

var
  lServer, lClient: TQuicTLSConfig;
  lContext: TQuicOpenSSLContext;
  lTicket, lParams: TBytes;

begin
  lServer:=ServerConfig;
  lServer.EnableEarlyData:=True;
  lClient:=ClientConfig;
  lClient.EnableEarlyData:=True;
  lContext:=TQuicOpenSSLContext.Create(True,lServer);
  try
    lTicket:=FirstTicket(lContext,lClient);
    FClient:=TTLSPeer.Create(False,lClient);
    FServer:=TTLSPeer.CreateShared(lContext);
    AssertTrue('0-RTT possible',FClient.TLS.Resume(lTicket,lParams));
    AssertEquals('Remembered server parameters',QuicBytesToHex(ServerParamBytes(1000)),QuicBytesToHex(lParams));
    Exchange(QuicHexToBytes('0402bbbb'),ServerParamBytes(1000));
    AssertTrue('Resumed handshake complete',FServer.TLS.IsComplete);
    AssertTrue('Client early secret',Length(FClient.EarlyWriteSecret)>0);
    AssertTrue('Server accepted early data',FServer.TLS.EarlyDataAccepted);
    AssertTrue('Client sees early data accepted',FClient.TLS.EarlyDataAccepted);
    AssertTrue('Early secrets match',QuicBytesEqual(FClient.EarlyWriteSecret,FServer.EarlyReadSecret));
  finally
    TearDown;
    lContext.Free;
  end;
end;


procedure TTestQuicOpenSSLTLS.TestEarlyDataRejectedForLowerLimits;

var
  lServer, lClient: TQuicTLSConfig;
  lContext: TQuicOpenSSLContext;
  lTicket, lParams: TBytes;

begin
  lServer:=ServerConfig;
  lServer.EnableEarlyData:=True;
  lClient:=ClientConfig;
  lClient.EnableEarlyData:=True;
  lContext:=TQuicOpenSSLContext.Create(True,lServer);
  try
    lTicket:=FirstTicket(lContext,lClient);
    FClient:=TTLSPeer.Create(False,lClient);
    FServer:=TTLSPeer.CreateShared(lContext);
    AssertTrue('0-RTT attempted',FClient.TLS.Resume(lTicket,lParams));
    Exchange(QuicHexToBytes('0402bbbb'),ServerParamBytes(500));
    AssertTrue('Handshake complete',FClient.TLS.IsComplete);
    AssertFalse('Client sees early data rejected',FClient.TLS.EarlyDataAccepted);
    AssertEquals('Server has no early secret',0,Length(FServer.EarlyReadSecret));
  finally
    TearDown;
    lContext.Free;
  end;
end;


procedure TTestQuicOpenSSLTLS.TestResumptionWithoutEarlyData;

var
  lClient: TQuicTLSConfig;
  lContext: TQuicOpenSSLContext;
  lTicket, lParams: TBytes;

begin
  lClient:=ClientConfig;
  lContext:=TQuicOpenSSLContext.Create(True,ServerConfig);
  try
    lTicket:=FirstTicket(lContext,lClient);
    FClient:=TTLSPeer.Create(False,lClient);
    FServer:=TTLSPeer.CreateShared(lContext);
    AssertFalse('No 0-RTT without EnableEarlyData',FClient.TLS.Resume(lTicket,lParams));
    AssertEquals('No parameters',0,Length(lParams));
    Exchange(QuicHexToBytes('0402bbbb'),ServerParamBytes(1000));
    AssertTrue('Resumed handshake complete',FClient.TLS.IsComplete);
  finally
    TearDown;
    lContext.Free;
  end;
end;


procedure TTestQuicOpenSSLTLS.TestInvalidTickets;

var
  lParams: TBytes;

begin
  FClient:=TTLSPeer.Create(False,ClientConfig);
  AssertFalse('Truncated ticket',FClient.TLS.Resume(QuicHexToBytes('0501'),lParams));
  AssertFalse('Invalid session',FClient.TLS.Resume(QuicHexToBytes('0301020300'),lParams));
end;


procedure TTestQuicOpenSSLTLS.TestCertificateData;

var
  lServer: TQuicTLSConfig;
  lCert: TBytes;

begin
  lServer:=ServerConfig;
  lCert:=FileBytes(TestDataDir+'server.crt');
  lServer.CertificateData:=Concat(lCert,lCert);
  lServer.PrivateKeyData:=FileBytes(TestDataDir+'server.key');
  lServer.CertificateFile:='';
  lServer.PrivateKeyFile:='';
  Handshake(ClientConfig,lServer);
  AssertTrue('Handshake with in-memory certificate',FClient.TLS.IsComplete);
end;


procedure TTestQuicOpenSSLTLS.TestBadCertificateData;

var
  lServer: TQuicTLSConfig;

begin
  lServer:=ServerConfig;
  lServer.CertificateData:=QuicHexToBytes('2d2d2d');
  lServer.PrivateKeyData:=FileBytes(TestDataDir+'server.key');
  try
    TQuicOpenSSLContext.Create(True,lServer).Free;
    Fail('Invalid certificate data must fail');
  except
    on E: EQuicOpenSSL do
      AssertTrue('Message',Pos('certificate',E.Message)>0);
  end;
end;


procedure TTestQuicOpenSSLTLS.TearDown;

begin
  FreeAndNil(FClient);
  FreeAndNil(FServer);
end;


procedure TTestQuicOpenSSLTLS.TestHandshake;

var
  lLevel: TQuicEncryptionLevel;

begin
  Handshake(ClientConfig,ServerConfig);
  AssertTrue('Client complete',FClient.TLS.IsComplete);
  AssertTrue('Server complete',FServer.TLS.IsComplete);
  AssertEquals('No client alert',-1,FClient.Alert);
  AssertEquals('No server alert',-1,FServer.Alert);
  AssertEquals('Client ALPN','hq-interop',FClient.TLS.NegotiatedAlpn);
  AssertEquals('Server ALPN','hq-interop',FServer.TLS.NegotiatedAlpn);
  AssertEquals('Server saw client params','0402bbbb',QuicBytesToHex(FServer.PeerParams));
  AssertEquals('Client saw server params','0401aa',QuicBytesToHex(FClient.PeerParams));
  for lLevel:=qelHandshake to qelApplication do
    begin
    AssertTrue('Client write secret',Length(FClient.WriteSecrets[lLevel])>0);
    AssertTrue('Client write = server read',QuicBytesEqual(FClient.WriteSecrets[lLevel],FServer.ReadSecrets[lLevel]));
    AssertTrue('Server write = client read',QuicBytesEqual(FServer.WriteSecrets[lLevel],FClient.ReadSecrets[lLevel]));
    end;
  AssertTrue('Session tickets delivered after completion',FClient.AppDataReceived>0);
end;


procedure TTestQuicOpenSSLTLS.TestCipherSuites;

const
  Names: array[TQuicCipherSuite] of AnsiString = ('TLS_AES_128_GCM_SHA256','TLS_AES_256_GCM_SHA384',
    'TLS_CHACHA20_POLY1305_SHA256');
  SecretLengths: array[TQuicCipherSuite] of Integer = (32,48,32);

var
  lSuite: TQuicCipherSuite;
  lClient: TQuicTLSConfig;

begin
  for lSuite:=Low(TQuicCipherSuite) to High(TQuicCipherSuite) do
    begin
    lClient:=ClientConfig;
    lClient.CipherSuites:=Names[lSuite];
    Handshake(lClient,ServerConfig);
    AssertTrue('Complete '+Names[lSuite],FClient.TLS.IsComplete);
    AssertTrue('Suite '+Names[lSuite],FClient.Suite=lSuite);
    AssertEquals('Secret length',SecretLengths[lSuite],Length(FClient.WriteSecrets[qelApplication]));
    TearDown;
    end;
end;


procedure TTestQuicOpenSSLTLS.TestKeyLog;

var
  lClient: TQuicTLSConfig;
  lFileName: String;
  lLines: TStringList;

begin
  lFileName:=GetTempFileName;
  DeleteFile(lFileName);
  lClient:=ClientConfig;
  lClient.KeyLogFile:=lFileName;
  Handshake(lClient,ServerConfig);
  AssertTrue('Complete',FClient.TLS.IsComplete);
  lLines:=TStringList.Create;
  try
    lLines.LoadFromFile(lFileName);
    AssertEquals('One line per secret',5,lLines.Count);
    AssertTrue('Server handshake secret logged',Pos('SERVER_HANDSHAKE_TRAFFIC_SECRET ',lLines.Text)>0);
    AssertTrue('Client application secret logged',Pos('CLIENT_TRAFFIC_SECRET_0 ',lLines.Text)>0);
  finally
    lLines.Free;
    DeleteFile(lFileName);
  end;
end;


procedure TTestQuicOpenSSLTLS.TestVerifiedHandshake;

var
  lClient: TQuicTLSConfig;

begin
  lClient:=ClientConfig;
  lClient.VerifyPeer:=True;
  lClient.CAFile:=TestDataDir+'server.crt';
  Handshake(lClient,ServerConfig);
  AssertTrue('Verified handshake complete',FClient.TLS.IsComplete);
end;


procedure TTestQuicOpenSSLTLS.TestVerifiedByCAPath;

var
  lClient: TQuicTLSConfig;

begin
  lClient:=ClientConfig;
  lClient.VerifyPeer:=True;
  lClient.CAPath:=TestDataDir+'capath';
  Handshake(lClient,ServerConfig);
  AssertTrue('Handshake verified with a hashed CA directory',FClient.TLS.IsComplete);
end;


procedure TTestQuicOpenSSLTLS.TestWrongServerName;

var
  lClient: TQuicTLSConfig;

begin
  lClient:=ClientConfig;
  lClient.VerifyPeer:=True;
  lClient.CAFile:=TestDataDir+'server.crt';
  lClient.ServerName:='example.org';
  Handshake(lClient,ServerConfig);
  AssertFalse('Not complete',FClient.TLS.IsComplete);
  AssertTrue('Client sent an alert',FClient.Alert>0);
end;


procedure TTestQuicOpenSSLTLS.TestDefaultTrustStore;

var
  lClient: TQuicTLSConfig;

begin
  lClient:=ClientConfig;
  lClient.VerifyPeer:=True;
  Handshake(lClient,ServerConfig);
  AssertFalse('Self-signed certificate rejected',FClient.TLS.IsComplete);
  AssertTrue('Client sent an alert',FClient.Alert>0);
end;


procedure TTestQuicOpenSSLTLS.TestAlpnMismatch;

var
  lClient: TQuicTLSConfig;

begin
  lClient:=ClientConfig;
  lClient.Alpn:=['other'];
  Handshake(lClient,ServerConfig);
  AssertEquals('no_application_protocol alert',120,FServer.Alert);
  AssertFalse('Server not complete',FServer.TLS.IsComplete);
end;


procedure TTestQuicOpenSSLTLS.TestBadCertificateFile;

var
  lServer: TQuicTLSConfig;

begin
  lServer:=ServerConfig;
  lServer.CertificateFile:=TestDataDir+'missing.crt';
  try
    TQuicOpenSSLContext.Create(True,lServer).Free;
    Fail('Missing certificate must fail');
  except
    on E: EQuicOpenSSL do
      AssertTrue('Message',Pos('certificate',E.Message)>0);
  end;
end;


function TTestQuicOpenSSLTLS.FailingHandshake(const aClient, aServer: TQuicTLSConfig; aServerFails: Boolean;
  const aEvent: String): String;

begin
  FClient:=TTLSPeer.Create(False,aClient);
  FServer:=TTLSPeer.Create(True,aServer);
  if aServerFails then
    FServer.FailOn:=aEvent
  else
    FClient.FailOn:=aEvent;
  Result:='';
  try
    Exchange(QuicHexToBytes('0402bbbb'),QuicHexToBytes('0401aa'));
  except
    on E: Exception do
      Result:=E.Message;
  end;
  TearDown;
end;


procedure TTestQuicOpenSSLTLS.ExpectConfigError(const aConfig: TQuicTLSConfig; const aMessage: String);

begin
  try
    TQuicOpenSSLContext.Create(False,aConfig).Free;
    Fail('Context creation must fail: '+aMessage);
  except
    on E: EQuicOpenSSL do
      AssertEquals('Message',aMessage,E.Message);
  end;
end;


procedure TTestQuicOpenSSLTLS.TestEventExceptions;

const
  ClientEvents: array[0..3] of String = ('send','secret','params','ticket');

var
  lEvent: String;
  lClient: TQuicTLSConfig;

begin
  for lEvent in ClientEvents do
    AssertEquals('Client '+lEvent,'Event '+lEvent+' failed',FailingHandshake(ClientConfig,ServerConfig,False,lEvent));
  lClient:=ClientConfig;
  lClient.Alpn:=['other'];
  AssertEquals('Server alert','Event alert failed',FailingHandshake(lClient,ServerConfig,True,'alert'));
end;


procedure TTestQuicOpenSSLTLS.TestEarlySecretException;

var
  lServer, lClient: TQuicTLSConfig;
  lContext: TQuicOpenSSLContext;
  lTicket, lParams: TBytes;

begin
  lServer:=ServerConfig;
  lServer.EnableEarlyData:=True;
  lClient:=ClientConfig;
  lClient.EnableEarlyData:=True;
  lContext:=TQuicOpenSSLContext.Create(True,lServer);
  try
    lTicket:=FirstTicket(lContext,lClient);
    FClient:=TTLSPeer.Create(False,lClient);
    FServer:=TTLSPeer.CreateShared(lContext);
    FClient.FailOn:='early';
    AssertTrue('0-RTT possible',FClient.TLS.Resume(lTicket,lParams));
    try
      Exchange(QuicHexToBytes('0402bbbb'),ServerParamBytes(1000));
      Fail('The early secret event must fail');
    except
      on E: Exception do
        AssertEquals('Message','Event early failed',E.Message);
    end;
  finally
    TearDown;
    lContext.Free;
  end;
end;


procedure TTestQuicOpenSSLTLS.TestKeyLogError;

var
  lClient: TQuicTLSConfig;

begin
  lClient:=ClientConfig;
  lClient.KeyLogFile:=TestDataDir+'missing-directory'+PathDelim+'keys.log';
  try
    Handshake(lClient,ServerConfig);
    Fail('Writing the key log must fail');
  except
    on E: EFCreateError do
      AssertFalse('Not complete',FClient.TLS.IsComplete);
  end;
end;


procedure TTestQuicOpenSSLTLS.TestVerifyNeedsServerName;

var
  lClient: TQuicTLSConfig;

begin
  lClient:=ClientConfig;
  lClient.VerifyPeer:=True;
  lClient.ServerName:='';
  ExpectConfigError(lClient,'VerifyPeer needs a ServerName');
end;


procedure TTestQuicOpenSSLTLS.TestMissingCAFile;

var
  lClient: TQuicTLSConfig;

begin
  lClient:=ClientConfig;
  lClient.VerifyPeer:=True;
  lClient.CAFile:=TestDataDir+'missing.crt';
  ExpectConfigError(lClient,'Cannot load the CA certificates');
end;


procedure TTestQuicOpenSSLTLS.TestInvalidCipherSuites;

var
  lClient: TQuicTLSConfig;

begin
  lClient:=ClientConfig;
  lClient.CipherSuites:='TLS_BOGUS';
  ExpectConfigError(lClient,'Invalid cipher suites');
end;


procedure TTestQuicOpenSSLTLS.TestUnsupportedCipherSuite;

var
  lClient, lServer: TQuicTLSConfig;

begin
  lClient:=ClientConfig;
  lClient.CipherSuites:='TLS_AES_128_CCM_SHA256';
  lServer:=ServerConfig;
  lServer.CipherSuites:='TLS_AES_128_CCM_SHA256';
  AssertEquals('CCM refused','Cipher suite TLS_AES_128_CCM_SHA256 is not supported by QUIC',
    FailingHandshake(lClient,lServer,False,''));
end;


initialization
  RegisterTest(TTestQuicOpenSSLTLS);
end.
