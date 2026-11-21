{
  This file is part of the Free Component Library.
  Tests for session resumption and 0-RTT data in FpQuic.Connection over the in-memory test network.
  See the file COPYING.FPC, included in this distribution, for details about the copyright.
}
unit utquicearlydata;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpcunit, testregistry, FpQuic.Consts, FpQuic.Buffers, FpQuic.Frames, FpQuic.Packets,
  FpQuic.TransportParams, FpQuic.Crypto, FpQuic.OpenSSL, FpQuic.Connection, utquichelpers, utquicnetwork;

type
  TTestQuicEarlyData = class(TTestCase)
  private
    FContext: TQuicOpenSSLContext;
    FNet: TTestNetwork;
    FZeroRttPackets: Integer;
    function CountFilter(aNetwork: TTestNetwork; aFromClient: Boolean; var aData: TBytes): Boolean;
    function NewNetwork: TTestNetwork;
    function FirstTicket: TBytes;
    function ReadAll(aConnection: TQuicConnection; aId: QWord): String;
    procedure StartResumed(aServerMaxData: QWord = 0);
    function OpenAndWrite(const aText: String): QWord;
    function ZeroRttDatagram(const aPayload: TBytes; aPn: QWord): TBytes;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTicketReceived;
    procedure TestEarlyDataAccepted;
    procedure TestAnswerInFirstFlight;
    procedure TestEarlyDataRejected;
    procedure TestEarlyDataBeforeInitial;
    procedure TestEarlyDataDroppedWhenRejected;
    procedure TestForbiddenEarlyFrame;
    procedure TestEarlyPacketToClient;
    procedure TestInvalidRememberedParams;
    procedure TestNoEarlyDataWithoutTicket;
  end;

implementation

const
  EarlyText = 'request sent in 0-RTT';

procedure TTestQuicEarlyData.SetUp;

var
  lServer: TQuicTLSConfig;

begin
  NeedOpenSSL;
  lServer:=Default(TQuicTLSConfig);
  lServer.Alpn:=['hq-interop'];
  lServer.CertificateFile:=TestDataDir+'server.crt';
  lServer.PrivateKeyFile:=TestDataDir+'server.key';
  lServer.EnableEarlyData:=True;
  FContext:=TQuicOpenSSLContext.Create(True,lServer);
  FZeroRttPackets:=0;
end;


procedure TTestQuicEarlyData.TearDown;

begin
  FreeAndNil(FNet);
  FreeAndNil(FContext);
end;


function TTestQuicEarlyData.CountFilter(aNetwork: TTestNetwork; aFromClient: Boolean; var aData: TBytes): Boolean;

var
  lHeader: TQuicPacketHeader;
  lOffset: Integer;

begin
  Result:=True;
  if not aFromClient then
    Exit;
  lOffset:=0;
  while (lOffset<Length(aData))
        and QuicParseHeader(@aData[lOffset],Length(aData)-lOffset,QUIC_LOCAL_CID_LENGTH,lHeader) do
    begin
    if lHeader.PacketType=qptZeroRtt then
      Inc(FZeroRttPackets);
    Inc(lOffset,lHeader.PacketLength);
    end;
end;


function TTestQuicEarlyData.NewNetwork: TTestNetwork;

begin
  Result:=TTestNetwork.Create;
  Result.ClientTLS.EnableEarlyData:=True;
  Result.SharedServerContext:=FContext;
  Result.Filter:=@CountFilter;
end;


function TTestQuicEarlyData.FirstTicket: TBytes;

var
  lNet: TTestNetwork;

begin
  lNet:=NewNetwork;
  try
    lNet.Handshake;
    AssertTrue('Client handshake confirmed',lNet.Client.Connection.HandshakeConfirmed);
    lNet.Run;
    AssertTrue('Tickets received',lNet.Client.TicketEvents>0);
    AssertFalse('No 0-RTT without a ticket',lNet.Client.Connection.EarlyDataAccepted);
    Result:=lNet.Client.Ticket;
  finally
    lNet.Free;
  end;
end;


function TTestQuicEarlyData.ReadAll(aConnection: TQuicConnection; aId: QWord): String;

var
  lBuf: array[0..4095] of Byte;
  lCount: Integer;

begin
  Result:='';
  repeat
    lCount:=aConnection.ReadStream(aId,@lBuf[0],SizeOf(lBuf));
    if lCount>0 then
      Result:=Result+TEncoding.UTF8.GetString(QuicBytesOf(@lBuf[0],lCount));
  until lCount=0;
end;


procedure TTestQuicEarlyData.StartResumed(aServerMaxData: QWord);

begin
  FNet:=NewNetwork;
  FNet.ClientTicket:=FirstTicket;
  if aServerMaxData>0 then
    FNet.ServerParams.InitialMaxData:=aServerMaxData;
  FNet.Start;
end;


function TTestQuicEarlyData.OpenAndWrite(const aText: String): QWord;

begin
  AssertTrue('Client can send 0-RTT data',FNet.Client.Connection.CanSendEarly);
  AssertTrue('Stream opened before the handshake',FNet.Client.Connection.OpenStream(True,Result));
  AssertTrue('Write before the handshake',
    FNet.Client.Connection.WriteStream(Result,TEncoding.UTF8.GetBytes(aText),True));
end;


function TTestQuicEarlyData.ZeroRttDatagram(const aPayload: TBytes; aPn: QWord): TBytes;

var
  lClient: TQuicConnection;
  lWriter: TQuicWriter;
  lPnOffset: Integer;

begin
  lClient:=FNet.Client.Connection;
  lWriter.Init;
  lPnOffset:=QuicWriteLongHeader(lWriter,qptZeroRtt,lClient.DestinationCid,lClient.LocalCid,nil,aPn,4,
    Length(aPayload)+QUIC_AEAD_TAG_LENGTH);
  lWriter.WriteBytes(aPayload);
  Result:=QuicProtectPacket(lClient.EarlyKeys,aPn,lWriter.ToBytes,lPnOffset,4);
end;


procedure TTestQuicEarlyData.TestTicketReceived;

var
  lTicket: TBytes;

begin
  lTicket:=FirstTicket;
  AssertTrue('Ticket with session and parameters',Length(lTicket)>100);
end;


procedure TTestQuicEarlyData.TestEarlyDataAccepted;

var
  lId, lServerId: QWord;
  lServer: TQuicConnection;

begin
  StartResumed;
  lId:=OpenAndWrite(EarlyText);
  FNet.Step;
  lServer:=FNet.Server.Connection;
  AssertTrue('Client sent 0-RTT packets',FZeroRttPackets>0);
  AssertFalse('Server handshake not complete yet',lServer.HandshakeComplete);
  AssertTrue('Server sees the early stream',lServer.AcceptStream(lServerId));
  AssertTrue('Stream marked as early',lServer.StreamIsEarly(lServerId));
  AssertEquals('Early request',EarlyText,ReadAll(lServer,lServerId));
  AssertTrue('Server answers before the handshake completes',
    lServer.WriteStream(lServerId,TEncoding.UTF8.GetBytes('answer'),True));
  FNet.Run;
  AssertTrue('Server accepted 0-RTT',lServer.EarlyDataAccepted);
  AssertTrue('Client sees 0-RTT accepted',FNet.Client.Connection.EarlyDataAccepted);
  AssertFalse('No early keys after the handshake',FNet.Client.Connection.CanSendEarly);
  AssertEquals('Answer','answer',ReadAll(FNet.Client.Connection,lId));
  AssertFalse('Unknown stream is not early',lServer.StreamIsEarly(1000));
end;


procedure TTestQuicEarlyData.TestAnswerInFirstFlight;

var
  lId, lServerId: QWord;
  lServer: TQuicConnection;
  lData: TBytes;
  lHeader: TQuicPacketHeader;

begin
  StartResumed;
  lId:=OpenAndWrite(EarlyText);
  FNet.Client.Collect(FNet.Now);
  for lData in FNet.Client.Outbox do
    begin
    if FNet.Server.Connection=nil then
      begin
      QuicParseHeader(PByte(lData),Length(lData),QUIC_LOCAL_CID_LENGTH,lHeader);
      FNet.Server.Connection:=TQuicConnection.CreateServer(FNet.ServerConfigForTest,lHeader.DestCid,FNet.Now);
      end;
    FNet.Server.Connection.ReceiveDatagram(lData,FNet.Now);
    end;
  FNet.Client.Outbox:=nil;
  lServer:=FNet.Server.Connection;
  AssertTrue('Server sees the early stream',lServer.AcceptStream(lServerId));
  AssertEquals('Early request',EarlyText,ReadAll(lServer,lServerId));
  AssertTrue('First write',lServer.WriteStream(lServerId,TEncoding.UTF8.GetBytes('first part '),False));
  AssertTrue('Second write',lServer.WriteStream(lServerId,TEncoding.UTF8.GetBytes('second part'),True));
  FNet.Run;
  AssertEquals('Answer','first part second part',ReadAll(FNet.Client.Connection,lId));
end;


procedure TTestQuicEarlyData.TestEarlyDataRejected;

var
  lId, lServerId: QWord;
  lServer: TQuicConnection;

begin
  StartResumed(50000);
  lId:=OpenAndWrite(EarlyText);
  FNet.Run;
  lServer:=FNet.Server.Connection;
  AssertTrue('Client sent 0-RTT packets',FZeroRttPackets>0);
  AssertTrue('Handshake complete',lServer.HandshakeComplete);
  AssertFalse('Server rejected 0-RTT',lServer.EarlyDataAccepted);
  AssertFalse('Client sees 0-RTT rejected',FNet.Client.Connection.EarlyDataAccepted);
  AssertTrue('Stream arrives in 1-RTT',lServer.AcceptStream(lServerId));
  AssertEquals('Same stream',lId,lServerId);
  AssertFalse('Stream not early',lServer.StreamIsEarly(lServerId));
  AssertEquals('Data resent after the handshake',EarlyText,ReadAll(lServer,lServerId));
end;


procedure TTestQuicEarlyData.TestEarlyDataBeforeInitial;

var
  lServerId: QWord;
  lOutbox: array of TBytes;
  lServer: TQuicConnection;
  I, lLast: Integer;

begin
  StartResumed;
  OpenAndWrite(EarlyText);
  FNet.Client.Collect(FNet.Now);
  lOutbox:=FNet.Client.Outbox;
  FNet.Client.Outbox:=nil;
  lLast:=High(lOutbox);
  for I:=1 to 20 do
    Insert(lOutbox[lLast],FNet.Client.Outbox,Length(FNet.Client.Outbox));
  for I:=lLast-1 downto 0 do
    Insert(lOutbox[I],FNet.Client.Outbox,Length(FNet.Client.Outbox));
  FNet.Run;
  lServer:=FNet.Server.Connection;
  AssertTrue('Server accepted 0-RTT',lServer.EarlyDataAccepted);
  AssertTrue('Early stream',lServer.AcceptStream(lServerId));
  AssertTrue('Stream marked as early',lServer.StreamIsEarly(lServerId));
  AssertEquals('Buffered 0-RTT data delivered',EarlyText,ReadAll(lServer,lServerId));
end;


procedure TTestQuicEarlyData.TestEarlyDataDroppedWhenRejected;

var
  lServerId: QWord;
  lOutbox: array of TBytes;
  lServer: TQuicConnection;
  I: Integer;

begin
  StartResumed(50000);
  OpenAndWrite(EarlyText);
  FNet.Client.Collect(FNet.Now);
  lOutbox:=FNet.Client.Outbox;
  FNet.Client.Outbox:=nil;
  for I:=High(lOutbox) downto 0 do
    Insert(lOutbox[I],FNet.Client.Outbox,Length(FNet.Client.Outbox));
  FNet.Run;
  lServer:=FNet.Server.Connection;
  AssertTrue('Handshake complete',lServer.HandshakeComplete);
  AssertFalse('Server rejected 0-RTT',lServer.EarlyDataAccepted);
  AssertTrue('Stream arrives in 1-RTT',lServer.AcceptStream(lServerId));
  AssertEquals('Data resent after the handshake',EarlyText,ReadAll(lServer,lServerId));
end;


procedure TTestQuicEarlyData.TestForbiddenEarlyFrame;

var
  lWriter: TQuicWriter;
  lDatagram: TBytes;

begin
  StartResumed;
  OpenAndWrite(EarlyText);
  lWriter.Init;
  QuicEncodeFrame(lWriter,QuicFrame(QUIC_FRAME_HANDSHAKE_DONE));
  lWriter.WriteZeros(20);
  lDatagram:=ZeroRttDatagram(lWriter.ToBytes,100);
  FNet.Step;
  AssertFalse('Server handshake not complete yet',FNet.Server.Connection.HandshakeComplete);
  FNet.Server.Connection.ReceiveDatagram(lDatagram,FNet.Now);
  AssertTrue('Server closes',FNet.Server.Connection.State=qcsClosing);
  AssertEquals('PROTOCOL_VIOLATION',QUIC_PROTOCOL_VIOLATION,FNet.Server.Connection.CloseErrorCode);
end;


procedure TTestQuicEarlyData.TestEarlyPacketToClient;

var
  lWriter: TQuicWriter;

begin
  StartResumed;
  OpenAndWrite(EarlyText);
  lWriter.Init;
  QuicEncodeFrame(lWriter,QuicFrame(QUIC_FRAME_PING));
  lWriter.WriteZeros(20);
  FNet.Client.Connection.ReceiveDatagram(ZeroRttDatagram(lWriter.ToBytes,100),FNet.Now);
  AssertTrue('Client ignores 0-RTT packets',FNet.Client.Connection.State=qcsHandshake);
  lWriter.Init;
  lWriter.WriteByte($C0);
  lWriter.WriteUInt32($0A0A0A0A);
  lWriter.WriteByte(QUIC_LOCAL_CID_LENGTH);
  lWriter.WriteBytes(FNet.Client.Connection.LocalCid);
  lWriter.WriteByte(0);
  lWriter.WriteZeros(30);
  FNet.Client.Connection.ReceiveDatagram(lWriter.ToBytes,FNet.Now);
  AssertTrue('Client ignores packets of other versions',FNet.Client.Connection.State=qcsHandshake);
  FNet.Run;
  AssertTrue('Handshake completes',FNet.Server.Connection.HandshakeComplete);
end;


procedure TTestQuicEarlyData.TestInvalidRememberedParams;

var
  lReader: TQuicReader;
  lWriter: TQuicWriter;
  lId: QWord;
  lTicket: TBytes;

begin
  FNet:=NewNetwork;
  lTicket:=FirstTicket;
  lReader.InitBytes(lTicket);
  lWriter.Init;
  lWriter.WriteVarBytes(lReader.ReadVarBytes);
  lWriter.WriteVarBytes(QuicHexToBytes('040101040101'));
  FNet.ClientTicket:=lWriter.ToBytes;
  FNet.Start;
  AssertTrue('0-RTT keys from the session',FNet.Client.Connection.CanSendEarly);
  AssertEquals('Remembered parameters ignored',0,FNet.Client.Connection.PeerParams.InitialMaxData);
  AssertFalse('No stream credit without valid parameters',FNet.Client.Connection.OpenStream(True,lId));
  FNet.Run;
  AssertTrue('Handshake completes',FNet.Server.Connection.HandshakeComplete);
  AssertTrue('Streams open after the handshake',FNet.Client.Connection.OpenStream(True,lId));
end;


procedure TTestQuicEarlyData.TestNoEarlyDataWithoutTicket;

var
  lId: QWord;

begin
  FNet:=NewNetwork;
  FNet.ClientTicket:=QuicHexToBytes('0102');
  FNet.Start;
  AssertFalse('No 0-RTT with an invalid ticket',FNet.Client.Connection.CanSendEarly);
  AssertFalse('No stream before the handshake',FNet.Client.Connection.OpenStream(True,lId));
  FNet.Run;
  AssertTrue('Full handshake',FNet.Server.Connection.HandshakeComplete);
end;


initialization
  RegisterTest(TTestQuicEarlyData);
end.
