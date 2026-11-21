{
  This file is part of the Free Component Library.
  Tests for FpQuic.Connection over the in-memory test network.
  See the file COPYING.FPC, included in this distribution, for details about the copyright.
}
unit utquicconnection;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpcunit, testregistry, FpQuic.Consts, FpQuic.Buffers, FpQuic.Frames, FpQuic.Packets,
  FpQuic.TransportParams, FpQuic.Crypto, FpQuic.Streams, FpQuic.Recovery, FpQuic.Connection, utquichelpers,
  utquicnetwork;

type
  TTestQuicConnection = class(TTestCase)
  private
    FNet: TTestNetwork;
    FDropClient: Integer;
    FDropServer: Integer;
    FDropEvery: Integer;
    FCounter: Integer;
    FCaptured: TBytes;
    FCapture: Boolean;
    FServerDatagrams: Integer;
    FServerBytes: Integer;
    FClientBytes: Integer;
    FDropAfterServerComplete: Boolean;
    FDropServerInitials: Integer;
    FDropClientIndex: Integer;
    FDropClientLast: Integer;
    FClientDatagrams: Integer;
    FDropServerAcks: Integer;
    FDropServerLarge: Integer;
    function ReadAll(aConnection: TQuicConnection; aId: QWord): String;
    function DropFilter(aNetwork: TTestNetwork; aFromClient: Boolean; var aData: TBytes): Boolean;
    function Encode(const aFrames: array of TQuicFrame): TBytes;
    procedure Inject(aFromServer: Boolean; aLevel: TQuicEncryptionLevel; const aPayload: TBytes;
      aFirstByteBits: Byte = 0; aPad: Integer = 0);
    procedure InjectFrames(aFromServer: Boolean; const aFrames: array of TQuicFrame);
    procedure ExpectClose(aConnection: TQuicConnection; aCode: QWord);
    procedure WriteString(aConnection: TQuicConnection; aId: QWord; const aText: String; aFin: Boolean);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHandshake;
    procedure TestStreamEcho;
    procedure TestHandshakeWithLoss;
    procedure TestServerInitialLossWithFullWindow;
    procedure TestLostSecondClientInitial;
    procedure TestTwoProbesPerTimeout;
    procedure TestServerAnswersClientProbe;
    procedure TestLostHandshakeDone;
    procedure TestLargeTransferWithLoss;
    procedure TestManyStreams;
    procedure TestNewStreamsInOrder;
    procedure TestUnidirectionalStreams;
    procedure TestResetStream;
    procedure TestStopSending;
    procedure TestApplicationClose;
    procedure TestCloseDuringHandshake;
    procedure TestClosingResendsClose;
    procedure TestIdleTimeout;
    procedure TestKeyUpdate;
    procedure TestRetry;
    procedure TestVersionNegotiation;
    procedure TestVersionNegotiationIgnored;
    procedure TestStatelessReset;
    procedure TestAmplificationLimit;
    procedure TestApiEdgeCases;
    procedure TestDuplicateAndGarbage;
    procedure TestSmallClientInitial;
    procedure TestAlpnMismatch;
    procedure TestHandshakeDoneFromClient;
    procedure TestNewTokenFromClient;
    procedure TestNewTokenFromServer;
    procedure TestRetireConnectionId;
    procedure TestLostNewConnectionId;
    procedure TestEmptyPacket;
    procedure TestReservedBits;
    procedure TestFrameNotAllowed;
    procedure TestUnknownFrame;
    procedure TestConnectionFlowControl;
    procedure TestResetFlowControl;
    procedure TestCryptoBufferExceeded;
    procedure TestAckOfUnsentPacket;
    procedure TestNewConnectionId;
    procedure TestConnectionIdLimit;
    procedure TestMiscFrames;
    procedure TestLostMaxData;
    procedure TestHandshakeAckValidatesAddress;
    procedure TestResetTokenOfUnusedCid;
    procedure TestHugeIdleTimeout;
    procedure TestIdleTimeoutFloor;
    procedure TestEventException;
    procedure TestAbort;
    procedure TestAutomaticKeyUpdate;
    procedure TestConfidentialityLimit;
    procedure TestIntegrityLimit;
  end;

implementation

const
  ServerResetToken = '00112233445566778899aabbccddeeff';

procedure TTestQuicConnection.SetUp;

begin
  FNet:=TTestNetwork.Create;
  FNet.Filter:=@DropFilter;
  FDropClient:=0;
  FDropServer:=0;
  FDropEvery:=0;
  FCounter:=0;
  FCapture:=False;
  FCaptured:=nil;
  FServerDatagrams:=0;
  FServerBytes:=0;
  FClientBytes:=0;
  FDropAfterServerComplete:=False;
  FDropServerInitials:=0;
  FDropClientIndex:=0;
  FDropClientLast:=0;
  FClientDatagrams:=0;
  FDropServerAcks:=0;
  FDropServerLarge:=0;
end;


procedure TTestQuicConnection.TearDown;

begin
  FNet.Free;
end;


function TTestQuicConnection.DropFilter(aNetwork: TTestNetwork; aFromClient: Boolean; var aData: TBytes): Boolean;

var
  lHeader: TQuicPacketHeader;

begin
  Result:=True;
  Inc(FCounter);
  if not aFromClient then
    begin
    Inc(FServerDatagrams);
    Inc(FServerBytes,Length(aData));
    end;
  if (not aFromClient) and (FDropServerLarge>0) and (Length(aData)>=QUIC_MIN_INITIAL_DATAGRAM) then
    begin
    Dec(FDropServerLarge);
    Exit(False);
    end;
  if (not aFromClient) and (FDropServerAcks>0) and (aData[0] and $80<>0) and (Length(aData)<QUIC_MIN_INITIAL_DATAGRAM) then
    begin
    Dec(FDropServerAcks);
    Exit(False);
    end;
  if aFromClient then
    begin
    Inc(FClientDatagrams);
    if (FClientDatagrams=FDropClientIndex) or ((FClientDatagrams>FDropClientIndex)
       and (FClientDatagrams<=FDropClientLast)) then
      Exit(False);
    end;
  if FCapture and aFromClient then
    begin
    FCapture:=False;
    FCaptured:=aData;
    Exit(False);
    end;
  if aFromClient and (FDropClient>0) then
    begin
    Dec(FDropClient);
    Exit(False);
    end;
  if (not aFromClient) and (FDropServer>0) then
    begin
    Dec(FDropServer);
    Exit(False);
    end;
  if (not aFromClient) and (FDropServerInitials>0) and (Length(aData)>=QUIC_MIN_INITIAL_DATAGRAM)
     and QuicParseHeader(PByte(aData),Length(aData),QUIC_LOCAL_CID_LENGTH,lHeader)
     and (lHeader.PacketType=qptInitial) and (lHeader.PacketLength=Length(aData)) then
    begin
    Dec(FDropServerInitials);
    Exit(False);
    end;
  if FDropAfterServerComplete and (not aFromClient) and aNetwork.Server.Connection.HandshakeComplete then
    begin
    FDropAfterServerComplete:=False;
    Exit(False);
    end;
  if (FDropEvery>0) and (FCounter mod FDropEvery=0) then
    Exit(False);
  if aFromClient then
    Inc(FClientBytes,Length(aData));
end;


function TTestQuicConnection.ReadAll(aConnection: TQuicConnection; aId: QWord): String;

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


procedure TTestQuicConnection.WriteString(aConnection: TQuicConnection; aId: QWord; const aText: String;
  aFin: Boolean);

begin
  AssertTrue('Write on stream '+IntToStr(aId),aConnection.WriteStream(aId,TEncoding.UTF8.GetBytes(aText),aFin));
end;


function TTestQuicConnection.Encode(const aFrames: array of TQuicFrame): TBytes;

var
  lWriter: TQuicWriter;
  lFrame: TQuicFrame;

begin
  lWriter.Init;
  for lFrame in aFrames do
    QuicEncodeFrame(lWriter,lFrame);
  Result:=lWriter.ToBytes;
end;


procedure TTestQuicConnection.Inject(aFromServer: Boolean; aLevel: TQuicEncryptionLevel; const aPayload: TBytes;
  aFirstByteBits: Byte; aPad: Integer);

var
  lSender, lReceiver: TQuicConnection;
  lSpace: TQuicSpace;
  lWriter: TQuicWriter;
  lPnOffset: Integer;
  lPlain: TBytes;
  lSent: TQuicSentPacket;

begin
  if aFromServer then
    begin
    lSender:=FNet.Server.Connection;
    lReceiver:=FNet.Client.Connection;
    end
  else
    begin
    lSender:=FNet.Client.Connection;
    lReceiver:=FNet.Server.Connection;
    end;
  lSpace:=lSender.Spaces[aLevel];
  lWriter.Init;
  if aLevel=qelApplication then
    lPnOffset:=QuicWriteShortHeader(lWriter,lReceiver.LocalCid,lSender.KeyPhase,lSpace.NextPn,4)
  else if aLevel=qelInitial then
    lPnOffset:=QuicWriteLongHeader(lWriter,qptInitial,lReceiver.LocalCid,lSender.LocalCid,nil,lSpace.NextPn,4,
      Length(aPayload)+aPad+16)
  else
    lPnOffset:=QuicWriteLongHeader(lWriter,qptHandshake,lReceiver.LocalCid,lSender.LocalCid,nil,lSpace.NextPn,4,
      Length(aPayload)+aPad+16);
  lWriter.WriteBytes(aPayload);
  lWriter.WriteZeros(aPad);
  lPlain:=lWriter.ToBytes;
  lPlain[0]:=lPlain[0] xor aFirstByteBits;
  lPlain:=QuicProtectPacket(lSpace.Tx,lSpace.NextPn,lPlain,lPnOffset,4);
  lSent:=TQuicSentPacket.Create;
  lSent.PacketNumber:=lSpace.NextPn;
  lSender.Recovery.OnPacketSent(aLevel,lSent,FNet.Now);
  Inc(lSpace.NextPn);
  lReceiver.ReceiveDatagram(lPlain,FNet.Now);
end;


procedure TTestQuicConnection.InjectFrames(aFromServer: Boolean; const aFrames: array of TQuicFrame);

begin
  Inject(aFromServer,qelApplication,Encode(aFrames));
end;


procedure TTestQuicConnection.ExpectClose(aConnection: TQuicConnection; aCode: QWord);

begin
  AssertTrue('Connection closed locally',(aConnection.State in [qcsClosing,qcsClosed]) and not aConnection.ClosedByPeer);
  AssertEquals('Close error code',aCode,aConnection.CloseErrorCode);
end;


procedure TTestQuicConnection.TestHandshake;

begin
  FNet.Handshake;
  AssertTrue('Client complete',FNet.Client.Connection.HandshakeComplete);
  AssertTrue('Client confirmed',FNet.Client.Connection.HandshakeConfirmed);
  AssertTrue('Server complete',FNet.Server.Connection.HandshakeComplete);
  AssertTrue('Client established',FNet.Client.Connection.State=qcsEstablished);
  AssertFalse('Client is not a server',FNet.Client.Connection.IsServer);
  AssertTrue('Server is a server',FNet.Server.Connection.IsServer);
  AssertEquals('ALPN','hq-interop',FNet.Client.Connection.Alpn);
  AssertEquals('Client handshake event',1,FNet.Client.HandshakeEvents);
  AssertEquals('Server handshake event',1,FNet.Server.HandshakeEvents);
  AssertTrue('Server knows the original DCID',
    QuicBytesEqual(FNet.Client.Connection.OriginalDcid,FNet.Server.Connection.OriginalDcid));
  AssertTrue('Client sends to the server CID',
    QuicBytesEqual(FNet.Client.Connection.DestinationCid,FNet.Server.Connection.LocalCid));
  AssertEquals('Peer params',10,FNet.Client.Connection.PeerParams.InitialMaxStreamsBidi);
  AssertTrue('RTT measured',FNet.Client.Connection.Recovery.Rtt.HasSample);
  AssertTrue('Initial discarded',FNet.Client.Connection.Spaces[qelInitial].Discarded);
  AssertTrue('Handshake discarded',FNet.Server.Connection.Spaces[qelHandshake].Discarded);
  AssertTrue('Congestion window',FNet.Client.Connection.CongestionControl.CongestionWindow>0);
  AssertNotNull('Streams',FNet.Client.Connection.Streams);
end;


procedure TTestQuicConnection.TestStreamEcho;

var
  lId, lServerId: QWord;
  lFrame: TQuicFrame;

begin
  FNet.Handshake;
  AssertTrue('Open stream',FNet.Client.Connection.OpenStream(True,lId));
  WriteString(FNet.Client.Connection,lId,'hello',True);
  AssertEquals('Buffered',5,FNet.Client.Connection.StreamBuffered(lId));
  FNet.Run;
  AssertTrue('Server accepts',FNet.Server.Connection.AcceptStream(lServerId));
  AssertEquals('Stream id',lId,lServerId);
  AssertTrue('Data event',FNet.Server.DataEvents>0);
  AssertEquals('Received','hello',ReadAll(FNet.Server.Connection,lServerId));
  AssertTrue('Finished',FNet.Server.Connection.StreamFinished(lServerId));
  AssertFalse('Write after fin',FNet.Client.Connection.WriteStream(lId,nil,False));
  WriteString(FNet.Server.Connection,lServerId,'world',True);
  FNet.Run;
  AssertEquals('Echo','world',ReadAll(FNet.Client.Connection,lId));
  FNet.Run;
  AssertEquals('Streams closed on both sides',0,FNet.Client.Connection.Streams.Count+FNet.Server.Connection.Streams.Count);
  lFrame:=QuicFrame(QUIC_FRAME_STREAM);
  lFrame.StreamId:=lId;
  lFrame.Data:=QuicHexToBytes('01');
  InjectFrames(False,[lFrame]);
  AssertTrue('Frame for a closed stream ignored',FNet.Server.Connection.State=qcsEstablished);
end;


procedure TTestQuicConnection.TestHandshakeWithLoss;

begin
  FDropClient:=1;
  FDropServer:=2;
  FNet.Handshake;
  AssertTrue('Client confirmed despite loss',FNet.Client.Connection.HandshakeConfirmed);
  AssertTrue('Server complete despite loss',FNet.Server.Connection.HandshakeComplete);
  AssertTrue('Datagrams dropped',FNet.Dropped>=3);
end;


procedure TTestQuicConnection.TestServerInitialLossWithFullWindow;

var
  lRounds: Integer;

begin
  FDropServerInitials:=5;
  FNet.Start;
  lRounds:=0;
  while ((FNet.Server.Connection=nil) or not (FNet.Client.Connection.HandshakeConfirmed
         and FNet.Server.Connection.HandshakeComplete)) and (lRounds<500) do
    begin
    if not FNet.Step then
      FNet.FireTimer(High(QWord) div 2);
    Inc(lRounds);
    end;
  AssertTrue('Client confirmed after repeated Initial loss',FNet.Client.Connection.HandshakeConfirmed);
  AssertTrue('Server complete after repeated Initial loss',FNet.Server.Connection.HandshakeComplete);
  AssertEquals('Five server Initial datagrams dropped',0,FDropServerInitials);
end;


procedure TTestQuicConnection.TestLostSecondClientInitial;

var
  lRounds: Integer;
  lStart: QWord;

begin
  FDropClientIndex:=2;
  FDropServerAcks:=10;
  lStart:=FNet.Now;
  FNet.Start;
  lRounds:=0;
  while ((FNet.Server.Connection=nil) or not (FNet.Client.Connection.HandshakeConfirmed
         and FNet.Server.Connection.HandshakeComplete)) and (lRounds<500) do
    begin
    if not FNet.Step then
      FNet.FireTimer(High(QWord) div 2);
    Inc(lRounds);
    end;
  AssertTrue('Client confirmed after losing the end of its ClientHello',FNet.Client.Connection.HandshakeConfirmed);
  AssertTrue('Server complete after losing the end of the ClientHello',FNet.Server.Connection.HandshakeComplete);
  AssertTrue('Server acknowledgements dropped',FDropServerAcks<10);
  AssertTrue('First probe resends the whole ClientHello',FNet.Now-lStart<3000000);
end;


procedure TTestQuicConnection.TestTwoProbesPerTimeout;

var
  lRounds, lTimeouts: Integer;

begin
  FDropClientIndex:=2;
  FDropClientLast:=7;
  FNet.Start;
  lRounds:=0;
  lTimeouts:=0;
  while ((FNet.Server.Connection=nil) or not (FNet.Client.Connection.HandshakeConfirmed
         and FNet.Server.Connection.HandshakeComplete)) and (lRounds<500) do
    begin
    if not FNet.Step then
      begin
      FNet.FireTimer(High(QWord) div 2);
      Inc(lTimeouts);
      end;
    Inc(lRounds);
    end;
  AssertTrue('Client confirmed after six lost datagrams',FNet.Client.Connection.HandshakeConfirmed);
  AssertTrue('Two probe datagrams per timeout',lTimeouts<=3);
end;


procedure TTestQuicConnection.TestServerAnswersClientProbe;

begin
  FDropServerLarge:=2;
  FNet.Start;
  FNet.Step;
  AssertEquals('Server flight dropped',0,FDropServerLarge);
  AssertTrue('Server flight unacknowledged',FNet.Server.Connection.Recovery.OldestUnacked(qelInitial)<>nil);
  Inject(False,qelInitial,Encode([QuicFrame(QUIC_FRAME_PING)]),0,QUIC_MIN_INITIAL_DATAGRAM);
  FNet.Server.Collect(FNet.Now);
  AssertTrue('Server answered the probe',Length(FNet.Server.Outbox)>0);
  AssertTrue('Answer contains the server flight',Length(FNet.Server.Outbox[0])>=QUIC_MIN_INITIAL_DATAGRAM);
end;


procedure TTestQuicConnection.TestLostHandshakeDone;

begin
  FDropAfterServerComplete:=True;
  FNet.Handshake;
  FNet.Run;
  AssertTrue('Client confirmed after retransmission',FNet.Client.Connection.HandshakeConfirmed);
end;


procedure TTestQuicConnection.TestLargeTransferWithLoss;

const
  Size = 300000;

var
  lId, lServerId: QWord;
  lData: TBytes;
  lBuf: array[0..65535] of Byte;
  lReceived, lCount, lRounds: Integer;
  lOk: Boolean;
  I: Integer;

begin
  FNet.Handshake;
  lData:=nil;
  SetLength(lData,Size);
  for I:=0 to Size-1 do
    lData[I]:=I mod 251;
  FDropEvery:=7;
  AssertTrue('Open stream',FNet.Client.Connection.OpenStream(True,lId));
  AssertTrue('Write',FNet.Client.Connection.WriteStream(lId,lData,True));
  lReceived:=0;
  lOk:=True;
  lRounds:=0;
  lServerId:=0;
  while (lReceived<Size) and (lRounds<2000) do
    begin
    FNet.Run(20);
    FNet.Server.Connection.AcceptStream(lServerId);
    repeat
      lCount:=FNet.Server.Connection.ReadStream(lId,@lBuf[0],SizeOf(lBuf));
      for I:=0 to lCount-1 do
        lOk:=lOk and (lBuf[I]=(lReceived+I) mod 251);
      Inc(lReceived,lCount);
    until lCount=0;
    Inc(lRounds);
    end;
  AssertEquals('All data received',Size,lReceived);
  AssertTrue('Data intact',lOk);
  AssertTrue('Fin received',FNet.Server.Connection.StreamFinished(lId));
  AssertTrue('Losses happened',FNet.Dropped>10);
end;


procedure TTestQuicConnection.TestManyStreams;

var
  lIds: array[0..9] of QWord;
  lId, lExtra: QWord;
  I: Integer;

begin
  FNet.Handshake;
  for I:=0 to 9 do
    begin
    AssertTrue('Open '+IntToStr(I),FNet.Client.Connection.OpenStream(True,lIds[I]));
    WriteString(FNet.Client.Connection,lIds[I],'ping'+IntToStr(I),True);
    end;
  AssertFalse('Stream limit',FNet.Client.Connection.OpenStream(True,lExtra));
  FNet.Run;
  for I:=0 to 9 do
    begin
    AssertTrue('Accept '+IntToStr(I),FNet.Server.Connection.AcceptStream(lId));
    AssertEquals('Data '+IntToStr(I),'ping'+IntToStr(I),ReadAll(FNet.Server.Connection,lId));
    WriteString(FNet.Server.Connection,lId,'pong',True);
    end;
  FNet.Step;
  FDropServer:=1;
  FNet.Run;
  for I:=0 to 9 do
    AssertEquals('Reply '+IntToStr(I),'pong',ReadAll(FNet.Client.Connection,lIds[I]));
  FNet.Run;
  AssertTrue('New credit after closing',FNet.Client.Connection.OpenStream(True,lExtra));
  AssertEquals('Next stream id',40,lExtra);
end;


procedure TTestQuicConnection.TestNewStreamsInOrder;

var
  lIds: array[0..6] of QWord;
  lId: QWord;
  I: Integer;

begin
  FNet.Handshake;
  for I:=0 to 3 do
    begin
    FNet.Client.Connection.OpenStream(True,lIds[I]);
    WriteString(FNet.Client.Connection,lIds[I],'ping',True);
    end;
  FNet.Run;
  while FNet.Server.Connection.AcceptStream(lId) do
    begin
    ReadAll(FNet.Server.Connection,lId);
    WriteString(FNet.Server.Connection,lId,'pong',True);
    end;
  FNet.Run;
  for I:=0 to 1 do
    AssertEquals('Reply '+IntToStr(I),'pong',ReadAll(FNet.Client.Connection,lIds[I]));
  FNet.Run;
  for I:=4 to 6 do
    begin
    FNet.Client.Connection.OpenStream(True,lIds[I]);
    WriteString(FNet.Client.Connection,lIds[I],StringOfChar('a',1100),True);
    end;
  FNet.Client.Collect(FNet.Now);
  FNet.Server.Connection.ReceiveDatagram(FNet.Client.Outbox[0],FNet.Now);
  AssertEquals('First datagram has all data of the lowest new stream',1100,
    Length(ReadAll(FNet.Server.Connection,lIds[4])));
end;


procedure TTestQuicConnection.TestUnidirectionalStreams;

var
  lId, lServerId, lClientId: QWord;

begin
  FNet.Handshake;
  AssertTrue('Open uni',FNet.Client.Connection.OpenStream(False,lId));
  AssertEquals('Client uni id',2,lId);
  WriteString(FNet.Client.Connection,lId,'up',True);
  AssertTrue('Server uni',FNet.Server.Connection.OpenStream(False,lServerId));
  AssertEquals('Server uni id',3,lServerId);
  WriteString(FNet.Server.Connection,lServerId,'down',True);
  FNet.Run;
  AssertTrue('Server accepts uni',FNet.Server.Connection.AcceptStream(lClientId));
  AssertEquals('Up','up',ReadAll(FNet.Server.Connection,lClientId));
  AssertFalse('Cannot write on a receive-only stream',FNet.Server.Connection.WriteStream(lClientId,nil,True));
  AssertTrue('Client accepts uni',FNet.Client.Connection.AcceptStream(lClientId));
  AssertEquals('Down','down',ReadAll(FNet.Client.Connection,lClientId));
  FNet.Run;
  AssertEquals('Uni streams closed',0,FNet.Server.Connection.Streams.Count);
  AssertTrue('Uni credit',FNet.Client.Connection.Streams.PeerMaxStreams(False)>10);
end;


procedure TTestQuicConnection.TestResetStream;

var
  lId, lServerId, lCode: QWord;

begin
  FNet.Handshake;
  FNet.Client.Connection.OpenStream(True,lId);
  WriteString(FNet.Client.Connection,lId,'partial',False);
  FNet.Run;
  FNet.Server.Connection.AcceptStream(lServerId);
  AssertFalse('Not reset yet',FNet.Server.Connection.StreamReset(lServerId,lCode));
  FNet.Client.Connection.ResetStream(lId,77);
  FDropClient:=1;
  FNet.Run;
  AssertTrue('Reset received',FNet.Server.Connection.StreamReset(lServerId,lCode));
  AssertEquals('Reset code',77,lCode);
  AssertFalse('No writes after reset',FNet.Client.Connection.WriteStream(lId,nil,True));
  WriteString(FNet.Server.Connection,lServerId,'',True);
  FNet.Run;
  ReadAll(FNet.Client.Connection,lId);
  FNet.Run;
  AssertNull('Client stream closed',FNet.Client.Connection.Streams.Find(lId));
end;


procedure TTestQuicConnection.TestStopSending;

var
  lId, lServerId, lCode: QWord;

begin
  FNet.Handshake;
  FNet.Client.Connection.OpenStream(True,lId);
  WriteString(FNet.Client.Connection,lId,'data',False);
  FNet.Run;
  FNet.Server.Connection.AcceptStream(lServerId);
  FNet.Server.Connection.StopSending(lServerId,88);
  FDropServer:=1;
  FNet.Run;
  AssertTrue('Client got STOP_SENDING',FNet.Client.Connection.Streams.Find(lId).StopSendingReceived);
  AssertTrue('Server sees the reset',FNet.Server.Connection.StreamReset(lServerId,lCode));
  AssertEquals('Reset code from STOP_SENDING',88,lCode);
  AssertNotNull('Server stream kept',FNet.Server.Connection.Streams.Find(lServerId));
  FNet.Server.Connection.ResetStream(lServerId,1);
  AssertTrue('Server reset pending',FNet.Server.Connection.Streams.Find(lServerId).ResetPending);
  FNet.Client.Connection.StopSending(lId,2);
  FNet.Run;
  AssertNotNull('Client stream kept',FNet.Client.Connection.Streams.Find(lId));
  AssertTrue('Client sees the server reset',FNet.Client.Connection.StreamReset(lId,lCode));
end;


procedure TTestQuicConnection.TestApplicationClose;

begin
  FNet.Handshake;
  FNet.Client.Connection.Close(5,'bye',FNet.Now);
  AssertTrue('Client closing',FNet.Client.Connection.State=qcsClosing);
  FNet.Client.Connection.Close(6,'again',FNet.Now);
  AssertEquals('First close wins',5,FNet.Client.Connection.CloseErrorCode);
  FNet.Step;
  AssertTrue('Server draining',FNet.Server.Connection.State=qcsDraining);
  AssertTrue('Closed by peer',FNet.Server.Connection.ClosedByPeer);
  AssertEquals('Error code',5,FNet.Server.Connection.CloseErrorCode);
  AssertEquals('Reason','bye',FNet.Server.Connection.CloseReason);
  AssertEquals('Close event',1,FNet.Server.CloseEvents);
  AssertTrue('Draining timer',FNet.Server.Connection.NextTimeout<High(QWord));
  FNet.FireTimer(High(QWord) div 2);
  FNet.FireTimer(High(QWord) div 2);
  AssertTrue('Client closed',FNet.Client.Connection.State=qcsClosed);
  AssertTrue('Server closed',FNet.Server.Connection.State=qcsClosed);
  AssertEquals('No timer when closed',High(QWord),FNet.Client.Connection.NextTimeout);
  FNet.Client.Connection.HandleTimeout(FNet.Now);
  FNet.Server.Connection.ReceiveDatagram(QuicHexToBytes('40'),FNet.Now);
  AssertFalse('Nothing to send when closed',FNet.Client.Connection.NextDatagram(FNet.Now,FCaptured));
end;


procedure TTestQuicConnection.TestCloseDuringHandshake;

begin
  FNet.Start;
  FNet.Step;
  FNet.Client.Connection.Close(9,'abort',FNet.Now);
  FNet.Run;
  AssertTrue('Server draining',FNet.Server.Connection.State=qcsDraining);
  AssertEquals('Application close becomes APPLICATION_ERROR',QUIC_APPLICATION_ERROR,
    FNet.Server.Connection.CloseErrorCode);
end;


procedure TTestQuicConnection.TestClosingResendsClose;

var
  lId: QWord;

begin
  FNet.Handshake;
  FNet.Server.Connection.OpenStream(True,lId);
  WriteString(FNet.Server.Connection,lId,'x',False);
  FNet.Client.Connection.Close(3,'',FNet.Now);
  FDropClient:=1;
  FNet.Step;
  AssertTrue('Server still established',FNet.Server.Connection.State=qcsEstablished);
  FNet.Step;
  FNet.Step;
  AssertTrue('Server draining after resent close',FNet.Server.Connection.State=qcsDraining);
  FNet.Server.Connection.ReceiveDatagram(QuicHexToBytes('40'),FNet.Now);
  AssertFalse('Draining sends nothing',FNet.Server.Connection.NextDatagram(FNet.Now,FCaptured));
  FNet.Server.Connection.Close(1,'',FNet.Now);
  AssertTrue('Draining stays draining',FNet.Server.Connection.State=qcsDraining);
end;


procedure TTestQuicConnection.TestIdleTimeout;

begin
  FNet.ServerParams.MaxIdleTimeout:=10000;
  FNet.Handshake;
  FNet.Run;
  FNet.FireTimer(60000000);
  FNet.FireTimer(60000000);
  AssertTrue('Client idle',FNet.Client.Connection.State=qcsClosed);
  AssertTrue('Server idle',FNet.Server.Connection.State=qcsClosed);
  AssertEquals('Reason','Idle timeout',FNet.Client.Connection.CloseReason);
  AssertTrue('Shorter peer idle timeout used',FNet.Now<FNet.Client.Connection.Recovery.Rtt.LatestRtt+15000000);
end;


procedure TTestQuicConnection.TestKeyUpdate;

var
  lId, lServerId: QWord;
  lOld: TBytes;

begin
  FNet.Handshake;
  AssertFalse('Server cannot update before an acknowledged 1-RTT packet',FNet.Server.Connection.InitiateKeyUpdate);
  FNet.Client.Connection.OpenStream(True,lId);
  WriteString(FNet.Client.Connection,lId,'a',False);
  FNet.Run;
  FNet.Server.Connection.AcceptStream(lServerId);
  FCapture:=True;
  WriteString(FNet.Client.Connection,lId,'b',False);
  FNet.Step;
  lOld:=FCaptured;
  AssertTrue('Client initiates',FNet.Client.Connection.InitiateKeyUpdate);
  AssertTrue('Client phase',FNet.Client.Connection.KeyPhase);
  AssertFalse('No second update before acknowledgement',FNet.Client.Connection.InitiateKeyUpdate);
  WriteString(FNet.Client.Connection,lId,'c',False);
  FNet.Run;
  AssertTrue('Server followed',FNet.Server.Connection.KeyPhase);
  FNet.Server.Connection.ReceiveDatagram(lOld,FNet.Now);
  FNet.Run;
  AssertEquals('Data across the update','abc',ReadAll(FNet.Server.Connection,lServerId));
  Inject(True,qelApplication,Encode([QuicFrame(QUIC_FRAME_PING)]),$04);
  AssertTrue('Undecryptable phase ignored',FNet.Client.Connection.State=qcsEstablished);
  WriteString(FNet.Server.Connection,lServerId,'d',False);
  FNet.Run;
  AssertTrue('Server initiates',FNet.Server.Connection.InitiateKeyUpdate);
  WriteString(FNet.Server.Connection,lServerId,'e',False);
  FNet.Run;
  AssertFalse('Client followed back',FNet.Client.Connection.KeyPhase);
  AssertEquals('Data after second update','de',ReadAll(FNet.Client.Connection,lId));
end;


procedure TTestQuicConnection.TestRetry;

var
  lData, lRetry, lNewCid: TBytes;
  lClient: TQuicConnection;

  procedure Deliver(const aToken: TBytes; aGoodTag: Boolean);

  var
    lTag: TBytes;

  begin
    lRetry:=QuicBuildRetry(lClient.LocalCid,lNewCid,aToken);
    lTag:=QuicRetryIntegrityTag(TestCipherFactory,lClient.OriginalDcid,lRetry);
    if not aGoodTag then
      lTag[0]:=lTag[0] xor 1;
    lClient.ReceiveDatagram(Concat(lRetry,lTag),FNet.Now);
  end;

begin
  FNet.Start;
  lClient:=FNet.Client.Connection;
  AssertTrue('First Initial',lClient.NextDatagram(FNet.Now,lData));
  lNewCid:=QuicHexToBytes('a1a2a3a4a5a6a7a8');
  Deliver(QuicHexToBytes('746f6b'),False);
  AssertFalse('Bad tag ignored',QuicBytesEqual(lClient.DestinationCid,lNewCid));
  Deliver(nil,True);
  AssertFalse('Empty token ignored',QuicBytesEqual(lClient.DestinationCid,lNewCid));
  Deliver(QuicHexToBytes('746f6b'),True);
  AssertTrue('Retry accepted',QuicBytesEqual(lClient.DestinationCid,lNewCid));
  lNewCid:=QuicHexToBytes('b1b2b3b4b5b6b7b8');
  Deliver(QuicHexToBytes('746f6b'),True);
  AssertFalse('Second retry ignored',QuicBytesEqual(lClient.DestinationCid,lNewCid));
  FNet.Run;
  AssertTrue('Client closed the connection',lClient.State>=qcsClosing);
  AssertEquals('Server lacks retry_source_connection_id',QUIC_TRANSPORT_PARAMETER_ERROR,lClient.CloseErrorCode);
end;


procedure TTestQuicConnection.TestVersionNegotiation;

var
  lData: TBytes;

begin
  FNet.Start;
  FNet.Client.Connection.NextDatagram(FNet.Now,lData);
  FNet.Client.Connection.ReceiveDatagram(QuicBuildVersionNegotiation(FNet.Client.Connection.LocalCid,
    FNet.Client.Connection.OriginalDcid,[$1a2a3a4a]),FNet.Now);
  AssertTrue('Closed',FNet.Client.Connection.State=qcsClosed);
  AssertEquals('Refused',QUIC_CONNECTION_REFUSED,FNet.Client.Connection.CloseErrorCode);
  AssertEquals('Close event',1,FNet.Client.CloseEvents);
end;


procedure TTestQuicConnection.TestVersionNegotiationIgnored;

var
  lVN: TBytes;

begin
  FNet.Start;
  lVN:=QuicBuildVersionNegotiation(FNet.Client.Connection.LocalCid,FNet.Client.Connection.OriginalDcid,
    [$1a2a3a4a,QUIC_VERSION_1]);
  FNet.Client.Connection.ReceiveDatagram(lVN,FNet.Now);
  AssertTrue('Listing version 1: ignored',FNet.Client.Connection.State=qcsHandshake);
  FNet.Handshake;
  FNet.Client.Connection.ReceiveDatagram(QuicBuildVersionNegotiation(FNet.Client.Connection.LocalCid,
    FNet.Client.Connection.OriginalDcid,[$1a2a3a4a]),FNet.Now);
  AssertTrue('After the handshake: ignored',FNet.Client.Connection.State=qcsEstablished);
  FNet.Server.Connection.ReceiveDatagram(lVN,FNet.Now);
  AssertTrue('Server ignores VN',FNet.Server.Connection.State=qcsEstablished);
end;


procedure TTestQuicConnection.TestStatelessReset;

var
  lReset: TBytes;

begin
  FNet.ServerParams.HasStatelessResetToken:=True;
  FNet.ServerParams.StatelessResetToken:=QuicHexToBytes(ServerResetToken);
  FNet.Handshake;
  FNet.Client.Connection.ReceiveDatagram(QuicHexToBytes('4102'),FNet.Now);
  AssertTrue('Short datagram is no reset',FNet.Client.Connection.State=qcsEstablished);
  lReset:=QuicHexToBytes('41aabbccddeeff00112233'+ServerResetToken);
  FNet.Client.Connection.ReceiveDatagram(Copy(lReset,0,Length(lReset)-1),FNet.Now);
  AssertTrue('Wrong token is no reset',FNet.Client.Connection.State=qcsEstablished);
  FNet.Client.Connection.ReceiveDatagram(lReset,FNet.Now);
  AssertTrue('Draining after reset',FNet.Client.Connection.State=qcsDraining);
  AssertEquals('Reason','Stateless reset',FNet.Client.Connection.CloseReason);
end;


procedure TTestQuicConnection.TestAmplificationLimit;

var
  I: Integer;

begin
  FNet.Start;
  FNet.Step;
  FDropClient:=1000;
  for I:=1 to 10 do
    if not FNet.Server.Connection.Recovery.AmplificationBlocked then
      begin
      FNet.Run(30);
      FNet.FireTimer(20000000);
      end;
  AssertTrue('Server limited to three times the received bytes',FServerBytes<=3*FClientBytes);
  AssertTrue('Server sent datagrams',FServerDatagrams>0);
  AssertTrue('Server blocked',FNet.Server.Connection.Recovery.AmplificationBlocked);
  AssertEquals('No PTO while blocked',0,FNet.Server.Connection.Recovery.Timer);
end;


procedure TTestQuicConnection.TestApiEdgeCases;

var
  lId, lCode: QWord;
  lBuf: Byte;

begin
  FNet.Start;
  AssertFalse('No stream before the handshake',FNet.Client.Connection.OpenStream(True,lId));
  AssertFalse('No key update before the handshake',FNet.Client.Connection.InitiateKeyUpdate);
  FNet.Handshake;
  AssertFalse('Unknown stream write',FNet.Client.Connection.WriteStream(99,nil,False));
  lBuf:=0;
  AssertEquals('Unknown stream read',0,FNet.Client.Connection.ReadStream(99,@lBuf,1));
  AssertTrue('Unknown stream finished',FNet.Client.Connection.StreamFinished(99));
  AssertFalse('Unknown stream reset',FNet.Client.Connection.StreamReset(99,lCode));
  AssertEquals('Unknown stream buffered',0,FNet.Client.Connection.StreamBuffered(99));
  FNet.Client.Connection.ResetStream(99,1);
  FNet.Client.Connection.StopSending(99,1);
  AssertFalse('Nothing accepted',FNet.Client.Connection.AcceptStream(lId));
  FNet.Client.Connection.OpenStream(False,lId);
  FNet.Client.Connection.StopSending(lId,1);
  AssertFalse('No STOP_SENDING on a send-only stream',FNet.Client.Connection.Streams.Find(lId).StopSendingPending);
end;


procedure TTestQuicConnection.TestDuplicateAndGarbage;

var
  lId: QWord;
  lPacket: TBytes;

begin
  FNet.Handshake;
  FNet.Client.Connection.OpenStream(True,lId);
  WriteString(FNet.Client.Connection,lId,'dup',False);
  FCapture:=True;
  FNet.Step;
  FNet.Server.Connection.ReceiveDatagram(FCaptured,FNet.Now);
  FNet.Server.Connection.ReceiveDatagram(FCaptured,FNet.Now);
  FNet.Run;
  AssertEquals('Duplicate delivered once','dup',ReadAll(FNet.Server.Connection,lId));
  FNet.Server.Connection.ReceiveDatagram(QuicHexToBytes('ffff'),FNet.Now);
  lPacket:=nil;
  SetLength(lPacket,1200);
  lPacket[0]:=$D0;
  lPacket[4]:=1;
  lPacket[5]:=8;
  lPacket[15]:=$44;
  FNet.Server.Connection.ReceiveDatagram(lPacket,FNet.Now);
  lPacket[0]:=$E0;
  FNet.Server.Connection.ReceiveDatagram(lPacket,FNet.Now);
  FNet.Client.Connection.ReceiveDatagram(QuicHexToBytes('40'+QuicBytesToHex(FNet.Client.Connection.LocalCid)+
    StringOfChar('0',60)),FNet.Now);
  AssertTrue('Garbage ignored',FNet.Server.Connection.State=qcsEstablished);
  AssertTrue('Undecryptable ignored',FNet.Client.Connection.State=qcsEstablished);
end;


procedure TTestQuicConnection.TestSmallClientInitial;

begin
  FNet.Start;
  FNet.Step;
  Inject(False,qelInitial,Encode([QuicFrame(QUIC_FRAME_PING)]));
  AssertTrue('Small Initial dropped',FNet.Server.Connection.State=qcsHandshake);
end;


procedure TTestQuicConnection.TestAlpnMismatch;

begin
  FNet.ClientTLS.Alpn:=['other'];
  FNet.Start;
  FNet.Run;
  AssertTrue('Client draining',FNet.Client.Connection.State=qcsDraining);
  AssertEquals('no_application_protocol',QUIC_CRYPTO_ERROR+120,FNet.Client.Connection.CloseErrorCode);
end;


procedure TTestQuicConnection.TestHandshakeDoneFromClient;

begin
  FNet.Handshake;
  InjectFrames(False,[QuicFrame(QUIC_FRAME_HANDSHAKE_DONE)]);
  ExpectClose(FNet.Server.Connection,QUIC_PROTOCOL_VIOLATION);
end;


procedure TTestQuicConnection.TestNewTokenFromClient;

var
  lFrame: TQuicFrame;

begin
  FNet.Handshake;
  lFrame:=QuicFrame(QUIC_FRAME_NEW_TOKEN);
  lFrame.Data:=QuicHexToBytes('01');
  InjectFrames(False,[lFrame]);
  ExpectClose(FNet.Server.Connection,QUIC_PROTOCOL_VIOLATION);
end;


procedure TTestQuicConnection.TestNewTokenFromServer;

var
  lFrame: TQuicFrame;

begin
  FNet.Handshake;
  lFrame:=QuicFrame(QUIC_FRAME_NEW_TOKEN);
  lFrame.Data:=QuicHexToBytes('01');
  InjectFrames(True,[lFrame,QuicFrame(QUIC_FRAME_HANDSHAKE_DONE)]);
  AssertTrue('Client accepts NEW_TOKEN and repeated HANDSHAKE_DONE',FNet.Client.Connection.State=qcsEstablished);
end;


procedure TTestQuicConnection.TestRetireConnectionId;

var
  lFrame: TQuicFrame;
  lCids: TQuicLocalCidArray;
  lUnknown: TBytes;

begin
  FNet.ServerParams.ActiveConnectionIdLimit:=8;
  FNet.ClientParams.ActiveConnectionIdLimit:=8;
  FNet.Handshake;
  FNet.Run;
  lCids:=FNet.Server.Connection.LocalCids;
  AssertEquals('Server issued connection IDs up to its maximum',4,Length(lCids));
  AssertEquals('Last issued sequence',3,lCids[3].Sequence);
  AssertTrue('Issued ID routes to the server',FNet.Server.Connection.OwnsCid(lCids[2].Cid));
  lUnknown:=QuicHexToBytes('0102030405060708');
  AssertFalse('Unknown ID',FNet.Server.Connection.OwnsCid(lUnknown));
  lFrame:=QuicFrame(QUIC_FRAME_RETIRE_CONNECTION_ID);
  lFrame.SequenceNumber:=1;
  InjectFrames(False,[lFrame]);
  InjectFrames(False,[lFrame]);
  lCids:=FNet.Server.Connection.LocalCids;
  AssertEquals('Retired ID replaced',4,Length(lCids));
  AssertEquals('Sequence 1 retired',2,lCids[1].Sequence);
  AssertEquals('Replacement sequence',4,lCids[3].Sequence);
  FNet.Run;
  AssertEquals('Server still established',Ord(qcsEstablished),Ord(FNet.Server.Connection.State));
  lFrame.SequenceNumber:=99;
  InjectFrames(False,[lFrame]);
  ExpectClose(FNet.Server.Connection,QUIC_PROTOCOL_VIOLATION);
end;


procedure TTestQuicConnection.TestLostNewConnectionId;

var
  lFrame: TQuicFrame;
  lCids: TQuicLocalCidArray;
  lRounds: Integer;

begin
  FNet.ServerParams.ActiveConnectionIdLimit:=4;
  FNet.ClientParams.ActiveConnectionIdLimit:=4;
  FDropAfterServerComplete:=True;
  FNet.Start;
  lRounds:=0;
  while ((FNet.Server.Connection=nil) or not FNet.Server.Connection.HandshakeComplete) and (lRounds<100) do
    begin
    if not FNet.Step then
      FNet.FireTimer;
    Inc(lRounds);
    end;
  AssertFalse('First server datagram after completion dropped',FDropAfterServerComplete);
  lFrame:=QuicFrame(QUIC_FRAME_RETIRE_CONNECTION_ID);
  lFrame.SequenceNumber:=1;
  InjectFrames(False,[lFrame]);
  FNet.Run;
  lCids:=FNet.Server.Connection.LocalCids;
  AssertEquals('Active IDs',4,Length(lCids));
  AssertEquals('Sequence 2 kept',2,lCids[1].Sequence);
  AssertTrue('Client confirmed after retransmission',FNet.Client.Connection.HandshakeConfirmed);
end;


procedure TTestQuicConnection.TestEmptyPacket;

begin
  FNet.Handshake;
  Inject(False,qelApplication,nil);
  ExpectClose(FNet.Server.Connection,QUIC_PROTOCOL_VIOLATION);
end;


procedure TTestQuicConnection.TestReservedBits;

begin
  FNet.Handshake;
  Inject(False,qelApplication,Encode([QuicFrame(QUIC_FRAME_PING)]),$08);
  ExpectClose(FNet.Server.Connection,QUIC_PROTOCOL_VIOLATION);
end;


procedure TTestQuicConnection.TestFrameNotAllowed;

var
  lFrame: TQuicFrame;

begin
  FNet.Start;
  FNet.Step;
  lFrame:=QuicFrame(QUIC_FRAME_STREAM);
  Inject(False,qelInitial,Encode([lFrame]),0,1200);
  ExpectClose(FNet.Server.Connection,QUIC_PROTOCOL_VIOLATION);
  FNet.Run;
  AssertTrue('Client closed by the server',FNet.Client.Connection.ClosedByPeer);
end;


procedure TTestQuicConnection.TestUnknownFrame;

begin
  FNet.Handshake;
  Inject(False,qelApplication,QuicHexToBytes('1f000000'));
  ExpectClose(FNet.Server.Connection,QUIC_FRAME_ENCODING_ERROR);
end;


procedure TTestQuicConnection.TestConnectionFlowControl;

var
  lFrames: array[0..2] of TQuicFrame;
  I: Integer;

begin
  FNet.Handshake;
  for I:=0 to 2 do
    begin
    lFrames[I]:=QuicFrame(QUIC_FRAME_STREAM);
    lFrames[I].StreamId:=I*4;
    lFrames[I].Offset:=39999;
    lFrames[I].Data:=QuicHexToBytes('01');
    end;
  InjectFrames(False,lFrames);
  ExpectClose(FNet.Server.Connection,QUIC_FLOW_CONTROL_ERROR);
end;


procedure TTestQuicConnection.TestResetFlowControl;

var
  lFrames: array[0..3] of TQuicFrame;
  I: Integer;

begin
  FNet.Handshake;
  for I:=0 to 2 do
    begin
    lFrames[I]:=QuicFrame(QUIC_FRAME_RESET_STREAM);
    lFrames[I].StreamId:=I*4;
    lFrames[I].Value:=40000;
    end;
  lFrames[3]:=lFrames[0];
  InjectFrames(False,[lFrames[0],lFrames[3]]);
  AssertTrue('Repeated reset accepted',FNet.Server.Connection.State=qcsEstablished);
  InjectFrames(False,[lFrames[1],lFrames[2]]);
  ExpectClose(FNet.Server.Connection,QUIC_FLOW_CONTROL_ERROR);
end;


procedure TTestQuicConnection.TestCryptoBufferExceeded;

var
  lFrame: TQuicFrame;

begin
  FNet.Handshake;
  lFrame:=QuicFrame(QUIC_FRAME_CRYPTO);
  lFrame.Offset:=100000;
  lFrame.Data:=QuicHexToBytes('01');
  InjectFrames(False,[lFrame]);
  ExpectClose(FNet.Server.Connection,QUIC_CRYPTO_BUFFER_EXCEEDED);
end;


procedure TTestQuicConnection.TestAckOfUnsentPacket;

var
  lFrame: TQuicFrame;

begin
  FNet.Handshake;
  lFrame:=QuicFrame(QUIC_FRAME_ACK_ECN);
  SetLength(lFrame.AckRanges,1);
  lFrame.AckRanges[0].Low:=0;
  lFrame.AckRanges[0].High:=0;
  InjectFrames(False,[lFrame]);
  AssertTrue('ACK_ECN of a sent packet accepted',FNet.Server.Connection.State=qcsEstablished);
  lFrame.AckRanges[0].High:=5000;
  InjectFrames(False,[lFrame]);
  ExpectClose(FNet.Server.Connection,QUIC_PROTOCOL_VIOLATION);
end;


procedure TTestQuicConnection.TestNewConnectionId;

var
  lFrame: TQuicFrame;
  lId: QWord;

begin
  FNet.Handshake;
  lFrame:=QuicFrame(QUIC_FRAME_NEW_CONNECTION_ID);
  lFrame.SequenceNumber:=5;
  lFrame.RetirePriorTo:=5;
  lFrame.ConnectionId:=QuicHexToBytes('c1c2c3c4c5c6c7c8');
  lFrame.ResetToken:=QuicHexToBytes(ServerResetToken);
  InjectFrames(True,[lFrame,lFrame]);
  AssertTrue('Switched to the new CID',QuicBytesEqual(FNet.Client.Connection.DestinationCid,lFrame.ConnectionId));
  FNet.Client.Connection.OpenStream(True,lId);
  FDropClient:=1;
  FNet.Run;
  AssertEquals('Server still established',Ord(qcsEstablished),Ord(FNet.Server.Connection.State));
  AssertEquals('Retired IDs replaced',2,Length(FNet.Server.Connection.LocalCids));
  AssertTrue('Sequences 0 and 1 retired',FNet.Server.Connection.LocalCids[0].Sequence>=2);
end;


procedure TTestQuicConnection.TestConnectionIdLimit;

var
  lFrame: TQuicFrame;

begin
  FNet.Handshake;
  lFrame:=QuicFrame(QUIC_FRAME_NEW_CONNECTION_ID);
  lFrame.SequenceNumber:=1;
  lFrame.ConnectionId:=QuicHexToBytes('c1c2c3c4c5c6c7c8');
  lFrame.ResetToken:=QuicHexToBytes(ServerResetToken);
  InjectFrames(True,[lFrame]);
  lFrame.SequenceNumber:=2;
  InjectFrames(True,[lFrame]);
  ExpectClose(FNet.Client.Connection,QUIC_CONNECTION_ID_LIMIT_ERROR);
end;


procedure TTestQuicConnection.TestMiscFrames;

var
  lFrames: array[0..7] of TQuicFrame;
  lId: QWord;

begin
  FNet.Handshake;
  FNet.Client.Connection.OpenStream(True,lId);
  lFrames[0]:=QuicFrame(QUIC_FRAME_PATH_CHALLENGE);
  lFrames[0].Data:=QuicHexToBytes('0102030405060708');
  lFrames[1]:=QuicFrame(QUIC_FRAME_MAX_DATA);
  lFrames[1].Value:=500000;
  lFrames[2]:=QuicFrame(QUIC_FRAME_MAX_STREAM_DATA);
  lFrames[2].StreamId:=lId;
  lFrames[2].Value:=900000;
  lFrames[3]:=QuicFrame(QUIC_FRAME_MAX_STREAMS_BIDI);
  lFrames[3].Value:=20;
  lFrames[4]:=QuicFrame(QUIC_FRAME_DATA_BLOCKED);
  lFrames[5]:=QuicFrame(QUIC_FRAME_STREAMS_BLOCKED_UNI);
  lFrames[6]:=QuicFrame(QUIC_FRAME_STREAM_DATA_BLOCKED);
  lFrames[6].StreamId:=1;
  lFrames[7]:=QuicFrame(QUIC_FRAME_MAX_STREAM_DATA);
  lFrames[7].StreamId:=lId;
  lFrames[7].Value:=10;
  InjectFrames(True,lFrames);
  AssertTrue('Client still established',FNet.Client.Connection.State=qcsEstablished);
  AssertEquals('Stream credit',QWord(900000),FNet.Client.Connection.Streams.Find(lId).SendMaxData);
  AssertEquals('Stream limit',20,FNet.Client.Connection.Streams.PeerMaxStreams(True));
  FDropClient:=1;
  FNet.Run;
  AssertTrue('Server accepted PATH_RESPONSE',FNet.Server.Connection.State=qcsEstablished);
end;


procedure TTestQuicConnection.TestLostMaxData;

var
  lId: QWord;
  lData: TBytes;
  lBuf: array[0..4095] of Byte;
  lReceived, lCount, lRounds: Integer;

begin
  FNet.ServerParams.InitialMaxData:=1000;
  FNet.Handshake;
  lData:=nil;
  SetLength(lData,3000);
  FNet.Client.Connection.OpenStream(True,lId);
  FNet.Client.Connection.WriteStream(lId,lData,True);
  FNet.Run;
  lReceived:=0;
  lRounds:=0;
  while (lReceived<3000) and (lRounds<50) do
    begin
    repeat
      lCount:=FNet.Server.Connection.ReadStream(lId,@lBuf[0],SizeOf(lBuf));
      Inc(lReceived,lCount);
    until lCount=0;
    if lRounds=0 then
      FDropServer:=1;
    FNet.Run;
    Inc(lRounds);
    end;
  AssertEquals('Data received after lost MAX_DATA',3000,lReceived);
end;


procedure TTestQuicConnection.TestHandshakeAckValidatesAddress;

var
  lFrame: TQuicFrame;
  lData: TBytes;

begin
  FNet.Start;
  FNet.Step;
  AssertFalse('Client not validated',FNet.Client.Connection.Recovery.PeerAddressValidated);
  AssertTrue('Client sends Handshake',FNet.Client.Connection.NextDatagram(FNet.Now,lData));
  lFrame:=QuicFrame(QUIC_FRAME_ACK);
  SetLength(lFrame.AckRanges,1);
  Inject(True,qelHandshake,Encode([lFrame]));
  AssertTrue('Handshake ACK validates the client address',FNet.Client.Connection.Recovery.PeerAddressValidated);
end;


procedure TTestQuicConnection.TestResetTokenOfUnusedCid;

const
  UnusedToken = 'ffeeddccbbaa99887766554433221100';

var
  lFrame: TQuicFrame;

begin
  FNet.Handshake;
  lFrame:=QuicFrame(QUIC_FRAME_NEW_CONNECTION_ID);
  lFrame.SequenceNumber:=1;
  lFrame.ConnectionId:=QuicHexToBytes('c1c2c3c4c5c6c7c8');
  lFrame.ResetToken:=QuicHexToBytes(UnusedToken);
  InjectFrames(True,[lFrame]);
  FNet.Client.Connection.ReceiveDatagram(QuicHexToBytes('41aabbccddeeff00112233'+UnusedToken),FNet.Now);
  AssertTrue('Token of an unused connection ID is no reset',FNet.Client.Connection.State=qcsEstablished);
end;


procedure TTestQuicConnection.TestHugeIdleTimeout;

begin
  FNet.ClientParams.MaxIdleTimeout:=QWord(1) shl 61;
  FNet.ServerParams.MaxIdleTimeout:=QWord(1) shl 61;
  FNet.Handshake;
  FNet.Run;
  AssertTrue('Client idle timer does not wrap',FNet.Client.Connection.NextTimeout>FNet.Now);
  AssertTrue('Server idle timer does not wrap',FNet.Server.Connection.NextTimeout>FNet.Now);
end;


procedure TTestQuicConnection.TestIdleTimeoutFloor;

var
  lStart, lMinimum: QWord;

begin
  FNet.ServerParams.MaxIdleTimeout:=1;
  FNet.Handshake;
  lStart:=FNet.Now;
  lMinimum:=3*FNet.Server.Connection.Recovery.PtoDuration;
  FNet.Server.Connection.HandleTimeout(lStart+2000);
  AssertTrue('Open after the 1 ms idle timeout',FNet.Server.Connection.State=qcsEstablished);
  FNet.Server.Connection.HandleTimeout(lStart+lMinimum+1000);
  AssertTrue('Closed after three probe timeouts',FNet.Server.Connection.State=qcsClosed);
  AssertEquals('Reason','Idle timeout',FNet.Server.Connection.CloseReason);
end;


procedure TTestQuicConnection.TestEventException;

begin
  FNet.Client.FailOnTicket:=True;
  FNet.Handshake;
  FNet.Run;
  ExpectClose(FNet.Client.Connection,QUIC_INTERNAL_ERROR);
  AssertEquals('Reason','Ticket store failed',FNet.Client.Connection.CloseReason);
end;


procedure TTestQuicConnection.TestAbort;

var
  lData: TBytes;

begin
  FNet.Handshake;
  FNet.Client.Connection.Abort('gone');
  AssertTrue('Closed',FNet.Client.Connection.State=qcsClosed);
  AssertEquals('Error code',QUIC_INTERNAL_ERROR,FNet.Client.Connection.CloseErrorCode);
  AssertEquals('Reason','gone',FNet.Client.Connection.CloseReason);
  AssertEquals('Close event',1,FNet.Client.CloseEvents);
  FNet.Client.Connection.Abort('again');
  AssertEquals('First reason kept','gone',FNet.Client.Connection.CloseReason);
  AssertEquals('One close event',1,FNet.Client.CloseEvents);
  AssertFalse('Nothing sent',FNet.Client.Connection.NextDatagram(FNet.Now,lData));
end;


procedure TTestQuicConnection.TestAutomaticKeyUpdate;

var
  lSaved: TQuicAeadLimits;
  lSuite: TQuicCipherSuite;
  lId, lServerId: QWord;
  lPhase: Boolean;
  lUpdates, I: Integer;

begin
  lSaved:=QuicConfidentialityLimit;
  try
    for lSuite:=Low(TQuicCipherSuite) to High(TQuicCipherSuite) do
      QuicConfidentialityLimit[lSuite]:=16;
    FNet.Handshake;
    FNet.Client.Connection.OpenStream(True,lId);
    lPhase:=FNet.Client.Connection.KeyPhase;
    lUpdates:=0;
    for I:=1 to 40 do
      begin
      WriteString(FNet.Client.Connection,lId,'x',False);
      FNet.Run;
      if FNet.Client.Connection.KeyPhase<>lPhase then
        begin
        lPhase:=not lPhase;
        Inc(lUpdates);
        end;
      end;
    AssertTrue('Keys updated more than once',lUpdates>=2);
    AssertTrue('Client established',FNet.Client.Connection.State=qcsEstablished);
    AssertTrue('Server stream',FNet.Server.Connection.AcceptStream(lServerId));
    AssertEquals('Data across the updates',StringOfChar('x',40),ReadAll(FNet.Server.Connection,lServerId));
  finally
    QuicConfidentialityLimit:=lSaved;
  end;
end;


procedure TTestQuicConnection.TestConfidentialityLimit;

var
  lSaved: TQuicAeadLimits;
  lSuite: TQuicCipherSuite;
  lId: QWord;
  lData: TBytes;
  I: Integer;

begin
  lSaved:=QuicConfidentialityLimit;
  try
    for lSuite:=Low(TQuicCipherSuite) to High(TQuicCipherSuite) do
      QuicConfidentialityLimit[lSuite]:=8;
    FNet.Handshake;
    FNet.Client.Connection.OpenStream(True,lId);
    for I:=1 to 30 do
      if FNet.Client.Connection.WriteStream(lId,QuicHexToBytes('78'),False) then
        FNet.Client.Connection.NextDatagram(FNet.Now,lData);
    ExpectClose(FNet.Client.Connection,QUIC_AEAD_LIMIT_REACHED);
  finally
    QuicConfidentialityLimit:=lSaved;
  end;
end;


procedure TTestQuicConnection.TestIntegrityLimit;

var
  lSaved: TQuicAeadLimits;
  lSuite: TQuicCipherSuite;
  lId: QWord;
  lBad: TBytes;

begin
  lSaved:=QuicIntegrityLimit;
  try
    for lSuite:=Low(TQuicCipherSuite) to High(TQuicCipherSuite) do
      QuicIntegrityLimit[lSuite]:=2;
    FNet.Handshake;
    FNet.Client.Connection.OpenStream(True,lId);
    WriteString(FNet.Client.Connection,lId,'x',False);
    FCapture:=True;
    FNet.Step;
    lBad:=Copy(FCaptured);
    lBad[High(lBad)]:=lBad[High(lBad)] xor 1;
    FNet.Server.Connection.ReceiveDatagram(lBad,FNet.Now);
    AssertTrue('One failure is tolerated',FNet.Server.Connection.State=qcsEstablished);
    FNet.Server.Connection.ReceiveDatagram(lBad,FNet.Now);
    ExpectClose(FNet.Server.Connection,QUIC_AEAD_LIMIT_REACHED);
  finally
    QuicIntegrityLimit:=lSaved;
  end;
end;


initialization
  RegisterTest(TTestQuicConnection);
end.
