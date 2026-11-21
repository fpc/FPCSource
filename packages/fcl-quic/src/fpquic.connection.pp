{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Sans-I/O QUIC connection (RFC 9000): datagrams and timestamps in, datagrams and timeouts out.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpQuic.Connection;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, System.Classes, FpQuic.Consts, FpQuic.Buffers,
  FpQuic.Frames, FpQuic.Packets, FpQuic.TransportParams, FpQuic.Crypto,
  FpQuic.TLS, FpQuic.Streams, FpQuic.Recovery;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils, Classes, FpQuic.Consts, FpQuic.Buffers, FpQuic.Frames, FpQuic.Packets, FpQuic.TransportParams,
  FpQuic.Crypto, FpQuic.TLS, FpQuic.Streams, FpQuic.Recovery;
{$ENDIF FPC_DOTTEDUNITS}

type
  TQuicConnection = class;

  // Connection life cycle; Closing sends CONNECTION_CLOSE, Draining waits silently after the peer closed.
  TQuicConnectionState = (qcsHandshake, qcsEstablished, qcsClosing, qcsDraining, qcsClosed);

  // Notifications from a connection; implemented and owned by the caller (for example an endpoint).
  IQuicConnectionEvents = interface
    // The TLS handshake completed.
    procedure QuicHandshakeComplete(aConnection: TQuicConnection);
    // Data, the end of the stream or a reset arrived on aStreamId; new peer streams are taken with AcceptStream.
    procedure QuicStreamData(aConnection: TQuicConnection; aStreamId: QWord);
    // The connection entered the closing, draining or closed state.
    procedure QuicConnectionClosed(aConnection: TQuicConnection);
    // A client received a session ticket for resuming a later connection (TQuicConnectionConfig.Ticket).
    procedure QuicSessionTicket(aConnection: TQuicConnection; const aTicket: TBytes);
  end;

  // Settings of a new connection.
  TQuicConnectionConfig = record
    IsServer: Boolean;
    // TLS session, owned by the caller; it must outlive the connection.
    TLS: IQuicTLS;
    // Cipher factory, owned by the caller.
    CipherFactory: IQuicCipherFactory;
    // Local transport parameters; connection ID parameters are filled in by the connection.
    Params: TQuicTransportParams;
    // Event receiver, may be nil.
    Events: IQuicConnectionEvents;
    // Client: session ticket to resume, empty for a full handshake; 0-RTT is used when the ticket allows it.
    Ticket: TBytes;
  end;

  // Keys, CRYPTO buffers and ACK state of one packet number space.
  TQuicSpace = class
  public
    Tx: TQuicKeys;
    Rx: TQuicKeys;
    CryptoSend: TQuicSendBuffer;
    CryptoRecv: TQuicReassembly;
    Ack: TQuicAckTracker;
    NextPn: QWord;
    LargestRecv: Int64;
    Discarded: Boolean;
    // Number of probe packets still to send after a probe timeout.
    Probes: Integer;
    constructor Create;
    destructor Destroy; override;
    // Frees the keys.
    procedure DropKeys;
  end;

  // Connection ID issued by the peer with NEW_CONNECTION_ID.
  TQuicPeerCid = record
    Sequence: QWord;
    Cid: TBytes;
    ResetToken: TBytes;
  end;

  // Connection ID issued by this endpoint and not retired by the peer.
  TQuicLocalCid = record
    Sequence: QWord;
    Cid: TBytes;
  end;
  TQuicLocalCidArray = array of TQuicLocalCid;

  // A QUIC connection without I/O: feed datagrams and timeouts, collect datagrams to send.
  TQuicConnection = class(TObject, IQuicTLSEvents)
  private
    FIsServer: Boolean;
    FState: TQuicConnectionState;
    FTLS: IQuicTLS;
    FFactory: IQuicCipherFactory;
    FEvents: IQuicConnectionEvents;
    FLocalParams: TQuicTransportParams;
    FPeerParams: TQuicTransportParams;
    FHavePeerParams: Boolean;
    FLocalCid: TBytes;
    FLocalCids: TQuicLocalCidArray;
    FNextCidSequence: QWord;
    FOriginalDcid: TBytes;
    FPeerInitialScid: TBytes;
    FPeerCids: array of TQuicPeerCid;
    FRetirePriorTo: QWord;
    FRetryScid: TBytes;
    FRetryToken: TBytes;
    FRetried: Boolean;
    FReceivedInitial: Boolean;
    FSpaces: array[TQuicEncryptionLevel] of TQuicSpace;
    FRxPrev: TQuicKeys;
    FTxPhase: Boolean;
    FRxPhase: Boolean;
    FRxPhaseStartPn: QWord;
    FTxPhaseStartPn: QWord;
    FTxPhaseAcked: Boolean;
    FCC: TQuicNewReno;
    FRecovery: TQuicLossDetector;
    FStreams: TQuicStreamMap;
    FStreamCursor: Integer;
    FSendMaxData: QWord;
    FSentData: QWord;
    FRecvMaxData: QWord;
    FRecvData: QWord;
    FRecvConsumed: QWord;
    FMaxDataPending: Boolean;
    FControl: TQuicFrameArray;
    FHandshakeComplete: Boolean;
    FHandshakeConfirmed: Boolean;
    FHandshakeDonePending: Boolean;
    FAddressValidated: Boolean;
    FBytesReceived: QWord;
    FBytesSent: QWord;
    FIdleTimeout: QWord;
    FLastActivity: QWord;
    FAuthFailures: QWord;
    FNow: QWord;
    FCloseFrame: TQuicFrame;
    FClosePending: Boolean;
    FCloseDeadline: QWord;
    FCloseErrorCode: QWord;
    FCloseReason: String;
    FClosedByPeer: Boolean;
    FPendingParams: TBytes;
    FHavePendingParams: Boolean;
    FPendingAlert: Integer;
    FEarlyKeys: TQuicKeys;
    FEarlySent: Boolean;
    FEarlyAccepted: Boolean;
    FEarlyPending: array of TBytes;
    FProcessingEarly: Boolean;
    function GetSpace(aLevel: TQuicEncryptionLevel): TQuicSpace;
    function IsEarly(aLevel: TQuicEncryptionLevel): Boolean;
    procedure ResumeSession(const aTicket: TBytes);
    procedure ProcessEarlyPending;
    procedure RejectEarlyData;
    procedure InstallInitialKeys(const aDcid: TBytes);
    procedure DiscardSpace(aLevel: TQuicEncryptionLevel);
    procedure CheckTLS;
    procedure ApplyPeerParams;
    procedure OnHandshakeComplete;
    procedure ConfirmHandshake;
    procedure EnterClosing(aErrorCode, aFrameType: QWord; const aReason: String; aApplication: Boolean);
    procedure EnterDraining(aErrorCode: QWord; const aReason: String);
    procedure SetClosed;
    function ProcessPacket(const aHeader: TQuicPacketHeader; aPacket: TBytes; aDatagramLength: Integer): Boolean;
    procedure ProcessRetry(const aHeader: TQuicPacketHeader; const aPacket: TBytes);
    function DecryptPacket(aLevel: TQuicEncryptionLevel; aEarly: Boolean; const aHeader: TQuicPacketHeader;
      var aPacket: TBytes; out aPn: QWord; out aPayload: TBytes): Boolean;
    function DecryptOneRtt(aFirstByte: Byte; aPn: QWord; const aPacket: TBytes; aHeaderLength: Integer;
      out aPayload: TBytes): Boolean;
    procedure ProcessFrames(aLevel: TQuicEncryptionLevel; const aPayload: TBytes; out aAckEliciting: Boolean);
    procedure ProcessFrame(aLevel: TQuicEncryptionLevel; const aFrame: TQuicFrame);
    procedure ProcessAck(aLevel: TQuicEncryptionLevel; const aFrame: TQuicFrame);
    procedure ProcessCrypto(aLevel: TQuicEncryptionLevel; const aFrame: TQuicFrame);
    procedure ProcessStream(const aFrame: TQuicFrame);
    procedure ProcessResetStream(const aFrame: TQuicFrame);
    procedure ProcessNewConnectionId(const aFrame: TQuicFrame);
    procedure ProcessRetireConnectionId(const aFrame: TQuicFrame);
    procedure IssueConnectionIds;
    function LocalCidIndex(aSequence: QWord): Integer;
    procedure OnFrameAcked(aLevel: TQuicEncryptionLevel; const aFrame: TQuicFrame);
    procedure OnFrameLost(aLevel: TQuicEncryptionLevel; const aFrame: TQuicFrame);
    procedure HandleLost(aLevel: TQuicEncryptionLevel; aLost: TFPList);
    procedure FreePackets(aList: TFPList);
    function IsStatelessReset(const aData: TBytes): Boolean;
    function PeerCid: TBytes;
    function AmplificationLimit: QWord;
    function LevelHasData(aLevel: TQuicEncryptionLevel): Boolean;
    function BuildPayload(aLevel: TQuicEncryptionLevel; aBudget: Integer; var aWriter: TQuicWriter;
      var aFrames: TQuicFrameArray): Boolean;
    procedure AddFrame(var aWriter: TQuicWriter; var aFrames: TQuicFrameArray; const aFrame: TQuicFrame);
    procedure RequeueForProbe(aLevel: TQuicEncryptionLevel);
    procedure AddStreamFrame(aStream: TQuicStream; var aWriter: TQuicWriter; var aFrames: TQuicFrameArray;
      aBudget: Integer);
    procedure AddStreamFrames(var aWriter: TQuicWriter; var aFrames: TQuicFrameArray; aBudget: Integer);
    procedure AddControlFrames(var aWriter: TQuicWriter; var aFrames: TQuicFrameArray; aBudget: Integer);
    function HeaderOverhead(aLevel: TQuicEncryptionLevel; aPnLength: Integer): Integer;
    function BuildClosePackets(out aData: TBytes): Boolean;
    function SealPacket(aLevel: TQuicEncryptionLevel; aPn: QWord; aPnLength: Integer; const aPayload: TBytes): TBytes;
    procedure UpdateRecoveryState;
    procedure Touch;
    function IdleDeadline: QWord;
    procedure CheckConfidentialityLimit;
  protected
    // IQuicTLSEvents
    procedure TLSSendCrypto(aLevel: TQuicEncryptionLevel; const aData: TBytes);
    procedure TLSSecret(aLevel: TQuicEncryptionLevel; aWrite: Boolean; aSuite: TQuicCipherSuite;
      const aSecret: TBytes);
    procedure TLSPeerTransportParams(const aParams: TBytes);
    procedure TLSAlert(aAlert: Byte);
    procedure TLSEarlySecret(aWrite: Boolean; aSuite: TQuicCipherSuite; const aSecret: TBytes);
    procedure TLSSessionTicket(const aTicket: TBytes);
  public
    // Creates a client connection and its first flight; aNow is the current time in microseconds.
    constructor CreateClient(const aConfig: TQuicConnectionConfig; aNow: QWord);
    // Creates a server connection for a client whose first Initial packet had Destination Connection ID aOriginalDcid.
    // aRetryScid is the Source Connection ID of the Retry packet the client answered, empty without Retry.
    constructor CreateServer(const aConfig: TQuicConnectionConfig; const aOriginalDcid: TBytes; aNow: QWord;
      const aRetryScid: TBytes = nil);
    destructor Destroy; override;
    // Processes one received UDP datagram.
    procedure ReceiveDatagram(const aData: TBytes; aNow: QWord);
    // Returns the next datagram to send; False when there is nothing to send now.
    function NextDatagram(aNow: QWord; out aData: TBytes): Boolean;
    // Time of the next timer event in microseconds, High(QWord) when none.
    function NextTimeout: QWord;
    // Handles expired timers.
    procedure HandleTimeout(aNow: QWord);
    // Closes the connection with an application error code.
    procedure Close(aErrorCode: QWord; const aReason: String; aNow: QWord);
    // Enters the closed state at once with INTERNAL_ERROR and aReason, without sending CONNECTION_CLOSE.
    procedure Abort(const aReason: String);
    // Opens a local stream; False when the peer stream limit is reached or the handshake is not complete.
    function OpenStream(aBidi: Boolean; out aId: QWord): Boolean;
    // Takes the next stream opened by the peer.
    function AcceptStream(out aId: QWord): Boolean;
    // Queues data on a stream, with aFin to end it; False when the stream cannot send.
    function WriteStream(aId: QWord; const aData: TBytes; aFin: Boolean = False): Boolean;
    // Reads received stream data; returns the number of bytes read.
    function ReadStream(aId: QWord; aBuffer: PByte; aCount: Integer): Integer;
    // True when all data of the stream was read, including the FIN.
    function StreamFinished(aId: QWord): Boolean;
    // True when the peer reset the stream; aErrorCode receives its error code. The stream is then forgotten
    // once its sending side is done.
    function StreamReset(aId: QWord; out aErrorCode: QWord): Boolean;
    // Number of bytes written to the stream and not yet acknowledged.
    function StreamBuffered(aId: QWord): Integer;
    // Abandons sending on a stream with RESET_STREAM.
    procedure ResetStream(aId, aErrorCode: QWord);
    // Asks the peer to stop sending on a stream with STOP_SENDING.
    procedure StopSending(aId, aErrorCode: QWord);
    // Starts a key update (RFC 9001 section 6); False when not allowed yet.
    function InitiateKeyUpdate: Boolean;
    // Negotiated application protocol.
    function Alpn: AnsiString;
    // Connection state.
    property State: TQuicConnectionState read FState;
    // True for a server connection.
    property IsServer: Boolean read FIsServer;
    // True once the TLS handshake completed.
    property HandshakeComplete: Boolean read FHandshakeComplete;
    // True once the handshake is confirmed (RFC 9001 section 4.1.2).
    property HandshakeConfirmed: Boolean read FHandshakeConfirmed;
    // True when the peer opened the stream with 0-RTT data.
    function StreamIsEarly(aId: QWord): Boolean;
    // True once the handshake completed and the server accepted 0-RTT data.
    property EarlyDataAccepted: Boolean read FEarlyAccepted;
    // 0-RTT packet protection keys until the handshake completes, nil otherwise.
    property EarlyKeys: TQuicKeys read FEarlyKeys;
    // True for a client that has 0-RTT keys and no 1-RTT keys yet: streams opened now send 0-RTT data.
    function CanSendEarly: Boolean;
    // True when aCid is a connection ID this endpoint issued and the peer has not retired.
    function OwnsCid(const aCid: TBytes): Boolean;
    // Connection ID chosen by this endpoint.
    property LocalCid: TBytes read FLocalCid;
    // Connection IDs issued by this endpoint and not retired, sequence 0 first.
    property LocalCids: TQuicLocalCidArray read FLocalCids;
    // Connection ID of the peer used as Destination Connection ID; a server has none until InitialReceived.
    property DestinationCid: TBytes read PeerCid;
    // True once an Initial packet from the peer was decrypted.
    property InitialReceived: Boolean read FReceivedInitial;
    // Key phase bit of sent 1-RTT packets.
    property KeyPhase: Boolean read FTxPhase;
    // Destination Connection ID of the client's first Initial packet.
    property OriginalDcid: TBytes read FOriginalDcid;
    // Transport parameters of the peer.
    property PeerParams: TQuicTransportParams read FPeerParams;
    // Error code of the CONNECTION_CLOSE sent or received.
    property CloseErrorCode: QWord read FCloseErrorCode;
    // Reason phrase of the CONNECTION_CLOSE sent or received.
    property CloseReason: String read FCloseReason;
    // True when the peer closed the connection.
    property ClosedByPeer: Boolean read FClosedByPeer;
    // Loss detector, exposing RTT and congestion state.
    property Recovery: TQuicLossDetector read FRecovery;
    // Congestion controller.
    property CongestionControl: TQuicNewReno read FCC;
    // Streams of the connection.
    property Streams: TQuicStreamMap read FStreams;
    // Packet number spaces.
    property Spaces[aLevel: TQuicEncryptionLevel]: TQuicSpace read GetSpace;
  end;

implementation

type
  TLevelWriters = array[TQuicEncryptionLevel] of TQuicWriter;

const
  CryptoBufferLimit = 65536;
  MaxIssuedCids = 4;
  MaxEarlyPending = 16;
  MaxIdleTimeoutMs = QWord(1) shl 40;
  // 1-RTT datagrams this close to QUIC_MAX_DATAGRAM are padded to it.
  FullDatagramSlack = 32;
  LongPacketTypes: array[qelInitial..qelHandshake] of TQuicPacketType = (qptInitial,qptHandshake);

{ TQuicSpace }

constructor TQuicSpace.Create;

begin
  CryptoSend:=TQuicSendBuffer.Create;
  CryptoRecv:=TQuicReassembly.Create;
  Ack:=TQuicAckTracker.Create;
  LargestRecv:=-1;
end;


destructor TQuicSpace.Destroy;

begin
  DropKeys;
  CryptoSend.Free;
  CryptoRecv.Free;
  Ack.Free;
  inherited Destroy;
end;


procedure TQuicSpace.DropKeys;

begin
  FreeAndNil(Tx);
  FreeAndNil(Rx);
end;


{ TQuicConnection }

// Converts a max_idle_timeout in milliseconds to microseconds, limited to MaxIdleTimeoutMs.
function IdleMicroseconds(aMilliseconds: QWord): QWord;

begin
  if aMilliseconds>MaxIdleTimeoutMs then
    aMilliseconds:=MaxIdleTimeoutMs;
  Result:=aMilliseconds*1000;
end;


constructor TQuicConnection.CreateClient(const aConfig: TQuicConnectionConfig; aNow: QWord);

var
  lLevel: TQuicEncryptionLevel;

begin
  FIsServer:=aConfig.IsServer;
  FTLS:=aConfig.TLS;
  FFactory:=aConfig.CipherFactory;
  FEvents:=aConfig.Events;
  FLocalParams:=aConfig.Params;
  FPeerParams:=QuicDefaultTransportParams;
  FPendingAlert:=-1;
  FNow:=aNow;
  FLastActivity:=aNow;
  FLocalCid:=QuicRandomBytes(QUIC_LOCAL_CID_LENGTH);
  SetLength(FLocalCids,1);
  FLocalCids[0].Sequence:=0;
  FLocalCids[0].Cid:=FLocalCid;
  FNextCidSequence:=1;
  FLocalParams.HasInitialSourceCid:=True;
  FLocalParams.InitialSourceCid:=FLocalCid;
  for lLevel:=Low(TQuicEncryptionLevel) to High(TQuicEncryptionLevel) do
    FSpaces[lLevel]:=TQuicSpace.Create;
  FCC:=TQuicNewReno.Create(QUIC_MAX_DATAGRAM);
  FRecovery:=TQuicLossDetector.Create(FCC);
  FRecovery.PeerAddressValidated:=FIsServer;
  FIdleTimeout:=IdleMicroseconds(FLocalParams.MaxIdleTimeout);
  FRecvMaxData:=FLocalParams.InitialMaxData;
  FStreams:=TQuicStreamMap.Create(FIsServer,FLocalParams);
  if FIsServer then
    Exit;
  FOriginalDcid:=QuicRandomBytes(QUIC_LOCAL_CID_LENGTH);
  SetLength(FPeerCids,1);
  FPeerCids[0].Cid:=FOriginalDcid;
  InstallInitialKeys(FOriginalDcid);
  if Length(aConfig.Ticket)>0 then
    ResumeSession(aConfig.Ticket);
  FTLS.Start(Self,QuicEncodeTransportParams(FLocalParams));
end;


procedure TQuicConnection.ResumeSession(const aTicket: TBytes);

var
  lParams: TBytes;
  lValid: Boolean;

begin
  if not FTLS.Resume(aTicket,lParams) then
    Exit;
  lValid:=True;
  try
    FPeerParams:=QuicRememberedParams(QuicDecodeTransportParams(lParams,True));
  except
    on EQuicError do
      lValid:=False;
  end;
  if not lValid then
    Exit;
  FStreams.SetPeerParams(FPeerParams);
  FSendMaxData:=FPeerParams.InitialMaxData;
end;


constructor TQuicConnection.CreateServer(const aConfig: TQuicConnectionConfig; const aOriginalDcid: TBytes;
  aNow: QWord; const aRetryScid: TBytes);

begin
  CreateClient(aConfig,aNow);
  FOriginalDcid:=aOriginalDcid;
  FLocalParams.HasOriginalDestinationCid:=True;
  FLocalParams.OriginalDestinationCid:=aOriginalDcid;
  FLocalParams.DisableActiveMigration:=True;
  if Length(aRetryScid)>0 then
    begin
    FLocalParams.HasRetrySourceCid:=True;
    FLocalParams.RetrySourceCid:=aRetryScid;
    InstallInitialKeys(aRetryScid);
    end
  else
    InstallInitialKeys(aOriginalDcid);
  FTLS.Start(Self,QuicEncodeTransportParams(FLocalParams));
end;


destructor TQuicConnection.Destroy;

var
  lLevel: TQuicEncryptionLevel;

begin
  FStreams.Free;
  FRecovery.Free;
  FCC.Free;
  FRxPrev.Free;
  FEarlyKeys.Free;
  for lLevel:=Low(TQuicEncryptionLevel) to High(TQuicEncryptionLevel) do
    FSpaces[lLevel].Free;
  inherited Destroy;
end;


function TQuicConnection.GetSpace(aLevel: TQuicEncryptionLevel): TQuicSpace;

begin
  Result:=FSpaces[aLevel];
end;


procedure TQuicConnection.InstallInitialKeys(const aDcid: TBytes);

var
  lClient, lServer: TBytes;

begin
  QuicInitialSecrets(aDcid,lClient,lServer);
  FSpaces[qelInitial].DropKeys;
  if FIsServer then
    begin
    FSpaces[qelInitial].Tx:=TQuicKeys.Create(FFactory,qcsAes128Gcm,lServer);
    FSpaces[qelInitial].Rx:=TQuicKeys.Create(FFactory,qcsAes128Gcm,lClient);
    end
  else
    begin
    FSpaces[qelInitial].Tx:=TQuicKeys.Create(FFactory,qcsAes128Gcm,lClient);
    FSpaces[qelInitial].Rx:=TQuicKeys.Create(FFactory,qcsAes128Gcm,lServer);
    end;
end;


procedure TQuicConnection.DiscardSpace(aLevel: TQuicEncryptionLevel);

begin
  if FSpaces[aLevel].Discarded then
    Exit;
  FSpaces[aLevel].Discarded:=True;
  FSpaces[aLevel].DropKeys;
  FSpaces[aLevel].CryptoSend.Free;
  FSpaces[aLevel].CryptoSend:=TQuicSendBuffer.Create;
  FSpaces[aLevel].CryptoRecv.Free;
  FSpaces[aLevel].CryptoRecv:=TQuicReassembly.Create;
  FRecovery.DiscardSpace(aLevel);
end;


function TQuicConnection.PeerCid: TBytes;

begin
  Result:=FPeerCids[0].Cid;
end;


procedure TQuicConnection.Touch;

begin
  FLastActivity:=FNow;
end;


// Time the connection closes without activity: the idle timeout, at least three probe timeouts.
function TQuicConnection.IdleDeadline: QWord;

var
  lMinimum: QWord;

begin
  lMinimum:=3*FRecovery.PtoDuration;
  if FIdleTimeout>lMinimum then
    Result:=FLastActivity+FIdleTimeout
  else
    Result:=FLastActivity+lMinimum;
end;


// Updates the 1-RTT keys halfway to the confidentiality limit and closes the connection when it is reached.
procedure TQuicConnection.CheckConfidentialityLimit;

var
  lSpace: TQuicSpace;
  lLimit: QWord;

begin
  lSpace:=FSpaces[qelApplication];
  if (FState<>qcsEstablished) or (lSpace.Tx=nil) then
    Exit;
  lLimit:=QuicConfidentialityLimit[lSpace.Tx.Suite];
  if lSpace.NextPn-FTxPhaseStartPn>=lLimit div 2 then
    InitiateKeyUpdate;
  if lSpace.NextPn-FTxPhaseStartPn>=lLimit then
    EnterClosing(QUIC_AEAD_LIMIT_REACHED,0,'AEAD confidentiality limit reached',False);
end;


procedure TQuicConnection.UpdateRecoveryState;

begin
  FRecovery.HandshakeConfirmed:=FHandshakeConfirmed;
  FRecovery.HasHandshakeKeys:=FSpaces[qelHandshake].Tx<>nil;
  FRecovery.AmplificationBlocked:=FIsServer and not FAddressValidated
                                  and (FBytesSent+QUIC_MAX_DATAGRAM>AmplificationLimit);
  FRecovery.UpdateTimer(FNow);
end;


function TQuicConnection.AmplificationLimit: QWord;

begin
  Result:=3*FBytesReceived;
end;


{ TLS events }

procedure TQuicConnection.TLSSendCrypto(aLevel: TQuicEncryptionLevel; const aData: TBytes);

begin
  FSpaces[aLevel].CryptoSend.Write(aData);
end;


procedure TQuicConnection.TLSSecret(aLevel: TQuicEncryptionLevel; aWrite: Boolean; aSuite: TQuicCipherSuite;
  const aSecret: TBytes);

var
  lKeys: TQuicKeys;

begin
  lKeys:=TQuicKeys.Create(FFactory,aSuite,aSecret);
  if aWrite then
    FSpaces[aLevel].Tx:=lKeys
  else
    FSpaces[aLevel].Rx:=lKeys;
end;


procedure TQuicConnection.TLSPeerTransportParams(const aParams: TBytes);

begin
  FPendingParams:=aParams;
  FHavePendingParams:=True;
end;


procedure TQuicConnection.TLSAlert(aAlert: Byte);

begin
  FPendingAlert:=aAlert;
end;


procedure TQuicConnection.TLSEarlySecret(aWrite: Boolean; aSuite: TQuicCipherSuite; const aSecret: TBytes);

begin
  FEarlyKeys.Free;
  FEarlyKeys:=TQuicKeys.Create(FFactory,aSuite,aSecret);
end;


procedure TQuicConnection.TLSSessionTicket(const aTicket: TBytes);

begin
  if FEvents<>nil then
    FEvents.QuicSessionTicket(Self,aTicket);
end;


function TQuicConnection.IsEarly(aLevel: TQuicEncryptionLevel): Boolean;

begin
  Result:=(aLevel=qelApplication) and (FSpaces[qelApplication].Tx=nil) and CanSendEarly;
end;


function TQuicConnection.CanSendEarly: Boolean;

begin
  Result:=(not FIsServer) and (FEarlyKeys<>nil) and (FState=qcsHandshake);
end;


procedure TQuicConnection.RejectEarlyData;

var
  lSent: TFPList;
  lFrame: TQuicFrame;
  I: Integer;

begin
  lSent:=FRecovery.SentPackets(qelApplication);
  for I:=0 to lSent.Count-1 do
    for lFrame in TQuicSentPacket(lSent[I]).Frames do
      OnFrameLost(qelApplication,lFrame);
  FRecovery.DiscardSpace(qelApplication);
end;


procedure TQuicConnection.CheckTLS;

begin
  if FPendingAlert>=0 then
    raise EQuicError.CreateCode(QUIC_CRYPTO_ERROR+QWord(FPendingAlert),'TLS alert '+IntToStr(FPendingAlert));
  if FHavePendingParams then
    begin
    FHavePendingParams:=False;
    ApplyPeerParams;
    end;
  if FTLS.IsComplete and not FHandshakeComplete then
    OnHandshakeComplete;
end;


// Raises TRANSPORT_PARAMETER_ERROR unless aPresent and aValue equals aExpected.
procedure CheckCid(aPresent: Boolean; const aValue, aExpected: TBytes; const aName: String);

begin
  if not (aPresent and QuicBytesEqual(aValue,aExpected)) then
    raise EQuicError.CreateCode(QUIC_TRANSPORT_PARAMETER_ERROR,'Invalid '+aName);
end;


procedure TQuicConnection.ApplyPeerParams;

var
  lPeerIdle: QWord;

begin
  FPeerParams:=QuicDecodeTransportParams(FPendingParams,not FIsServer);
  CheckCid(FPeerParams.HasInitialSourceCid,FPeerParams.InitialSourceCid,FPeerInitialScid,
    'initial_source_connection_id');
  if not FIsServer then
    begin
    if FRetried or FPeerParams.HasRetrySourceCid then
      CheckCid(FPeerParams.HasRetrySourceCid,FPeerParams.RetrySourceCid,FRetryScid,'retry_source_connection_id');
    CheckCid(FPeerParams.HasOriginalDestinationCid,FPeerParams.OriginalDestinationCid,FOriginalDcid,
      'original_destination_connection_id');
    if FPeerParams.HasStatelessResetToken then
      FPeerCids[0].ResetToken:=FPeerParams.StatelessResetToken;
    end;
  FHavePeerParams:=True;
  FStreams.SetPeerParams(FPeerParams);
  FSendMaxData:=FPeerParams.InitialMaxData;
  FRecovery.MaxAckDelay:=FPeerParams.MaxAckDelay*1000;
  lPeerIdle:=IdleMicroseconds(FPeerParams.MaxIdleTimeout);
  if (lPeerIdle>0) and ((FIdleTimeout=0) or (lPeerIdle<FIdleTimeout)) then
    FIdleTimeout:=lPeerIdle;
end;


procedure TQuicConnection.OnHandshakeComplete;

begin
  FHandshakeComplete:=True;
  FState:=qcsEstablished;
  FEarlyAccepted:=FTLS.EarlyDataAccepted;
  if FEarlySent and not FEarlyAccepted then
    RejectEarlyData;
  FreeAndNil(FEarlyKeys);
  FEarlyPending:=nil;
  IssueConnectionIds;
  if FIsServer then
    begin
    FHandshakeDonePending:=True;
    ConfirmHandshake;
    end;
  if FEvents<>nil then
    FEvents.QuicHandshakeComplete(Self);
end;


procedure TQuicConnection.ConfirmHandshake;

begin
  FHandshakeConfirmed:=True;
  FRecovery.PeerAddressValidated:=True;
  DiscardSpace(qelHandshake);
end;


{ Closing }

procedure TQuicConnection.EnterClosing(aErrorCode, aFrameType: QWord; const aReason: String; aApplication: Boolean);

begin
  if FState>=qcsClosing then
    Exit;
  if aApplication then
    FCloseFrame:=QuicFrame(QUIC_FRAME_CONNECTION_CLOSE_APP)
  else
    FCloseFrame:=QuicFrame(QUIC_FRAME_CONNECTION_CLOSE);
  FCloseFrame.ErrorCode:=aErrorCode;
  FCloseFrame.CloseFrameType:=aFrameType;
  FCloseFrame.Data:=TEncoding.UTF8.GetBytes(UnicodeString(aReason));
  FCloseErrorCode:=aErrorCode;
  FCloseReason:=aReason;
  FClosePending:=True;
  FState:=qcsClosing;
  FCloseDeadline:=FNow+3*FRecovery.PtoDuration;
  if FEvents<>nil then
    FEvents.QuicConnectionClosed(Self);
end;


procedure TQuicConnection.EnterDraining(aErrorCode: QWord; const aReason: String);

begin
  if FState>=qcsDraining then
    Exit;
  FClosedByPeer:=True;
  FCloseErrorCode:=aErrorCode;
  FCloseReason:=aReason;
  FState:=qcsDraining;
  FCloseDeadline:=FNow+3*FRecovery.PtoDuration;
  if FEvents<>nil then
    FEvents.QuicConnectionClosed(Self);
end;


procedure TQuicConnection.SetClosed;

begin
  FState:=qcsClosed;
  if FEvents<>nil then
    FEvents.QuicConnectionClosed(Self);
end;


procedure TQuicConnection.Close(aErrorCode: QWord; const aReason: String; aNow: QWord);

begin
  FNow:=aNow;
  EnterClosing(aErrorCode,0,aReason,True);
end;


procedure TQuicConnection.Abort(const aReason: String);

begin
  if FState=qcsClosed then
    Exit;
  FCloseErrorCode:=QUIC_INTERNAL_ERROR;
  FCloseReason:=aReason;
  SetClosed;
end;


{ Receiving }

function TQuicConnection.IsStatelessReset(const aData: TBytes): Boolean;

begin
  Result:=(Length(aData)>=21) and (Length(FPeerCids)>0)
          and QuicBytesEqualConstantTime(FPeerCids[0].ResetToken,
                Copy(aData,Length(aData)-QUIC_RESET_TOKEN_LENGTH,QUIC_RESET_TOKEN_LENGTH));
end;


procedure TQuicConnection.ReceiveDatagram(const aData: TBytes; aNow: QWord);

var
  lOffset: Integer;
  lHeader: TQuicPacketHeader;
  lProcessed: Boolean;

begin
  FNow:=aNow;
  if FState>=qcsDraining then
    Exit;
  Inc(FBytesReceived,Length(aData));
  lOffset:=0;
  lProcessed:=False;
  try
    while (lOffset<Length(aData))
          and QuicParseHeader(@aData[lOffset],Length(aData)-lOffset,QUIC_LOCAL_CID_LENGTH,lHeader) do
      begin
      if ProcessPacket(lHeader,Copy(aData,lOffset,lHeader.PacketLength),Length(aData)) then
        lProcessed:=True;
      Inc(lOffset,lHeader.PacketLength);
      end;
    ProcessEarlyPending;
    if (not lProcessed) and IsStatelessReset(aData) then
      EnterDraining(QUIC_NO_ERROR,'Stateless reset');
  except
    on E: EQuicError do
      EnterClosing(E.ErrorCode,E.FrameType,E.Message,False);
    on E: Exception do
      EnterClosing(QUIC_INTERNAL_ERROR,0,E.Message,False);
  end;
  FStreams.RemoveClosed;
  UpdateRecoveryState;
end;


procedure TQuicConnection.ProcessEarlyPending;

var
  lPackets: array of TBytes;
  lPacket: TBytes;
  lHeader: TQuicPacketHeader;

begin
  if Length(FEarlyPending)=0 then
    Exit;
  if FEarlyKeys=nil then
    begin
    if FSpaces[qelHandshake].Tx<>nil then
      FEarlyPending:=nil;
    Exit;
    end;
  lPackets:=FEarlyPending;
  FEarlyPending:=nil;
  for lPacket in lPackets do
    if QuicParseHeader(PByte(lPacket),Length(lPacket),QUIC_LOCAL_CID_LENGTH,lHeader) then
      ProcessPacket(lHeader,lPacket,QUIC_MIN_INITIAL_DATAGRAM);
end;


procedure TQuicConnection.ProcessRetry(const aHeader: TQuicPacketHeader; const aPacket: TBytes);

var
  lTag: TBytes;

begin
  if FIsServer or FRetried or FReceivedInitial or (Length(aHeader.Token)=0) then
    Exit;
  lTag:=QuicRetryIntegrityTag(FFactory,FOriginalDcid,Copy(aPacket,0,Length(aPacket)-QUIC_AEAD_TAG_LENGTH));
  if not QuicBytesEqual(lTag,Copy(aPacket,Length(aPacket)-QUIC_AEAD_TAG_LENGTH,QUIC_AEAD_TAG_LENGTH)) then
    Exit;
  FRetried:=True;
  FRetryScid:=aHeader.SrcCid;
  FRetryToken:=aHeader.Token;
  FPeerCids[0].Cid:=aHeader.SrcCid;
  InstallInitialKeys(aHeader.SrcCid);
  FSpaces[qelInitial].CryptoSend.OnLost(0,FSpaces[qelInitial].CryptoSend.NextOffset,False);
  FRecovery.DiscardSpace(qelInitial);
end;


function TQuicConnection.DecryptOneRtt(aFirstByte: Byte; aPn: QWord; const aPacket: TBytes; aHeaderLength: Integer;
  out aPayload: TBytes): Boolean;

var
  lSpace: TQuicSpace;
  lNext: TQuicKeys;

begin
  lSpace:=FSpaces[qelApplication];
  if ((aFirstByte and $04)<>0)=FRxPhase then
    Exit(QuicDecryptPayload(lSpace.Rx,aPn,aPacket,aHeaderLength,aPayload));
  if (FRxPrev<>nil) and (aPn<FRxPhaseStartPn) then
    Exit(QuicDecryptPayload(FRxPrev,aPn,aPacket,aHeaderLength,aPayload));
  lNext:=lSpace.Rx.NextGeneration;
  Result:=QuicDecryptPayload(lNext,aPn,aPacket,aHeaderLength,aPayload);
  if not Result then
    begin
    lNext.Free;
    Exit;
    end;
  FRxPrev.Free;
  FRxPrev:=lSpace.Rx;
  lSpace.Rx:=lNext;
  FRxPhase:=not FRxPhase;
  FRxPhaseStartPn:=aPn;
  if FTxPhase<>FRxPhase then
    begin
    lNext:=lSpace.Tx.NextGeneration;
    lSpace.Tx.Free;
    lSpace.Tx:=lNext;
    FTxPhase:=FRxPhase;
    FTxPhaseStartPn:=lSpace.NextPn;
    FTxPhaseAcked:=False;
    end;
end;


function TQuicConnection.DecryptPacket(aLevel: TQuicEncryptionLevel; aEarly: Boolean;
  const aHeader: TQuicPacketHeader; var aPacket: TBytes; out aPn: QWord; out aPayload: TBytes): Boolean;

var
  lSpace: TQuicSpace;
  lKeys: TQuicKeys;
  lPnLength: Integer;
  lTruncated: QWord;
  lReserved: Byte;

begin
  aPn:=0;
  aPayload:=nil;
  lSpace:=FSpaces[aLevel];
  if aEarly then
    lKeys:=FEarlyKeys
  else
    lKeys:=lSpace.Rx;
  if (lKeys=nil) or not QuicUnprotectHeader(lKeys.HP,aPacket,aHeader.PnOffset,lPnLength,lTruncated) then
    Exit(False);
  aPn:=QuicDecodePacketNumber(lSpace.LargestRecv,lTruncated,lPnLength);
  if (aLevel=qelApplication) and not aEarly then
    Result:=DecryptOneRtt(aPacket[0],aPn,aPacket,aHeader.PnOffset+lPnLength,aPayload)
  else
    Result:=QuicDecryptPayload(lKeys,aPn,aPacket,aHeader.PnOffset+lPnLength,aPayload);
  if not Result then
    begin
    if (aLevel=qelApplication) and not aEarly then
      begin
      Inc(FAuthFailures);
      if FAuthFailures>=QuicIntegrityLimit[lKeys.Suite] then
        raise EQuicError.CreateCode(QUIC_AEAD_LIMIT_REACHED,'AEAD integrity limit reached');
      end;
    Exit;
    end;
  if (aLevel=qelApplication) and not aEarly then
    lReserved:=aPacket[0] and $18
  else
    lReserved:=aPacket[0] and $0C;
  if lReserved<>0 then
    raise EQuicError.CreateCode(QUIC_PROTOCOL_VIOLATION,'Reserved header bits set');
end;


// True when aVersions contains QUIC version 1.
function ListsVersion1(const aVersions: array of Cardinal): Boolean;

var
  lVersion: Cardinal;

begin
  for lVersion in aVersions do
    if lVersion=QUIC_VERSION_1 then
      Exit(True);
  Result:=False;
end;


function TQuicConnection.ProcessPacket(const aHeader: TQuicPacketHeader; aPacket: TBytes;
  aDatagramLength: Integer): Boolean;

var
  lLevel: TQuicEncryptionLevel;
  lPn: QWord;
  lPayload: TBytes;
  lAckEliciting, lEarly: Boolean;
  lSpace: TQuicSpace;

begin
  Result:=False;
  lEarly:=False;
  case aHeader.PacketType of
    qptVersionNegotiation:
      begin
      if (not FIsServer) and (not FReceivedInitial) and not ListsVersion1(aHeader.Versions) then
        begin
        FCloseErrorCode:=QUIC_CONNECTION_REFUSED;
        FCloseReason:='No compatible QUIC version';
        SetClosed;
        end;
      Exit;
      end;
    qptRetry:
      begin
      ProcessRetry(aHeader,aPacket);
      Exit;
      end;
    qptInitial:
      begin
      if FIsServer and (aDatagramLength<QUIC_MIN_INITIAL_DATAGRAM) then
        Exit;
      lLevel:=qelInitial;
      end;
    qptHandshake:
      lLevel:=qelHandshake;
    qptOneRtt:
      lLevel:=qelApplication;
    qptZeroRtt:
      begin
      if not FIsServer or FHandshakeComplete then
        Exit;
      if (FEarlyKeys=nil) and (Length(FEarlyPending)<MaxEarlyPending) then
        Insert(aPacket,FEarlyPending,Length(FEarlyPending));
      lLevel:=qelApplication;
      lEarly:=True;
      end;
  else
    Exit;
  end;
  if not DecryptPacket(lLevel,lEarly,aHeader,aPacket,lPn,lPayload) then
    Exit;
  lSpace:=FSpaces[lLevel];
  if lSpace.Ack.IsDuplicate(lPn) then
    Exit;
  Result:=True;
  Touch;
  if Int64(lPn)>lSpace.LargestRecv then
    lSpace.LargestRecv:=lPn;
  if FState=qcsClosing then
    begin
    FClosePending:=True;
    Exit;
    end;
  if (lLevel=qelInitial) and not FReceivedInitial then
    begin
    FReceivedInitial:=True;
    FPeerInitialScid:=aHeader.SrcCid;
    SetLength(FPeerCids,1);
    FPeerCids[0].Cid:=aHeader.SrcCid;
    end;
  if FIsServer and (lLevel=qelHandshake) then
    begin
    FAddressValidated:=True;
    DiscardSpace(qelInitial);
    end;
  FProcessingEarly:=lEarly;
  try
    ProcessFrames(lLevel,lPayload,lAckEliciting);
  finally
    FProcessingEarly:=False;
  end;
  lSpace.Ack.OnPacket(lPn,lAckEliciting,lLevel<>qelApplication,FNow,FLocalParams.MaxAckDelay*1000);
  // A client probe while the server flight is unacknowledged: resend the flight now.
  if FIsServer and (lLevel=qelInitial) and lAckEliciting and (FState<qcsClosing)
     and (FRecovery.OldestUnacked(qelInitial)<>nil) then
    for lLevel:=qelInitial to qelHandshake do
      if FSpaces[lLevel].Tx<>nil then
        begin
        FSpaces[lLevel].Probes:=1;
        RequeueForProbe(lLevel);
        end;
end;


// True when a frame of type aType may be sent in a 0-RTT packet (RFC 9000 section 12.4).
function FrameAllowedEarly(aType: QWord): Boolean;

begin
  case aType of
    QUIC_FRAME_ACK,
    QUIC_FRAME_ACK_ECN,
    QUIC_FRAME_CRYPTO,
    QUIC_FRAME_NEW_TOKEN,
    QUIC_FRAME_PATH_RESPONSE,
    QUIC_FRAME_RETIRE_CONNECTION_ID,
    QUIC_FRAME_HANDSHAKE_DONE:
      Result:=False;
  else
    Result:=True;
  end;
end;


procedure TQuicConnection.ProcessFrames(aLevel: TQuicEncryptionLevel; const aPayload: TBytes;
  out aAckEliciting: Boolean);

var
  lReader: TQuicReader;
  lFrame: TQuicFrame;

begin
  aAckEliciting:=False;
  if Length(aPayload)=0 then
    raise EQuicError.CreateCode(QUIC_PROTOCOL_VIOLATION,'Packet without frames');
  lReader.InitBytes(aPayload);
  while not lReader.EOF do
    begin
    lFrame:=QuicDecodeFrame(lReader);
    if not QuicFrameAllowed(lFrame.FrameType,aLevel)
       or (FProcessingEarly and not FrameAllowedEarly(lFrame.FrameType)) then
      raise EQuicError.CreateCode(QUIC_PROTOCOL_VIOLATION,'Frame not allowed at this encryption level',
        lFrame.FrameType);
    aAckEliciting:=aAckEliciting or QuicFrameAckEliciting(lFrame.FrameType);
    ProcessFrame(aLevel,lFrame);
    if FState>=qcsClosing then
      Exit;
    end;
end;


procedure TQuicConnection.ProcessFrame(aLevel: TQuicEncryptionLevel; const aFrame: TQuicFrame);

var
  lStream: TQuicStream;
  lResponse: TQuicFrame;

begin
  case aFrame.FrameType of
    QUIC_FRAME_ACK,
    QUIC_FRAME_ACK_ECN:
      ProcessAck(aLevel,aFrame);
    QUIC_FRAME_CRYPTO:
      ProcessCrypto(aLevel,aFrame);
    QUIC_FRAME_STREAM:
      ProcessStream(aFrame);
    QUIC_FRAME_RESET_STREAM:
      ProcessResetStream(aFrame);
    QUIC_FRAME_STOP_SENDING:
      begin
      lStream:=FStreams.GetForFrame(aFrame.StreamId,True);
      if lStream<>nil then
        begin
        lStream.StopSendingReceived:=True;
        lStream.StopSendingErrorCode:=aFrame.ErrorCode;
        lStream.Reset(aFrame.ErrorCode);
        end;
      end;
    QUIC_FRAME_MAX_DATA:
      if aFrame.Value>FSendMaxData then
        FSendMaxData:=aFrame.Value;
    QUIC_FRAME_MAX_STREAM_DATA:
      begin
      lStream:=FStreams.GetForFrame(aFrame.StreamId,True);
      if (lStream<>nil) and (aFrame.Value>lStream.SendMaxData) then
        lStream.SendMaxData:=aFrame.Value;
      end;
    QUIC_FRAME_MAX_STREAMS_BIDI,
    QUIC_FRAME_MAX_STREAMS_UNI:
      FStreams.OnMaxStreams(aFrame.FrameType=QUIC_FRAME_MAX_STREAMS_BIDI,aFrame.Value);
    QUIC_FRAME_STREAM_DATA_BLOCKED:
      FStreams.GetForFrame(aFrame.StreamId,False);
    QUIC_FRAME_NEW_TOKEN:
      if FIsServer then
        raise EQuicError.CreateCode(QUIC_PROTOCOL_VIOLATION,'NEW_TOKEN from a client',aFrame.FrameType);
    QUIC_FRAME_NEW_CONNECTION_ID:
      ProcessNewConnectionId(aFrame);
    QUIC_FRAME_RETIRE_CONNECTION_ID:
      ProcessRetireConnectionId(aFrame);
    QUIC_FRAME_PATH_CHALLENGE:
      begin
      lResponse:=QuicFrame(QUIC_FRAME_PATH_RESPONSE);
      lResponse.Data:=aFrame.Data;
      Insert(lResponse,FControl,Length(FControl));
      end;
    QUIC_FRAME_CONNECTION_CLOSE,
    QUIC_FRAME_CONNECTION_CLOSE_APP:
      EnterDraining(aFrame.ErrorCode,String(TEncoding.UTF8.GetString(aFrame.Data)));
    QUIC_FRAME_HANDSHAKE_DONE:
      begin
      if FIsServer then
        raise EQuicError.CreateCode(QUIC_PROTOCOL_VIOLATION,'HANDSHAKE_DONE from a client',aFrame.FrameType);
      if not FHandshakeConfirmed then
        ConfirmHandshake;
      end;
  end;
end;


procedure TQuicConnection.ProcessAck(aLevel: TQuicEncryptionLevel; const aFrame: TQuicFrame);

var
  lAcked, lLost: TFPList;
  lPacket: TQuicSentPacket;
  lFrame: TQuicFrame;
  I: Integer;

begin
  lAcked:=TFPList.Create;
  lLost:=TFPList.Create;
  try
    FRecovery.OnAckReceived(aLevel,aFrame,FPeerParams.AckDelayExponent,FNow,lAcked,lLost);
    for I:=0 to lAcked.Count-1 do
      begin
      lPacket:=TQuicSentPacket(lAcked[I]);
      if (aLevel=qelApplication) and (lPacket.PacketNumber>=FTxPhaseStartPn) then
        FTxPhaseAcked:=True;
      for lFrame in lPacket.Frames do
        OnFrameAcked(aLevel,lFrame);
      end;
    HandleLost(aLevel,lLost);
    if (not FIsServer) and (aLevel=qelHandshake) then
      FRecovery.PeerAddressValidated:=True;
  finally
    FreePackets(lAcked);
    FreePackets(lLost);
    lAcked.Free;
    lLost.Free;
  end;
end;


procedure TQuicConnection.FreePackets(aList: TFPList);

var
  I: Integer;

begin
  for I:=0 to aList.Count-1 do
    TQuicSentPacket(aList[I]).Free;
  aList.Clear;
end;


procedure TQuicConnection.HandleLost(aLevel: TQuicEncryptionLevel; aLost: TFPList);

var
  lFrame: TQuicFrame;
  I: Integer;

begin
  for I:=0 to aLost.Count-1 do
    for lFrame in TQuicSentPacket(aLost[I]).Frames do
      OnFrameLost(aLevel,lFrame);
end;


procedure TQuicConnection.OnFrameAcked(aLevel: TQuicEncryptionLevel; const aFrame: TQuicFrame);

var
  lStream: TQuicStream;

begin
  case aFrame.FrameType of
    QUIC_FRAME_CRYPTO:
      FSpaces[aLevel].CryptoSend.OnAcked(aFrame.Offset,aFrame.Value,False);
    QUIC_FRAME_STREAM:
      begin
      lStream:=FStreams.Find(aFrame.StreamId);
      if lStream<>nil then
        lStream.Send.OnAcked(aFrame.Offset,aFrame.Value,aFrame.Fin);
      end;
    QUIC_FRAME_RESET_STREAM:
      begin
      lStream:=FStreams.Find(aFrame.StreamId);
      if lStream<>nil then
        lStream.ResetAcked:=True;
      end;
  end;
end;


procedure TQuicConnection.OnFrameLost(aLevel: TQuicEncryptionLevel; const aFrame: TQuicFrame);

var
  lStream: TQuicStream;

begin
  lStream:=FStreams.Find(aFrame.StreamId);
  case aFrame.FrameType of
    QUIC_FRAME_CRYPTO:
      FSpaces[aLevel].CryptoSend.OnLost(aFrame.Offset,aFrame.Value,False);
    QUIC_FRAME_STREAM:
      if (lStream<>nil) and not lStream.LocalReset then
        lStream.Send.OnLost(aFrame.Offset,aFrame.Value,aFrame.Fin);
    QUIC_FRAME_RESET_STREAM:
      if lStream<>nil then
        lStream.ResetPending:=True;
    QUIC_FRAME_STOP_SENDING:
      if lStream<>nil then
        lStream.StopSendingPending:=True;
    QUIC_FRAME_MAX_STREAM_DATA:
      if lStream<>nil then
        lStream.MaxStreamDataPending:=True;
    QUIC_FRAME_MAX_DATA:
      FMaxDataPending:=True;
    QUIC_FRAME_MAX_STREAMS_BIDI,
    QUIC_FRAME_MAX_STREAMS_UNI:
      FStreams.MaxStreamsPending[aFrame.FrameType=QUIC_FRAME_MAX_STREAMS_BIDI]:=True;
    QUIC_FRAME_HANDSHAKE_DONE:
      FHandshakeDonePending:=True;
    QUIC_FRAME_RETIRE_CONNECTION_ID:
      Insert(aFrame,FControl,Length(FControl));
    QUIC_FRAME_NEW_CONNECTION_ID:
      if LocalCidIndex(aFrame.SequenceNumber)>=0 then
        Insert(aFrame,FControl,Length(FControl));
  end;
end;


procedure TQuicConnection.ProcessCrypto(aLevel: TQuicEncryptionLevel; const aFrame: TQuicFrame);

var
  lRecv: TQuicReassembly;
  lData: TBytes;

begin
  lRecv:=FSpaces[aLevel].CryptoRecv;
  if aFrame.Offset+QWord(Length(aFrame.Data))>lRecv.ReadOffset+CryptoBufferLimit then
    raise EQuicError.CreateCode(QUIC_CRYPTO_BUFFER_EXCEEDED,'CRYPTO data too far ahead',aFrame.FrameType);
  lRecv.Insert(aFrame.Offset,aFrame.Data,False);
  lData:=lRecv.ReadAll;
  if Length(lData)=0 then
    Exit;
  FTLS.ProvideData(aLevel,lData);
  CheckTLS;
end;


procedure TQuicConnection.ProcessStream(const aFrame: TQuicFrame);

var
  lStream: TQuicStream;

begin
  lStream:=FStreams.GetForFrame(aFrame.StreamId,False);
  if lStream=nil then
    Exit;
  if FProcessingEarly then
    lStream.EarlyData:=True;
  Inc(FRecvData,lStream.OnData(aFrame.Offset,aFrame.Data,aFrame.Fin));
  if FRecvData>FRecvMaxData then
    raise EQuicError.CreateCode(QUIC_FLOW_CONTROL_ERROR,'Connection data beyond MAX_DATA',aFrame.FrameType);
  if FEvents<>nil then
    FEvents.QuicStreamData(Self,aFrame.StreamId);
end;


procedure TQuicConnection.ProcessResetStream(const aFrame: TQuicFrame);

var
  lStream: TQuicStream;
  lUnread: QWord;

begin
  lStream:=FStreams.GetForFrame(aFrame.StreamId,False);
  if (lStream=nil) or lStream.ResetReceived then
    Exit;
  Inc(FRecvData,lStream.OnReset(aFrame.ErrorCode,aFrame.Value));
  if FRecvData>FRecvMaxData then
    raise EQuicError.CreateCode(QUIC_FLOW_CONTROL_ERROR,'Connection data beyond MAX_DATA',aFrame.FrameType);
  lUnread:=aFrame.Value-lStream.Recv.ReadOffset;
  Inc(FRecvConsumed,lUnread);
  if FEvents<>nil then
    FEvents.QuicStreamData(Self,aFrame.StreamId);
end;


procedure TQuicConnection.ProcessNewConnectionId(const aFrame: TQuicFrame);

var
  lCid: TQuicPeerCid;
  lRetire: TQuicFrame;
  I: Integer;

begin
  for lCid in FPeerCids do
    if lCid.Sequence=aFrame.SequenceNumber then
      Exit;
  lCid.Sequence:=aFrame.SequenceNumber;
  lCid.Cid:=aFrame.ConnectionId;
  lCid.ResetToken:=aFrame.ResetToken;
  Insert(lCid,FPeerCids,Length(FPeerCids));
  if aFrame.RetirePriorTo>FRetirePriorTo then
    FRetirePriorTo:=aFrame.RetirePriorTo;
  I:=0;
  while I<Length(FPeerCids) do
    if FPeerCids[I].Sequence<FRetirePriorTo then
      begin
      lRetire:=QuicFrame(QUIC_FRAME_RETIRE_CONNECTION_ID);
      lRetire.SequenceNumber:=FPeerCids[I].Sequence;
      Insert(lRetire,FControl,Length(FControl));
      Delete(FPeerCids,I,1);
      end
    else
      Inc(I);
  if QWord(Length(FPeerCids))>FLocalParams.ActiveConnectionIdLimit then
    raise EQuicError.CreateCode(QUIC_CONNECTION_ID_LIMIT_ERROR,'Too many connection IDs',aFrame.FrameType);
end;


function TQuicConnection.LocalCidIndex(aSequence: QWord): Integer;

begin
  for Result:=0 to High(FLocalCids) do
    if FLocalCids[Result].Sequence=aSequence then
      Exit;
  Result:=-1;
end;


procedure TQuicConnection.IssueConnectionIds;

var
  lFrame: TQuicFrame;
  lLimit: QWord;

begin
  lLimit:=FPeerParams.ActiveConnectionIdLimit;
  if lLimit>MaxIssuedCids then
    lLimit:=MaxIssuedCids;
  while QWord(Length(FLocalCids))<lLimit do
    begin
    lFrame:=QuicFrame(QUIC_FRAME_NEW_CONNECTION_ID);
    lFrame.SequenceNumber:=FNextCidSequence;
    lFrame.ConnectionId:=QuicRandomBytes(QUIC_LOCAL_CID_LENGTH);
    lFrame.ResetToken:=QuicRandomBytes(QUIC_RESET_TOKEN_LENGTH);
    SetLength(FLocalCids,Length(FLocalCids)+1);
    FLocalCids[High(FLocalCids)].Sequence:=FNextCidSequence;
    FLocalCids[High(FLocalCids)].Cid:=lFrame.ConnectionId;
    Inc(FNextCidSequence);
    Insert(lFrame,FControl,Length(FControl));
    end;
end;


procedure TQuicConnection.ProcessRetireConnectionId(const aFrame: TQuicFrame);

var
  lIndex: Integer;

begin
  if aFrame.SequenceNumber>=FNextCidSequence then
    raise EQuicError.CreateCode(QUIC_PROTOCOL_VIOLATION,'Retirement of an unissued connection ID',aFrame.FrameType);
  lIndex:=LocalCidIndex(aFrame.SequenceNumber);
  if lIndex<0 then
    Exit;
  Delete(FLocalCids,lIndex,1);
  IssueConnectionIds;
end;


function TQuicConnection.StreamIsEarly(aId: QWord): Boolean;

var
  lStream: TQuicStream;

begin
  lStream:=FStreams.Find(aId);
  Result:=(lStream<>nil) and lStream.EarlyData;
end;


function TQuicConnection.OwnsCid(const aCid: TBytes): Boolean;

var
  lCid: TQuicLocalCid;

begin
  for lCid in FLocalCids do
    if QuicBytesEqual(lCid.Cid,aCid) then
      Exit(True);
  Result:=False;
end;


{ Timers }

function TQuicConnection.NextTimeout: QWord;

  procedure Consider(aTime: QWord);

  begin
    if (aTime<>0) and (aTime<Result) then
      Result:=aTime;
  end;

var
  lLevel: TQuicEncryptionLevel;

begin
  Result:=High(QWord);
  if FState=qcsClosed then
    Exit;
  if FState>=qcsClosing then
    Exit(FCloseDeadline);
  Consider(FRecovery.Timer);
  for lLevel:=Low(TQuicEncryptionLevel) to High(TQuicEncryptionLevel) do
    if not FSpaces[lLevel].Discarded then
      Consider(FSpaces[lLevel].Ack.Deadline);
  if FIdleTimeout>0 then
    Consider(IdleDeadline);
end;


procedure TQuicConnection.HandleTimeout(aNow: QWord);

var
  lLost: TFPList;
  lProbe: Boolean;
  lSpace, lLevel: TQuicEncryptionLevel;

begin
  FNow:=aNow;
  if FState=qcsClosed then
    Exit;
  if FState>=qcsClosing then
    begin
    if aNow>=FCloseDeadline then
      SetClosed;
    Exit;
    end;
  if (FIdleTimeout>0) and (aNow>=IdleDeadline) then
    begin
    FCloseReason:='Idle timeout';
    SetClosed;
    Exit;
    end;
  if (FRecovery.Timer=0) or (aNow<FRecovery.Timer) then
    Exit;
  lLost:=TFPList.Create;
  try
    FRecovery.OnTimeout(aNow,lLost,lProbe,lSpace);
    HandleLost(lSpace,lLost);
    FreePackets(lLost);
  finally
    lLost.Free;
  end;
  if not lProbe then
    Exit;
  // RFC 9002 6.2.4: probe all handshake spaces with data in flight or pending, not only the expired one.
  for lLevel:=Low(TQuicEncryptionLevel) to High(TQuicEncryptionLevel) do
    if (lLevel=lSpace) or ((lLevel<>qelApplication) and (FSpaces[lLevel].Tx<>nil)
       and ((FRecovery.OldestUnacked(lLevel)<>nil) or FSpaces[lLevel].CryptoSend.HasPending)) then
      begin
      FSpaces[lLevel].Probes:=2;
      RequeueForProbe(lLevel);
      end;
end;


procedure TQuicConnection.RequeueForProbe(aLevel: TQuicEncryptionLevel);

var
  lPacket: TQuicSentPacket;
  lFrame: TQuicFrame;
  I: Integer;

begin
  if aLevel=qelApplication then
    begin
    lPacket:=FRecovery.OldestUnacked(aLevel);
    if lPacket<>nil then
      for lFrame in lPacket.Frames do
        OnFrameLost(aLevel,lFrame);
    end
  else
    for I:=0 to FRecovery.SentPackets(aLevel).Count-1 do
      for lFrame in TQuicSentPacket(FRecovery.SentPackets(aLevel)[I]).Frames do
        OnFrameLost(aLevel,lFrame);
end;


{ Sending }

function TQuicConnection.HeaderOverhead(aLevel: TQuicEncryptionLevel; aPnLength: Integer): Integer;

begin
  if (aLevel=qelApplication) and not IsEarly(aLevel) then
    Result:=1+Length(PeerCid)+aPnLength
  else
    begin
    Result:=1+4+1+Length(PeerCid)+1+Length(FLocalCid)+2+aPnLength;
    if aLevel=qelInitial then
      Inc(Result,QuicVarIntSize(Length(FRetryToken))+Length(FRetryToken));
    end;
  Inc(Result,QUIC_AEAD_TAG_LENGTH);
end;


procedure TQuicConnection.AddFrame(var aWriter: TQuicWriter; var aFrames: TQuicFrameArray; const aFrame: TQuicFrame);

var
  lRecord: TQuicFrame;

begin
  QuicEncodeFrame(aWriter,aFrame);
  lRecord:=aFrame;
  if (aFrame.FrameType=QUIC_FRAME_STREAM) or (aFrame.FrameType=QUIC_FRAME_CRYPTO) then
    begin
    lRecord.Value:=Length(aFrame.Data);
    lRecord.Data:=nil;
    end;
  lRecord.AckRanges:=nil;
  Insert(lRecord,aFrames,Length(aFrames));
end;


function TQuicConnection.LevelHasData(aLevel: TQuicEncryptionLevel): Boolean;

var
  I: Integer;

begin
  Result:=(FSpaces[aLevel].Probes>0) or FSpaces[aLevel].CryptoSend.HasPending;
  if Result or (aLevel<>qelApplication) then
    Exit;
  if IsEarly(aLevel) then
    begin
    for I:=0 to FStreams.Count-1 do
      Result:=Result or FStreams[I].WantsToSend;
    Exit;
    end;
  Result:=FHandshakeDonePending or FMaxDataPending or (Length(FControl)>0)
          or FStreams.MaxStreamsPending[True] or FStreams.MaxStreamsPending[False];
  for I:=0 to FStreams.Count-1 do
    with FStreams[I] do
      Result:=Result or ResetPending or StopSendingPending or MaxStreamDataPending or WantsToSend;
end;


procedure TQuicConnection.AddControlFrames(var aWriter: TQuicWriter; var aFrames: TQuicFrameArray;
  aBudget: Integer);

var
  lFrame: TQuicFrame;
  lStream: TQuicStream;
  lBidi: Boolean;
  I: Integer;

  function Fits(const aCandidate: TQuicFrame): Boolean;

  begin
    Result:=aWriter.Length+QuicFrameSize(aCandidate)<=aBudget;
    if Result then
      AddFrame(aWriter,aFrames,aCandidate);
  end;

begin
  if FHandshakeDonePending and Fits(QuicFrame(QUIC_FRAME_HANDSHAKE_DONE)) then
    FHandshakeDonePending:=False;
  while (Length(FControl)>0) and Fits(FControl[0]) do
    Delete(FControl,0,1);
  if FMaxDataPending then
    begin
    lFrame:=QuicFrame(QUIC_FRAME_MAX_DATA);
    lFrame.Value:=FRecvMaxData;
    FMaxDataPending:=not Fits(lFrame);
    end;
  for lBidi:=False to True do
    if FStreams.MaxStreamsPending[lBidi] then
      begin
      if lBidi then
        lFrame:=QuicFrame(QUIC_FRAME_MAX_STREAMS_BIDI)
      else
        lFrame:=QuicFrame(QUIC_FRAME_MAX_STREAMS_UNI);
      lFrame.Value:=FStreams.LocalMaxStreams(lBidi);
      FStreams.MaxStreamsPending[lBidi]:=not Fits(lFrame);
      end;
  for I:=0 to FStreams.Count-1 do
    begin
    lStream:=FStreams[I];
    if lStream.ResetPending then
      begin
      lFrame:=QuicFrame(QUIC_FRAME_RESET_STREAM);
      lFrame.StreamId:=lStream.Id;
      lFrame.ErrorCode:=lStream.LocalResetCode;
      lFrame.Value:=lStream.Send.NextOffset;
      lStream.ResetPending:=not Fits(lFrame);
      end;
    if lStream.StopSendingPending then
      begin
      lFrame:=QuicFrame(QUIC_FRAME_STOP_SENDING);
      lFrame.StreamId:=lStream.Id;
      lFrame.ErrorCode:=lStream.LocalStopCode;
      lStream.StopSendingPending:=not Fits(lFrame);
      end;
    if lStream.MaxStreamDataPending then
      begin
      lFrame:=QuicFrame(QUIC_FRAME_MAX_STREAM_DATA);
      lFrame.StreamId:=lStream.Id;
      lFrame.Value:=lStream.RecvMaxData;
      lStream.MaxStreamDataPending:=not Fits(lFrame);
      end;
    end;
end;


procedure TQuicConnection.AddStreamFrame(aStream: TQuicStream; var aWriter: TQuicWriter;
  var aFrames: TQuicFrameArray; aBudget: Integer);

var
  lFrame: TQuicFrame;
  lRoom: Integer;
  lLimit, lBefore: QWord;

begin
  lFrame:=QuicFrame(QUIC_FRAME_STREAM);
  lFrame.StreamId:=aStream.Id;
  lRoom:=aBudget-aWriter.Length-1-QuicVarIntSize(aStream.Id)-8-2;
  lLimit:=aStream.Send.NextOffset+(FSendMaxData-FSentData);
  if lLimit>aStream.SendMaxData then
    lLimit:=aStream.SendMaxData;
  lBefore:=aStream.Send.NextOffset;
  if aStream.Send.NextChunk(lRoom,lLimit,lFrame.Offset,lFrame.Data,lFrame.Fin) then
    begin
    Inc(FSentData,aStream.Send.NextOffset-lBefore);
    AddFrame(aWriter,aFrames,lFrame);
    end;
end;


procedure TQuicConnection.AddStreamFrames(var aWriter: TQuicWriter; var aFrames: TQuicFrameArray; aBudget: Integer);

var
  lStream: TQuicStream;
  lCount, lIndex: Integer;

begin
  // Streams that have sent nothing yet go first, in stream ID order; the others share the rest in turn.
  for lIndex:=0 to FStreams.Count-1 do
    begin
    lStream:=FStreams[lIndex];
    if lStream.WantsToSend and (lStream.Send.NextOffset=0) then
      AddStreamFrame(lStream,aWriter,aFrames,aBudget);
    end;
  lCount:=FStreams.Count;
  while lCount>0 do
    begin
    Dec(lCount);
    if FStreamCursor>=FStreams.Count then
      FStreamCursor:=0;
    lIndex:=FStreamCursor;
    Inc(FStreamCursor);
    lStream:=FStreams[lIndex];
    if lStream.WantsToSend then
      AddStreamFrame(lStream,aWriter,aFrames,aBudget);
    end;
end;


function TQuicConnection.BuildPayload(aLevel: TQuicEncryptionLevel; aBudget: Integer; var aWriter: TQuicWriter;
  var aFrames: TQuicFrameArray): Boolean;

var
  lSpace: TQuicSpace;
  lFrame: TQuicFrame;
  lCanSend, lHasData, lElicit: Boolean;
  lRoom: Integer;

begin
  lSpace:=FSpaces[aLevel];
  if (lSpace.Probes>0) and (aLevel<>qelApplication) and not lSpace.CryptoSend.HasPending then
    RequeueForProbe(aLevel);
  lHasData:=LevelHasData(aLevel);
  if lSpace.Ack.HasNewPackets and (lSpace.Ack.AckDue(FNow) or lHasData) and not IsEarly(aLevel) then
    begin
    lFrame:=lSpace.Ack.BuildFrame(FNow,FLocalParams.AckDelayExponent);
    if QuicFrameSize(lFrame)<=aBudget then
      AddFrame(aWriter,aFrames,lFrame);
    end;
  lCanSend:=(lSpace.Probes>0) or FCC.CanSend(QUIC_MAX_DATAGRAM);
  if lCanSend and lHasData then
    begin
    while True do
      begin
      lFrame:=QuicFrame(QUIC_FRAME_CRYPTO);
      lRoom:=aBudget-aWriter.Length-1-8-2;
      if not lSpace.CryptoSend.NextChunk(lRoom,High(QWord),lFrame.Offset,lFrame.Data,lFrame.Fin) then
        Break;
      AddFrame(aWriter,aFrames,lFrame);
      end;
    if aLevel=qelApplication then
      begin
      if not IsEarly(aLevel) then
        AddControlFrames(aWriter,aFrames,aBudget);
      AddStreamFrames(aWriter,aFrames,aBudget);
      end;
    end;
  if lSpace.Probes>0 then
    begin
    Dec(lSpace.Probes);
    lElicit:=False;
    for lFrame in aFrames do
      lElicit:=lElicit or QuicFrameAckEliciting(lFrame.FrameType);
    if not lElicit then
      AddFrame(aWriter,aFrames,QuicFrame(QUIC_FRAME_PING));
    end;
  Result:=Length(aFrames)>0;
end;


function TQuicConnection.SealPacket(aLevel: TQuicEncryptionLevel; aPn: QWord; aPnLength: Integer;
  const aPayload: TBytes): TBytes;

var
  lWriter: TQuicWriter;
  lPnOffset: Integer;
  lKeys: TQuicKeys;

begin
  lWriter.Init;
  lKeys:=FSpaces[aLevel].Tx;
  if IsEarly(aLevel) then
    begin
    lKeys:=FEarlyKeys;
    FEarlySent:=True;
    lPnOffset:=QuicWriteLongHeader(lWriter,qptZeroRtt,PeerCid,FLocalCid,nil,aPn,aPnLength,
      Length(aPayload)+QUIC_AEAD_TAG_LENGTH);
    end
  else if aLevel=qelApplication then
    lPnOffset:=QuicWriteShortHeader(lWriter,PeerCid,FTxPhase,aPn,aPnLength)
  else
    lPnOffset:=QuicWriteLongHeader(lWriter,LongPacketTypes[aLevel],PeerCid,FLocalCid,FRetryToken,aPn,aPnLength,
      Length(aPayload)+QUIC_AEAD_TAG_LENGTH);
  lWriter.WriteBytes(aPayload);
  Result:=QuicProtectPacket(lKeys,aPn,lWriter.ToBytes,lPnOffset,aPnLength);
end;


function TQuicConnection.BuildClosePackets(out aData: TBytes): Boolean;

var
  lLevel: TQuicEncryptionLevel;
  lFrame: TQuicFrame;
  lWriter: TQuicWriter;
  lSpace: TQuicSpace;

begin
  aData:=nil;
  Result:=FClosePending;
  if not Result then
    Exit;
  FClosePending:=False;
  for lLevel:=Low(TQuicEncryptionLevel) to High(TQuicEncryptionLevel) do
    begin
    lSpace:=FSpaces[lLevel];
    if lSpace.Tx=nil then
      Continue;
    lFrame:=FCloseFrame;
    if (lLevel<>qelApplication) and (lFrame.FrameType=QUIC_FRAME_CONNECTION_CLOSE_APP) then
      begin
      lFrame:=QuicFrame(QUIC_FRAME_CONNECTION_CLOSE);
      lFrame.ErrorCode:=QUIC_APPLICATION_ERROR;
      end;
    lWriter.Init;
    QuicEncodeFrame(lWriter,lFrame);
    lWriter.WriteZeros(3);
    aData:=Concat(aData,SealPacket(lLevel,lSpace.NextPn,4,lWriter.ToBytes));
    Inc(lSpace.NextPn);
    end;
end;


function TQuicConnection.NextDatagram(aNow: QWord; out aData: TBytes): Boolean;

var
  lLevel, lLast: TQuicEncryptionLevel;
  lSpace: TQuicSpace;
  lPayloads: TLevelWriters;
  lFrames: array[TQuicEncryptionLevel] of TQuicFrameArray;
  lPnLengths: array[TQuicEncryptionLevel] of Integer;
  lUsed: array[TQuicEncryptionLevel] of Boolean;
  lSize, lBudget, lMax, lPad: Integer;
  lPacket: TBytes;
  lSent: TQuicSentPacket;
  lAckEliciting, lPadInitial, lAny, lPadded: Boolean;
  lFrame: TQuicFrame;

begin
  FNow:=aNow;
  aData:=nil;
  if FState>=qcsDraining then
    Exit(False);
  CheckConfidentialityLimit;
  if FState=qcsClosing then
    begin
    Result:=BuildClosePackets(aData);
    Exit;
    end;
  lPayloads:=Default(TLevelWriters);
  lMax:=QUIC_MAX_DATAGRAM;
  if FIsServer and not FAddressValidated and (FBytesSent+QUIC_MAX_DATAGRAM>AmplificationLimit) then
    Exit(False);
  lSize:=0;
  lAny:=False;
  lPadInitial:=False;
  lLast:=qelInitial;
  for lLevel:=Low(TQuicEncryptionLevel) to High(TQuicEncryptionLevel) do
    begin
    lSpace:=FSpaces[lLevel];
    lUsed[lLevel]:=False;
    lFrames[lLevel]:=nil;
    lPayloads[lLevel].Init;
    if (lSpace.Tx=nil) and not IsEarly(lLevel) then
      Continue;
    lPnLengths[lLevel]:=QuicPacketNumberLength(lSpace.NextPn,FRecovery.LargestAcked(lLevel));
    lBudget:=lMax-lSize-HeaderOverhead(lLevel,lPnLengths[lLevel]);
    if lBudget<16 then
      Continue;
    if not BuildPayload(lLevel,lBudget,lPayloads[lLevel],lFrames[lLevel]) then
      Continue;
    lUsed[lLevel]:=True;
    lAny:=True;
    lLast:=lLevel;
    if lPayloads[lLevel].Length+lPnLengths[lLevel]<4 then
      lPayloads[lLevel].WriteZeros(4-lPnLengths[lLevel]-lPayloads[lLevel].Length);
    for lFrame in lFrames[lLevel] do
      if (lLevel=qelInitial) and QuicFrameAckEliciting(lFrame.FrameType) then
        lPadInitial:=True;
    Inc(lSize,lPayloads[lLevel].Length+HeaderOverhead(lLevel,lPnLengths[lLevel]));
    end;
  Result:=lAny;
  if not lAny then
    Exit;
  lPadded:=(lPadInitial or not FIsServer and lUsed[qelInitial]) and (lSize<QUIC_MIN_INITIAL_DATAGRAM);
  if lPadded then
    begin
    lPad:=QUIC_MIN_INITIAL_DATAGRAM-lSize;
    lPayloads[lLast].WriteZeros(lPad);
    end
  else if (lLast=qelApplication) and (lSize<QUIC_MAX_DATAGRAM) and (lSize>QUIC_MAX_DATAGRAM-FullDatagramSlack) then
    lPayloads[lLast].WriteZeros(QUIC_MAX_DATAGRAM-lSize);
  for lLevel:=Low(TQuicEncryptionLevel) to High(TQuicEncryptionLevel) do
    begin
    if not lUsed[lLevel] then
      Continue;
    lSpace:=FSpaces[lLevel];
    lPacket:=SealPacket(lLevel,lSpace.NextPn,lPnLengths[lLevel],lPayloads[lLevel].ToBytes);
    aData:=Concat(aData,lPacket);
    lAckEliciting:=False;
    for lFrame in lFrames[lLevel] do
      lAckEliciting:=lAckEliciting or QuicFrameAckEliciting(lFrame.FrameType);
    lSent:=TQuicSentPacket.Create;
    lSent.PacketNumber:=lSpace.NextPn;
    lSent.Size:=Length(lPacket);
    lSent.AckEliciting:=lAckEliciting;
    lSent.InFlight:=lAckEliciting or (lPadded and (lLevel=lLast));
    lSent.Frames:=lFrames[lLevel];
    Inc(lSpace.NextPn);
    FRecovery.OnPacketSent(lLevel,lSent,aNow);
    if lAckEliciting then
      Touch;
    end;
  Inc(FBytesSent,Length(aData));
  if (not FIsServer) and lUsed[qelHandshake] then
    DiscardSpace(qelInitial);
  UpdateRecoveryState;
end;


{ Streams }

function TQuicConnection.OpenStream(aBidi: Boolean; out aId: QWord): Boolean;

var
  lStream: TQuicStream;

begin
  aId:=0;
  lStream:=nil;
  if (FHandshakeComplete and (FState=qcsEstablished)) or CanSendEarly then
    lStream:=FStreams.OpenLocal(aBidi);
  Result:=lStream<>nil;
  if Result then
    aId:=lStream.Id;
end;


function TQuicConnection.AcceptStream(out aId: QWord): Boolean;

begin
  Result:=FStreams.Accept(aId);
end;


function TQuicConnection.WriteStream(aId: QWord; const aData: TBytes; aFin: Boolean): Boolean;

var
  lStream: TQuicStream;

begin
  lStream:=FStreams.Find(aId);
  Result:=(lStream<>nil) and lStream.CanSend and not lStream.LocalReset and not lStream.Send.Finished
          and (FState<qcsClosing);
  if not Result then
    Exit;
  lStream.Send.Write(aData);
  if aFin then
    lStream.Send.Finish;
end;


function TQuicConnection.ReadStream(aId: QWord; aBuffer: PByte; aCount: Integer): Integer;

var
  lStream: TQuicStream;

begin
  lStream:=FStreams.Find(aId);
  if lStream=nil then
    Exit(0);
  Result:=lStream.Read(aBuffer,aCount);
  Inc(FRecvConsumed,Result);
  if FRecvMaxData-FRecvConsumed<FLocalParams.InitialMaxData div 2 then
    begin
    FRecvMaxData:=FRecvConsumed+FLocalParams.InitialMaxData;
    FMaxDataPending:=True;
    end;
  FStreams.RemoveClosed;
end;


function TQuicConnection.StreamFinished(aId: QWord): Boolean;

var
  lStream: TQuicStream;

begin
  lStream:=FStreams.Find(aId);
  Result:=(lStream=nil) or (lStream.Recv.IsFinished and not lStream.ResetReceived);
end;


function TQuicConnection.StreamReset(aId: QWord; out aErrorCode: QWord): Boolean;

var
  lStream: TQuicStream;

begin
  aErrorCode:=0;
  lStream:=FStreams.Find(aId);
  Result:=(lStream<>nil) and lStream.ResetReceived;
  if not Result then
    Exit;
  aErrorCode:=lStream.ResetErrorCode;
  lStream.ResetDelivered:=True;
  FStreams.RemoveClosed;
end;


function TQuicConnection.StreamBuffered(aId: QWord): Integer;

var
  lStream: TQuicStream;

begin
  lStream:=FStreams.Find(aId);
  if lStream=nil then
    Exit(0);
  Result:=lStream.Send.Buffered;
end;


procedure TQuicConnection.ResetStream(aId, aErrorCode: QWord);

var
  lStream: TQuicStream;

begin
  lStream:=FStreams.Find(aId);
  if (lStream<>nil) and lStream.CanSend then
    lStream.Reset(aErrorCode);
end;


procedure TQuicConnection.StopSending(aId, aErrorCode: QWord);

var
  lStream: TQuicStream;

begin
  lStream:=FStreams.Find(aId);
  if (lStream<>nil) and lStream.CanReceive then
    lStream.StopSending(aErrorCode);
end;


function TQuicConnection.InitiateKeyUpdate: Boolean;

var
  lSpace: TQuicSpace;
  lNext: TQuicKeys;

begin
  lSpace:=FSpaces[qelApplication];
  Result:=FHandshakeConfirmed and FTxPhaseAcked and (FTxPhase=FRxPhase) and (FState=qcsEstablished);
  if not Result then
    Exit;
  lNext:=lSpace.Tx.NextGeneration;
  lSpace.Tx.Free;
  lSpace.Tx:=lNext;
  FTxPhase:=not FTxPhase;
  FTxPhaseStartPn:=lSpace.NextPn;
  FTxPhaseAcked:=False;
end;


function TQuicConnection.Alpn: AnsiString;

begin
  Result:=FTLS.NegotiatedAlpn;
end;


end.
