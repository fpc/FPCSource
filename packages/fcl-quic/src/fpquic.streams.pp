{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    QUIC streams (RFC 9000 sections 2-4): send/receive buffers, stream flow control and stream limits.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpQuic.Streams;

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, System.Classes, FpQuic.Consts, FpQuic.Buffers,
  FpQuic.TransportParams;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils, Classes, FpQuic.Consts, FpQuic.Buffers, FpQuic.TransportParams;
{$ENDIF FPC_DOTTEDUNITS}

var
  // Largest number of separate received ranges a reassembly buffer keeps.
  QuicMaxReceiveRanges: Integer = 1024;

type
  // Receive buffer that delivers bytes in order from out-of-order fragments.
  TQuicReassembly = class
  private
    FReadOffset: QWord;
    FBuffer: TBytes;
    FReceived: TQuicRangeSet;
    FHighestOffset: QWord;
    FFinalSize: QWord;
    FHasFinalSize: Boolean;
    procedure CheckFinalSize(aEnd: QWord; aFin: Boolean);
    procedure Consume(aCount: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    // Adds aData at aOffset; aFin marks the end of the stream. Data received twice is stored once.
    // Raises FINAL_SIZE_ERROR on inconsistencies and INTERNAL_ERROR above QuicMaxReceiveRanges ranges.
    procedure Insert(aOffset: QWord; const aData: TBytes; aFin: Boolean);
    // Fixes the final size without data (RESET_STREAM); raises FINAL_SIZE_ERROR on inconsistencies.
    procedure SetFinalSize(aFinalSize: QWord);
    // Copies up to aCount in-order bytes to aBuffer and returns the number copied.
    function Read(aBuffer: PByte; aCount: Integer): Integer;
    // Returns and consumes all in-order bytes.
    function ReadAll: TBytes;
    // Number of in-order bytes ready to read.
    function Available: Integer;
    // Number of bytes held from the read offset up to the highest stored byte, gaps included.
    function Buffered: Integer;
    // True when all bytes up to the final size were read.
    function IsFinished: Boolean;
    // Offset of the next byte to read.
    property ReadOffset: QWord read FReadOffset;
    // Highest offset received.
    property HighestOffset: QWord read FHighestOffset;
    // True when the final size is known.
    property HasFinalSize: Boolean read FHasFinalSize;
    // Final size of the stream, valid when HasFinalSize.
    property FinalSize: QWord read FFinalSize;
  end;

  // Send buffer keeping data until it is acknowledged, with retransmission of lost ranges.
  TQuicSendBuffer = class
  private
    FData: TBytes;
    FBaseOffset: QWord;
    FNextOffset: QWord;
    FLost: TQuicRangeSet;
    FAcked: TQuicRangeSet;
    FFin: Boolean;
    FFinSent: Boolean;
    FFinAcked: Boolean;
    FFinLost: Boolean;
    function GetData(aOffset: QWord; aLength: Integer): TBytes;
  public
    constructor Create;
    destructor Destroy; override;
    // Appends application data.
    procedure Write(const aData: TBytes);
    // Marks the end of the data.
    procedure Finish;
    // Offset just after the last written byte.
    function EndOffset: QWord;
    // Returns the next frame payload: lost data first, then new data below aMaxOffset, at most aMaxLength bytes.
    // False when nothing can be sent.
    function NextChunk(aMaxLength: Integer; aMaxOffset: QWord; out aOffset: QWord; out aData: TBytes;
      out aFin: Boolean): Boolean;
    // Records the acknowledgement of a sent range.
    procedure OnAcked(aOffset: QWord; aLength: Integer; aFin: Boolean);
    // Schedules a sent range for retransmission.
    procedure OnLost(aOffset: QWord; aLength: Integer; aFin: Boolean);
    // True when there is lost or unsent data, or an unsent FIN, regardless of flow control.
    function HasPending: Boolean;
    // True when the FIN and all data were acknowledged.
    function AllAcked: Boolean;
    // Number of written bytes not yet acknowledged.
    function Buffered: Integer;
    // Offset of the first byte never sent.
    property NextOffset: QWord read FNextOffset;
    // True once Finish was called.
    property Finished: Boolean read FFin;
  end;

  // One QUIC stream with its flow control and reset state.
  TQuicStream = class
  private
    FId: QWord;
    FCanSend: Boolean;
    FCanReceive: Boolean;
    FSend: TQuicSendBuffer;
    FRecv: TQuicReassembly;
    FRecvWindow: QWord;
  public
    // Peer limit on the send offset (MAX_STREAM_DATA).
    SendMaxData: QWord;
    // Receive offset limit advertised to the peer.
    RecvMaxData: QWord;
    // True when a MAX_STREAM_DATA frame must be sent.
    MaxStreamDataPending: Boolean;
    // RESET_STREAM received from the peer, its error code, and whether the application was told.
    ResetReceived: Boolean;
    ResetErrorCode: QWord;
    ResetDelivered: Boolean;
    // True when the peer sent data on the stream in 0-RTT packets.
    EarlyData: Boolean;
    // STOP_SENDING received from the peer and its error code.
    StopSendingReceived: Boolean;
    StopSendingErrorCode: QWord;
    // Local RESET_STREAM: requested, frame pending, acknowledged, error code.
    LocalReset: Boolean;
    ResetPending: Boolean;
    ResetAcked: Boolean;
    LocalResetCode: QWord;
    // Local STOP_SENDING: requested, frame pending, error code.
    LocalStopSending: Boolean;
    StopSendingPending: Boolean;
    LocalStopCode: QWord;
    // Creates a stream; aSendMax and aRecvMax are the initial flow control limits.
    constructor Create(aId: QWord; aCanSend, aCanReceive: Boolean; aSendMax, aRecvMax: QWord);
    destructor Destroy; override;
    // Handles STREAM frame data; returns the increase of the highest received offset.
    function OnData(aOffset: QWord; const aData: TBytes; aFin: Boolean): QWord;
    // Handles RESET_STREAM; returns the increase of the highest received offset.
    function OnReset(aErrorCode, aFinalSize: QWord): QWord;
    // Reads received data and updates the receive window; returns the number of bytes read.
    function Read(aBuffer: PByte; aCount: Integer): Integer;
    // Requests a RESET_STREAM with aErrorCode; unsent data is dropped.
    procedure Reset(aErrorCode: QWord);
    // Requests a STOP_SENDING with aErrorCode.
    procedure StopSending(aErrorCode: QWord);
    // True when there are stream frames to send within the stream limit.
    function WantsToSend: Boolean;
    // True when both directions are finished and the stream can be forgotten.
    function IsClosed: Boolean;
    // Stream identifier.
    property Id: QWord read FId;
    // True when this endpoint can send on the stream.
    property CanSend: Boolean read FCanSend;
    // True when this endpoint can receive on the stream.
    property CanReceive: Boolean read FCanReceive;
    // Send buffer.
    property Send: TQuicSendBuffer read FSend;
    // Receive buffer.
    property Recv: TQuicReassembly read FRecv;
  end;

  // All streams of a connection with stream ID allocation and stream count limits.
  TQuicStreamMap = class
  private
    FIsServer: Boolean;
    FStreams: TFPList;
    FLocal: TQuicTransportParams;
    FPeer: TQuicTransportParams;
    FPeerMaxStreams: array[Boolean] of QWord;
    FLocalOpened: array[Boolean] of QWord;
    FLocalMaxStreams: array[Boolean] of QWord;
    FRemoteOpened: array[Boolean] of QWord;
    FAccept: array of QWord;
    function GetCount: Integer;
    function GetStream(aIndex: Integer): TQuicStream;
    function CreateStream(aId: QWord): TQuicStream;
  public
    // True when MAX_STREAMS (bidirectional: True, unidirectional: False) must be sent.
    MaxStreamsPending: array[Boolean] of Boolean;
    // Creates the map with the local transport parameters.
    constructor Create(aIsServer: Boolean; const aLocal: TQuicTransportParams);
    destructor Destroy; override;
    // Sets the peer transport parameters that give the initial send limits.
    procedure SetPeerParams(const aPeer: TQuicTransportParams);
    // Raises the number of streams this endpoint may open (MAX_STREAMS).
    procedure OnMaxStreams(aBidi: Boolean; aValue: QWord);
    // Opens a local stream; nil when the peer stream limit is reached.
    function OpenLocal(aBidi: Boolean): TQuicStream;
    // Returns the stream aId for a frame received from the peer; nil for a stream that was already closed.
    // aSending is True for frames about the sending direction of this endpoint (MAX_STREAM_DATA, STOP_SENDING).
    // Raises STREAM_STATE_ERROR or STREAM_LIMIT_ERROR for invalid stream IDs.
    function GetForFrame(aId: QWord; aSending: Boolean): TQuicStream;
    // Returns the stream aId or nil.
    function Find(aId: QWord): TQuicStream;
    // Takes the next stream opened by the peer; False when none.
    function Accept(out aId: QWord): Boolean;
    // Frees closed streams and grants new stream credit to the peer.
    procedure RemoveClosed;
    // Number of streams this endpoint may open towards the peer.
    function PeerMaxStreams(aBidi: Boolean): QWord;
    // Current stream limit granted to the peer.
    function LocalMaxStreams(aBidi: Boolean): QWord;
    // Number of open streams.
    property Count: Integer read GetCount;
    // Open streams.
    property Streams[aIndex: Integer]: TQuicStream read GetStream; default;
  end;

// True for a bidirectional stream ID.
function QuicStreamIsBidi(aId: QWord): Boolean;

// True when stream aId was initiated by a server.
function QuicStreamIsServerInitiated(aId: QWord): Boolean;

implementation

function QuicStreamIsBidi(aId: QWord): Boolean;

begin
  Result:=(aId and 2)=0;
end;


function QuicStreamIsServerInitiated(aId: QWord): Boolean;

begin
  Result:=(aId and 1)=1;
end;


{ TQuicReassembly }

constructor TQuicReassembly.Create;

begin
  FReceived:=TQuicRangeSet.Create;
end;


destructor TQuicReassembly.Destroy;

begin
  FReceived.Free;
  inherited Destroy;
end;


procedure TQuicReassembly.Consume(aCount: Integer);

begin
  if aCount=0 then
    Exit;
  Delete(FBuffer,0,aCount);
  FReceived.Remove(FReadOffset,FReadOffset+QWord(aCount)-1);
  Inc(FReadOffset,aCount);
end;


procedure TQuicReassembly.CheckFinalSize(aEnd: QWord; aFin: Boolean);

begin
  if FHasFinalSize and ((aEnd>FFinalSize) or (aFin and (aEnd<>FFinalSize))) then
    raise EQuicError.CreateCode(QUIC_FINAL_SIZE_ERROR,'Data inconsistent with the final size');
  if aFin and (aEnd<FHighestOffset) then
    raise EQuicError.CreateCode(QUIC_FINAL_SIZE_ERROR,'Final size below received data');
  if aFin then
    begin
    FHasFinalSize:=True;
    FFinalSize:=aEnd;
    end;
  if aEnd>FHighestOffset then
    FHighestOffset:=aEnd;
end;


procedure TQuicReassembly.Insert(aOffset: QWord; const aData: TBytes; aFin: Boolean);

var
  lEnd, lSkip: QWord;

begin
  lEnd:=aOffset+QWord(Length(aData));
  CheckFinalSize(lEnd,aFin);
  if (Length(aData)=0) or (lEnd<=FReadOffset) then
    Exit;
  lSkip:=0;
  if aOffset<FReadOffset then
    lSkip:=FReadOffset-aOffset;
  if lEnd-FReadOffset>QWord(Length(FBuffer)) then
    SetLength(FBuffer,lEnd-FReadOffset);
  Move(aData[lSkip],FBuffer[aOffset+lSkip-FReadOffset],QWord(Length(aData))-lSkip);
  FReceived.Add(aOffset+lSkip,lEnd-1);
  if FReceived.Count>QuicMaxReceiveRanges then
    raise EQuicError.CreateCode(QUIC_INTERNAL_ERROR,'Too many gaps in received data');
end;


procedure TQuicReassembly.SetFinalSize(aFinalSize: QWord);

begin
  CheckFinalSize(aFinalSize,True);
end;


function TQuicReassembly.Read(aBuffer: PByte; aCount: Integer): Integer;

begin
  Result:=Available;
  if Result>aCount then
    Result:=aCount;
  if Result<=0 then
    Exit(0);
  Move(FBuffer[0],aBuffer^,Result);
  Consume(Result);
end;


function TQuicReassembly.ReadAll: TBytes;

begin
  Result:=Copy(FBuffer,0,Available);
  Consume(Length(Result));
end;


function TQuicReassembly.Available: Integer;

begin
  if (FReceived.Count>0) and (FReceived[0].Low=FReadOffset) then
    Result:=FReceived[0].High-FReadOffset+1
  else
    Result:=0;
end;


function TQuicReassembly.Buffered: Integer;

begin
  Result:=Length(FBuffer);
end;


function TQuicReassembly.IsFinished: Boolean;

begin
  Result:=FHasFinalSize and (FReadOffset=FFinalSize);
end;


{ TQuicSendBuffer }

constructor TQuicSendBuffer.Create;

begin
  FLost:=TQuicRangeSet.Create;
  FAcked:=TQuicRangeSet.Create;
end;


destructor TQuicSendBuffer.Destroy;

begin
  FLost.Free;
  FAcked.Free;
  inherited Destroy;
end;


function TQuicSendBuffer.GetData(aOffset: QWord; aLength: Integer): TBytes;

begin
  Result:=Copy(FData,aOffset-FBaseOffset,aLength);
end;


procedure TQuicSendBuffer.Write(const aData: TBytes);

begin
  FData:=Concat(FData,aData);
end;


procedure TQuicSendBuffer.Finish;

begin
  FFin:=True;
end;


function TQuicSendBuffer.EndOffset: QWord;

begin
  Result:=FBaseOffset+QWord(Length(FData));
end;


function TQuicSendBuffer.NextChunk(aMaxLength: Integer; aMaxOffset: QWord; out aOffset: QWord; out aData: TBytes;
  out aFin: Boolean): Boolean;

var
  lRange: TQuicRange;
  lLength: QWord;

begin
  aOffset:=0;
  aData:=nil;
  aFin:=False;
  if aMaxLength<=0 then
    Exit(False);
  if FLost.Count>0 then
    begin
    lRange:=FLost[0];
    lLength:=lRange.High-lRange.Low+1;
    if lLength>QWord(aMaxLength) then
      lLength:=aMaxLength;
    FLost.Remove(lRange.Low,lRange.Low+lLength-1);
    aOffset:=lRange.Low;
    aData:=GetData(aOffset,lLength);
    aFin:=FFinSent and (not FFinAcked) and (aOffset+lLength=EndOffset);
    if aFin then
      FFinLost:=False;
    Exit(True);
    end;
  if FFinLost then
    begin
    FFinLost:=False;
    aOffset:=EndOffset;
    aFin:=True;
    Exit(True);
    end;
  Result:=False;
  if (FNextOffset<EndOffset) and (FNextOffset<aMaxOffset) then
    begin
    lLength:=EndOffset-FNextOffset;
    if lLength>aMaxOffset-FNextOffset then
      lLength:=aMaxOffset-FNextOffset;
    if lLength>QWord(aMaxLength) then
      lLength:=aMaxLength;
    aOffset:=FNextOffset;
    aData:=GetData(aOffset,lLength);
    Inc(FNextOffset,lLength);
    Result:=True;
    end;
  if FFin and (not FFinSent) and (FNextOffset=EndOffset) then
    begin
    if not Result then
      aOffset:=FNextOffset;
    FFinSent:=True;
    aFin:=True;
    Result:=True;
    end;
end;


procedure TQuicSendBuffer.OnAcked(aOffset: QWord; aLength: Integer; aFin: Boolean);

var
  lRange: TQuicRange;

begin
  if aFin then
    FFinAcked:=True;
  if aLength>0 then
    FAcked.Add(aOffset,aOffset+QWord(aLength)-1);
  while (FAcked.Count>0) and (FAcked.Min<=FBaseOffset) do
    begin
    lRange:=FAcked[0];
    FAcked.Remove(lRange.Low,lRange.High);
    if lRange.High>=FBaseOffset then
      begin
      Delete(FData,0,lRange.High-FBaseOffset+1);
      FBaseOffset:=lRange.High+1;
      end;
    end;
  if FBaseOffset>0 then
    FLost.Remove(0,FBaseOffset-1);
end;


procedure TQuicSendBuffer.OnLost(aOffset: QWord; aLength: Integer; aFin: Boolean);

var
  I: Integer;

begin
  if aFin and not FFinAcked then
    FFinLost:=True;
  if (aLength<=0) or (aOffset+QWord(aLength)<=FBaseOffset) then
    Exit;
  if aOffset<FBaseOffset then
    begin
    Dec(aLength,FBaseOffset-aOffset);
    aOffset:=FBaseOffset;
    end;
  FLost.Add(aOffset,aOffset+QWord(aLength)-1);
  for I:=0 to FAcked.Count-1 do
    FLost.Remove(FAcked[I].Low,FAcked[I].High);
  if FLost.Count>0 then
    FFinLost:=FFinLost and (FLost.Max+1<>EndOffset);
end;


function TQuicSendBuffer.HasPending: Boolean;

begin
  Result:=(FLost.Count>0) or FFinLost or (FNextOffset<EndOffset) or (FFin and not FFinSent);
end;


function TQuicSendBuffer.AllAcked: Boolean;

begin
  Result:=FFinAcked and (Length(FData)=0);
end;


function TQuicSendBuffer.Buffered: Integer;

begin
  Result:=Length(FData);
end;


{ TQuicStream }

constructor TQuicStream.Create(aId: QWord; aCanSend, aCanReceive: Boolean; aSendMax, aRecvMax: QWord);

begin
  FId:=aId;
  FCanSend:=aCanSend;
  FCanReceive:=aCanReceive;
  FSend:=TQuicSendBuffer.Create;
  FRecv:=TQuicReassembly.Create;
  SendMaxData:=aSendMax;
  RecvMaxData:=aRecvMax;
  FRecvWindow:=aRecvMax;
end;


destructor TQuicStream.Destroy;

begin
  FSend.Free;
  FRecv.Free;
  inherited Destroy;
end;


function TQuicStream.OnData(aOffset: QWord; const aData: TBytes; aFin: Boolean): QWord;

var
  lHighest: QWord;

begin
  if aOffset+QWord(Length(aData))>RecvMaxData then
    raise EQuicError.CreateCode(QUIC_FLOW_CONTROL_ERROR,'Stream data beyond MAX_STREAM_DATA');
  lHighest:=FRecv.HighestOffset;
  if ResetReceived then
    FRecv.CheckFinalSize(aOffset+QWord(Length(aData)),aFin)
  else
    FRecv.Insert(aOffset,aData,aFin);
  Result:=FRecv.HighestOffset-lHighest;
end;


function TQuicStream.OnReset(aErrorCode, aFinalSize: QWord): QWord;

var
  lHighest: QWord;

begin
  if aFinalSize>RecvMaxData then
    raise EQuicError.CreateCode(QUIC_FLOW_CONTROL_ERROR,'Final size beyond MAX_STREAM_DATA');
  lHighest:=FRecv.HighestOffset;
  FRecv.SetFinalSize(aFinalSize);
  ResetReceived:=True;
  ResetErrorCode:=aErrorCode;
  Result:=FRecv.HighestOffset-lHighest;
end;


function TQuicStream.Read(aBuffer: PByte; aCount: Integer): Integer;

begin
  Result:=FRecv.Read(aBuffer,aCount);
  if (not FRecv.HasFinalSize) and (RecvMaxData-FRecv.ReadOffset<FRecvWindow div 2) then
    begin
    RecvMaxData:=FRecv.ReadOffset+FRecvWindow;
    MaxStreamDataPending:=True;
    end;
end;


procedure TQuicStream.Reset(aErrorCode: QWord);

begin
  if LocalReset or FSend.AllAcked then
    Exit;
  LocalReset:=True;
  ResetPending:=True;
  LocalResetCode:=aErrorCode;
end;


procedure TQuicStream.StopSending(aErrorCode: QWord);

begin
  if LocalStopSending or ResetReceived or FRecv.IsFinished then
    Exit;
  LocalStopSending:=True;
  StopSendingPending:=True;
  LocalStopCode:=aErrorCode;
end;


function TQuicStream.WantsToSend: Boolean;

begin
  Result:=(not LocalReset) and FSend.HasPending;
end;


function TQuicStream.IsClosed: Boolean;

begin
  Result:=((not FCanSend) or FSend.AllAcked or ResetAcked)
          and ((not FCanReceive) or ResetDelivered or (FRecv.IsFinished and not ResetReceived));
end;


{ TQuicStreamMap }

constructor TQuicStreamMap.Create(aIsServer: Boolean; const aLocal: TQuicTransportParams);

begin
  FIsServer:=aIsServer;
  FStreams:=TFPList.Create;
  FLocal:=aLocal;
  FPeer:=QuicDefaultTransportParams;
  FLocalMaxStreams[True]:=aLocal.InitialMaxStreamsBidi;
  FLocalMaxStreams[False]:=aLocal.InitialMaxStreamsUni;
end;


destructor TQuicStreamMap.Destroy;

var
  I: Integer;

begin
  for I:=0 to FStreams.Count-1 do
    TQuicStream(FStreams[I]).Free;
  FStreams.Free;
  inherited Destroy;
end;


function TQuicStreamMap.GetCount: Integer;

begin
  Result:=FStreams.Count;
end;


function TQuicStreamMap.GetStream(aIndex: Integer): TQuicStream;

begin
  Result:=TQuicStream(FStreams[aIndex]);
end;


function TQuicStreamMap.CreateStream(aId: QWord): TQuicStream;

var
  lLocal, lBidi: Boolean;
  lSendMax, lRecvMax: QWord;

begin
  lLocal:=QuicStreamIsServerInitiated(aId)=FIsServer;
  lBidi:=QuicStreamIsBidi(aId);
  if not lBidi then
    begin
    lSendMax:=FPeer.InitialMaxStreamDataUni;
    lRecvMax:=FLocal.InitialMaxStreamDataUni;
    end
  else if lLocal then
    begin
    lSendMax:=FPeer.InitialMaxStreamDataBidiRemote;
    lRecvMax:=FLocal.InitialMaxStreamDataBidiLocal;
    end
  else
    begin
    lSendMax:=FPeer.InitialMaxStreamDataBidiLocal;
    lRecvMax:=FLocal.InitialMaxStreamDataBidiRemote;
    end;
  Result:=TQuicStream.Create(aId,lBidi or lLocal,lBidi or not lLocal,lSendMax,lRecvMax);
  FStreams.Add(Result);
end;


procedure TQuicStreamMap.SetPeerParams(const aPeer: TQuicTransportParams);

begin
  FPeer:=aPeer;
  FPeerMaxStreams[True]:=aPeer.InitialMaxStreamsBidi;
  FPeerMaxStreams[False]:=aPeer.InitialMaxStreamsUni;
end;


procedure TQuicStreamMap.OnMaxStreams(aBidi: Boolean; aValue: QWord);

begin
  if aValue>FPeerMaxStreams[aBidi] then
    FPeerMaxStreams[aBidi]:=aValue;
end;


function TQuicStreamMap.OpenLocal(aBidi: Boolean): TQuicStream;

var
  lId: QWord;

begin
  if FLocalOpened[aBidi]>=FPeerMaxStreams[aBidi] then
    Exit(nil);
  lId:=FLocalOpened[aBidi]*4;
  if not aBidi then
    lId:=lId or 2;
  if FIsServer then
    lId:=lId or 1;
  Inc(FLocalOpened[aBidi]);
  Result:=CreateStream(lId);
end;


function TQuicStreamMap.GetForFrame(aId: QWord; aSending: Boolean): TQuicStream;

var
  lBidi, lLocal: Boolean;
  lIndex, lNext: QWord;

begin
  lBidi:=QuicStreamIsBidi(aId);
  lLocal:=QuicStreamIsServerInitiated(aId)=FIsServer;
  lIndex:=aId shr 2;
  if (not lBidi) and (lLocal<>aSending) then
    raise EQuicError.CreateCode(QUIC_STREAM_STATE_ERROR,'Frame for the wrong direction of a unidirectional stream');
  if lLocal then
    begin
    if lIndex>=FLocalOpened[lBidi] then
      raise EQuicError.CreateCode(QUIC_STREAM_STATE_ERROR,'Frame for a stream that was not opened');
    Exit(Find(aId));
    end;
  if lIndex>=FLocalMaxStreams[lBidi] then
    raise EQuicError.CreateCode(QUIC_STREAM_LIMIT_ERROR,'Stream ID beyond the stream limit');
  if lIndex<FRemoteOpened[lBidi] then
    Exit(Find(aId));
  lNext:=FRemoteOpened[lBidi];
  while lNext<=lIndex do
    begin
    Result:=CreateStream((lNext shl 2) or (aId and 3));
    Insert(Result.Id,FAccept,Length(FAccept));
    Inc(lNext);
    end;
  FRemoteOpened[lBidi]:=lNext;
end;


function TQuicStreamMap.Find(aId: QWord): TQuicStream;

var
  I: Integer;

begin
  for I:=0 to FStreams.Count-1 do
    if TQuicStream(FStreams[I]).Id=aId then
      Exit(TQuicStream(FStreams[I]));
  Result:=nil;
end;


function TQuicStreamMap.Accept(out aId: QWord): Boolean;

begin
  aId:=0;
  Result:=Length(FAccept)>0;
  if Result then
    begin
    aId:=FAccept[0];
    Delete(FAccept,0,1);
    end;
end;


procedure TQuicStreamMap.RemoveClosed;

var
  I: Integer;
  lStream: TQuicStream;
  lBidi: Boolean;

begin
  for I:=FStreams.Count-1 downto 0 do
    begin
    lStream:=TQuicStream(FStreams[I]);
    if not lStream.IsClosed then
      Continue;
    if QuicStreamIsServerInitiated(lStream.Id)<>FIsServer then
      begin
      lBidi:=QuicStreamIsBidi(lStream.Id);
      Inc(FLocalMaxStreams[lBidi]);
      MaxStreamsPending[lBidi]:=True;
      end;
    FStreams.Delete(I);
    lStream.Free;
    end;
end;


function TQuicStreamMap.PeerMaxStreams(aBidi: Boolean): QWord;

begin
  Result:=FPeerMaxStreams[aBidi];
end;


function TQuicStreamMap.LocalMaxStreams(aBidi: Boolean): QWord;

begin
  Result:=FLocalMaxStreams[aBidi];
end;


end.
