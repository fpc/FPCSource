{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    QUIC loss detection and congestion control (RFC 9002): ACK tracking, RTT, loss detection, PTO, NewReno.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpQuic.Recovery;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}
{$modeswitch advancedrecords}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, System.Classes, FpQuic.Consts, FpQuic.Buffers, FpQuic.Frames;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils, Classes, FpQuic.Consts, FpQuic.Buffers, FpQuic.Frames;
{$ENDIF FPC_DOTTEDUNITS}

const
  // Timer granularity in microseconds.
  QUIC_GRANULARITY = 1000;
  // Initial RTT in microseconds.
  QUIC_INITIAL_RTT = 333000;
  // Packet reordering threshold.
  QUIC_PACKET_THRESHOLD = 3;
  // Number of ACK ranges kept for ACK frames.
  QUIC_MAX_ACK_RANGES = 32;

type
  // A sent packet kept until it is acknowledged or declared lost.
  TQuicSentPacket = class
  public
    PacketNumber: QWord;
    TimeSent: QWord;
    Size: Integer;
    AckEliciting: Boolean;
    InFlight: Boolean;
    // Frames of the packet, used to act on acknowledgement or loss.
    Frames: TQuicFrameArray;
  end;

  // Received packet numbers of one packet number space and the resulting ACK frames.
  TQuicAckTracker = class
  private
    FReceived: TQuicRangeSet;
    FFloor: QWord;
    FLargestTime: QWord;
    FUnacked: Integer;
    FNewPackets: Boolean;
    FDeadline: QWord;
  public
    constructor Create;
    destructor Destroy; override;
    // True when packet number aPn was already received or is older than the ranges kept.
    function IsDuplicate(aPn: QWord): Boolean;
    // Records a received packet; aImmediate requests an ACK without delay, aMaxAckDelay is in microseconds.
    procedure OnPacket(aPn: QWord; aAckEliciting, aImmediate: Boolean; aNow, aMaxAckDelay: QWord);
    // True when an ACK frame must be sent now.
    function AckDue(aNow: QWord): Boolean;
    // True when packets arrived since the last ACK frame.
    function HasNewPackets: Boolean;
    // Builds an ACK frame and resets the pending state; aExponent scales the ACK delay.
    function BuildFrame(aNow: QWord; aExponent: Integer): TQuicFrame;
    // Time an ACK is due, 0 when none.
    property Deadline: QWord read FDeadline;
  end;

  // RTT estimator of RFC 9002 section 5; all values in microseconds.
  TQuicRttEstimator = record
    LatestRtt: QWord;
    SmoothedRtt: QWord;
    RttVar: QWord;
    MinRtt: QWord;
    HasSample: Boolean;
    // Resets to the initial RTT.
    procedure Init;
    // Adds an RTT sample; aAckDelay is ignored before the handshake is confirmed and capped by aMaxAckDelay after.
    procedure Update(aLatest, aAckDelay: QWord; aHandshakeConfirmed: Boolean; aMaxAckDelay: QWord);
    // Probe timeout duration without backoff and ACK delay.
    function PtoBase: QWord;
  end;

  // Congestion controller notified of sent, acknowledged and lost packets; the connection owns the object.
  IQuicCongestionControl = interface
    // A packet of aSize bytes counting towards bytes in flight was sent.
    procedure OnPacketSent(aSize: Integer);
    // An in-flight packet sent at aTimeSent was acknowledged.
    procedure OnPacketAcked(aSize: Integer; aTimeSent: QWord);
    // In-flight packets of aSize bytes were lost; aLargestTimeSent is the send time of the newest one.
    procedure OnPacketsLost(aSize: Integer; aLargestTimeSent, aNow: QWord; aPersistent: Boolean);
    // An in-flight packet was discarded with its keys.
    procedure OnPacketDiscarded(aSize: Integer);
  end;

  // NewReno congestion control (RFC 9002 section 7).
  TQuicNewReno = class(TObject, IQuicCongestionControl)
  private
    FMaxDatagram: Integer;
    FWindow: Integer;
    FInFlight: Integer;
    FSsThresh: Integer;
    FRecoveryStart: QWord;
    FInRecovery: Boolean;
  public
    // Creates the controller for datagrams of at most aMaxDatagram bytes.
    constructor Create(aMaxDatagram: Integer);
    // A packet was sent.
    procedure OnPacketSent(aSize: Integer);
    // A packet was acknowledged.
    procedure OnPacketAcked(aSize: Integer; aTimeSent: QWord);
    // Packets were lost.
    procedure OnPacketsLost(aSize: Integer; aLargestTimeSent, aNow: QWord; aPersistent: Boolean);
    // A packet was discarded.
    procedure OnPacketDiscarded(aSize: Integer);
    // True when aSize more bytes fit the congestion window.
    function CanSend(aSize: Integer): Boolean;
    // Bytes in flight.
    function BytesInFlight: Integer;
    // Congestion window in bytes.
    function CongestionWindow: Integer;
    // Slow start threshold in bytes.
    property SlowStartThreshold: Integer read FSsThresh;
  end;

  // Sent packet tracking, loss detection and probe timeout of RFC 9002 section 6.
  TQuicLossDetector = class
  private
    FCC: IQuicCongestionControl;
    FSent: array[TQuicEncryptionLevel] of TFPList;
    FLargestAcked: array[TQuicEncryptionLevel] of Int64;
    FLargestSent: array[TQuicEncryptionLevel] of Int64;
    FLossTime: array[TQuicEncryptionLevel] of QWord;
    FFirstSampleTime: QWord;
    FTimer: QWord;
    function AckElicitingInFlight(aSpace: TQuicEncryptionLevel): Boolean;
    function AnyAckElicitingInFlight: Boolean;
    function PtoTime(aNow: QWord; out aSpace: TQuicEncryptionLevel): QWord;
    function EarliestLossTime(out aSpace: TQuicEncryptionLevel): QWord;
    procedure DetectLost(aSpace: TQuicEncryptionLevel; aNow: QWord; aLost: TFPList);
    function IsPersistent(aLost: TFPList): Boolean;
    procedure ReportLost(aLost: TFPList; aNow: QWord);
  public
    // RTT estimate.
    Rtt: TQuicRttEstimator;
    // Probe timeout backoff exponent.
    PtoCount: Integer;
    // Peer max_ack_delay in microseconds.
    MaxAckDelay: QWord;
    // Connection state used by the probe timeout.
    HandshakeConfirmed: Boolean;
    HasHandshakeKeys: Boolean;
    PeerAddressValidated: Boolean;
    AmplificationBlocked: Boolean;
    // Creates the detector reporting to aCC.
    constructor Create(const aCC: IQuicCongestionControl);
    destructor Destroy; override;
    // Records a sent packet; the detector owns aPacket.
    procedure OnPacketSent(aSpace: TQuicEncryptionLevel; aPacket: TQuicSentPacket; aNow: QWord);
    // Processes an ACK frame; newly acknowledged and lost packets are added to aAcked and aLost (caller frees them).
    // Raises PROTOCOL_VIOLATION when unsent packets are acknowledged.
    procedure OnAckReceived(aSpace: TQuicEncryptionLevel; const aFrame: TQuicFrame; aAckDelayExponent: Integer;
      aNow: QWord; aAcked, aLost: TFPList);
    // Handles the expiry of the loss detection timer. Lost packets go to aLost (caller frees them);
    // when no packet is lost, aProbe is True and aProbeSpace tells where to send a probe.
    procedure OnTimeout(aNow: QWord; aLost: TFPList; out aProbe: Boolean; out aProbeSpace: TQuicEncryptionLevel);
    // Drops all packets of aSpace when its keys are discarded.
    procedure DiscardSpace(aSpace: TQuicEncryptionLevel);
    // Recomputes the loss detection timer.
    procedure UpdateTimer(aNow: QWord);
    // Oldest unacknowledged ack-eliciting packet of aSpace, nil when none.
    function OldestUnacked(aSpace: TQuicEncryptionLevel): TQuicSentPacket;
    // Unacknowledged packets of aSpace in sending order; owned by the detector.
    function SentPackets(aSpace: TQuicEncryptionLevel): TFPList;
    // Probe timeout duration with backoff, for idle and closing timers.
    function PtoDuration: QWord;
    // Largest acknowledged packet number of aSpace, -1 when none.
    function LargestAcked(aSpace: TQuicEncryptionLevel): Int64;
    // Loss detection timer, 0 when not armed.
    property Timer: QWord read FTimer;
  end;

implementation

{ TQuicAckTracker }

constructor TQuicAckTracker.Create;

begin
  FReceived:=TQuicRangeSet.Create;
end;


destructor TQuicAckTracker.Destroy;

begin
  FReceived.Free;
  inherited Destroy;
end;


function TQuicAckTracker.IsDuplicate(aPn: QWord): Boolean;

begin
  Result:=(aPn<FFloor) or FReceived.Contains(aPn);
end;


procedure TQuicAckTracker.OnPacket(aPn: QWord; aAckEliciting, aImmediate: Boolean; aNow, aMaxAckDelay: QWord);

var
  lOutOfOrder: Boolean;

begin
  lOutOfOrder:=(FReceived.Count>0) and (aPn<>FReceived.Max+1);
  if (FReceived.Count=0) or (aPn>FReceived.Max) then
    FLargestTime:=aNow;
  FReceived.Add(aPn,aPn);
  if FReceived.Count>QUIC_MAX_ACK_RANGES then
    begin
    FReceived.Remove(0,FReceived[0].High);
    FFloor:=FReceived[0].Low;
    end;
  FNewPackets:=True;
  if not aAckEliciting then
    Exit;
  Inc(FUnacked);
  if aImmediate or lOutOfOrder or (FUnacked>=2) then
    FDeadline:=aNow
  else if FDeadline=0 then
    FDeadline:=aNow+aMaxAckDelay;
end;


function TQuicAckTracker.AckDue(aNow: QWord): Boolean;

begin
  Result:=(FDeadline<>0) and (FDeadline<=aNow);
end;


function TQuicAckTracker.HasNewPackets: Boolean;

begin
  Result:=FNewPackets;
end;


function TQuicAckTracker.BuildFrame(aNow: QWord; aExponent: Integer): TQuicFrame;

var
  I: Integer;

begin
  Result:=QuicFrame(QUIC_FRAME_ACK);
  SetLength(Result.AckRanges,FReceived.Count);
  for I:=0 to FReceived.Count-1 do
    Result.AckRanges[I]:=FReceived[FReceived.Count-1-I];
  Result.AckDelay:=(aNow-FLargestTime) shr aExponent;
  FUnacked:=0;
  FNewPackets:=False;
  FDeadline:=0;
end;


{ TQuicRttEstimator }

procedure TQuicRttEstimator.Init;

begin
  LatestRtt:=0;
  SmoothedRtt:=QUIC_INITIAL_RTT;
  RttVar:=QUIC_INITIAL_RTT div 2;
  MinRtt:=0;
  HasSample:=False;
end;


procedure TQuicRttEstimator.Update(aLatest, aAckDelay: QWord; aHandshakeConfirmed: Boolean; aMaxAckDelay: QWord);

var
  lAdjusted, lDiff: QWord;

begin
  LatestRtt:=aLatest;
  if not HasSample then
    begin
    HasSample:=True;
    MinRtt:=aLatest;
    SmoothedRtt:=aLatest;
    RttVar:=aLatest div 2;
    Exit;
    end;
  if aLatest<MinRtt then
    MinRtt:=aLatest;
  if not aHandshakeConfirmed then
    aAckDelay:=0
  else if aAckDelay>aMaxAckDelay then
    aAckDelay:=aMaxAckDelay;
  lAdjusted:=aLatest;
  if aLatest>=MinRtt+aAckDelay then
    lAdjusted:=aLatest-aAckDelay;
  if SmoothedRtt>lAdjusted then
    lDiff:=SmoothedRtt-lAdjusted
  else
    lDiff:=lAdjusted-SmoothedRtt;
  RttVar:=(3*RttVar+lDiff) div 4;
  SmoothedRtt:=(7*SmoothedRtt+lAdjusted) div 8;
end;


function TQuicRttEstimator.PtoBase: QWord;

begin
  Result:=4*RttVar;
  if Result<QUIC_GRANULARITY then
    Result:=QUIC_GRANULARITY;
  Inc(Result,SmoothedRtt);
end;


{ TQuicNewReno }

constructor TQuicNewReno.Create(aMaxDatagram: Integer);

begin
  FMaxDatagram:=aMaxDatagram;
  FWindow:=10*aMaxDatagram;
  if FWindow>14720 then
    FWindow:=14720;
  if FWindow<2*aMaxDatagram then
    FWindow:=2*aMaxDatagram;
  FSsThresh:=High(Integer);
end;


procedure TQuicNewReno.OnPacketSent(aSize: Integer);

begin
  Inc(FInFlight,aSize);
end;


procedure TQuicNewReno.OnPacketAcked(aSize: Integer; aTimeSent: QWord);

begin
  Dec(FInFlight,aSize);
  if FInRecovery and (aTimeSent<=FRecoveryStart) then
    Exit;
  if FWindow<FSsThresh then
    Inc(FWindow,aSize)
  else
    Inc(FWindow,FMaxDatagram*aSize div FWindow);
end;


procedure TQuicNewReno.OnPacketsLost(aSize: Integer; aLargestTimeSent, aNow: QWord; aPersistent: Boolean);

begin
  Dec(FInFlight,aSize);
  if aPersistent then
    begin
    FWindow:=2*FMaxDatagram;
    FInRecovery:=False;
    Exit;
    end;
  if FInRecovery and (aLargestTimeSent<=FRecoveryStart) then
    Exit;
  FInRecovery:=True;
  FRecoveryStart:=aNow;
  FSsThresh:=FWindow div 2;
  FWindow:=FSsThresh;
  if FWindow<2*FMaxDatagram then
    FWindow:=2*FMaxDatagram;
end;


procedure TQuicNewReno.OnPacketDiscarded(aSize: Integer);

begin
  Dec(FInFlight,aSize);
end;


function TQuicNewReno.CanSend(aSize: Integer): Boolean;

begin
  Result:=FInFlight+aSize<=FWindow;
end;


function TQuicNewReno.BytesInFlight: Integer;

begin
  Result:=FInFlight;
end;


function TQuicNewReno.CongestionWindow: Integer;

begin
  Result:=FWindow;
end;


{ TQuicLossDetector }

constructor TQuicLossDetector.Create(const aCC: IQuicCongestionControl);

var
  lSpace: TQuicEncryptionLevel;

begin
  FCC:=aCC;
  Rtt.Init;
  MaxAckDelay:=25000;
  for lSpace:=Low(TQuicEncryptionLevel) to High(TQuicEncryptionLevel) do
    begin
    FSent[lSpace]:=TFPList.Create;
    FLargestAcked[lSpace]:=-1;
    FLargestSent[lSpace]:=-1;
    end;
end;


destructor TQuicLossDetector.Destroy;

var
  lSpace: TQuicEncryptionLevel;
  I: Integer;

begin
  for lSpace:=Low(TQuicEncryptionLevel) to High(TQuicEncryptionLevel) do
    begin
    for I:=0 to FSent[lSpace].Count-1 do
      TQuicSentPacket(FSent[lSpace][I]).Free;
    FSent[lSpace].Free;
    end;
  inherited Destroy;
end;


function TQuicLossDetector.AckElicitingInFlight(aSpace: TQuicEncryptionLevel): Boolean;

begin
  Result:=OldestUnacked(aSpace)<>nil;
end;


function TQuicLossDetector.AnyAckElicitingInFlight: Boolean;

var
  lSpace: TQuicEncryptionLevel;

begin
  for lSpace:=Low(TQuicEncryptionLevel) to High(TQuicEncryptionLevel) do
    if AckElicitingInFlight(lSpace) then
      Exit(True);
  Result:=False;
end;


procedure TQuicLossDetector.OnPacketSent(aSpace: TQuicEncryptionLevel; aPacket: TQuicSentPacket; aNow: QWord);

begin
  aPacket.TimeSent:=aNow;
  FLargestSent[aSpace]:=aPacket.PacketNumber;
  FSent[aSpace].Add(aPacket);
  if aPacket.InFlight then
    FCC.OnPacketSent(aPacket.Size);
  UpdateTimer(aNow);
end;


// Send time of the newest ack-eliciting packet in aList, which must contain one.
function LastAckEliciting(aList: TFPList): QWord;

var
  I: Integer;

begin
  I:=aList.Count-1;
  while not TQuicSentPacket(aList[I]).AckEliciting do
    Dec(I);
  Result:=TQuicSentPacket(aList[I]).TimeSent;
end;


procedure TQuicLossDetector.OnAckReceived(aSpace: TQuicEncryptionLevel; const aFrame: TQuicFrame;
  aAckDelayExponent: Integer; aNow: QWord; aAcked, aLost: TFPList);

var
  lLargest, lAckDelay: QWord;
  lPacket, lLargestPacket: TQuicSentPacket;
  lAckEliciting: Boolean;
  I, J: Integer;

begin
  lLargest:=aFrame.AckRanges[0].High;
  if Int64(lLargest)>FLargestSent[aSpace] then
    raise EQuicError.CreateCode(QUIC_PROTOCOL_VIOLATION,'ACK of an unsent packet',aFrame.FrameType);
  if Int64(lLargest)>FLargestAcked[aSpace] then
    FLargestAcked[aSpace]:=lLargest;
  lLargestPacket:=nil;
  lAckEliciting:=False;
  I:=0;
  while I<FSent[aSpace].Count do
    begin
    lPacket:=TQuicSentPacket(FSent[aSpace][I]);
    J:=0;
    while (J<Length(aFrame.AckRanges))
          and not ((lPacket.PacketNumber>=aFrame.AckRanges[J].Low) and (lPacket.PacketNumber<=aFrame.AckRanges[J].High)) do
      Inc(J);
    if J<Length(aFrame.AckRanges) then
      begin
      FSent[aSpace].Delete(I);
      aAcked.Add(lPacket);
      if lPacket.PacketNumber=lLargest then
        lLargestPacket:=lPacket;
      lAckEliciting:=lAckEliciting or lPacket.AckEliciting;
      if lPacket.InFlight then
        FCC.OnPacketAcked(lPacket.Size,lPacket.TimeSent);
      end
    else
      Inc(I);
    end;
  if aAcked.Count=0 then
    Exit;
  if (lLargestPacket<>nil) and lAckEliciting then
    begin
    lAckDelay:=0;
    if aSpace=qelApplication then
      lAckDelay:=aFrame.AckDelay shl aAckDelayExponent;
    if not Rtt.HasSample then
      FFirstSampleTime:=aNow;
    Rtt.Update(aNow-lLargestPacket.TimeSent,lAckDelay,HandshakeConfirmed,MaxAckDelay);
    end;
  DetectLost(aSpace,aNow,aLost);
  ReportLost(aLost,aNow);
  if PeerAddressValidated then
    PtoCount:=0;
  UpdateTimer(aNow);
end;


procedure TQuicLossDetector.DetectLost(aSpace: TQuicEncryptionLevel; aNow: QWord; aLost: TFPList);

var
  lDelay, lTime: QWord;
  lPacket: TQuicSentPacket;
  I: Integer;

begin
  FLossTime[aSpace]:=0;
  lDelay:=Rtt.LatestRtt;
  if Rtt.SmoothedRtt>lDelay then
    lDelay:=Rtt.SmoothedRtt;
  lDelay:=lDelay*9 div 8;
  if lDelay<QUIC_GRANULARITY then
    lDelay:=QUIC_GRANULARITY;
  I:=0;
  while I<FSent[aSpace].Count do
    begin
    lPacket:=TQuicSentPacket(FSent[aSpace][I]);
    if Int64(lPacket.PacketNumber)>FLargestAcked[aSpace] then
      Break;
    lTime:=lPacket.TimeSent+lDelay;
    if (lTime<=aNow) or (FLargestAcked[aSpace]>=Int64(lPacket.PacketNumber)+QUIC_PACKET_THRESHOLD) then
      begin
      FSent[aSpace].Delete(I);
      aLost.Add(lPacket);
      end
    else
      begin
      if (FLossTime[aSpace]=0) or (lTime<FLossTime[aSpace]) then
        FLossTime[aSpace]:=lTime;
      Inc(I);
      end;
    end;
end;


function TQuicLossDetector.IsPersistent(aLost: TFPList): Boolean;

var
  lDuration, lStart: QWord;
  lPacket, lPrevious: TQuicSentPacket;
  lHaveStart: Boolean;
  I: Integer;

begin
  Result:=False;
  if not Rtt.HasSample then
    Exit;
  lDuration:=(Rtt.PtoBase+MaxAckDelay)*3;
  lHaveStart:=False;
  lStart:=0;
  lPrevious:=nil;
  for I:=0 to aLost.Count-1 do
    begin
    lPacket:=TQuicSentPacket(aLost[I]);
    if (lPrevious<>nil) and (lPacket.PacketNumber<>lPrevious.PacketNumber+1) then
      lHaveStart:=False;
    lPrevious:=lPacket;
    if (not lPacket.AckEliciting) or (lPacket.TimeSent<FFirstSampleTime) then
      Continue;
    if not lHaveStart then
      begin
      lHaveStart:=True;
      lStart:=lPacket.TimeSent;
      end
    else if lPacket.TimeSent-lStart>=lDuration then
      Exit(True);
    end;
end;


procedure TQuicLossDetector.ReportLost(aLost: TFPList; aNow: QWord);

var
  lSize: Integer;
  lLargestTime: QWord;
  lPacket: TQuicSentPacket;
  I: Integer;

begin
  lSize:=0;
  lLargestTime:=0;
  for I:=0 to aLost.Count-1 do
    begin
    lPacket:=TQuicSentPacket(aLost[I]);
    if not lPacket.InFlight then
      Continue;
    Inc(lSize,lPacket.Size);
    if lPacket.TimeSent>lLargestTime then
      lLargestTime:=lPacket.TimeSent;
    end;
  if lSize>0 then
    FCC.OnPacketsLost(lSize,lLargestTime,aNow,IsPersistent(aLost));
end;


function TQuicLossDetector.EarliestLossTime(out aSpace: TQuicEncryptionLevel): QWord;

var
  lSpace: TQuicEncryptionLevel;

begin
  Result:=0;
  aSpace:=qelInitial;
  for lSpace:=Low(TQuicEncryptionLevel) to High(TQuicEncryptionLevel) do
    if (FLossTime[lSpace]<>0) and ((Result=0) or (FLossTime[lSpace]<Result)) then
      begin
      Result:=FLossTime[lSpace];
      aSpace:=lSpace;
      end;
end;


function TQuicLossDetector.PtoDuration: QWord;

begin
  Result:=Rtt.PtoBase shl PtoCount;
end;


function TQuicLossDetector.PtoTime(aNow: QWord; out aSpace: TQuicEncryptionLevel): QWord;

var
  lSpace: TQuicEncryptionLevel;
  lDuration, lTime: QWord;

begin
  lDuration:=PtoDuration;
  if not AnyAckElicitingInFlight then
    begin
    if HasHandshakeKeys then
      aSpace:=qelHandshake
    else
      aSpace:=qelInitial;
    Exit(aNow+lDuration);
    end;
  Result:=0;
  aSpace:=qelInitial;
  for lSpace:=Low(TQuicEncryptionLevel) to High(TQuicEncryptionLevel) do
    begin
    if not AckElicitingInFlight(lSpace) then
      Continue;
    if lSpace=qelApplication then
      begin
      if not HandshakeConfirmed then
        Break;
      Inc(lDuration,MaxAckDelay shl PtoCount);
      end;
    lTime:=LastAckEliciting(FSent[lSpace])+lDuration;
    if (Result=0) or (lTime<Result) then
      begin
      Result:=lTime;
      aSpace:=lSpace;
      end;
    end;
end;


procedure TQuicLossDetector.UpdateTimer(aNow: QWord);

var
  lSpace: TQuicEncryptionLevel;

begin
  FTimer:=EarliestLossTime(lSpace);
  if FTimer<>0 then
    Exit;
  if AmplificationBlocked or ((not AnyAckElicitingInFlight) and PeerAddressValidated) then
    Exit;
  FTimer:=PtoTime(aNow,lSpace);
end;


procedure TQuicLossDetector.OnTimeout(aNow: QWord; aLost: TFPList; out aProbe: Boolean;
  out aProbeSpace: TQuicEncryptionLevel);

begin
  aProbe:=False;
  if EarliestLossTime(aProbeSpace)<>0 then
    begin
    DetectLost(aProbeSpace,aNow,aLost);
    ReportLost(aLost,aNow);
    end
  else
    begin
    PtoTime(aNow,aProbeSpace);
    aProbe:=True;
    Inc(PtoCount);
    end;
  UpdateTimer(aNow);
end;


procedure TQuicLossDetector.DiscardSpace(aSpace: TQuicEncryptionLevel);

var
  lPacket: TQuicSentPacket;
  I: Integer;

begin
  for I:=0 to FSent[aSpace].Count-1 do
    begin
    lPacket:=TQuicSentPacket(FSent[aSpace][I]);
    if lPacket.InFlight then
      FCC.OnPacketDiscarded(lPacket.Size);
    lPacket.Free;
    end;
  FSent[aSpace].Clear;
  FLossTime[aSpace]:=0;
  PtoCount:=0;
end;


function TQuicLossDetector.LargestAcked(aSpace: TQuicEncryptionLevel): Int64;

begin
  Result:=FLargestAcked[aSpace];
end;


function TQuicLossDetector.OldestUnacked(aSpace: TQuicEncryptionLevel): TQuicSentPacket;

var
  I: Integer;

begin
  for I:=0 to FSent[aSpace].Count-1 do
    if TQuicSentPacket(FSent[aSpace][I]).AckEliciting then
      Exit(TQuicSentPacket(FSent[aSpace][I]));
  Result:=nil;
end;


function TQuicLossDetector.SentPackets(aSpace: TQuicEncryptionLevel): TFPList;

begin
  Result:=FSent[aSpace];
end;


end.
