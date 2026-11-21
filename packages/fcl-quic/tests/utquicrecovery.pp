{
  This file is part of the Free Component Library.
  Tests for FpQuic.Recovery.
  See the file COPYING.FPC, included in this distribution, for details about the copyright.
}
unit utquicrecovery;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpcunit, testregistry, FpQuic.Consts, FpQuic.Buffers, FpQuic.Frames, FpQuic.Recovery;

type
  TTestQuicAckTracker = class(TTestCase)
  published
    procedure TestImmediateAndDelayed;
    procedure TestOutOfOrder;
    procedure TestNonAckEliciting;
    procedure TestFrame;
    procedure TestRangeLimit;
  end;

  TTestQuicRtt = class(TTestCase)
  published
    procedure TestSamples;
    procedure TestAckDelay;
    procedure TestPtoBase;
  end;

  TTestQuicNewReno = class(TTestCase)
  published
    procedure TestInitialWindow;
    procedure TestGrowth;
    procedure TestRecovery;
    procedure TestPersistentAndDiscard;
  end;

  TTestQuicLossDetector = class(TTestCase)
  private
    FCC: TQuicNewReno;
    FDetector: TQuicLossDetector;
    FAcked, FLost: TFPList;
    procedure Send(aSpace: TQuicEncryptionLevel; aPn, aTime: QWord; aAckEliciting: Boolean = True;
      aInFlight: Boolean = True);
    procedure Ack(aSpace: TQuicEncryptionLevel; aLow, aHigh, aNow: QWord; aDelay: QWord = 0);
    procedure ClearLists;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAckOfUnsentPacket;
    procedure TestAckAndRtt;
    procedure TestDuplicateAck;
    procedure TestPacketThreshold;
    procedure TestTimeThreshold;
    procedure TestPtoSpaces;
    procedure TestPtoWithoutInFlight;
    procedure TestTimerDisabled;
    procedure TestPersistentCongestion;
    procedure TestNotPersistent;
    procedure TestDiscardSpace;
    procedure TestPtoCountReset;
    procedure TestLossWithoutRttSample;
  end;

implementation

{ TTestQuicAckTracker }

procedure TTestQuicAckTracker.TestImmediateAndDelayed;

var
  lTracker: TQuicAckTracker;

begin
  lTracker:=TQuicAckTracker.Create;
  try
    lTracker.OnPacket(0,True,True,100,25000);
    AssertTrue('Immediate',lTracker.AckDue(100));
    lTracker.BuildFrame(100,3);
    lTracker.OnPacket(1,True,False,200,25000);
    AssertFalse('Delayed',lTracker.AckDue(200));
    AssertEquals('Deadline',25200,lTracker.Deadline);
    lTracker.OnPacket(2,True,False,300,25000);
    AssertTrue('Second packet',lTracker.AckDue(300));
    AssertTrue('Duplicate',lTracker.IsDuplicate(1));
    AssertFalse('Not duplicate',lTracker.IsDuplicate(5));
  finally
    lTracker.Free;
  end;
end;


procedure TTestQuicAckTracker.TestOutOfOrder;

var
  lTracker: TQuicAckTracker;

begin
  lTracker:=TQuicAckTracker.Create;
  try
    lTracker.OnPacket(0,True,False,100,25000);
    lTracker.BuildFrame(100,3);
    lTracker.OnPacket(2,True,False,200,25000);
    AssertTrue('Gap triggers immediate ACK',lTracker.AckDue(200));
  finally
    lTracker.Free;
  end;
end;


procedure TTestQuicAckTracker.TestNonAckEliciting;

var
  lTracker: TQuicAckTracker;

begin
  lTracker:=TQuicAckTracker.Create;
  try
    lTracker.OnPacket(0,False,True,100,25000);
    AssertFalse('No ACK needed',lTracker.AckDue(100));
    AssertTrue('New packets',lTracker.HasNewPackets);
    AssertEquals('No deadline',0,lTracker.Deadline);
  finally
    lTracker.Free;
  end;
end;


procedure TTestQuicAckTracker.TestFrame;

var
  lTracker: TQuicAckTracker;
  lFrame: TQuicFrame;

begin
  lTracker:=TQuicAckTracker.Create;
  try
    lTracker.OnPacket(0,True,False,1000,25000);
    lTracker.OnPacket(1,True,False,1000,25000);
    lTracker.OnPacket(5,True,False,2000,25000);
    lTracker.OnPacket(3,True,False,3000,25000);
    lFrame:=lTracker.BuildFrame(10000,3);
    AssertEquals('Ranges',3,Length(lFrame.AckRanges));
    AssertEquals('Largest',5,lFrame.AckRanges[0].High);
    AssertEquals('Middle',3,lFrame.AckRanges[1].Low);
    AssertEquals('Smallest',0,lFrame.AckRanges[2].Low);
    AssertEquals('Delay from largest',1000,lFrame.AckDelay);
    AssertFalse('No new packets',lTracker.HasNewPackets);
    AssertEquals('Deadline cleared',0,lTracker.Deadline);
  finally
    lTracker.Free;
  end;
end;


procedure TTestQuicAckTracker.TestRangeLimit;

var
  lTracker: TQuicAckTracker;
  I: Integer;

begin
  lTracker:=TQuicAckTracker.Create;
  try
    for I:=0 to QUIC_MAX_ACK_RANGES do
      lTracker.OnPacket(I*2,True,False,1,25000);
    AssertEquals('Ranges capped',QUIC_MAX_ACK_RANGES,Length(lTracker.BuildFrame(1,3).AckRanges));
    AssertTrue('Packet of the dropped range stays a duplicate',lTracker.IsDuplicate(0));
    AssertTrue('Packet below the dropped range is a duplicate',lTracker.IsDuplicate(1));
    AssertFalse('Gap above the dropped range is no duplicate',lTracker.IsDuplicate(3));
  finally
    lTracker.Free;
  end;
end;


{ TTestQuicRtt }

procedure TTestQuicRtt.TestSamples;

var
  lRtt: TQuicRttEstimator;

begin
  lRtt.Init;
  AssertEquals('Initial smoothed',QWord(QUIC_INITIAL_RTT),lRtt.SmoothedRtt);
  AssertFalse('No sample',lRtt.HasSample);
  lRtt.Update(100000,0,False,25000);
  AssertEquals('First smoothed',QWord(100000),lRtt.SmoothedRtt);
  AssertEquals('First variance',50000,lRtt.RttVar);
  AssertEquals('Min',QWord(100000),lRtt.MinRtt);
  lRtt.Update(80000,0,False,25000);
  AssertEquals('Min decreased',QWord(80000),lRtt.MinRtt);
  AssertEquals('Latest',QWord(80000),lRtt.LatestRtt);
  AssertEquals('Smoothed',QWord(97500),lRtt.SmoothedRtt);
  AssertEquals('Variance',42500,lRtt.RttVar);
  lRtt.Update(200000,0,False,25000);
  AssertEquals('Smoothed up',QWord(110312),lRtt.SmoothedRtt);
end;


procedure TTestQuicRtt.TestAckDelay;

var
  lRtt: TQuicRttEstimator;

begin
  lRtt.Init;
  lRtt.Update(100000,0,True,25000);
  lRtt.Update(150000,40000,False,25000);
  AssertEquals('Delay ignored before confirmation',QWord(106250),lRtt.SmoothedRtt);
  lRtt.Init;
  lRtt.Update(100000,0,True,25000);
  lRtt.Update(150000,40000,True,25000);
  AssertEquals('Delay capped',QWord(103125),lRtt.SmoothedRtt);
  lRtt.Init;
  lRtt.Update(100000,0,True,25000);
  lRtt.Update(110000,20000,True,25000);
  AssertEquals('Delay not subtracted below min RTT',QWord(101250),lRtt.SmoothedRtt);
end;


procedure TTestQuicRtt.TestPtoBase;

var
  lRtt: TQuicRttEstimator;

begin
  lRtt.Init;
  AssertEquals('Initial PTO',QWord(QUIC_INITIAL_RTT+2*QUIC_INITIAL_RTT),lRtt.PtoBase);
  lRtt.Update(100,0,False,0);
  AssertEquals('Granularity floor',100+QUIC_GRANULARITY,lRtt.PtoBase);
end;


{ TTestQuicNewReno }

procedure TTestQuicNewReno.TestInitialWindow;

var
  lCC: TQuicNewReno;

begin
  lCC:=TQuicNewReno.Create(1200);
  AssertEquals('Default',12000,lCC.CongestionWindow);
  lCC.Free;
  lCC:=TQuicNewReno.Create(1500);
  AssertEquals('Capped',14720,lCC.CongestionWindow);
  lCC.Free;
  lCC:=TQuicNewReno.Create(10000);
  AssertEquals('Two datagrams',20000,lCC.CongestionWindow);
  lCC.Free;
end;


procedure TTestQuicNewReno.TestGrowth;

var
  lCC: TQuicNewReno;

begin
  lCC:=TQuicNewReno.Create(1200);
  try
    lCC.OnPacketSent(1200);
    AssertEquals('In flight',1200,lCC.BytesInFlight);
    AssertTrue('Can send',lCC.CanSend(10800));
    AssertFalse('Window full',lCC.CanSend(10801));
    lCC.OnPacketAcked(1200,10);
    AssertEquals('Slow start',13200,lCC.CongestionWindow);
    AssertEquals('In flight after ack',0,lCC.BytesInFlight);
  finally
    lCC.Free;
  end;
end;


procedure TTestQuicNewReno.TestRecovery;

var
  lCC: TQuicNewReno;

begin
  lCC:=TQuicNewReno.Create(1200);
  try
    lCC.OnPacketSent(3600);
    lCC.OnPacketsLost(1200,10,100,False);
    AssertEquals('Halved',6000,lCC.CongestionWindow);
    AssertEquals('Threshold',6000,lCC.SlowStartThreshold);
    lCC.OnPacketsLost(1200,50,120,False);
    AssertEquals('Same recovery period',6000,lCC.CongestionWindow);
    lCC.OnPacketAcked(1200,90);
    AssertEquals('No growth for old packets',6000,lCC.CongestionWindow);
    lCC.OnPacketSent(1200);
    lCC.OnPacketAcked(1200,200);
    AssertEquals('Congestion avoidance',6240,lCC.CongestionWindow);
    lCC.OnPacketSent(1200);
    lCC.OnPacketsLost(1200,300,400,False);
    AssertEquals('Halved again',3120,lCC.CongestionWindow);
    lCC.OnPacketSent(1200);
    lCC.OnPacketsLost(1200,500,600,False);
    AssertEquals('Minimum window',2400,lCC.CongestionWindow);
  finally
    lCC.Free;
  end;
end;


procedure TTestQuicNewReno.TestPersistentAndDiscard;

var
  lCC: TQuicNewReno;

begin
  lCC:=TQuicNewReno.Create(1200);
  try
    lCC.OnPacketSent(2400);
    lCC.OnPacketsLost(1200,10,100,True);
    AssertEquals('Persistent congestion',2400,lCC.CongestionWindow);
    lCC.OnPacketDiscarded(1200);
    AssertEquals('Discarded',0,lCC.BytesInFlight);
  finally
    lCC.Free;
  end;
end;


{ TTestQuicLossDetector }

procedure TTestQuicLossDetector.SetUp;

begin
  FCC:=TQuicNewReno.Create(1200);
  FDetector:=TQuicLossDetector.Create(FCC);
  FAcked:=TFPList.Create;
  FLost:=TFPList.Create;
end;


procedure TTestQuicLossDetector.ClearLists;

var
  I: Integer;

begin
  for I:=0 to FAcked.Count-1 do
    TQuicSentPacket(FAcked[I]).Free;
  for I:=0 to FLost.Count-1 do
    TQuicSentPacket(FLost[I]).Free;
  FAcked.Clear;
  FLost.Clear;
end;


procedure TTestQuicLossDetector.TearDown;

begin
  ClearLists;
  FAcked.Free;
  FLost.Free;
  FDetector.Free;
  FCC.Free;
end;


procedure TTestQuicLossDetector.Send(aSpace: TQuicEncryptionLevel; aPn, aTime: QWord; aAckEliciting: Boolean;
  aInFlight: Boolean);

var
  lPacket: TQuicSentPacket;

begin
  lPacket:=TQuicSentPacket.Create;
  lPacket.PacketNumber:=aPn;
  lPacket.Size:=1000;
  lPacket.AckEliciting:=aAckEliciting;
  lPacket.InFlight:=aInFlight;
  FDetector.OnPacketSent(aSpace,lPacket,aTime);
end;


procedure TTestQuicLossDetector.Ack(aSpace: TQuicEncryptionLevel; aLow, aHigh, aNow: QWord; aDelay: QWord);

var
  lFrame: TQuicFrame;

begin
  lFrame:=QuicFrame(QUIC_FRAME_ACK);
  SetLength(lFrame.AckRanges,1);
  lFrame.AckRanges[0].Low:=aLow;
  lFrame.AckRanges[0].High:=aHigh;
  lFrame.AckDelay:=aDelay;
  FDetector.OnAckReceived(aSpace,lFrame,3,aNow,FAcked,FLost);
end;


procedure TTestQuicLossDetector.TestAckOfUnsentPacket;

begin
  try
    Ack(qelInitial,0,0,10);
    Fail('ACK of unsent packet must fail');
  except
    on E: EQuicError do
      AssertEquals('Protocol violation',QUIC_PROTOCOL_VIOLATION,E.ErrorCode);
  end;
end;


procedure TTestQuicLossDetector.TestAckAndRtt;

begin
  FDetector.HandshakeConfirmed:=True;
  Send(qelApplication,0,1000);
  Send(qelApplication,1,2000);
  AssertEquals('In flight',2000,FCC.BytesInFlight);
  Ack(qelApplication,0,1,52000,1000);
  AssertEquals('Both acked',2,FAcked.Count);
  AssertEquals('RTT sample',50000,FDetector.Rtt.LatestRtt);
  AssertEquals('In flight after ack',0,FCC.BytesInFlight);
  AssertNull('Nothing unacked',FDetector.OldestUnacked(qelApplication));
end;


procedure TTestQuicLossDetector.TestDuplicateAck;

begin
  Send(qelInitial,0,1000,False,False);
  Send(qelInitial,1,1000);
  Ack(qelInitial,0,0,5000);
  AssertEquals('Acked',1,FAcked.Count);
  AssertFalse('No RTT sample for a non ack-eliciting packet',FDetector.Rtt.HasSample);
  ClearLists;
  Ack(qelInitial,0,0,6000);
  AssertEquals('Nothing newly acked',0,FAcked.Count);
end;


procedure TTestQuicLossDetector.TestPacketThreshold;

var
  lProbe: Boolean;
  lSpace: TQuicEncryptionLevel;
  I: Integer;

begin
  for I:=0 to 4 do
    Send(qelInitial,I,1000+I);
  Ack(qelInitial,4,4,2000);
  AssertEquals('Packets 0 and 1 lost',2,FLost.Count);
  AssertEquals('First lost',0,TQuicSentPacket(FLost[0]).PacketNumber);
  AssertTrue('Loss timer armed',FDetector.Timer>0);
  ClearLists;
  FDetector.OnTimeout(FDetector.Timer,FLost,lProbe,lSpace);
  AssertEquals('Packet 2 lost by time',1,FLost.Count);
end;


procedure TTestQuicLossDetector.TestTimeThreshold;

var
  lProbe: Boolean;
  lSpace: TQuicEncryptionLevel;

begin
  Send(qelHandshake,0,1000);
  Send(qelHandshake,1,1100);
  Ack(qelHandshake,1,1,1600);
  AssertEquals('Nothing lost yet',0,FLost.Count);
  AssertEquals('Loss time armed',2000,FDetector.Timer);
  FDetector.OnTimeout(FDetector.Timer,FLost,lProbe,lSpace);
  AssertFalse('No probe',lProbe);
  AssertTrue('Handshake space',lSpace=qelHandshake);
  AssertEquals('Packet 0 lost',1,FLost.Count);
end;


procedure TTestQuicLossDetector.TestPtoSpaces;

var
  lProbe: Boolean;
  lSpace: TQuicEncryptionLevel;
  lTimer: QWord;

begin
  FDetector.PeerAddressValidated:=True;
  Send(qelApplication,0,1000);
  AssertEquals('Application ignored before confirmation',0,FDetector.Timer);
  Send(qelHandshake,0,5000);
  Send(qelInitial,0,2000);
  lTimer:=FDetector.Timer;
  AssertEquals('Initial PTO',2000+FDetector.PtoDuration,lTimer);
  FDetector.OnTimeout(lTimer,FLost,lProbe,lSpace);
  AssertTrue('Probe',lProbe);
  AssertTrue('Initial probe',lSpace=qelInitial);
  AssertEquals('Backoff',1,FDetector.PtoCount);
  FDetector.DiscardSpace(qelInitial);
  FDetector.DiscardSpace(qelHandshake);
  FDetector.HandshakeConfirmed:=True;
  FDetector.UpdateTimer(6000);
  AssertEquals('Application PTO includes max ACK delay',1000+FDetector.PtoDuration+FDetector.MaxAckDelay,
    FDetector.Timer);
end;


procedure TTestQuicLossDetector.TestPtoWithoutInFlight;

var
  lProbe: Boolean;
  lSpace: TQuicEncryptionLevel;

begin
  FDetector.UpdateTimer(1000);
  AssertEquals('Client anti-deadlock timer',1000+FDetector.PtoDuration,FDetector.Timer);
  FDetector.OnTimeout(FDetector.Timer,FLost,lProbe,lSpace);
  AssertTrue('Initial probe',lProbe and (lSpace=qelInitial));
  FDetector.HasHandshakeKeys:=True;
  FDetector.OnTimeout(FDetector.Timer,FLost,lProbe,lSpace);
  AssertTrue('Handshake probe',lProbe and (lSpace=qelHandshake));
end;


procedure TTestQuicLossDetector.TestTimerDisabled;

begin
  FDetector.PeerAddressValidated:=True;
  FDetector.UpdateTimer(1000);
  AssertEquals('Nothing in flight',0,FDetector.Timer);
  FDetector.AmplificationBlocked:=True;
  Send(qelInitial,0,1000);
  AssertEquals('Amplification limit',0,FDetector.Timer);
end;


procedure TTestQuicLossDetector.TestPersistentCongestion;

begin
  FDetector.PeerAddressValidated:=True;
  Send(qelInitial,0,1000);
  Ack(qelInitial,0,0,2000);
  ClearLists;
  Send(qelInitial,1,10000,False);
  Send(qelInitial,2,10000);
  Send(qelInitial,3,4000000);
  Send(qelInitial,4,8000000);
  Send(qelInitial,5,8000001);
  Ack(qelInitial,5,5,8000002);
  AssertEquals('Three lost',3,FLost.Count);
  AssertEquals('Minimum window after persistent congestion',2400,FCC.CongestionWindow);
end;


procedure TTestQuicLossDetector.TestNotPersistent;

begin
  Send(qelInitial,0,1000);
  Send(qelInitial,1,4000000);
  Send(qelInitial,2,8000000);
  Send(qelInitial,3,8000001);
  Ack(qelInitial,3,3,8000002);
  AssertEquals('Lost without RTT sample',2,FLost.Count);
  AssertEquals('No persistent congestion without a prior sample',6500,FCC.CongestionWindow);
  ClearLists;
  Send(qelInitial,4,9000000);
  Send(qelInitial,6,20000000,False,False);
  Send(qelInitial,7,30000000);
  Send(qelInitial,8,30000001);
  Send(qelInitial,9,30000001);
  Send(qelInitial,10,30000002);
  Ack(qelInitial,10,10,30000003);
  AssertEquals('Lost',4,FLost.Count);
  AssertEquals('Gap in packet numbers prevents persistent congestion',3342,FCC.CongestionWindow);
end;


procedure TTestQuicLossDetector.TestDiscardSpace;

begin
  Send(qelInitial,0,1000);
  Send(qelInitial,1,1000,False,False);
  FDetector.PtoCount:=2;
  FDetector.DiscardSpace(qelInitial);
  AssertEquals('In flight released',0,FCC.BytesInFlight);
  AssertEquals('Backoff reset',0,FDetector.PtoCount);
  AssertNull('Nothing left',FDetector.OldestUnacked(qelInitial));
end;


procedure TTestQuicLossDetector.TestPtoCountReset;

begin
  Send(qelInitial,0,1000);
  Send(qelInitial,1,1000);
  FDetector.PtoCount:=2;
  Ack(qelInitial,0,0,2000);
  AssertEquals('Kept before address validation',2,FDetector.PtoCount);
  FDetector.PeerAddressValidated:=True;
  Ack(qelInitial,1,1,2000);
  AssertEquals('Reset after validation',0,FDetector.PtoCount);
end;


procedure TTestQuicLossDetector.TestLossWithoutRttSample;

var
  lProbe: Boolean;
  lSpace: TQuicEncryptionLevel;

begin
  Send(qelInitial,0,1000);
  Send(qelInitial,1,1000,False);
  Ack(qelInitial,1,1,2000);
  AssertFalse('No RTT sample',FDetector.Rtt.HasSample);
  FDetector.OnTimeout(FDetector.Timer,FLost,lProbe,lSpace);
  AssertEquals('Lost by time',1,FLost.Count);
  AssertEquals('Halved, not persistent',6500,FCC.CongestionWindow);
end;


initialization
  RegisterTests([TTestQuicAckTracker,TTestQuicRtt,TTestQuicNewReno,TTestQuicLossDetector]);
end.
