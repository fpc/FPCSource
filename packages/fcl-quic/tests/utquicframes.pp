{
  This file is part of the Free Component Library.
  Tests for FpQuic.Frames.
  See the file COPYING.FPC, included in this distribution, for details about the copyright.
}
unit utquicframes;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpcunit, testregistry, FpQuic.Consts, FpQuic.Buffers, FpQuic.Frames;

type
  TTestQuicFrames = class(TTestCase)
  private
    function RoundTrip(const aFrame: TQuicFrame): TQuicFrame;
    procedure ExpectDecodeError(const aHex: String; aCode: QWord = QUIC_FRAME_ENCODING_ERROR);
  published
    procedure TestSimpleFrames;
    procedure TestPaddingRun;
    procedure TestAck;
    procedure TestAckEcn;
    procedure TestAckErrors;
    procedure TestStreamFrames;
    procedure TestStreamWithoutLength;
    procedure TestStreamErrors;
    procedure TestCryptoAndToken;
    procedure TestFlowControlFrames;
    procedure TestConnectionIdFrames;
    procedure TestPathAndClose;
    procedure TestUnknownFrames;
    procedure TestFrameRules;
  end;

implementation

function TTestQuicFrames.RoundTrip(const aFrame: TQuicFrame): TQuicFrame;

var
  lWriter: TQuicWriter;
  lReader: TQuicReader;
  lBytes: TBytes;

begin
  lWriter.Init;
  QuicEncodeFrame(lWriter,aFrame);
  lBytes:=lWriter.ToBytes;
  AssertEquals('Frame size',Length(lBytes),QuicFrameSize(aFrame));
  lReader.InitBytes(lBytes);
  Result:=QuicDecodeFrame(lReader);
  AssertTrue('Whole frame consumed',lReader.EOF);
  AssertEquals('Frame type',aFrame.FrameType,Result.FrameType);
end;


procedure TTestQuicFrames.ExpectDecodeError(const aHex: String; aCode: QWord);

var
  lReader: TQuicReader;
  lBytes: TBytes;

begin
  lBytes:=QuicHexToBytes(aHex);
  lReader.InitBytes(lBytes);
  try
    QuicDecodeFrame(lReader);
    Fail('Expected decode error for '+aHex);
  except
    on E: EQuicError do
      AssertEquals('Error code for '+aHex,aCode,E.ErrorCode);
  end;
end;


procedure TTestQuicFrames.TestSimpleFrames;

begin
  RoundTrip(QuicFrame(QUIC_FRAME_PING));
  RoundTrip(QuicFrame(QUIC_FRAME_HANDSHAKE_DONE));
  RoundTrip(QuicFrame(QUIC_FRAME_PADDING));
end;


procedure TTestQuicFrames.TestPaddingRun;

var
  lReader: TQuicReader;
  lBytes: TBytes;
  lFrame: TQuicFrame;

begin
  lBytes:=QuicHexToBytes('00000000 01');
  lReader.InitBytes(lBytes);
  lFrame:=QuicDecodeFrame(lReader);
  AssertEquals('Padding',QUIC_FRAME_PADDING,lFrame.FrameType);
  AssertEquals('Padding run consumed',4,lReader.Position);
  lFrame:=QuicDecodeFrame(lReader);
  AssertEquals('Ping after padding',QUIC_FRAME_PING,lFrame.FrameType);
end;


procedure TTestQuicFrames.TestAck;

var
  lFrame, lOut: TQuicFrame;

begin
  lFrame:=QuicFrame(QUIC_FRAME_ACK);
  SetLength(lFrame.AckRanges,3);
  lFrame.AckRanges[0].Low:=95;
  lFrame.AckRanges[0].High:=100;
  lFrame.AckRanges[1].Low:=50;
  lFrame.AckRanges[1].High:=60;
  lFrame.AckRanges[2].Low:=0;
  lFrame.AckRanges[2].High:=0;
  lFrame.AckDelay:=1234;
  lOut:=RoundTrip(lFrame);
  AssertEquals('Ranges',3,Length(lOut.AckRanges));
  AssertEquals('Largest',100,lOut.AckRanges[0].High);
  AssertEquals('First low',95,lOut.AckRanges[0].Low);
  AssertEquals('Second high',60,lOut.AckRanges[1].High);
  AssertEquals('Second low',50,lOut.AckRanges[1].Low);
  AssertEquals('Third',0,lOut.AckRanges[2].High);
  AssertEquals('Delay',1234,lOut.AckDelay);
end;


procedure TTestQuicFrames.TestAckEcn;

var
  lFrame, lOut: TQuicFrame;

begin
  lFrame:=QuicFrame(QUIC_FRAME_ACK_ECN);
  SetLength(lFrame.AckRanges,1);
  lFrame.AckRanges[0].Low:=3;
  lFrame.AckRanges[0].High:=7;
  lFrame.Ect0:=1;
  lFrame.Ect1:=2;
  lFrame.EcnCe:=3;
  lOut:=RoundTrip(lFrame);
  AssertEquals('Ect0',1,lOut.Ect0);
  AssertEquals('Ect1',2,lOut.Ect1);
  AssertEquals('CE',3,lOut.EcnCe);
end;


procedure TTestQuicFrames.TestAckErrors;

begin
  ExpectDecodeError('02 05 00 00 06');
  ExpectDecodeError('02 05 00 05 00');
  ExpectDecodeError('02 0a 00 01 00 09 00');
  ExpectDecodeError('02 0a 00 01 00 00 09');
  ExpectDecodeError('02 0a 00 01');
end;


procedure TTestQuicFrames.TestStreamFrames;

var
  lFrame, lOut: TQuicFrame;

begin
  lFrame:=QuicFrame(QUIC_FRAME_STREAM);
  lFrame.StreamId:=4;
  lFrame.Data:=QuicHexToBytes('68656c6c6f');
  lOut:=RoundTrip(lFrame);
  AssertEquals('Stream id',4,lOut.StreamId);
  AssertEquals('Offset',0,lOut.Offset);
  AssertFalse('No fin',lOut.Fin);
  AssertEquals('Data','68656c6c6f',QuicBytesToHex(lOut.Data));
  lFrame.Offset:=1000;
  lFrame.Fin:=True;
  lOut:=RoundTrip(lFrame);
  AssertEquals('Offset set',1000,lOut.Offset);
  AssertTrue('Fin set',lOut.Fin);
end;


procedure TTestQuicFrames.TestStreamWithoutLength;

var
  lReader: TQuicReader;
  lBytes: TBytes;
  lFrame: TQuicFrame;

begin
  lBytes:=QuicHexToBytes('0d 01 05 aabb');
  lReader.InitBytes(lBytes);
  lFrame:=QuicDecodeFrame(lReader);
  AssertEquals('Normalised type',QUIC_FRAME_STREAM,lFrame.FrameType);
  AssertEquals('Offset',5,lFrame.Offset);
  AssertTrue('Fin',lFrame.Fin);
  AssertEquals('Rest of packet','aabb',QuicBytesToHex(lFrame.Data));
end;


procedure TTestQuicFrames.TestStreamErrors;

begin
  ExpectDecodeError('0e 00 ffffffffffffffff 01 aa');
  ExpectDecodeError('0a 00 05 aa');
end;


procedure TTestQuicFrames.TestCryptoAndToken;

var
  lFrame, lOut: TQuicFrame;

begin
  lFrame:=QuicFrame(QUIC_FRAME_CRYPTO);
  lFrame.Offset:=77;
  lFrame.Data:=QuicHexToBytes('0102');
  lOut:=RoundTrip(lFrame);
  AssertEquals('Crypto offset',77,lOut.Offset);
  AssertEquals('Crypto data','0102',QuicBytesToHex(lOut.Data));
  ExpectDecodeError('06 ffffffffffffffff 01 aa');
  lFrame:=QuicFrame(QUIC_FRAME_NEW_TOKEN);
  lFrame.Data:=QuicHexToBytes('abcdef');
  lOut:=RoundTrip(lFrame);
  AssertEquals('Token','abcdef',QuicBytesToHex(lOut.Data));
  ExpectDecodeError('07 00');
end;


procedure TTestQuicFrames.TestFlowControlFrames;

const
  ValueTypes: array[0..5] of QWord = (QUIC_FRAME_MAX_DATA,QUIC_FRAME_DATA_BLOCKED,QUIC_FRAME_MAX_STREAMS_BIDI,
    QUIC_FRAME_MAX_STREAMS_UNI,QUIC_FRAME_STREAMS_BLOCKED_BIDI,QUIC_FRAME_STREAMS_BLOCKED_UNI);

var
  lFrame, lOut: TQuicFrame;
  I: Integer;

begin
  for I:=0 to High(ValueTypes) do
    begin
    lFrame:=QuicFrame(ValueTypes[I]);
    lFrame.Value:=1000+I;
    lOut:=RoundTrip(lFrame);
    AssertEquals('Value '+IntToStr(I),QWord(1000+I),lOut.Value);
    end;
  lFrame:=QuicFrame(QUIC_FRAME_MAX_STREAM_DATA);
  lFrame.StreamId:=8;
  lFrame.Value:=99;
  lOut:=RoundTrip(lFrame);
  AssertEquals('MSD stream',8,lOut.StreamId);
  AssertEquals('MSD value',99,lOut.Value);
  lFrame.FrameType:=QUIC_FRAME_STREAM_DATA_BLOCKED;
  lOut:=RoundTrip(lFrame);
  AssertEquals('SDB value',99,lOut.Value);
  lFrame:=QuicFrame(QUIC_FRAME_RESET_STREAM);
  lFrame.StreamId:=3;
  lFrame.ErrorCode:=7;
  lFrame.Value:=500;
  lOut:=RoundTrip(lFrame);
  AssertEquals('Reset stream',3,lOut.StreamId);
  AssertEquals('Reset error',7,lOut.ErrorCode);
  AssertEquals('Reset final size',500,lOut.Value);
  lFrame:=QuicFrame(QUIC_FRAME_STOP_SENDING);
  lFrame.StreamId:=3;
  lFrame.ErrorCode:=9;
  lOut:=RoundTrip(lFrame);
  AssertEquals('Stop error',9,lOut.ErrorCode);
  ExpectDecodeError('12 d000000000000001');
end;


procedure TTestQuicFrames.TestConnectionIdFrames;

var
  lFrame, lOut: TQuicFrame;

begin
  lFrame:=QuicFrame(QUIC_FRAME_NEW_CONNECTION_ID);
  lFrame.SequenceNumber:=2;
  lFrame.RetirePriorTo:=1;
  lFrame.ConnectionId:=QuicHexToBytes('0102030405060708');
  lFrame.ResetToken:=QuicHexToBytes('00112233445566778899aabbccddeeff');
  lOut:=RoundTrip(lFrame);
  AssertEquals('Seq',2,lOut.SequenceNumber);
  AssertEquals('Retire',1,lOut.RetirePriorTo);
  AssertEquals('CID','0102030405060708',QuicBytesToHex(lOut.ConnectionId));
  AssertEquals('Token','00112233445566778899aabbccddeeff',QuicBytesToHex(lOut.ResetToken));
  ExpectDecodeError('18 01 00 00');
  ExpectDecodeError('18 01 00 15');
  ExpectDecodeError('18 01 02 01 aa 00112233445566778899aabbccddeeff');
  lFrame:=QuicFrame(QUIC_FRAME_RETIRE_CONNECTION_ID);
  lFrame.SequenceNumber:=5;
  lOut:=RoundTrip(lFrame);
  AssertEquals('Retire seq',5,lOut.SequenceNumber);
end;


procedure TTestQuicFrames.TestPathAndClose;

var
  lFrame, lOut: TQuicFrame;

begin
  lFrame:=QuicFrame(QUIC_FRAME_PATH_CHALLENGE);
  lFrame.Data:=QuicHexToBytes('0102030405060708');
  lOut:=RoundTrip(lFrame);
  AssertEquals('Challenge','0102030405060708',QuicBytesToHex(lOut.Data));
  lFrame.FrameType:=QUIC_FRAME_PATH_RESPONSE;
  RoundTrip(lFrame);
  lFrame:=QuicFrame(QUIC_FRAME_CONNECTION_CLOSE);
  lFrame.ErrorCode:=QUIC_PROTOCOL_VIOLATION;
  lFrame.CloseFrameType:=QUIC_FRAME_ACK;
  lFrame.Data:=TEncoding.UTF8.GetBytes('bad');
  lOut:=RoundTrip(lFrame);
  AssertEquals('Close error',QUIC_PROTOCOL_VIOLATION,lOut.ErrorCode);
  AssertEquals('Close frame type',QUIC_FRAME_ACK,lOut.CloseFrameType);
  AssertEquals('Reason','626164',QuicBytesToHex(lOut.Data));
  lFrame.FrameType:=QUIC_FRAME_CONNECTION_CLOSE_APP;
  lFrame.CloseFrameType:=0;
  lOut:=RoundTrip(lFrame);
  AssertEquals('App close error',QUIC_PROTOCOL_VIOLATION,lOut.ErrorCode);
end;


procedure TTestQuicFrames.TestUnknownFrames;

var
  lWriter: TQuicWriter;

begin
  ExpectDecodeError('1f');
  ExpectDecodeError('4020');
  lWriter.Init;
  try
    QuicEncodeFrame(lWriter,QuicFrame($1f));
    Fail('Encoding an unknown frame type must fail');
  except
    on E: EQuicError do
      AssertEquals('Internal error',QUIC_INTERNAL_ERROR,E.ErrorCode);
  end;
end;


procedure TTestQuicFrames.TestFrameRules;

begin
  AssertTrue('ACK in Initial',QuicFrameAllowed(QUIC_FRAME_ACK,qelInitial));
  AssertTrue('CRYPTO in Handshake',QuicFrameAllowed(QUIC_FRAME_CRYPTO,qelHandshake));
  AssertFalse('STREAM in Initial',QuicFrameAllowed(QUIC_FRAME_STREAM,qelInitial));
  AssertFalse('App close in Handshake',QuicFrameAllowed(QUIC_FRAME_CONNECTION_CLOSE_APP,qelHandshake));
  AssertFalse('Large type in Initial',QuicFrameAllowed(1000,qelInitial));
  AssertTrue('STREAM in 1-RTT',QuicFrameAllowed(QUIC_FRAME_STREAM,qelApplication));
  AssertTrue('PING elicits',QuicFrameAckEliciting(QUIC_FRAME_PING));
  AssertFalse('ACK does not elicit',QuicFrameAckEliciting(QUIC_FRAME_ACK));
  AssertFalse('PADDING does not elicit',QuicFrameAckEliciting(QUIC_FRAME_PADDING));
  AssertFalse('CLOSE does not elicit',QuicFrameAckEliciting(QUIC_FRAME_CONNECTION_CLOSE));
end;


initialization
  RegisterTest(TTestQuicFrames);
end.
