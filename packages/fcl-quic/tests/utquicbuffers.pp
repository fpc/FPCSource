{
  This file is part of the Free Component Library.
  Tests for FpQuic.Consts and FpQuic.Buffers.
  See the file COPYING.FPC, included in this distribution, for details about the copyright.
}
unit utquicbuffers;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpcunit, testregistry, FpQuic.Consts, FpQuic.Buffers;

type
  TTestQuicBuffers = class(TTestCase)
  private
    procedure ExpectEncodingError(const aHex: String; aOp: Integer);
  published
    procedure TestVarIntSizes;
    procedure TestVarIntRfcSamples;
    procedure TestVarIntTooLarge;
    procedure TestVarIntSized;
    procedure TestWriterRoundTrip;
    procedure TestReaderErrors;
    procedure TestReaderPeekSkip;
    procedure TestHexHelpers;
    procedure TestBytesEqual;
    procedure TestRangeSetAdd;
    procedure TestRangeSetRemove;
    procedure TestErrorClass;
  end;

implementation

procedure TTestQuicBuffers.ExpectEncodingError(const aHex: String; aOp: Integer);

var
  lReader: TQuicReader;
  lBytes: TBytes;

begin
  lBytes:=QuicHexToBytes(aHex);
  lReader.InitBytes(lBytes);
  try
    case aOp of
      0: lReader.ReadByte;
      1: lReader.ReadUInt16;
      2: lReader.ReadUInt32;
      3: lReader.ReadVarInt;
      4: lReader.ReadBytes(5);
      5: lReader.Skip(5);
      6: lReader.PeekByte;
      7: lReader.ReadVarBytes;
    end;
    Fail('Expected EQuicError for operation '+IntToStr(aOp));
  except
    on E: EQuicError do
      AssertEquals('Error code',QUIC_FRAME_ENCODING_ERROR,E.ErrorCode);
  end;
end;


procedure TTestQuicBuffers.TestVarIntSizes;

begin
  AssertEquals('63',1,QuicVarIntSize(63));
  AssertEquals('64',2,QuicVarIntSize(64));
  AssertEquals('16383',2,QuicVarIntSize(16383));
  AssertEquals('16384',4,QuicVarIntSize(16384));
  AssertEquals('2^30-1',4,QuicVarIntSize(1073741823));
  AssertEquals('2^30',8,QuicVarIntSize(1073741824));
  AssertEquals('max',8,QuicVarIntSize(QUIC_MAX_VARINT));
end;


procedure TTestQuicBuffers.TestVarIntRfcSamples;

const
  Samples: array[0..4] of String = ('c2197c5eff14e88c','9d7f3e7d','7bbd','25','4025');
  Values: array[0..4] of QWord = (151288809941952652,494878333,15293,37,37);

var
  lReader: TQuicReader;
  lWriter: TQuicWriter;
  lBytes: TBytes;
  I: Integer;

begin
  for I:=0 to High(Samples) do
    begin
    lBytes:=QuicHexToBytes(Samples[I]);
    lReader.InitBytes(lBytes);
    AssertEquals('Decode '+Samples[I],Values[I],lReader.ReadVarInt);
    AssertTrue('Consumed '+Samples[I],lReader.EOF);
    end;
  for I:=0 to 3 do
    begin
    lWriter.Init;
    lWriter.WriteVarInt(Values[I]);
    AssertEquals('Encode '+Samples[I],Samples[I],QuicBytesToHex(lWriter.ToBytes));
    end;
end;


procedure TTestQuicBuffers.TestVarIntTooLarge;

var
  lWriter: TQuicWriter;

begin
  lWriter.Init;
  try
    lWriter.WriteVarInt(QUIC_MAX_VARINT+1);
    Fail('Expected EQuicError');
  except
    on E: EQuicError do
      AssertEquals('Error code',QUIC_INTERNAL_ERROR,E.ErrorCode);
  end;
  try
    lWriter.WriteVarIntSized(64,1);
    Fail('Expected EQuicError for size');
  except
    on E: EQuicError do
      AssertEquals('Error code size',QUIC_INTERNAL_ERROR,E.ErrorCode);
  end;
end;


procedure TTestQuicBuffers.TestVarIntSized;

var
  lWriter: TQuicWriter;

begin
  lWriter.Init;
  lWriter.WriteVarIntSized(37,2);
  lWriter.WriteVarIntSized(5,4);
  lWriter.WriteVarIntSized(5,8);
  AssertEquals('Sized encodings','4025'+'80000005'+'c000000000000005',QuicBytesToHex(lWriter.ToBytes));
end;


procedure TTestQuicBuffers.TestWriterRoundTrip;

var
  lReader: TQuicReader;
  lWriter: TQuicWriter;
  lBytes, lBlob: TBytes;
  I: Integer;

begin
  lWriter.Init;
  lWriter.WriteByte($AB);
  lWriter.WriteUInt16($1234);
  lWriter.WriteUInt32($89ABCDEF);
  lWriter.WriteVarBytes(QuicHexToBytes('010203'));
  lWriter.WriteBytes(nil);
  lWriter.WriteBuffer(nil,0);
  lWriter.WriteZeros(0);
  lWriter.WriteZeros(3);
  lBlob:=nil;
  SetLength(lBlob,300);
  for I:=0 to 299 do
    lBlob[I]:=I and $FF;
  lWriter.WriteBytes(lBlob);
  AssertEquals('Length',1+2+4+4+3+300,lWriter.Length);
  lBytes:=lWriter.ToBytes;
  lReader.InitBytes(lBytes);
  AssertEquals('Byte',$AB,lReader.ReadByte);
  AssertEquals('UInt16',$1234,lReader.ReadUInt16);
  AssertEquals('UInt32',$89ABCDEF,lReader.ReadUInt32);
  AssertEquals('VarBytes','010203',QuicBytesToHex(lReader.ReadVarBytes));
  AssertEquals('Position',11,lReader.Position);
  AssertEquals('Zeros','000000',QuicBytesToHex(lReader.ReadBytes(3)));
  AssertTrue('Blob',QuicBytesEqual(lBlob,lReader.ReadBytes(300)));
  AssertEquals('Remaining',0,lReader.Remaining);
  AssertTrue('EOF',lReader.EOF);
end;


procedure TTestQuicBuffers.TestReaderErrors;

begin
  ExpectEncodingError('',0);
  ExpectEncodingError('01',1);
  ExpectEncodingError('010203',2);
  ExpectEncodingError('40',3);
  ExpectEncodingError('',3);
  ExpectEncodingError('0102',4);
  ExpectEncodingError('0102',5);
  ExpectEncodingError('',6);
  ExpectEncodingError('0501',7);
end;


procedure TTestQuicBuffers.TestReaderPeekSkip;

var
  lReader: TQuicReader;
  lBytes: TBytes;

begin
  lBytes:=QuicHexToBytes('0a0b0c');
  lReader.Init(PByte(lBytes),3);
  AssertEquals('Peek',$0A,lReader.PeekByte);
  lReader.Skip(2);
  AssertEquals('After skip',$0C,lReader.ReadByte);
  try
    lReader.Skip(-1);
    Fail('Negative skip must fail');
  except
    on E: EQuicError do
      AssertEquals('Error code',QUIC_FRAME_ENCODING_ERROR,E.ErrorCode);
  end;
end;


procedure TTestQuicBuffers.TestHexHelpers;

begin
  AssertEquals('Hex with spaces','0a1bff',QuicBytesToHex(QuicHexToBytes(' 0A 1b'#10'FF ')));
  AssertEquals('Empty','',QuicBytesToHex(nil));
  AssertEquals('BytesOf','0203',QuicBytesToHex(QuicBytesOf(PByte(QuicHexToBytes('0203')),2)));
  AssertEquals('BytesOf empty',0,Length(QuicBytesOf(nil,0)));
end;


procedure TTestQuicBuffers.TestBytesEqual;

var
  lBytes, lAlias: TBytes;

begin
  AssertTrue('Empty equal',QuicBytesEqual(nil,nil));
  AssertTrue('Equal',QuicBytesEqual(QuicHexToBytes('0102'),QuicHexToBytes('0102')));
  AssertFalse('Different content',QuicBytesEqual(QuicHexToBytes('0102'),QuicHexToBytes('0103')));
  AssertFalse('Different length',QuicBytesEqual(QuicHexToBytes('0102'),QuicHexToBytes('01')));
  AssertTrue('Constant time: empty equal',QuicBytesEqualConstantTime(nil,nil));
  AssertTrue('Constant time: equal',QuicBytesEqualConstantTime(QuicHexToBytes('0102'),QuicHexToBytes('0102')));
  AssertFalse('Constant time: different content',
    QuicBytesEqualConstantTime(QuicHexToBytes('0102'),QuicHexToBytes('0103')));
  AssertFalse('Constant time: different length',QuicBytesEqualConstantTime(QuicHexToBytes('0102'),QuicHexToBytes('01')));
  lBytes:=QuicHexToBytes('0102');
  lAlias:=lBytes;
  QuicWipe(lBytes);
  AssertEquals('Wiped array released',0,Length(lBytes));
  AssertEquals('Content overwritten','0000',QuicBytesToHex(lAlias));
end;


procedure TTestQuicBuffers.TestRangeSetAdd;

var
  lSet: TQuicRangeSet;

begin
  lSet:=TQuicRangeSet.Create;
  try
    lSet.Add(10,12);
    lSet.Add(20,22);
    lSet.Add(0,1);
    AssertEquals('Three ranges',3,lSet.Count);
    lSet.Add(13,13);
    AssertEquals('Adjacent merge',3,lSet.Count);
    AssertEquals('Merged high',13,lSet[1].High);
    lSet.Add(14,19);
    AssertEquals('Bridge merge',2,lSet.Count);
    AssertEquals('Bridge low',10,lSet[1].Low);
    AssertEquals('Bridge high',22,lSet[1].High);
    lSet.Add(5,30);
    AssertEquals('Overlap merge',2,lSet.Count);
    AssertEquals('Overlap low',5,lSet[1].Low);
    AssertEquals('Min',0,lSet.Min);
    AssertEquals('Max',30,lSet.Max);
    lSet.Add(2,4);
    AssertEquals('Adjacent below',1,lSet.Count);
    AssertTrue('Contains',lSet.Contains(17));
    AssertFalse('Not contains',lSet.Contains(31));
    AssertTrue('Contains range',lSet.ContainsRange(3,30));
    AssertFalse('Not contains range',lSet.ContainsRange(3,31));
    lSet.Clear;
    AssertEquals('Cleared',0,lSet.Count);
  finally
    lSet.Free;
  end;
end;


procedure TTestQuicBuffers.TestRangeSetRemove;

var
  lSet: TQuicRangeSet;

begin
  lSet:=TQuicRangeSet.Create;
  try
    lSet.Add(0,100);
    lSet.Remove(10,19);
    AssertEquals('Split',2,lSet.Count);
    AssertEquals('Left high',9,lSet[0].High);
    AssertEquals('Right low',20,lSet[1].Low);
    lSet.Remove(0,5);
    AssertEquals('Trim left',6,lSet.Min);
    lSet.Remove(95,200);
    AssertEquals('Trim right',94,lSet.Max);
    lSet.Remove(300,400);
    AssertEquals('Remove outside',2,lSet.Count);
    lSet.Remove(0,1000);
    AssertEquals('Remove all',0,lSet.Count);
  finally
    lSet.Free;
  end;
end;


procedure TTestQuicBuffers.TestErrorClass;

var
  lError: EQuicError;

begin
  lError:=EQuicError.CreateCode(QUIC_PROTOCOL_VIOLATION,'msg',QUIC_FRAME_ACK);
  try
    AssertEquals('Code',QUIC_PROTOCOL_VIOLATION,lError.ErrorCode);
    AssertEquals('Frame',QUIC_FRAME_ACK,lError.FrameType);
    AssertEquals('Message','msg',lError.Message);
  finally
    lError.Free;
  end;
end;


initialization
  RegisterTest(TTestQuicBuffers);
end.
