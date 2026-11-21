{
  This file is part of the Free Component Library.
  Tests for FpQuic.Packets.
  See the file COPYING.FPC, included in this distribution, for details about the copyright.
}
unit utquicpackets;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpcunit, testregistry, FpQuic.Consts, FpQuic.Buffers, FpQuic.Packets;

type
  TTestQuicPackets = class(TTestCase)
  private
    function Parse(const aHex: String; out aHeader: TQuicPacketHeader; aCidLength: Integer = 8): Boolean;
  published
    procedure TestRfcClientInitialHeader;
    procedure TestLongHeaderRoundTrip;
    procedure TestHandshakeHeader;
    procedure TestShortHeader;
    procedure TestVersionNegotiation;
    procedure TestUnsupportedVersion;
    procedure TestRetry;
    procedure TestMalformedHeaders;
    procedure TestPacketNumberLength;
    procedure TestDecodePacketNumber;
  end;

implementation

function TTestQuicPackets.Parse(const aHex: String; out aHeader: TQuicPacketHeader; aCidLength: Integer): Boolean;

var
  lBytes: TBytes;

begin
  lBytes:=QuicHexToBytes(aHex);
  Result:=QuicParseHeader(PByte(lBytes),Length(lBytes),aCidLength,aHeader);
end;


procedure TTestQuicPackets.TestRfcClientInitialHeader;

var
  lHeader: TQuicPacketHeader;
  lHex: String;

begin
  lHex:='c000000001088394c8f03e5157080000449e'+StringOfChar('0',1182*2);
  AssertTrue('Parsed',Parse(lHex,lHeader));
  AssertTrue('Initial',lHeader.PacketType=qptInitial);
  AssertEquals('Version',QUIC_VERSION_1,lHeader.Version);
  AssertEquals('DCID','8394c8f03e515708',QuicBytesToHex(lHeader.DestCid));
  AssertEquals('SCID empty',0,Length(lHeader.SrcCid));
  AssertEquals('Token empty',0,Length(lHeader.Token));
  AssertEquals('PN offset',18,lHeader.PnOffset);
  AssertEquals('Packet length',1200,lHeader.PacketLength);
end;


procedure TTestQuicPackets.TestLongHeaderRoundTrip;

var
  lWriter: TQuicWriter;
  lHeader: TQuicPacketHeader;
  lBytes: TBytes;
  lPnOffset: Integer;

begin
  lWriter.Init;
  lPnOffset:=QuicWriteLongHeader(lWriter,qptInitial,QuicHexToBytes('0102'),QuicHexToBytes('030405'),
    QuicHexToBytes('aabb'),$1234,2,20);
  lWriter.WriteZeros(20);
  lBytes:=lWriter.ToBytes;
  AssertEquals('PN bytes','1234',QuicBytesToHex(Copy(lBytes,lPnOffset,2)));
  AssertTrue('Parsed',QuicParseHeader(PByte(lBytes),Length(lBytes)+5,8,lHeader));
  AssertTrue('Type',lHeader.PacketType=qptInitial);
  AssertEquals('First byte PN length bits',1,lHeader.FirstByte and 3);
  AssertEquals('DCID','0102',QuicBytesToHex(lHeader.DestCid));
  AssertEquals('SCID','030405',QuicBytesToHex(lHeader.SrcCid));
  AssertEquals('Token','aabb',QuicBytesToHex(lHeader.Token));
  AssertEquals('PN offset',lPnOffset,lHeader.PnOffset);
  AssertEquals('Packet length excludes trailing data',Length(lBytes),lHeader.PacketLength);
end;


procedure TTestQuicPackets.TestHandshakeHeader;

var
  lWriter: TQuicWriter;
  lHeader: TQuicPacketHeader;
  lBytes: TBytes;

begin
  lWriter.Init;
  QuicWriteLongHeader(lWriter,qptHandshake,QuicHexToBytes('01'),QuicHexToBytes('02'),nil,7,1,4);
  lWriter.WriteZeros(4);
  lBytes:=lWriter.ToBytes;
  AssertTrue('Parsed',QuicParseHeader(PByte(lBytes),Length(lBytes),8,lHeader));
  AssertTrue('Handshake',lHeader.PacketType=qptHandshake);
  lBytes[0]:=lBytes[0] and $CF or $10;
  AssertTrue('Parsed 0-RTT',QuicParseHeader(PByte(lBytes),Length(lBytes),8,lHeader));
  AssertTrue('0-RTT',lHeader.PacketType=qptZeroRtt);
end;


procedure TTestQuicPackets.TestShortHeader;

var
  lWriter: TQuicWriter;
  lHeader: TQuicPacketHeader;
  lBytes: TBytes;
  lPnOffset: Integer;

begin
  lWriter.Init;
  lPnOffset:=QuicWriteShortHeader(lWriter,QuicHexToBytes('1122334455667788'),True,$010203,3);
  lWriter.WriteZeros(10);
  lBytes:=lWriter.ToBytes;
  AssertEquals('PN offset',9,lPnOffset);
  AssertEquals('First byte',$46,lBytes[0]);
  AssertEquals('PN','010203',QuicBytesToHex(Copy(lBytes,9,3)));
  AssertTrue('Parsed',QuicParseHeader(PByte(lBytes),Length(lBytes),8,lHeader));
  AssertTrue('1-RTT',lHeader.PacketType=qptOneRtt);
  AssertEquals('DCID','1122334455667788',QuicBytesToHex(lHeader.DestCid));
  AssertEquals('Length',Length(lBytes),lHeader.PacketLength);
  lWriter.Init;
  QuicWriteShortHeader(lWriter,nil,False,1,1);
  AssertEquals('No key phase','4001',QuicBytesToHex(lWriter.ToBytes));
end;


procedure TTestQuicPackets.TestVersionNegotiation;

var
  lBytes: TBytes;
  lHeader: TQuicPacketHeader;

begin
  lBytes:=QuicBuildVersionNegotiation(QuicHexToBytes('aa'),QuicHexToBytes('bbcc'),[QUIC_VERSION_1,$ff00001d]);
  AssertEquals('Encoding','c000000000'+'01aa'+'02bbcc'+'00000001'+'ff00001d',QuicBytesToHex(lBytes));
  AssertTrue('Parsed',QuicParseHeader(PByte(lBytes),Length(lBytes),8,lHeader));
  AssertTrue('VN',lHeader.PacketType=qptVersionNegotiation);
  AssertEquals('Versions',2,Length(lHeader.Versions));
  AssertEquals('Second version',$ff00001d,lHeader.Versions[1]);
end;


procedure TTestQuicPackets.TestUnsupportedVersion;

var
  lHeader: TQuicPacketHeader;

begin
  AssertTrue('Parsed',Parse('c0 1a2a3a4a 02 aabb 01 cc ffff',lHeader));
  AssertTrue('Unsupported',lHeader.PacketType=qptUnsupported);
  AssertEquals('Version',$1a2a3a4a,lHeader.Version);
  AssertEquals('DCID','aabb',QuicBytesToHex(lHeader.DestCid));
  AssertEquals('SCID','cc',QuicBytesToHex(lHeader.SrcCid));
end;


procedure TTestQuicPackets.TestRetry;

var
  lBytes: TBytes;
  lHeader: TQuicPacketHeader;

begin
  lBytes:=QuicBuildRetry(QuicHexToBytes('01'),QuicHexToBytes('0203'),QuicHexToBytes('746f6b656e'));
  AssertEquals('Encoding','f000000001'+'0101'+'020203'+'746f6b656e',QuicBytesToHex(lBytes));
  SetLength(lBytes,Length(lBytes)+16);
  AssertTrue('Parsed',QuicParseHeader(PByte(lBytes),Length(lBytes),8,lHeader));
  AssertTrue('Retry',lHeader.PacketType=qptRetry);
  AssertEquals('Token','746f6b656e',QuicBytesToHex(lHeader.Token));
  AssertEquals('Length',Length(lBytes),lHeader.PacketLength);
end;


procedure TTestQuicPackets.TestMalformedHeaders;

var
  lHeader: TQuicPacketHeader;

begin
  AssertFalse('Empty',Parse('',lHeader));
  AssertFalse('Short without fixed bit',Parse('01 1122334455667788 00',lHeader));
  AssertFalse('Short too short',Parse('41 1122',lHeader));
  AssertFalse('Long without fixed bit',Parse('80 00000001 00 00 00 01 00',lHeader));
  AssertFalse('CID too long',Parse('c0 00000001 15'+StringOfChar('0',42)+' 00 00 01 00',lHeader));
  AssertFalse('SCID too long',Parse('c0 00000001 00 15'+StringOfChar('0',42)+' 00 01 00',lHeader));
  AssertFalse('Length beyond datagram',Parse('e0 00000001 00 00 05 00',lHeader));
  AssertFalse('Truncated version',Parse('c0 0000',lHeader));
  AssertFalse('Retry too short',Parse('f0 00000001 00 00 0102',lHeader));
end;


procedure TTestQuicPackets.TestPacketNumberLength;

begin
  AssertEquals('First packet',1,QuicPacketNumberLength(0,-1));
  AssertEquals('Small range',1,QuicPacketNumberLength(100,0));
  AssertEquals('RFC example 16 bits',2,QuicPacketNumberLength($ac5c02,$abe8b3));
  AssertEquals('RFC example 24 bits',3,QuicPacketNumberLength($ace8fe,$abe8b3));
  AssertEquals('Large range',4,QuicPacketNumberLength($10000000,0));
end;


procedure TTestQuicPackets.TestDecodePacketNumber;

begin
  AssertEquals('RFC example',$a82f9b32,QuicDecodePacketNumber($a82f30ea,$9b32,2));
  AssertEquals('No largest',5,QuicDecodePacketNumber(-1,5,1));
  AssertEquals('Wrap forward',$200,QuicDecodePacketNumber($1fe,$00,1));
  AssertEquals('Wrap backward',$ff,QuicDecodePacketNumber($100,$ff,1));
  AssertEquals('Same window',$105,QuicDecodePacketNumber($100,$05,1));
end;


initialization
  RegisterTest(TTestQuicPackets);
end.
