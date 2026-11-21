{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    QUIC packet header parsing and building, packet number encoding (RFC 9000 sections 17 and A).

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpQuic.Packets;

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, FpQuic.Consts, FpQuic.Buffers;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils, FpQuic.Consts, FpQuic.Buffers;
{$ENDIF FPC_DOTTEDUNITS}

type
  // Packet types; qptUnsupported is a long header packet of an unknown version.
  TQuicPacketType = (qptInitial, qptZeroRtt, qptHandshake, qptRetry, qptVersionNegotiation,
                     qptOneRtt, qptUnsupported);

  // Header fields readable before header protection is removed.
  TQuicPacketHeader = record
    PacketType: TQuicPacketType;
    FirstByte: Byte;
    Version: Cardinal;
    DestCid: TBytes;
    SrcCid: TBytes;
    // Initial token or Retry token.
    Token: TBytes;
    // Offset of the packet number within the packet; Initial, Handshake, 0-RTT and 1-RTT only.
    PnOffset: Integer;
    // Number of bytes of this packet in the datagram.
    PacketLength: Integer;
    // Versions listed in a Version Negotiation packet.
    Versions: array of Cardinal;
  end;

// Parses the header of the packet at aData (aLength bytes left in the datagram).
// aShortCidLength is the length of connection IDs this endpoint issued. Returns False for malformed headers.
function QuicParseHeader(aData: PByte; aLength, aShortCidLength: Integer; out aHeader: TQuicPacketHeader): Boolean;

// Number of bytes (1-4) to encode packet number aPn; aLargestAcked is -1 when nothing is acknowledged.
function QuicPacketNumberLength(aPn: QWord; aLargestAcked: Int64): Integer;

// Recovers a full packet number from aTruncated (aLength bytes); aLargest is -1 when no packet was received.
function QuicDecodePacketNumber(aLargest: Int64; aTruncated: QWord; aLength: Integer): QWord;

// Writes a long header up to and including the packet number and returns the packet number offset.
// aPayloadLength is the number of bytes after the packet number, including the AEAD tag.
function QuicWriteLongHeader(var aWriter: TQuicWriter; aType: TQuicPacketType; const aDestCid, aSrcCid,
  aToken: TBytes; aPn: QWord; aPnLength, aPayloadLength: Integer): Integer;

// Writes a short (1-RTT) header and returns the packet number offset.
function QuicWriteShortHeader(var aWriter: TQuicWriter; const aDestCid: TBytes; aKeyPhase: Boolean;
  aPn: QWord; aPnLength: Integer): Integer;

// Builds a Version Negotiation packet echoing the connection IDs of a received packet.
function QuicBuildVersionNegotiation(const aDestCid, aSrcCid: TBytes; const aVersions: array of Cardinal): TBytes;

// Builds a Retry packet without its integrity tag.
function QuicBuildRetry(const aDestCid, aSrcCid, aToken: TBytes): TBytes;

implementation

const
  LongTypeBits: array[qptInitial..qptRetry] of Byte = ($00,$10,$20,$30);

// Parses the long header fields of a QUIC version 1 packet after the connection IDs.
procedure ParseLongV1(var aReader: TQuicReader; aLength: Integer; var aHeader: TQuicPacketHeader);

var
  lPayload: QWord;

begin
  case (aHeader.FirstByte shr 4) and 3 of
    0: aHeader.PacketType:=qptInitial;
    1: aHeader.PacketType:=qptZeroRtt;
    2: aHeader.PacketType:=qptHandshake;
    3: aHeader.PacketType:=qptRetry;
  end;
  if aHeader.PacketType=qptRetry then
    begin
    aHeader.Token:=aReader.ReadBytes(aReader.Remaining-QUIC_AEAD_TAG_LENGTH);
    aHeader.PacketLength:=aLength;
    Exit;
    end;
  if aHeader.PacketType=qptInitial then
    aHeader.Token:=aReader.ReadVarBytes;
  lPayload:=aReader.ReadVarInt;
  aHeader.PnOffset:=aReader.Position;
  if lPayload>QWord(aReader.Remaining) then
    aReader.Skip(aReader.Remaining+1);
  aHeader.PacketLength:=aHeader.PnOffset+lPayload;
end;


function QuicParseHeader(aData: PByte; aLength, aShortCidLength: Integer; out aHeader: TQuicPacketHeader): Boolean;

var
  lReader: TQuicReader;
  lCidLength: Integer;

begin
  aHeader:=Default(TQuicPacketHeader);
  lReader.Init(aData,aLength);
  try
    aHeader.FirstByte:=lReader.ReadByte;
    if (aHeader.FirstByte and $80)=0 then
      begin
      aHeader.PacketType:=qptOneRtt;
      aHeader.Version:=QUIC_VERSION_1;
      aHeader.DestCid:=lReader.ReadBytes(aShortCidLength);
      aHeader.PnOffset:=lReader.Position;
      aHeader.PacketLength:=aLength;
      Exit((aHeader.FirstByte and $40)<>0);
      end;
    aHeader.Version:=lReader.ReadUInt32;
    lCidLength:=lReader.ReadByte;
    aHeader.DestCid:=lReader.ReadBytes(lCidLength);
    lCidLength:=lReader.ReadByte;
    aHeader.SrcCid:=lReader.ReadBytes(lCidLength);
    aHeader.PacketLength:=aLength;
    if aHeader.Version=0 then
      begin
      aHeader.PacketType:=qptVersionNegotiation;
      while lReader.Remaining>=4 do
        Insert(lReader.ReadUInt32,aHeader.Versions,Length(aHeader.Versions));
      Exit(True);
      end;
    if aHeader.Version<>QUIC_VERSION_1 then
      begin
      aHeader.PacketType:=qptUnsupported;
      Exit(True);
      end;
    if (Length(aHeader.DestCid)>QUIC_MAX_CID_LENGTH) or (Length(aHeader.SrcCid)>QUIC_MAX_CID_LENGTH)
       or ((aHeader.FirstByte and $40)=0) then
      Exit(False);
    ParseLongV1(lReader,aLength,aHeader);
    Result:=True;
  except
    on EQuicError do
      Result:=False;
  end;
end;


function QuicPacketNumberLength(aPn: QWord; aLargestAcked: Int64): Integer;

var
  lRange: QWord;

begin
  if aLargestAcked<0 then
    lRange:=(aPn+1)*2
  else
    lRange:=(aPn-QWord(aLargestAcked))*2;
  if lRange<=$FF then
    Result:=1
  else if lRange<=$FFFF then
    Result:=2
  else if lRange<=$FFFFFF then
    Result:=3
  else
    Result:=4;
end;


function QuicDecodePacketNumber(aLargest: Int64; aTruncated: QWord; aLength: Integer): QWord;

var
  lExpected, lWin, lHalf, lMask, lCandidate: QWord;

begin
  lExpected:=QWord(aLargest+1);
  lWin:=QWord(1) shl (aLength*8);
  lHalf:=lWin div 2;
  lMask:=lWin-1;
  lCandidate:=(lExpected and not lMask) or aTruncated;
  if (lCandidate+lHalf<=lExpected) and (lCandidate<(QWord(1) shl 62)-lWin) then
    Result:=lCandidate+lWin
  else if (lCandidate>lExpected+lHalf) and (lCandidate>=lWin) then
    Result:=lCandidate-lWin
  else
    Result:=lCandidate;
end;


// Writes the low aLength bytes of aPn in network order.
procedure WritePacketNumber(var aWriter: TQuicWriter; aPn: QWord; aLength: Integer);

var
  I: Integer;

begin
  for I:=aLength-1 downto 0 do
    aWriter.WriteByte((aPn shr (I*8)) and $FF);
end;


function QuicWriteLongHeader(var aWriter: TQuicWriter; aType: TQuicPacketType; const aDestCid, aSrcCid,
  aToken: TBytes; aPn: QWord; aPnLength, aPayloadLength: Integer): Integer;

begin
  aWriter.WriteByte($C0 or LongTypeBits[aType] or (aPnLength-1));
  aWriter.WriteUInt32(QUIC_VERSION_1);
  aWriter.WriteByte(Length(aDestCid));
  aWriter.WriteBytes(aDestCid);
  aWriter.WriteByte(Length(aSrcCid));
  aWriter.WriteBytes(aSrcCid);
  if aType=qptInitial then
    aWriter.WriteVarBytes(aToken);
  aWriter.WriteVarIntSized(aPnLength+aPayloadLength,2);
  Result:=aWriter.Length;
  WritePacketNumber(aWriter,aPn,aPnLength);
end;


function QuicWriteShortHeader(var aWriter: TQuicWriter; const aDestCid: TBytes; aKeyPhase: Boolean;
  aPn: QWord; aPnLength: Integer): Integer;

var
  lFirst: Byte;

begin
  lFirst:=$40 or (aPnLength-1);
  if aKeyPhase then
    lFirst:=lFirst or $04;
  aWriter.WriteByte(lFirst);
  aWriter.WriteBytes(aDestCid);
  Result:=aWriter.Length;
  WritePacketNumber(aWriter,aPn,aPnLength);
end;


function QuicBuildVersionNegotiation(const aDestCid, aSrcCid: TBytes; const aVersions: array of Cardinal): TBytes;

var
  lWriter: TQuicWriter;
  lVersion: Cardinal;

begin
  lWriter.Init;
  lWriter.WriteByte($C0);
  lWriter.WriteUInt32(0);
  lWriter.WriteByte(Length(aDestCid));
  lWriter.WriteBytes(aDestCid);
  lWriter.WriteByte(Length(aSrcCid));
  lWriter.WriteBytes(aSrcCid);
  for lVersion in aVersions do
    lWriter.WriteUInt32(lVersion);
  Result:=lWriter.ToBytes;
end;


function QuicBuildRetry(const aDestCid, aSrcCid, aToken: TBytes): TBytes;

var
  lWriter: TQuicWriter;

begin
  lWriter.Init;
  lWriter.WriteByte($C0 or LongTypeBits[qptRetry]);
  lWriter.WriteUInt32(QUIC_VERSION_1);
  lWriter.WriteByte(Length(aDestCid));
  lWriter.WriteBytes(aDestCid);
  lWriter.WriteByte(Length(aSrcCid));
  lWriter.WriteBytes(aSrcCid);
  lWriter.WriteBytes(aToken);
  Result:=lWriter.ToBytes;
end;


end.
