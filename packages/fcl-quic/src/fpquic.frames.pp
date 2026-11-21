{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    QUIC frame encoding and decoding (RFC 9000 section 19).

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpQuic.Frames;

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
  // One decoded or to-be-encoded frame; only the fields of FrameType are meaningful.
  TQuicFrame = record
    FrameType: QWord;
    // ACK: ranges in descending order, delay in encoded units, ECN counts for ACK_ECN.
    AckRanges: TQuicRangeArray;
    AckDelay: QWord;
    Ect0, Ect1, EcnCe: QWord;
    // STREAM, RESET_STREAM, STOP_SENDING, MAX_STREAM_DATA, STREAM_DATA_BLOCKED.
    StreamId: QWord;
    // STREAM and CRYPTO offset.
    Offset: QWord;
    // STREAM FIN bit.
    Fin: Boolean;
    // STREAM and CRYPTO data, NEW_TOKEN token, PATH_CHALLENGE/RESPONSE data, CONNECTION_CLOSE reason.
    Data: TBytes;
    // RESET_STREAM, STOP_SENDING and CONNECTION_CLOSE error code.
    ErrorCode: QWord;
    // RESET_STREAM final size; MAX_DATA, MAX_STREAM_DATA, MAX_STREAMS and *_BLOCKED limit.
    Value: QWord;
    // NEW_CONNECTION_ID and RETIRE_CONNECTION_ID sequence number.
    SequenceNumber: QWord;
    RetirePriorTo: QWord;
    ConnectionId: TBytes;
    ResetToken: TBytes;
    // CONNECTION_CLOSE (transport) offending frame type.
    CloseFrameType: QWord;
  end;
  TQuicFrameArray = array of TQuicFrame;

// Appends the encoding of aFrame to aWriter. STREAM frames always contain an explicit length.
procedure QuicEncodeFrame(var aWriter: TQuicWriter; const aFrame: TQuicFrame);

// Decodes one frame; consecutive PADDING bytes are returned as a single PADDING frame.
function QuicDecodeFrame(var aReader: TQuicReader): TQuicFrame;

// Size in bytes of the encoding of aFrame.
function QuicFrameSize(const aFrame: TQuicFrame): Integer;

// True when a frame of type aFrameType may appear in a packet of level aLevel.
function QuicFrameAllowed(aFrameType: QWord; aLevel: TQuicEncryptionLevel): Boolean;

// True when a frame of type aFrameType makes a packet ack-eliciting.
function QuicFrameAckEliciting(aFrameType: QWord): Boolean;

// Returns an empty frame of type aFrameType.
function QuicFrame(aFrameType: QWord): TQuicFrame;

implementation

function QuicFrame(aFrameType: QWord): TQuicFrame;

begin
  Result:=Default(TQuicFrame);
  Result.FrameType:=aFrameType;
end;


function QuicFrameAllowed(aFrameType: QWord; aLevel: TQuicEncryptionLevel): Boolean;

begin
  Result:=(aLevel=qelApplication)
          or ((aFrameType<256) and (Byte(aFrameType) in [QUIC_FRAME_PADDING,QUIC_FRAME_PING,QUIC_FRAME_ACK,QUIC_FRAME_ACK_ECN,
                             QUIC_FRAME_CRYPTO,QUIC_FRAME_CONNECTION_CLOSE]));
end;


function QuicFrameAckEliciting(aFrameType: QWord): Boolean;

begin
  Result:=(aFrameType>=256) or not (Byte(aFrameType) in [QUIC_FRAME_PADDING,QUIC_FRAME_ACK,QUIC_FRAME_ACK_ECN,
                              QUIC_FRAME_CONNECTION_CLOSE,QUIC_FRAME_CONNECTION_CLOSE_APP]);
end;


// Writes the ACK frame body: largest, delay, range count, first range and gap/length pairs.
procedure EncodeAck(var aWriter: TQuicWriter; const aFrame: TQuicFrame);

var
  I: Integer;

begin
  aWriter.WriteVarInt(aFrame.AckRanges[0].High);
  aWriter.WriteVarInt(aFrame.AckDelay);
  aWriter.WriteVarInt(Length(aFrame.AckRanges)-1);
  aWriter.WriteVarInt(aFrame.AckRanges[0].High-aFrame.AckRanges[0].Low);
  for I:=1 to High(aFrame.AckRanges) do
    begin
    aWriter.WriteVarInt(aFrame.AckRanges[I-1].Low-aFrame.AckRanges[I].High-2);
    aWriter.WriteVarInt(aFrame.AckRanges[I].High-aFrame.AckRanges[I].Low);
    end;
  if aFrame.FrameType=QUIC_FRAME_ACK_ECN then
    begin
    aWriter.WriteVarInt(aFrame.Ect0);
    aWriter.WriteVarInt(aFrame.Ect1);
    aWriter.WriteVarInt(aFrame.EcnCe);
    end;
end;


procedure QuicEncodeFrame(var aWriter: TQuicWriter; const aFrame: TQuicFrame);

var
  lType: QWord;

begin
  lType:=aFrame.FrameType;
  if (lType>=QUIC_FRAME_STREAM) and (lType<=QUIC_FRAME_STREAM_LAST) then
    begin
    lType:=QUIC_FRAME_STREAM or QUIC_STREAM_BIT_LEN;
    if aFrame.Offset>0 then
      lType:=lType or QUIC_STREAM_BIT_OFF;
    if aFrame.Fin then
      lType:=lType or QUIC_STREAM_BIT_FIN;
    end;
  aWriter.WriteVarInt(lType);
  case aFrame.FrameType of
    QUIC_FRAME_PADDING,
    QUIC_FRAME_PING,
    QUIC_FRAME_HANDSHAKE_DONE:
      ;
    QUIC_FRAME_ACK,
    QUIC_FRAME_ACK_ECN:
      EncodeAck(aWriter,aFrame);
    QUIC_FRAME_RESET_STREAM:
      begin
      aWriter.WriteVarInt(aFrame.StreamId);
      aWriter.WriteVarInt(aFrame.ErrorCode);
      aWriter.WriteVarInt(aFrame.Value);
      end;
    QUIC_FRAME_STOP_SENDING:
      begin
      aWriter.WriteVarInt(aFrame.StreamId);
      aWriter.WriteVarInt(aFrame.ErrorCode);
      end;
    QUIC_FRAME_CRYPTO:
      begin
      aWriter.WriteVarInt(aFrame.Offset);
      aWriter.WriteVarBytes(aFrame.Data);
      end;
    QUIC_FRAME_NEW_TOKEN:
      aWriter.WriteVarBytes(aFrame.Data);
    QUIC_FRAME_STREAM..QUIC_FRAME_STREAM_LAST:
      begin
      aWriter.WriteVarInt(aFrame.StreamId);
      if aFrame.Offset>0 then
        aWriter.WriteVarInt(aFrame.Offset);
      aWriter.WriteVarBytes(aFrame.Data);
      end;
    QUIC_FRAME_MAX_DATA,
    QUIC_FRAME_MAX_STREAMS_BIDI,
    QUIC_FRAME_MAX_STREAMS_UNI,
    QUIC_FRAME_DATA_BLOCKED,
    QUIC_FRAME_STREAMS_BLOCKED_BIDI,
    QUIC_FRAME_STREAMS_BLOCKED_UNI:
      aWriter.WriteVarInt(aFrame.Value);
    QUIC_FRAME_MAX_STREAM_DATA,
    QUIC_FRAME_STREAM_DATA_BLOCKED:
      begin
      aWriter.WriteVarInt(aFrame.StreamId);
      aWriter.WriteVarInt(aFrame.Value);
      end;
    QUIC_FRAME_NEW_CONNECTION_ID:
      begin
      aWriter.WriteVarInt(aFrame.SequenceNumber);
      aWriter.WriteVarInt(aFrame.RetirePriorTo);
      aWriter.WriteByte(Length(aFrame.ConnectionId));
      aWriter.WriteBytes(aFrame.ConnectionId);
      aWriter.WriteBytes(aFrame.ResetToken);
      end;
    QUIC_FRAME_RETIRE_CONNECTION_ID:
      aWriter.WriteVarInt(aFrame.SequenceNumber);
    QUIC_FRAME_PATH_CHALLENGE,
    QUIC_FRAME_PATH_RESPONSE:
      aWriter.WriteBytes(aFrame.Data);
    QUIC_FRAME_CONNECTION_CLOSE,
    QUIC_FRAME_CONNECTION_CLOSE_APP:
      begin
      aWriter.WriteVarInt(aFrame.ErrorCode);
      if aFrame.FrameType=QUIC_FRAME_CONNECTION_CLOSE then
        aWriter.WriteVarInt(aFrame.CloseFrameType);
      aWriter.WriteVarBytes(aFrame.Data);
      end;
  else
    raise EQuicError.CreateCode(QUIC_INTERNAL_ERROR,'Cannot encode frame type '+IntToStr(aFrame.FrameType));
  end;
end;


function QuicFrameSize(const aFrame: TQuicFrame): Integer;

var
  lWriter: TQuicWriter;

begin
  lWriter.Init;
  QuicEncodeFrame(lWriter,aFrame);
  Result:=lWriter.Length;
end;


// Raises FRAME_ENCODING_ERROR for frame aType when aCondition holds.
procedure Check(aCondition: Boolean; aType: QWord; const aMessage: String);

begin
  if aCondition then
    raise EQuicError.CreateCode(QUIC_FRAME_ENCODING_ERROR,aMessage,aType);
end;


// Reads the ACK frame body into aFrame.
procedure DecodeAck(var aReader: TQuicReader; var aFrame: TQuicFrame);

var
  lLargest, lCount, lLength, lGap, lLow: QWord;
  I: Integer;

begin
  lLargest:=aReader.ReadVarInt;
  aFrame.AckDelay:=aReader.ReadVarInt;
  lCount:=aReader.ReadVarInt;
  lLength:=aReader.ReadVarInt;
  Check(lLength>lLargest,aFrame.FrameType,'ACK range below zero');
  Check(lCount>QWord(aReader.Remaining),aFrame.FrameType,'ACK range count too large');
  SetLength(aFrame.AckRanges,lCount+1);
  aFrame.AckRanges[0].High:=lLargest;
  aFrame.AckRanges[0].Low:=lLargest-lLength;
  lLow:=lLargest-lLength;
  for I:=1 to lCount do
    begin
    lGap:=aReader.ReadVarInt;
    lLength:=aReader.ReadVarInt;
    Check(lGap+2>lLow,aFrame.FrameType,'ACK gap below zero');
    aFrame.AckRanges[I].High:=lLow-lGap-2;
    Check(lLength>aFrame.AckRanges[I].High,aFrame.FrameType,'ACK range below zero');
    aFrame.AckRanges[I].Low:=aFrame.AckRanges[I].High-lLength;
    lLow:=aFrame.AckRanges[I].Low;
    end;
  if aFrame.FrameType=QUIC_FRAME_ACK_ECN then
    begin
    aFrame.Ect0:=aReader.ReadVarInt;
    aFrame.Ect1:=aReader.ReadVarInt;
    aFrame.EcnCe:=aReader.ReadVarInt;
    end;
end;


function QuicDecodeFrame(var aReader: TQuicReader): TQuicFrame;

var
  lType, lLength: QWord;

begin
  Result:=Default(TQuicFrame);
  lType:=aReader.ReadVarInt;
  Result.FrameType:=lType;
  case lType of
    QUIC_FRAME_PADDING:
      while (not aReader.EOF) and (aReader.PeekByte=0) do
        aReader.Skip(1);
    QUIC_FRAME_PING,
    QUIC_FRAME_HANDSHAKE_DONE:
      ;
    QUIC_FRAME_ACK,
    QUIC_FRAME_ACK_ECN:
      DecodeAck(aReader,Result);
    QUIC_FRAME_RESET_STREAM:
      begin
      Result.StreamId:=aReader.ReadVarInt;
      Result.ErrorCode:=aReader.ReadVarInt;
      Result.Value:=aReader.ReadVarInt;
      end;
    QUIC_FRAME_STOP_SENDING:
      begin
      Result.StreamId:=aReader.ReadVarInt;
      Result.ErrorCode:=aReader.ReadVarInt;
      end;
    QUIC_FRAME_CRYPTO:
      begin
      Result.Offset:=aReader.ReadVarInt;
      Result.Data:=aReader.ReadVarBytes;
      Check(Result.Offset+Length(Result.Data)>QUIC_MAX_VARINT,lType,'CRYPTO data beyond maximum offset');
      end;
    QUIC_FRAME_NEW_TOKEN:
      begin
      Result.Data:=aReader.ReadVarBytes;
      Check(Length(Result.Data)=0,lType,'Empty NEW_TOKEN');
      end;
    QUIC_FRAME_STREAM..QUIC_FRAME_STREAM_LAST:
      begin
      Result.FrameType:=QUIC_FRAME_STREAM;
      Result.StreamId:=aReader.ReadVarInt;
      if (lType and QUIC_STREAM_BIT_OFF)<>0 then
        Result.Offset:=aReader.ReadVarInt;
      if (lType and QUIC_STREAM_BIT_LEN)<>0 then
        lLength:=aReader.ReadVarInt
      else
        lLength:=aReader.Remaining;
      Result.Data:=aReader.ReadBytes(lLength);
      Result.Fin:=(lType and QUIC_STREAM_BIT_FIN)<>0;
      Check(Result.Offset+lLength>QUIC_MAX_VARINT,lType,'STREAM data beyond maximum offset');
      end;
    QUIC_FRAME_MAX_DATA,
    QUIC_FRAME_DATA_BLOCKED:
      Result.Value:=aReader.ReadVarInt;
    QUIC_FRAME_MAX_STREAMS_BIDI,
    QUIC_FRAME_MAX_STREAMS_UNI,
    QUIC_FRAME_STREAMS_BLOCKED_BIDI,
    QUIC_FRAME_STREAMS_BLOCKED_UNI:
      begin
      Result.Value:=aReader.ReadVarInt;
      Check(Result.Value>QUIC_MAX_STREAMS_LIMIT,lType,'Stream count above 2^60');
      end;
    QUIC_FRAME_MAX_STREAM_DATA,
    QUIC_FRAME_STREAM_DATA_BLOCKED:
      begin
      Result.StreamId:=aReader.ReadVarInt;
      Result.Value:=aReader.ReadVarInt;
      end;
    QUIC_FRAME_NEW_CONNECTION_ID:
      begin
      Result.SequenceNumber:=aReader.ReadVarInt;
      Result.RetirePriorTo:=aReader.ReadVarInt;
      lLength:=aReader.ReadByte;
      Check((lLength<1) or (lLength>QUIC_MAX_CID_LENGTH),lType,'Invalid connection ID length');
      Check(Result.RetirePriorTo>Result.SequenceNumber,lType,'Retire Prior To above Sequence Number');
      Result.ConnectionId:=aReader.ReadBytes(lLength);
      Result.ResetToken:=aReader.ReadBytes(QUIC_RESET_TOKEN_LENGTH);
      end;
    QUIC_FRAME_RETIRE_CONNECTION_ID:
      Result.SequenceNumber:=aReader.ReadVarInt;
    QUIC_FRAME_PATH_CHALLENGE,
    QUIC_FRAME_PATH_RESPONSE:
      Result.Data:=aReader.ReadBytes(8);
    QUIC_FRAME_CONNECTION_CLOSE,
    QUIC_FRAME_CONNECTION_CLOSE_APP:
      begin
      Result.ErrorCode:=aReader.ReadVarInt;
      if lType=QUIC_FRAME_CONNECTION_CLOSE then
        Result.CloseFrameType:=aReader.ReadVarInt;
      Result.Data:=aReader.ReadVarBytes;
      end;
  else
    Check(True,lType,'Unknown frame type '+IntToStr(lType));
  end;
end;


end.
