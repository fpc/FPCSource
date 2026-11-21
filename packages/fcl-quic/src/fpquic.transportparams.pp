{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    QUIC transport parameters encoding, decoding and validation (RFC 9000 section 18).

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpQuic.TransportParams;

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
  // Transport parameters of one endpoint; Has* flags tell whether optional values are present.
  TQuicTransportParams = record
    HasOriginalDestinationCid: Boolean;
    OriginalDestinationCid: TBytes;
    MaxIdleTimeout: QWord;
    HasStatelessResetToken: Boolean;
    StatelessResetToken: TBytes;
    MaxUdpPayloadSize: QWord;
    InitialMaxData: QWord;
    InitialMaxStreamDataBidiLocal: QWord;
    InitialMaxStreamDataBidiRemote: QWord;
    InitialMaxStreamDataUni: QWord;
    InitialMaxStreamsBidi: QWord;
    InitialMaxStreamsUni: QWord;
    AckDelayExponent: QWord;
    MaxAckDelay: QWord;
    DisableActiveMigration: Boolean;
    HasPreferredAddress: Boolean;
    PreferredAddress: TBytes;
    ActiveConnectionIdLimit: QWord;
    HasInitialSourceCid: Boolean;
    InitialSourceCid: TBytes;
    HasRetrySourceCid: Boolean;
    RetrySourceCid: TBytes;
  end;

// Returns the parameters with the default values of RFC 9000 for absent parameters.
function QuicDefaultTransportParams: TQuicTransportParams;

// Encodes aParams; absent optional values and default-valued parameters are omitted.
function QuicEncodeTransportParams(const aParams: TQuicTransportParams): TBytes;

// Decodes and validates parameters sent by a server (aFromServer) or a client.
// Raises EQuicError(TRANSPORT_PARAMETER_ERROR) for invalid input.
function QuicDecodeTransportParams(const aData: TBytes; aFromServer: Boolean): TQuicTransportParams;

// Default parameters with the limits a client remembers for 0-RTT (RFC 9000 section 7.4.1) taken from aParams.
function QuicRememberedParams(const aParams: TQuicTransportParams): TQuicTransportParams;

// True when aCurrent lowers none of the limits remembered in aRemembered.
function QuicEarlyParamsCompatible(const aRemembered, aCurrent: TQuicTransportParams): Boolean;

implementation

function QuicDefaultTransportParams: TQuicTransportParams;

begin
  Result:=Default(TQuicTransportParams);
  Result.MaxUdpPayloadSize:=65527;
  Result.AckDelayExponent:=3;
  Result.MaxAckDelay:=25;
  Result.ActiveConnectionIdLimit:=2;
end;


// Writes a parameter whose value is a variable-length integer, unless it equals aDefault.
procedure WriteIntParam(var aWriter: TQuicWriter; aId, aValue, aDefault: QWord);

begin
  if aValue=aDefault then
    Exit;
  aWriter.WriteVarInt(aId);
  aWriter.WriteVarInt(QuicVarIntSize(aValue));
  aWriter.WriteVarInt(aValue);
end;


// Writes a parameter with a byte string value when aPresent.
procedure WriteBytesParam(var aWriter: TQuicWriter; aId: QWord; aPresent: Boolean; const aValue: TBytes);

begin
  if not aPresent then
    Exit;
  aWriter.WriteVarInt(aId);
  aWriter.WriteVarBytes(aValue);
end;


function QuicEncodeTransportParams(const aParams: TQuicTransportParams): TBytes;

var
  lWriter: TQuicWriter;

begin
  lWriter.Init;
  with aParams do
    begin
    WriteBytesParam(lWriter,QUIC_TP_ORIGINAL_DESTINATION_CONNECTION_ID,HasOriginalDestinationCid,OriginalDestinationCid);
    WriteIntParam(lWriter,QUIC_TP_MAX_IDLE_TIMEOUT,MaxIdleTimeout,0);
    WriteBytesParam(lWriter,QUIC_TP_STATELESS_RESET_TOKEN,HasStatelessResetToken,StatelessResetToken);
    WriteIntParam(lWriter,QUIC_TP_MAX_UDP_PAYLOAD_SIZE,MaxUdpPayloadSize,65527);
    WriteIntParam(lWriter,QUIC_TP_INITIAL_MAX_DATA,InitialMaxData,0);
    WriteIntParam(lWriter,QUIC_TP_INITIAL_MAX_STREAM_DATA_BIDI_LOCAL,InitialMaxStreamDataBidiLocal,0);
    WriteIntParam(lWriter,QUIC_TP_INITIAL_MAX_STREAM_DATA_BIDI_REMOTE,InitialMaxStreamDataBidiRemote,0);
    WriteIntParam(lWriter,QUIC_TP_INITIAL_MAX_STREAM_DATA_UNI,InitialMaxStreamDataUni,0);
    WriteIntParam(lWriter,QUIC_TP_INITIAL_MAX_STREAMS_BIDI,InitialMaxStreamsBidi,0);
    WriteIntParam(lWriter,QUIC_TP_INITIAL_MAX_STREAMS_UNI,InitialMaxStreamsUni,0);
    WriteIntParam(lWriter,QUIC_TP_ACK_DELAY_EXPONENT,AckDelayExponent,3);
    WriteIntParam(lWriter,QUIC_TP_MAX_ACK_DELAY,MaxAckDelay,25);
    WriteBytesParam(lWriter,QUIC_TP_DISABLE_ACTIVE_MIGRATION,DisableActiveMigration,nil);
    WriteBytesParam(lWriter,QUIC_TP_PREFERRED_ADDRESS,HasPreferredAddress,PreferredAddress);
    WriteIntParam(lWriter,QUIC_TP_ACTIVE_CONNECTION_ID_LIMIT,ActiveConnectionIdLimit,2);
    WriteBytesParam(lWriter,QUIC_TP_INITIAL_SOURCE_CONNECTION_ID,HasInitialSourceCid,InitialSourceCid);
    WriteBytesParam(lWriter,QUIC_TP_RETRY_SOURCE_CONNECTION_ID,HasRetrySourceCid,RetrySourceCid);
    end;
  Result:=lWriter.ToBytes;
end;


// Raises TRANSPORT_PARAMETER_ERROR when aCondition holds.
procedure Check(aCondition: Boolean; const aMessage: String);

begin
  if aCondition then
    raise EQuicError.CreateCode(QUIC_TRANSPORT_PARAMETER_ERROR,aMessage);
end;


// Decodes a parameter value that must hold exactly one variable-length integer.
function IntValue(const aValue: TBytes): QWord;

var
  lReader: TQuicReader;

begin
  Result:=0;
  lReader.InitBytes(aValue);
  try
    Result:=lReader.ReadVarInt;
  except
    on EQuicError do
      Check(True,'Malformed integer transport parameter');
  end;
  Check(not lReader.EOF,'Malformed integer transport parameter');
end;


// Stores one decoded parameter in aParams.
procedure ApplyParam(var aParams: TQuicTransportParams; aId: QWord; const aValue: TBytes; aFromServer: Boolean);

begin
  Check((not aFromServer) and (aId<256) and (Byte(aId) in [QUIC_TP_ORIGINAL_DESTINATION_CONNECTION_ID,
        QUIC_TP_STATELESS_RESET_TOKEN,QUIC_TP_PREFERRED_ADDRESS,QUIC_TP_RETRY_SOURCE_CONNECTION_ID]),
        'Server-only transport parameter from client');
  case aId of
    QUIC_TP_ORIGINAL_DESTINATION_CONNECTION_ID:
      begin
      aParams.HasOriginalDestinationCid:=True;
      aParams.OriginalDestinationCid:=aValue;
      end;
    QUIC_TP_MAX_IDLE_TIMEOUT:
      aParams.MaxIdleTimeout:=IntValue(aValue);
    QUIC_TP_STATELESS_RESET_TOKEN:
      begin
      Check(Length(aValue)<>QUIC_RESET_TOKEN_LENGTH,'Invalid stateless reset token length');
      aParams.HasStatelessResetToken:=True;
      aParams.StatelessResetToken:=aValue;
      end;
    QUIC_TP_MAX_UDP_PAYLOAD_SIZE:
      begin
      aParams.MaxUdpPayloadSize:=IntValue(aValue);
      Check(aParams.MaxUdpPayloadSize<1200,'max_udp_payload_size below 1200');
      end;
    QUIC_TP_INITIAL_MAX_DATA:
      aParams.InitialMaxData:=IntValue(aValue);
    QUIC_TP_INITIAL_MAX_STREAM_DATA_BIDI_LOCAL:
      aParams.InitialMaxStreamDataBidiLocal:=IntValue(aValue);
    QUIC_TP_INITIAL_MAX_STREAM_DATA_BIDI_REMOTE:
      aParams.InitialMaxStreamDataBidiRemote:=IntValue(aValue);
    QUIC_TP_INITIAL_MAX_STREAM_DATA_UNI:
      aParams.InitialMaxStreamDataUni:=IntValue(aValue);
    QUIC_TP_INITIAL_MAX_STREAMS_BIDI:
      begin
      aParams.InitialMaxStreamsBidi:=IntValue(aValue);
      Check(aParams.InitialMaxStreamsBidi>QUIC_MAX_STREAMS_LIMIT,'initial_max_streams_bidi above 2^60');
      end;
    QUIC_TP_INITIAL_MAX_STREAMS_UNI:
      begin
      aParams.InitialMaxStreamsUni:=IntValue(aValue);
      Check(aParams.InitialMaxStreamsUni>QUIC_MAX_STREAMS_LIMIT,'initial_max_streams_uni above 2^60');
      end;
    QUIC_TP_ACK_DELAY_EXPONENT:
      begin
      aParams.AckDelayExponent:=IntValue(aValue);
      Check(aParams.AckDelayExponent>20,'ack_delay_exponent above 20');
      end;
    QUIC_TP_MAX_ACK_DELAY:
      begin
      aParams.MaxAckDelay:=IntValue(aValue);
      Check(aParams.MaxAckDelay>=16384,'max_ack_delay of 2^14 or more');
      end;
    QUIC_TP_DISABLE_ACTIVE_MIGRATION:
      begin
      Check(Length(aValue)<>0,'disable_active_migration with a value');
      aParams.DisableActiveMigration:=True;
      end;
    QUIC_TP_PREFERRED_ADDRESS:
      begin
      aParams.HasPreferredAddress:=True;
      aParams.PreferredAddress:=aValue;
      end;
    QUIC_TP_ACTIVE_CONNECTION_ID_LIMIT:
      begin
      aParams.ActiveConnectionIdLimit:=IntValue(aValue);
      Check(aParams.ActiveConnectionIdLimit<2,'active_connection_id_limit below 2');
      end;
    QUIC_TP_INITIAL_SOURCE_CONNECTION_ID:
      begin
      aParams.HasInitialSourceCid:=True;
      aParams.InitialSourceCid:=aValue;
      end;
    QUIC_TP_RETRY_SOURCE_CONNECTION_ID:
      begin
      aParams.HasRetrySourceCid:=True;
      aParams.RetrySourceCid:=aValue;
      end;
  end;
end;


function QuicDecodeTransportParams(const aData: TBytes; aFromServer: Boolean): TQuicTransportParams;

var
  lReader: TQuicReader;
  lSeen: TQuicRangeSet;
  lId: QWord;
  lValue: TBytes;

begin
  Result:=QuicDefaultTransportParams;
  lReader.InitBytes(aData);
  lSeen:=TQuicRangeSet.Create;
  try
    while not lReader.EOF do
      begin
      try
        lId:=lReader.ReadVarInt;
        lValue:=lReader.ReadVarBytes;
      except
        on EQuicError do
          Check(True,'Truncated transport parameters');
      end;
      Check(lSeen.Contains(lId),'Duplicate transport parameter');
      lSeen.Add(lId,lId);
      ApplyParam(Result,lId,lValue,aFromServer);
      end;
  finally
    lSeen.Free;
  end;
end;


function QuicRememberedParams(const aParams: TQuicTransportParams): TQuicTransportParams;

begin
  Result:=QuicDefaultTransportParams;
  Result.ActiveConnectionIdLimit:=aParams.ActiveConnectionIdLimit;
  Result.InitialMaxData:=aParams.InitialMaxData;
  Result.InitialMaxStreamDataBidiLocal:=aParams.InitialMaxStreamDataBidiLocal;
  Result.InitialMaxStreamDataBidiRemote:=aParams.InitialMaxStreamDataBidiRemote;
  Result.InitialMaxStreamDataUni:=aParams.InitialMaxStreamDataUni;
  Result.InitialMaxStreamsBidi:=aParams.InitialMaxStreamsBidi;
  Result.InitialMaxStreamsUni:=aParams.InitialMaxStreamsUni;
end;


function QuicEarlyParamsCompatible(const aRemembered, aCurrent: TQuicTransportParams): Boolean;

begin
  Result:=(aCurrent.ActiveConnectionIdLimit>=aRemembered.ActiveConnectionIdLimit)
          and (aCurrent.InitialMaxData>=aRemembered.InitialMaxData)
          and (aCurrent.InitialMaxStreamDataBidiLocal>=aRemembered.InitialMaxStreamDataBidiLocal)
          and (aCurrent.InitialMaxStreamDataBidiRemote>=aRemembered.InitialMaxStreamDataBidiRemote)
          and (aCurrent.InitialMaxStreamDataUni>=aRemembered.InitialMaxStreamDataUni)
          and (aCurrent.InitialMaxStreamsBidi>=aRemembered.InitialMaxStreamsBidi)
          and (aCurrent.InitialMaxStreamsUni>=aRemembered.InitialMaxStreamsUni);
end;


end.
