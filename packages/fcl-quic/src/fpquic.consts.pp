{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    QUIC (RFC 9000) protocol constants, shared enumerations and the QUIC exception class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpQuic.Consts;

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

const
  QUIC_VERSION_1 = $00000001;
  QUIC_MAX_VARINT = QWord($3FFFFFFFFFFFFFFF);
  QUIC_MAX_CID_LENGTH = 20;
  QUIC_LOCAL_CID_LENGTH = 8;
  QUIC_MIN_INITIAL_DATAGRAM = 1200;
  QUIC_MAX_DATAGRAM = 1200;
  QUIC_AEAD_TAG_LENGTH = 16;
  QUIC_HP_SAMPLE_LENGTH = 16;
  QUIC_RESET_TOKEN_LENGTH = 16;
  QUIC_MAX_STREAMS_LIMIT = QWord(1) shl 60;

  { Frame types (RFC 9000 section 19) }
  QUIC_FRAME_PADDING = $00;
  QUIC_FRAME_PING = $01;
  QUIC_FRAME_ACK = $02;
  QUIC_FRAME_ACK_ECN = $03;
  QUIC_FRAME_RESET_STREAM = $04;
  QUIC_FRAME_STOP_SENDING = $05;
  QUIC_FRAME_CRYPTO = $06;
  QUIC_FRAME_NEW_TOKEN = $07;
  QUIC_FRAME_STREAM = $08;
  QUIC_FRAME_STREAM_LAST = $0F;
  QUIC_FRAME_MAX_DATA = $10;
  QUIC_FRAME_MAX_STREAM_DATA = $11;
  QUIC_FRAME_MAX_STREAMS_BIDI = $12;
  QUIC_FRAME_MAX_STREAMS_UNI = $13;
  QUIC_FRAME_DATA_BLOCKED = $14;
  QUIC_FRAME_STREAM_DATA_BLOCKED = $15;
  QUIC_FRAME_STREAMS_BLOCKED_BIDI = $16;
  QUIC_FRAME_STREAMS_BLOCKED_UNI = $17;
  QUIC_FRAME_NEW_CONNECTION_ID = $18;
  QUIC_FRAME_RETIRE_CONNECTION_ID = $19;
  QUIC_FRAME_PATH_CHALLENGE = $1A;
  QUIC_FRAME_PATH_RESPONSE = $1B;
  QUIC_FRAME_CONNECTION_CLOSE = $1C;
  QUIC_FRAME_CONNECTION_CLOSE_APP = $1D;
  QUIC_FRAME_HANDSHAKE_DONE = $1E;

  { STREAM frame type bits }
  QUIC_STREAM_BIT_FIN = $01;
  QUIC_STREAM_BIT_LEN = $02;
  QUIC_STREAM_BIT_OFF = $04;

  { Transport error codes (RFC 9000 section 20.1) }
  QUIC_NO_ERROR = $00;
  QUIC_INTERNAL_ERROR = $01;
  QUIC_CONNECTION_REFUSED = $02;
  QUIC_FLOW_CONTROL_ERROR = $03;
  QUIC_STREAM_LIMIT_ERROR = $04;
  QUIC_STREAM_STATE_ERROR = $05;
  QUIC_FINAL_SIZE_ERROR = $06;
  QUIC_FRAME_ENCODING_ERROR = $07;
  QUIC_TRANSPORT_PARAMETER_ERROR = $08;
  QUIC_CONNECTION_ID_LIMIT_ERROR = $09;
  QUIC_PROTOCOL_VIOLATION = $0A;
  QUIC_INVALID_TOKEN = $0B;
  QUIC_APPLICATION_ERROR = $0C;
  QUIC_CRYPTO_BUFFER_EXCEEDED = $0D;
  QUIC_KEY_UPDATE_ERROR = $0E;
  QUIC_AEAD_LIMIT_REACHED = $0F;
  QUIC_NO_VIABLE_PATH = $10;
  QUIC_CRYPTO_ERROR = $100;

  { Transport parameter identifiers (RFC 9000 section 18.2) }
  QUIC_TP_ORIGINAL_DESTINATION_CONNECTION_ID = $00;
  QUIC_TP_MAX_IDLE_TIMEOUT = $01;
  QUIC_TP_STATELESS_RESET_TOKEN = $02;
  QUIC_TP_MAX_UDP_PAYLOAD_SIZE = $03;
  QUIC_TP_INITIAL_MAX_DATA = $04;
  QUIC_TP_INITIAL_MAX_STREAM_DATA_BIDI_LOCAL = $05;
  QUIC_TP_INITIAL_MAX_STREAM_DATA_BIDI_REMOTE = $06;
  QUIC_TP_INITIAL_MAX_STREAM_DATA_UNI = $07;
  QUIC_TP_INITIAL_MAX_STREAMS_BIDI = $08;
  QUIC_TP_INITIAL_MAX_STREAMS_UNI = $09;
  QUIC_TP_ACK_DELAY_EXPONENT = $0A;
  QUIC_TP_MAX_ACK_DELAY = $0B;
  QUIC_TP_DISABLE_ACTIVE_MIGRATION = $0C;
  QUIC_TP_PREFERRED_ADDRESS = $0D;
  QUIC_TP_ACTIVE_CONNECTION_ID_LIMIT = $0E;
  QUIC_TP_INITIAL_SOURCE_CONNECTION_ID = $0F;
  QUIC_TP_RETRY_SOURCE_CONNECTION_ID = $10;

type
  // Encryption level of a packet; each level except 0-RTT has its own packet number space.
  TQuicEncryptionLevel = (qelInitial, qelHandshake, qelApplication);

  // TLS 1.3 cipher suites usable for QUIC packet protection.
  TQuicCipherSuite = (qcsAes128Gcm, qcsAes256Gcm, qcsChaCha20Poly1305);

  // Error raised for protocol violations; ErrorCode is the QUIC transport error code to send.
  EQuicError = class(Exception)
  private
    FErrorCode: QWord;
    FFrameType: QWord;
  public
    // Creates the exception with a transport error code and the offending frame type.
    constructor CreateCode(aErrorCode: QWord; const aMessage: String; aFrameType: QWord = 0);
    // Transport error code to report to the peer.
    property ErrorCode: QWord read FErrorCode;
    // Frame type that triggered the error, 0 when unknown.
    property FrameType: QWord read FFrameType;
  end;

implementation

constructor EQuicError.CreateCode(aErrorCode: QWord; const aMessage: String; aFrameType: QWord);

begin
  inherited Create(aMessage);
  FErrorCode:=aErrorCode;
  FFrameType:=aFrameType;
end;


end.
