{
    This file is part of the Free Component Library (Fcl)
    Copyright (c) 2021 - by the Free Pascal development team

    HTTP/2 protocol constants

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit fphttp2consts;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}{$H+}

interface

type
  // RFC 9113 §11.2 frame-type registry. Ordinals equal the wire byte (assign
  // every value explicitly; contiguous 0..9 keeps the enum dense/indexable).
  TH2FrameType = (
    ftData          = 0,
    ftHeaders       = 1,
    ftPriority      = 2,
    ftRSTStream     = 3,
    ftSettings      = 4,
    ftPushPromise   = 5,
    ftPing          = 6,
    ftGoAway        = 7,
    ftWindowUpdate  = 8,
    ftContinuation  = 9
  );

  // RFC 9113 §11.4 error-code registry. Contiguous 0..13 keeps the enum dense.
  TH2ErrorCode = (
    ecNoError            = 0,
    ecProtocolError      = 1,
    ecInternalError      = 2,
    ecFlowControlError   = 3,
    ecSettingsTimeout    = 4,
    ecStreamClosed       = 5,
    ecFrameSizeError     = 6,
    ecRefusedStream      = 7,
    ecCancel             = 8,
    ecCompressionError   = 9,
    ecConnectError       = 10,
    ecEnhanceYourCalm    = 11,
    ecInadequateSecurity = 12,
    ecHTTP11Required     = 13
  );

  // Tunable DoS limits, read once into each TH2Connection at construction.
  TH2Limits = record
    MaxConcurrentStreams: Cardinal;   // §5.1.2 - active client streams cap
    MaxFrameSize: Cardinal;           // §4.2   - negotiated max frame payload
    MaxHeaderListSize: Cardinal;      // §6.5.2 - decompressed header-list cap
    MaxContinuationBytes: Cardinal;   // CVE-2024-27316 - HEADERS+CONTINUATION accumulation cap
    HPACKTableLimit: Cardinal;        // RFC 7541 §6.3 - decoder dynamic-table bound
    MaxResetStreams: Cardinal;        // CVE-2023-44487 - rapid-reset (open-then-RST) budget between completed requests
    MaxPingFlood: Cardinal;           // §6.7 - non-ACK PINGs between completed requests
    MaxSettingsFlood: Cardinal;       // §6.5 - non-ACK SETTINGS between completed requests
  end;

const
  // Frame flags (RFC 9113 §6). END_STREAM and ACK deliberately share bit $01
  // this is correct per the RFC, not a copy-paste error.
  H2_FLAG_END_STREAM  : Byte = $01;
  H2_FLAG_ACK         : Byte = $01;
  H2_FLAG_END_HEADERS : Byte = $04;
  H2_FLAG_PADDED      : Byte = $08;
  H2_FLAG_PRIORITY    : Byte = $20;

  // SETTINGS parameter identifiers (RFC 9113 §11.3) - 16-bit on the wire.
  H2_SETTINGS_HEADER_TABLE_SIZE      : Word = $01;
  H2_SETTINGS_ENABLE_PUSH            : Word = $02;
  H2_SETTINGS_MAX_CONCURRENT_STREAMS : Word = $03;
  H2_SETTINGS_INITIAL_WINDOW_SIZE    : Word = $04;
  H2_SETTINGS_MAX_FRAME_SIZE         : Word = $05;
  H2_SETTINGS_MAX_HEADER_LIST_SIZE   : Word = $06;

  // RFC 9113 §3.4 - client connection preface, exactly 24 octets.
  // Byte-array form lets the codec compare seeded/received bytes against it as TBytes.
  H2_CLIENT_PREFACE: array[0..23] of Byte = (
    $50,$52,$49,$20,$2A,$20,$48,$54,$54,$50,$2F,$32,$2E,$30,  // 'PRI * HTTP/2.0'
    $0D,$0A,$0D,$0A,                                          //  CR LF CR LF
    $53,$4D,                                                  // 'SM'
    $0D,$0A,$0D,$0A);                                         //  CR LF CR LF

  // Default / fixed protocol values (imposed by RFC; NOT tunable DoS limits).
  H2_DEFAULT_INITIAL_WINDOW_SIZE = 65535;
  H2_DEFAULT_MAX_FRAME_SIZE      = 16384;
  H2_MAX_ALLOWED_FRAME_SIZE      = 16777215;
  H2_MAX_WINDOW_SIZE             = 2147483647;
  H2_DEFAULT_HEADER_TABLE_SIZE   = 4096;

  // ---- Tunable DoS-limit DEFAULTS (canonical safe defaults for TH2Limits) ----
  // 100 concurrent streams: matches Go net/http2 / nghttp2.
  H2_DEFAULT_MAX_CONCURRENT_STREAMS = 100;
  // Max frame size reuses H2_DEFAULT_MAX_FRAME_SIZE (the RFC 9113 §4.2 floor):
  H2_DEFAULT_MAX_HEADER_LIST_SIZE   = 32768;
  // 64 KiB CONTINUATION-accumulation: bounds the CVE-2024-27316 endless-CONTINUATION flood;
  H2_DEFAULT_MAX_CONTINUATION_BYTES = 65536;
  // HPACK table limit = RFC 7541 default dynamic-table size.
  H2_DEFAULT_HPACK_TABLE_LIMIT      = H2_DEFAULT_HEADER_TABLE_SIZE;   // 4096

  // ---- Tunable FLOOD / RAPID-RESET DEFAULTS ----
  // 100 in-flight resets: the open-then-RST flood (CVE-2023-44487) never completes a request, 
  H2_DEFAULT_MAX_RESET_STREAMS      = 100; 
  // 1000 non-ACK PINGs: PINGs are the cheapest control frame (8-byte echo).
  H2_DEFAULT_MAX_PING_FLOOD         = 1000;
  // 100 non-ACK SETTINGS: legitimate peers send SETTINGS rarely
  H2_DEFAULT_MAX_SETTINGS_FLOOD     = 100; 

  { RFC 9113 §8.3 pseudo-header field names - single source of truth for the
    ':'-prefixed strings. The four request pseudo-headers map onto TRequest fields;
     :status is the sole response pseudo-header.}
  H2_PSEUDO_METHOD    = ':method';
  H2_PSEUDO_PATH      = ':path';
  H2_PSEUDO_AUTHORITY = ':authority';
  H2_PSEUDO_SCHEME    = ':scheme';
  H2_PSEUDO_STATUS    = ':status';

  // RFC 7540 §3.2 - the token a cleartext client sends in `Upgrade: h2c`.
  H2C_UPGRADE_NAME    = 'h2c';

  // Request-body bound: the connection retains a stream's buffered DATA up to
  // this size; exceeding it is a stream-scoped REFUSED_STREAM. 8 MiB default.
  H2_DEFAULT_MAX_BODY_SIZE       = 8 * 1024 * 1024;   // 8 MiB

  // Single source of truth for the limit defaults: 
  DEFAULT_H2_LIMITS: TH2Limits = (
    MaxConcurrentStreams: H2_DEFAULT_MAX_CONCURRENT_STREAMS;
    MaxFrameSize:         H2_DEFAULT_MAX_FRAME_SIZE;
    MaxHeaderListSize:    H2_DEFAULT_MAX_HEADER_LIST_SIZE;
    MaxContinuationBytes: H2_DEFAULT_MAX_CONTINUATION_BYTES;
    HPACKTableLimit:      H2_DEFAULT_HPACK_TABLE_LIMIT;
    MaxResetStreams:      H2_DEFAULT_MAX_RESET_STREAMS;
    MaxPingFlood:         H2_DEFAULT_MAX_PING_FLOOD;
    MaxSettingsFlood:     H2_DEFAULT_MAX_SETTINGS_FLOOD
  );

implementation

end.
