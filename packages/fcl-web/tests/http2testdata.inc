{ HTTP/2 frame byte fixtures (RFC 9113) for tcfphttp2frames.

  Included into the test unit via {$i http2testdata.inc}. Each fixture is a
  hand-derived octet sequence with its decoded meaning in the comment so a
  reviewer can re-derive it from the RFC independently of the codec under test.

  Wire header layout (§4.1, 9 octets):
    Length(24) Type(8) Flags(8) R(1) StreamID(31)

  Type bytes: DATA=$00 HEADERS=$01 RST_STREAM=$03 SETTINGS=$04 PING=$06
              GOAWAY=$07 WINDOW_UPDATE=$08 CONTINUATION=$09
  Flag bits : END_STREAM/ACK=$01 END_HEADERS=$04 PADDED=$08 PRIORITY=$20 }

const
  // --- Well-formed frames -------------------------------------------------

  // Header with the reserved (R) bit set in the StreamID dword ($80000001).
  // Read must mask R off and yield StreamID = 1. Header-only (no payload).
  //   Len=0  Type=PING($06)  Flags=0  StreamID raw=$80000001
  H2F_RESERVED_BIT_HEADER: array[0..8] of Byte = (
    $00, $00, $00,            // Length = 0
    $06,                      // Type = PING
    $00,                      // Flags = 0
    $80, $00, $00, $01);      // R=1, StreamID = 1

  // Maximum representable frame length ($FFFFFF == H2_MAX_ALLOWED_FRAME_SIZE).
  // Boundary case: must be ACCEPTED (no payload read in the test). Header-only.
  //   Len=$FFFFFF  Type=DATA($00)  Flags=0  StreamID=1
  H2F_MAX_LEN_HEADER: array[0..8] of Byte = (
    $FF, $FF, $FF,            // Length = 16777215
    $00,                      // Type = DATA
    $00,                      // Flags = 0
    $00, $00, $00, $01);      // StreamID = 1

  // DATA frame, stream 1, application data "Hi" (= $48 $69), no padding.
  //   Len=2  Type=DATA  Flags=0  StreamID=1
  H2F_DATA: array[0..10] of Byte = (
    $00, $00, $02,            // Length = 2
    $00,                      // Type = DATA
    $00,                      // Flags = 0
    $00, $00, $00, $01,       // StreamID = 1
    $48, $69);                // "Hi"

  // Padded DATA frame, stream 1, data "Hi", Pad Length = 3.
  //   payload = padoctet(1) + data(2) + padding(3) = 6
  //   Len=6  Type=DATA  Flags=PADDED($08)  StreamID=1
  H2F_DATA_PADDED: array[0..14] of Byte = (
    $00, $00, $06,            // Length = 6
    $00,                      // Type = DATA
    $08,                      // Flags = PADDED
    $00, $00, $00, $01,       // StreamID = 1
    $03,                      // Pad Length = 3
    $48, $69,                 // "Hi"
    $00, $00, $00);           // 3 padding octets

  // Boundary case: Pad Length equals the bytes remaining after the pad-length
  // octet (3 == Len-1), i.e. ZERO application data, all-padding. Per RFC 9113
  // §6.1 this is VALID (the codec rejects only pad-length STRICTLY greater than
  // the remaining payload); reading it must succeed and yield an empty payload.
  //   payload = padoctet(1) + data(0) + padding(3) = 4
  //   Len=4  Type=DATA  Flags=PADDED($08)  StreamID=1
  H2F_DATA_FULL_PAD: array[0..12] of Byte = (
    $00, $00, $04,            // Length = 4
    $00,                      // Type = DATA
    $08,                      // Flags = PADDED
    $00, $00, $00, $01,       // StreamID = 1
    $03,                      // Pad Length = 3 (== remaining; data length 0)
    $00, $00, $00);           // 3 padding octets, no data

  // HEADERS frame, stream 1, END_HEADERS, header-block fragment $82 $84.
  //   Len=2  Type=HEADERS($01)  Flags=END_HEADERS($04)  StreamID=1
  H2F_HEADERS: array[0..10] of Byte = (
    $00, $00, $02,            // Length = 2
    $01,                      // Type = HEADERS
    $04,                      // Flags = END_HEADERS
    $00, $00, $00, $01,       // StreamID = 1
    $82, $84);                // header-block fragment

  // Padded + PRIORITY HEADERS, stream 1, END_HEADERS, fragment $82 $84.
  //   payload = padoctet(1) + priority(5) + fragment(2) + padding(2) = 10
  //   priority block: E+StreamDep = $0000000B (dep stream 11), Weight = $09
  //   Len=10  Type=HEADERS  Flags=END_HEADERS|PADDED|PRIORITY ($04|$08|$20=$2C)
  H2F_HEADERS_PAD_PRIO: array[0..18] of Byte = (
    $00, $00, $0A,            // Length = 10
    $01,                      // Type = HEADERS
    $2C,                      // Flags = END_HEADERS|PADDED|PRIORITY
    $00, $00, $00, $01,       // StreamID = 1
    $02,                      // Pad Length = 2
    $00, $00, $00, $0B,       // E=0, StreamDep = 11
    $09,                      // Weight = 9
    $82, $84,                 // header-block fragment
    $00, $00);                // 2 padding octets

  // HEADERS (no END_HEADERS) + CONTINUATION (END_HEADERS), both stream 1.
  // Assembled header block = $AA $BB $CC $DD.
  H2F_HEADERS_CONT: array[0..21] of Byte = (
    // HEADERS: Len=2 Type=HEADERS Flags=0 StreamID=1, fragment $AA $BB
    $00, $00, $02, $01, $00, $00, $00, $00, $01, $AA, $BB,
    // CONTINUATION: Len=2 Type=CONTINUATION($09) Flags=END_HEADERS($04) StreamID=1
    $00, $00, $02, $09, $04, $00, $00, $00, $01, $CC, $DD);

  // SETTINGS frame, 2 pairs: MAX_CONCURRENT_STREAMS($03)=100($64),
  //                          INITIAL_WINDOW_SIZE($04)=65535($FFFF).
  //   Len=12  Type=SETTINGS($04)  Flags=0  StreamID=0
  H2F_SETTINGS: array[0..20] of Byte = (
    $00, $00, $0C,            // Length = 12
    $04,                      // Type = SETTINGS
    $00,                      // Flags = 0
    $00, $00, $00, $00,       // StreamID = 0
    $00, $03, $00, $00, $00, $64,   // id=3, value=100
    $00, $04, $00, $00, $FF, $FF);  // id=4, value=65535

  // SETTINGS ACK: empty payload.
  //   Len=0  Type=SETTINGS  Flags=ACK($01)  StreamID=0
  H2F_SETTINGS_ACK: array[0..8] of Byte = (
    $00, $00, $00,            // Length = 0
    $04,                      // Type = SETTINGS
    $01,                      // Flags = ACK
    $00, $00, $00, $00);      // StreamID = 0

  // WINDOW_UPDATE, stream 1, Window Size Increment = 65535 ($0000FFFF).
  //   Len=4  Type=WINDOW_UPDATE($08)  Flags=0  StreamID=1
  H2F_WINDOW_UPDATE: array[0..12] of Byte = (
    $00, $00, $04,            // Length = 4
    $08,                      // Type = WINDOW_UPDATE
    $00,                      // Flags = 0
    $00, $00, $00, $01,       // StreamID = 1
    $00, $00, $FF, $FF);      // R=0, Increment = 65535

  // RST_STREAM, stream 1, Error Code = $000000FF (255 - UNREGISTERED on the
  // wire, exercising the raw-Cardinal contract; not in TH2ErrorCode).
  //   Len=4  Type=RST_STREAM($03)  Flags=0  StreamID=1
  H2F_RST_STREAM: array[0..12] of Byte = (
    $00, $00, $04,            // Length = 4
    $03,                      // Type = RST_STREAM
    $00,                      // Flags = 0
    $00, $00, $00, $01,       // StreamID = 1
    $00, $00, $00, $FF);      // Error Code = 255

  // GOAWAY, Last-Stream-ID = 1, Error Code = $0B (ENHANCE_YOUR_CALM=11),
  // Additional Debug Data = "x" ($78).
  //   payload = lastStreamID(4) + errorCode(4) + debug(1) = 9
  //   Len=9  Type=GOAWAY($07)  Flags=0  StreamID=0
  H2F_GOAWAY: array[0..17] of Byte = (
    $00, $00, $09,            // Length = 9
    $07,                      // Type = GOAWAY
    $00,                      // Flags = 0
    $00, $00, $00, $00,       // StreamID = 0
    $00, $00, $00, $01,       // R=0, Last-Stream-ID = 1
    $00, $00, $00, $0B,       // Error Code = 11
    $78);                     // debug data "x"

  // PING, opaque data = $01..$08.
  //   Len=8  Type=PING($06)  Flags=0  StreamID=0
  H2F_PING: array[0..16] of Byte = (
    $00, $00, $08,            // Length = 8
    $06,                      // Type = PING
    $00,                      // Flags = 0
    $00, $00, $00, $00,       // StreamID = 0
    $01, $02, $03, $04, $05, $06, $07, $08);   // opaque data

  // --- Malformed frames (must raise EH2FrameError) ------------------------

  // WINDOW_UPDATE with Length = 3 (≠ 4) -> ecFrameSizeError.
  H2F_BAD_WINDOW_UPDATE_SHORT: array[0..11] of Byte = (
    $00, $00, $03,            // Length = 3 (invalid)
    $08,                      // Type = WINDOW_UPDATE
    $00,
    $00, $00, $00, $01,       // StreamID = 1
    $00, $00, $FF);

  // RST_STREAM with Length = 5 (≠ 4) -> ecFrameSizeError.
  H2F_BAD_RST_STREAM_LEN: array[0..13] of Byte = (
    $00, $00, $05,            // Length = 5 (invalid)
    $03,                      // Type = RST_STREAM
    $00,
    $00, $00, $00, $01,       // StreamID = 1
    $00, $00, $00, $00, $00);

  // PING with Length = 7 (≠ 8) -> ecFrameSizeError.
  H2F_BAD_PING_LEN: array[0..15] of Byte = (
    $00, $00, $07,            // Length = 7 (invalid)
    $06,                      // Type = PING
    $00,
    $00, $00, $00, $00,       // StreamID = 0
    $01, $02, $03, $04, $05, $06, $07);

  // SETTINGS with Length = 5 (not a multiple of 6) -> ecFrameSizeError.
  H2F_BAD_SETTINGS_ODD: array[0..13] of Byte = (
    $00, $00, $05,            // Length = 5 (invalid)
    $04,                      // Type = SETTINGS
    $00,
    $00, $00, $00, $00,       // StreamID = 0
    $00, $03, $00, $00, $00);

  // GOAWAY with Length = 7 (< 8) -> ecFrameSizeError.
  H2F_BAD_GOAWAY_SHORT: array[0..15] of Byte = (
    $00, $00, $07,            // Length = 7 (invalid)
    $07,                      // Type = GOAWAY
    $00,
    $00, $00, $00, $00,       // StreamID = 0
    $00, $00, $00, $01, $00, $00, $00);

  // Padded DATA whose Pad Length (5) exceeds the bytes following the pad-length
  // octet (only 1) -> ecProtocolError.
  //   Len=2  Type=DATA  Flags=PADDED  StreamID=1  padoctet=$05  data=$99
  H2F_BAD_DATA_PADDING: array[0..10] of Byte = (
    $00, $00, $02,            // Length = 2
    $00,                      // Type = DATA
    $08,                      // Flags = PADDED
    $00, $00, $00, $01,       // StreamID = 1
    $05,                      // Pad Length = 5 (invalid: > 1 remaining)
    $99);                     // single data/padding octet

  // Padded HEADERS whose Pad Length (5) exceeds the bytes following the
  // pad-length octet (only 1) -> ecProtocolError (mirrors the DATA case for the
  // HEADERS pad-length guard, AC#4).
  //   Len=2  Type=HEADERS($01)  Flags=PADDED($08)  StreamID=1  padoctet=$05
  H2F_BAD_HEADERS_PADDING: array[0..10] of Byte = (
    $00, $00, $02,            // Length = 2
    $01,                      // Type = HEADERS
    $08,                      // Flags = PADDED
    $00, $00, $00, $01,       // StreamID = 1
    $05,                      // Pad Length = 5 (invalid: > 1 remaining)
    $99);                     // single fragment/padding octet

  // HEADERS (no END_HEADERS) on stream 1 followed by a CONTINUATION on a
  // DIFFERENT stream (3) -> ecProtocolError during reassembly.
  H2F_CONT_FOREIGN_STREAM: array[0..19] of Byte = (
    // HEADERS: Len=1 Type=HEADERS Flags=0 StreamID=1 fragment $AA
    $00, $00, $01, $01, $00, $00, $00, $00, $01, $AA,
    // CONTINUATION: Len=1 Type=CONTINUATION Flags=END_HEADERS StreamID=3 frag $BB
    $00, $00, $01, $09, $04, $00, $00, $00, $03, $BB);

  // HEADERS (no END_HEADERS) on stream 1 followed by a DATA frame (not
  // CONTINUATION) before END_HEADERS -> ecProtocolError during reassembly.
  H2F_CONT_INTERLEAVED: array[0..19] of Byte = (
    // HEADERS: Len=1 Type=HEADERS Flags=0 StreamID=1 fragment $AA
    $00, $00, $01, $01, $00, $00, $00, $00, $01, $AA,
    // DATA: Len=1 Type=DATA Flags=0 StreamID=1 data $BB
    $00, $00, $01, $00, $00, $00, $00, $00, $01, $BB);
