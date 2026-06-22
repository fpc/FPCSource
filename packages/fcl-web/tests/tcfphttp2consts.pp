unit tcfphttp2consts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  fphttp2consts;

type

  TTestHTTP2Consts = class(TTestCase)
  published
    procedure TestFrameTypeOrdinals;
    procedure TestErrorCodeOrdinals;
    procedure TestFlagConstants;
    procedure TestSettingsIdConstants;
    procedure TestWindowAndFrameSizeConstants;
    procedure TestClientPreface;
  end;

implementation

procedure TTestHTTP2Consts.TestFrameTypeOrdinals;
begin
  // RFC 9113 §11.2 frame-type registry  ordinal must equal the wire byte.
  AssertEquals(0, Ord(ftData));
  AssertEquals(1, Ord(ftHeaders));
  AssertEquals(2, Ord(ftPriority));
  AssertEquals(3, Ord(ftRSTStream));
  AssertEquals(4, Ord(ftSettings));
  AssertEquals(5, Ord(ftPushPromise));
  AssertEquals(6, Ord(ftPing));
  AssertEquals(7, Ord(ftGoAway));
  AssertEquals(8, Ord(ftWindowUpdate));
  AssertEquals(9, Ord(ftContinuation));
end;

procedure TTestHTTP2Consts.TestErrorCodeOrdinals;
begin
  // RFC 9113 §11.4 error-code registry.
  AssertEquals(0, Ord(ecNoError));
  AssertEquals(1, Ord(ecProtocolError));
  AssertEquals(2, Ord(ecInternalError));
  AssertEquals(3, Ord(ecFlowControlError));
  AssertEquals(4, Ord(ecSettingsTimeout));
  AssertEquals(5, Ord(ecStreamClosed));
  AssertEquals(6, Ord(ecFrameSizeError));
  AssertEquals(7, Ord(ecRefusedStream));
  AssertEquals(8, Ord(ecCancel));
  AssertEquals(9, Ord(ecCompressionError));
  AssertEquals(10, Ord(ecConnectError));
  AssertEquals(11, Ord(ecEnhanceYourCalm));
  AssertEquals(12, Ord(ecInadequateSecurity));
  AssertEquals(13, Ord(ecHTTP11Required));
end;

procedure TTestHTTP2Consts.TestFlagConstants;
begin
  // RFC 9113 §6 frame flags. END_STREAM and ACK share bit $01 by design.
  AssertEquals($01, H2_FLAG_END_STREAM);
  AssertEquals($01, H2_FLAG_ACK);
  AssertEquals($04, H2_FLAG_END_HEADERS);
  AssertEquals($08, H2_FLAG_PADDED);
  AssertEquals($20, H2_FLAG_PRIORITY);
end;

procedure TTestHTTP2Consts.TestSettingsIdConstants;
begin
  // RFC 9113 §11.3 SETTINGS parameter identifiers.
  AssertEquals($01, H2_SETTINGS_HEADER_TABLE_SIZE);
  AssertEquals($02, H2_SETTINGS_ENABLE_PUSH);
  AssertEquals($03, H2_SETTINGS_MAX_CONCURRENT_STREAMS);
  AssertEquals($04, H2_SETTINGS_INITIAL_WINDOW_SIZE);
  AssertEquals($05, H2_SETTINGS_MAX_FRAME_SIZE);
  AssertEquals($06, H2_SETTINGS_MAX_HEADER_LIST_SIZE);
end;

procedure TTestHTTP2Consts.TestWindowAndFrameSizeConstants;
begin
  // RFC-mandated protocol defaults / ceilings 
  AssertEquals(65535, H2_DEFAULT_INITIAL_WINDOW_SIZE);     // $FFFF, §6.9.2
  AssertEquals(16384, H2_DEFAULT_MAX_FRAME_SIZE);          // $4000, §4.2
  AssertEquals(16777215, H2_MAX_ALLOWED_FRAME_SIZE);       // $FFFFFF, §4.2
  AssertEquals(2147483647, H2_MAX_WINDOW_SIZE);            // $7FFFFFFF, §6.9.1
  AssertEquals(4096, H2_DEFAULT_HEADER_TABLE_SIZE);        // $1000, RFC 7541 §4.2
end;

procedure TTestHTTP2Consts.TestClientPreface;
const
  // RFC 9113 §3.4 - the human-readable preface text. Derive the reference octets
  // from THIS string (not a duplicated byte array) so a transcription error in
  // H2_CLIENT_PREFACE cannot be masked by an identical typo in the test.
  RefText = 'PRI * HTTP/2.0'#13#10#13#10'SM'#13#10#13#10;
var
  I: Integer;
begin
  // The literal itself is exactly 24 octets (independent of the constant).
  AssertEquals(24, Length(RefText));
  // Exactly 24 octets 
  AssertEquals(24, Length(H2_CLIENT_PREFACE));
  // Byte-for-byte equal to the RFC literal text (RefText is 1-based; the byte
  // array is 0-based - project-context.md FPC trap).
  for I := 0 to 23 do
    AssertEquals('preface byte ' + IntToStr(I), Ord(RefText[I + 1]), H2_CLIENT_PREFACE[I]);
end;

initialization

  RegisterTest(TTestHTTP2Consts);
end.
