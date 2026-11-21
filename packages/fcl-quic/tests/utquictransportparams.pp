{
  This file is part of the Free Component Library.
  Tests for FpQuic.TransportParams.
  See the file COPYING.FPC, included in this distribution, for details about the copyright.
}
unit utquictransportparams;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpcunit, testregistry, FpQuic.Consts, FpQuic.Buffers, FpQuic.TransportParams;

type
  TTestQuicTransportParams = class(TTestCase)
  private
    procedure ExpectError(const aHex: String; aFromServer: Boolean);
  published
    procedure TestDefaults;
    procedure TestEmptyEncoding;
    procedure TestRoundTripAll;
    procedure TestUnknownParameterIgnored;
    procedure TestInvalidParameters;
    procedure TestServerOnlyFromClient;
  end;

implementation

procedure TTestQuicTransportParams.ExpectError(const aHex: String; aFromServer: Boolean);

begin
  try
    QuicDecodeTransportParams(QuicHexToBytes(aHex),aFromServer);
    Fail('Expected transport parameter error for '+aHex);
  except
    on E: EQuicError do
      AssertEquals('Error code for '+aHex,QUIC_TRANSPORT_PARAMETER_ERROR,E.ErrorCode);
  end;
end;


procedure TTestQuicTransportParams.TestDefaults;

var
  lParams: TQuicTransportParams;

begin
  lParams:=QuicDefaultTransportParams;
  AssertEquals('UDP payload',65527,lParams.MaxUdpPayloadSize);
  AssertEquals('Ack delay exponent',3,lParams.AckDelayExponent);
  AssertEquals('Max ack delay',25,lParams.MaxAckDelay);
  AssertEquals('CID limit',2,lParams.ActiveConnectionIdLimit);
  AssertEquals('Max data',0,lParams.InitialMaxData);
end;


procedure TTestQuicTransportParams.TestEmptyEncoding;

begin
  AssertEquals('Defaults encode to nothing',0,Length(QuicEncodeTransportParams(QuicDefaultTransportParams)));
  AssertEquals('Empty decodes to defaults',25,QuicDecodeTransportParams(nil,True).MaxAckDelay);
end;


procedure TTestQuicTransportParams.TestRoundTripAll;

var
  lParams, lOut: TQuicTransportParams;

begin
  lParams:=QuicDefaultTransportParams;
  lParams.HasOriginalDestinationCid:=True;
  lParams.OriginalDestinationCid:=QuicHexToBytes('0102');
  lParams.MaxIdleTimeout:=30000;
  lParams.HasStatelessResetToken:=True;
  lParams.StatelessResetToken:=QuicHexToBytes('00112233445566778899aabbccddeeff');
  lParams.MaxUdpPayloadSize:=1500;
  lParams.InitialMaxData:=1048576;
  lParams.InitialMaxStreamDataBidiLocal:=1;
  lParams.InitialMaxStreamDataBidiRemote:=2;
  lParams.InitialMaxStreamDataUni:=3;
  lParams.InitialMaxStreamsBidi:=100;
  lParams.InitialMaxStreamsUni:=101;
  lParams.AckDelayExponent:=4;
  lParams.MaxAckDelay:=30;
  lParams.DisableActiveMigration:=True;
  lParams.HasPreferredAddress:=True;
  lParams.PreferredAddress:=QuicHexToBytes('aabb');
  lParams.ActiveConnectionIdLimit:=4;
  lParams.HasInitialSourceCid:=True;
  lParams.InitialSourceCid:=QuicHexToBytes('03');
  lParams.HasRetrySourceCid:=True;
  lParams.RetrySourceCid:=QuicHexToBytes('04');
  lOut:=QuicDecodeTransportParams(QuicEncodeTransportParams(lParams),True);
  AssertTrue('ODCID present',lOut.HasOriginalDestinationCid);
  AssertEquals('ODCID','0102',QuicBytesToHex(lOut.OriginalDestinationCid));
  AssertEquals('Idle',30000,lOut.MaxIdleTimeout);
  AssertTrue('Token present',lOut.HasStatelessResetToken);
  AssertEquals('Token','00112233445566778899aabbccddeeff',QuicBytesToHex(lOut.StatelessResetToken));
  AssertEquals('UDP payload',1500,lOut.MaxUdpPayloadSize);
  AssertEquals('Max data',QWord(1048576),lOut.InitialMaxData);
  AssertEquals('Bidi local',1,lOut.InitialMaxStreamDataBidiLocal);
  AssertEquals('Bidi remote',2,lOut.InitialMaxStreamDataBidiRemote);
  AssertEquals('Uni',3,lOut.InitialMaxStreamDataUni);
  AssertEquals('Streams bidi',100,lOut.InitialMaxStreamsBidi);
  AssertEquals('Streams uni',101,lOut.InitialMaxStreamsUni);
  AssertEquals('Exponent',4,lOut.AckDelayExponent);
  AssertEquals('Ack delay',30,lOut.MaxAckDelay);
  AssertTrue('Migration disabled',lOut.DisableActiveMigration);
  AssertTrue('Preferred address',lOut.HasPreferredAddress);
  AssertEquals('Preferred address value','aabb',QuicBytesToHex(lOut.PreferredAddress));
  AssertEquals('CID limit',4,lOut.ActiveConnectionIdLimit);
  AssertEquals('Initial SCID','03',QuicBytesToHex(lOut.InitialSourceCid));
  AssertTrue('Retry SCID present',lOut.HasRetrySourceCid);
  AssertEquals('Retry SCID','04',QuicBytesToHex(lOut.RetrySourceCid));
end;


procedure TTestQuicTransportParams.TestUnknownParameterIgnored;

var
  lOut: TQuicTransportParams;

begin
  lOut:=QuicDecodeTransportParams(QuicHexToBytes('4a2a 02 abcd 1b 00 04 01 07'),False);
  AssertEquals('Known parameter after unknown ones',7,lOut.InitialMaxData);
end;


procedure TTestQuicTransportParams.TestInvalidParameters;

begin
  ExpectError('04 01 07 04 01 08',True);
  ExpectError('04',True);
  ExpectError('04 05 01',True);
  ExpectError('04 00',True);
  ExpectError('04 02 07 07',True);
  ExpectError('02 01 00',True);
  ExpectError('03 02 4100',True);
  ExpectError('08 08 d000000000000001',True);
  ExpectError('09 08 d000000000000001',True);
  ExpectError('0a 01 15',True);
  ExpectError('0b 04 80004000',True);
  ExpectError('0c 01 00',True);
  ExpectError('0e 01 01',True);
end;


procedure TTestQuicTransportParams.TestServerOnlyFromClient;

begin
  ExpectError('00 01 aa',False);
  ExpectError('02 10 00112233445566778899aabbccddeeff',False);
  ExpectError('0d 00',False);
  ExpectError('10 01 aa',False);
end;


initialization
  RegisterTest(TTestQuicTransportParams);
end.
