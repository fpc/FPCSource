{
  This file is part of the Free Component Library.
  Tests for FpQuic.Crypto and the OpenSSL cipher factory, using the RFC 9001 appendix A vectors.
  See the file COPYING.FPC, included in this distribution, for details about the copyright.
}
unit utquiccrypto;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpcunit, testregistry, FpQuic.Consts, FpQuic.Buffers, FpQuic.Packets, FpQuic.Crypto, FpQuic.OpenSSL, utquichelpers;

type
  TTestQuicCrypto = class(TTestCase)
  private
    function Factory: IQuicCipherFactory;
    procedure CheckRoundTrip(aSuite: TQuicCipherSuite; aSecretLength: Integer);
  published
    procedure TestHkdfExtract;
    procedure TestInitialSecrets;
    procedure TestInitialKeys;
    procedure TestClientInitialHeaderProtection;
    procedure TestChaChaShortHeaderPacket;
    procedure TestKeyUpdate;
    procedure TestRoundTripAllSuites;
    procedure TestDecryptFailures;
    procedure TestRetryIntegrityTag;
    procedure TestSuiteKeyLength;
    procedure TestOpenSSLError;
    procedure TestRandomBytes;
  end;

implementation

const
  ClientDcid = '8394c8f03e515708';

function NonceHex(aKeys: TQuicKeys; aPn: QWord): String;

var
  lNonce: TQuicNonce;

begin
  lNonce:=aKeys.Nonce(aPn);
  Result:=QuicBytesToHex(QuicBytesOf(@lNonce[0],12));
end;


function TTestQuicCrypto.Factory: IQuicCipherFactory;

begin
  Result:=TestCipherFactory;
end;


procedure TTestQuicCrypto.TestHkdfExtract;

var
  lSalt, lIKM: TBytes;

begin
  lSalt:=QuicHexToBytes('000102030405060708090a0b0c');
  lIKM:=QuicHexToBytes('0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b');
  AssertEquals('RFC 5869 case 1 PRK','077709362c2e32df0ddc3f0dc47bba6390b6c73bb50f9c3122ec844ad7c2b3e5',
    QuicBytesToHex(QuicHkdfExtract(qcsAes128Gcm,lSalt,lIKM)));
  AssertEquals('SHA-384 PRK',
    '704b39990779ce1dc548052c7dc39f303570dd13fb39f7acc564680bef80e8dec70ee9a7e1f3e293ef68eceb072a5ade',
    QuicBytesToHex(QuicHkdfExtract(qcsAes256Gcm,lSalt,lIKM)));
end;


procedure TTestQuicCrypto.TestInitialSecrets;

var
  lClient, lServer: TBytes;

begin
  QuicInitialSecrets(QuicHexToBytes(ClientDcid),lClient,lServer);
  AssertEquals('Client secret','c00cf151ca5be075ed0ebfb5c80323c42d6b7db67881289af4008f1f6c357aea',QuicBytesToHex(lClient));
  AssertEquals('Server secret','3c199828fd139efd216c155ad844cc81fb82fa8d7446fa7d78be803acdda951b',QuicBytesToHex(lServer));
end;


procedure TTestQuicCrypto.TestInitialKeys;

var
  lClient, lServer: TBytes;

begin
  QuicInitialSecrets(QuicHexToBytes(ClientDcid),lClient,lServer);
  AssertEquals('Client key','1f369613dd76d5467730efcbe3b1a22d',
    QuicBytesToHex(QuicHkdfExpandLabel(qcsAes128Gcm,lClient,'quic key',nil,16)));
  AssertEquals('Client iv','fa044b2f42a3fd3b46fb255c',
    QuicBytesToHex(QuicHkdfExpandLabel(qcsAes128Gcm,lClient,'quic iv',nil,12)));
  AssertEquals('Client hp','9f50449e04a0e810283a1e9933adedd2',
    QuicBytesToHex(QuicHkdfExpandLabel(qcsAes128Gcm,lClient,'quic hp',nil,16)));
  AssertEquals('Server key','cf3a5331653c364c88f0f379b6067e37',
    QuicBytesToHex(QuicHkdfExpandLabel(qcsAes128Gcm,lServer,'quic key',nil,16)));
  AssertEquals('Server iv','0ac1493ca1905853b0bba03e',
    QuicBytesToHex(QuicHkdfExpandLabel(qcsAes128Gcm,lServer,'quic iv',nil,12)));
  AssertEquals('Server hp','c206b8d9b9f0f37644430b490eeaa314',
    QuicBytesToHex(QuicHkdfExpandLabel(qcsAes128Gcm,lServer,'quic hp',nil,16)));
end;


procedure TTestQuicCrypto.TestClientInitialHeaderProtection;

var
  lClient, lServer: TBytes;
  lKeys: TQuicKeys;
  lMask: TQuicHPMask;
  lSample: TBytes;

begin
  QuicInitialSecrets(QuicHexToBytes(ClientDcid),lClient,lServer);
  lKeys:=TQuicKeys.Create(Factory,qcsAes128Gcm,lClient);
  try
    lSample:=QuicHexToBytes('d1b1c98dd7689fb8ec11d242b123dc9b');
    lKeys.HP.Mask(PByte(lSample),lMask);
    AssertEquals('Mask','437b9aec36',QuicBytesToHex(QuicBytesOf(@lMask[0],5)));
    AssertEquals('Nonce for pn 2','fa044b2f42a3fd3b46fb255e',NonceHex(lKeys,2));
  finally
    lKeys.Free;
  end;
end;


procedure TTestQuicCrypto.TestChaChaShortHeaderPacket;

var
  lKeys: TQuicKeys;
  lProtected, lPacket, lPayload: TBytes;
  lPnLength: Integer;
  lTruncated: QWord;

begin
  lKeys:=TQuicKeys.Create(Factory,qcsChaCha20Poly1305,
    QuicHexToBytes('9ac312a7f877468ebe69422748ad00a15443f18203a07d6060f688f30f21632b'));
  try
    AssertEquals('Nonce','e0459b3474bdd0e46d417eb0',NonceHex(lKeys,654360564));
    lProtected:=QuicProtectPacket(lKeys,654360564,QuicHexToBytes('4200bff401'),1,3);
    AssertEquals('Protected packet','4cfe4189655e5cd55c41f69080575d7999c25a5bfb',QuicBytesToHex(lProtected));
    lPacket:=Copy(lProtected);
    AssertTrue('Header unprotected',QuicUnprotectHeader(lKeys.HP,lPacket,1,lPnLength,lTruncated));
    AssertEquals('PN length',3,lPnLength);
    AssertEquals('Truncated PN',$00bff4,lTruncated);
    AssertEquals('First byte',$42,lPacket[0]);
    AssertTrue('Decrypted',QuicDecryptPayload(lKeys,654360564,lPacket,4,lPayload));
    AssertEquals('Payload','01',QuicBytesToHex(lPayload));
  finally
    lKeys.Free;
  end;
end;


procedure TTestQuicCrypto.TestKeyUpdate;

var
  lKeys, lNext: TQuicKeys;
  lProtected, lPacket, lPayload: TBytes;
  lPnLength: Integer;
  lTruncated: QWord;

begin
  lKeys:=TQuicKeys.Create(Factory,qcsChaCha20Poly1305,
    QuicHexToBytes('9ac312a7f877468ebe69422748ad00a15443f18203a07d6060f688f30f21632b'));
  lNext:=lKeys.NextGeneration;
  try
    AssertEquals('Updated secret','1223504755036d556342ee9361d253421a826c9ecdf3c7148684b36b714881f9',
      QuicBytesToHex(lNext.Secret));
    AssertTrue('Suite kept',lNext.Suite=qcsChaCha20Poly1305);
    lProtected:=QuicProtectPacket(lNext,7,QuicHexToBytes('4407 00000000'),1,1);
    lPacket:=Copy(lProtected);
    AssertTrue('Same header protection key',QuicUnprotectHeader(lKeys.HP,lPacket,1,lPnLength,lTruncated));
    AssertEquals('PN',7,lTruncated);
    AssertFalse('Old keys cannot decrypt',QuicDecryptPayload(lKeys,7,lPacket,2,lPayload));
    AssertTrue('New keys decrypt',QuicDecryptPayload(lNext,7,lPacket,2,lPayload));
  finally
    lNext.Free;
    lKeys.Free;
  end;
end;


procedure TTestQuicCrypto.CheckRoundTrip(aSuite: TQuicCipherSuite; aSecretLength: Integer);

var
  lKeys: TQuicKeys;
  lSecret, lPlain, lProtected, lPayload: TBytes;
  lWriter: TQuicWriter;
  lPnOffset, lPnLength: Integer;
  lTruncated: QWord;

begin
  lSecret:=nil;
  SetLength(lSecret,aSecretLength);
  FillChar(lSecret[0],aSecretLength,7);
  lKeys:=TQuicKeys.Create(Factory,aSuite,lSecret);
  try
    lWriter.Init;
    lPnOffset:=QuicWriteLongHeader(lWriter,qptInitial,QuicHexToBytes('0102'),nil,nil,300,2,18+16);
    lWriter.WriteBytes(TEncoding.UTF8.GetBytes('hello quic payload'));
    lPlain:=lWriter.ToBytes;
    lProtected:=QuicProtectPacket(lKeys,300,lPlain,lPnOffset,2);
    AssertEquals('Tag appended',Length(lPlain)+16,Length(lProtected));
    AssertTrue('Header unprotected',QuicUnprotectHeader(lKeys.HP,lProtected,lPnOffset,lPnLength,lTruncated));
    AssertEquals('PN length',2,lPnLength);
    AssertEquals('PN',300,lTruncated);
    AssertTrue('Decrypted',QuicDecryptPayload(lKeys,300,lProtected,lPnOffset+2,lPayload));
    AssertEquals('Payload','hello quic payload',TEncoding.UTF8.GetString(lPayload));
  finally
    lKeys.Free;
  end;
end;


procedure TTestQuicCrypto.TestRoundTripAllSuites;

begin
  CheckRoundTrip(qcsAes128Gcm,32);
  CheckRoundTrip(qcsAes256Gcm,48);
  CheckRoundTrip(qcsChaCha20Poly1305,32);
end;


procedure TTestQuicCrypto.TestDecryptFailures;

var
  lKeys: TQuicKeys;
  lPacket, lPayload: TBytes;
  lPnLength: Integer;
  lTruncated: QWord;

begin
  lKeys:=TQuicKeys.Create(Factory,qcsAes128Gcm,QuicHexToBytes(StringOfChar('1',64)));
  try
    lPacket:=QuicHexToBytes('4100 0102');
    AssertFalse('Too short to sample',QuicUnprotectHeader(lKeys.HP,lPacket,1,lPnLength,lTruncated));
    AssertEquals('No PN length',0,lPnLength);
    AssertFalse('Shorter than a tag',QuicDecryptPayload(lKeys,0,lPacket,2,lPayload));
    lPacket:=QuicProtectPacket(lKeys,1,QuicHexToBytes('4001 00000000'),1,1);
    lPacket[Length(lPacket)-1]:=lPacket[Length(lPacket)-1] xor 1;
    AssertFalse('Tampered tag',QuicDecryptPayload(lKeys,1,lPacket,2,lPayload));
  finally
    lKeys.Free;
  end;
end;


procedure TTestQuicCrypto.TestRetryIntegrityTag;

begin
  AssertEquals('RFC 9001 A.4 tag','04a265ba2eff4d829058fb3f0f2496ba',
    QuicBytesToHex(QuicRetryIntegrityTag(Factory,QuicHexToBytes(ClientDcid),
      QuicHexToBytes('ff000000010008f067a5502a4262b5746f6b656e'))));
end;


procedure TTestQuicCrypto.TestSuiteKeyLength;

begin
  AssertEquals('AES-128',16,QuicSuiteKeyLength(qcsAes128Gcm));
  AssertEquals('AES-256',32,QuicSuiteKeyLength(qcsAes256Gcm));
  AssertEquals('ChaCha20',32,QuicSuiteKeyLength(qcsChaCha20Poly1305));
end;


procedure TTestQuicCrypto.TestOpenSSLError;

var
  lAead: TQuicAead;
  lNonce: TQuicNonce;
  lOut: array[0..31] of Byte;

begin
  lNonce:=Default(TQuicNonce);
  FillChar(lOut,SizeOf(lOut),0);
  lAead:=Factory.CreateAead(qcsAes128Gcm,QuicHexToBytes(StringOfChar('2',32)));
  try
    try
      lAead.Seal(lNonce,@lOut[0],-1,nil,0,@lOut[0]);
      Fail('Negative associated data length must fail');
    except
      on E: EQuicOpenSSL do
        AssertTrue('Message',Pos('AEAD',E.Message)>0);
    end;
  finally
    lAead.Free;
  end;
end;


procedure TTestQuicCrypto.TestRandomBytes;

var
  lFirst, lSecond: TBytes;

begin
  lFirst:=QuicRandomBytes(16);
  lSecond:=QuicRandomBytes(16);
  AssertEquals('Length',16,Length(lFirst));
  AssertFalse('Different values',QuicBytesEqual(lFirst,lSecond));
end;


initialization
  RegisterTest(TTestQuicCrypto);
end.
