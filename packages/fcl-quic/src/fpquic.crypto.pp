{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    QUIC packet protection (RFC 9001 section 5): key derivation, AEAD and header protection.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpQuic.Crypto;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, System.Hash.Sha256, System.Hash.Sha512, System.Hash.Utils, FpQuic.Consts, FpQuic.Buffers;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils, fpsha256, fpsha512, fphashutils, FpQuic.Consts, FpQuic.Buffers;
{$ENDIF FPC_DOTTEDUNITS}

type
  // A packet count limit per cipher suite.
  TQuicAeadLimits = array[TQuicCipherSuite] of QWord;

var
  // Number of packets protected with one key at which a connection closes; keys are updated at half of it
  // (RFC 9001 section 6.6).
  QuicConfidentialityLimit: TQuicAeadLimits = (8388608,8388608,High(QWord));
  // Number of received packets failing authentication at which a connection closes (RFC 9001 section 6.6).
  QuicIntegrityLimit: TQuicAeadLimits = (4503599627370496,4503599627370496,68719476736);

type
  TQuicNonce = array[0..11] of Byte;
  TQuicHPMask = array[0..4] of Byte;

  // AEAD cipher bound to one key.
  TQuicAead = class
  public
    // Encrypts aDataLen bytes of aData into aOut, followed by the 16-byte tag.
    procedure Seal(const aNonce: TQuicNonce; aAD: PByte; aADLen: Integer; aData: PByte; aDataLen: Integer;
      aOut: PByte); virtual; abstract;
    // Decrypts aDataLen bytes (ciphertext and tag) into aOut; False when authentication fails.
    function Open(const aNonce: TQuicNonce; aAD: PByte; aADLen: Integer; aData: PByte; aDataLen: Integer;
      aOut: PByte): Boolean; virtual; abstract;
  end;

  // Header protection cipher bound to one key.
  TQuicHeaderProtection = class
  public
    // Computes the 5-byte mask from a 16-byte sample.
    procedure Mask(aSample: PByte; out aMask: TQuicHPMask); virtual; abstract;
  end;

  // Creates cipher objects; the caller owns and frees the returned objects.
  IQuicCipherFactory = interface
    // Creates the AEAD of aSuite with a copy of key aKey.
    function CreateAead(aSuite: TQuicCipherSuite; const aKey: TBytes): TQuicAead;
    // Creates the header protection cipher of aSuite with a copy of key aKey.
    function CreateHeaderProtection(aSuite: TQuicCipherSuite; const aKey: TBytes): TQuicHeaderProtection;
  end;

  // Packet protection keys of one direction, one encryption level and one key phase generation.
  TQuicKeys = class
  private
    FFactory: IQuicCipherFactory;
    FSuite: TQuicCipherSuite;
    FSecret: TBytes;
    FIV: TBytes;
    FHPKey: TBytes;
    FAead: TQuicAead;
    FHP: TQuicHeaderProtection;
  public
    // Derives key, IV and header protection key from aSecret (RFC 9001 section 5.1).
    constructor Create(const aFactory: IQuicCipherFactory; aSuite: TQuicCipherSuite; const aSecret: TBytes);
    // Variant for key updates: derives packet keys from aSecret and keeps header protection key aHPKey.
    constructor CreateUpdate(const aFactory: IQuicCipherFactory; aSuite: TQuicCipherSuite;
      const aSecret, aHPKey: TBytes);
    destructor Destroy; override;
    // Returns the keys of the next key phase (RFC 9001 section 6).
    function NextGeneration: TQuicKeys;
    // Builds the AEAD nonce for packet number aPn.
    function Nonce(aPn: QWord): TQuicNonce;
    // Cipher suite of these keys.
    property Suite: TQuicCipherSuite read FSuite;
    // Traffic secret these keys were derived from.
    property Secret: TBytes read FSecret;
    // Packet payload AEAD.
    property Aead: TQuicAead read FAead;
    // Header protection cipher.
    property HP: TQuicHeaderProtection read FHP;
  end;

// HKDF-Extract (RFC 5869) with the hash of aSuite.
function QuicHkdfExtract(aSuite: TQuicCipherSuite; const aSalt, aIKM: TBytes): TBytes;

// HKDF-Expand-Label (RFC 8446 section 7.1) with the hash of aSuite.
function QuicHkdfExpandLabel(aSuite: TQuicCipherSuite; const aSecret: TBytes; const aLabel: AnsiString;
  const aContext: TBytes; aLength: Integer): TBytes;

// Derives the client and server Initial secrets from the client's first Destination Connection ID.
procedure QuicInitialSecrets(const aDestCid: TBytes; out aClientSecret, aServerSecret: TBytes);

// Key length of aSuite in bytes.
function QuicSuiteKeyLength(aSuite: TQuicCipherSuite): Integer;

// Returns aLength bytes from the random source of the operating system.
function QuicRandomBytes(aLength: Integer): TBytes;

// Encrypts the payload after the header and applies header protection.
// aPacket holds the header (unprotected packet number at aPnOffset, aPnLength bytes) and the plaintext.
function QuicProtectPacket(aKeys: TQuicKeys; aPn: QWord; const aPacket: TBytes; aPnOffset, aPnLength: Integer): TBytes;

// Removes header protection in place; False when the packet is too short to sample.
function QuicUnprotectHeader(aHP: TQuicHeaderProtection; var aPacket: TBytes; aPnOffset: Integer;
  out aPnLength: Integer; out aTruncatedPn: QWord): Boolean;

// Decrypts the payload following aHeaderLength header bytes; False when authentication fails.
function QuicDecryptPayload(aKeys: TQuicKeys; aPn: QWord; const aPacket: TBytes; aHeaderLength: Integer;
  out aPayload: TBytes): Boolean;

// Computes the Retry integrity tag (RFC 9001 section 5.8).
function QuicRetryIntegrityTag(const aFactory: IQuicCipherFactory; const aOriginalDestCid,
  aRetryWithoutTag: TBytes): TBytes;

implementation

const
  InitialSalt = '38762cf7f55934b34d179ae6a4c80cadccbb7f0a';
  RetryKey = 'be0c690b9f66575a1d766b54e368c84e';
  RetryNonce = '461599d35d632bf2239825bb';

// HMAC of aData with the hash of aSuite, using aPrefix and aSuffix around aData.
function Hmac(aSuite: TQuicCipherSuite; const aKey: TBytes; aPrefix: PByte; aPrefixLen: Integer;
  const aData: TBytes; aSuffix: Byte): TBytes;

var
  lDigest256: TSHA256Digest;
  lDigest384: TSHA384Digest;

begin
  if aSuite=qcsAes256Gcm then
    begin
    lDigest384:=Default(TSHA384Digest);
    TSHA384.HMAC(PByte(aKey),Length(aKey),aPrefix,aPrefixLen,PByte(aData),Length(aData),@aSuffix,1,lDigest384);
    Result:=QuicBytesOf(@lDigest384[0],SizeOf(lDigest384));
    end
  else
    begin
    lDigest256:=Default(TSHA256Digest);
    TSHA256.HMAC(PByte(aKey),Length(aKey),aPrefix,aPrefixLen,PByte(aData),Length(aData),@aSuffix,1,lDigest256);
    Result:=QuicBytesOf(@lDigest256[0],SizeOf(lDigest256));
    end;
end;


function QuicHkdfExtract(aSuite: TQuicCipherSuite; const aSalt, aIKM: TBytes): TBytes;

var
  lDigest256: TSHA256Digest;
  lDigest384: TSHA384Digest;

begin
  if aSuite=qcsAes256Gcm then
    begin
    lDigest384:=Default(TSHA384Digest);
    TSHA384.HMAC(PByte(aSalt),Length(aSalt),PByte(aIKM),Length(aIKM),lDigest384);
    Result:=QuicBytesOf(@lDigest384[0],SizeOf(lDigest384));
    end
  else
    begin
    lDigest256:=Default(TSHA256Digest);
    TSHA256.HMAC(PByte(aSalt),Length(aSalt),PByte(aIKM),Length(aIKM),lDigest256);
    Result:=QuicBytesOf(@lDigest256[0],SizeOf(lDigest256));
    end;
end;


function QuicHkdfExpandLabel(aSuite: TQuicCipherSuite; const aSecret: TBytes; const aLabel: AnsiString;
  const aContext: TBytes; aLength: Integer): TBytes;

const
  LabelPrefix: array[0..5] of AnsiChar = 'tls13 ';

var
  lInfo: TQuicWriter;
  lInfoBytes, lBlock: TBytes;
  lCounter: Byte;

begin
  lInfo.Init;
  lInfo.WriteUInt16(aLength);
  lInfo.WriteByte(SizeOf(LabelPrefix)+Length(aLabel));
  lInfo.WriteBuffer(@LabelPrefix[0],SizeOf(LabelPrefix));
  lInfo.WriteBuffer(PByte(aLabel),Length(aLabel));
  lInfo.WriteByte(Length(aContext));
  lInfo.WriteBytes(aContext);
  lInfoBytes:=lInfo.ToBytes;
  Result:=nil;
  lBlock:=nil;
  lCounter:=1;
  while Length(Result)<aLength do
    begin
    lBlock:=Hmac(aSuite,aSecret,PByte(lBlock),Length(lBlock),lInfoBytes,lCounter);
    Result:=Concat(Result,lBlock);
    Inc(lCounter);
    end;
  SetLength(Result,aLength);
end;


procedure QuicInitialSecrets(const aDestCid: TBytes; out aClientSecret, aServerSecret: TBytes);

var
  lInitial: TBytes;

begin
  lInitial:=QuicHkdfExtract(qcsAes128Gcm,QuicHexToBytes(InitialSalt),aDestCid);
  aClientSecret:=QuicHkdfExpandLabel(qcsAes128Gcm,lInitial,'client in',nil,32);
  aServerSecret:=QuicHkdfExpandLabel(qcsAes128Gcm,lInitial,'server in',nil,32);
end;


function QuicSuiteKeyLength(aSuite: TQuicCipherSuite): Integer;

begin
  if aSuite=qcsAes128Gcm then
    Result:=16
  else
    Result:=32;
end;


function QuicRandomBytes(aLength: Integer): TBytes;

begin
{$IF NOT DEFINED(MSWINDOWS) AND NOT DEFINED(UNIX)}
  if not OSRandomBytesAvailable then
    raise EQuicError.CreateCode(QUIC_INTERNAL_ERROR,'The operating system has no random source');
{$ENDIF}
  Result:=nil;
  SetLength(Result,aLength);
  CryptoGetRandomBytes(PByte(Result),aLength);
end;


{ TQuicKeys }

constructor TQuicKeys.Create(const aFactory: IQuicCipherFactory; aSuite: TQuicCipherSuite; const aSecret: TBytes);

begin
  CreateUpdate(aFactory,aSuite,aSecret,QuicHkdfExpandLabel(aSuite,aSecret,'quic hp',nil,QuicSuiteKeyLength(aSuite)));
end;


constructor TQuicKeys.CreateUpdate(const aFactory: IQuicCipherFactory; aSuite: TQuicCipherSuite;
  const aSecret, aHPKey: TBytes);

var
  lKey: TBytes;

begin
  FFactory:=aFactory;
  FSuite:=aSuite;
  FSecret:=Copy(aSecret);
  FHPKey:=Copy(aHPKey);
  FIV:=QuicHkdfExpandLabel(aSuite,aSecret,'quic iv',nil,12);
  lKey:=QuicHkdfExpandLabel(aSuite,aSecret,'quic key',nil,QuicSuiteKeyLength(aSuite));
  FAead:=aFactory.CreateAead(aSuite,lKey);
  QuicWipe(lKey);
  FHP:=aFactory.CreateHeaderProtection(aSuite,aHPKey);
end;


destructor TQuicKeys.Destroy;

begin
  FAead.Free;
  FHP.Free;
  QuicWipe(FSecret);
  QuicWipe(FIV);
  QuicWipe(FHPKey);
  inherited Destroy;
end;


function TQuicKeys.NextGeneration: TQuicKeys;

var
  lSecret: TBytes;

begin
  lSecret:=QuicHkdfExpandLabel(FSuite,FSecret,'quic ku',nil,Length(FSecret));
  Result:=TQuicKeys.CreateUpdate(FFactory,FSuite,lSecret,FHPKey);
end;


function TQuicKeys.Nonce(aPn: QWord): TQuicNonce;

var
  I: Integer;

begin
  Result:=Default(TQuicNonce);
  Move(FIV[0],Result[0],12);
  for I:=0 to 7 do
    Result[11-I]:=Result[11-I] xor ((aPn shr (I*8)) and $FF);
end;


// Applies the header protection mask to the packet number bytes of aPacket.
procedure ApplyMask(var aPacket: TBytes; aPnOffset, aPnLength: Integer; const aMask: TQuicHPMask);

var
  I: Integer;

begin
  for I:=0 to aPnLength-1 do
    aPacket[aPnOffset+I]:=aPacket[aPnOffset+I] xor aMask[1+I];
end;


// Masks the low bits of the first byte: 4 bits for long headers, 5 bits for short headers.
function FirstByteMask(aFirstByte: Byte): Byte;

begin
  if (aFirstByte and $80)<>0 then
    Result:=$0F
  else
    Result:=$1F;
end;


function QuicProtectPacket(aKeys: TQuicKeys; aPn: QWord; const aPacket: TBytes; aPnOffset, aPnLength: Integer): TBytes;

var
  lHeaderLength, lPlainLength: Integer;
  lMask: TQuicHPMask;

begin
  lHeaderLength:=aPnOffset+aPnLength;
  lPlainLength:=Length(aPacket)-lHeaderLength;
  Result:=nil;
  SetLength(Result,Length(aPacket)+QUIC_AEAD_TAG_LENGTH);
  Move(aPacket[0],Result[0],lHeaderLength);
  aKeys.Aead.Seal(aKeys.Nonce(aPn),@aPacket[0],lHeaderLength,PByte(aPacket)+lHeaderLength,lPlainLength,
    @Result[lHeaderLength]);
  aKeys.HP.Mask(@Result[aPnOffset+4],lMask);
  Result[0]:=Result[0] xor (lMask[0] and FirstByteMask(Result[0]));
  ApplyMask(Result,aPnOffset,aPnLength,lMask);
end;


function QuicUnprotectHeader(aHP: TQuicHeaderProtection; var aPacket: TBytes; aPnOffset: Integer;
  out aPnLength: Integer; out aTruncatedPn: QWord): Boolean;

var
  lMask: TQuicHPMask;
  I: Integer;

begin
  aPnLength:=0;
  aTruncatedPn:=0;
  if Length(aPacket)<aPnOffset+4+QUIC_HP_SAMPLE_LENGTH then
    Exit(False);
  aHP.Mask(@aPacket[aPnOffset+4],lMask);
  aPacket[0]:=aPacket[0] xor (lMask[0] and FirstByteMask(aPacket[0]));
  aPnLength:=(aPacket[0] and 3)+1;
  ApplyMask(aPacket,aPnOffset,aPnLength,lMask);
  for I:=0 to aPnLength-1 do
    aTruncatedPn:=(aTruncatedPn shl 8) or aPacket[aPnOffset+I];
  Result:=True;
end;


function QuicDecryptPayload(aKeys: TQuicKeys; aPn: QWord; const aPacket: TBytes; aHeaderLength: Integer;
  out aPayload: TBytes): Boolean;

var
  lCipherLength: Integer;

begin
  aPayload:=nil;
  lCipherLength:=Length(aPacket)-aHeaderLength;
  if lCipherLength<QUIC_AEAD_TAG_LENGTH then
    Exit(False);
  SetLength(aPayload,lCipherLength-QUIC_AEAD_TAG_LENGTH);
  Result:=aKeys.Aead.Open(aKeys.Nonce(aPn),@aPacket[0],aHeaderLength,PByte(aPacket)+aHeaderLength,lCipherLength,
    PByte(aPayload));
end;


function QuicRetryIntegrityTag(const aFactory: IQuicCipherFactory; const aOriginalDestCid,
  aRetryWithoutTag: TBytes): TBytes;

var
  lAead: TQuicAead;
  lPseudo: TQuicWriter;
  lNonce: TQuicNonce;
  lBytes: TBytes;

begin
  lPseudo.Init;
  lPseudo.WriteByte(Length(aOriginalDestCid));
  lPseudo.WriteBytes(aOriginalDestCid);
  lPseudo.WriteBytes(aRetryWithoutTag);
  lBytes:=lPseudo.ToBytes;
  lNonce:=Default(TQuicNonce);
  Move(QuicHexToBytes(RetryNonce)[0],lNonce[0],12);
  Result:=nil;
  SetLength(Result,QUIC_AEAD_TAG_LENGTH);
  lAead:=aFactory.CreateAead(qcsAes128Gcm,QuicHexToBytes(RetryKey));
  try
    lAead.Seal(lNonce,PByte(lBytes),Length(lBytes),nil,0,PByte(Result));
  finally
    lAead.Free;
  end;
end;


end.
