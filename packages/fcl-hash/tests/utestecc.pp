unit utestecc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpecc, fphashutils;

type

  { TTestECC }

  TTestECC = class(TTestCase)
  protected
    // Private key from 64 hex digits
    function PrivateKeyFromHex(const aHex: String): TEccPrivateKey;
    // Upper case hex of a public key
    function PublicKeyToHex(const aKey: TEccPublicKey): String;
    // Sign aHash with aPrivateKey and check the signature with the matching public key
    procedure CheckSignVerify(const aMsg: String; const aPrivateKey: TEccPrivateKey; const aHash: TEccHash);
  published
    procedure TestPublicKeyFromPrivateKey;
    procedure TestSignVerifyEdgeKeys;
    procedure TestSignVerifyRandomKeys;
    procedure TestEcdhAgreement;
    procedure TestNoRandomBytes;
    procedure TestInvalidPublicKeys;
    procedure TestImportX963;
    procedure TestRandomScalarRejection;
  end;

implementation

const
  // Private key, compressed public key; computed with OpenSSL
  KeyVectors: array[0..11, 0..1] of String = (
    ('0000000000000000000000000000000000000000000000000000000000000001','036B17D1F2E12C4247F8BCE6E563A440F277037D812DEB33A0F4A13945D898C296'),
    ('0000000000000000000000000000000000000000000000000000000000000002','037CF27B188D034F7E8A52380304B51AC3C08969E277F21B35A60B48FC47669978'),
    ('0000000000000000000000000000000000000000000000000000000000000003','025ECBE4D1A6330A44C8F7EF951D4BF165E6C6B721EFADA985FB41661BC6E7FD6C'),
    ('FFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632550','026B17D1F2E12C4247F8BCE6E563A440F277037D812DEB33A0F4A13945D898C296'),
    ('FFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC63254F','027CF27B188D034F7E8A52380304B51AC3C08969E277F21B35A60B48FC47669978'),
    ('8000000000000000000000000000000000000000000000000000000000000000','0377B20A912E6B23135066E911891524BC4EFE3560E3E92350B52DEC8F375F2B54'),
    ('7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF','03C1D17269E46E387ACBE299EC2CC9CC2DADA3F05E4CF412F2AD946B700AA2613A'),
    ('000000000000000000000000000000000000000000000000000000000000005A','03B250BB19A7768783D051C1A254E735B22AA996CE835871B896A19FBC3F50A457'),
    ('000000000000000000000000000000005A5A5A5A5A5A5A5A5A5A5A5A5A5A5A5A','029967DA841E4A44464A69330CD5AB962CEFCF2C9DFB11505A649CEBFEFFADE807'),
    ('5A5A5A5A5A5A5A5A5A5A5A5A5A5A5A5A5A5A5A5A5A5A5A5A5A5A5A5A5A5A5A5A','03F68B1DE0F5CE686192F4BD0FD34163863F2E89D385F8BCFB85979533A57AC9B0'),
    ('FFFFFFFEFFFFFF00FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632551','02285250EDC3BCFCD9027BBA12176D343A26035672D5B4A81155D4E37A2FD20BAE'),
    ('00000000000000000000000000000000123456789ABCDEF0FEDCBA9876543210','03CFF9839CBB40EC26ED68292ACA28802ACEBBE4AA28A244EEA5C890C03965B7EB')
  );

const
  Point2GCompressed = '037CF27B188D034F7E8A52380304B51AC3C08969E277F21B35A60B48FC47669978';
  Point2GUncompressed = '047CF27B188D034F7E8A52380304B51AC3C08969E277F21B35A60B48FC47669978'
                        +'07775510DB8ED040293D9AC69F7430DBBA7DADE63CE982299E04B79D227873D1';
  // x=1 (no curve point), x >= p, and a valid x with prefixes 04 and 00
  InvalidPublicKeys: array[0..3] of String = (
    '020000000000000000000000000000000000000000000000000000000000000001',
    '03FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF',
    '047CF27B188D034F7E8A52380304B51AC3C08969E277F21B35A60B48FC47669978',
    '007CF27B188D034F7E8A52380304B51AC3C08969E277F21B35A60B48FC47669978'
  );

// Public key from 66 hex digits, without validation
function PublicKeyFromHex(const aHex: String): TEccPublicKey;
var
  lBytes: TBytes;
begin
  lBytes:=HexStrToBytes(aHex);
  Move(lBytes[0],Result,SizeOf(Result));
end;


// True when EccPublicKeyFromHexa raises EHashUtil for aHex
function PublicKeyFromHexaRaises(const aHex: String): Boolean;
var
  lKey: TEccPublicKey;
begin
  Result:=False;
  try
    EccPublicKeyFromHexa(lKey,aHex);
  except
    on EHashUtil do
      Result:=True;
  end;
end;


var
  // Number of calls to StubScalars
  StubCalls: Integer;
  // Calls of StubScalars that return 2^256-1 and 0 before returning 2
  StubBadCalls: Integer;

// Random scalars: 2^256-1 and 0 alternately for the first StubBadCalls calls, then 2
function StubScalars(out VLI: TVLI): Boolean;
begin
  Inc(StubCalls);
  FillChar(VLI,SizeOf(VLI),0);
  if StubCalls<=StubBadCalls then
  begin
    if Odd(StubCalls) then
      FillChar(VLI,SizeOf(VLI),$FF);
  end else
    VLI[0]:=2;
  Result:=True;
end;


// Random source that always fails
function FailingRandomBytes(aBytes: PByte; aCount: Integer): Boolean;
begin
  Result:=False;
end;


{ TTestECC }

function TTestECC.PrivateKeyFromHex(const aHex: String): TEccPrivateKey;
var
  lBytes: TBytes;
begin
  lBytes:=HexStrToBytes(aHex);
  AssertEquals('Private key length',SizeOf(Result),Length(lBytes));
  Move(lBytes[0],Result,SizeOf(Result));
end;


function TTestECC.PublicKeyToHex(const aKey: TEccPublicKey): String;
begin
  Result:=UpperCase(BytesToHexStr(BytesFromVar(@aKey,SizeOf(aKey))));
end;


procedure TTestECC.CheckSignVerify(const aMsg: String; const aPrivateKey: TEccPrivateKey; const aHash: TEccHash);
var
  lPublicKey: TEccPublicKey;
  lSignature: TEccSignature;
  lOtherHash: TEccHash;
begin
  EccPublicKeyFromPrivateKey(lPublicKey,aPrivateKey);
  AssertTrue(aMsg+': signed',EcdsaSign(aPrivateKey,aHash,lSignature));
  AssertTrue(aMsg+': signature verifies',EcdsaVerify(lPublicKey,aHash,lSignature));
  lOtherHash:=aHash;
  lOtherHash[0]:=lOtherHash[0] xor 1;
  AssertFalse(aMsg+': signature does not verify another hash',EcdsaVerify(lPublicKey,lOtherHash,lSignature));
end;


procedure TTestECC.TestPublicKeyFromPrivateKey;
var
  I: Integer;
  lPublicKey: TEccPublicKey;
begin
  for I:=Low(KeyVectors) to High(KeyVectors) do
    begin
    EccPublicKeyFromPrivateKey(lPublicKey,PrivateKeyFromHex(KeyVectors[I,0]));
    AssertEquals('Public key for private key '+KeyVectors[I,0],KeyVectors[I,1],PublicKeyToHex(lPublicKey));
    end;
end;


procedure TTestECC.TestSignVerifyEdgeKeys;
var
  I, J: Integer;
  lHash: TEccHash;
begin
  for I:=Low(KeyVectors) to High(KeyVectors) do
    // Keys whose public key is not a small multiple of G
    if I in [5,6,8,9,10,11] then
      for J:=1 to 5 do
        begin
        FillChar(lHash,SizeOf(lHash),I*16+J);
        CheckSignVerify('Key '+KeyVectors[I,0]+' hash '+IntToStr(J),PrivateKeyFromHex(KeyVectors[I,0]),lHash);
        end;
end;


procedure TTestECC.TestSignVerifyRandomKeys;
var
  I, J: Integer;
  lPublicKey: TEccPublicKey;
  lPrivateKey: TEccPrivateKey;
  lHash: TEccHash;
begin
  for I:=1 to 10 do
    begin
    AssertTrue('Key '+IntToStr(I)+' made',EccMakeKey(lPublicKey,lPrivateKey));
    for J:=0 to SizeOf(lHash)-1 do
      lHash[J]:=(I*7+J*13) and $FF;
    CheckSignVerify('Random key '+IntToStr(I),lPrivateKey,lHash);
    end;
end;


procedure TTestECC.TestEcdhAgreement;
var
  I: Integer;
  lPublicA, lPublicB: TEccPublicKey;
  lPrivateA, lPrivateB: TEccPrivateKey;
  lSecretA, lSecretB: TEccSecretKey;
begin
  for I:=1 to 5 do
    begin
    AssertTrue('Key A made',EccMakeKey(lPublicA,lPrivateA));
    AssertTrue('Key B made',EccMakeKey(lPublicB,lPrivateB));
    AssertTrue('Secret A computed',EcdhSharedSecret(lPublicB,lPrivateA,lSecretA));
    AssertTrue('Secret B computed',EcdhSharedSecret(lPublicA,lPrivateB,lSecretB));
    AssertTrue('Both sides compute the same secret',CompareMem(@lSecretA,@lSecretB,SizeOf(lSecretA)));
    end;
  lPrivateA:=PrivateKeyFromHex(KeyVectors[0,0]);
  EccPublicKeyFromPrivateKey(lPublicA,lPrivateA);
  AssertTrue('Secret with private key 1 computed',EcdhSharedSecret(lPublicB,lPrivateA,lSecretA));
  AssertEquals('Secret with private key 1 is the x of the other public key',
               Copy(PublicKeyToHex(lPublicB),3,64),UpperCase(BytesToHexStr(BytesFromVar(@lSecretA,SizeOf(lSecretA)))));
end;


procedure TTestECC.TestNoRandomBytes;
var
  lSaved: TGetRandomBytes;
  lPublicKey, lOtherPublicKey: TEccPublicKey;
  lPrivateKey: TEccPrivateKey;
  lHash: TEccHash;
  lSignature: TEccSignature;
  lSecret: TEccSecretKey;
  I: Integer;
begin
  AssertTrue('Other key made',EccMakeKey(lOtherPublicKey,lPrivateKey));
  FillChar(lHash,SizeOf(lHash),1);
  lSaved:=GetRandomBytes;
  try
    for I:=0 to 1 do
      begin
      if I=0 then
        GetRandomBytes:=@FailingRandomBytes
      else
        GetRandomBytes:=nil;
      AssertFalse('No key without random bytes ('+IntToStr(I)+')',EccMakeKey(lPublicKey,lPrivateKey));
      AssertFalse('No signature without random bytes ('+IntToStr(I)+')',EcdsaSign(lPrivateKey,lHash,lSignature));
      AssertFalse('No shared secret without random bytes ('+IntToStr(I)+')',EcdhSharedSecret(lOtherPublicKey,lPrivateKey,lSecret));
      end;
  finally
    GetRandomBytes:=lSaved;
  end;
end;


procedure TTestECC.TestInvalidPublicKeys;
var
  I: Integer;
  lPublicKey, lInvalidKey: TEccPublicKey;
  lPrivateKey: TEccPrivateKey;
  lHash: TEccHash;
  lSignature: TEccSignature;
  lSecret: TEccSecretKey;
  lExport: array[0..2*ECC_BYTES] of Byte;
begin
  AssertTrue('Key made',EccMakeKey(lPublicKey,lPrivateKey));
  FillChar(lHash,SizeOf(lHash),3);
  AssertTrue('Signed',EcdsaSign(lPrivateKey,lHash,lSignature));
  AssertTrue('Valid key exported',EccExportX963(PublicKeyFromHex(Point2GCompressed),@lExport,SizeOf(lExport)));
  AssertEquals('Exported point',Point2GUncompressed,UpperCase(BytesToHexStr(BytesFromVar(@lExport,SizeOf(lExport)))));
  AssertFalse('Valid compressed key accepted by EccPublicKeyFromHexa',PublicKeyFromHexaRaises(Point2GCompressed));
  for I:=Low(InvalidPublicKeys) to High(InvalidPublicKeys) do
    begin
    lInvalidKey:=PublicKeyFromHex(InvalidPublicKeys[I]);
    AssertFalse('No shared secret with '+InvalidPublicKeys[I],EcdhSharedSecret(lInvalidKey,lPrivateKey,lSecret));
    AssertFalse('No verification with '+InvalidPublicKeys[I],EcdsaVerify(lInvalidKey,lHash,lSignature));
    AssertFalse('No export of '+InvalidPublicKeys[I],EccExportX963(lInvalidKey,@lExport,SizeOf(lExport)));
    AssertTrue('EccPublicKeyFromHexa rejects '+InvalidPublicKeys[I],PublicKeyFromHexaRaises(InvalidPublicKeys[I]));
    end;
end;


procedure TTestECC.TestImportX963;
var
  lPoint, lBadPoint: TBytes;
  lPublicKey: TEccPublicKey;
begin
  lPoint:=HexStrToBytes(Point2GUncompressed);
  AssertTrue('Valid point imported',EccImportX963(@lPoint[0],Length(lPoint),lPublicKey));
  AssertEquals('Imported key',Point2GCompressed,PublicKeyToHex(lPublicKey));
  AssertTrue('Valid point imported from x and y',EccImportX963FromXY(@lPoint[1],ECC_BYTES,@lPoint[1+ECC_BYTES],ECC_BYTES,lPublicKey));
  AssertEquals('Key imported from x and y',Point2GCompressed,PublicKeyToHex(lPublicKey));
  AssertEquals('EccPublicKeyFromHexa with an uncompressed point',Point2GCompressed,PublicKeyToHex(EccPublicKeyFromHexa(Point2GUncompressed)));
  AssertEquals('EccPublicKeyFromHexa with x and y',Point2GCompressed,PublicKeyToHex(EccPublicKeyFromHexa(Copy(Point2GUncompressed,3,128))));
  lBadPoint:=Copy(lPoint);
  lBadPoint[Length(lBadPoint)-1]:=lBadPoint[Length(lBadPoint)-1] xor 2;
  AssertFalse('Point with a wrong y rejected',EccImportX963(@lBadPoint[0],Length(lBadPoint),lPublicKey));
  AssertFalse('Point with a wrong y rejected from x and y',EccImportX963FromXY(@lBadPoint[1],ECC_BYTES,@lBadPoint[1+ECC_BYTES],ECC_BYTES,lPublicKey));
  AssertTrue('EccPublicKeyFromHexa rejects a wrong y',PublicKeyFromHexaRaises(BytesToHexStr(lBadPoint)));
end;


procedure TTestECC.TestRandomScalarRejection;
var
  lSaved: TEccGetRandomNumber;
  lPublicKey: TEccPublicKey;
  lPrivateKey: TEccPrivateKey;
begin
  lSaved:=EccGetRandomNumber;
  try
    EccGetRandomNumber:=@StubScalars;
    StubCalls:=0;
    StubBadCalls:=2;
    AssertTrue('Key made after 2^256-1 and 0 were drawn',EccMakeKey(lPublicKey,lPrivateKey));
    AssertEquals('Private key is the first value in range',
                 '0000000000000000000000000000000000000000000000000000000000000002',
                 UpperCase(BytesToHexStr(BytesFromVar(@lPrivateKey,SizeOf(lPrivateKey)))));
    AssertEquals('Random numbers drawn',3,StubCalls);
    StubCalls:=0;
    StubBadCalls:=MaxInt;
    AssertFalse('No key when every value is out of range',EccMakeKey(lPublicKey,lPrivateKey));
  finally
    EccGetRandomNumber:=lSaved;
  end;
end;


initialization
  RegisterTest(TTestECC);
end.
