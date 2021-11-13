unit utestsha256;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, fpecc, fpsha256, fphashutils;

type

  { TTestSHA256 }

  TTestSHA256 = class(TTestCase)
  Public
    Procedure TestHexString(Const aString,aDigest : String);
    Procedure TestBase64String(Const aString,aDigest : String);
    Procedure TestHMACString(Const aString,aKey,aDigest : String);
  published
    procedure TestEmpty;
    procedure TestSmallString;
    procedure TestEmptyBase64;
    procedure TestSmallBase64;
    procedure TestSmallHMAC;
    procedure TestHMACStream;
  end;

  { TTestECDSASHA256 }

  TTestECDSASHA256 = Class(TTestCase)
    // base64url encoded
    Const
    aInput =
      'eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.' +
      'eyJpYXQiOjE1MTYyMzkwMjIsImV4cCI6MTUxNjI0OTAyMiwiaXNzIjoiRGVscGhpIEpPU0UgYW5kIEpXVCBMaWJyYXJ5In0';
    aOutput =
      '4QDMKAvHwb6pA5fN0oQjlzuKmPIlNpmIQ8vPH7zy4fjZdtcPVJMtfiVhztwQldQL9A5yzBKI8q2puVygm-2Adw';
    // Private key in PEM format
    Const APrivateKeyPem =
        '-----BEGIN EC PRIVATE KEY-----'+ #10+
        'MHcCAQEEIFzS3/5bCnrlpa4902/zkYzURF6E2D8pazgnJu4smhpQoAoGCCqGSM49'+ #10+
        'AwEHoUQDQgAEqTjyg2z65i+zbyUZW8BQ+K87DNsICRaEH7Fy7Rm3MseXy9ItSCQU'+ #10+
        'VeJbtO6kYUA00mx7bKoC1sx5sbtFExnYPQ=='+ #10+
        '-----END EC PRIVATE KEY-----';
   Published
     Procedure TestSignVerify;
     Procedure TestVerify;
     Procedure TestVerifyFromKey;
   end;

implementation

uses
   fppem, fpecdsa,  basenenc;

{ TTestECDSASHA256 }

procedure TTestECDSASHA256.TestSignVerify;

var
  aPrivateKey : TEccPrivateKey;
  aPublicKey : TEccPublicKey;
  aSignature : TECCSignature;
  X,Y : Ansistring;
  S : TStringStream;

begin
  S:=TStringStream.Create(APrivateKeyPem);
  try
    AssertTrue('Loaded key',PemLoadECDSA(S,aPrivateKey,aPublicKey,X,Y));
    AssertTrue('Encrypted',TECDSA.SignSHA256(aInput,aPrivateKey,aSignature));
    EccPublicKeyFromPrivateKey(aPublicKey,aPrivateKey);
    AssertTrue('Verified our own',TECDSA.SignSHA256(aInput,aPrivateKey,aSignature));
  finally
    S.Free;
  end;
end;

procedure TTestECDSASHA256.TestVerify;

var
  aPrivateKey : TEccPrivateKey;
  aPublicKey : TEccPublicKey;
  aSignature : TECCSignature;
  X,Y : Ansistring;
  S : TStringStream;

begin
  S:=TStringStream.Create(APrivateKeyPem);
  try
    AssertTrue('Loaded key',PemLoadECDSA(S,aPrivateKey,aPublicKey,X,Y));
    AssertTrue('Encrypted',TECDSA.SignSHA256(aInput,aPrivateKey,aSignature));
    // Now verify with result of someone else (random elements)
    BytesToVar(Base64URL.Decode(aOutput),aSignature,SizeOf(aSignature));
    AssertTrue('Verified other',TECDSA.VerifySHA256(aInput,aPrivateKey,aSignature));
  finally
    S.Free;
  end;

end;

procedure TTestECDSASHA256.TestVerifyFromKey;

Const
  // from JWT.IO
  aInput2 = 'eyJhbGciOiJFUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWUsImlhdCI6MTUxNjIzOTAyMn0';
  aOutput2 = 'tyh-VfuzIxCyGYDlkBA7DfyjrqmSHu6pQ2hoZuFqUSLPNY2N0mpHb3nk5K17HWP_3cYHBw7AhHale5wky6-sVA';

  aPrivateKey2: TEccPrivateKey = ($7a,$f6,$73,$2f,$58,$1d,$00,$5a,$fc,$f2,$16,$f6,$38,$5f,$f6,
                $37,$10,$29,$24,$2c,$c6,$08,$40,$dd,$7d,$2a,$7a,$55,$03,$b7,
                $d2,$1c);

var
  aSignature : TECCSignature;

begin
  BytesToVar(Base64URL.Decode(aOutput2),aSignature,SizeOf(aSignature));
  AssertTrue('Verified other',TECDSA.VerifySHA256(aInput2,aPrivateKey2,aSignature));
end;

{ TTestSHA256 }

Procedure TTestSHA256.TestHexString(Const aString,aDigest : String);

var
  Digest : AnsiString;
  S : TBytes;

begin
  S:=[];
  Digest:='';
  S:=TEncoding.UTF8.GetAnsiBytes(aString);
  TSHA256.DigestHexa(S, Digest);
  AssertEquals('Correct hex digest',aDigest, Digest);
end;

procedure TTestSHA256.TestBase64String(const aString, aDigest: String);
var
  Digest : AnsiString;
  S : TBytes;

begin
  S:=TEncoding.UTF8.GetAnsiBytes(aString);
  Digest:='';
  TSHA256.DigestBase64(S,False,Digest);
  AssertEquals('Correct base64 digest',aDigest, Digest);
end;

procedure TTestSHA256.TestHMACString(const aString, aKey, aDigest: String);
var
  Digest : AnsiString;
  S,K : TBytes;

begin
  S:=TEncoding.UTF8.GetAnsiBytes(aString);
  K:=TEncoding.UTF8.GetAnsiBytes(aKey);
  TSHA256.HMACHexa(K,S,Digest);
  AssertEquals('Correct digest',aDigest, Digest);
end;

procedure TTestSHA256.TestEmpty;


begin
  TestHexString('','E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855');
end;

procedure TTestSHA256.TestSmallString;

begin
  TestHexString('abc','BA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD');
end;

procedure TTestSHA256.TestEmptyBase64;
begin
  TestBase64String('','47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU');
end;

procedure TTestSHA256.TestSmallBase64;

begin
  TestBase64String('abc','ungWv48Bz+pBQUDeXa4iI7ADYaOWF3qctBD/YfIAFa0');
end;

procedure TTestSHA256.TestSmallHMAC;
begin
  TestHMACString('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ',
                 'Secret key' ,
                 '6AE3261635F57BF68B6E3DF9C06ED14D3FA793F1B7BE55BC3429895B09F52F77');
end;

procedure TTestSHA256.TestHMACStream;

Var
  S : TStringStream;

begin
  S:=TStringStream.Create('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ');
  try
    AssertEquals('Correct hash','3964294B664613798D1A477EB8AD02118B48D0C5738C427613202F2ED123B5F1',TSHA256.StreamHexa(S));
  finally
    S.Free;
  end;
end;

initialization
  RegisterTests([TTestSHA256,TTestECDSASHA256]);
end.

