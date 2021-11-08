unit tcjwt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjwt;

type


  { TMyClaims }

  TMyClaims = Class(TClaims)
  private
    FAdmin: Boolean;
    FName: string;
  Published
    Property Name : string Read FName Write FName;
    Property admin : Boolean Read FAdmin Write FAdmin;
  end;

  { TMyJWT }

  TMyJWT = Class(TJWT)
    Function CreateClaims : TClaims; override;
  end;

  { TTestJWT }
  TTestJWT= class(TTestCase)
  private
    FJWT: TJWT;
    FKey : TJWTKey;
    FVerifyResult : TJWT;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    Property JWT : TJWT Read FJWT;
    Property Key : TJWTKey Read FKey;
  published
    procedure TestSignNone;
    procedure TestVerifyNone;
    procedure TestSignSHA256;
    procedure TestVerifySHA256;
    procedure TestSignSHA512;
    procedure TestVerifySHA512;
    procedure TestSignSHA384;
    procedure TestVerifySHA384;
    procedure TestVerifyES256;
    procedure TestVerifyES256Pem;
  end;

implementation

uses
  basenenc, sha256, fpjwasha256, sha512, fpjwasha512, fpjwasha384, fpjwaes256, ecc, pem;

{ TMyJWT }

function TMyJWT.CreateClaims: TClaims;
begin
  Result:=TMyClaims.Create;
end;

procedure TTestJWT.TestSignNone;

Var
  P1,P2 : String;

begin
  P1:=FJWT.JOSE.AsEncodedString;
  P2:=FJWT.Claims.AsEncodedString;
  AssertEquals('Signed with none',P1+'.'+P2+'.',FJWT.Sign(TJWTKey.Empty));
end;

procedure TTestJWT.TestVerifyNone;

Var
  aJWT : String;

begin
  aJWT:=FJWT.AsEncodedString;
  FVerifyResult:=TJWT.ValidateJWT(aJWT,TJWTKey.Empty,TMyJWT);
  AssertNotNull('Have result',FVerifyResult);
  AssertEquals('Correct class',TMyJWT,FVerifyResult.ClassType);
end;

procedure TTestJWT.TestSignSHA256;

Var
  Sign,P1,P2 : UTF8String;
  aDigest : TSHA256Digest;
  B : TBytes;

begin
  FJWT.JOSE.alg:='HS256';
//  Writeln('JOSE: ',FJWT.JOSE.AsString);
//  Writeln('Claims: ',FJWT.Claims.AsString);
  P1:=FJWT.JOSE.AsEncodedString;
  P2:=FJWT.Claims.AsEncodedString;
  B:=TEncoding.UTF8.GetAnsiBytes(P1+'.'+P2);
  if not TSHA256.HMAC(FKey.AsPointer,FKey.Length,PByte(B),Length(B),aDigest) then
    Fail('Could not HMAC');
  Sign:=Base64URL.Encode(@aDigest[0],Length(aDigest),False);
//  Writeln('Signed: ',P1+'.'+P2+'.'+Sign);
  AssertEquals('Signed with SHA256',P1+'.'+P2+'.'+Sign,FJWT.Sign(FKey));
end;

procedure TTestJWT.TestVerifySHA256;

Const
  JWTText ='eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.'+
           'eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.'+
           'SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c';


begin
  FKey:=TJWTKey.Create('your-256-bit-secret');
  FVerifyResult:=TJWT.ValidateJWT(JWTText,FKey);
  AssertNotNull('Have result',FVerifyResult);
  AssertEquals('Have correct algorithm','HS256',FVerifyResult.JOSE.Alg);
  AssertEquals('Have correct typ','JWT',FVerifyResult.JOSE.typ);
  AssertEquals('Have correct sub','1234567890',FVerifyResult.Claims.sub);
end;

procedure TTestJWT.TestSignSHA512;
Var
  Sign,P1,P2 : UTF8String;
  aDigest : TSHA512Digest;
  B : TBytes;

begin
  FJWT.JOSE.alg:='HS512';
//  Writeln('JOSE: ',FJWT.JOSE.AsString);
//  Writeln('Claims: ',FJWT.Claims.AsString);
  P1:=FJWT.JOSE.AsEncodedString;
  P2:=FJWT.Claims.AsEncodedString;
  B:=TEncoding.UTF8.GetAnsiBytes(P1+'.'+P2);
  if not TSHA512.HMAC(FKey.AsPointer,FKey.Length,PByte(B),Length(B),aDigest) then
    Fail('Could not HMAC');
  Sign:=Base64URL.Encode(@aDigest[0],Length(aDigest),False);
//  Writeln('Signed: ',P1+'.'+P2+'.'+Sign);
  AssertEquals('Signed with SHA512',P1+'.'+P2+'.'+Sign,FJWT.Sign(FKey));
end;

procedure TTestJWT.TestVerifySHA512;

Const
  JWTText = 'eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.'+
            'eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWUsImlhdCI6MTUxNjIzOTAyMn0.'+
            'FEBOl5fjgnPe4gcc5ElXrHDl0jWsshiJ9rS0hlehItc-PKQEzwRKbhcz69V8kwRCUM2rDtuwaXK6DJfO1VOZdw';

begin
  FKey:=TJWTKey.Create('mysecretkey');
  FVerifyResult:=TMyJWT.ValidateJWT(JWTText,FKey);
  AssertNotNull('Have result',FVerifyResult);
  AssertEquals('Correct class',TMyJWT,FVerifyResult.ClassType);
  AssertNotNull('Have result.claims',FVerifyResult.Claims);
  AssertEquals('Correct claims class',TMyClaims,FVerifyResult.Claims.ClassType);
  AssertEquals('Have correct algorithm','HS512',FVerifyResult.JOSE.Alg);
  AssertEquals('Have correct typ','JWT',FVerifyResult.JOSE.typ);
  AssertEquals('Have correct sub','1234567890',FVerifyResult.Claims.sub);
  AssertEquals('Have correct name','John Doe',(TMyJWT(FVerifyResult).Claims as TMyClaims).Name);
  AssertEquals('Have correct admin',true,(TMyJWT(FVerifyResult).Claims as TMyClaims).Admin);
end;

procedure TTestJWT.TestSignSHA384;
Var
  Sign,P1,P2 : UTF8String;
  aDigest : TSHA384Digest;
  B : TBytes;

begin
  FJWT.JOSE.alg:='HS384';
//  Writeln('JOSE: ',FJWT.JOSE.AsString);
//  Writeln('Claims: ',FJWT.Claims.AsString);
  P1:=FJWT.JOSE.AsEncodedString;
  P2:=FJWT.Claims.AsEncodedString;
  B:=TEncoding.UTF8.GetAnsiBytes(P1+'.'+P2);
  if not TSHA384.HMAC(FKey.AsPointer,FKey.Length,PByte(B),Length(B),aDigest) then
    Fail('Could not HMAC');
  Sign:=Base64URL.Encode(@aDigest[0],Length(aDigest),False);
  // Writeln('Signed: ',P1+'.'+P2+'.'+Sign);
  AssertEquals('Signed with SHA384',P1+'.'+P2+'.'+Sign,FJWT.Sign(FKey));
end;

procedure TTestJWT.TestVerifySHA384;

Const
  JWTText =
     'eyJhbGciOiJIUzM4NCIsInR5cCI6IkpXVCJ9.'+
     'eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWUsImlhdCI6MTUxNjIzOTAyMn0.'+
     '8XBKpuFoIEyTxqiP7Rw32VkkxSPGrujBw2ZiKgcX5ZgjH3M8OmTWfYeRDAR6NRVB';

begin
  FKey:=TJWTKey.Create('mysecretkey');
  FVerifyResult:=TMyJWT.ValidateJWT(JWTText,FKey);
  AssertNotNull('Have result',FVerifyResult);
  AssertEquals('Correct class',TMyJWT,FVerifyResult.ClassType);
  AssertNotNull('Have result.claims',FVerifyResult.Claims);
  AssertEquals('Correct claims class',TMyClaims,FVerifyResult.Claims.ClassType);
  AssertEquals('Have correct algorithm','HS384',FVerifyResult.JOSE.Alg);
  AssertEquals('Have correct typ','JWT',FVerifyResult.JOSE.typ);
  AssertEquals('Have correct sub','1234567890',FVerifyResult.Claims.sub);
  AssertEquals('Have correct name','John Doe',(TMyJWT(FVerifyResult).Claims as TMyClaims).Name);
  AssertEquals('Have correct admin',true,(TMyJWT(FVerifyResult).Claims as TMyClaims).Admin);
end;

procedure TTestJWT.TestVerifyES256;
Const
  // from JWT.IO
  aJWT =
     'eyJhbGciOiJFUzI1NiIsInR5cCI6IkpXVCJ9.'+
     'eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWUsImlhdCI6MTUxNjIzOTAyMn0.'+
     'tyh-VfuzIxCyGYDlkBA7DfyjrqmSHu6pQ2hoZuFqUSLPNY2N0mpHb3nk5K17HWP_3cYHBw7AhHale5wky6-sVA';

Var
  aPrivateKey2: TEccPrivateKey = ($7a,$f6,$73,$2f,$58,$1d,$00,$5a,$fc,$f2,$16,$f6,$38,$5f,$f6,
                $37,$10,$29,$24,$2c,$c6,$08,$40,$dd,$7d,$2a,$7a,$55,$03,$b7,
                $d2,$1c);


begin
  FKey:=TJWTKey.Create(@aPrivateKey2,SizeOf(TEccPrivateKey));
  FVerifyResult:=TMyJWT.ValidateJWT(aJWT,FKey);
  AssertNotNull('Have result',FVerifyResult);
  AssertEquals('Correct class',TMyJWT,FVerifyResult.ClassType);
  AssertNotNull('Have result.claims',FVerifyResult.Claims);
  AssertEquals('Correct claims class',TMyClaims,FVerifyResult.Claims.ClassType);
  AssertEquals('Have correct algorithm','ES256',FVerifyResult.JOSE.Alg);
  AssertEquals('Have correct typ','JWT',FVerifyResult.JOSE.typ);
  AssertEquals('Have correct sub','1234567890',FVerifyResult.Claims.sub);
  AssertEquals('Have correct name','John Doe',(TMyJWT(FVerifyResult).Claims as TMyClaims).Name);
  AssertEquals('Have correct admin',true,(TMyJWT(FVerifyResult).Claims as TMyClaims).Admin);

end;

procedure TTestJWT.TestVerifyES256Pem;

Const
  aInput =
    'eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.' +
    'eyJpYXQiOjE1MTYyMzkwMjIsImV4cCI6MTUxNjI0OTAyMiwiaXNzIjoiRGVscGhpIEpPU0UgYW5kIEpXVCBMaWJyYXJ5In0.'+
    '4QDMKAvHwb6pA5fN0oQjlzuKmPIlNpmIQ8vPH7zy4fjZdtcPVJMtfiVhztwQldQL9A5yzBKI8q2puVygm-2Adw';

// Private key in PEM format
Const APrivateKeyPem =
    '-----BEGIN EC PRIVATE KEY-----'+ #10+
    'MHcCAQEEIFzS3/5bCnrlpa4902/zkYzURF6E2D8pazgnJu4smhpQoAoGCCqGSM49'+ #10+
    'AwEHoUQDQgAEqTjyg2z65i+zbyUZW8BQ+K87DNsICRaEH7Fy7Rm3MseXy9ItSCQU'+ #10+
    'VeJbtO6kYUA00mx7bKoC1sx5sbtFExnYPQ=='+ #10+
    '-----END EC PRIVATE KEY-----';


Var
  S : TStringStream;
  aPrivateKey : TEccPrivateKey;
  aPublicKey : TEccPublicKey;
  X,Y : AnsiString;

begin
  S:=TStringStream.Create(aPrivateKeyPem);
  try
    PemLoadECDSA(S,aPrivateKey,aPublicKey,X,Y);
  finally
    S.Free;
  end;
  FKey:=TJWTKey.Create(@aPrivateKey,SizeOf(TEccPrivateKey));
  FVerifyResult:=TMyJWT.ValidateJWT(aInput,FKey);
  AssertNotNull('Have result',FVerifyResult);
  AssertEquals('Correct class',TMyJWT,FVerifyResult.ClassType);
  AssertNotNull('Have result.claims',FVerifyResult.Claims);
  AssertEquals('Correct claims class',TMyClaims,FVerifyResult.Claims.ClassType);
  AssertEquals('Have correct algorithm','ES256',FVerifyResult.JOSE.Alg);
  AssertEquals('Have correct typ','JWT',FVerifyResult.JOSE.typ);
  AssertEquals('Have correct sub','',FVerifyResult.Claims.sub);
  AssertEquals('Have correct name','',(TMyJWT(FVerifyResult).Claims as TMyClaims).Name);
  AssertEquals('Have correct admin',False,(TMyJWT(FVerifyResult).Claims as TMyClaims).Admin);
end;

procedure TTestJWT.SetUp;


begin
  Inherited;
  FKey:=TJWTKey.Create('mysecretkey');
  FJWT:=TMyJWT.Create;
  FJWT.JOSE.alg:='none';
  FJWT.JOSE.typ:='JWT';
  FJWT.Claims.sub:='1234567890';
  FJWT.Claims.iat:=1516239022;
  (FJWT.Claims as TMyClaims).Name:='John Doe';
end;

procedure TTestJWT.TearDown;
begin
  FreeAndNil(FJWT);
  FreeAndNil(FVerifyResult);
  Inherited;
end;

initialization
  RegisterTest(TTestJWT);
end.

