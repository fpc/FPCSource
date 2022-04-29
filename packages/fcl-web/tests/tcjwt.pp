unit tcjwt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, DateUtils, fpjwt, fpjwarsa;

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
    function CreateUnsignedInput(JOSEAlg, ClaimsIssuer: string): string;
    Property JWT : TJWT Read FJWT;
    Property Key : TJWTKey Read FKey;
    procedure TestVerifyRSAPem(SignerClass: TJWTSignerRSAClass); virtual;
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
    procedure TestVerifyRS256Pem;
    procedure TestVerifyRS384Pem;
    procedure TestVerifyRS512Pem;
  end;

implementation

uses
  basenenc, fpsha256, fpjwasha256, fpsha512, fpjwasha512, fpjwasha384, fpjwaes256, fpecc, fppem;

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
  if not TSHA256.HMAC(FKey.AsPointer,FKey.Length,PByte(B),Length(B),aDigest{%H-}) then
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
  FKey.AsString:='your-256-bit-secret';
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
  if not TSHA512.HMAC(FKey.AsPointer,FKey.Length,PByte(B),Length(B),aDigest{%H-}) then
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
  writeln('AAA1 TTestJWT.TestVerifyES256Pem ');
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

procedure TTestJWT.TestVerifyRS256Pem;
begin
  TestVerifyRSAPem(TJWTSignerRS256);
end;

procedure TTestJWT.TestVerifyRS384Pem;
begin
  TestVerifyRSAPem(TJWTSignerRS384);
end;

procedure TTestJWT.TestVerifyRS512Pem;
begin
  TestVerifyRSAPem(TJWTSignerRS512);
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

function TTestJWT.CreateUnsignedInput(JOSEAlg, ClaimsIssuer: string): string;
var
  IssuedAt, Expire: Int64;
  Header, Claims: String;
begin
  IssuedAt:=DateTimeToUnix(Now-1);
  Expire:=IssuedAt+1000000;
  Header:='{"typ":"JWT","alg":"'+JOSEAlg+'"}';
  Claims:='{"iat":'+IntToStr(IssuedAt)+',"exp":'+IntToStr(Expire)+',"iss":"'+ClaimsIssuer+'"}';
  Result:=Base64URL.Encode(Header,false)+'.'+Base64URL.Encode(Claims,false);
end;

procedure TTestJWT.TestVerifyRSAPem(SignerClass: TJWTSignerRSAClass);
const
  // generated with
  //   openssl genrsa -out private.pem 2048
  APrivateKeyPem =
    '-----BEGIN RSA PRIVATE KEY-----'#10+
    'MIIEpQIBAAKCAQEAvkRfGW8psCZ3G4+hBA6W/CR/FHhBLB3k3QLypamPbRFlFBxL'#10+
    'tOK2NblBybY22vUiMLZbb5x8OoOj/IhOrJAlTqhtbTWLy/0K3qbG09vLm8V40kEK'#10+
    '8/p0STrp3UmsxHNkccj9MRSKk7pOyEvxSCY6K5JGK1VTsMuDCS7DCYk6Vqr3zjX7'#10+
    'qedF1PVM+Z5t0B+f//kt3oBETNlic4IooEpG/PN2GUQ0oZpa16DDtfgGu7wT3X3Q'#10+
    'EZFWLJYQTvGc82NpachBIUvqNdIt1npbK38MXU4IPHVrSN/HdK2nQPSMLdKnTV+E'#10+
    'h/HcxpfjBjarg+VjgDqlmqJ9bkosOVn35vsg8wIDAQABAoIBAQCZxVwujB7fFFdS'#10+
    '2QPC6Z+w7DYgbwgNBaP/0vAUXzNhbJuKY0v0Rv4H8U9wHGm9EDyvrdG8JHZqPBX+'#10+
    'dJNQ97aPGaRGjO4M0NdGFve+JXcqz6/UDWkywYnV3V1A0NhmdPQK2et3DSjqN7qQ'#10+
    'OoAoVWzR5gf74Zwf2Hpwo3BRdqzFeUYVDOH7e7q1SOf2QeU54kVUG21saJR0wsyH'#10+
    'oSX8BMU2kmg1Un8ET4FM5xEwhdTZzgFTJVZhc6EfOKVbQt6cKmW3aER3c9vR7M3l'#10+
    'N6Oq73vqrfmy+jFMwz1SoPObQQ7UAnr7YUowaX0AzxHpYm/afyVm+Toym0qWGrrY'#10+
    'MY/l+vNRAoGBAOsi72pJj30ApfVbSpx8/8QIpweLbEgAD+Ssd41Kgc4O/N7azB61'#10+
    'RjzSOs1BGhpAZNU6muAAbucm9EssfG5WTAjIM2W2LVuZXXEVXqEGkIymPz9NGugf'#10+
    'JaCWLaoibmwHkKa+ZV9kDwasmx/VkbAfAbRWaz49ejdrMmkpCW77lYjHAoGBAM8m'#10+
    'PVJWvFhQrB21xQGSWKd5iSUn2V92gICeDoORqfVtt/UPOaDT915KzXPh4bJeOwg6'#10+
    'Kkx5wX6UwaNSRH39loDSY1rsBYioV8bxW0BpBvEJG7KXRbBvxzr0+TJkCHgmGMns'#10+
    'dhePYUcriCaqpQi1yzf201oLTZ6PlJxkmHQobXJ1AoGBAIgWPg576InmWCa64WHU'#10+
    'joq8nz8kmFTLhGdK0h56IspJrlyksUKMk8wbuGCW7y6GWlV2h7BhT86Eoxrm8lVB'#10+
    'qNvkUqrpVzMOfiA2x//WNs7QYQaX75ysejCI+oDfUJ1Be5yl0TH2TSQFvfoctycB'#10+
    'qxDee08YcaWlaxWl5InRHeh9AoGABm3XZWDPw6XtUZa8oIncOoZpHUAZXP8eid9d'#10+
    '7/NrZPScyvxH+5fYi5Kiwb/280Q9bMnxWiJFQRp40ArTmV1veFwPPVkp6s3eu4vu'#10+
    'GxenYX+43lgXj5xIgKntugSkxqXYCxxNpfmLOVw+g4S0Torl3bzJXngPVqZ6JEhy'#10+
    '+tfuXakCgYEA19/JCD/5pVPJtwyDDAYnUUESK+JfBPq1cTbsxcOq01mp5ntsqR4y'#10+
    'dtOAmxMASvsqud3XIM5fO5m3Jpl1phiGhCw4nvVLcYzVWxYY+oWoeCSyECgu5tmT'#10+
    'Fo8vn4EEXCkEAA2YPiEuVcrcYsWkLivCTC19lJDfUNMmpwSdiGz/tDU='#10+
    '-----END RSA PRIVATE KEY-----'#10;
var
  aInput: String;
  Signer: TJWTSignerRSA;
begin
  // header
  jwt.JOSE.alg:=SignerClass.AlgorithmName;

  // claims
  jwt.Claims.exp:=DateTimeToUnix(Now+10);
  jwt.Claims.iss:='FPC JWT';

  // load private key from pem
  FKey.AsBytes:=PemToDER(APrivateKeyPem,_BEGIN_RSA_PRIVATE_KEY,_END_RSA_PRIVATE_KEY);

  Signer:=TJWTSignerRSA(SignerClass.Create);
  try
    aInput:=Signer.AppendSignature(JWT,Key);
  finally
    Signer.Free;
  end;

  FVerifyResult:=TMyJWT.ValidateJWT(aInput,FKey);
  AssertNotNull('Have result',FVerifyResult);
  AssertEquals('Correct class',TMyJWT,FVerifyResult.ClassType);
  AssertNotNull('Have result.claims',FVerifyResult.Claims);
  AssertEquals('Correct claims class',TMyClaims,FVerifyResult.Claims.ClassType);
  AssertEquals('Have correct algorithm',SignerClass.AlgorithmName,FVerifyResult.JOSE.Alg);
  AssertEquals('Have correct typ','JWT',FVerifyResult.JOSE.typ);
  AssertEquals('Have correct sub','1234567890',FVerifyResult.Claims.sub);
  AssertEquals('Have correct name','John Doe',(TMyJWT(FVerifyResult).Claims as TMyClaims).Name);
  AssertEquals('Have correct admin',False,(TMyJWT(FVerifyResult).Claims as TMyClaims).Admin);
end;

initialization
  RegisterTest(TTestJWT);
end.

