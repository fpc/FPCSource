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
  end;

implementation

uses basenenc, sha256, fpjwasha256, sha512, fpjwasha512, fpjwasha384;

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
  Writeln('Signed: ',P1+'.'+P2+'.'+Sign);
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

