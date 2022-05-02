unit tcjwt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, DateUtils, fprsa, fphashutils,
  fpjwt, fpjwarsa;

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
    procedure GetTestPEM(out aPrivateKeyPEM, aPublicKeyPEM: string);
    procedure TestVerifyRSAPem(SignerClass: TJWTSignerClass); virtual;
  published
    procedure TestSignNone;
    procedure TestVerifyNone;

    // SHA
    procedure TestSignSHA256;
    procedure TestVerifySHA256;
    procedure TestSignSHA512;
    procedure TestVerifySHA512;
    procedure TestSignSHA384;
    procedure TestVerifySHA384;

    // ES
    procedure TestVerifyES256;
    procedure TestVerifyES256Pem;

    // RSA
    procedure TestVerifyRS256Pem;
    procedure TestVerifyRS384Pem;
    procedure TestVerifyRS512Pem;
    procedure TestVerifyRS256_rfc7515;
    procedure TestI2OSP;
    procedure TestMGF1SHA1;
    procedure TestMGF1SHA256;
    procedure TestVerifyPS256;
    procedure TestVerifyPS256Pem;
    procedure TestVerifyPS384Pem;
    procedure TestVerifyPS512Pem;
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

procedure TTestJWT.TestVerifyRS256_rfc7515;
const
  // values from RFC 7515
  HeaderJSON = '{"alg":"RS256"}';
  HeaderExpected = 'eyJhbGciOiJSUzI1NiJ9';

  PayloadJSON = '{"iss":"joe",'#13#10+
                ' "exp":1300819380,'#13#10+
                ' "http://example.com/is_root":true}';
  PayloadExpected = 'eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ';

  SignInputExpected: TBytes =
    (101, 121, 74, 104, 98, 71, 99, 105, 79, 105, 74, 83, 85, 122, 73,
     49, 78, 105, 74, 57, 46, 101, 121, 74, 112, 99, 51, 77, 105, 79, 105,
     74, 113, 98, 50, 85, 105, 76, 65, 48, 75, 73, 67, 74, 108, 101, 72,
     65, 105, 79, 106, 69, 122, 77, 68, 65, 52, 77, 84, 107, 122, 79, 68,
     65, 115, 68, 81, 111, 103, 73, 109, 104, 48, 100, 72, 65, 54, 76,
     121, 57, 108, 101, 71, 70, 116, 99, 71, 120, 108, 76, 109, 78, 118,
     98, 83, 57, 112, 99, 49, 57, 121, 98, 50, 57, 48, 73, 106, 112, 48,
     99, 110, 86, 108, 102, 81);

  RSA_n = 'ofgWCuLjybRlzo0tZWJjNiuSfb4p4fAkd_wWJcyQoTbji9k0l8W26mPddx'+
          'HmfHQp-Vaw-4qPCJrcS2mJPMEzP1Pt0Bm4d4QlL-yRT-SFd2lZS-pCgNMs'+
          'D1W_YpRPEwOWvG6b32690r2jZ47soMZo9wGzjb_7OMg0LOL-bSf63kpaSH'+
          'SXndS5z5rexMdbBYUsLA9e-KXBdQOS-UTo7WTBEMa2R2CapHg665xsmtdV'+
          'MTBQY4uDZlxvb3qCo5ZwKh9kG4LT6_I5IhlJH7aGhyxXFvUK-DWNmoudF8'+
          'NAco9_h9iaGNj8q2ethFkMLs91kzk2PAcDTW9gb54h4FRWyuXpoQ';
  RSA_e = 'AQAB';
  RSA_d = 'Eq5xpGnNCivDflJsRQBXHx1hdR1k6Ulwe2JZD50LpXyWPEAeP88vLNO97I'+
          'jlA7_GQ5sLKMgvfTeXZx9SE-7YwVol2NXOoAJe46sui395IW_GO-pWJ1O0'+
          'BkTGoVEn2bKVRUCgu-GjBVaYLU6f3l9kJfFNS3E0QbVdxzubSu3Mkqzjkn'+
          '439X0M_V51gfpRLI9JYanrC4D4qAdGcopV_0ZHHzQlBjudU2QvXt4ehNYT'+
          'CBr6XCLQUShb1juUO1ZdiYoFaFQT5Tw8bGUl_x_jTj3ccPDVZFD9pIuhLh'+
          'BOneufuBiB4cS98l2SR_RQyGWSeWjnczT0QU91p1DhOVRuOopznQ';
  RSA_p = '4BzEEOtIpmVdVEZNCqS7baC4crd0pqnRH_5IB3jw3bcxGn6QLvnEtfdUdi'+
          'YrqBdss1l58BQ3KhooKeQTa9AB0Hw_Py5PJdTJNPY8cQn7ouZ2KKDcmnPG'+
          'BY5t7yLc1QlQ5xHdwW1VhvKn-nXqhJTBgIPgtldC-KDV5z-y2XDwGUc';
  RSA_q = 'uQPEfgmVtjL0Uyyx88GZFF1fOunH3-7cepKmtH4pxhtCoHqpWmT8YAmZxa'+
          'ewHgHAjLYsp1ZSe7zFYHj7C6ul7TjeLQeZD_YwD66t62wDmpe_HlB-TnBA'+
          '-njbglfIsRLtXlnDzQkv5dTltRJ11BKBBypeeF6689rjcJIDEz9RWdc';
  RSA_dp = 'BwKfV3Akq5_MFZDFZCnW-wzl-CCo83WoZvnLQwCTeDv8uzluRSnm71I3Q'+
          'CLdhrqE2e9YkxvuxdBfpT_PI7Yz-FOKnu1R6HsJeDCjn12Sk3vmAktV2zb'+
          '34MCdy7cpdTh_YVr7tss2u6vneTwrA86rZtu5Mbr1C1XsmvkxHQAdYo0';
  RSA_dq = 'h_96-mK1R_7glhsum81dZxjTnYynPbZpHziZjeeHcXYsXaaMwkOlODsWa'+
          '7I9xXDoRwbKgB719rrmI2oKr6N3Do9U0ajaHF-NKJnwgjMd2w9cjz3_-ky'+
          'NlxAr2v4IKhGNpmM5iIgOS1VZnOZ68m6_pbLBSp3nssTdlqvd0tIiTHU';
  RSA_qi = 'IYd7DHOhrWvxkwPQsRM2tOgrjbcrfvtQJipd-DlcxyVuuM9sQLdgjVk2o'+
          'y26F0EmpScGLq2MowX7fhd_QJQ3ydy5cY7YIBi87w93IKLEdfnbJtoOPLU'+
          'W0ITrJReOgo1cq9SbsxYawBgfp_gh6A5603k2-ZQwVK0JKSHuLFkuQ3U';

  Signature ='cC4hiUPoj9Eetdgtv3hF80EGrhuB__dzERat0XF9g2VtQgr9PJbu3XOiZj5RZmh7'+
             'AAuHIm4Bh-0Qc_lF5YKt_O8W2Fp5jujGbds9uJdbF9CUAr7t1dnZcAcQjbKBYNX4'+
             'BAynRFdiuB--f_nZLgrnbyTyWzO75vRK5h6xBArLIARNPvkSjtQBMHlb1L07Qe7K'+
             '0GarZRmB_eSN9383LcOLn6_dO--xi12jzDwusC-eOkHWEsqtFZESc6BfI7noOPqv'+
             'hJ1phCnvWh6IeYI2w9QOYEUipUTI8np6LbgGY9Fs98rqVt5AXLIhWkWywlVmtVrB'+
             'p0igcN_IoypGlUPQGe77Rw';

var
  HeaderEncoded, PayloadEncoded, SignInput, aInput: String;
  X509RSAPrivateKey: TX509RSAPrivateKey;
  X509RSAPublicKey: TX509RSAPublicKey;
  RSA: TRSA;
begin
  HeaderEncoded:=Base64URL.Encode(HeaderJSON,false);
  AssertEquals('Header',HeaderExpected,HeaderEncoded);

  PayloadEncoded:=Base64URL.Encode(PayloadJSON,false);
  AssertEquals('Payload',PayloadExpected,PayloadEncoded);

  SignInput:=HeaderEncoded+'.'+PayloadEncoded;
  if (length(SignInput)<>length(SignInputExpected))
      or not CompareMem(@SignInput[1],@SignInputExpected[0],length(SignInput)) then
    Fail('SignInput');

  X509RSAPrivateKey.InitWithBase64UrlEncoded(RSA_n,RSA_e,RSA_d,RSA_p,RSA_q,RSA_dp,RSA_dq,RSA_qi);
  X509RSAPublicKey.InitWithBase64UrlEncoded(RSA_n,RSA_e);

  RSACreate(RSA);
  try
    RSAInitFromPublicKey(RSA,X509RSAPublicKey);
    FKey.AsBytes:=X509RSAPublicKey.AsDER;

    aInput:=SignInput+'.'+Signature;
    // verify
    FVerifyResult:=TMyJWT.ValidateJWT(aInput,FKey);
    AssertNotNull('Have result',FVerifyResult);
    AssertEquals('Correct class',TMyJWT,FVerifyResult.ClassType);
    AssertNotNull('Have result.claims',FVerifyResult.Claims);
    AssertEquals('Correct claims class',TMyClaims,FVerifyResult.Claims.ClassType);
    AssertEquals('Have correct algorithm','RS256',FVerifyResult.JOSE.Alg);
    AssertEquals('Have correct typ','',FVerifyResult.JOSE.typ);
    AssertEquals('Have correct iss','joe',FVerifyResult.Claims.iss);
    AssertEquals('Have correct exp',1300819380,FVerifyResult.Claims.exp);

  finally
    RSAFree(RSA);
  end;
end;

procedure TTestJWT.TestI2OSP;

  procedure t(c: DWord; Len: integer; const Expected: string);
  var
    Actual: String;
  begin
    Actual:=I2OSP(c,Len);
    if Actual<>Expected then
      Fail('I2OSP('+IntToStr(c)+','+IntToStr(Len)+') expected "'+StringToHex(Expected)+'", but got "'+StringToHex(Actual)+'"');
  end;

begin
  t(0,0,'');
  t(0,1,#0);
  t(1,1,#1);
  t(1,2,#0#1);
  t(258,2,#1#2);
  t($10203,3,#1#2#3);
  t($1020304,4,#1#2#3#4);
  t($ffffffff,4,#255#255#255#255);
end;

procedure TTestJWT.TestMGF1SHA1;

  procedure t(const InputStr: string; Len: integer; const ExpectedHex: String);
  var
    ActualHex: string;
  begin
    ActualHex:=StringToHex(MGF1SHA1(InputStr,Len));
    if ActualHex<>ExpectedHex then
      Fail('MGF1SHA1('+StringToHex(InputStr)+','+IntToStr(Len)+') expected "'+ExpectedHex+'", but got "'+ActualHex+'"');
  end;

begin
  t('foo',3,'1AC907');
  t('foo',5,'1AC9075CD4');
  t('bar',5,'BC0C655E01');
  t('bar',50,'BC0C655E016BC2931D85A2E675181ADCEF7F581F76DF2739DA74FAAC41627BE2F7F415C89E983FD0CE80CED9878641CB4876');
end;

procedure TTestJWT.TestMGF1SHA256;

  procedure t(const InputStr: string; Len: integer; const ExpectedHex: String);
  var
    ActualHex: string;
  begin
    ActualHex:=StringToHex(MGF1SHA256(InputStr,Len));
    if ActualHex<>ExpectedHex then
      Fail('MGF1SHA256('+StringToHex(InputStr)+','+IntToStr(Len)+') expected "'+ExpectedHex+'", but got "'+ActualHex+'"');
  end;

begin
  t('bar',1,'38');
  t('bar',50,'382576A7841021CC28FC4C0948753FB8312090CEA942EA4C4E735D10DC724B155F9F6069F289D61DACA0CB814502EF04EAE1');
end;

procedure TTestJWT.TestVerifyPS256;
const
  HeaderJSON = '{"alg":"PS256"}';
  HeaderExpected = 'eyJhbGciOiJQUzI1NiJ9';

  PayloadJSON = 'In our village, folks say God crumbles up the old moon into stars.';
  PayloadExpected = 'SW4gb3VyIHZpbGxhZ2UsIGZvbGtzIHNheSBHb2QgY3J1bWJsZXMgdXAgdGhlIG9sZCBtb29uIGludG8gc3RhcnMu';

  RSA_n = 'ofgWCuLjybRlzo0tZWJjNiuSfb4p4fAkd_wWJcyQoTbji9k0l8W26mPddx'+
          'HmfHQp-Vaw-4qPCJrcS2mJPMEzP1Pt0Bm4d4QlL-yRT-SFd2lZS-pCgNMs'+
          'D1W_YpRPEwOWvG6b32690r2jZ47soMZo9wGzjb_7OMg0LOL-bSf63kpaSH'+
          'SXndS5z5rexMdbBYUsLA9e-KXBdQOS-UTo7WTBEMa2R2CapHg665xsmtdV'+
          'MTBQY4uDZlxvb3qCo5ZwKh9kG4LT6_I5IhlJH7aGhyxXFvUK-DWNmoudF8'+
          'NAco9_h9iaGNj8q2ethFkMLs91kzk2PAcDTW9gb54h4FRWyuXpoQ';
  RSA_e = 'AQAB';

  SignatureEncoded =
     'TRWhwRo5dMv9-8OzrInfJTwmUGYgjLfHk8lqF072ND-FmLWEBnUTOpY8oJXp'
    +'8FdWw2SalbdOeNlrtlJjwk4XK8Ql2iJ_2qMCtxsvLPhKBOqFoAF4aBvTOEDV'
    +'JDxf0DaBSiydEEtfTVV2iwBcjWabu5J2XieR5y7QZQtuHsn7T3qKBvCcCejN'
    +'3Y2oqAT3qMHvu1fTms1r_91wBn_K7Wjd9UkZ1n02qQcUHJznR_OF2BgN7_KW'
    +'IDAF9ZS9keoju2NPpPelO4yxa2XUPnehY3G7dHKoCxUEQR4d2Xc5voqDASTV'
    +'CDqQS4PVOZdvT3Ein6-SanAlCwbWBbkvT8g6-5PImQ';
var
  X509RSAPublicKey: TX509RSAPublicKey;
  RSA: TRSA;
  HeaderEncoded, PayloadEncoded, SignInput: String;
  r: Int64;
  Signature: TBytes;
begin
  // RSASSA-PSS using SHA-256 and MGF1 with SHA-256

  HeaderEncoded:=Base64URL.Encode(HeaderJSON,false);
  AssertEquals('Header',HeaderExpected,HeaderEncoded);

  PayloadEncoded:=Base64URL.Encode(PayloadJSON,false);
  AssertEquals('Payload',PayloadExpected,PayloadEncoded);

  SignInput:=HeaderEncoded+'.'+PayloadEncoded;

  // load public key
  X509RSAPublicKey.InitWithBase64UrlEncoded(RSA_n,RSA_e);

  RSACreate(RSA);
  try
    RSAInitFromPublicKey(RSA,X509RSAPublicKey);
    AssertEquals('RSA.ModulusLen',256,RSA.ModulusLen);
    AssertEquals('RSA.ModulusBits',2048,RSA.ModulusBits);

    Signature:=Base64URL.Decode(SignatureEncoded,false);
    AssertEquals('length(Signature)',RSA.ModulusLen,length(Signature));

    r:=RSASSA_PS256_Verify(RSA,@SignInput[1],length(SignInput),@Signature[0]);
    AssertEquals('RSASSA_PS256_Verify',0,r);

  finally
    RSAFree(RSA);
  end;
end;

procedure TTestJWT.TestVerifyPS256Pem;
begin
  TestVerifyRSAPem(TJWTSignerPS256);
end;

procedure TTestJWT.TestVerifyPS384Pem;
begin
  TestVerifyRSAPem(TJWTSignerPS384);
end;

procedure TTestJWT.TestVerifyPS512Pem;
begin
  TestVerifyRSAPem(TJWTSignerPS512);
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

procedure TTestJWT.GetTestPEM(out aPrivateKeyPEM, aPublicKeyPEM: string);
const
  // generated with
  //   openssl genrsa -out private.pem 2048
  PrivateKeyPem =
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
  PublicKeyPem =
    '-----BEGIN PUBLIC KEY-----'#10+
    'MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAvkRfGW8psCZ3G4+hBA6W'#10+
    '/CR/FHhBLB3k3QLypamPbRFlFBxLtOK2NblBybY22vUiMLZbb5x8OoOj/IhOrJAl'#10+
    'TqhtbTWLy/0K3qbG09vLm8V40kEK8/p0STrp3UmsxHNkccj9MRSKk7pOyEvxSCY6'#10+
    'K5JGK1VTsMuDCS7DCYk6Vqr3zjX7qedF1PVM+Z5t0B+f//kt3oBETNlic4IooEpG'#10+
    '/PN2GUQ0oZpa16DDtfgGu7wT3X3QEZFWLJYQTvGc82NpachBIUvqNdIt1npbK38M'#10+
    'XU4IPHVrSN/HdK2nQPSMLdKnTV+Eh/HcxpfjBjarg+VjgDqlmqJ9bkosOVn35vsg'#10+
    '8wIDAQAB'#10+
    '-----END PUBLIC KEY-----';
begin
  aPrivateKeyPEM:=PrivateKeyPem;
  aPublicKeyPEM:=PublicKeyPem;
end;

procedure TTestJWT.TestVerifyRSAPem(SignerClass: TJWTSignerClass);
var
  aInput, aPrivateKeyPEM, aPublicKeyPEM: String;
  Signer: TJWTSigner;
  NewDER: TBytes;
  RSAPublic: TX509RSAPublicKey;
  RSAPrivate: TX509RSAPrivateKey;
begin
  GetTestPEM(aPrivateKeyPEM, aPublicKeyPEM);

  // header
  jwt.JOSE.alg:=SignerClass.AlgorithmName;

  // claims
  jwt.Claims.exp:=DateTimeToUnix(Now+10);
  jwt.Claims.iss:='FPC JWT';

  // load private key from pem
  FKey.AsBytes:=PemToDER(APrivateKeyPem,_BEGIN_RSA_PRIVATE_KEY,_END_RSA_PRIVATE_KEY);
  X509RsaPrivateKeyInitFromDER(RSAPrivate,FKey.AsBytes);
  NewDER:=RSAPrivate.AsDER;
  if (length(FKey.AsBytes)<>length(NewDER)) or
      not CompareMem(@FKey.AsBytes[0],@NewDER[0],length(NewDER)) then
    Fail('TX509RSAPrivateKey.AsDER');

  // sign
  Signer:=TJWTSigner(SignerClass.Create);
  try
    aInput:=Signer.AppendSignature(JWT,Key);
  finally
    Signer.Free;
  end;

  // load public key from pem
  FKey.AsBytes:=PemToDER(APublicKeyPem,_BEGIN_PUBLIC_KEY,_END_PUBLIC_KEY);
  X509RsaPublicKeyInitFromDER(RSAPublic,FKey.AsBytes);
  NewDER:=RSAPublic.AsDER;
  if (length(FKey.AsBytes)<>length(NewDER)) or
      not CompareMem(@FKey.AsBytes[0],@NewDER[0],length(NewDER)) then
    Fail('TX509RSAPublicKey.AsDER');

  // verify
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

