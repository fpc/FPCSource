unit testsha256;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, sha256, sha512, hashutils;

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

implementation

uses
  basenenc;

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
  RegisterTest(TTestSHA256);
end.

