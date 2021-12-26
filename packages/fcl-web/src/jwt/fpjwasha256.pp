unit fpjwasha256;

{$mode ObjFPC}{$H+}

interface

uses
  sysutils, fpjwt;

Type

  { TJWTSignerHS256 }

  TJWTSignerHS256 = Class(TJWTSigner)
  Public
    Class function AlgorithmName : String; override;
    Function CreateSignature(aJWT : TJWT; aKey : TJWTKey) : String; override;
    Function Verify(const aJWT : String; aKey : TJWTKey) : Boolean; override;
  end;

implementation

uses fpsha256, basenenc;

Const
  SErrHMACFailed = 'HMAC SHA256 of JWT Failed';



{ TJWTSignerHS256 }

class function TJWTSignerHS256.AlgorithmName: String;
begin
  Result:='HS256';
end;

function TJWTSignerHS256.CreateSignature(aJWT: TJWT; aKey: TJWTKey): String;

Var
  B : TBytes;
  aDigest : TSHA256Digest;

begin
  B:=GetSignInput(aJWT);
  aDigest:=Default(TSHA256Digest);
  if Not TSHA256.HMAC(aKey.AsPointer,aKey.Length,PByte(B),Length(B),aDigest) then
    Raise EJWT.Create(SErrHMACFailed);
  Result:=Base64URL.Encode(@aDigest[0],Length(aDigest),False);
end;

function TJWTSignerHS256.Verify(const aJWT: String; aKey: TJWTKey): Boolean;

Var
  J,C,S,S2 : String;
  B : TBytes;
  aDigest : TSHA256Digest;

begin
  Result:=GetParts(aJWT,J,C,S);
  if Not Result then
    exit;
  aDigest:=Default(TSHA256Digest);
  B:=TEncoding.UTF8.GetAnsiBytes(J+'.'+C);
  if Not TSHA256.HMAC(aKey.AsPointer,aKey.Length,PByte(B),Length(B),aDigest) then
    exit;
  S2:=Base64URL.Encode(@aDigest[0],Length(aDigest),False);
  Result:=(S2=S);
end;

initialization
  TJWTSignerHS256.Register;
end.

