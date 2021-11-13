unit fpjwasha512;

{$mode ObjFPC}{$H+}

interface

uses
  sysutils, fpjwt;

Type

  { TJWTSignerHS512 }

  TJWTSignerHS512 = Class(TJWTSigner)
  Public
    Class function AlgorithmName : String; override;
    Function CreateSignature(aJWT : TJWT; aKey : TJWTKey) : String; override;
    Function Verify(const aJWT : String; aKey : TJWTKey) : Boolean; override;
  end;

implementation

uses fpsha512, basenenc;

Const
  SErrHMACFailed = 'HMAC SHA512 of JWT Failed';



{ TJWTSignerHS512 }

class function TJWTSignerHS512.AlgorithmName: String;
begin
  Result:='HS512';
end;

function TJWTSignerHS512.CreateSignature(aJWT: TJWT; aKey: TJWTKey): String;

Var
  B : TBytes;
  aDigest : TSHA512Digest;

begin
  B:=GetSignInput(aJWT);
  aDigest:=Default(TSHA512Digest);
  if Not TSHA512.HMAC(aKey.AsPointer,aKey.Length,PByte(B),Length(B),aDigest) then
    Raise EJWT.Create(SErrHMACFailed);
  Result:=Base64URL.Encode(@aDigest[0],Length(aDigest),False);
end;

function TJWTSignerHS512.Verify(const aJWT: String; aKey: TJWTKey): Boolean;

Var
  J,C,S,S2 : String;
  B : TBytes;
  aDigest : TSHA512Digest;

begin
  Result:=GetParts(aJWT,J,C,S);
  if Not Result then
    exit;
  aDigest:=Default(TSHA512Digest);
  B:=TEncoding.UTF8.GetAnsiBytes(J+'.'+C);
  if Not TSHA512.HMAC(aKey.AsPointer,aKey.Length,PByte(B),Length(B),aDigest) then
    exit;
  S2:=Base64URL.Encode(@aDigest[0],Length(aDigest),False);
  Result:=(S2=S);
end;

initialization
  TJWTSignerHS512.Register;
end.

