{$IFNDEF FPC_DOTTEDUNITS}
unit fpjwasha384;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, Jwt.Types;
{$ELSE FPC_DOTTEDUNITS}
uses
  sysutils, fpjwt;
{$ENDIF FPC_DOTTEDUNITS}

Type

  { TJWTSignerHS384 }

  TJWTSignerHS384 = Class(TJWTSigner)
  Public
    Class function AlgorithmName : String; override;
    Function CreateSignature(aJWT : TJWT; aKey : TJWTKey) : String; override;
    Function Verify(const aJWT : String; aKey : TJWTKey) : Boolean; override;
  end;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.Hash.Sha512, Fcl.BaseNEnc;
{$ELSE FPC_DOTTEDUNITS}
uses fpsha512, basenenc;
{$ENDIF FPC_DOTTEDUNITS}

Const
  SErrHMACFailed = 'HMAC SHA384 of JWT Failed';



{ TJWTSignerHS384 }

class function TJWTSignerHS384.AlgorithmName: String;
begin
  Result:='HS384';
end;

function TJWTSignerHS384.CreateSignature(aJWT: TJWT; aKey: TJWTKey): String;

Var
  B : TBytes;
  aDigest : TSHA384Digest;

begin
  B:=GetSignInput(aJWT);
  aDigest:=Default(TSHA384Digest);
  if Not TSHA384.HMAC(aKey.AsPointer,aKey.Length,PByte(B),Length(B),aDigest) then
    Raise EJWT.Create(SErrHMACFailed);
  Result:=Base64URL.Encode(@aDigest[0],Length(aDigest),False);
end;

function TJWTSignerHS384.Verify(const aJWT: String; aKey: TJWTKey): Boolean;

Var
  J,C,S,S2 : String;
  B : TBytes;
  aDigest : TSHA384Digest;

begin
  Result:=GetParts(aJWT,J,C,S);
  if Not Result then
    exit;
  aDigest:=Default(TSHA384Digest);
  B:=TEncoding.UTF8.GetAnsiBytes(J+'.'+C);
  if Not TSHA384.HMAC(aKey.AsPointer,aKey.Length,PByte(B),Length(B),aDigest) then
    exit;
  S2:=Base64URL.Encode(@aDigest[0],Length(aDigest),False);
  Result:=(S2=S);
end;

initialization
  TJWTSignerHS384.Register;
end.

