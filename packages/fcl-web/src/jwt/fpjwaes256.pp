{$IFNDEF FPC_DOTTEDUNITS}
unit fpjwaes256;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, Jwt.Types, System.Hash.Ecc;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, fpjwt, fpEcc;
{$ENDIF FPC_DOTTEDUNITS}

Type
  { TJWTSignerES256 }

  TJWTSignerES256 = Class(TJWTSigner)
  Public
    Class function AlgorithmName : String; override;
    Function CreateSignature(aJWT : TJWT; aKey : TJWTKey) : String; override;
    Function Verify(const aJWT : String; aKey : TJWTKey) : Boolean; override; overload;
    Class Function Verify(const aJWT : String; aPrivateKey : TECCPrivateKey) : Boolean; overload;
    Class Function Verify(const aJWT : String; aPublicKey : TECCPublicKey) : Boolean; overload;
  end;


implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.Hash.Utils, Fcl.BaseNEnc, System.Hash.Ecdsa;
{$ELSE FPC_DOTTEDUNITS}
uses fphashutils, basenenc, fpecdsa;
{$ENDIF FPC_DOTTEDUNITS}

{ TJWTSignerES256 }

class function TJWTSignerES256.AlgorithmName: String;
begin
  Result:='ES256';
end;

function TJWTSignerES256.CreateSignature(aJWT: TJWT; aKey: TJWTKey): String;
Var
  B : TBytes;
  aPrivateKey : TEccPrivateKey;
  aSignature : TEccSignature;

begin
  Result:='';
  aPrivateKey:=Default(TECCPrivateKey);
  Move(aKey.AsPointer,aPrivateKey,Sizeof(aPrivateKey));
  B:=GetSignInput(aJWT);
  if TECDSA.SignSHA256(B,aPrivateKey,aSignature) then
    Result:=Base64URL.Encode(@aSignature[0],Length(aSignature),False);
end;

function TJWTSignerES256.Verify(const aJWT: String; aKey: TJWTKey): Boolean;

var
  aPrivateKey : TECCPrivateKey;

begin
  aPrivateKey:=Default(TECCPrivateKey);
  Move(aKey.AsPointer^,aPrivateKey,Sizeof(aPrivateKey));
  Result:=Verify(aJWT,aPrivateKey);
end;

Class function TJWTSignerES256.Verify(const aJWT: String; aPrivateKey: TECCPrivateKey): Boolean;

Var
  J,C,S : String;
  aSignature : TEccSignature;
  B : TBytes;

begin
  Result:=GetParts(aJWT,J,C,S);
  if Not Result then
    exit;
{$IF SIZEOF(CHAR)=2}    
  B:=TEncoding.UTF8.GetBytes(J+'.'+C);
{$ELSE}  
  B:=TEncoding.UTF8.GetAnsiBytes(J+'.'+C);
{$ENDIF}  
  BytesToVar(Base64url.Decode(S),aSignature,Sizeof(aSignature));
  Result:=TECDSA.verifySHA256(B,aPrivateKey,aSignature);
end;

class function TJWTSignerES256.Verify(const aJWT: String; aPublicKey: TECCPublicKey): Boolean;

Var
  J,C,S : String;
  aSignature : TEccSignature;
  B : TBytes;

begin
  Result:=GetParts(aJWT,J,C,S);
  if Not Result then
    exit;
{$IF SIZEOF(CHAR)=2}    
  B:=TEncoding.UTF8.GetBytes(J+'.'+C);
{$ELSE}     
  B:=TEncoding.UTF8.GetAnsiBytes(J+'.'+C);
{$ENDIF}  
  Base64url.Decode(S,@aSignature);
  Result:=TECDSA.verifySHA256(B,aPublicKey,aSignature);
end;

initialization
  TJWTSignerES256.Register;
end.

