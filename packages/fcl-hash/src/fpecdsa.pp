unit fpecdsa;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}
interface

uses
  sysutils, fpecc, fpsha256;

Type
  { TECDSA }

  TECDSA = record
    Class Function SignSHA256(aPayLoad : String; aPrivateKey : TECCPrivateKey; out aSignature : TEccSignature) : Boolean; static; overload;
    Class Function SignSHA256(aPayLoad : TBytes; aPrivateKey : TECCPrivateKey; out aSignature : TEccSignature) : Boolean; static; overload;
    Class Function VerifySHA256(aPayLoad : String; aPrivateKey : TECCPrivateKey; const aSignature : TEccSignature) : Boolean; static; overload;
    Class Function VerifySHA256(aPayLoad : TBytes; aPrivateKey : TECCPrivateKey; const aSignature : TEccSignature) : Boolean; static; overload;
    Class Function VerifySHA256(aPayLoad : String; aPublicKey : TECCPublicKey; const aSignature : TEccSignature) : Boolean; static; overload;
    Class Function VerifySHA256(aPayLoad : TBytes; aPublicKey : TECCPublicKey; const aSignature : TEccSignature) : Boolean; static;overload;
   end;

implementation



{ TECDSA }

class Function TECDSA.SignSHA256(aPayLoad: String; aPrivateKey: TECCPrivateKey; out aSignature: TEccSignature) : boolean;
begin
  Result:=SignSha256(TEncoding.UTF8.GetAnsiBytes(aPayload),aPrivateKey,aSignature);
end;

class Function TECDSA.SignSHA256(aPayLoad: TBytes; aPrivateKey: TECCPrivateKey; out aSignature: TEccSignature) : Boolean;

var
  lSHA256: TSHA256;
  eccHash : TECCHash;
  Pubkey : TECCpublicKey;

begin
  aSignature:=Default(TEccSignature);
  lSHA256.Init;
  lSHA256.Update(aPayload);
  lSHA256.Final;
  Move(lSHA256.Digest[0], eccHash, SizeOf(eccHash));
  Result:=EcdsaSign(aPrivateKey,eccHash,aSignature);
  if Result then
    begin
    EccPublicKeyFromPrivateKey(PubKey,aPrivateKey);
    Result:=EcdsaVerify(Pubkey,eccHash,aSignature);
    end;
end;

Class Function TECDSA.VerifySHA256(aPayLoad : String; aPrivateKey : TECCPrivateKey; const aSignature : TEccSignature) : Boolean; static;

var
  Pubkey : TECCpublicKey;

begin
  EccPublicKeyFromPrivateKey(PubKey,aPrivateKey);
  Result:=VerifySHA256(aPayLoad,PubKey,aSignature);
end;

Class Function TECDSA.VerifySHA256(aPayLoad : TBytes; aPrivateKey : TECCPrivateKey; const aSignature : TEccSignature) : Boolean; static;

var
  Pubkey : TECCpublicKey;

begin
  EccPublicKeyFromPrivateKey(PubKey,aPrivateKey);
  Result:=VerifySHA256(aPayLoad,PubKey,aSignature);
end;

Class Function TECDSA.VerifySHA256(aPayLoad : String; aPublicKey : TECCPublicKey; const aSignature : TEccSignature) : Boolean; static;

begin
  Result:=VerifySHA256(TEncoding.UTF8.GetAnsiBytes(aPayload),aPublicKey,aSignature);
end;

Class Function TECDSA.VerifySHA256(aPayLoad : TBytes; aPublicKey : TECCPublicKey; const aSignature : TEccSignature) : Boolean; static;

var
  lSHA256: TSHA256;
  eccHash : TECCHash;

begin
  lSHA256.Init;
  lSHA256.Update(aPayload);
  lSHA256.Final;
  Move(lSHA256.Digest[0], eccHash, SizeOf(eccHash));
  Result:=EcdsaVerify(aPublickey,eccHash,aSignature);
end;



end.

