unit fpjwarsa;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, basenenc, fpjwt, fprsa, fpsha256;

Type

  { TJWTSignerRS256 }

  TJWTSignerRS256 = Class(TJWTSigner)
  Public
    Class function AlgorithmName : String; override;
    Function CreateSignature(aJWT : TJWT; aKey : TJWTKey) : String; override;
    Function Verify(const aJWT : String; aKey : TJWTKey) : Boolean; override; overload;
  end;

implementation

{ TJWTSignerRS256 }

class function TJWTSignerRS256.AlgorithmName: String;
begin
  Result:='rs256';
end;

function TJWTSignerRS256.CreateSignature(aJWT: TJWT; aKey: TJWTKey): String;
var
  aSignInput, Hash, aSignature: TBytes;
  RSA: TRSA;
begin
  Result:='';

  aSignInput:=GetSignInput(aJWT);
  if length(aSignInput)=0 then
    raise Exception.Create('20220430010854: missing SignInput');

  Hash:=nil;
  TSHA256.DigestBytes(aSignInput,Hash);

  RSACreate(RSA);
  try
    RSAInitFromPrivateKeyDER(RSA,aKey.AsBytes);
    SetLength(aSignature{%H-},RSA.ModulusLen);
    if RSAEncryptSign(RSA,@Hash[0],length(Hash),@aSignature[0],false)<RSA.ModulusLen then
      raise Exception.Create('20220429223334');
    Result:=Base64URL.Encode(@aSignature[0],Length(aSignature),False);
  finally
    RSAFree(RSA);
  end;
end;

function TJWTSignerRS256.Verify(const aJWT: String; aKey: TJWTKey): Boolean;
var
  aHeader, theClaims, aSignature, aInput: String;
  InputBytes, EncryptedHash, DecryptedHash, ActualHash: TBytes;
  RSA: TRSA;
  HashLen: Integer;
begin
  Result:=false;
  if aJWT='' then exit;

  if not GetParts(aJWT,aHeader,theClaims,aSignature) then exit;
  if aSignature='' then exit;

  EncryptedHash:=Base64URL.Decode(aSignature);

  // decrypt hash
  RSACreate(RSA);
  try
    RSAInitFromPrivateKeyDER(RSA,aKey.AsBytes);
    SetLength(DecryptedHash{%H-},length(EncryptedHash));
    HashLen:=RSADecryptVerify(RSA,@EncryptedHash[0],@DecryptedHash[0],length(DecryptedHash),false);
    if HashLen<=0 then exit;
    SetLength(DecryptedHash,HashLen);
  finally
    RSAFree(RSA);
  end;

  // hash of header.claims
  aInput:=aHeader+'.'+theClaims;
  SetLength(InputBytes{%H-},length(aInput));
  Move(aInput[1],InputBytes[0],length(aInput));
  ActualHash:=nil;
  TSHA256.DigestBytes(InputBytes,ActualHash);

  // check decrypted hash and actual hash fit
  Result:=(length(DecryptedHash)=length(ActualHash))
    and CompareMem(@DecryptedHash[0],@ActualHash[0],length(DecryptedHash));
end;

initialization
  TJWTSignerRS256.Register;
end.

