unit fpjwarsa;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, basenenc, fpjwt, fprsa, fpsha256, fpsha512;

Type

  { TJWTSignerRSA }

  TJWTSignerRSA = Class(TJWTSigner)
  Public
    Class function AlgorithmName : String; override;
    function ComputeASNHash(const Value: TBytes): TBytes; virtual; abstract;
    Function CreateSignature(aJWT : TJWT; aPrivateKey : TJWTKey) : String; override;
    Function Verify(const aJWT : String; aPublicKey : TJWTKey) : Boolean; override; overload;
  end;
  TJWTSignerRSAClass = class of TJWTSignerRSA;

  { TJWTSignerRS256 }

  TJWTSignerRS256 = class(TJWTSignerRSA)
  public
    class function AlgorithmName : String; override;
    function ComputeASNHash(const Value: TBytes): TBytes; override;
  end;

  { TJWTSignerRS384 }

  TJWTSignerRS384 = class(TJWTSignerRSA)
  public
    class function AlgorithmName : String; override;
    function ComputeASNHash(const Value: TBytes): TBytes; override;
  end;

  { TJWTSignerRS512 }

  TJWTSignerRS512 = class(TJWTSignerRSA)
  public
    class function AlgorithmName : String; override;
    function ComputeASNHash(const Value: TBytes): TBytes; override;
  end;

implementation

{ TJWTSignerRS512 }

class function TJWTSignerRS512.AlgorithmName: String;
begin
  Result:='RS512';
end;

function TJWTSignerRS512.ComputeASNHash(const Value: TBytes): TBytes;
begin
  Result:=nil;
  TSHA512.DigestBytes(Value,Result);
  Result:=Concat(EncodeDigestInfoSHA(RSADigestInfoSHA512,length(Result)),Result);
end;

{ TJWTSignerRS384 }

class function TJWTSignerRS384.AlgorithmName: String;
begin
  Result:='RS384';
end;

function TJWTSignerRS384.ComputeASNHash(const Value: TBytes): TBytes;
begin
  Result:=nil;
  TSHA384.DigestBytes(Value,Result);
  Result:=Concat(EncodeDigestInfoSHA(RSADigestInfoSHA384,length(Result)),Result);
end;

{ TJWTSignerRS256 }

class function TJWTSignerRS256.AlgorithmName: String;
begin
  Result:='RS256';
end;

function TJWTSignerRS256.ComputeASNHash(const Value: TBytes): TBytes;
begin
  Result:=nil;
  TSHA256.DigestBytes(Value,Result);
  Result:=Concat(EncodeDigestInfoSHA(RSADigestInfoSHA256,length(Result)),Result);
end;

{ TJWTSignerRSA }

class function TJWTSignerRSA.AlgorithmName: String;
begin
  raise Exception.Create('20220430014637 abstract class');
  Result:='RSA';
end;

function TJWTSignerRSA.CreateSignature(aJWT: TJWT; aPrivateKey: TJWTKey): String;
var
  aSignInput, ASNHash, aSignature: TBytes;
  RSA: TRSA;
begin
  Result:='';

  aSignInput:=GetSignInput(aJWT);
  if length(aSignInput)=0 then
    raise Exception.Create('20220430010854: missing SignInput');
  ASNHash:=ComputeASNHash(aSignInput);

  RSACreate(RSA);
  try
    RSAInitFromPrivateKeyDER(RSA,aPrivateKey.AsBytes);
    SetLength(aSignature{%H-},RSA.ModulusLen);
    if RSAEncryptSign(RSA,@ASNHash[0],length(ASNHash),@aSignature[0],true)<RSA.ModulusLen then
      raise Exception.Create('20220429223334');
    Result:=Base64URL.Encode(@aSignature[0],Length(aSignature),False);
  finally
    RSAFree(RSA);
  end;
end;

function TJWTSignerRSA.Verify(const aJWT: String; aPublicKey: TJWTKey): Boolean;
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
    RSAInitFromPublicKeyDER(RSA,aPublicKey.AsBytes);
    SetLength(DecryptedHash{%H-},length(EncryptedHash));
    HashLen:=RSADecryptVerify(RSA,@EncryptedHash[0],@DecryptedHash[0],length(DecryptedHash),true);
    if HashLen<=0 then exit;
    SetLength(DecryptedHash,HashLen);
  finally
    RSAFree(RSA);
  end;

  // hash of header.claims
  aInput:=aHeader+'.'+theClaims;
  SetLength(InputBytes{%H-},length(aInput));
  Move(aInput[1],InputBytes[0],length(aInput));
  ActualHash:=ComputeASNHash(InputBytes);

  // check decrypted hash and actual hash fit
  Result:=(length(DecryptedHash)=length(ActualHash))
    and CompareMem(@DecryptedHash[0],@ActualHash[0],length(DecryptedHash));
end;

initialization
  TJWTSignerRS256.Register;
  TJWTSignerRS384.Register;
  TJWTSignerRS512.Register;
end.

