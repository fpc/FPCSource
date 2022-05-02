unit fpjwarsa;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, basenenc, fpjwt, fprsa, fpsha256, fpsha512, fphashutils;

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

  { TJWTSignerRSAPSS }

  TJWTSignerRSAPSS = Class(TJWTSigner)
  Public
    Class function AlgorithmName : String; override;
    Class procedure GetHashFunc(var HashFunc: TRSAHashFuncInfo); virtual; abstract;
    Function CreateSignature(aJWT : TJWT; aPrivateKey : TJWTKey) : String; override;
    Function Verify(const aJWT : String; aPublicKey : TJWTKey) : Boolean; override; overload;
  end;
  TJWTSignerRSAPSSClass = class of TJWTSignerRSAPSS;

  { TJWTSignerPS256 }

  TJWTSignerPS256 = class(TJWTSignerRSAPSS)
  public
    class function AlgorithmName : String; override;
    class procedure GetHashFunc(var HashFunc: TRSAHashFuncInfo); override;
  end;

  { TJWTSignerPS384 }

  TJWTSignerPS384 = class(TJWTSignerRSAPSS)
  public
    class function AlgorithmName : String; override;
    class procedure GetHashFunc(var HashFunc: TRSAHashFuncInfo); override;
  end;

  { TJWTSignerPS512 }

  TJWTSignerPS512 = class(TJWTSignerRSAPSS)
  public
    class function AlgorithmName : String; override;
    class procedure GetHashFunc(var HashFunc: TRSAHashFuncInfo); override;
  end;

implementation

{ TJWTSignerPS512 }

class function TJWTSignerPS512.AlgorithmName: String;
begin
  Result:='PS512';
end;

class procedure TJWTSignerPS512.GetHashFunc(var HashFunc: TRSAHashFuncInfo);
begin
  HashFunc.UseSHA512;
end;

{ TJWTSignerPS384 }

class function TJWTSignerPS384.AlgorithmName: String;
begin
  Result:='PS384';
end;

class procedure TJWTSignerPS384.GetHashFunc(var HashFunc: TRSAHashFuncInfo);
begin
  HashFunc.UseSHA384;
end;

{ TJWTSignerPS256 }

class function TJWTSignerPS256.AlgorithmName: String;
begin
  Result:='PS256';
end;

class procedure TJWTSignerPS256.GetHashFunc(var HashFunc: TRSAHashFuncInfo);
begin
  HashFunc.UseSHA256;
end;

{ TJWTSignerRSAPSS }

class function TJWTSignerRSAPSS.AlgorithmName: String;
begin
  raise Exception.Create('20220503003125 abstract class');
  Result:='RSAPSS';
end;

function TJWTSignerRSAPSS.CreateSignature(aJWT: TJWT; aPrivateKey: TJWTKey
  ): String;
var
  aSignInput, aSignature: TBytes;
  RSA: TRSA;
  HashFunc: TRSAHashFuncInfo;
begin
  Result:='';

  aSignInput:=GetSignInput(aJWT);
  if length(aSignInput)=0 then
    raise Exception.Create('20220503003238: missing SignInput');

  GetHashFunc(HashFunc);

  RSACreate(RSA);
  try
    RSAInitFromPrivateKeyDER(RSA,aPrivateKey.AsBytes);
    SetLength(aSignature{%H-},RSA.ModulusLen);
    if RSASSA_PSS_Sign(RSA,@aSignInput[0],length(aSignInput),@HashFunc,@aSignature[0])<RSA.ModulusLen then
      raise Exception.Create('20220503003617');
    Result:=Base64URL.Encode(@aSignature[0],Length(aSignature),False);
  finally
    RSAFree(RSA);
  end;
end;

function TJWTSignerRSAPSS.Verify(const aJWT: String; aPublicKey: TJWTKey
  ): Boolean;
var
  aHeader, theClaims, aSignature, aSignInput: String;
  EncryptedHash: TBytes;
  RSA: TRSA;
  HashFunc: TRSAHashFuncInfo;
  r: Int64;
begin
  Result:=false;
  if aJWT='' then exit;

  try
    if not GetParts(aJWT,aHeader,theClaims,aSignature) then exit;
    if aSignature='' then exit;

    aSignInput:=aHeader+'.'+theClaims;
    EncryptedHash:=Base64URL.Decode(aSignature);
    GetHashFunc(HashFunc);

    // verify hash
    RSACreate(RSA);
    try
      RSAInitFromPublicKeyDER(RSA,aPublicKey.AsBytes);
      if length(EncryptedHash)<>RSA.ModulusLen then
        exit;
      r:=RSASSA_PSS_Verify(RSA,@aSignInput[1],length(aSignInput),@HashFunc,@EncryptedHash[0]);
      Result:=r=0;
    finally
      RSAFree(RSA);
    end;
  except
  end;
end;

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

  try
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
  except
  end;
end;

initialization
  TJWTSignerRS256.Register;
  TJWTSignerRS384.Register;
  TJWTSignerRS512.Register;
  TJWTSignerPS256.Register;
  TJWTSignerPS384.Register;
  TJWTSignerPS512.Register;
end.

